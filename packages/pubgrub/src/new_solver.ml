open Std

module Log = struct
  let debug _ = ()

  let info _ = ()

  let error _ = ()

  let trace _ = ()
end

open Std.Collections

type package = string

type version = Version.t

let version_compare = fun a b ->
  match Version.compare a b with
  | Lt -> (-1)
  | Eq -> 0
  | Gt -> 1

let equal_ranges = fun left right ->
  Ranges.subset_of ~compare_v:version_compare left right
  && Ranges.subset_of ~compare_v:version_compare right left

let equal_constraint = fun left right ->
  match (left, right) with
  | `Undecided, `Undecided -> true
  | `Decided left, `Decided right -> version_compare left right = 0
  | `Constrained left, `Constrained right -> equal_ranges left right
  | _ -> false

let equal_term = fun left right ->
  String.equal (Term.package left) (Term.package right)
  && Bool.equal (Term.is_positive left) (Term.is_positive right)
  && equal_ranges (Term.ranges left) (Term.ranges right)

let equal_incompatibility_terms = fun left right ->
  let left_terms = Incompatibility.terms left in
  let right_terms = Incompatibility.terms right in
  let rec remove_first_match term acc = function
    | [] -> None
    | candidate :: tail when equal_term term candidate -> Some (List.reverse_append acc tail)
    | candidate :: tail -> remove_first_match term (candidate :: acc) tail
  in
  let rec consume unmatched remaining =
    match remaining with
    | [] -> true
    | term :: rest -> (
        match remove_first_match term [] unmatched with
        | Some unmatched -> consume unmatched rest
        | None -> false
      )
  in
  List.length left_terms = List.length right_terms && consume right_terms left_terms

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

(* ============================================================================
   Core Data Structures
   ============================================================================ *)

(* Dependency information: which packages depend on what *)

type dependency = {
  dependent: package;  (* The package that has this dependency *)
  dependent_version: version;  (* At which version *)
  dependency: package;  (* The package it depends on *)
  ranges: version Ranges.t;  (* The version range required *)
  decision_level: int;  (* When this dependency was added *)
}

(* The dependency graph tracks all dependencies explicitly *)

module DependencyGraph = struct
  type t = {
    (* Map from (package, version) to its list of dependencies *)
    deps: (package * version, (package * version Ranges.t) list) HashMap.t;
    (* Map from package to which packages depend on it (reverse index) *)
    reverse: (package, (package * version) list) HashMap.t;
  }

  let empty = fun () -> { deps = HashMap.create (); reverse = HashMap.create () }

  let add_dependencies = fun graph pkg ver deps ->
    (* Store forward mapping: (pkg, ver) -> deps *)
    let _ = HashMap.insert graph.deps ~key:(pkg, ver) ~value:deps in
    (* Store reverse mapping: dep_pkg -> [(pkg, ver), ...] *)
    List.for_each deps
      ~fn:(fun (dep_pkg, _ranges) ->
        let existing =
          match HashMap.get graph.reverse ~key:dep_pkg with
          | Some l -> l
          | None -> []
        in
        if not (List.contains existing ~value:(pkg, ver)) then
          let _ = HashMap.insert graph.reverse ~key:dep_pkg ~value:((pkg, ver) :: existing) in
          ());
    graph

  let get_dependencies = fun graph pkg ver ->
    match HashMap.get graph.deps ~key:(pkg, ver) with
    | Some deps -> deps
    | None -> []

  let get_dependents = fun graph pkg ->
    match HashMap.get graph.reverse ~key:pkg with
    | Some dependents -> dependents
    | None -> []
end

(* State type - notice NO pending field! *)

type state = {
  solution: Partial_solution.t;
  incompatibilities: (package, Incompatibility.t list) HashMap.t;
  dependency_graph: DependencyGraph.t;
  (* Track which incompatibilities are already contradicted at which decision level.
     We skip these during unit propagation until we backtrack past their level. *)
  contradicted: (Incompatibility.t, int) HashMap.t;
}

(* ============================================================================
   Compute Pending - The Key Innovation
   ============================================================================ *)

(* Compute pending packages based on current solution state and dependencies.
   This is always consistent because it's derived from the current state.
   
   Algorithm:
   1. For each decided/constrained package in solution
   2. Look up its dependencies in the dependency graph
   3. For each dependency that is UNDECIDED, add to pending
   4. Merge ranges if same package appears multiple times
*)

let compute_pending state: (package * version Ranges.t) list =
  Log.debug "compute_pending called";
  let pending_map = HashMap.create () in
  (* Helper to add a package to pending with range merging *)
  let add_to_pending pkg ranges =
    let existing_ranges =
      match HashMap.get pending_map ~key:pkg with
      | Some r -> r
      | None -> Ranges.full
    in
    let new_ranges = Ranges.intersection ~compare_v:version_compare existing_ranges ranges in
    (* Also intersect with any derived constraints from solution *)
    let final_ranges =
      match Partial_solution.get_constraint state.solution pkg with
      | `Constrained derived_ranges ->
          Log.debug ("Package " ^ pkg ^ " has derived constraint, intersecting");
          Ranges.intersection ~compare_v:version_compare new_ranges derived_ranges
      | _ -> new_ranges
    in
    let _ = HashMap.insert pending_map ~key:pkg ~value:final_ranges in
    ()
  in
  (* Get all assignments from solution - we need to expose this in Partial_solution *)
  (* For now, we'll work around by iterating through incompatibilities *)
  (* which contain dependency information *)
  (* Alternative: iterate through incompatibilities to find dependencies *)
  HashMap.for_each state.incompatibilities
    ~fn:(fun pkg _incompats ->
      match Partial_solution.get_constraint state.solution pkg with
      | `Decided ver ->
          (* This package is decided, check its dependencies *)
          let deps = DependencyGraph.get_dependencies state.dependency_graph pkg ver in
          List.for_each deps
            ~fn:(fun (dep_pkg, dep_ranges) ->
              match Partial_solution.get_constraint state.solution dep_pkg with
              | `Undecided -> add_to_pending dep_pkg dep_ranges
              | `Constrained _constrained_ranges ->
                  (* Constrained but not decided - still needs version chosen *)
                  add_to_pending dep_pkg dep_ranges
              | `Decided _ -> ())
      | `Constrained _ranges ->
          (* Constrained packages will be picked up as dependencies of decided packages *)
          ()
      | `Undecided -> ());
  (* Convert HashMap to list *)
  let result = ref [] in
  HashMap.for_each pending_map ~fn:(fun pkg ranges -> result := (pkg, ranges) :: !result);
  (* Sort by constraint score (higher = more constrained = choose first) *)
  (* Strategy: Choose packages with constrained ranges first, but deprioritize *)
  (* packages that are involved in 2-term conflicts with other pending packages *)
  let pending_packages =
    List.map !result ~fn:(fun (pkg, _ranges) -> pkg)
  in
  let score_package ((pkg, ranges)) =
    let num_incompats =
      match HashMap.get state.incompatibilities ~key:pkg with
      | Some incompats ->
          (* Check how many are simple 2-term conflicts with other pending packages *)
          let num_pending_conflicts =
            List.fold_left incompats
              ~acc:0
              ~fn:(fun count incompat ->
                let terms = Incompatibility.terms incompat in
                if List.length terms = 2 then
                  let other_packages =
                    List.filter_map terms
                      ~fn:(fun t ->
                        let t_pkg = Term.package t in
                        if t_pkg != pkg then
                          Some t_pkg
                        else
                          None)
                  in
                  if List.any other_packages ~fn:(fun p -> List.contains pending_packages ~value:p) then
                    count + 1
                  else
                    count
                else
                  count)
          in
          (List.length incompats, num_pending_conflicts)
      | None -> (0, 0)
    in
    let total_incompats, pending_conflicts = num_incompats in
    let is_constrained = not (Ranges.is_empty ranges || ranges = Ranges.full) in
    (* Score: prioritize constrained ranges, then total incompats, *)
    (* but PENALIZE packages involved in pending conflicts (choose them last) *)
    (
      if is_constrained then
        1_000
      else
        0
    ) + total_incompats - (pending_conflicts * 500)
  in
  let sorted =
    List.sort !result
      ~compare:(fun (pkg1, ranges1) (pkg2, ranges2) ->
        let score1 = score_package (pkg1, ranges1) in
        let score2 = score_package (pkg2, ranges2) in
        if score1 = score2 then
          String.compare pkg1 pkg2
        else
          (* Higher score first *)
          Int.compare score2 score1)
  in
  Log.info "📊 Pending packages sorted by score:";
  List.for_each sorted
    ~fn:(fun (pkg, ranges) ->
      Log.info
        ("   "
        ^ pkg
        ^ " (score: "
        ^ Int.to_string (score_package (pkg, ranges))
        ^ ")"));
  sorted

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let add_incompatibility = fun state package incompat ->
  let existing =
    match HashMap.get state.incompatibilities ~key:package with
    | Some incompats -> incompats
    | None -> []
  in
  let _ = HashMap.insert state.incompatibilities ~key:package ~value:(incompat :: existing) in
  ()

(* ============================================================================
   Conflict Resolution
   ============================================================================ *)

(* Returns (package, root_cause_incompat, new_solution) or Error for terminal incompatibility *)

(* This matches Rust's conflict_resolution signature and logic *)

let rec conflict_resolution = fun ~emit root_package root_version state incompat ->
  let rec resolve current_incompat current_incompat_changed =
    if Incompatibility.is_terminal current_incompat root_package root_version then
      Error current_incompat
    else
      let pkg, search_result = Partial_solution.satisfier_search state.solution current_incompat in
      match search_result with
      | `DifferentDecisionLevels previous_level ->
          emit
            (Trace.ConflictResolvedDifferent {
              package = pkg;
              previous_level;
              incompatibility = current_incompat
            });
          Log.info ("🔙 Backtracking to decision level " ^ Int.to_string previous_level);
          (* Backtrack the solution *)
          let new_solution = Partial_solution.backtrack state.solution previous_level in
          (* Clean up contradicted incompatibilities at higher levels *)
          let to_remove = ref [] in
          HashMap.for_each state.contradicted
            ~fn:(fun incompat level ->
              if level > previous_level then
                to_remove := incompat :: !to_remove);
          List.for_each !to_remove
            ~fn:(fun incompat ->
              let _ = HashMap.remove state.contradicted ~key:incompat in
              ());
          (* If incompatibility changed, merge it (like Rust's backtrack does) *)
          if current_incompat_changed then
            List.for_each (Incompatibility.terms current_incompat)
              ~fn:(fun term ->
                let term_pkg = Term.package term in
                add_incompatibility state term_pkg current_incompat);
          let terms = Incompatibility.terms current_incompat in
          Log.info
            ("📚 Learned incompatibility with "
            ^ Int.to_string (List.length terms)
            ^ " terms, backtracked to level "
            ^ Int.to_string previous_level);
          List.for_each terms
            ~fn:(fun t ->
              Log.info
                (
                  "    Term: " ^ (
                    if Term.is_positive t then
                      ""
                    else
                      "NOT "
                  ) ^ Term.package t
                ));
          (* Return the package, root cause, and backtracked solution *)
          Ok (pkg, current_incompat, new_solution)
      | `SameDecisionLevels { cause=satisfier_cause; extra_term } ->
          Log.info ("🔄 Same decision level, computing prior cause for " ^ pkg);
          (* Check if satisfier_cause is the same as current to avoid infinite loop *)
          if Ptr.equal satisfier_cause current_incompat then
            (
              Log.error
                "Satisfier cause same as current incompatibility - treating as \
               terminal";
              Error current_incompat
            )
          else
            let prior = Incompatibility.prior_cause ?extra_term current_incompat satisfier_cause pkg in
            emit
              (Trace.ConflictResolvedSame {
                package = pkg;
                incompatibility = current_incompat;
                cause = satisfier_cause;
                prior
              });
            if equal_incompatibility_terms prior current_incompat then
              (
                Log.error "Prior cause produced the same incompatibility terms - treating as terminal";
                Error current_incompat
              )
            else (
              Log.info "Prior cause computed, continuing resolution";
              (* Continue loop with prior_cause, mark as changed *)
              resolve prior true
            )
  in
  resolve incompat false

(* ============================================================================
   Unit Propagation
   ============================================================================ *)

(* Unit propagation now handles conflict resolution internally *)

(* Returns Ok state on success, Error terminal_incompat on terminal failure *)

let unit_propagation = fun ~emit root_package root_version state changed_packages ->
  Log.info ("🔄 Unit propagation called with packages: " ^ (String.concat ", " changed_packages));
  let rec process_packages = fun state ->
    function
    | [] ->
        Log.info "🔄 Unit propagation complete";
        Ok state
    | pkg :: rest -> (
        Log.info ("🔄 Processing package: " ^ pkg);
        match HashMap.get state.incompatibilities ~key:pkg with
        | None ->
            Log.info ("🔄 No incompatibilities for " ^ pkg ^ ", skipping");
            process_packages state rest
        | Some incompats -> (
            Log.info
              ("  💡 Found "
              ^ (Int.to_string (List.length incompats))
              ^ " incompatibilities for "
              ^ pkg);
            let rec check_incompats = fun state ->
              function
              | [] ->
                  Log.info ("  ✨ All incompats processed for " ^ pkg);
                  Ok state
              | incompat :: remaining -> (
                  let incompat_terms = Incompatibility.terms incompat in
                  let terms_str =
                    String.concat ", "
                      (
                        List.map incompat_terms
                          ~fn:(fun t ->
                            (Term.package t) ^ "@" ^ "ranges" ^ (
                              if Term.is_positive t then
                                ""
                              else
                                "(neg)"
                            ))
                      )
                  in
                  Log.info ("    🔍 Checking incomp [" ^ terms_str ^ "]");
                  let rel = Partial_solution.relation state.solution incompat in
                  (
                    match rel with
                    | `Satisfied -> Log.debug "    Relation: SATISFIED → conflict!"
                    | `AlmostSatisfied _ -> Log.debug "    Relation: ALMOST_SATISFIED"
                    | `Contradicted _ -> Log.debug "    Relation: CONTRADICTED"
                    | `Unknown -> Log.debug "    Relation: UNKNOWN"
                  );
                  match rel with
                  | `Satisfied -> (
                      Log.info "  CONFLICT: Incompatibility satisfied - resolving";
                      (* Handle conflict resolution internally *)
                      match conflict_resolution ~emit root_package root_version state incompat with
                      | Error terminal_incompat ->
                          Log.error "Terminal incompatibility, no solution";
                          Error terminal_incompat
                      | Ok (resolved_pkg, root_cause, backtracked_solution) ->
                          emit
                            (Trace.LearnedIncompatibility {
                              package = resolved_pkg;
                              incompatibility = root_cause
                            });
                          Log.info
                            ("  ✅ Conflict resolved for " ^ resolved_pkg ^ ", adding derivation");
                          let before_constraint = Partial_solution.get_constraint
                            backtracked_solution
                            resolved_pkg in
                          (* Add derivation - it will negate the term from root_cause *)
                          let new_solution = Partial_solution.add_derivation
                            backtracked_solution
                            resolved_pkg
                            root_cause in
                          let after_constraint = Partial_solution.get_constraint new_solution resolved_pkg in
                          (* Add the root_cause to incompatibilities so choose_version can see it *)
                          List.for_each (Incompatibility.terms root_cause)
                            ~fn:(fun term ->
                              let term_pkg = Term.package term in
                              add_incompatibility state term_pkg root_cause);
                          let new_state =
                            if equal_constraint before_constraint after_constraint then
                              state
                            else
                              { state with solution = new_solution }
                          in
                          if equal_constraint before_constraint after_constraint then
                            process_packages new_state rest
                          else (
                            Log.info
                              ("🔄 Continuing unit propagation from learned package " ^ resolved_pkg);
                            process_packages new_state [ resolved_pkg ]
                          )
                    )
                  | `AlmostSatisfied satisfier_pkg -> (
                      Log.info ("  🎯 Almost satisfied, deriving constraint for " ^ satisfier_pkg);
                      Log.info
                        ("  📋 Remaining incompats: "
                        ^ (Int.to_string (List.length remaining))
                        ^ ", remaining packages: "
                        ^ (Int.to_string (List.length rest)));
                      (* RUST: Just add derivation with the incompatibility *)
                      (* add_derivation will negate the term to get the derived ranges *)
                      let before_constraint = Partial_solution.get_constraint state.solution satisfier_pkg in
                      let new_solution = Partial_solution.add_derivation
                        state.solution
                        satisfier_pkg
                        incompat in
                      let after_constraint = Partial_solution.get_constraint new_solution satisfier_pkg in
                      let new_state =
                        if equal_constraint before_constraint after_constraint then
                          state
                        else
                          { state with solution = new_solution }
                      in
                      emit
                        (Trace.DerivedConstraint {
                          package = satisfier_pkg;
                          incompatibility = incompat;
                          changed = not (equal_constraint before_constraint after_constraint)
                        });
                      (* Continue with remaining incompats, then process satisfier_pkg *)
                      Log.info
                        ("  ⏭️  Continuing: satisfier="
                        ^ satisfier_pkg
                        ^ ", rest="
                        ^ (String.concat "," rest));
                      match check_incompats new_state remaining with
                      | Ok state' ->
                          let next_packages =
                            if equal_constraint before_constraint after_constraint then
                              rest
                            else
                              satisfier_pkg :: rest
                          in
                          Log.info
                            ("  ✅ check_incompats OK, processing ["
                            ^ (String.concat "," next_packages)
                            ^ "]");
                          process_packages state' next_packages
                      | Error _ as err ->
                          Log.error "  ❌ check_incompats ERROR";
                          err
                    )
                  | `Contradicted _ ->
                      check_incompats state remaining
                  | `Unknown ->
                      check_incompats state remaining
                )
            in
            (* After checking all incompats for this package, continue with remaining packages *)
            match check_incompats state incompats with
            | Ok state' -> process_packages state' rest
            | Error _ as err -> err
          )
      )
  in
  process_packages state changed_packages

(* ============================================================================
   Version Selection
   ============================================================================ *)

let choose_version = fun provider state pkg ranges ->
  Log.debug ("Choosing version for " ^ pkg);
  (* Get incompatibilities for this package to constrain the search *)
  let incompats =
    match HashMap.get state.incompatibilities ~key:pkg with
    | Some incompats -> incompats
    | None -> []
  in
  Log.info
    ("  💡 Found " ^ (Int.to_string (List.length incompats)) ^ " incompatibilities for " ^ pkg);
  (* Find effective ranges by checking which incompatibilities apply *)
  let effective_ranges = ref ranges in
  List.for_each incompats ~fn:(fun incompat ->
    let terms = Incompatibility.terms incompat in
    Log.debug ("  Checking incompatibility with " ^ Int.to_string (List.length terms) ^ " terms");
    (* Check if all OTHER terms are satisfied *)
    let all_other_satisfied = ref true in
    List.for_each terms ~fn:(fun term ->
      let term_pkg = Term.package term in
      if term_pkg != pkg then (
        let constraint_status = Partial_solution.get_constraint state.solution term_pkg in
        (
          match constraint_status with
          | `Undecided -> Log.info ("      Term pkg=" ^ term_pkg ^ " is Undecided")
          | `Decided v ->
              Log.info ("      Term pkg=" ^ term_pkg ^ " is Decided@" ^ Version.to_string v)
          | `Constrained _ -> Log.info ("      Term pkg=" ^ term_pkg ^ " is Constrained")
        );
        match constraint_status with
        | `Undecided ->
            all_other_satisfied := false
        | `Decided ver ->
            let in_range = Ranges.contains ~compare_v:version_compare (Term.ranges term) ver in
            let term_satisfied =
              (Term.is_positive term && in_range)
              || ((not (Term.is_positive term)) && not in_range)
            in
            if not term_satisfied then
              all_other_satisfied := false
        | `Constrained constrained_ranges ->
            let term_satisfied =
              if Term.is_positive term then
                Ranges.subset_of
                  ~compare_v:version_compare
                  constrained_ranges
                  (Term.ranges term)
              else
                Ranges.is_disjoint
                  ~compare_v:version_compare
                  constrained_ranges
                  (Term.ranges term)
            in
            if not term_satisfied then
              all_other_satisfied := false
      ));
    (* If all other terms satisfied, constrain by this incompatibility *)
    if !all_other_satisfied then (
      Log.info ("    ✨ All other terms satisfied for " ^ pkg ^ "!");
      match Incompatibility.get_term incompat pkg with
      | Some term when Term.is_positive term ->
          Log.info "    ➕ Positive term - EXCLUDE these ranges to avoid conflict";
          let complement_ranges = Ranges.complement ~compare_v:version_compare (Term.ranges term) in
          effective_ranges := Ranges.intersection ~compare_v:version_compare !effective_ranges complement_ranges
      | Some term ->
          Log.info "    ➖ Negative term - INCLUDE only these ranges";
          effective_ranges := Ranges.intersection
            ~compare_v:version_compare
            !effective_ranges
            (Term.ranges term)
      | None ->
          ()
    ));
  Log.debug "  Effective ranges computed";
  (* Ask provider for a version in the effective ranges *)
  match provider.Provider.choose_version pkg !effective_ranges with
  | Ok (Some ver) ->
      Log.debug ("Chose " ^ pkg ^ "@" ^ Version.to_string ver);
      Ok (Some ver, !effective_ranges)
  | Ok None ->
      Log.debug ("No version available for " ^ pkg);
      Ok (None, !effective_ranges)
  | Error err ->
      Log.error ("Error choosing version for " ^ pkg ^ ": " ^ err);
      Error err

(* ============================================================================
   Main Solve Function
   ============================================================================ *)

let solve = fun ?trace_ctx provider root_package root_version ->
  let emit event =
    match trace_ctx with
    | Some ctx -> Trace.record ctx event
    | None -> ()
  in
  Log.debug
    ("Starting NEW PubGrub solver for " ^ root_package ^ "@" ^ Version.to_string root_version);
  (* Initialize state *)
  (* NOTE: Unlike some implementations, we don't add a not_root incompatibility
     because root is already decided. Adding it would cause unit_propagation
     to immediately detect a conflict. *)
  let incompats = HashMap.create () in
  let solution = Partial_solution.empty () in
  let solution = Partial_solution.add_decision solution root_package root_version in
  let initial_state = {
    solution;
    incompatibilities = incompats;
    dependency_graph = DependencyGraph.empty ();
    contradicted = HashMap.create ()
  } in
  (* Get root dependencies *)
  match provider.Provider.get_dependencies root_package root_version with
  | Error err ->
      Log.error ("Failed to get dependencies for root: " ^ err);
      Error err
  | Ok (Provider.Unavailable reason) ->
      Log.error ("Root package unavailable: " ^ reason);
      Error ("Root package unavailable: " ^ reason)
  | Ok (Provider.Available deps) -> (
      (* Add root dependencies to graph *)
      let dep_list =
        List.map deps ~fn:(fun (dep_pkg, dep_ranges) -> (dep_pkg, dep_ranges))
      in
      let dep_graph = DependencyGraph.add_dependencies
        initial_state.dependency_graph
        root_package
        root_version
        dep_list in
      let state = { initial_state with dependency_graph = dep_graph } in
      (* Check for impossible root dependencies *)
      let has_impossible_root_dep = ref None in
      List.for_each deps
        ~fn:(fun (dep_pkg, dep_ranges) ->
          (* Check for self-dependency with incompatible version *)
          if
            dep_pkg = root_package
            && not (Ranges.contains ~compare_v:version_compare dep_ranges root_version)
          then
            (
              Log.info
                ("Impossible root self-dependency: "
                ^ root_package
                ^ "@"
                ^ (Version.to_string root_version)
                ^ " depends on "
                ^ dep_pkg
                ^ " with incompatible range");
              has_impossible_root_dep := Some (dep_pkg, dep_ranges)
            )
          else if Ranges.is_empty dep_ranges then
            (
              Log.info
                ("Impossible root dependency: "
                ^ root_package
                ^ "@"
                ^ (Version.to_string root_version)
                ^ " depends on "
                ^ dep_pkg
                ^ " with empty range");
              has_impossible_root_dep := Some (dep_pkg, dep_ranges)
            ));
      (* If there's an impossible root dependency, fail immediately *)
      match !has_impossible_root_dep with
      | Some (dep_pkg, dep_ranges) ->
          let impossible_incompat = Incompatibility.from_dependency
            root_package
            root_version
            (dep_pkg, dep_ranges) in
          Log.error "Impossible root dependency detected, failing";
          Ok (Failure impossible_incompat)
      | None ->
          (* Add dependency incompatibilities *)
          let dep_packages = ref [] in
          List.for_each deps
            ~fn:(fun (dep_pkg, dep_ranges) ->
              let dep_incompat = Incompatibility.from_dependency
                root_package
                root_version
                (dep_pkg, dep_ranges) in
              add_incompatibility state dep_pkg dep_incompat;
              dep_packages := dep_pkg :: !dep_packages);
          (* Run initial unit propagation on all root dependencies to create derivations *)
          let state =
            match unit_propagation ~emit root_package root_version state !dep_packages with
            | Ok state -> state
            | Error _ -> state
          in
          (* Main solve loop *)
          let rec solve_loop state iteration next_pkg =
            if iteration > 1_000 then
              (
                Log.error "Iteration limit reached after 1000 iterations!";
                Error "Too many iterations - likely infinite loop"
              )
            else (
              emit (Trace.Iteration { iteration; next_package = next_pkg });
              (* Unit propagation on next package *)
              Log.debug
                ("Iteration " ^ (Int.to_string iteration) ^ ": unit propagation on " ^ next_pkg);
              match unit_propagation ~emit root_package root_version state [ next_pkg ] with
              | Error terminal_incompat ->
                  Log.error "Terminal incompatibility, no solution";
                  Ok (Failure terminal_incompat)
              | Ok propagated_state -> (
                  (* Pick next highest priority package *)
                  let prioritizer pkg ranges =
                    let num_incompats =
                      match HashMap.get state.incompatibilities ~key:pkg with
                      | Some incompats -> List.length incompats
                      | None -> 0
                    in
                    let matching_versions =
                      match provider.Provider.count_versions pkg ranges with
                      | Ok n -> n
                      | Error _ -> Int.max_int
                    in
                    let constraint_score =
                      if Ranges.is_empty ranges || ranges = Ranges.full then
                        0
                      else
                        1_000_000
                    in
                    let availability_score =
                      if matching_versions = Int.max_int then
                        0
                      else
                        Int.max 0 (100_000 - matching_versions)
                    in
                    constraint_score + availability_score + num_incompats
                  in
                  match Partial_solution.pick_highest_priority_pkg propagated_state.solution prioritizer with
                  | None ->
                      Log.debug "No more pending packages, solution found";
                      let solution = Partial_solution.extract_solution propagated_state.solution in
                      emit (Trace.Solved { solution });
                      Ok (Success solution)
                  | Some (pkg, ranges) -> (
                      emit (Trace.PickedPackage { package = pkg; ranges });
                      Log.info ("Choosing version for pending package " ^ pkg);
                      (* Try to choose a version for the pending package *)
                      match choose_version provider propagated_state pkg ranges with
                      | Error err ->
                          Error err
                      | Ok (None, effective_ranges) ->
                          emit
                            (Trace.NoVersionAvailable { package = pkg; ranges = effective_ranges });
                          (* No version available, add no_versions incompatibility and continue *)
                          (* This will trigger conflict resolution in the next iteration *)
                          Log.info
                            ("📭 No version available for " ^ pkg ^ ", adding no_versions incompatibility");
                          (* Get constraint from partial solution, or use full if undecided *)
                          let no_ver_incompat = Incompatibility.no_versions pkg effective_ranges in
                          add_incompatibility propagated_state pkg no_ver_incompat;
                          (* Continue loop - unit propagation will run at the top *)
                          solve_loop propagated_state (iteration + 1) pkg
                      | Ok (Some ver, _) -> (
                          emit (Trace.ChoseVersion { package = pkg; version = ver });
                          (* Get dependencies BEFORE adding decision (like Rust) *)
                          match provider.Provider.get_dependencies pkg ver with
                          | Error err ->
                              Error err
                          | Ok (Provider.Unavailable _reason) ->
                              (* Package unavailable, treat as no version *)
                              Log.debug
                                ("Package " ^ pkg ^ "@" ^ Version.to_string ver ^ " unavailable");
                              solve_loop propagated_state (iteration + 1) pkg
                          | Ok (Provider.Available pkg_deps) -> (
                              Log.debug
                                ("Processing "
                                ^ Int.to_string (List.length pkg_deps)
                                ^ " dependencies for "
                                ^ pkg
                                ^ "@"
                                ^ Version.to_string ver);
                              (* Check for impossible dependencies first *)
                              let has_impossible_dep = ref None in
                              List.for_each pkg_deps
                                ~fn:(fun (dep_pkg, dep_ranges) ->
                                  (* Check for self-dependency with incompatible version *)
                                  if
                                    dep_pkg = pkg
                                    && not
                                      (Ranges.contains ~compare_v:version_compare dep_ranges ver)
                                  then
                                    (
                                      Log.info
                                        ("Impossible self-dependency: "
                                        ^ pkg
                                        ^ "@"
                                        ^ (Version.to_string ver)
                                        ^ " depends on "
                                        ^ dep_pkg
                                        ^ " with incompatible range");
                                      has_impossible_dep := Some (dep_pkg, dep_ranges)
                                    )
                                  else if Ranges.is_empty dep_ranges then
                                    (
                                      Log.info
                                        ("Impossible dependency: "
                                        ^ pkg
                                        ^ "@"
                                        ^ (Version.to_string ver)
                                        ^ " depends on "
                                        ^ dep_pkg
                                        ^ " with empty range");
                                      has_impossible_dep := Some (dep_pkg, dep_ranges)
                                    ));
                              (* If impossible dependency, fail *)
                              match !has_impossible_dep with
                              | Some (dep_pkg, dep_ranges) ->
                                  let impossible_incompat = Incompatibility.from_dependency
                                    pkg
                                    ver
                                    (dep_pkg, dep_ranges) in
                                  Log.error "Impossible dependency detected, failing";
                                  Ok (Failure impossible_incompat)
                              | None -> (
                                  (* PORT OF RUST: Just add decision and incompatibilities *)
                                  Log.info
                                    ("✅ Adding decision: " ^ pkg ^ "@" ^ (Version.to_string ver));
                                  let new_solution = Partial_solution.add_decision
                                    propagated_state.solution
                                    pkg
                                    ver in
                                  let new_state = { propagated_state with solution = new_solution } in
                                  (* Add dependencies to graph *)
                                  let dep_list =
                                    List.map pkg_deps ~fn:(fun (dep_pkg, dep_ranges) -> (dep_pkg, dep_ranges))
                                  in
                                  let new_dep_graph = DependencyGraph.add_dependencies
                                    new_state.dependency_graph
                                    pkg
                                    ver
                                    dep_list in
                                  let new_state = { new_state with dependency_graph = new_dep_graph } in
                                  (* Add dependency incompatibilities and collect affected packages *)
                                  let affected_packages = ref [] in
                                  List.for_each pkg_deps
                                    ~fn:(fun (dep_pkg, dep_ranges) ->
                                      let dep_incompat = Incompatibility.from_dependency
                                        pkg
                                        ver
                                        (dep_pkg, dep_ranges) in
                                      Log.info
                                        ("📦 Added dependency incompatibility for " ^ dep_pkg);
                                      add_incompatibility new_state dep_pkg dep_incompat;
                                      affected_packages := dep_pkg :: !affected_packages);
                                  Log.info
                                    ("🔄 Added "
                                    ^ (Int.to_string (List.length !affected_packages))
                                    ^ " dependency incompatibilities: "
                                    ^ (String.concat ", " !affected_packages));
                                  (* Run unit propagation on affected packages *)
                                  match unit_propagation ~emit root_package root_version new_state !affected_packages with
                                  | Error terminal_incompat ->
                                      Log.error
                                        "Terminal incompatibility after adding \
                                         dependencies";
                                      Ok (Failure terminal_incompat)
                                  | Ok propagated_state ->
                                      (* Pick first affected package as next (or pkg if no deps) *)
                                      let next_to_check =
                                        match !affected_packages with
                                        | first :: _ -> first
                                        | [] -> pkg
                                      in
                                      solve_loop propagated_state (iteration + 1) next_to_check
                                )
                            )
                        )
                    )
                )
            )
          in
          (* Start loop with first dependency package, or root if no deps *)
          let initial_next =
            match deps with
            | (first_dep, _ranges) :: _ -> first_dep
            | [] -> root_package
          in
          solve_loop state 0 initial_next
    )
