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

let version_compare = Version.compare

let equal_ranges = fun left right ->
  Ranges.subset_of ~compare_v:version_compare left right
  && Ranges.subset_of ~compare_v:version_compare right left

let equal_constraint = fun left right ->
  match (left, right) with
  | `Undecided, `Undecided -> true
  | `Decided left, `Decided right -> version_compare left right = Order.EQ
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
    | candidate :: tail when equal_term term candidate -> Some (List.append (List.reverse acc) tail)
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

type options = {
  max_iterations: int;
}

type stats = {
  iterations: int;
  decisions: int;
  derivations: int;
  conflicts: int;
  learned_incompatibilities: int;
  backtracks: int;
  provider_choose_version_calls: int;
  provider_count_versions_calls: int;
  provider_get_dependencies_calls: int;
  provider_calls: int;
  max_decision_depth: int;
}

type outcome = {
  result: (solve_result, string) result;
  stats: stats;
}

type stats_acc = {
  mutable iterations: int;
  mutable decisions: int;
  mutable derivations: int;
  mutable conflicts: int;
  mutable learned_incompatibilities: int;
  mutable backtracks: int;
  mutable provider_choose_version_calls: int;
  mutable provider_count_versions_calls: int;
  mutable provider_get_dependencies_calls: int;
  mutable max_decision_depth: int;
}

let default_options = { max_iterations = 1_000 }

let empty_stats = fun () ->
  {
    iterations = 0;
    decisions = 0;
    derivations = 0;
    conflicts = 0;
    learned_incompatibilities = 0;
    backtracks = 0;
    provider_choose_version_calls = 0;
    provider_count_versions_calls = 0;
    provider_get_dependencies_calls = 0;
    max_decision_depth = 0;
  }

let snapshot_stats = fun stats ->
  let provider_calls = stats.provider_choose_version_calls
  + stats.provider_count_versions_calls
  + stats.provider_get_dependencies_calls in
  {
    iterations = stats.iterations;
    decisions = stats.decisions;
    derivations = stats.derivations;
    conflicts = stats.conflicts;
    learned_incompatibilities = stats.learned_incompatibilities;
    backtracks = stats.backtracks;
    provider_choose_version_calls = stats.provider_choose_version_calls;
    provider_count_versions_calls = stats.provider_count_versions_calls;
    provider_get_dependencies_calls = stats.provider_get_dependencies_calls;
    provider_calls;
    max_decision_depth = stats.max_decision_depth;
  }

let record_decision = fun stats solution ->
  stats.decisions <- stats.decisions + 1;
  stats.max_decision_depth <- Int.max
    stats.max_decision_depth
    (Partial_solution.current_decision_level solution)

let record_derivation = fun stats -> stats.derivations <- stats.derivations + 1

let provider_get_dependencies = fun stats provider pkg ver ->
  stats.provider_get_dependencies_calls <- stats.provider_get_dependencies_calls + 1;
  provider.Provider.get_dependencies pkg ver

let provider_count_versions = fun stats provider pkg ranges ->
  stats.provider_count_versions_calls <- stats.provider_count_versions_calls + 1;
  provider.Provider.count_versions pkg ranges

let provider_choose_version = fun stats provider pkg ranges ->
  stats.provider_choose_version_calls <- stats.provider_choose_version_calls + 1;
  provider.Provider.choose_version pkg ranges

type incompatibility_store = {
  arena: (int, Incompatibility.t) HashMap.t;
  ids_by_key: (string, int) HashMap.t;
  by_package: (package, int list) HashMap.t;
  mutable next_id: int;
}

let create_incompatibility_store = fun () ->
  {
    arena = HashMap.create ();
    ids_by_key = HashMap.create ();
    by_package = HashMap.create ();
    next_id = 0
  }

let incompatibility_key = fun incompat ->
  let render_term term =
    let sign =
      if Term.is_positive term then
        "+"
      else
        "-"
    in
    sign
    ^ Term.package term
    ^ ":"
    ^ Ranges.to_string ~to_string_v:Version.to_string (Term.ranges term)
  in
  Incompatibility.terms incompat
  |> List.map ~fn:render_term
  |> List.sort ~compare:String.compare
  |> String.concat "|"

let get_incompatibility_ids = fun store pkg ->
  match HashMap.get store.by_package ~key:pkg with
  | Some ids -> ids
  | None -> []

let get_incompatibility = fun store id ->
  match HashMap.get store.arena ~key:id with
  | Some incompat -> incompat
  | None -> panic ("Missing incompatibility id " ^ Int.to_string id)

let get_incompatibilities = fun store pkg ->
  List.sort (List.map (get_incompatibility_ids store pkg) ~fn:(get_incompatibility store))
    ~compare:(fun left right ->
      String.compare (incompatibility_key left) (incompatibility_key right))

let add_incompatibility_to_store = fun store package incompat ->
  let key = incompatibility_key incompat in
  let id =
    match HashMap.get store.ids_by_key ~key with
    | Some id -> id
    | None ->
        let id = store.next_id in
        store.next_id <- id + 1;
        let _ = HashMap.insert store.arena ~key:id ~value:incompat in
        let _ = HashMap.insert store.ids_by_key ~key ~value:id in
        id
  in
  let existing = get_incompatibility_ids store package in
  if not (List.contains existing ~value:id) then
    let _ = HashMap.insert store.by_package ~key:package ~value:(id :: existing) in
    ()

type state = {
  solution: Partial_solution.t;
  incompatibilities: incompatibility_store;
}

let add_incompatibility = fun state package incompat ->
  add_incompatibility_to_store state.incompatibilities package incompat

let normalize_packages = fun packages ->
  List.unique (List.sort packages ~compare:String.compare) ~compare:String.compare

let compare_dependency = fun (left_pkg, left_ranges) (right_pkg, right_ranges) ->
  let pkg_compare = String.compare left_pkg right_pkg in
  if pkg_compare != Order.EQ then
    pkg_compare
  else
    String.compare
      (Ranges.to_string ~to_string_v:Version.to_string left_ranges)
      (Ranges.to_string ~to_string_v:Version.to_string right_ranges)

(* ============================================================================
   Conflict Resolution
   ============================================================================ *)

(* Returns (package, root_cause_incompat, new_solution) or Error for terminal incompatibility *)

(* This matches Rust's conflict_resolution signature and logic *)

let rec conflict_resolution = fun ~stats ~emit root_package root_version state incompat ->
  let rec resolve current_incompat current_incompat_changed =
    if Incompatibility.is_terminal current_incompat root_package root_version then
      Error current_incompat
    else
      let pkg, search_result = Partial_solution.satisfier_search state.solution current_incompat in
      match search_result with
      | `DifferentDecisionLevels previous_level ->
          stats.backtracks <- stats.backtracks + 1;
          emit
            (Trace.ConflictResolvedDifferent {
              package = pkg;
              previous_level;
              incompatibility = current_incompat
            });
          Log.info ("🔙 Backtracking to decision level " ^ Int.to_string previous_level);
          (* Backtrack the solution *)
          let new_solution = Partial_solution.backtrack state.solution previous_level in
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

let unit_propagation = fun ~stats ~emit root_package root_version state changed_packages ->
  let changed_packages = normalize_packages changed_packages in
  Log.info ("🔄 Unit propagation called with packages: " ^ (String.concat ", " changed_packages));
  let rec process_packages = fun state ->
    function
    | [] ->
        Log.info "🔄 Unit propagation complete";
        Ok state
    | pkg :: rest -> (
        Log.info ("🔄 Processing package: " ^ pkg);
        let incompats = get_incompatibilities state.incompatibilities pkg in
        match incompats with
        | [] ->
            Log.info ("🔄 No incompatibilities for " ^ pkg ^ ", skipping");
            process_packages state rest
        | _ -> (
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
                      stats.conflicts <- stats.conflicts + 1;
                      Log.info "  CONFLICT: Incompatibility satisfied - resolving";
                      (* Handle conflict resolution internally *)
                      match conflict_resolution ~stats ~emit root_package root_version state incompat with
                      | Error terminal_incompat ->
                          Log.error "Terminal incompatibility, no solution";
                          Error terminal_incompat
                      | Ok (resolved_pkg, root_cause, backtracked_solution) ->
                          stats.learned_incompatibilities <- stats.learned_incompatibilities + 1;
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
                          if not (equal_constraint before_constraint after_constraint) then
                            record_derivation stats;
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
                      if not (equal_constraint before_constraint after_constraint) then
                        record_derivation stats;
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

let choose_version = fun stats provider state pkg ranges ->
  Log.debug ("Choosing version for " ^ pkg);
  (* Get incompatibilities for this package to constrain the search *)
  let incompats = get_incompatibilities state.incompatibilities pkg in
  Log.info
    ("  💡 Found " ^ (Int.to_string (List.length incompats)) ^ " incompatibilities for " ^ pkg);
  (* Find effective ranges by checking which incompatibilities apply *)
  let effective_ranges = ref ranges in
  let term_satisfying_ranges term =
    if Term.is_positive term then
      Term.ranges term
    else
      Ranges.complement ~compare_v:version_compare (Term.ranges term)
  in
  List.for_each incompats
    ~fn:(fun incompat ->
      let terms = Incompatibility.terms incompat in
      Log.debug ("  Checking incompatibility with " ^ Int.to_string (List.length terms) ^ " terms");
      (* Check if all OTHER terms are satisfied *)
      let all_other_satisfied = ref true in
      List.for_each terms
        ~fn:(fun term ->
          let term_pkg = Term.package term in
          if not (String.equal term_pkg pkg) then
            (
              let constraint_status = Partial_solution.get_constraint state.solution term_pkg in
              (
                match constraint_status with
                | `Undecided -> Log.info ("      Term pkg=" ^ term_pkg ^ " is Undecided")
                | `Decided v -> Log.info
                  ("      Term pkg=" ^ term_pkg ^ " is Decided@" ^ Version.to_string v)
                | `Constrained _ -> Log.info ("      Term pkg=" ^ term_pkg ^ " is Constrained")
              );
              match constraint_status with
              | `Undecided ->
                  all_other_satisfied := false
              | `Decided ver ->
                  let in_range = Ranges.contains ~compare_v:version_compare (Term.ranges term) ver in
                  let term_satisfied =
                    (Term.is_positive term && in_range)
                    || ((not (Term.is_positive term)) && not in_range) in
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
      if !all_other_satisfied then
        (
          Log.info ("    ✨ All other terms satisfied for " ^ pkg ^ "!");
          let pkg_terms =
            List.filter terms
              ~fn:(fun term ->
                String.equal (Term.package term) pkg)
          in
          match pkg_terms with
          | [] -> ()
          | term :: rest ->
              let conflict_ranges =
                List.fold_left
                  rest
                  ~init:(term_satisfying_ranges term)
                  ~fn:(fun acc term ->
                    Ranges.intersection ~compare_v:version_compare acc (term_satisfying_ranges term))
              in
              let allowed_ranges = Ranges.complement ~compare_v:version_compare conflict_ranges in
              effective_ranges := Ranges.intersection ~compare_v:version_compare !effective_ranges allowed_ranges
        ));
  Log.debug "  Effective ranges computed";
  (* Ask provider for a version in the effective ranges *)
  match provider_choose_version stats provider pkg !effective_ranges with
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

let solve_with_stats = fun ?trace_ctx ?(options = default_options) provider root_package root_version ->
  let stats = empty_stats () in
  let finish result = { result; stats = snapshot_stats stats } in
  let max_iterations = Int.max 0 options.max_iterations in
  let emit event =
    match trace_ctx with
    | Some ctx -> Trace.record ctx event
    | None -> ()
  in
  Log.debug ("Starting PubGrub solver for " ^ root_package ^ "@" ^ Version.to_string root_version);
  (* Initialize state *)
  (* NOTE: Unlike some implementations, we don't add a not_root incompatibility
     because root is already decided. Adding it would cause unit_propagation
     to immediately detect a conflict. *)
  let incompats = create_incompatibility_store () in
  let solution = Partial_solution.empty () in
  let solution = Partial_solution.add_decision solution root_package root_version in
  record_decision stats solution;
  let initial_state = { solution; incompatibilities = incompats } in
  (* Get root dependencies *)
  match provider_get_dependencies stats provider root_package root_version with
  | Error err ->
      Log.error ("Failed to get dependencies for root: " ^ err);
      finish (Error err)
  | Ok (Provider.Unavailable reason) ->
      Log.error ("Root package unavailable: " ^ reason);
      finish (Error ("Root package unavailable: " ^ reason))
  | Ok (Provider.Available deps) -> (
      let state = initial_state in
      (* Check for impossible root dependencies *)
      let impossible_root_deps = ref [] in
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
              impossible_root_deps := (dep_pkg, dep_ranges) :: !impossible_root_deps
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
              impossible_root_deps := (dep_pkg, dep_ranges) :: !impossible_root_deps
            ));
      (* If there's an impossible root dependency, fail immediately *)
      match List.sort !impossible_root_deps ~compare:compare_dependency with
      | (dep_pkg, dep_ranges) :: _ ->
          let impossible_incompat = Incompatibility.from_dependency
            root_package
            root_version
            (dep_pkg, dep_ranges) in
          Log.error "Impossible root dependency detected, failing";
          finish (Ok (Failure impossible_incompat))
      | [] ->
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
          let dep_packages = normalize_packages !dep_packages in
          (* Run initial unit propagation on all root dependencies to create derivations *)
          match unit_propagation ~stats ~emit root_package root_version state dep_packages with
          | Error terminal_incompat ->
              Log.error "Terminal incompatibility during initial propagation";
              finish (Ok (Failure terminal_incompat))
          | Ok state ->
              (* Main solve loop *)
              let rec solve_loop state iteration next_pkg =
                if iteration > max_iterations then
                  (
                    Log.error
                      ("Iteration limit reached after " ^ Int.to_string max_iterations ^ " iterations!");
                    finish (Error "Too many iterations - likely infinite loop")
                  )
                else (
                  stats.iterations <- stats.iterations + 1;
                  emit (Trace.Iteration { iteration; next_package = next_pkg });
                  (* Unit propagation on next package *)
                  Log.debug
                    ("Iteration " ^ (Int.to_string iteration) ^ ": unit propagation on " ^ next_pkg);
                  match unit_propagation ~stats ~emit root_package root_version state [ next_pkg ] with
                  | Error terminal_incompat ->
                      Log.error "Terminal incompatibility, no solution";
                      finish (Ok (Failure terminal_incompat))
                  | Ok propagated_state -> (
                      (* Pick next highest priority package *)
                      let prioritizer pkg ranges =
                        let num_incompats = List.length
                          (get_incompatibility_ids propagated_state.incompatibilities pkg) in
                        let matching_versions =
                          match provider_count_versions stats provider pkg ranges with
                          | Ok n -> n
                          | Error _ -> Int.max_int
                        in
                        let constraint_score =
                          if
                            Ranges.is_empty ranges
                            || Ranges.equal ~compare_v:version_compare ranges Ranges.full
                          then
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
                          finish (Ok (Success solution))
                      | Some (pkg, ranges) -> (
                          emit (Trace.PickedPackage { package = pkg; ranges });
                          Log.info ("Choosing version for pending package " ^ pkg);
                          (* Try to choose a version for the pending package *)
                          match choose_version stats provider propagated_state pkg ranges with
                          | Error err ->
                              finish (Error err)
                          | Ok (None, effective_ranges) ->
                              let unavailable_ranges =
                                if Ranges.is_empty effective_ranges && not (Ranges.is_empty ranges) then
                                  ranges
                                else
                                  effective_ranges
                              in
                              emit
                                (Trace.NoVersionAvailable {
                                  package = pkg;
                                  ranges = unavailable_ranges
                                });
                              (* No version available, add no_versions incompatibility and continue *)
                              (* This will trigger conflict resolution in the next iteration *)
                              Log.info
                                ("📭 No version available for " ^ pkg ^ ", adding no_versions incompatibility");
                              let no_ver_incompat = Incompatibility.no_versions pkg unavailable_ranges in
                              add_incompatibility propagated_state pkg no_ver_incompat;
                              solve_loop propagated_state (iteration + 1) pkg
                          | Ok (Some ver, _) -> (
                              emit (Trace.ChoseVersion { package = pkg; version = ver });
                              (* Get dependencies BEFORE adding decision (like Rust) *)
                              match provider_get_dependencies stats provider pkg ver with
                              | Error err ->
                                  finish (Error err)
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
                                  let impossible_deps = ref [] in
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
                                          impossible_deps := (dep_pkg, dep_ranges) :: !impossible_deps
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
                                          impossible_deps := (dep_pkg, dep_ranges) :: !impossible_deps
                                        ));
                                  (* If the chosen version is impossible, learn that and retry. *)
                                  match List.sort !impossible_deps ~compare:compare_dependency with
                                  | (dep_pkg, dep_ranges) :: _ ->
                                      let impossible_incompat = Incompatibility.from_dependency
                                        pkg
                                        ver
                                        (dep_pkg, dep_ranges) in
                                      Log.info
                                        ("Impossible dependency for "
                                        ^ pkg
                                        ^ "@"
                                        ^ Version.to_string ver
                                        ^ ", learning incompatibility and retrying");
                                      add_incompatibility propagated_state pkg impossible_incompat;
                                      solve_loop propagated_state (iteration + 1) pkg
                                  | [] -> (
                                      (* PORT OF RUST: Just add decision and incompatibilities *)
                                      Log.info
                                        ("✅ Adding decision: "
                                        ^ pkg
                                        ^ "@"
                                        ^ (Version.to_string ver));
                                      let new_solution = Partial_solution.add_decision
                                        propagated_state.solution
                                        pkg
                                        ver in
                                      record_decision stats new_solution;
                                      let new_state = {
                                        propagated_state
                                        with solution = new_solution
                                      } in
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
                                      let affected_packages = normalize_packages !affected_packages in
                                      Log.info
                                        ("🔄 Added "
                                        ^ (Int.to_string (List.length affected_packages))
                                        ^ " dependency incompatibilities: "
                                        ^ (String.concat ", " affected_packages));
                                      (* Run unit propagation on affected packages *)
                                      match unit_propagation
                                        ~stats
                                        ~emit
                                        root_package
                                        root_version
                                        new_state
                                        affected_packages with
                                      | Error terminal_incompat ->
                                          Log.error
                                            "Terminal incompatibility after adding \
                                             dependencies";
                                          finish (Ok (Failure terminal_incompat))
                                      | Ok propagated_state ->
                                          (* Pick first affected package as next (or pkg if no deps) *)
                                          let next_to_check =
                                            match affected_packages with
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
                match dep_packages with
                | first_dep :: _ -> first_dep
                | [] -> root_package
              in
              solve_loop state 0 initial_next
    )

let solve = fun ?trace_ctx ?(options = default_options) provider root_package root_version ->
  let outcome = solve_with_stats ?trace_ctx ~options provider root_package root_version in
  outcome.result
