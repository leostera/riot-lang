open Std
open Std.Collections

type package = string
type version = Version.t

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

(* ============================================================================
   Core Data Structures
   ============================================================================ *)

(* Dependency information: which packages depend on what *)
type dependency = {
  dependent : package; (* The package that has this dependency *)
  dependent_version : version; (* At which version *)
  dependency : package; (* The package it depends on *)
  ranges : version Ranges.t; (* The version range required *)
  decision_level : int; (* When this dependency was added *)
}

(* The dependency graph tracks all dependencies explicitly *)
module DependencyGraph = struct
  type t = {
    (* Map from (package, version) to its list of dependencies *)
    deps : (package * version, (package * version Ranges.t) list) HashMap.t;
    (* Map from package to which packages depend on it (reverse index) *)
    reverse : (package, (package * version) list) HashMap.t;
  }

  let empty () = { deps = HashMap.create (); reverse = HashMap.create () }

  let add_dependencies graph pkg ver deps =
    (* Store forward mapping: (pkg, ver) -> deps *)
    ignore (HashMap.insert graph.deps (pkg, ver) deps);

    (* Store reverse mapping: dep_pkg -> [(pkg, ver), ...] *)
    List.iter
      (fun (dep_pkg, _ranges) ->
        let existing =
          match HashMap.get graph.reverse dep_pkg with
          | Some l -> l
          | None -> []
        in
        if not (List.mem (pkg, ver) existing) then
          ignore (HashMap.insert graph.reverse dep_pkg ((pkg, ver) :: existing)))
      deps;

    graph

  let get_dependencies graph pkg ver =
    match HashMap.get graph.deps (pkg, ver) with
    | Some deps -> deps
    | None -> []

  let get_dependents graph pkg =
    match HashMap.get graph.reverse pkg with
    | Some dependents -> dependents
    | None -> []
end

(* State type - notice NO pending field! *)
type state = {
  solution : Partial_solution.t;
  incompatibilities : (package, Incompatibility.t list) HashMap.t;
  dependency_graph : DependencyGraph.t;
  (* Track which incompatibilities are already contradicted at which decision level.
     We skip these during unit propagation until we backtrack past their level. *)
  contradicted : (Incompatibility.t, int) HashMap.t;
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
let compute_pending state : (package * version Ranges.t) list =
  Log.debug "compute_pending called";
  let pending_map = HashMap.create () in

  (* Helper to add a package to pending with range merging *)
  let add_to_pending pkg ranges =
    let existing_ranges =
      match HashMap.get pending_map pkg with Some r -> r | None -> Ranges.full
    in
    let new_ranges =
      Ranges.intersection ~compare_v:version_compare existing_ranges ranges
    in
    (* Also intersect with any derived constraints from solution *)
    let final_ranges =
      match Partial_solution.get_constraint state.solution pkg with
      | `Constrained derived_ranges ->
          Log.debug "Package %s has derived constraint, intersecting" pkg;
          Ranges.intersection ~compare_v:version_compare new_ranges
            derived_ranges
      | _ -> new_ranges
    in
    ignore (HashMap.insert pending_map pkg final_ranges)
  in

  (* Iterate through all decided packages and collect their undecided dependencies *)
  (* We need to iterate through all assignments in the solution *)
  let rec collect_from_assignments = function
    | [] -> ()
    | assignment :: rest ->
        (match assignment with
        | Partial_solution.Decision (pkg, ver, _level, _gidx) ->
            (* Get dependencies for this decided package *)
            let deps =
              DependencyGraph.get_dependencies state.dependency_graph pkg ver
            in
            List.iter
              (fun (dep_pkg, dep_ranges) ->
                (* Check if dependency is undecided *)
                match
                  Partial_solution.get_constraint state.solution dep_pkg
                with
                | `Undecided -> add_to_pending dep_pkg dep_ranges
                | `Decided _ | `Constrained _ -> () (* Already handled *))
              deps
        | Partial_solution.Derivation (pkg, _ranges, _cause, _level, _gidx) ->
            (* Derivations don't have dependencies in our model *)
            (* Only decisions correspond to actual package versions with deps *)
            ());
        collect_from_assignments rest
  in

  (* Get all assignments from solution - we need to expose this in Partial_solution *)
  (* For now, we'll work around by iterating through incompatibilities *)
  (* which contain dependency information *)

  (* Alternative: iterate through incompatibilities to find dependencies *)
  HashMap.iter
    (fun pkg incompats ->
      match Partial_solution.get_constraint state.solution pkg with
      | `Decided ver ->
          (* This package is decided, check its dependencies *)
          let deps =
            DependencyGraph.get_dependencies state.dependency_graph pkg ver
          in
          List.iter
            (fun (dep_pkg, dep_ranges) ->
              match Partial_solution.get_constraint state.solution dep_pkg with
              | `Undecided -> add_to_pending dep_pkg dep_ranges
              | `Constrained constrained_ranges ->
                  (* Constrained but not decided - still needs version chosen *)
                  add_to_pending dep_pkg dep_ranges
              | `Decided _ -> ())
            deps
      | `Constrained _ranges ->
          (* Constrained packages will be picked up as dependencies of decided packages *)
          ()
      | `Undecided -> ())
    state.incompatibilities;

  (* Convert HashMap to list *)
  let result = ref [] in
  HashMap.iter
    (fun pkg ranges -> result := (pkg, ranges) :: !result)
    pending_map;

  (* Sort by constraint score (higher = more constrained = choose first) *)
  (* Strategy: Choose packages with constrained ranges first, but deprioritize *)
  (* packages that are involved in 2-term conflicts with other pending packages *)
  let pending_packages = List.map fst !result in

  let score_package (pkg, ranges) =
    let num_incompats =
      match HashMap.get state.incompatibilities pkg with
      | Some incompats ->
          (* Check how many are simple 2-term conflicts with other pending packages *)
          let num_pending_conflicts =
            List.fold_left
              (fun count incompat ->
                let terms = Incompatibility.terms incompat in
                if List.length terms = 2 then
                  (* Check if the other term references a pending package *)
                  let other_packages =
                    List.filter_map
                      (fun t ->
                        let t_pkg = Term.package t in
                        if t_pkg <> pkg then Some t_pkg else None)
                      terms
                  in
                  if
                    List.exists
                      (fun p -> List.mem p pending_packages)
                      other_packages
                  then count + 1
                  else count
                else count)
              0 incompats
          in
          (List.length incompats, num_pending_conflicts)
      | None -> (0, 0)
    in
    let total_incompats, pending_conflicts = num_incompats in
    let is_constrained = not (Ranges.is_empty ranges || ranges = Ranges.full) in
    (* Score: prioritize constrained ranges, then total incompats, *)
    (* but PENALIZE packages involved in pending conflicts (choose them last) *)
    (if is_constrained then 1000 else 0)
    + total_incompats - (pending_conflicts * 500)
  in

  let sorted =
    List.sort
      (fun (pkg1, _r1) (pkg2, _r2) ->
        let score1 = score_package (pkg1, _r1) in
        let score2 = score_package (pkg2, _r2) in
        if score1 = score2 then
          (* Tie-breaker: alphabetical order (prefer earlier) *)
          String.compare pkg1 pkg2
        else
          (* Higher score first *)
          compare score2 score1)
      !result
  in
  Log.info "📊 Pending packages sorted by score:";
  List.iter
    (fun (pkg, _) ->
      Log.info "   %s (score: %d)" pkg
        (score_package (pkg, snd (List.find (fun (p, _) -> p = pkg) !result))))
    sorted;
  sorted

(* ============================================================================
   Helper Functions
   ============================================================================ *)

let add_incompatibility state package incompat =
  let existing =
    match HashMap.get state.incompatibilities package with
    | Some incompats -> incompats
    | None -> []
  in
  ignore (HashMap.insert state.incompatibilities package (incompat :: existing))

(* ============================================================================
   Conflict Resolution
   ============================================================================ *)

(* Returns (package, root_cause_incompat, new_solution) or Error for terminal incompatibility *)
(* This matches Rust's conflict_resolution signature and logic *)
let rec conflict_resolution root_package root_version state incompat =
  let rec resolve current_incompat current_incompat_changed =
    if Incompatibility.is_terminal current_incompat root_package root_version
    then Error current_incompat
    else
      let pkg, search_result =
        Partial_solution.satisfier_search state.solution current_incompat
      in
      match search_result with
      | `DifferentDecisionLevels previous_level ->
          Log.info "🔙 Backtracking to decision level %d" previous_level;
          (* Backtrack the solution *)
          let new_solution =
            Partial_solution.backtrack state.solution previous_level
          in

          (* Clean up contradicted incompatibilities at higher levels *)
          let to_remove = ref [] in
          HashMap.iter
            (fun incompat level ->
              if level > previous_level then to_remove := incompat :: !to_remove)
            state.contradicted;
          List.iter
            (fun incompat ->
              ignore (HashMap.remove state.contradicted incompat))
            !to_remove;

          (* If incompatibility changed, merge it (like Rust's backtrack does) *)
          if current_incompat_changed then
            List.iter
              (fun term ->
                let term_pkg = Term.package term in
                add_incompatibility state term_pkg current_incompat)
              (Incompatibility.terms current_incompat);

          Log.info "📚 Learned incompatibility, backtracked to level %d"
            previous_level;
          (* Return the package, root cause, and backtracked solution *)
          Ok (pkg, current_incompat, new_solution)
      | `SameDecisionLevels satisfier_cause ->
          Log.info "🔄 Same decision level, computing prior cause for %s" pkg;
          (* Check if satisfier_cause is the same as current to avoid infinite loop *)
          if satisfier_cause == current_incompat then (
            Log.error
              "Satisfier cause same as current incompatibility - treating as \
               terminal";
            Error current_incompat)
          else
            let prior =
              Incompatibility.prior_cause current_incompat satisfier_cause pkg
            in
            Log.info "Prior cause computed, continuing resolution";
            (* Continue loop with prior_cause, mark as changed *)
            resolve prior true
  in
  resolve incompat false

(* ============================================================================
   Unit Propagation
   ============================================================================ *)

(* Unit propagation now handles conflict resolution internally *)
(* Returns Ok state on success, Error terminal_incompat on terminal failure *)
let unit_propagation root_package root_version state changed_packages =
  Log.info "🔄 Unit propagation called with packages: %s"
    (String.concat ", " changed_packages);
  let rec process_packages state = function
    | [] ->
        Log.info "🔄 Unit propagation complete";
        Ok state
    | pkg :: rest -> (
        Log.info "🔄 Processing package: %s" pkg;
        match HashMap.get state.incompatibilities pkg with
        | None ->
            Log.info "🔄 No incompatibilities for %s, skipping" pkg;
            process_packages state rest
        | Some incompats ->
            Log.info "  💡 Found %d incompatibilities for %s"
              (List.length incompats) pkg;
            let rec check_incompats state = function
              | [] ->
                  Log.info "  ✨ All incompats processed for %s" pkg;
                  Ok state
              | incompat :: remaining -> (
                  let incompat_terms = Incompatibility.terms incompat in
                  Log.info "    🔍 Checking incomp with %d terms" (List.length incompat_terms);
                  (* Skip incompatibilities that are already contradicted *)
                  match HashMap.get state.contradicted incompat with
                  | Some _ ->
                      Log.info "    ⏭️  Skipping contradicted incompatibility";
                      check_incompats state remaining
                  | None -> (
                      let rel =
                        Partial_solution.relation state.solution incompat
                      in
                      (match rel with
                      | `Satisfied ->
                          Log.debug "    Relation: SATISFIED → conflict!"
                      | `AlmostSatisfied _ ->
                          Log.debug "    Relation: ALMOST_SATISFIED"
                      | `Contradicted _ ->
                          Log.debug "    Relation: CONTRADICTED"
                      | `Unknown -> Log.debug "    Relation: UNKNOWN");
                      match rel with
                      | `Satisfied -> (
                          Log.info
                            "  CONFLICT: Incompatibility satisfied - resolving";
                          (* Handle conflict resolution internally *)
                          match
                            conflict_resolution root_package root_version state
                              incompat
                          with
                          | Error terminal_incompat ->
                              Log.error "Terminal incompatibility, no solution";
                              Error terminal_incompat
                          | Ok (resolved_pkg, root_cause, backtracked_solution)
                            ->
                              Log.info
                                "  ✅ Conflict resolved for %s, adding \
                                 derivation"
                                resolved_pkg;
                              (* Add derivation - it will negate the term from root_cause *)
                              let new_solution =
                                Partial_solution.add_derivation
                                  backtracked_solution resolved_pkg root_cause
                              in
                              (* Mark the root cause as contradicted at current decision level *)
                              let current_level =
                                Partial_solution.current_decision_level
                                  new_solution
                              in
                              ignore
                                (HashMap.insert state.contradicted root_cause
                                   current_level);
                              let new_state =
                                { state with solution = new_solution }
                              in
                              (* Continue processing this package since we added a derivation *)
                              process_packages new_state (resolved_pkg :: rest))
                       | `AlmostSatisfied satisfier_pkg -> (
                          Log.info
                            "  🎯 Almost satisfied, deriving constraint for %s"
                            satisfier_pkg;
                          Log.info "  📋 Remaining incompats: %d, remaining packages: %d"
                            (List.length remaining) (List.length rest);
                          (* RUST: Just add derivation with the incompatibility *)
                          (* add_derivation will negate the term to get the derived ranges *)
                          let new_solution =
                            Partial_solution.add_derivation state.solution
                              satisfier_pkg incompat
                          in
                          (* Mark as contradicted immediately after adding derivation *)
                          let current_level =
                            Partial_solution.current_decision_level new_solution
                          in
                          ignore
                            (HashMap.insert state.contradicted incompat
                               current_level);
                          let new_state =
                            { state with solution = new_solution }
                          in
                          (* Continue with remaining incompats, then process satisfier_pkg *)
                          Log.info "  ⏭️  Continuing: satisfier=%s, rest=%s" satisfier_pkg
                            (String.concat "," rest);
                          match check_incompats new_state remaining with
                          | Ok state' ->
                              Log.info "  ✅ check_incompats OK, processing [%s]"
                                (String.concat "," (satisfier_pkg :: rest));
                              process_packages state' (satisfier_pkg :: rest)
                          | Error _ as err ->
                              Log.error "  ❌ check_incompats ERROR";
                              err)
                      | `Contradicted _ ->
                          (* Mark as contradicted so we don't check it again *)
                          let current_level =
                            Partial_solution.current_decision_level
                              state.solution
                          in
                          ignore
                            (HashMap.insert state.contradicted incompat
                               current_level);
                          check_incompats state remaining
                      | `Unknown -> check_incompats state remaining))
            in
            (* After checking all incompats for this package, continue with remaining packages *)
            match check_incompats state incompats with
            | Ok state' -> process_packages state' rest
            | Error _ as err -> err)
  in
  process_packages state changed_packages

(* ============================================================================
   Version Selection
   ============================================================================ *)

let choose_version provider state pkg ranges =
  Log.debug "Choosing version for %s" pkg;

  (* Get incompatibilities for this package to constrain the search *)
  let incompats =
    match HashMap.get state.incompatibilities pkg with
    | Some incompats -> incompats
    | None -> []
  in

  Log.info "  💡 Found %d incompatibilities for %s" (List.length incompats) pkg;

  (* Find effective ranges by checking which incompatibilities apply *)
  let effective_ranges = ref ranges in
  List.iter
    (fun incompat ->
      let terms = Incompatibility.terms incompat in
      Log.debug "  Checking incompatibility with %d terms" (List.length terms);

      (* Check if all OTHER terms are satisfied *)
      let all_other_satisfied = ref true in
      List.iter
        (fun term ->
          let term_pkg = Term.package term in
          if term_pkg <> pkg then (
            let constraint_status =
              Partial_solution.get_constraint state.solution term_pkg
            in
            (match constraint_status with
            | `Undecided -> Log.info "      Term pkg=%s is Undecided" term_pkg
            | `Decided v ->
                Log.info "      Term pkg=%s is Decided@%s" term_pkg
                  (Version.to_string v)
            | `Constrained _ ->
                Log.info "      Term pkg=%s is Constrained" term_pkg);
            match constraint_status with
            | `Undecided -> all_other_satisfied := false
            | `Decided ver ->
                let in_range =
                  Ranges.contains ~compare_v:version_compare (Term.ranges term)
                    ver
                in
                let term_satisfied =
                  (Term.is_positive term && in_range)
                  || ((not (Term.is_positive term)) && not in_range)
                in
                if not term_satisfied then all_other_satisfied := false
            | `Constrained constrained_ranges ->
                let term_satisfied =
                  if Term.is_positive term then
                    Ranges.subset_of ~compare_v:version_compare
                      constrained_ranges (Term.ranges term)
                  else
                    Ranges.is_disjoint ~compare_v:version_compare
                      constrained_ranges (Term.ranges term)
                in
                if not term_satisfied then all_other_satisfied := false))
        terms;

      (* If all other terms satisfied, constrain by this incompatibility *)
      if !all_other_satisfied then (
        Log.info "    ✨ All other terms satisfied for %s!" pkg;
        match Incompatibility.get_term incompat pkg with
        | Some term when Term.is_positive term ->
            Log.info
              "    ➕ Positive term - EXCLUDE these ranges to avoid conflict";
            let complement_ranges =
              Ranges.complement ~compare_v:version_compare (Term.ranges term)
            in
            effective_ranges :=
              Ranges.intersection ~compare_v:version_compare !effective_ranges
                complement_ranges
        | Some term ->
            Log.info "    ➖ Negative term - INCLUDE only these ranges";
            effective_ranges :=
              Ranges.intersection ~compare_v:version_compare !effective_ranges
                (Term.ranges term)
        | None -> ()))
    incompats;

  Log.debug "  Effective ranges computed";

  (* Ask provider for a version in the effective ranges *)
  match provider.Provider.choose_version pkg !effective_ranges with
  | Ok (Some ver) ->
      Log.debug "Chose %s@%s" pkg (Version.to_string ver);
      Ok (Some ver)
  | Ok None ->
      Log.debug "No version available for %s" pkg;
      Ok None
  | Error err ->
      Log.error "Error choosing version for %s: %s" pkg err;
      Error err

(* ============================================================================
   Main Solve Function
   ============================================================================ *)

let solve provider root_package root_version =
  Log.debug "Starting NEW PubGrub solver for %s@%s" root_package
    (Version.to_string root_version);

  (* Initialize state *)
  (* NOTE: Unlike some implementations, we don't add a not_root incompatibility
     because root is already decided. Adding it would cause unit_propagation
     to immediately detect a conflict. *)
  let incompats = HashMap.create () in

  let solution = Partial_solution.empty () in
  let solution =
    Partial_solution.add_decision solution root_package root_version
  in

  let initial_state =
    {
      solution;
      incompatibilities = incompats;
      dependency_graph = DependencyGraph.empty ();
      contradicted = HashMap.create ();
    }
  in

  (* Get root dependencies *)
  match provider.Provider.get_dependencies root_package root_version with
  | Error err ->
      Log.error "Failed to get dependencies for root: %s" err;
      Error err
  | Ok (Provider.Unavailable reason) ->
      Log.error "Root package unavailable: %s" reason;
      Error (format "Root package unavailable: %s" reason)
  | Ok (Provider.Available deps) -> (
      (* Add root dependencies to graph *)
      let dep_list =
        List.map (fun (dep_pkg, dep_ranges) -> (dep_pkg, dep_ranges)) deps
      in
      let dep_graph =
        DependencyGraph.add_dependencies initial_state.dependency_graph
          root_package root_version dep_list
      in
      let state = { initial_state with dependency_graph = dep_graph } in

      (* Check for impossible root dependencies *)
      let has_impossible_root_dep = ref None in
      List.iter
        (fun (dep_pkg, dep_ranges) ->
          (* Check for self-dependency with incompatible version *)
          if
            dep_pkg = root_package
            && not
                 (Ranges.contains ~compare_v:version_compare dep_ranges
                    root_version)
          then (
            Log.info
              "Impossible root self-dependency: %s@%s depends on %s with \
               incompatible range"
              root_package
              (Version.to_string root_version)
              dep_pkg;
            has_impossible_root_dep := Some (dep_pkg, dep_ranges))
          else if Ranges.is_empty dep_ranges then (
            Log.info
              "Impossible root dependency: %s@%s depends on %s with empty range"
              root_package
              (Version.to_string root_version)
              dep_pkg;
            has_impossible_root_dep := Some (dep_pkg, dep_ranges)))
        deps;

      (* If there's an impossible root dependency, fail immediately *)
      match !has_impossible_root_dep with
      | Some (dep_pkg, dep_ranges) ->
          let impossible_incompat =
            Incompatibility.from_dependency root_package root_version
              (dep_pkg, dep_ranges)
          in
          Log.error "Impossible root dependency detected, failing";
          Ok (Failure impossible_incompat)
      | None ->
          (* Add dependency incompatibilities *)
          let dep_packages = ref [] in
          List.iter
            (fun (dep_pkg, dep_ranges) ->
              let dep_incompat =
                Incompatibility.from_dependency root_package root_version
                  (dep_pkg, dep_ranges)
              in
              add_incompatibility state dep_pkg dep_incompat;
              dep_packages := dep_pkg :: !dep_packages)
            deps;

          (* Run initial unit propagation on all root dependencies to create derivations *)
          let state =
            match unit_propagation root_package root_version state !dep_packages with
            | Ok state -> state
            | Error _ -> state
          in

          (* Main solve loop *)
          let rec solve_loop state iteration next_pkg =
            if iteration > 1000 then (
              Log.error "Iteration limit reached after 1000 iterations!";
              Error "Too many iterations - likely infinite loop")
            else (
              (* Unit propagation on next package *)
              Log.debug "Iteration %d: unit propagation on %s" iteration
                next_pkg;
              match
                unit_propagation root_package root_version state [ next_pkg ]
              with
              | Error terminal_incompat ->
                  Log.error "Terminal incompatibility, no solution";
                  Ok (Failure terminal_incompat)
              | Ok propagated_state -> (
                  (* Pick next highest priority package *)
                  let prioritizer pkg ranges =
                    (* Simple prioritizer: more constrained = higher priority *)
                    let num_incompats =
                      match HashMap.get state.incompatibilities pkg with
                      | Some incompats -> List.length incompats
                      | None -> 0
                    in
                    if Ranges.is_empty ranges || ranges = Ranges.full then
                      num_incompats
                    else 1000 + num_incompats
                  in

                  match
                    Partial_solution.pick_highest_priority_pkg
                      propagated_state.solution prioritizer
                  with
                  | None ->
                      Log.debug "No more pending packages, solution found";
                      Ok
                        (Success
                           (Partial_solution.extract_solution
                              propagated_state.solution))
                  | Some (pkg, ranges) -> (
                      Log.info "Choosing version for pending package %s" pkg;
                      (* Try to choose a version for the pending package *)
                      match
                        choose_version provider propagated_state pkg ranges
                      with
                      | Error err -> Error err
                      | Ok None ->
                          (* No version available, add no_versions incompatibility and continue *)
                          (* This will trigger conflict resolution in the next iteration *)
                          Log.info
                            "📭 No version available for %s, adding no_versions \
                             incompatibility"
                            pkg;
                          (* Get constraint from partial solution, or use full if undecided *)
                          let constraint_ranges =
                            match
                              Partial_solution.get_constraint
                                propagated_state.solution pkg
                            with
                            | `Constrained r -> r
                            | `Undecided -> Ranges.full
                            | `Decided _ ->
                                panic
                                  "Package already decided in no_versions path"
                          in
                          let no_ver_incompat =
                            Incompatibility.no_versions pkg constraint_ranges
                          in
                          add_incompatibility propagated_state pkg
                            no_ver_incompat;
                          (* Continue loop - unit propagation will run at the top *)
                          solve_loop propagated_state (iteration + 1) pkg
                      | Ok (Some ver) -> (
                          (* Get dependencies BEFORE adding decision (like Rust) *)
                          match provider.Provider.get_dependencies pkg ver with
                          | Error err -> Error err
                          | Ok (Provider.Unavailable _reason) ->
                              (* Package unavailable, treat as no version *)
                              Log.debug "Package %s@%s unavailable" pkg
                                (Version.to_string ver);
                              solve_loop propagated_state (iteration + 1) pkg
                          | Ok (Provider.Available pkg_deps) -> (
                              Log.debug "Processing %d dependencies for %s@%s"
                                (List.length pkg_deps) pkg
                                (Version.to_string ver);

                              (* Check for impossible dependencies first *)
                              let has_impossible_dep = ref None in
                              List.iter
                                (fun (dep_pkg, dep_ranges) ->
                                  (* Check for self-dependency with incompatible version *)
                                  if
                                    dep_pkg = pkg
                                    && not
                                         (Ranges.contains
                                            ~compare_v:version_compare
                                            dep_ranges ver)
                                  then (
                                    Log.info
                                      "Impossible self-dependency: %s@%s \
                                       depends on %s with incompatible range"
                                      pkg (Version.to_string ver) dep_pkg;
                                    has_impossible_dep :=
                                      Some (dep_pkg, dep_ranges))
                                  else if Ranges.is_empty dep_ranges then (
                                    Log.info
                                      "Impossible dependency: %s@%s depends on \
                                       %s with empty range"
                                      pkg (Version.to_string ver) dep_pkg;
                                    has_impossible_dep :=
                                      Some (dep_pkg, dep_ranges)))
                                pkg_deps;

                              (* If impossible dependency, fail *)
                              match !has_impossible_dep with
                              | Some (dep_pkg, dep_ranges) ->
                                  let impossible_incompat =
                                    Incompatibility.from_dependency pkg ver
                                      (dep_pkg, dep_ranges)
                                  in
                                  Log.error
                                    "Impossible dependency detected, failing";
                                  Ok (Failure impossible_incompat)
                              | None -> (
                                  (* PORT OF RUST: Just add decision and incompatibilities *)
                                  Log.info "✅ Adding decision: %s@%s" pkg
                                    (Version.to_string ver);
                                  let new_solution =
                                    Partial_solution.add_decision
                                      propagated_state.solution pkg ver
                                  in
                                  let new_state =
                                    {
                                      propagated_state with
                                      solution = new_solution;
                                    }
                                  in

                                  (* Add dependencies to graph *)
                                  let dep_list =
                                    List.map (fun (d, r) -> (d, r)) pkg_deps
                                  in
                                  let new_dep_graph =
                                    DependencyGraph.add_dependencies
                                      new_state.dependency_graph pkg ver
                                      dep_list
                                  in
                                  let new_state =
                                    {
                                      new_state with
                                      dependency_graph = new_dep_graph;
                                    }
                                  in

                                  (* Add dependency incompatibilities and collect affected packages *)
                                  let affected_packages = ref [] in
                                  List.iter
                                    (fun (dep_pkg, dep_ranges) ->
                                      let dep_incompat =
                                        Incompatibility.from_dependency pkg ver
                                          (dep_pkg, dep_ranges)
                                      in
                                      Log.info "📦 Added dependency incompatibility for %s" dep_pkg;
                                      add_incompatibility new_state dep_pkg
                                        dep_incompat;
                                      affected_packages :=
                                        dep_pkg :: !affected_packages)
                                    pkg_deps;

                                  Log.info "🔄 Added %d dependency incompatibilities: %s"
                                    (List.length !affected_packages)
                                    (String.concat ", " !affected_packages);

                                  (* Run unit propagation on affected packages *)
                                  match
                                    unit_propagation root_package root_version
                                      new_state !affected_packages
                                  with
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
                                      solve_loop propagated_state (iteration + 1)
                                        next_to_check))))))
          in

          (* Start loop with first dependency package, or root if no deps *)
          let initial_next =
            match deps with first_dep :: _ -> fst first_dep | [] -> root_package
          in
          solve_loop state 0 initial_next)
