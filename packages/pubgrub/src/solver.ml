open Std
open Std.Collections

type package = string
type version = Version.t

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

type state = {
  solution : Partial_solution.t;
  incompatibilities : (package, Incompatibility.t list) HashMap.t;
  pending : (package * version Ranges.t) list;
}

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

let add_incompatibility state incompat =
  let terms = Incompatibility.terms incompat in
  List.iter
    (fun term ->
      let pkg = Term.package term in
      match HashMap.get state.incompatibilities pkg with
      | None -> ignore (HashMap.insert state.incompatibilities pkg [ incompat ])
      | Some incompats ->
          ignore
            (HashMap.insert state.incompatibilities pkg (incompat :: incompats)))
    terms

(* Force rebuild - depth limiting to prevent infinite recursion *)

let rec unit_propagation root_package root_version state initial_changed =
  (* Use a mutable list as a buffer for packages to process *)
  let buffer = ref initial_changed in
  let current_state = ref state in
  let iteration_count = ref 0 in
  (* Track packages we've already added to prevent infinite loops *)
  let already_added = ref [] in

  let rec process_buffer () =
    incr iteration_count;
    if !iteration_count > 1000 then
      panic
        (format "unit_propagation: too many iterations (%d)" !iteration_count);

    match !buffer with
    | [] -> Ok !current_state
    | pkg :: rest -> (
        buffer := rest;

        match HashMap.get !current_state.incompatibilities pkg with
        | None -> process_buffer ()
        | Some incompats ->
            (* Process all incompatibilities for this package *)
            let rec check_incompats = function
              | [] -> process_buffer ()
              | incompat :: remaining_incompats -> (
                  match
                    Partial_solution.relation !current_state.solution incompat
                  with
                  | `Satisfied ->
                      conflict_resolution root_package root_version
                        !current_state incompat
                  | `AlmostSatisfied satisfier_pkg ->
                      (* Get the term for this package to derive the correct ranges *)
                      let satisfier_term =
                        Incompatibility.get_term incompat satisfier_pkg
                      in
                      let derived_ranges =
                        match satisfier_term with
                        | Some term when Term.is_positive term ->
                            (* Positive term: derive the ranges to make term true *)
                            Term.ranges term
                        | Some term ->
                            (* Negative term: derive complement to make term true *)
                            Ranges.complement ~compare_v:version_compare
                              (Term.ranges term)
                        | None -> Ranges.full
                      in
                      let new_solution =
                        Partial_solution.add_derivation !current_state.solution
                          satisfier_pkg derived_ranges incompat
                      in
                      current_state :=
                        { !current_state with solution = new_solution };
                      (* Add satisfier_pkg to buffer only if we haven't added it before in this call *)
                      if
                        (not (List.mem satisfier_pkg !already_added))
                        && not (List.mem satisfier_pkg !buffer)
                      then (
                        buffer := satisfier_pkg :: !buffer;
                        already_added := satisfier_pkg :: !already_added);
                      check_incompats remaining_incompats
                  | `Contradicted _ | `Unknown -> check_incompats remaining_incompats)
            in
            check_incompats (List.rev incompats))
  in
  process_buffer ()

and conflict_resolution root_package root_version state incompat =
  let rec resolve_conflict depth current_incompat =
    if depth > 10 then (
      Log.error "PANIC: conflict_resolution depth limit exceeded: %d" depth;
      panic (format "conflict_resolution: depth limit exceeded (%d)" depth));
    if Incompatibility.is_terminal current_incompat root_package root_version
    then Error (Failure current_incompat)
    else
      let pkg, search_result =
        Partial_solution.satisfier_search state.solution current_incompat
      in
      match search_result with
      | `DifferentDecisionLevels previous_level ->
          Log.debug "Backtracking to decision level %d" previous_level;
          if previous_level = 0 then (
            Log.debug "Backtracking to level 0 means no solution exists!";
            Error (Failure current_incompat))
          else
            let new_solution =
              Partial_solution.backtrack state.solution previous_level
            in
            let new_state = { state with solution = new_solution } in
            add_incompatibility new_state current_incompat;
            Ok new_state
      | `SameDecisionLevels satisfier_cause ->
          Log.debug "Same decision level, computing prior cause for package %s"
            pkg;
          let prior =
            Incompatibility.prior_cause current_incompat satisfier_cause pkg
          in
          resolve_conflict (depth + 1) prior
  in
  resolve_conflict 0 incompat

let solve provider root_package root_version =
  Log.debug "Starting PubGrub solver for %s@%s" root_package
    (Version.to_string root_version);

  let root_incompat = Incompatibility.not_root root_package root_version in
  let incompats = HashMap.create () in
  ignore (HashMap.insert incompats root_package [ root_incompat ]);

  let solution = Partial_solution.empty () in
  let solution =
    Partial_solution.add_decision solution root_package root_version
  in

  let state = { solution; incompatibilities = incompats; pending = [] } in

  match provider.Provider.get_dependencies root_package root_version with
  | Error err -> Error err
  | Ok (Provider.Unavailable reason) ->
      Log.debug "Root package unavailable: %s" reason;
      Ok (Failure (Incompatibility.no_versions root_package Ranges.full))
  | Ok (Provider.Available deps) -> (
      let already_decided_deps = ref [] in
      let incompatible_dep = ref None in
      List.iter
        (fun (dep_pkg, dep_ranges) ->
          let dep_incompat =
            Incompatibility.from_dependency root_package root_version
              (dep_pkg, dep_ranges)
          in
          add_incompatibility state dep_incompat;
          match Partial_solution.get_constraint state.solution dep_pkg with
          | `Decided decided_ver ->
              Log.debug "Root dependency %s is already decided as %s!" dep_pkg
                (Version.to_string decided_ver);
              if
                not
                  (Ranges.contains ~compare_v:version_compare dep_ranges
                     decided_ver)
              then (
                Log.debug
                  "  But decided version %s is NOT in required range! \
                   Impossible!"
                  (Version.to_string decided_ver);
                incompatible_dep := Some dep_incompat);
              already_decided_deps := dep_pkg :: !already_decided_deps
          | `Constrained constrained_ranges ->
              Log.debug "Root dependency %s is already constrained" dep_pkg;
              if
                Ranges.is_disjoint ~compare_v:version_compare constrained_ranges
                  dep_ranges
              then (
                Log.debug
                  "  But constrained ranges are disjoint with required range! \
                   Impossible!";
                incompatible_dep := Some dep_incompat);
              already_decided_deps := dep_pkg :: !already_decided_deps
          | `Undecided -> ())
        deps;

      match !incompatible_dep with
      | Some incompat ->
          Log.debug
            "Found incompatible root dependency, returning failure immediately";
          Ok (Failure incompat)
      | None ->
          let pending =
            List.filter
              (fun (pkg, _) ->
                match Partial_solution.get_constraint state.solution pkg with
                | `Undecided -> true
                | `Decided _ | `Constrained _ -> false)
              deps
          in
          let state = { state with pending } in

          let iteration_count = ref 0 in
          let rec solve_loop state next_packages =
            incr iteration_count;
            if !iteration_count > 10000 then
              panic
                (format
                   "solve_loop: too many iterations (%d), likely infinite loop"
                   !iteration_count);
            (* Call unit_propagation at the start of each iteration *)
            match
              unit_propagation root_package root_version state next_packages
            with
            | Error result -> Ok result
            | Ok propagated_state -> (
                let state = propagated_state in
                match state.pending with
                | [] ->
                    Log.debug "No more pending packages, solution found";
                    Ok
                      (Success
                         (Partial_solution.extract_solution state.solution))
                | (pkg, ranges) :: rest_pending -> (
                    match Partial_solution.get_constraint state.solution pkg with
                    | `Decided _ | `Constrained _ ->
                        Log.debug "Package %s already decided/constrained, skipping"
                          pkg;
                        solve_loop { state with pending = rest_pending } []
                    | `Undecided -> (
                        Log.debug "Choosing version for package: %s" pkg;

                        let effective_ranges, applied_incompats =
                          match HashMap.get state.incompatibilities pkg with
                          | None ->
                              Log.debug "  No incompatibilities found for %s"
                                pkg;
                              (ranges, [])
                          | Some incompats ->
                              Log.debug "  Found %d incompatibilities for %s"
                                (List.length incompats) pkg;
                              List.fold_left
                                (fun (acc_ranges, acc_incompats) incompat ->
                                  let terms = Incompatibility.terms incompat in
                                  Log.debug
                                    "    Checking incompatibility with %d terms"
                                    (List.length terms);
                                  let all_other_terms_satisfied = ref true in
                                  let pkg_term = ref None in
                                  List.iter
                                    (fun term ->
                                      let term_pkg = Term.package term in
                                      if term_pkg = pkg then (
                                        Log.debug
                                          "      Term for %s (the package \
                                           we're choosing)"
                                          pkg;
                                        pkg_term := Some term)
                                      else (
                                        Log.debug "      Other term for %s"
                                          term_pkg;
                                        match
                                          Partial_solution.get_constraint
                                            state.solution term_pkg
                                        with
                                        | `Undecided ->
                                            Log.debug "        Not decided yet";
                                            all_other_terms_satisfied := false
                                        | `Decided ver ->
                                            let in_range =
                                              Ranges.contains
                                                ~compare_v:version_compare
                                                (Term.ranges term) ver
                                            in
                                            let term_is_false =
                                              Term.is_positive term
                                              && not in_range
                                              || (not (Term.is_positive term))
                                                 && in_range
                                            in
                                            Log.debug
                                              "        Decided as %s, term %s, \
                                               in_range=%b, term_is_false=%b"
                                              (Version.to_string ver)
                                              (if Term.is_positive term then
                                                 "positive"
                                               else "negative")
                                              in_range term_is_false;
                                            if not term_is_false then
                                              all_other_terms_satisfied := false
                                        | `Constrained constrained_ranges ->
                                            (* Check if this term is satisfied given the constraint *)
                                            let term_ranges = Term.ranges term in
                                            let is_positive = Term.is_positive term in
                                            let term_is_satisfied =
                                              if is_positive then
                                                (* Positive: satisfied if constrained ⊆ term_ranges *)
                                                Ranges.subset_of
                                                  ~compare_v:version_compare
                                                  constrained_ranges term_ranges
                                              else
                                                (* Negative: satisfied if disjoint *)
                                                Ranges.is_disjoint
                                                  ~compare_v:version_compare
                                                  constrained_ranges term_ranges
                                            in
                                            Log.debug
                                              "        Constrained, term_is_satisfied=%b"
                                              term_is_satisfied;
                                            if not term_is_satisfied then
                                              all_other_terms_satisfied := false))
                                    terms;

                                  if !all_other_terms_satisfied then (
                                    Log.debug "    All other terms satisfied!";
                                    match !pkg_term with
                                    | Some term when Term.is_positive term ->
                                        Log.debug
                                          "  Constraining %s by \
                                           incompatibility (positive term)"
                                          pkg;
                                        ( Ranges.intersection
                                            ~compare_v:version_compare
                                            acc_ranges (Term.ranges term),
                                          incompat :: acc_incompats )
                                    | Some term ->
                                        Log.debug
                                          "  Constraining %s by \
                                           incompatibility (negative term - \
                                           excluding range)"
                                          pkg;
                                        let excluded_ranges =
                                          Term.ranges term
                                        in
                                        let allowed_ranges =
                                          Ranges.complement
                                            ~compare_v:version_compare
                                            excluded_ranges
                                        in
                                        ( Ranges.intersection
                                            ~compare_v:version_compare
                                            acc_ranges allowed_ranges,
                                          incompat :: acc_incompats )
                                    | None -> (acc_ranges, acc_incompats))
                                  else (
                                    Log.debug
                                      "    Not all other terms satisfied";
                                    (acc_ranges, acc_incompats)))
                                (ranges, []) incompats
                        in

                        match
                          provider.Provider.choose_version pkg effective_ranges
                        with
                        | Error err -> Error err
                        | Ok None ->
                            Log.debug
                              "No version available for %s, adding no_versions \
                               incompatibility"
                              pkg;
                            (* Add no_versions incompatibility and continue *)
                            (* The next unit_propagation call will process it *)
                            let no_ver_incompat =
                              Incompatibility.no_versions pkg effective_ranges
                            in
                            Log.debug "  Created no_versions incompatibility";
                            add_incompatibility state no_ver_incompat;
                            Log.debug
                              "  Added to state, continuing solve_loop with pkg=%s \
                               in changed list"
                              pkg;
                            (* Continue loop with this package for unit_propagation *)
                            solve_loop
                              { state with pending = rest_pending }
                              [ pkg ]
                        | Ok (Some ver) -> (
                            Log.debug "Chose %s@%s" pkg (Version.to_string ver);

                            let new_solution =
                              Partial_solution.add_decision state.solution pkg
                                ver
                            in
                            let new_state =
                              {
                                state with
                                solution = new_solution;
                                pending = rest_pending;
                              }
                            in

                            match
                              provider.Provider.get_dependencies pkg ver
                            with
                            | Error err -> Error err
                            | Ok (Provider.Unavailable reason) ->
                                Log.debug "Package %s@%s unavailable: %s" pkg
                                  (Version.to_string ver) reason;
                                let no_ver_incompat =
                                  Incompatibility.no_versions pkg ranges
                                in
                                Ok (Failure no_ver_incompat)
                            | Ok (Provider.Available deps) -> (
                                Log.debug "Adding %d dependencies for %s@%s"
                                  (List.length deps) pkg (Version.to_string ver);

                                let already_decided_deps = ref [] in
                                let incompatible_dep = ref None in
                                List.iter
                                  (fun (dep_pkg, dep_ranges) ->
                                    Log.debug
                                      "Processing dependency: %s@%s -> %s" pkg
                                      (Version.to_string ver) dep_pkg;
                                    let dep_incompat =
                                      Incompatibility.from_dependency pkg ver
                                        (dep_pkg, dep_ranges)
                                    in
                                    add_incompatibility new_state dep_incompat;
                                    match
                                      Partial_solution.get_constraint
                                        new_state.solution dep_pkg
                                    with
                                    | `Decided decided_ver ->
                                        Log.debug
                                          "Dependency %s is already decided as \
                                           %s!"
                                          dep_pkg
                                          (Version.to_string decided_ver);
                                        if
                                          not
                                            (Ranges.contains
                                               ~compare_v:version_compare
                                               dep_ranges decided_ver)
                                        then (
                                          Log.debug
                                            "  But decided version %s is NOT \
                                             in required range! Impossible!"
                                            (Version.to_string decided_ver);
                                          incompatible_dep := Some dep_incompat);
                                        already_decided_deps :=
                                          dep_pkg :: !already_decided_deps
                                    | `Constrained constrained_ranges ->
                                        Log.debug
                                          "Dependency %s is already constrained"
                                          dep_pkg;
                                        if
                                          Ranges.is_disjoint ~compare_v:version_compare
                                            constrained_ranges dep_ranges
                                        then (
                                          Log.debug
                                            "  But constrained ranges are disjoint \
                                             with required range! Impossible!";
                                          incompatible_dep := Some dep_incompat);
                                        already_decided_deps :=
                                          dep_pkg :: !already_decided_deps
                                    | `Undecided -> ())
                                  deps;

                                match !incompatible_dep with
                                | Some incompat -> (
                                    Log.debug
                                      "Found incompatible dependency, triggering \
                                       conflict resolution";
                                    match
                                      conflict_resolution root_package root_version
                                        new_state incompat
                                    with
                                    | Error result -> Ok result
                                    | Ok resolved_state ->
                                        solve_loop resolved_state [ pkg ])
                                | None ->
                                    let new_pending =
                                      List.fold_left
                                        (fun acc (dep_pkg, dep_ranges) ->
                                          match
                                            Partial_solution.get_constraint
                                              new_state.solution dep_pkg
                                          with
                                          | `Undecided -> (dep_pkg, dep_ranges) :: acc
                                          | `Decided _ | `Constrained _ -> acc)
                                        rest_pending deps
                                    in

                                    let final_state =
                                      { new_state with pending = new_pending }
                                    in

                                    let changed_pkgs =
                                      pkg :: !already_decided_deps
                                    in
                                    solve_loop final_state changed_pkgs)))))
          in
          solve_loop state (root_package :: !already_decided_deps))
