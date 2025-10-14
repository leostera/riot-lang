open Std

type package = string
type version = Version.t

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

type state = {
  solution : Partial_solution.t;
  incompatibilities : (package, Incompatibility.t list) Collections.HashMap.t;
  pending : (package * version Ranges.t) list;
}

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

let add_incompatibility state incompat =
  let terms = Incompatibility.terms incompat in
  List.iter
    (fun term ->
      let pkg = Term.package term in
      match Collections.HashMap.get state.incompatibilities pkg with
      | None ->
          ignore
            (Collections.HashMap.insert state.incompatibilities pkg [ incompat ])
      | Some incompats ->
          ignore
            (Collections.HashMap.insert state.incompatibilities pkg
               (incompat :: incompats)))
    terms

let rec unit_propagation root_package root_version state changed =
  match changed with
  | [] -> Ok state
  | pkg :: rest -> (
      match Collections.HashMap.get state.incompatibilities pkg with
      | None -> unit_propagation root_package root_version state rest
      | Some incompats ->
          let rec check_incompats = function
            | [] -> unit_propagation root_package root_version state rest
            | incompat :: remaining_incompats -> (
                match Partial_solution.relation state.solution incompat with
                | `Satisfied ->
                    Log.debug "Incompatibility satisfied for %s" pkg;
                    conflict_resolution root_package root_version state incompat
                | `AlmostSatisfied satisfier_pkg ->
                    Log.debug "Almost satisfied, deriving for package: %s"
                      satisfier_pkg;
                    let new_solution =
                      Partial_solution.add_derivation state.solution
                        satisfier_pkg Ranges.full incompat
                    in
                    let new_state = { state with solution = new_solution } in
                    unit_propagation root_package root_version new_state
                      (satisfier_pkg :: rest)
                | `Contradicted _ | `Unknown ->
                    check_incompats remaining_incompats)
          in
          check_incompats (List.rev incompats))

and conflict_resolution root_package root_version state incompat =
  let rec resolve_conflict current_incompat =
    if Incompatibility.is_terminal current_incompat root_package root_version then
      Error (Failure current_incompat)
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
          resolve_conflict prior
  in
  resolve_conflict incompat

let solve provider root_package root_version =
  Log.debug "Starting PubGrub solver for %s@%s" root_package
    (Version.to_string root_version);

  let root_incompat = Incompatibility.not_root root_package root_version in
  let incompats = Collections.HashMap.create () in
  ignore (Collections.HashMap.insert incompats root_package [ root_incompat ]);

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
          match Partial_solution.get_decision state.solution dep_pkg with
          | Some decided_ver ->
              Log.debug "Root dependency %s is already decided as %s!" dep_pkg
                (Version.to_string decided_ver);
              if not (Ranges.contains ~compare_v:version_compare dep_ranges decided_ver)
              then (
                Log.debug
                  "  But decided version %s is NOT in required range! Impossible!"
                  (Version.to_string decided_ver);
                incompatible_dep := Some dep_incompat);
              already_decided_deps := dep_pkg :: !already_decided_deps
          | None -> ())
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
                Partial_solution.get_decision state.solution pkg = None)
              deps
          in
          let state = { state with pending } in

          match
            unit_propagation root_package root_version state
              (root_package :: !already_decided_deps)
          with
          | Error result -> Ok result
          | Ok propagated_state ->
              let state = propagated_state in

              let rec solve_loop state =
                match state.pending with
                | [] ->
                    Log.debug "No more pending packages, solution found";
                    Ok
                      (Success (Partial_solution.extract_solution state.solution))
                | (pkg, ranges) :: rest_pending -> (
                match Partial_solution.get_decision state.solution pkg with
                | Some _ ->
                    Log.debug "Package %s already decided, skipping" pkg;
                    solve_loop { state with pending = rest_pending }
                | None -> (
                    Log.debug "Choosing version for package: %s" pkg;

                    match provider.Provider.choose_version pkg ranges with
                    | Error err -> Error err
                    | Ok None ->
                        Log.debug "No version available for %s" pkg;
                        let no_ver_incompat =
                          Incompatibility.no_versions pkg ranges
                        in
                        Ok (Failure no_ver_incompat)
                    | Ok (Some ver) -> (
                        Log.debug "Chose %s@%s" pkg (Version.to_string ver);

                        let new_solution =
                          Partial_solution.add_decision state.solution pkg ver
                        in
                        let new_state =
                          {
                            state with
                            solution = new_solution;
                            pending = rest_pending;
                          }
                        in

                        match provider.Provider.get_dependencies pkg ver with
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
                                Log.debug "Processing dependency: %s@%s -> %s"
                                  pkg (Version.to_string ver) dep_pkg;
                                let dep_incompat =
                                  Incompatibility.from_dependency pkg ver
                                    (dep_pkg, dep_ranges)
                                in
                                add_incompatibility new_state dep_incompat;
                                match
                                  Partial_solution.get_decision
                                    new_state.solution dep_pkg
                                with
                                | Some decided_ver ->
                                    Log.debug
                                      "Dependency %s is already decided as %s!"
                                      dep_pkg (Version.to_string decided_ver);
                                    if
                                      not
                                        (Ranges.contains ~compare_v:version_compare
                                           dep_ranges decided_ver)
                                    then (
                                      Log.debug
                                        "  But decided version %s is NOT in \
                                         required range! Impossible!"
                                        (Version.to_string decided_ver);
                                      incompatible_dep := Some dep_incompat);
                                    already_decided_deps :=
                                      dep_pkg :: !already_decided_deps
                                | None -> ())
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
                                | Ok resolved_state -> solve_loop resolved_state)
                            | None ->
                                let new_pending =
                              List.fold_left
                                (fun acc (dep_pkg, dep_ranges) ->
                                  if
                                    Partial_solution.get_decision
                                      new_state.solution dep_pkg
                                    = None
                                  then (dep_pkg, dep_ranges) :: acc
                                  else acc)
                                rest_pending deps
                            in

                            let final_state =
                              { new_state with pending = new_pending }
                            in

                                let changed_pkgs = pkg :: !already_decided_deps in
                                match
                                  unit_propagation root_package root_version
                                    final_state changed_pkgs
                                with
                                | Error result -> Ok result
                                | Ok propagated_state ->
                                    solve_loop propagated_state))))
              in
              solve_loop state)
