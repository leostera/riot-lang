open Std

type package = string
type version = Version.t

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

type state = {
  solution : Partial_solution.t;
  incompatibilities : (package, Incompatibility.t list) Collections.HashMap.t;
  undecided : (package * version Ranges.t) list;
}

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

let relation_to_term incompat solution =
  let open Term in
  let all_satisfied = ref true in
  let almost_satisfied = ref None in

  List.iter
    (fun term ->
      let pkg = package term in
      let ranges = ranges term in
      let is_positive = is_positive term in

      match Partial_solution.get_decision solution pkg with
      | None ->
          if !almost_satisfied = None then
            almost_satisfied := Some (pkg, ranges, term)
          else all_satisfied := false
      | Some ver ->
          let in_range =
            Ranges.contains ~compare_v:version_compare ranges ver
          in
          if (is_positive && not in_range) || ((not is_positive) && in_range)
          then ()
          else if is_positive && in_range then ()
          else all_satisfied := false)
    incompat.Incompatibility.terms;

  if !all_satisfied then
    match !almost_satisfied with
    | None -> `Satisfied
    | Some (pkg, _, _) -> `AlmostSatisfied pkg
  else `Contradicted

let solve provider root_package root_version =
  Log.debug "Starting solver for %s@%s" root_package
    (Version.to_string root_version);

  let root_incompat = Incompatibility.not_root root_package root_version in
  let incompats = Collections.HashMap.create () in
  ignore (Collections.HashMap.insert incompats root_package [ root_incompat ]);

  let solution = Partial_solution.empty () in
  let solution =
    Partial_solution.add_decision solution root_package root_version
  in

  let state =
    {
      solution;
      incompatibilities = incompats;
      undecided = [ (root_package, Ranges.full) ];
    }
  in

  let rec solve_loop state =
    match state.undecided with
    | [] ->
        Log.debug "No more undecided packages, extracting solution";
        Ok (Success (Partial_solution.extract_solution state.solution))
    | (pkg, ranges) :: rest_undecided -> (
        Log.debug "Choosing version for package: %s" pkg;

        match provider.Provider.choose_version pkg ranges with
        | Error err -> Error err
        | Ok None ->
            Log.debug "No version available for %s in range" pkg;
            let no_ver_incompat = Incompatibility.no_versions pkg ranges in
            Ok (Failure no_ver_incompat)
        | Ok (Some ver) -> (
            Log.debug "Chose %s@%s" pkg (Version.to_string ver);

            let new_solution =
              Partial_solution.add_decision state.solution pkg ver
            in

            match provider.Provider.get_dependencies pkg ver with
            | Error err -> Error err
            | Ok (Provider.Unavailable reason) ->
                Log.debug "Package %s@%s unavailable: %s" pkg
                  (Version.to_string ver) reason;
                let no_ver_incompat = Incompatibility.no_versions pkg ranges in
                Ok (Failure no_ver_incompat)
            | Ok (Provider.Available deps) ->
                Log.debug "Adding %d dependencies for %s@%s" (List.length deps)
                  pkg (Version.to_string ver);

                let new_undecided =
                  List.fold_left
                    (fun acc (dep_pkg, dep_ranges) ->
                      match
                        Partial_solution.get_decision new_solution dep_pkg
                      with
                      | Some _ -> acc
                      | None -> (dep_pkg, dep_ranges) :: acc)
                    rest_undecided deps
                in

                let new_state =
                  {
                    solution = new_solution;
                    incompatibilities = state.incompatibilities;
                    undecided = new_undecided;
                  }
                in
                solve_loop new_state))
  in

  solve_loop state
