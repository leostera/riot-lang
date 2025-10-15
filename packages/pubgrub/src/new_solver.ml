open Std
open Std.Collections

type package = string
type version = Version.t

let version_compare a b =
  match Version.compare a b with Lt -> -1 | Eq -> 0 | Gt -> 1

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
    deps: ((package * version), (package * version Ranges.t) list) HashMap.t;
    (* Map from package to which packages depend on it (reverse index) *)
    reverse: (package, (package * version) list) HashMap.t;
  }

  let empty () = {
    deps = HashMap.create ();
    reverse = HashMap.create ();
  }

  let add_dependencies graph pkg ver deps =
    (* Store forward mapping: (pkg, ver) -> deps *)
    ignore (HashMap.insert graph.deps (pkg, ver) deps);
    
    (* Store reverse mapping: dep_pkg -> [(pkg, ver), ...] *)
    List.iter (fun (dep_pkg, _ranges) ->
      let existing = 
        match HashMap.get graph.reverse dep_pkg with
        | Some l -> l
        | None -> []
      in
      if not (List.mem (pkg, ver) existing) then
        ignore (HashMap.insert graph.reverse dep_pkg ((pkg, ver) :: existing))
    ) deps;
    
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
  solution: Partial_solution.t;
  incompatibilities: (package, Incompatibility.t list) HashMap.t;
  dependency_graph: DependencyGraph.t;
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
  let pending_map = HashMap.create () in
  
  (* Helper to add a package to pending with range merging *)
  let add_to_pending pkg ranges =
    let existing_ranges =
      match HashMap.get pending_map pkg with
      | Some r -> r
      | None -> Ranges.full
    in
    let new_ranges =
      Ranges.intersection ~compare_v:version_compare existing_ranges ranges
    in
    ignore (HashMap.insert pending_map pkg new_ranges)
  in
  
  (* Iterate through all decided packages and collect their undecided dependencies *)
  (* We need to iterate through all assignments in the solution *)
  let rec collect_from_assignments = function
    | [] -> ()
    | assignment :: rest ->
        (match assignment with
        | Partial_solution.Decision (pkg, ver, _level) ->
            (* Get dependencies for this decided package *)
            let deps = DependencyGraph.get_dependencies state.dependency_graph pkg ver in
            List.iter (fun (dep_pkg, dep_ranges) ->
              (* Check if dependency is undecided *)
              match Partial_solution.get_constraint state.solution dep_pkg with
              | `Undecided -> add_to_pending dep_pkg dep_ranges
              | `Decided _ | `Constrained _ -> ()  (* Already handled *)
            ) deps
        | Partial_solution.Derivation (pkg, _ranges, _cause, _level) ->
            (* Derivations don't have dependencies in our model *)
            (* Only decisions correspond to actual package versions with deps *)
            ()
        );
        collect_from_assignments rest
  in
  
  (* Get all assignments from solution - we need to expose this in Partial_solution *)
  (* For now, we'll work around by iterating through incompatibilities *)
  (* which contain dependency information *)
  
  (* Alternative: iterate through incompatibilities to find dependencies *)
  HashMap.iter (fun pkg incompats ->
    match Partial_solution.get_constraint state.solution pkg with
    | `Decided ver ->
        (* This package is decided, check its dependencies *)
        let deps = DependencyGraph.get_dependencies state.dependency_graph pkg ver in
        List.iter (fun (dep_pkg, dep_ranges) ->
          match Partial_solution.get_constraint state.solution dep_pkg with
          | `Undecided -> add_to_pending dep_pkg dep_ranges
          | _ -> ()
        ) deps
    | `Constrained _ranges ->
        (* Constrained packages don't have specific version deps yet *)
        ()
    | `Undecided -> ()
  ) state.incompatibilities;
  
  (* Convert HashMap to list *)
  let result = ref [] in
  HashMap.iter (fun pkg ranges ->
    result := (pkg, ranges) :: !result
  ) pending_map;
  !result

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
   Unit Propagation
   ============================================================================ *)

(* Unit propagation can either succeed with a new state, or hit a conflict *)
type propagation_result = 
  | PropagationOk of state
  | Conflict of Incompatibility.t

let unit_propagation root_package root_version state changed_packages =
  let rec process_packages state = function
    | [] -> PropagationOk state
    | pkg :: rest ->
        Log.debug "Unit propagation: processing %s" pkg;
        match HashMap.get state.incompatibilities pkg with
        | None -> process_packages state rest
        | Some incompats ->
            let rec check_incompats state = function
              | [] -> PropagationOk state
              | incompat :: remaining ->
                  match Partial_solution.relation state.solution incompat with
                  | `Satisfied ->
                      Log.debug "  Incompatibility satisfied, conflict!";
                      Conflict incompat
                  | `AlmostSatisfied satisfier_pkg ->
                      Log.debug "  Almost satisfied, deriving for %s" satisfier_pkg;
                      (* Get the term for this package *)
                      let satisfier_term =
                        Incompatibility.get_term incompat satisfier_pkg
                      in
                      let derived_ranges =
                        match satisfier_term with
                        | Some term when Term.is_positive term ->
                            Term.ranges term
                        | Some term ->
                            Ranges.complement ~compare_v:version_compare
                              (Term.ranges term)
                        | None -> Ranges.full
                      in
                      let new_solution =
                        Partial_solution.add_derivation state.solution
                          satisfier_pkg derived_ranges incompat
                      in
                      let new_state = { state with solution = new_solution } in
                      (* Continue with remaining incompats, then process satisfier_pkg *)
                      (match check_incompats new_state remaining with
                      | PropagationOk state' -> process_packages state' (satisfier_pkg :: rest)
                      | Conflict _ as err -> err)
                  | `Contradicted _ | `Unknown ->
                      check_incompats state remaining
            in
            check_incompats state incompats
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
  
  (* Find effective ranges by checking which incompatibilities apply *)
  let effective_ranges = ref ranges in
  List.iter (fun incompat ->
    let terms = Incompatibility.terms incompat in
    (* Check if all OTHER terms are satisfied *)
    let all_other_satisfied = ref true in
    List.iter (fun term ->
      let term_pkg = Term.package term in
      if term_pkg <> pkg then
        match Partial_solution.get_constraint state.solution term_pkg with
        | `Undecided -> all_other_satisfied := false
        | `Decided ver ->
            let in_range = Ranges.contains ~compare_v:version_compare (Term.ranges term) ver in
            let term_satisfied = 
              (Term.is_positive term && in_range) || 
              (not (Term.is_positive term) && not in_range)
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
            if not term_satisfied then all_other_satisfied := false
    ) terms;
    
    (* If all other terms satisfied, constrain by this incompatibility *)
    if !all_other_satisfied then
      match Incompatibility.get_term incompat pkg with
      | Some term when Term.is_positive term ->
          Log.debug "  Constraining by incompatibility";
          effective_ranges := 
            Ranges.intersection ~compare_v:version_compare 
              !effective_ranges (Term.ranges term)
      | _ -> ()
  ) incompats;
  
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

type solve_result =
  | Success of (package * version) list
  | Failure of Incompatibility.t

let solve provider root_package root_version =
  Log.debug "Starting NEW PubGrub solver for %s@%s" root_package
    (Version.to_string root_version);
  
  (* Initialize state *)
  let root_incompat = Incompatibility.not_root root_package root_version in
  let incompats = HashMap.create () in
  ignore (HashMap.insert incompats root_package [ root_incompat ]);
  
  let solution = Partial_solution.empty () in
  let solution = Partial_solution.add_decision solution root_package root_version in
  
  let initial_state = {
    solution;
    incompatibilities = incompats;
    dependency_graph = DependencyGraph.empty ();
  } in
  
  (* Get root dependencies *)
  match provider.Provider.get_dependencies root_package root_version with
  | Error err ->
      Log.error "Failed to get dependencies for root: %s" err;
      Error err
  | Ok (Provider.Unavailable reason) ->
      Log.error "Root package unavailable: %s" reason;
      Error (format "Root package unavailable: %s" reason)
  | Ok (Provider.Available deps) ->
      (* Add root dependencies to graph *)
      let dep_list = List.map (fun (dep_pkg, dep_ranges) ->
        (dep_pkg, dep_ranges)
      ) deps in
      let dep_graph = 
        DependencyGraph.add_dependencies 
          initial_state.dependency_graph 
          root_package 
          root_version 
          dep_list
      in
      let state = { initial_state with dependency_graph = dep_graph } in
      
      (* Add dependency incompatibilities *)
      List.iter (fun (dep_pkg, dep_ranges) ->
        let dep_incompat =
          Incompatibility.from_dependency root_package root_version (dep_pkg, dep_ranges)
        in
        add_incompatibility state dep_pkg dep_incompat
      ) deps;
      
      (* Main solve loop *)
      let rec solve_loop state iteration =
        if iteration > 10000 then
          panic "NEW solve_loop: too many iterations, likely infinite loop"
        else
          (* Compute pending from current state *)
          let pending = compute_pending state in
          Log.debug "Iteration %d: pending has %d packages" iteration (List.length pending);
          
          (* Unit propagation first *)
          match unit_propagation root_package root_version state [] with
          | Conflict incompat ->
              Log.debug "Conflict detected, returning failure";
              Ok (Failure incompat)
          | PropagationOk propagated_state ->
              (* Recompute pending after propagation *)
              let pending = compute_pending propagated_state in
              
              match pending with
              | [] ->
                  Log.debug "No more pending packages, solution found";
                  Ok (Success (Partial_solution.extract_solution propagated_state.solution))
              | (pkg, ranges) :: _rest ->
                  (* Try to choose a version for the first pending package *)
                  (match choose_version provider propagated_state pkg ranges with
                  | Error err -> Error err
                  | Ok None ->
                      (* No version available, add no_versions incompatibility *)
                      Log.debug "No version available for %s, adding no_versions" pkg;
                      let no_ver_incompat = Incompatibility.no_versions pkg ranges in
                      add_incompatibility propagated_state pkg no_ver_incompat;
                      (* Trigger unit propagation with this package *)
                      (match unit_propagation root_package root_version propagated_state [pkg] with
                      | Conflict incompat -> Ok (Failure incompat)
                      | PropagationOk new_state -> solve_loop new_state (iteration + 1))
                  | Ok (Some ver) ->
                      (* Add decision *)
                      let new_solution = 
                        Partial_solution.add_decision propagated_state.solution pkg ver
                      in
                      let new_state = { propagated_state with solution = new_solution } in
                      
                      (* Get dependencies for this package *)
                      (match provider.Provider.get_dependencies pkg ver with
                      | Error err -> Error err
                      | Ok (Provider.Unavailable _reason) ->
                          (* Package unavailable, treat as no version *)
                          Log.debug "Package %s@%s unavailable" pkg (Version.to_string ver);
                          solve_loop propagated_state (iteration + 1)
                      | Ok (Provider.Available pkg_deps) ->
                          (* Add dependencies to graph *)
                          let dep_list = List.map (fun (d, r) -> (d, r)) pkg_deps in
                          let new_dep_graph =
                            DependencyGraph.add_dependencies
                              new_state.dependency_graph pkg ver dep_list
                          in
                          let new_state = { new_state with dependency_graph = new_dep_graph } in
                          
                          (* Add dependency incompatibilities *)
                          List.iter (fun (dep_pkg, dep_ranges) ->
                            let dep_incompat =
                              Incompatibility.from_dependency pkg ver (dep_pkg, dep_ranges)
                            in
                            add_incompatibility new_state dep_pkg dep_incompat
                          ) pkg_deps;
                          
                          (* Continue solving with updated state *)
                          solve_loop new_state (iteration + 1)))
      in
      
      solve_loop state 0
