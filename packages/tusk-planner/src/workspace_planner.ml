open Std
open Tusk_model

type target = All | Package of string

type package_plan = {
  packages : Package.t list;
  package_graph : Package_graph.t;
  workspace : Workspace.t;
}

type plan_error =
  | PackageNotFound of { name : string; available : string list }
  | CycleDetected of { cycle : string list }
  | MissingDependencies of { missing : Package_graph.missing_dependency list }
  | PackageLoadFailed of { errors : Workspace_manager.load_error list }

let plan_workspace ~workspace ~target ~load_errors =
  (* Check for package load errors first *)
  if List.length load_errors > 0 then
    Error (PackageLoadFailed { errors = load_errors })
  else
    match Package_graph.create workspace with
    | Error (Package_graph.MissingPackages { missing }) ->
        Error (MissingDependencies { missing })
    | Ok package_graph ->
      let target_graph =
        match target with
        | All -> Ok package_graph
        | Package pkg_name ->
            let filtered =
              Package_graph.filter_for_package package_graph pkg_name
            in
            if Package_graph.size filtered = 0 then
              let available =
                List.map (fun (p : Package.t) -> p.name) workspace.packages
              in
              Error (PackageNotFound { name = pkg_name; available })
            else Ok filtered
      in

      match target_graph with
      | Error e -> Error e
      | Ok graph -> (
          let sorted_packages =
            try Ok (Package_graph.topological_sort graph)
            with Package_graph.Cycle_detected cycle ->
              Error (CycleDetected { cycle })
          in

          match sorted_packages with
          | Error e -> Error e
          | Ok nodes ->
              let packages = List.map Package_graph.get_package nodes in
              Ok { packages; package_graph = graph; workspace })

let packages_in_plan plan = plan.packages
