open Std
open Std.Collections
open Tusk_model

type target =
  All
  | Package of string
  | Packages of string list

type package_plan = {
  packages : Package.t list;
  package_graph : Package_graph.t;
  workspace : Workspace.t;
}

type plan_error =
  | PackageNotFound of {
      name : string;
      available : string list;
    }
  | PackagesNotFound of {
      names : string list;
      available : string list;
    }
  | CycleDetected of {
      cycle : string list;
    }
  | MissingDependencies of {
      missing : Package_graph.missing_dependency list;
    }
  | PackageLoadFailed of {
      errors : Workspace_manager.load_error list;
    }

let plan_workspace = fun ~workspace ~target ~(scope:Package_graph.build_scope) ~load_errors ->
  (* Check for package load errors first *)
  if List.length load_errors > 0 then
    Error (PackageLoadFailed {errors = load_errors})
  else
    match Package_graph.create ~scope workspace with
    | Error (Package_graph.MissingPackages { missing }) -> Error (MissingDependencies {missing})
    | Ok package_graph ->
        let target_graph =
          let available =
            List.map (fun (p:Package.t) -> p.name) workspace.packages
          in
          let filter_packages = fun pkg_names ->
            let missing = List.filter
            (fun pkg_name -> Option.is_none (Package_graph.find_package package_graph pkg_name))
            pkg_names
            |> List.sort_uniq String.compare in
            match missing with
            | [] -> Ok (Package_graph.filter_for_packages package_graph pkg_names)
            | [ name ] -> Error (PackageNotFound {name; available})
            | names -> Error (PackagesNotFound {names; available})
          in
          match target with
          | All -> Ok package_graph
          | Package pkg_name -> filter_packages [ pkg_name ]
          | Packages pkg_names -> filter_packages pkg_names
        in
        match target_graph with
        | Error e -> Error e
        | Ok graph -> (
            let sorted_packages =
              try Ok (Package_graph.topological_sort graph) with
              | Package_graph.Cycle_detected cycle -> Error (CycleDetected {cycle})
            in
            match sorted_packages with
            | Error e -> Error e
            | Ok nodes ->
                let packages = List.map Package_graph.get_package nodes in
                Ok {packages; package_graph = graph; workspace}
          )

let packages_in_plan = fun plan -> plan.packages
