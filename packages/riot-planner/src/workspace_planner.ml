open Std
open Std.Collections
open Riot_model

type target =
  | All
  | Package of Package_name.t
  | Packages of Package_name.t list

type planning_breakdown = {
  manifest_filter_duration: Time.Duration.t;
  filtered_workspace_package_count: int;
  package_graph_duration: Time.Duration.t;
  package_graph_node_count: int;
  package_graph_create_breakdown: Package_graph.create_breakdown;
  target_graph_filter_duration: Time.Duration.t;
  target_graph_node_count: int;
  topological_sort_duration: Time.Duration.t;
  sorted_package_count: int;
}

type package_plan = {
  packages: Package.t list;
  nodes: Package_graph.package_node list;
  package_graph: Package_graph.t;
  workspace: Workspace.t;
  breakdown: planning_breakdown;
}

type plan_error =
  | PackageNotFound of {
      name: Package_name.t;
      available: Package_name.t list;
    }
  | PackagesNotFound of {
      names: Package_name.t list;
      available: Package_name.t list;
    }
  | CycleDetected of {
      cycle: string list;
    }
  | MissingDependencies of {
      missing: Package_graph.missing_dependency list;
    }
  | PackageLoadFailed of {
      errors: Workspace_manager.load_error list;
    }

let manifest_dependency_names_for_scope = fun (scope: Package_graph.build_scope) (
  pkg: Package_manifest.t
) ->
  let dependency_name (dep: Package.dependency) = dep.name in
  match scope with
  | Package_graph.Build -> List.map pkg.build_dependencies ~fn:dependency_name
  | Package_graph.Runtime -> List.map pkg.dependencies ~fn:dependency_name
  | Package_graph.Dev ->
      List.concat
        [
          List.map pkg.dependencies ~fn:dependency_name;
          List.map pkg.dev_dependencies ~fn:dependency_name;
        ]

let package_manifest_table = fun (workspace: Workspace.t) ->
  let table = HashMap.create () in
  List.for_each
    workspace.packages
    ~fn:(fun (pkg: Package_manifest.t) ->
      let _ = HashMap.insert table ~key:pkg.name ~value:pkg in
      ());
  table

let target_package_names = function
  | All -> []
  | Package name -> [ name ]
  | Packages names -> names

let target_missing_package_names = fun ~(workspace:Workspace.t) target ->
  let available_set = HashSet.create () in
  List.for_each
    workspace.packages
    ~fn:(fun (pkg: Package_manifest.t) ->
      let _ = HashSet.insert available_set ~value:pkg.name in
      ());
  target_package_names target
  |> List.filter ~fn:(fun pkg_name -> not (HashSet.contains available_set ~value:pkg_name))
  |> List.sort ~compare:Package_name.compare
  |> List.unique ~compare:Package_name.compare

let filter_workspace_for_target = fun ~(workspace:Workspace.t) ~target ~(scope:Package_graph.build_scope) ->
  match target with
  | All -> workspace
  | Package _
  | Packages _ ->
      let manifests_by_name = package_manifest_table workspace in
      let seen = HashSet.create () in
      let rec visit = function
        | [] -> ()
        | pkg_name :: rest when HashSet.contains seen ~value:pkg_name -> visit rest
        | pkg_name :: rest ->
            let _ = HashSet.insert seen ~value:pkg_name in
            let deps =
              match HashMap.get manifests_by_name ~key:pkg_name with
              | None -> []
              | Some pkg -> manifest_dependency_names_for_scope scope pkg
            in
            visit (deps @ rest)
      in
      let initial_targets =
        target_package_names target
        |> List.unique ~compare:Package_name.compare
      in
      let () = visit initial_targets in
      {
        workspace with
        packages = List.filter
          workspace.packages
          ~fn:(fun (pkg: Package_manifest.t) -> HashSet.contains seen ~value:pkg.name);
      }

let plan_workspace = fun ~(workspace:Workspace.t) ~target ~(scope:Package_graph.build_scope) ~load_errors ~dev_artifacts ->
  (* Check for package load errors first *)
  if List.length load_errors > 0 then
    Error (PackageLoadFailed { errors = load_errors })
  else
    let available = List.map workspace.packages ~fn:(fun (p: Package_manifest.t) -> p.name) in
    let missing_targets = target_missing_package_names ~workspace target in
    if not (List.is_empty missing_targets) then
      match missing_targets with
      | [ name ] -> Error (PackageNotFound { name; available })
      | names -> Error (PackagesNotFound { names; available })
    else
      let manifest_filter_started_at = Time.Instant.now () in
      let workspace = filter_workspace_for_target ~workspace ~target ~scope in
      let manifest_filter_duration =
        Time.Instant.duration_since ~earlier:manifest_filter_started_at (Time.Instant.now ())
      in
      (
        let package_graph_started_at = Time.Instant.now () in
        match Package_graph.create_with_breakdown ~scope ~dev_artifacts workspace with
        | Error (Package_graph.MissingPackages { missing }) ->
            Error (MissingDependencies { missing })
        | Ok (package_graph, package_graph_create_breakdown) ->
            let package_graph_duration =
              Time.Instant.duration_since ~earlier:package_graph_started_at (Time.Instant.now ())
            in
            let target_graph_filter_started_at = Time.Instant.now () in
            let target_graph =
              let filter_packages pkg_names =
                let missing =
                  List.filter
                    pkg_names
                    ~fn:(fun pkg_name ->
                      Option.is_none (Package_graph.find_package package_graph pkg_name))
                  |> List.sort ~compare:Package_name.compare
                  |> List.unique ~compare:Package_name.compare
                in
                match missing with
                | [] -> Ok (Package_graph.filter_for_packages package_graph pkg_names)
                | [ name ] -> Error (PackageNotFound { name; available })
                | names -> Error (PackagesNotFound { names; available })
              in
              match target with
              | All -> Ok package_graph
              | Package pkg_name -> filter_packages [ pkg_name ]
              | Packages pkg_names -> filter_packages pkg_names
            in
            let target_graph_filter_duration =
              Time.Instant.duration_since
                ~earlier:target_graph_filter_started_at
                (Time.Instant.now ())
            in
            match target_graph with
            | Error e -> Error e
            | Ok graph -> (
                let topological_sort_started_at = Time.Instant.now () in
                let sorted_nodes =
                  try Ok (Package_graph.topological_sort graph) with
                  | Package_graph.Cycle_detected cycle -> Error (CycleDetected { cycle })
                in
                let topological_sort_duration =
                  Time.Instant.duration_since
                    ~earlier:topological_sort_started_at
                    (Time.Instant.now ())
                in
                match sorted_nodes with
                | Error e -> Error e
                | Ok nodes ->
                    let packages = List.map nodes ~fn:Package_graph.get_package in
                    let breakdown = {
                      manifest_filter_duration;
                      filtered_workspace_package_count = List.length workspace.packages;
                      package_graph_duration;
                      package_graph_node_count = Package_graph.size package_graph;
                      package_graph_create_breakdown;
                      target_graph_filter_duration;
                      target_graph_node_count = Package_graph.size graph;
                      topological_sort_duration;
                      sorted_package_count = List.length nodes;
                    }
                    in
                    Ok {
                      packages;
                      nodes;
                      package_graph = graph;
                      workspace;
                      breakdown;
                    }
              )
      )

let packages_in_plan = fun plan -> plan.packages

let planning_breakdown = fun plan -> plan.breakdown
