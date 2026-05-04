open Std
open Std.Collections
open Riot_model

module G = Graph.SimpleGraph

type request_kind =
  | Runtime
  | Dev of Package.dev_artifacts

type synthetic_tool = {
  package: Package_name.t;
  name: string;
}

type request = {
  roots: Package_name.t list option;
  targets: Target.t list;
  profile: Profile.t;
  kind: request_kind;
  synthetic_tools: synthetic_tool list;
}

type missing_dependency = {
  package: Package_name.t;
  dependency: Package_name.t;
}

type missing_package =
  | Root of Package_name.t
  | Dependency of missing_dependency

type create_error =
  | MissingPackages of {
      missing: missing_package list;
    }

type t = {
  graph: Build_unit.t G.t;
  nodes: (Package.key, Build_unit.t G.node) HashMap.t;
  edges: Package.key HashSet.t;
  processed_libraries: Package.key HashSet.t;
}

type node = Build_unit.t G.node

let size = fun t ->
  let count = ref 0 in
  G.iter t.graph ~fn:(fun _ _ -> count := !count + 1);
  !count

let keys = fun t ->
  G.map t.graph ~fn:(fun (_, node) -> node.G.value.Build_unit.key)
  |> List.sort ~compare:Build_unit.compare_key

let find = fun t key -> HashMap.get t.nodes ~key:(Build_unit.package_key key)

let node_value = fun node -> node.G.value

let dependencies = fun t key ->
  match find t key with
  | None -> []
  | Some node ->
      node.G.deps
      |> List.filter_map
        ~fn:(fun node_id ->
          G.get_node t.graph node_id
          |> Option.map ~fn:(fun node -> node.G.value.Build_unit.key))
      |> List.sort ~compare:Build_unit.compare_key

let topological_sort = fun t ->
  match G.topo_sort t.graph with
  | Ok nodes -> Ok (List.map nodes ~fn:(fun node -> node.G.value))
  | Error node_ids ->
      Error (
        List.filter_map
          node_ids
          ~fn:(fun node_id ->
            G.get_node t.graph node_id
            |> Option.map ~fn:(fun node -> node.G.value.Build_unit.key))
      )

let package_table = fun (workspace: Workspace.t) ->
  let table = HashMap.with_capacity ~size:(List.length workspace.packages) in
  List.for_each
    workspace.packages
    ~fn:(fun (manifest: Package_manifest.t) ->
      let _ = HashMap.insert table ~key:manifest.name ~value:manifest in
      ());
  table

let workspace_member_names = fun (workspace: Workspace.t) ->
  workspace.packages
  |> List.filter ~fn:Package_manifest.is_workspace_member
  |> List.map ~fn:(fun (manifest: Package_manifest.t) -> manifest.name)

let selected_roots = fun workspace roots ->
  match roots with
  | Some roots -> roots
  | None -> workspace_member_names workspace

let is_well_known_package = fun name ->
  Package.is_builtin_dependency_name
    (Package_name.to_string name)

let target_key = fun ~package ~artifact ~target ~profile ->
  Build_unit.{
    package;
    artifact;
    target;
    profile;
  }

let package_for_library = fun workspace (manifest: Package_manifest.t) ->
  Workspace.realize_package ~intent:Package.Runtime manifest
  |> Package.for_scope Package.Normal

let package_for_artifact = fun workspace (manifest: Package_manifest.t) artifact ->
  match artifact with
  | Build_unit.Library -> Some (package_for_library workspace manifest)
  | RuntimeBinary { name } ->
      Workspace.realize_package ~intent:Package.Runtime manifest
      |> Package.for_binary ~binary_name:name
  | TestBinary { name }
  | ExampleBinary { name }
  | BenchBinary { name } ->
      Workspace.realize_package ~intent:Package.Dev manifest
      |> Package.for_binary ~binary_name:name
  | SyntheticTool _ ->
      Workspace.realize_package ~intent:Package.Runtime manifest
      |> Package.for_scope Package.Normal
      |> Option.some

let add_unit = fun t ~(package:Package.t) ~artifact ~target ~profile ->
  let key = target_key ~package:package.name ~artifact ~target ~profile in
  let package_key = Build_unit.package_key key in
  match HashMap.get t.nodes ~key:package_key with
  | Some node -> node
  | None ->
      let node = G.add_node t.graph Build_unit.{ key; package } in
      ignore (HashMap.insert t.nodes ~key:package_key ~value:node);
      node

let add_package_artifact = fun
  t ~workspace ~(manifest:Package_manifest.t) ~artifact ~target ~profile ->
  match package_for_artifact workspace manifest artifact with
  | Some package -> Some (add_unit t ~package ~artifact ~target ~profile)
  | None -> None

let edge_key = fun from_key to_key ->
  Package.key_of_string
    (Build_unit.key_to_string from_key ^ "->" ^ Build_unit.key_to_string to_key)

let add_edge = fun t node ~depends_on ->
  let key = edge_key node.G.value.Build_unit.key depends_on.G.value.Build_unit.key in
  if HashSet.insert t.edges ~value:key then
    G.add_edge node ~depends_on

let binary_artifact_kind = fun (binary: Package.binary) ->
  let path = Path.to_string binary.path in
  if String.starts_with ~prefix:"tests/" path then
    Build_unit.TestBinary { name = binary.name }
  else if String.starts_with ~prefix:"examples/" path then
    Build_unit.ExampleBinary { name = binary.name }
  else if String.starts_with ~prefix:"bench/" path then
    Build_unit.BenchBinary { name = binary.name }
  else
    Build_unit.RuntimeBinary { name = binary.name }

let dev_artifact_enabled = fun (dev_artifacts: Package.dev_artifacts) artifact ->
  match artifact with
  | Build_unit.TestBinary _ -> dev_artifacts.tests
  | ExampleBinary _ -> dev_artifacts.examples
  | BenchBinary _ -> dev_artifacts.benches
  | RuntimeBinary _
  | Library
  | SyntheticTool _ -> false

let root_artifacts = fun request_kind (manifest: Package_manifest.t) ->
  let package =
    match request_kind with
    | Runtime -> Package_manifest.realize ~intent:Package.Runtime manifest
    | Dev _ -> Package_manifest.realize ~intent:Package.Dev manifest
  in
  let artifacts = Vector.with_capacity ~size:(1 + List.length package.binaries) in
  (
    match package.library with
    | Some _ -> Vector.push artifacts ~value:Build_unit.Library
    | None -> ()
  );
  List.for_each
    package.binaries
    ~fn:(fun binary ->
      let artifact = binary_artifact_kind binary in
      match artifact with
      | Build_unit.RuntimeBinary _ -> Vector.push artifacts ~value:artifact
      | TestBinary _
      | ExampleBinary _
      | BenchBinary _ -> (
          match request_kind with
          | Runtime -> ()
          | Dev dev_artifacts ->
              if dev_artifact_enabled dev_artifacts artifact then
                Vector.push artifacts ~value:artifact
        )
      | Library
      | SyntheticTool _ -> ());
  Array.to_list (Vector.to_array artifacts)

let dependency_names = fun (dependencies: Package.dependency list) ->
  List.map
    dependencies
    ~fn:(fun (dependency: Package.dependency) -> dependency.name)

let create workspace request =
  let manifests = package_table workspace in
  let graph = G.make () in
  let nodes = HashMap.with_capacity ~size:128 in
  let edges = HashSet.with_capacity ~size:256 in
  let processed_libraries = HashSet.with_capacity ~size:128 in
  let t = {
    graph;
    nodes;
    edges;
    processed_libraries;
  }
  in
  let missing = Vector.with_capacity ~size:4 in
  let missing_seen = HashSet.with_capacity ~size:4 in
  let host_target = Target.host () in
  let report_missing missing_package =
    let key =
      match missing_package with
      | Root package -> Package.key_of_string ("root:" ^ Package_name.to_string package)
      | Dependency { package; dependency } ->
          Package.key_of_string
            ("dependency:"
            ^ Package_name.to_string package
            ^ "->"
            ^ Package_name.to_string dependency)
    in
    if HashSet.insert missing_seen ~value:key then
      Vector.push missing ~value:missing_package
  in
  let lookup_dependency ~package dependency =
    match HashMap.get manifests ~key:dependency with
    | Some manifest -> Some manifest
    | None ->
        if not (is_well_known_package dependency) then
          report_missing (Dependency { package; dependency });
        None
  in
  let lookup_root root =
    match HashMap.get manifests ~key:root with
    | Some manifest -> Some manifest
    | None ->
        if not (is_well_known_package root) then
          report_missing (Root root);
        None
  in
  let rec require_library = fun package_name target ->
    match HashMap.get manifests ~key:package_name with
    | None ->
        if is_well_known_package package_name then
          None
        else (
          report_missing (Root package_name);
          None
        )
    | Some manifest -> (
        match manifest.library with
        | None -> None
        | Some _ ->
            let key =
              target_key
                ~package:manifest.name
                ~artifact:Build_unit.Library
                ~target
                ~profile:request.profile
            in
            let node =
              add_package_artifact
                t
                ~workspace
                ~manifest
                ~artifact:Build_unit.Library
                ~target
                ~profile:request.profile
            in
            match node with
            | None -> None
            | Some node ->
                let processed_key = Build_unit.package_key key in
                if HashSet.insert t.processed_libraries ~value:processed_key then (
                  List.for_each
                    manifest.dependencies
                    ~fn:(fun dependency ->
                      match lookup_dependency ~package:manifest.name dependency.name with
                      | None -> ()
                      | Some _ ->
                          require_library dependency.name target
                          |> Option.for_each
                            ~fn:(fun dependency_node ->
                              add_edge t node ~depends_on:dependency_node));
                  ()
                );
                Some node
      )
  in
  let add_dependency_edges = fun node (manifest: Package_manifest.t) target dependency_names ->
    List.for_each
      dependency_names
      ~fn:(fun dependency ->
        match lookup_dependency ~package:manifest.name dependency with
        | None -> ()
        | Some _ ->
            require_library dependency target
            |> Option.for_each
              ~fn:(fun dependency_node ->
                add_edge t node ~depends_on:dependency_node))
  in
  let add_build_dependency_edges = fun node (manifest: Package_manifest.t) ->
    List.for_each
      manifest.build_dependencies
      ~fn:(fun dependency ->
        match lookup_dependency ~package:manifest.name dependency.name with
        | None -> ()
        | Some _ ->
            require_library dependency.name host_target
            |> Option.for_each
              ~fn:(fun dependency_node ->
                add_edge t node ~depends_on:dependency_node))
  in
  let require_root_artifact = fun (manifest: Package_manifest.t) target artifact ->
    match artifact with
    | Build_unit.Library -> ignore (require_library manifest.name target)
    | RuntimeBinary _
    | TestBinary _
    | ExampleBinary _
    | BenchBinary _
    | SyntheticTool _ ->
        match add_package_artifact t ~workspace ~manifest ~artifact ~target ~profile:request.profile with
        | None -> ()
        | Some node ->
            require_library manifest.name target
            |> Option.for_each ~fn:(fun library_node -> add_edge t node ~depends_on:library_node);
            add_dependency_edges node manifest target (dependency_names manifest.dependencies);
            (
              match artifact with
              | TestBinary _
              | ExampleBinary _
              | BenchBinary _ ->
                  add_dependency_edges
                    node
                    manifest
                    target
                    (dependency_names manifest.dev_dependencies)
              | RuntimeBinary _
              | Library
              | SyntheticTool _ -> ()
            );
  in
  let require_synthetic_tool = fun (synthetic: synthetic_tool) ->
    match lookup_root synthetic.package with
    | None -> ()
    | Some manifest ->
        let node =
          add_package_artifact
            t
            ~workspace
            ~manifest
            ~artifact:(Build_unit.SyntheticTool { name = synthetic.name })
            ~target:host_target
            ~profile:request.profile
        in
        match node with
        | None -> ()
        | Some node ->
            require_library synthetic.package host_target
            |> Option.for_each ~fn:(fun library_node -> add_edge t node ~depends_on:library_node);
            add_dependency_edges node manifest host_target (dependency_names manifest.dependencies);
            add_build_dependency_edges node manifest
  in
  let roots = selected_roots workspace request.roots in
  List.for_each
    roots
    ~fn:(fun root ->
      match lookup_root root with
      | None -> ()
      | Some manifest ->
          List.for_each
            request.targets
            ~fn:(fun target ->
              List.for_each
                (root_artifacts request.kind manifest)
                ~fn:(require_root_artifact manifest target)));
  List.for_each request.synthetic_tools ~fn:require_synthetic_tool;
  if not (Vector.is_empty missing) then
    Error (MissingPackages { missing = Array.to_list (Vector.to_array missing) })
  else
    Ok t
