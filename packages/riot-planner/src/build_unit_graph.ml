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

type graph_key = Build_unit.id

type edge_key = {
  from_: graph_key;
  to_: graph_key;
}

type artifact_cache_key = {
  artifact_package: Package_name.t;
  artifact: Build_unit.artifact_kind;
}

type unit_spec = {
  manifest: Package_manifest.t;
  package: Package.t;
  artifact: Build_unit.artifact_kind;
  target: Target.t;
  profile: Profile.t;
}

type t = {
  graph: Build_unit.t G.t;
  nodes: (graph_key, Build_unit.t G.node) HashMap.t;
  edges: edge_key HashSet.t;
  processed_libraries: graph_key HashSet.t;
}

type node = Build_unit.t G.node

let size = fun t ->
  let count = ref 0 in
  G.iter t.graph ~fn:(fun _ _ -> count := !count + 1);
  !count

let keys = fun t ->
  G.map t.graph ~fn:(fun (_, node) -> Build_unit.key node.G.value)
  |> List.sort ~compare:Build_unit.compare_key

let graph_key_from_build_key = Build_unit.id_of_key

let graph_key_to_string = fun (key:graph_key) ->
  Crypto.Digest.hex key

let find = fun t key -> HashMap.get t.nodes ~key:(graph_key_from_build_key key)

let node_value = fun node -> node.G.value

let dependencies = fun t key ->
  match find t key with
  | None -> []
  | Some node ->
      node.G.deps
      |> List.filter_map
        ~fn:(fun node_id ->
          G.get_node t.graph node_id
          |> Option.map ~fn:(fun node -> Build_unit.key node.G.value))
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
            |> Option.map ~fn:(fun node -> Build_unit.key node.G.value))
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

let target_key = fun ~(package:Package_name.t) ~(artifact:Build_unit.artifact_kind) ~(target:Target.t) ~(profile:Profile.t) ->
  let package_name:Package_name.t = package in
  let artifact_kind:Build_unit.artifact_kind = artifact in
  let target_value:Target.t = target in
  let profile_value:Profile.t = profile in
  ({
    package = package_name;
    artifact = artifact_kind;
    target = target_value;
    profile = profile_value;
  }:Build_unit.key)

type realization_cache = {
  runtime: (Package_name.t, Package.t) HashMap.t;
  dev: (Package_name.t, Package.t) HashMap.t;
  artifact_packages: (artifact_cache_key, Package.t option) HashMap.t;
}

let create_realization_cache = fun () -> {
  runtime = HashMap.with_capacity ~size:128;
  dev = HashMap.with_capacity ~size:128;
  artifact_packages = HashMap.with_capacity ~size:512;
}

let cached_realized_package = fun
  cache ~(intent:Package.realization_intent) (manifest: Package_manifest.t) ->
  let table =
    match intent with
    | Package.Runtime -> cache.runtime
    | Package.Dev -> cache.dev
    | Package.Build
    | Package.Run
    | Package.Test
    | Package.Bench
    | Package.Doc
    | Package.Check -> cache.runtime
  in
  match HashMap.get table ~key:manifest.name with
  | Some package -> package
  | None ->
      let package = Package_manifest.realize ~intent manifest in
      ignore (HashMap.insert table ~key:manifest.name ~value:package);
      package

let package_for_library = fun cache (manifest: Package_manifest.t) ->
  cached_realized_package cache ~intent:Package.Runtime manifest
  |> Package.for_scope Package.Normal

let package_for_artifact = fun cache (manifest: Package_manifest.t) artifact ->
  let key = { artifact_package = manifest.name; artifact } in
  match HashMap.get cache.artifact_packages ~key with
  | Some package -> package
  | None ->
      let package =
        match artifact with
        | Build_unit.Library -> Some (package_for_library cache manifest)
        | RuntimeBinary { name } ->
            cached_realized_package cache ~intent:Package.Runtime manifest
            |> Package.for_binary ~binary_name:name
        | TestBinary { name }
        | ExampleBinary { name }
        | BenchBinary { name } ->
            cached_realized_package cache ~intent:Package.Dev manifest
            |> Package.for_binary ~binary_name:name
        | SyntheticTool _ ->
            cached_realized_package cache ~intent:Package.Runtime manifest
            |> Package.for_scope Package.Normal
            |> Option.some
      in
      ignore (HashMap.insert cache.artifact_packages ~key ~value:package);
      package

let unit_spec_key = fun (spec: unit_spec) ->
  target_key ~package:spec.package.name ~artifact:spec.artifact ~target:spec.target ~profile:spec.profile

let add_graph_unit = fun t (spec: unit_spec) ->
  let key = unit_spec_key spec in
  let graph_key = graph_key_from_build_key key in
  if HashMap.has_key t.nodes ~key:graph_key then
    panic ("duplicate build unit during graph creation: " ^ Build_unit.key_to_string key)
  else
    let node =
      G.add_node
        t.graph
        (Build_unit.from_artifact
          ~package:spec.package
          ~artifact:spec.artifact
          ~target:spec.target
          ~profile:spec.profile)
    in
    ignore (HashMap.insert t.nodes ~key:graph_key ~value:node)

let add_collected_unit = fun
  unit_specs unit_keys ~(manifest:Package_manifest.t) ~(package:Package.t) ~artifact ~target ~profile ->
  let key = target_key ~package:package.name ~artifact ~target ~profile in
  let graph_key = graph_key_from_build_key key in
  if HashSet.insert unit_keys ~value:graph_key then
    Vector.push unit_specs ~value:{ manifest; package; artifact; target; profile };
  graph_key

let collect_package_artifact = fun
  unit_specs unit_keys ~cache ~(manifest:Package_manifest.t) ~artifact ~target ~profile ->
  match package_for_artifact cache manifest artifact with
  | Some package ->
      Some (
        add_collected_unit
          unit_specs
          unit_keys
          ~manifest
          ~package
          ~artifact
          ~target
          ~profile
      )
  | None -> None

let edge_key = fun from_key to_key -> {
  from_ = graph_key_from_build_key from_key;
  to_ = graph_key_from_build_key to_key;
}

let add_edge = fun t node ~depends_on ->
  let key = edge_key (Build_unit.key node.G.value) (Build_unit.key depends_on.G.value) in
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

let root_artifacts = fun cache request_kind (manifest: Package_manifest.t) ->
  let package =
    match request_kind with
    | Runtime -> cached_realized_package cache ~intent:Package.Runtime manifest
    | Dev _ -> cached_realized_package cache ~intent:Package.Dev manifest
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

let create (workspace: Workspace.t) (request: request) =
  let manifests = package_table workspace in
  let cache = create_realization_cache () in
  let unit_specs = Vector.with_capacity ~size:128 in
  let unit_keys = HashSet.with_capacity ~size:128 in
  let planned_libraries = HashSet.with_capacity ~size:128 in
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
  let rec collect_library = fun package_name target ->
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
            let library_key =
              collect_package_artifact
                unit_specs
                unit_keys
                ~cache
                ~manifest
                ~artifact:Build_unit.Library
                ~target
                ~profile:request.profile
            in
            match library_key with
            | None -> None
            | Some library_key ->
                if HashSet.insert planned_libraries ~value:library_key then (
                  List.for_each
                    manifest.dependencies
                    ~fn:(fun dependency ->
                      match lookup_dependency ~package:manifest.name dependency.name with
                      | None -> ()
                      | Some _ -> ignore (collect_library dependency.name target));
                  ()
                );
                Some library_key
      )
  in
  let collect_dependency_libraries = fun (manifest: Package_manifest.t) target dependency_names ->
    List.for_each
      dependency_names
      ~fn:(fun dependency ->
        match lookup_dependency ~package:manifest.name dependency with
        | None -> ()
        | Some _ -> ignore (collect_library dependency target))
  in
  let collect_build_dependency_libraries = fun (manifest: Package_manifest.t) ->
    List.for_each
      manifest.build_dependencies
      ~fn:(fun dependency ->
        match lookup_dependency ~package:manifest.name dependency.name with
        | None -> ()
        | Some _ -> ignore (collect_library dependency.name host_target))
  in
  let collect_root_artifact = fun (manifest: Package_manifest.t) target artifact ->
    match artifact with
    | Build_unit.Library -> ignore (collect_library manifest.name target)
    | RuntimeBinary _
    | TestBinary _
    | ExampleBinary _
    | BenchBinary _
    | SyntheticTool _ ->
        match collect_package_artifact
          unit_specs
          unit_keys
          ~cache
          ~manifest
          ~artifact
          ~target
          ~profile:request.profile with
        | None -> ()
        | Some _ ->
            ignore (collect_library manifest.name target);
            collect_dependency_libraries manifest target (dependency_names manifest.dependencies);
            (
              match artifact with
              | TestBinary _
              | ExampleBinary _
              | BenchBinary _ ->
                  collect_dependency_libraries manifest target (dependency_names manifest.dev_dependencies)
              | RuntimeBinary _
              | Library
              | SyntheticTool _ -> ()
            );
  in
  let collect_synthetic_tool = fun (synthetic: synthetic_tool) ->
    match lookup_root synthetic.package with
    | None -> ()
    | Some manifest ->
        let tool_key =
          collect_package_artifact
            unit_specs
            unit_keys
            ~cache
            ~manifest
            ~artifact:(Build_unit.SyntheticTool { name = synthetic.name })
            ~target:host_target
            ~profile:request.profile
        in
        match tool_key with
        | None -> ()
        | Some _ ->
            ignore (collect_library synthetic.package host_target);
            collect_build_dependency_libraries manifest
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
                (root_artifacts cache request.kind manifest)
                ~fn:(collect_root_artifact manifest target)));
  List.for_each request.synthetic_tools ~fn:collect_synthetic_tool;
  if not (Vector.is_empty missing) then
    Error (MissingPackages { missing = Array.to_list (Vector.to_array missing) })
  else (
    let graph = G.make () in
    let nodes = HashMap.with_capacity ~size:(Vector.length unit_specs) in
    let edges = HashSet.with_capacity ~size:256 in
    let t = {
      graph;
      nodes;
      edges;
      processed_libraries = planned_libraries;
    }
    in
    let unit_specs = Array.to_list (Vector.to_array unit_specs) in
    List.for_each unit_specs ~fn:(add_graph_unit t);
    let find_required_node = fun key ->
      match HashMap.get t.nodes ~key with
      | Some node -> node
      | None -> panic ("missing build unit during edge wiring: " ^ graph_key_to_string key)
    in
    let add_required_edge = fun from_key to_key ->
      let from_node = find_required_node from_key in
      let to_node = find_required_node to_key in
      add_edge t from_node ~depends_on:to_node
    in
    let add_library_edge = fun from_key dependency_name target ->
      match HashMap.get manifests ~key:dependency_name with
      | None -> ()
      | Some dependency_manifest -> (
          match dependency_manifest.library with
          | None -> ()
          | Some _ ->
              let dependency_key =
                graph_key_from_build_key
                  (target_key
                    ~package:dependency_manifest.name
                    ~artifact:Build_unit.Library
                    ~target
                    ~profile:request.profile)
              in
              if HashMap.has_key t.nodes ~key:dependency_key then
                add_required_edge from_key dependency_key
        )
    in
    let add_library_edges = fun from_key (manifest: Package_manifest.t) target dependency_names ->
      List.for_each
        dependency_names
        ~fn:(fun dependency -> add_library_edge from_key dependency target)
    in
    List.for_each
      unit_specs
      ~fn:(fun spec ->
        let from_key = graph_key_from_build_key (unit_spec_key spec) in
        match spec.artifact with
        | Build_unit.Library ->
            add_library_edges
              from_key
              spec.manifest
              spec.target
              (dependency_names spec.manifest.dependencies)
        | RuntimeBinary _
        | TestBinary _
        | ExampleBinary _
        | BenchBinary _ ->
            add_library_edge from_key spec.manifest.name spec.target;
            add_library_edges
              from_key
              spec.manifest
              spec.target
              (dependency_names spec.manifest.dependencies);
            (
              match spec.artifact with
              | TestBinary _
              | ExampleBinary _
              | BenchBinary _ ->
                  add_library_edges
                    from_key
                    spec.manifest
                    spec.target
                    (dependency_names spec.manifest.dev_dependencies)
              | RuntimeBinary _
              | Library
              | SyntheticTool _ -> ()
            )
        | SyntheticTool _ ->
            add_library_edge from_key spec.manifest.name spec.target;
            List.for_each
              spec.manifest.build_dependencies
              ~fn:(fun dependency -> add_library_edge from_key dependency.name spec.target));
    Ok t
  )
