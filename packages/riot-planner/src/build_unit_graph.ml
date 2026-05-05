open Std
open Std.Collections
open Std.Result.Syntax
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

type unit_spec = {
  manifest: Package_manifest.t;
  package: Package.t;
  artifact: Build_unit.artifact_kind;
  target: Target.t;
  profile: Profile.t;
}

type unit_ref = {
  ref_manifest: Package_manifest.t;
  ref_artifact: Build_unit.artifact_kind;
  ref_target: Target.t;
  ref_profile: Profile.t;
}

type root_realization = {
  realized_intent: Package.realization_intent;
  realized_package: Package.t;
}

type root_artifact_discovery = {
  root_manifest: Package_manifest.t;
  root_realization: root_realization;
  root_artifacts: Build_unit.artifact_kind list;
}

type unit_ref_bucket = {
  bucket_manifest: Package_manifest.t;
  bucket_refs: unit_ref Vector.t;
}

type unit_ref_group = {
  group_manifest: Package_manifest.t;
  group_refs: unit_ref list;
  group_realization: root_realization option;
  group_source_ignore_patterns: string list;
}

type create_context = {
  workspace: Workspace.t;
  request: request;
  manifests: (Package_name.t, Package_manifest.t) HashMap.t;
  host_target: Target.t;
  missing: missing_package Vector.t;
  missing_seen: Package.key HashSet.t;
}

type node_plan = {
  units: unit_spec list;
  planned_libraries: graph_key HashSet.t;
  missing_packages: missing_package list;
}

type node_task =
  | RootTask of Package_manifest.t
  | SyntheticTask of {
      synthetic: synthetic_tool;
      manifest: Package_manifest.t;
    }

type library_seed = {
  seed_manifest: Package_manifest.t;
  seed_target: Target.t;
}

type root_node_result = {
  result_unit_refs: unit_ref list;
  result_library_seeds: library_seed list;
  result_missing_packages: missing_package list;
  result_realization: (Package_name.t * root_realization) option;
}

type library_node_result = {
  library_unit_refs: unit_ref list;
  library_seeds: library_seed list;
  library_missing_packages: missing_package list;
}

type root_node_collector = {
  root_context: create_context;
  root_unit_refs: unit_ref Vector.t;
  root_library_seeds: library_seed Vector.t;
  root_library_seed_keys: graph_key HashSet.t;
  root_missing: missing_package Vector.t;
  root_missing_seen: Package.key HashSet.t;
}

type t = {
  graph: Build_unit.t G.t;
  nodes: (graph_key, Build_unit.t G.node) HashMap.t;
  edges: edge_key HashSet.t;
  processed_libraries: graph_key HashSet.t;
}

type node = Build_unit.t G.node

let trace_enabled = fun () ->
  match Env.get Env.String ~var:"RIOT_PLANNER_TRACE" with
  | Some ("1" | "true" | "yes") -> true
  | _ -> false

let trace = fun message ->
  if trace_enabled () then
    eprintln ("riot-planner build-unit-graph " ^ message)

let trace_probe = fun ~started_at message ->
  if trace_enabled () then
    let duration =
      Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ())
      |> Time.Duration.to_micros
    in
    trace (message ^ "_us=" ^ Int.to_string duration)

let size = fun t ->
  let count = ref 0 in
  G.iter t.graph ~fn:(fun _ _ -> count := !count + 1);
  !count

let keys = fun t ->
  G.map t.graph ~fn:(fun (_, node) -> Build_unit.key (G.value node))
  |> List.sort ~compare:Build_unit.compare_key

let graph_key_from_build_key = Build_unit.id_of_key

let graph_key_to_string = fun (key:graph_key) ->
  Crypto.Digest.hex key

let find = fun t key -> HashMap.get t.nodes ~key:(graph_key_from_build_key key)

let node_value = fun node -> G.value node

let dependencies = fun t key ->
  match find t key with
  | None -> []
  | Some node ->
      G.deps node
      |> List.filter_map
        ~fn:(fun node_id ->
          G.get_node t.graph node_id
          |> Option.map ~fn:(fun node -> Build_unit.key (G.value node)))
      |> List.sort ~compare:Build_unit.compare_key

let topological_sort = fun t ->
  match G.topo_sort t.graph with
  | Ok nodes -> Ok (List.map nodes ~fn:(fun node -> G.value node))
  | Error node_ids ->
      Error (
        List.filter_map
          node_ids
          ~fn:(fun node_id ->
            G.get_node t.graph node_id
            |> Option.map ~fn:(fun node -> Build_unit.key (G.value node)))
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

let edge_key = fun from_ to_ -> { from_; to_ }

let add_edge = fun t node ~depends_on ->
  let key = edge_key (Build_unit.id (G.value node)) (Build_unit.id (G.value depends_on)) in
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

let request_intent = fun __tmp1 ->
  match __tmp1 with
  | Runtime -> Package.Runtime
  | Dev _ -> Package.Dev

let artifacts_for_package = fun request_kind (package: Package.t) ->
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

let discover_root_artifacts = fun request_kind source_ignore_patterns (manifest: Package_manifest.t) ->
  let intent = request_intent request_kind in
  let package = Package_manifest.realize ~intent ~source_ignore_patterns manifest in
  {
    root_manifest = manifest;
    root_realization = {
      realized_intent = intent;
      realized_package = package;
    };
    root_artifacts = artifacts_for_package request_kind package;
  }

let dependency_names = fun (dependencies: Package.dependency list) ->
  List.map
    dependencies
    ~fn:(fun (dependency: Package.dependency) -> dependency.name)

let unit_ref_key = fun (unit_ref: unit_ref) ->
  target_key
    ~package:unit_ref.ref_manifest.name
    ~artifact:unit_ref.ref_artifact
    ~target:unit_ref.ref_target
    ~profile:unit_ref.ref_profile

let library_seed_key = fun (ctx: create_context) (seed: library_seed) ->
  graph_key_from_build_key
    (target_key
      ~package:seed.seed_manifest.name
      ~artifact:Build_unit.Library
      ~target:seed.seed_target
      ~profile:ctx.request.profile)

let merge_unit_ref = fun unit_refs unit_keys unit_ref ->
  let graph_key = graph_key_from_build_key (unit_ref_key unit_ref) in
  if HashSet.insert unit_keys ~value:graph_key then
    Vector.push unit_refs ~value:unit_ref;
  graph_key

let artifact_needs_dev_realization = fun __tmp1 ->
  match __tmp1 with
  | Build_unit.TestBinary _
  | ExampleBinary _
  | BenchBinary _ -> true
  | Library
  | RuntimeBinary _
  | SyntheticTool _ -> false

let group_unit_refs = fun unit_refs root_realizations source_ignore_patterns ->
  let buckets = HashMap.with_capacity ~size:(List.length unit_refs) in
  List.for_each
    unit_refs
    ~fn:(fun unit_ref ->
      let bucket =
        match HashMap.get buckets ~key:unit_ref.ref_manifest.name with
        | Some bucket -> bucket
        | None ->
            let bucket = {
              bucket_manifest = unit_ref.ref_manifest;
              bucket_refs = Vector.with_capacity ~size:4;
            }
            in
            ignore (HashMap.insert buckets ~key:unit_ref.ref_manifest.name ~value:bucket);
            bucket
      in
      Vector.push bucket.bucket_refs ~value:unit_ref);
  HashMap.to_list buckets
  |> List.map
    ~fn:(fun (package_name, bucket) -> {
      group_manifest = bucket.bucket_manifest;
      group_refs = Vector.to_array bucket.bucket_refs |> Array.to_list;
      group_realization = HashMap.get root_realizations ~key:package_name;
      group_source_ignore_patterns = source_ignore_patterns;
    })

let realize_unit_group = fun (group: unit_ref_group) ->
  let needs_dev =
    List.any group.group_refs ~fn:(fun unit_ref ->
      artifact_needs_dev_realization unit_ref.ref_artifact)
  in
  let realized_package =
    match group.group_realization with
    | Some { realized_intent = Package.Dev; realized_package } -> realized_package
    | Some { realized_intent = Package.Runtime; realized_package } when not needs_dev ->
        realized_package
    | _ ->
        Package_manifest.realize
          ~intent:(if needs_dev then Package.Dev else Package.Runtime)
          ~source_ignore_patterns:group.group_source_ignore_patterns
          group.group_manifest
  in
  let artifact_packages = HashMap.with_capacity ~size:(List.length group.group_refs) in
  let projection_cache = Package.make_projection_cache realized_package in
  let runtime_projected = ref None in
  let runtime_package = fun () ->
    match !runtime_projected with
    | Some package -> package
    | None ->
        let package = Package.for_scope Package.Normal realized_package in
        runtime_projected := Some package;
        package
  in
  let package_for_artifact artifact =
    match HashMap.get artifact_packages ~key:artifact with
    | Some package -> package
    | None ->
        let package =
          match artifact with
          | Build_unit.Library
          | SyntheticTool _ -> Some (runtime_package ())
          | RuntimeBinary { name }
          | TestBinary { name }
          | ExampleBinary { name }
          | BenchBinary { name } ->
              Package.for_binary_with_projection_cache projection_cache ~binary_name:name
        in
        ignore (HashMap.insert artifact_packages ~key:artifact ~value:package);
        package
  in
  List.filter_map
    group.group_refs
    ~fn:(fun unit_ref ->
      match package_for_artifact unit_ref.ref_artifact with
      | None -> None
      | Some package ->
          Some {
            manifest = unit_ref.ref_manifest;
            package;
            artifact = unit_ref.ref_artifact;
            target = unit_ref.ref_target;
            profile = unit_ref.ref_profile;
          })

let create_context = fun (workspace: Workspace.t) (request: request) ->
  let package_table_started_at = Time.Instant.now () in
  let manifests = package_table workspace in
  trace_probe ~started_at:package_table_started_at "package_table";
  {
    workspace;
    request;
    manifests;
    host_target = Target.host ();
    missing = Vector.with_capacity ~size:4;
    missing_seen = HashSet.with_capacity ~size:4;
  }

let missing_package_key = fun __tmp1 ->
  match __tmp1 with
  | Root package -> Package.key_of_string ("root:" ^ Package_name.to_string package)
  | Dependency { package; dependency } ->
      Package.key_of_string
        ("dependency:"
        ^ Package_name.to_string package
        ^ "->"
        ^ Package_name.to_string dependency)

let report_missing = fun (ctx: create_context) missing_package ->
  let key = missing_package_key missing_package in
  if HashSet.insert ctx.missing_seen ~value:key then
    Vector.push ctx.missing ~value:missing_package

let lookup_root = fun (ctx: create_context) root ->
  match HashMap.get ctx.manifests ~key:root with
  | Some manifest -> Some manifest
  | None ->
      if not (is_well_known_package root) then
        report_missing ctx (Root root);
      None

let missing_list = fun (ctx: create_context) ->
  Vector.to_array ctx.missing |> Array.to_list

let create_root_node_collector = fun ctx -> {
  root_context = ctx;
  root_unit_refs = Vector.with_capacity ~size:16;
  root_library_seeds = Vector.with_capacity ~size:16;
  root_library_seed_keys = HashSet.with_capacity ~size:16;
  root_missing = Vector.with_capacity ~size:2;
  root_missing_seen = HashSet.with_capacity ~size:2;
}

let root_report_missing = fun collector missing_package ->
  let key = missing_package_key missing_package in
  if HashSet.insert collector.root_missing_seen ~value:key then
    Vector.push collector.root_missing ~value:missing_package

let root_lookup_dependency = fun collector ~package dependency ->
  match HashMap.get collector.root_context.manifests ~key:dependency with
  | Some manifest -> Some manifest
  | None ->
      if not (is_well_known_package dependency) then
        root_report_missing collector (Dependency { package; dependency });
      None

let root_add_unit_ref = fun collector ~manifest ~artifact ~target ->
  Vector.push
    collector.root_unit_refs
    ~value:{
      ref_manifest = manifest;
      ref_artifact = artifact;
      ref_target = target;
      ref_profile = collector.root_context.request.profile;
    }

let root_add_library_seed = fun collector (manifest: Package_manifest.t) target ->
  match manifest.library with
  | None -> None
  | Some _ ->
      let seed = {
        seed_manifest = manifest;
        seed_target = target;
      }
      in
      let key = library_seed_key collector.root_context seed in
      if HashSet.insert collector.root_library_seed_keys ~value:key then
        Vector.push collector.root_library_seeds ~value:seed;
      Some key

let root_collect_dependency_library_seeds = fun collector (manifest: Package_manifest.t) target dependency_names ->
  List.for_each
    dependency_names
    ~fn:(fun dependency ->
      match root_lookup_dependency collector ~package:manifest.name dependency with
      | None -> ()
      | Some dependency_manifest ->
          ignore (root_add_library_seed collector dependency_manifest target))

let root_collect_build_dependency_library_seeds = fun collector (manifest: Package_manifest.t) ->
  List.for_each
    manifest.build_dependencies
    ~fn:(fun dependency ->
      match root_lookup_dependency collector ~package:manifest.name dependency.name with
      | None -> ()
      | Some dependency_manifest ->
          ignore
            (root_add_library_seed
              collector
              dependency_manifest
              collector.root_context.host_target))

let root_collect_target_artifacts = fun collector (manifest: Package_manifest.t) target artifacts ->
  let needs_runtime_dependencies = ref false in
  let needs_dev_dependencies = ref false in
  List.for_each
    artifacts
    ~fn:(fun artifact ->
      match artifact with
      | Build_unit.Library -> ignore (root_add_library_seed collector manifest target)
      | RuntimeBinary _
      | TestBinary _
      | ExampleBinary _
      | BenchBinary _
      | SyntheticTool _ ->
          ignore (root_add_unit_ref collector ~manifest ~artifact ~target);
          needs_runtime_dependencies := true;
          if artifact_needs_dev_realization artifact then
            needs_dev_dependencies := true);
  if !needs_runtime_dependencies then (
    ignore (root_add_library_seed collector manifest target);
    root_collect_dependency_library_seeds
      collector
      manifest
      target
      (dependency_names manifest.dependencies);
    if !needs_dev_dependencies then
      root_collect_dependency_library_seeds
        collector
        manifest
        target
        (dependency_names manifest.dev_dependencies)
  )

let root_collector_result = fun collector ~realization -> {
  result_unit_refs = Vector.to_array collector.root_unit_refs |> Array.to_list;
  result_library_seeds = Vector.to_array collector.root_library_seeds |> Array.to_list;
  result_missing_packages = Vector.to_array collector.root_missing |> Array.to_list;
  result_realization = realization;
}

let library_collector_result = fun collector -> {
  library_unit_refs = Vector.to_array collector.root_unit_refs |> Array.to_list;
  library_seeds = Vector.to_array collector.root_library_seeds |> Array.to_list;
  library_missing_packages = Vector.to_array collector.root_missing |> Array.to_list;
}

let create_root_nodes = fun ctx __tmp1 ->
  match __tmp1 with
  | RootTask manifest ->
      let discovery =
        discover_root_artifacts
          ctx.request.kind
          ctx.workspace.source_ignore_patterns
          manifest
      in
      let collector = create_root_node_collector ctx in
      List.for_each
        ctx.request.targets
        ~fn:(fun target ->
          root_collect_target_artifacts
            collector
            discovery.root_manifest
            target
            discovery.root_artifacts);
      root_collector_result
        collector
        ~realization:(Some (discovery.root_manifest.name, discovery.root_realization))
  | SyntheticTask { synthetic; manifest } ->
      let collector = create_root_node_collector ctx in
      ignore (root_add_unit_ref
        collector
        ~manifest
        ~artifact:(Build_unit.SyntheticTool { name = synthetic.name })
        ~target:ctx.host_target);
      ignore (root_add_library_seed collector manifest ctx.host_target);
      root_collect_build_dependency_library_seeds collector manifest;
      root_collector_result collector ~realization:None

let create_library_nodes = fun ctx seed ->
  let collector = create_root_node_collector ctx in
  ignore
    (root_add_unit_ref
      collector
      ~manifest:seed.seed_manifest
      ~artifact:Build_unit.Library
      ~target:seed.seed_target);
  root_collect_dependency_library_seeds
    collector
    seed.seed_manifest
    seed.seed_target
    (dependency_names seed.seed_manifest.dependencies);
  library_collector_result collector

let node_tasks = fun ctx ->
  let root_tasks =
    selected_roots ctx.workspace ctx.request.roots
    |> List.filter_map
      ~fn:(fun root ->
        match lookup_root ctx root with
        | None -> None
        | Some manifest -> Some (RootTask manifest))
  in
  let synthetic_tasks =
    List.filter_map
      ctx.request.synthetic_tools
      ~fn:(fun synthetic ->
        match lookup_root ctx synthetic.package with
        | None -> None
        | Some manifest -> Some (SyntheticTask { synthetic; manifest }))
  in
  root_tasks @ synthetic_tasks

let enqueue_library_seed = fun ctx planned_libraries frontier seed ->
  let key = library_seed_key ctx seed in
  if HashSet.insert planned_libraries ~value:key then
    Vector.push frontier ~value:seed

let merge_root_node_result = fun ctx unit_refs unit_keys planned_libraries library_frontier root_realizations result ->
  List.for_each result.result_missing_packages ~fn:(report_missing ctx);
  List.for_each
    result.result_unit_refs
    ~fn:(fun unit_ref -> ignore (merge_unit_ref unit_refs unit_keys unit_ref));
  List.for_each
    result.result_library_seeds
    ~fn:(enqueue_library_seed ctx planned_libraries library_frontier);
  match result.result_realization with
  | None -> ()
  | Some (package_name, realization) ->
      ignore (HashMap.insert root_realizations ~key:package_name ~value:realization)

let merge_library_node_result = fun ctx unit_refs unit_keys planned_libraries next_frontier result ->
  List.for_each result.library_missing_packages ~fn:(report_missing ctx);
  List.for_each
    result.library_unit_refs
    ~fn:(fun unit_ref -> ignore (merge_unit_ref unit_refs unit_keys unit_ref));
  List.for_each
    result.library_seeds
    ~fn:(enqueue_library_seed ctx planned_libraries next_frontier)

let rec expand_library_frontier_at_depth = fun depth ctx unit_refs unit_keys planned_libraries frontier ->
  let seeds = Vector.to_array frontier |> Array.to_list in
  match seeds with
  | [] -> ()
  | _ ->
      let started_at = Time.Instant.now () in
      let next_frontier = Vector.with_capacity ~size:(List.length seeds) in
      WorkerPool.SimpleWorkerPool.run
        ~tasks:seeds
        ~fn:(create_library_nodes ctx)
        ()
      |> List.for_each
        ~fn:(fun (_index, result) ->
          merge_library_node_result
            ctx
            unit_refs
            unit_keys
            planned_libraries
            next_frontier
            result);
      trace_probe
        ~started_at
        ("library_frontier depth="
        ^ Int.to_string depth
        ^ " seeds="
        ^ Int.to_string (List.length seeds)
        ^ " next="
        ^ Int.to_string (Vector.length next_frontier));
      expand_library_frontier_at_depth
        (depth + 1)
        ctx
        unit_refs
        unit_keys
        planned_libraries
        next_frontier

let expand_library_frontier = fun ctx unit_refs unit_keys planned_libraries frontier ->
  expand_library_frontier_at_depth 0 ctx unit_refs unit_keys planned_libraries frontier

let create_unit_specs = fun unit_groups ->
  WorkerPool.SimpleWorkerPool.run
    ~tasks:unit_groups
    ~fn:realize_unit_group
    ()
  |> List.flat_map ~fn:(fun (_index, specs) -> specs)

let create_nodes = fun (ctx: create_context) ->
  let collect_started_at = Time.Instant.now () in
  let unit_refs = Vector.with_capacity ~size:128 in
  let unit_keys = HashSet.with_capacity ~size:128 in
  let planned_libraries = HashSet.with_capacity ~size:128 in
  let library_frontier = Vector.with_capacity ~size:128 in
  let root_realizations = HashMap.with_capacity ~size:128 in
  let tasks = node_tasks ctx in
  let root_started_at = Time.Instant.now () in
  WorkerPool.SimpleWorkerPool.run
    ~tasks
    ~fn:(create_root_nodes ctx)
    ()
  |> List.for_each
    ~fn:(fun (_index, result) ->
      merge_root_node_result
        ctx
        unit_refs
        unit_keys
        planned_libraries
        library_frontier
        root_realizations
        result);
  trace_probe
    ~started_at:root_started_at
    ("root_nodes tasks="
    ^ Int.to_string (List.length tasks)
    ^ " frontier="
    ^ Int.to_string (Vector.length library_frontier)
    ^ " refs="
    ^ Int.to_string (Vector.length unit_refs));
  let library_started_at = Time.Instant.now () in
  expand_library_frontier ctx unit_refs unit_keys planned_libraries library_frontier;
  trace_probe
    ~started_at:library_started_at
    ("library_nodes libraries="
    ^ Int.to_string (HashSet.length planned_libraries)
    ^ " refs="
    ^ Int.to_string (Vector.length unit_refs));
  let group_started_at = Time.Instant.now () in
  let unit_groups =
    group_unit_refs
      (Vector.to_array unit_refs |> Array.to_list)
      root_realizations
      ctx.workspace.source_ignore_patterns
  in
  trace_probe
    ~started_at:group_started_at
    ("group_units groups=" ^ Int.to_string (List.length unit_groups));
  let spec_started_at = Time.Instant.now () in
  let unit_specs = create_unit_specs unit_groups in
  trace_probe
    ~started_at:spec_started_at
    ("realize_units units=" ^ Int.to_string (List.length unit_specs));
  trace_probe
    ~started_at:collect_started_at
    ("collect_units units="
    ^ Int.to_string (List.length unit_specs)
    ^ " unique="
    ^ Int.to_string (HashSet.length unit_keys)
    ^ " libraries="
    ^ Int.to_string (HashSet.length planned_libraries)
    ^ " groups="
    ^ Int.to_string (List.length unit_groups)
    ^ " roots="
    ^ Int.to_string (List.length tasks)
    ^ " missing="
    ^ Int.to_string (Vector.length ctx.missing));
  Ok {
    units = unit_specs;
    planned_libraries;
    missing_packages = missing_list ctx;
  }

let wire_dependencies = fun (ctx: create_context) node_plan ->
  let graph_init_started_at = Time.Instant.now () in
  let graph = G.make () in
  let nodes = HashMap.with_capacity ~size:(List.length node_plan.units) in
  let edges = HashSet.with_capacity ~size:256 in
  let t = {
    graph;
    nodes;
    edges;
    processed_libraries = node_plan.planned_libraries;
  }
  in
  trace_probe ~started_at:graph_init_started_at "graph_init";
  let add_nodes_started_at = Time.Instant.now () in
  List.for_each node_plan.units ~fn:(add_graph_unit t);
  trace_probe ~started_at:add_nodes_started_at "add_nodes";
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
    match HashMap.get ctx.manifests ~key:dependency_name with
    | None ->
        ()
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
                  ~profile:ctx.request.profile)
            in
            if HashMap.has_key t.nodes ~key:dependency_key then
              add_required_edge from_key dependency_key
      )
  in
  let add_library_edges = fun from_key (_manifest: Package_manifest.t) target dependency_names ->
    List.for_each
      dependency_names
      ~fn:(fun dependency -> add_library_edge from_key dependency target)
  in
  let wire_edges_started_at = Time.Instant.now () in
  List.for_each
    node_plan.units
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
  trace_probe
    ~started_at:wire_edges_started_at
    ("wire_edges edges=" ^ Int.to_string (HashSet.length t.edges));
  Ok t

let validate_graph = fun node_plan _graph ->
  match node_plan.missing_packages with
  | [] -> Ok ()
  | missing -> Error (MissingPackages { missing })

let create (workspace: Workspace.t) (request: request) =
  let create_started_at = Time.Instant.now () in
  let ctx = create_context workspace request in
  let* nodes = create_nodes ctx in
  let* graph = wire_dependencies ctx nodes in
  let* () = validate_graph nodes graph in
  trace_probe
    ~started_at:create_started_at
    ("create_total units="
    ^ Int.to_string (List.length nodes.units)
    ^ " edges="
    ^ Int.to_string (HashSet.length graph.edges));
  Ok graph
