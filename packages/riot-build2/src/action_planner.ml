open Std
open Std.Collections

module G = Graph.SimpleGraph
module Module_node = Riot_planner.Module_node

type input = {
  package: Riot_model.Package.t;
  profile: Riot_model.Profile.t;
  target: Riot_model.Target.t;
  build_ctx: Riot_model.Build_ctx.t;
  toolchain: Riot_toolchain.t;
  depset: Riot_planner.Dependency.t list;
  sandbox_dir: Path.t;
  module_graph: Module_node.t G.t;
}

let stdlib_flags = fun (package: Riot_model.Package.t) ->
  let has_stdlib_dep =
    List.any
      (Riot_model.Package.build_graph_dependencies package)
      ~fn:(fun (dep: Riot_model.Package.dependency) ->
        Riot_model.Package_name.equal
          dep.name
          (
            Riot_model.Package_name.from_string "stdlib"
            |> Result.expect ~msg:"expected valid package name"
          ))
  in
  if has_stdlib_dep then
    [ Riot_toolchain.Ocamlc.NoPervasives ]
  else
    [ Riot_toolchain.Ocamlc.NoPervasives; Riot_toolchain.Ocamlc.NoStdlib ]

let profile_compile_flags = fun (profile: Riot_model.Profile.t) ->
  let flags = [] in
  let flags =
    if profile.no_alias_deps then
      Riot_toolchain.Ocamlc.NoAliasDeps :: flags
    else
      flags
  in
  let flags =
    match profile.inline with
    | Some threshold -> flags @ [ Riot_toolchain.Ocamlc.Inline threshold ]
    | None -> flags
  in
  let flags =
    if profile.no_assert then
      flags @ [ Riot_toolchain.Ocamlc.NoAssert ]
    else
      flags
  in
  let flags =
    if profile.compact then
      flags @ [ Riot_toolchain.Ocamlc.Compact ]
    else
      flags
  in
  let flags =
    if profile.unsafe then
      flags @ [ Riot_toolchain.Ocamlc.Unsafe ]
    else
      flags
  in
  let flags =
    if List.is_empty profile.warnings then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.Warning profile.warnings ]
  in
  let flags =
    if List.is_empty profile.errors then
      flags
    else
      flags @ [ Riot_toolchain.Ocamlc.WarnError profile.errors ]
  in
  flags
  @ List.map profile.open_modules ~fn:(fun mod_name -> Riot_toolchain.Ocamlc.Open mod_name)
  @ List.map profile.ocamlc_flags ~fn:(fun flag -> Riot_toolchain.Ocamlc.Raw flag)

let dedup_flags = fun flags ->
  let seen = HashSet.create () in
  List.filter_map
    flags
    ~fn:(fun flag ->
      let rendered =
        Riot_toolchain.Ocamlc.flags_to_string [ flag ]
        |> String.concat "\000"
      in
      if HashSet.insert seen ~value:rendered then
        Some flag
      else
        None)

let dedup_paths = fun paths ->
  let seen = HashSet.create () in
  List.filter_map
    paths
    ~fn:(fun path ->
      let rendered = Path.to_string path in
      if HashSet.insert seen ~value:rendered then
        Some path
      else
        None)

let open_modules = fun (modules: Module_node.t G.node list) ->
  List.filter_map
    modules
    ~fn:(fun (node: Module_node.t G.node) ->
      let value: Module_node.t = G.value node in
      match value.kind with
      | Module_node.ML mod_
      | MLI mod_ -> Some (Riot_model.Module.namespaced_name mod_)
      | _ -> None)

let source_for_module = fun (node: Module_node.t G.node) ->
  let value: Module_node.t = G.value node in
  match (value.kind, value.file) with
  | (Module_node.MLI mod_, Concrete path) ->
      Some (
        {
          Action.source = path;
          staged = Riot_model.Module_name.canonical_mli (Riot_model.Module.module_name mod_);
          kind = Action.LibraryInterface;
          content = None;
          opens = open_modules value.open_modules;
        },
        [ Riot_model.Module.cmti mod_; Riot_model.Module.cmi mod_ ],
        false
      )
  | (ML mod_, Concrete path) ->
      Some (
        {
          Action.source = path;
          staged = Riot_model.Module_name.canonical_ml (Riot_model.Module.module_name mod_);
          kind = Action.LibraryImplementation;
          content = None;
          opens = open_modules value.open_modules;
        },
        [
          Riot_model.Module.cmt mod_;
          Riot_model.Module.cmi mod_;
          Riot_model.Module.cmx mod_;
          Riot_model.Module.o mod_;
        ],
        false
      )
  | (MLI mod_, Generated { path; contents }) ->
      Some (
        {
          Action.source = path;
          staged = Riot_model.Module_name.canonical_mli (Riot_model.Module.module_name mod_);
          kind = Action.LibraryInterface;
          content = Some contents;
          opens = open_modules value.open_modules;
        },
        [ Riot_model.Module.cmti mod_; Riot_model.Module.cmi mod_ ],
        false
      )
  | (ML mod_, Generated { path; contents }) ->
      let is_alias_file = String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path) in
      Some (
        {
          Action.source = path;
          staged = Riot_model.Module_name.canonical_ml (Riot_model.Module.module_name mod_);
          kind = Action.LibraryImplementation;
          content = Some contents;
          opens = open_modules value.open_modules;
        },
        [
          Riot_model.Module.cmt mod_;
          Riot_model.Module.cmi mod_;
          Riot_model.Module.cmx mod_;
          Riot_model.Module.o mod_;
        ],
        is_alias_file
      )
  | _ -> None

type source_info = {
  node: Module_node.t G.node;
  source: Action.compile_library_source;
  outputs: Path.t list;
  is_alias: bool;
}

type source_group_kind =
  | AliasGroup
  | FolderSourcesGroup
  | ModuleSourcesGroup
  | LibraryInterfaceGroup

type source_group = {
  key: string;
  name: string;
  kind: source_group_kind;
  mutable sources: source_info list;
}

let source_info_for_module = fun node ->
  source_for_module node
  |> Option.map
    ~fn:(fun (source, outputs, is_alias) -> { node; source; outputs; is_alias })

let drop_last = fun values ->
  match List.reverse values with
  | [] -> []
  | _ :: rest -> List.reverse rest

let module_name_segments = fun module_name ->
  (
    Riot_model.Module_name.namespace module_name
    |> Riot_model.Namespace.to_list
  ) @ [ Riot_model.Module_name.to_string module_name ]

let source_info_module_segments = fun info ->
  let value: Module_node.t = G.value info.node in
  match value.kind with
  | Module_node.ML mod_
  | MLI mod_ -> module_name_segments (Riot_model.Module.module_name mod_)
  | _ -> []

let normalize_source_path = fun (package: Riot_model.Package.t) path ->
  if Path.is_absolute path then
    match Path.strip_prefix (Path.normalize path) ~prefix:(Path.normalize package.path) with
    | Ok relative -> relative
    | Error _ -> path
  else
    path

let sanitize_path_segment = fun segment ->
  Riot_model.Module_name.from_string segment
  |> Riot_model.Module_name.to_string

let folder_unit_segments = fun (package: Riot_model.Package.t) path ->
  let root = Riot_model.Package.root_module_name package in
  let path = normalize_source_path package path in
  match Path.parent path with
  | None -> [ root ]
  | Some dir -> (
      let components =
        Path.components dir
        |> List.map ~fn:Path.to_string
        |> List.filter ~fn:(fun part -> not (String.is_empty part))
      in
      match components with
      | "src" :: rest -> root :: List.map rest ~fn:sanitize_path_segment
      | _ -> [ root ])

let is_generated_library_interface = fun info ->
  match info.source.Action.content with
  | Some content ->
      String.starts_with
        ~prefix:"(* Library interface module generated by riot *)"
        content
  | None -> false

let rec string_list_equal = fun left right ->
  match (left, right) with
  | ([], []) -> true
  | (left :: left_rest, right :: right_rest) ->
      String.equal left right && string_list_equal left_rest right_rest
  | ([], _ :: _)
  | (_ :: _, []) -> false

let is_concrete_library_root = fun package info ->
  match info.source.Action.content with
  | Some _ -> false
  | None ->
      let folder_segments = folder_unit_segments package info.source.source in
      let module_segments = source_info_module_segments info in
      string_list_equal folder_segments module_segments

let source_group_kind_prefix = fun __tmp1 ->
  match __tmp1 with
  | AliasGroup -> "alias"
  | FolderSourcesGroup -> "folder"
  | ModuleSourcesGroup -> "module"
  | LibraryInterfaceGroup -> "interface"

let source_group_key = fun kind name -> source_group_kind_prefix kind ^ ":" ^ name

let source_group_for_info = fun package info ->
  let kind =
    if info.is_alias then
      AliasGroup
    else if is_generated_library_interface info || is_concrete_library_root package info then
      LibraryInterfaceGroup
    else
      FolderSourcesGroup
  in
  let segments =
    match kind with
    | AliasGroup -> drop_last (source_info_module_segments info)
    | LibraryInterfaceGroup -> source_info_module_segments info
    | FolderSourcesGroup -> folder_unit_segments package info.source.source
    | ModuleSourcesGroup -> source_info_module_segments info
  in
  let segments =
    match segments with
    | [] -> [ Riot_model.Package.root_module_name package ]
    | segments -> segments
  in
  let name = String.concat "__" segments in
  { key = source_group_key kind name; name; kind; sources = [] }

let add_source_to_group = fun groups group info ->
  let group =
    match HashMap.get groups ~key:group.key with
    | Some existing -> existing
    | None ->
        ignore (HashMap.insert groups ~key:group.key ~value:group);
        group
  in
  group.sources <- info :: group.sources

let node_groups_for_groups = fun groups ->
  let node_groups = HashMap.create () in
  List.for_each
    groups
    ~fn:(fun group ->
      List.for_each
        group.sources
        ~fn:(fun info ->
          ignore (HashMap.insert node_groups ~key:(G.id info.node) ~value:group.key)));
  node_groups

let sorted_source_groups = fun groups ->
  HashMap.values groups
  |> List.map ~fn:(fun group -> { group with sources = List.reverse group.sources })
  |> List.sort ~compare:(fun left right -> String.compare left.key right.key)

let collect_source_groups = fun package sorted_modules ->
  let groups = HashMap.create () in
  let sources =
    sorted_modules
    |> List.filter_map ~fn:source_info_for_module
  in
  List.for_each
    sources
    ~fn:(fun info ->
      let group = source_group_for_info package info in
      add_source_to_group groups group info);
  (sources, sorted_source_groups groups)

let native_compile_actions = fun (input: input) sorted_modules ->
  let base_ccflags = input.profile.Riot_model.Profile.cc_flags in
  let ccflags =
    match Riot_model.Build_ctx.sysroot input.build_ctx with
    | Some sysroot -> ("--sysroot=" ^ Path.to_string sysroot) :: base_ccflags
    | None -> base_ccflags
  in
  sorted_modules
  |> List.flat_map
    ~fn:(fun node ->
      let value: Module_node.t = G.value node in
      match value.kind with
      | Module_node.Native { files } ->
          files
          |> List.filter ~fn:(fun path -> String.ends_with ~suffix:".c" (Path.to_string path))
          |> List.map
            ~fn:(fun source ->
              let output =
                Path.remove_extension source
                |> Path.add_extension ~ext:"o"
                |> Path.basename
                |> Path.v
              in
              Action.CompileC { source; outputs = [ output ]; ccflags })
      | _ -> [])

let needs_package = fun name (package: Riot_model.Package.t) ->
  let expected =
    Riot_model.Package_name.from_string name
    |> Result.expect ~msg:("expected valid package name: " ^ name)
  in
  List.any
    (Riot_model.Package.build_graph_dependencies package)
    ~fn:(fun (dependency: Riot_model.Package.dependency) ->
      Riot_model.Package_name.equal
        dependency.name
        expected)

let dependency_includes = fun input ->
  let transitive_deps = Riot_planner.Dependency.transitive_closure input.depset in
  let needs_unix =
    needs_package "unix" input.package
    || List.any
      transitive_deps
      ~fn:(fun dep -> needs_package "unix" dep.Riot_planner.Dependency.package)
  in
  let needs_dynlink =
    needs_package "dynlink" input.package
    || List.any
      transitive_deps
      ~fn:(fun dep -> needs_package "dynlink" dep.Riot_planner.Dependency.package)
  in
  let stdlib_includes =
    (
      if needs_unix then
        [ Path.v "+unix" ]
      else
        []
    ) @ (
      if needs_dynlink then
        [ Path.v "+dynlink" ]
      else
        []
    )
  in
  let dep_cache_includes =
    List.map transitive_deps ~fn:(fun dep -> dep.Riot_planner.Dependency.artifact_dir)
  in
  stdlib_includes @ dep_cache_includes

let library_outputs = fun package ->
  let name = Riot_model.Package.root_module_name package in
  [
    Riot_model.Module_name.(from_string name
    |> cmxa);
    Riot_model.Module_name.(from_string name
    |> a);
  ]

let flags_for_sources = fun input sources ->
  let base_flags = stdlib_flags input.package @ profile_compile_flags input.profile in
  let flags =
    if List.any sources ~fn:(fun info -> info.is_alias) then
      Riot_toolchain.Ocamlc.NoAliasDeps :: base_flags
    else
      base_flags
  in
  dedup_flags flags

let source_group_output = fun group ->
  Path.v ("__riot_build2_" ^ source_group_kind_prefix group.kind ^ "_" ^ group.name ^ ".cmxa")

let first_output_with_extension = fun outputs extension ->
  List.find outputs ~fn:(fun output -> Path.extension output = Some extension)

let compile_source_output = fun info ->
  match info.source.Action.kind with
  | Action.LibraryInterface ->
      first_output_with_extension info.outputs ".cmi"
      |> Option.unwrap_or ~default:(
        match info.outputs with
        | output :: _ -> output
        | [] -> Path.v "source.cmi"
      )
  | Action.LibraryImplementation ->
      first_output_with_extension info.outputs ".cmx"
      |> Option.unwrap_or ~default:(
        match info.outputs with
        | output :: _ -> output
        | [] -> Path.v "source.cmx"
      )

let source_group_action = fun input group ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      if List.is_empty group.sources then
        None
      else
        match (group.kind, group.sources) with
        | (ModuleSourcesGroup, [ info ]) ->
            Some (
              Action.CompileSource {
                source = info.source;
                outputs = info.outputs;
                output = compile_source_output info;
                includes = dedup_paths (Path.v "." :: dependency_includes input);
                flags = flags_for_sources input [ info ];
              }
            )
        | (ModuleSourcesGroup, _)
        | (AliasGroup, _)
        | (FolderSourcesGroup, _)
        | (LibraryInterfaceGroup, _) ->
            Some (
              Action.CompileLibrary {
                sources = List.map group.sources ~fn:(fun info -> info.source);
                objects = [];
                outputs =
                  group.sources
                  |> List.flat_map ~fn:(fun info -> info.outputs)
                  |> dedup_paths;
                output = source_group_output group;
                includes = dedup_paths (Path.v "." :: dependency_includes input);
                flags = flags_for_sources input group.sources;
              }
            )

let final_library_action = fun input source_infos c_actions ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      let module_objects =
        source_infos
        |> List.reverse
        |> List.flat_map ~fn:(fun info -> info.outputs)
        |> List.filter ~fn:(fun output -> Path.extension output = Some ".cmx")
        |> dedup_paths
      in
      if List.is_empty module_objects then
        None
      else
        let c_objects =
          c_actions
          |> List.flat_map ~fn:Action.outputs
          |> List.filter ~fn:(fun output -> Path.extension output = Some ".o")
          |> dedup_paths
        in
        let library_outputs = library_outputs input.package in
        Some (
          Action.CompileLibrary {
            sources = [];
            objects = module_objects @ c_objects;
            outputs = library_outputs;
            output =
              (
                match library_outputs with
                | output :: _ -> output
                | [] -> Path.v "library.cmxa"
              );
            includes = dedup_paths (Path.v "." :: dependency_includes input);
            flags = [];
          }
        )

let make_execution = fun input ~dependencies action ->
  Action_execution.make
    ~package:input.package
    ~profile:input.profile
    ~target:input.target
    ~toolchain:input.toolchain
    ~action
    ~dependencies
    ~sandbox_dir:input.sandbox_dir

let source_group_dependencies = fun node_groups group ->
  group.sources
  |> List.flat_map
    ~fn:(fun info ->
      G.deps info.node
      |> List.filter_map
        ~fn:(fun dep_id ->
          match HashMap.get node_groups ~key:dep_id with
          | Some dep_group_key when not (String.equal dep_group_key group.key) ->
              Some dep_group_key
          | Some _
          | None -> None))
  |> List.sort ~compare:String.compare
  |> List.unique ~compare:String.compare

let module_source_group_for_info = fun package info ->
  let segments =
    match source_info_module_segments info with
    | [] -> [ Riot_model.Package.root_module_name package ]
    | segments -> segments
  in
  let suffix =
    match info.source.Action.kind with
    | Action.LibraryInterface -> "mli"
    | Action.LibraryImplementation -> "ml"
  in
  let name = String.concat "__" (segments @ [ suffix ]) in
  { key = source_group_key ModuleSourcesGroup name; name; kind = ModuleSourcesGroup; sources = [] }

let split_folder_source_groups = fun package should_split groups ->
  let split_groups = HashMap.create () in
  List.for_each
    groups
    ~fn:(fun group ->
      match group.kind with
      | FolderSourcesGroup when should_split group ->
          List.for_each
            group.sources
            ~fn:(fun info ->
              add_source_to_group split_groups (module_source_group_for_info package info) info)
      | FolderSourcesGroup
      | AliasGroup
      | ModuleSourcesGroup
      | LibraryInterfaceGroup ->
          let empty_group = { group with sources = [] } in
          List.for_each group.sources ~fn:(add_source_to_group split_groups empty_group));
  sorted_source_groups split_groups

let cyclic_source_group_keys = fun node_groups groups ->
  let group_keys = HashSet.create () in
  List.for_each groups ~fn:(fun group -> ignore (HashSet.insert group_keys ~value:group.key));
  let dependents_by_group = HashMap.create () in
  let dependency_counts = HashMap.create () in
  List.for_each
    groups
    ~fn:(fun group ->
      let dependencies =
        source_group_dependencies node_groups group
        |> List.filter ~fn:(fun dependency -> HashSet.contains group_keys ~value:dependency)
      in
      ignore (HashMap.insert dependency_counts ~key:group.key ~value:(List.length dependencies));
      List.for_each
        dependencies
        ~fn:(fun dependency ->
          let dependents =
            match HashMap.get dependents_by_group ~key:dependency with
            | Some dependents -> group.key :: dependents
            | None -> [ group.key ]
          in
          ignore (HashMap.insert dependents_by_group ~key:dependency ~value:dependents)));
  let ready =
    groups
    |> List.filter_map
      ~fn:(fun group ->
        match HashMap.get dependency_counts ~key:group.key with
        | Some 0 -> Some group.key
        | Some _
        | None -> None)
  in
  let rec loop processed = fun __tmp1 ->
    match __tmp1 with
    | [] -> processed
    | key :: rest ->
        let dependents =
          HashMap.get dependents_by_group ~key
          |> Option.unwrap_or ~default:[]
        in
        let newly_ready =
          dependents
          |> List.filter_map
            ~fn:(fun dependent ->
              match HashMap.get dependency_counts ~key:dependent with
              | Some count ->
                  let count = Int.pred count in
                  ignore (HashMap.insert dependency_counts ~key:dependent ~value:count);
                  if Int.equal count 0 then
                    Some dependent
                  else
                    None
              | None -> None)
        in
        loop (Int.succ processed) (rest @ newly_ready)
  in
  let processed = loop 0 ready in
  if Int.equal processed (List.length groups) then
    Ok []
  else
    Error (
      groups
      |> List.filter_map
        ~fn:(fun group ->
          match HashMap.get dependency_counts ~key:group.key with
          | Some count when count > 0 -> Some group.key
          | Some _
          | None -> None)
    )

let validate_acyclic_source_groups = fun node_groups groups ->
  match cyclic_source_group_keys node_groups groups with
  | Ok _ -> Ok ()
  | Error cyclic_keys ->
    let cyclic_key_set = HashSet.create () in
    List.for_each cyclic_keys ~fn:(fun key -> ignore (HashSet.insert cyclic_key_set ~value:key));
    let blocked =
      groups
      |> List.filter_map
        ~fn:(fun group ->
          if HashSet.contains cyclic_key_set ~value:group.key then
            let dependencies =
              source_group_dependencies node_groups group
              |> List.filter ~fn:(fun dependency -> HashSet.contains cyclic_key_set ~value:dependency)
            in
            Some (group.key ^ " waits for " ^ String.concat "," dependencies)
          else
            None)
      |> String.concat "; "
    in
    Error (Error.ExecutorInvariantViolated {
      message = "cyclic compile-library source groups: " ^ blocked;
    })

let refine_source_groups = fun package groups ->
  let rec loop groups =
      let node_groups = node_groups_for_groups groups in
      match cyclic_source_group_keys node_groups groups with
      | Ok _ -> Ok (groups, node_groups)
      | Error cyclic_keys ->
          let cyclic_key_set = HashSet.create () in
          List.for_each cyclic_keys ~fn:(fun key -> ignore (HashSet.insert cyclic_key_set ~value:key));
          let has_splittable_group =
            List.any
              groups
              ~fn:(fun group ->
                match group.kind with
                | FolderSourcesGroup -> HashSet.contains cyclic_key_set ~value:group.key
                | AliasGroup
                | ModuleSourcesGroup
                | LibraryInterfaceGroup -> false)
          in
          if has_splittable_group then
            let should_split = fun group -> HashSet.contains cyclic_key_set ~value:group.key in
            split_folder_source_groups package should_split groups
            |> loop
          else
            match validate_acyclic_source_groups node_groups groups with
            | Ok () -> Ok (groups, node_groups)
            | Error error -> Error error
  in
  loop groups

let plan = fun input ->
  match G.topo_sort input.module_graph with
  | Error _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "build2 action planner received a cyclic module graph";
      })
  | Ok sorted_modules ->
      let c_actions = native_compile_actions input sorted_modules in
      let c_executions = List.map c_actions ~fn:(make_execution input ~dependencies:[]) in
      let (source_infos, groups) =
        collect_source_groups input.package sorted_modules
      in
      match refine_source_groups input.package groups with
      | Error _ as error -> error
      | Ok (groups, node_groups) ->
          let group_actions =
            groups
            |> List.filter_map
              ~fn:(fun group ->
                source_group_action input group
                |> Option.map ~fn:(fun action -> (group, action)))
          in
          let group_refs = HashMap.create () in
          List.for_each
            group_actions
            ~fn:(fun (group, action) ->
              let ref_ =
                Action_execution.ref_from_action
                  ~package:input.package
                  ~profile:input.profile
                  ~target:input.target
                  ~toolchain:input.toolchain
                  action
              in
              ignore (HashMap.insert group_refs ~key:group.key ~value:ref_));
          let group_executions =
            group_actions
            |> List.map
              ~fn:(fun (group, action) ->
                let dependencies =
                  source_group_dependencies node_groups group
                  |> List.filter_map ~fn:(fun group_key -> HashMap.get group_refs ~key:group_key)
                in
                make_execution input ~dependencies action)
          in
          match final_library_action input source_infos c_actions with
          | None -> Ok (c_executions @ group_executions)
          | Some library_action ->
              let dependencies =
                (
                  List.map c_executions ~fn:(fun action -> action.Action_execution.ref_)
                ) @ (
                  List.map group_executions ~fn:(fun action -> action.Action_execution.ref_)
                )
              in
              Ok (
                c_executions
                @ group_executions
                @ [ make_execution input ~dependencies library_action ]
              )
