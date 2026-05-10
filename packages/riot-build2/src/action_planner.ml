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
  dep_analysis: Dep_analysis.t;
}

type source_info = {
  node: Module_node.t G.node;
  source: Action.compile_library_source;
  outputs: Path.t list;
  is_alias: bool;
}

let cache_key_version = "riot-build2-action-planner:v5"

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

let source_info_for_module = fun node ->
  source_for_module node
  |> Option.map
    ~fn:(fun (source, outputs, is_alias) ->
      {
        node;
        source;
        outputs;
        is_alias;
      })

let module_of_source_info = fun info ->
  match (G.value info.node).Module_node.kind with
  | Module_node.ML mod_
  | MLI mod_ -> Some mod_
  | _ -> None

let implementation_has_interface = fun source_infos info ->
  match (info.source.Action.kind, module_of_source_info info) with
  | (Action.LibraryImplementation, Some mod_) ->
      List.any
        source_infos
        ~fn:(fun candidate ->
          match (candidate.source.Action.kind, module_of_source_info candidate) with
          | (Action.LibraryInterface, Some candidate_mod) ->
              Riot_model.Module.eq candidate_mod mod_
          | _ -> false)
  | _ -> false

let source_info_with_owned_outputs = fun source_infos info ->
  if implementation_has_interface source_infos info then
    {
      info with
      outputs =
        info.outputs
        |> List.filter ~fn:(fun output -> not (Path.extension output = Some ".cmi"));
    }
  else
    info

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

let flags_for_source = fun input source ->
  let base_flags = stdlib_flags input.package @ profile_compile_flags input.profile in
  let base_flags =
    match source.source.kind with
    | Action.LibraryImplementation ->
        Riot_toolchain.Ocamlc.Raw "-opaque" :: base_flags
    | Action.LibraryInterface -> base_flags
  in
  let flags =
    if source.is_alias then
      Riot_toolchain.Ocamlc.NoAliasDeps :: base_flags
    else
      base_flags
  in
  dedup_flags flags

let first_output_with_extension = fun outputs extension ->
  List.find
    outputs
    ~fn:(fun output -> Path.extension output = Some extension)

let compile_source_output = fun info ->
  match info.source.Action.kind with
  | Action.LibraryInterface ->
      first_output_with_extension info.outputs ".cmi"
      |> Option.unwrap_or
        ~default:(
          match info.outputs with
          | output :: _ -> output
          | [] -> Path.v "source.cmi"
        )
  | Action.LibraryImplementation ->
      first_output_with_extension info.outputs ".cmx"
      |> Option.unwrap_or
        ~default:(
          match info.outputs with
          | output :: _ -> output
          | [] -> Path.v "source.cmx"
        )

let source_action = fun input info ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      Some (
        Action.CompileSource {
          source = info.source;
          outputs = info.outputs;
          output = compile_source_output info;
          includes = dedup_paths (Path.v "." :: dependency_includes input);
          flags = flags_for_source input info;
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
  let ref_ =
    Action_execution.ref_from_action
      ~package:input.package
      ~profile:input.profile
      ~target:input.target
      ~toolchain:input.toolchain
      action
  in
  Action_execution.make
    ~package:input.package
    ~profile:input.profile
    ~target:input.target
    ~toolchain:input.toolchain
    ~action
    ~dependencies
    ~sandbox_dir:(Action_execution.sandbox_dir_for_ref ~base_sandbox_dir:input.sandbox_dir ref_)

let source_dependencies = fun input source_refs info ->
  Dep_analysis.compile_dependency_ids input.dep_analysis input.module_graph info.node
  |> List.filter_map ~fn:(fun dep_id -> HashMap.get source_refs ~key:dep_id)

let plan = fun input ->
  match G.topo_sort input.module_graph with
  | Error _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "build2 action planner received a cyclic module graph";
      })
  | Ok sorted_modules ->
      let c_actions = native_compile_actions input sorted_modules in
      let c_executions = List.map c_actions ~fn:(make_execution input ~dependencies:[]) in
      let source_infos =
        sorted_modules
        |> List.filter_map ~fn:source_info_for_module
        |> fun source_infos ->
            List.map source_infos ~fn:(source_info_with_owned_outputs source_infos)
      in
      let source_actions =
        source_infos
        |> List.filter_map
          ~fn:(fun info ->
            source_action input info
            |> Option.map ~fn:(fun action -> (info, action)))
      in
      let source_refs = HashMap.create () in
      List.for_each
        source_actions
        ~fn:(fun (info, action) ->
          let ref_ =
            Action_execution.ref_from_action
              ~package:input.package
              ~profile:input.profile
              ~target:input.target
              ~toolchain:input.toolchain
              action
          in
          ignore (HashMap.insert source_refs ~key:(G.id info.node) ~value:ref_));
      let source_executions =
        source_actions
        |> List.map
          ~fn:(fun (info, action) ->
            make_execution
              input
              ~dependencies:(source_dependencies input source_refs info)
              action)
      in
      match final_library_action input source_infos c_actions with
      | None -> Ok (c_executions @ source_executions)
      | Some library_action ->
          let dependencies =
            (List.map c_executions ~fn:(fun action -> action.Action_execution.ref_))
            @ (List.map source_executions ~fn:(fun action -> action.Action_execution.ref_))
          in
          Ok (c_executions
          @ source_executions
          @ [ make_execution input ~dependencies library_action ])
