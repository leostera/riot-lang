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
  let name = Riot_model.Package_name.to_string package.Riot_model.Package.name in
  [
    Riot_model.Module_name.(from_string name
    |> cmxa);
    Riot_model.Module_name.(from_string name
    |> a);
  ]

let library_action = fun input sorted_modules c_actions ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      let compile_sources =
        sorted_modules
        |> List.filter_map ~fn:source_for_module
      in
      if List.is_empty compile_sources then
        None
      else
        let sources = List.map compile_sources ~fn:(fun (source, _, _) -> source) in
        let module_outputs =
          compile_sources
          |> List.flat_map ~fn:(fun (_, outputs, _) -> outputs)
          |> dedup_paths
        in
        let has_alias_sources = List.any compile_sources ~fn:(fun (_, _, is_alias) -> is_alias) in
        let base_flags = stdlib_flags input.package @ profile_compile_flags input.profile in
        let flags =
          if has_alias_sources then
            Riot_toolchain.Ocamlc.NoAliasDeps :: base_flags
          else
            base_flags
            |> dedup_flags
        in
        let c_objects =
          c_actions
          |> List.flat_map ~fn:Action.outputs
          |> List.filter ~fn:(fun output -> Path.extension output = Some ".o")
          |> dedup_paths
        in
        let library_outputs = library_outputs input.package in
        let outputs = dedup_paths (module_outputs @ library_outputs) in
        Some (
          Action.CompileLibrary {
            sources;
            objects = c_objects;
            outputs;
            output =
              (
                match library_outputs with
                | output :: _ -> output
                | [] -> Path.v "library.cmxa"
              );
            includes = dedup_paths (Path.v "." :: dependency_includes input);
            flags;
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

let plan = fun input ->
  match G.topo_sort input.module_graph with
  | Error _ ->
      Error (Error.ExecutorInvariantViolated {
        message = "build2 action planner received a cyclic module graph";
      })
  | Ok sorted_modules ->
      let c_actions = native_compile_actions input sorted_modules in
      let c_executions = List.map c_actions ~fn:(make_execution input ~dependencies:[]) in
      match library_action input sorted_modules c_actions with
      | None -> Ok c_executions
      | Some library_action ->
          let dependencies =
            List.map c_executions ~fn:(fun action -> action.Action_execution.ref_)
          in
          Ok (c_executions @ [ make_execution input ~dependencies library_action ])
