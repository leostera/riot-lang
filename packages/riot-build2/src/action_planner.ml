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

let cache_key_version = "riot-build2-action-planner:v13"

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

let check_path = fun path -> Path.(Package_sandbox.check_dir / path)

let link_path = fun path -> Path.(Package_sandbox.link_dir / path)

let interface_outputs = fun mod_ ->
  [ check_path (Riot_model.Module.cmi mod_) ]

let byte_implementation_outputs = fun mod_ ->
  [ check_path (Riot_model.Module.cmi mod_) ]

let byte_implementation_output = fun mod_ ->
  check_path (
    Riot_model.Module.cmi mod_
    |> Path.replace_extension ~ext:"cmo"
  )

let native_implementation_outputs = fun mod_ ->
  [
    link_path (Riot_model.Module.cmx mod_);
    link_path (Riot_model.Module.o mod_);
  ]

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
          kind = Action.LibraryInterface;
          content = None;
        },
        interface_outputs mod_,
        false
      )
  | (ML mod_, Concrete path) ->
      Some (
        {
          Action.source = path;
          kind = Action.LibraryImplementation;
          content = None;
        },
        native_implementation_outputs mod_,
        false
      )
  | (MLI mod_, Generated { path; contents }) ->
      Some (
        {
          Action.source = path;
          kind = Action.LibraryInterface;
          content = Some contents;
        },
        interface_outputs mod_,
        false
      )
  | (ML mod_, Generated { path; contents }) ->
      let is_alias_file = String.ends_with ~suffix:"Aliases.ml-gen" (Path.to_string path) in
      Some (
        {
          Action.source = path;
          kind = Action.LibraryImplementation;
          content = Some contents;
        },
        native_implementation_outputs mod_,
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
                |> link_path
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

let stdlib_includes = fun input ->
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

let transitive_dependency_packages = fun input ->
  let seen = HashSet.create () in
  input.depset
  @ Riot_planner.Dependency.transitive_closure input.depset
  |> List.filter_map
    ~fn:(fun dep ->
      let package = dep.Riot_planner.Dependency.package in
      let name = Riot_model.Package_name.to_string package.name in
      if HashSet.insert seen ~value:name then
        Some package.name
      else
        None)

let dependency_check_includes = fun input ->
  stdlib_includes input
  @ List.map (transitive_dependency_packages input) ~fn:Package_sandbox.dep_check_dir

let dependency_link_includes = fun input ->
  stdlib_includes input
  @ List.flat_map
    (transitive_dependency_packages input)
    ~fn:(fun package -> [ Package_sandbox.dep_check_dir package; Package_sandbox.dep_link_dir package ])

let source_includes = fun input ->
  dedup_paths (
    [
      Package_sandbox.check_dir;
      Package_sandbox.link_dir;
    ]
    @ dependency_check_includes input
  )

let archive_includes = fun input ->
  dedup_paths (
    [
      Package_sandbox.check_dir;
      Package_sandbox.link_dir;
    ]
    @ dependency_link_includes input
  )

let library_outputs = fun package ->
  let name = Riot_model.Package.root_module_name package in
  [
    Riot_model.Module_name.(from_string name
    |> cmxa
    |> link_path);
    Riot_model.Module_name.(from_string name
    |> a
    |> link_path);
  ]

let flags_for_source = fun input info ->
  let base_flags =
    Riot_toolchain.Ocamlc.Raw "-opaque"
    :: stdlib_flags input.package
    @ profile_compile_flags input.profile
  in
  let generated_impl_flags =
    match (info.source.Action.kind, info.source.content) with
    | (Action.LibraryImplementation, Some _) ->
        [ Riot_toolchain.Ocamlc.Impl info.source.source ]
    | _ -> []
  in
  let alias_flags =
    if info.is_alias then
      [ Riot_toolchain.Ocamlc.NoAliasDeps ]
    else
      []
  in
  let implicit_open_flags =
    let node_value: Module_node.t = G.value info.node in
    open_modules node_value.open_modules
    |> List.map ~fn:(fun mod_name -> Riot_toolchain.Ocamlc.Open mod_name)
  in
  let flags =
    base_flags @ generated_impl_flags @ alias_flags @ implicit_open_flags
  in
  dedup_flags flags

let bytecode_flags_for_source = fun input info ->
  flags_for_source input info
  |> List.filter
    ~fn:(fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Inline _
      | Riot_toolchain.Ocamlc.Compact -> false
      | Riot_toolchain.Ocamlc.Raw "-afl-instrument" -> false
      | Riot_toolchain.Ocamlc.NoAliasDeps
      | Riot_toolchain.Ocamlc.Open _
      | Riot_toolchain.Ocamlc.NoStdlib
      | Riot_toolchain.Ocamlc.NoPervasives
      | Riot_toolchain.Ocamlc.NoAssert
      | Riot_toolchain.Ocamlc.Unsafe
      | Riot_toolchain.Ocamlc.Impl _
      | Riot_toolchain.Ocamlc.Warning _
      | Riot_toolchain.Ocamlc.WarnError _
      | Riot_toolchain.Ocamlc.Raw _
      | Riot_toolchain.Ocamlc.LinkAll -> true)

let first_output_with_extension = fun outputs extension ->
  List.find
    outputs
    ~fn:(fun output -> Path.extension output = Some extension)

let output_with_extension_or = fun outputs extension default ->
  first_output_with_extension outputs extension
  |> Option.unwrap_or
    ~default:(
      match outputs with
      | output :: _ -> output
      | [] -> default
    )

let interface_output = fun info ->
  output_with_extension_or info.outputs ".cmi" (Path.v "source.cmi")

let byte_implementation_compiler_output = fun info ->
  match module_of_source_info info with
  | Some mod_ -> byte_implementation_output mod_
  | None -> Path.v "source.cmo"

let native_implementation_output = fun info ->
  output_with_extension_or info.outputs ".cmx" (Path.v "source.cmx")

let cmi_file_for_source = fun info ->
  match module_of_source_info info with
  | Some mod_ -> Some (check_path (Riot_model.Module.cmi mod_))
  | None -> first_output_with_extension info.outputs ".cmi"

let interface_action = fun input info ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ -> (
      match info.source.kind with
      | Action.LibraryInterface ->
          Some (
            Action.CompileInterface {
              source = info.source;
              outputs = info.outputs;
              output = interface_output info;
              includes = source_includes input;
              flags = bytecode_flags_for_source input info;
            }
          )
      | Action.LibraryImplementation -> None
    )

let byte_implementation_action = fun input source_infos info ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      if implementation_has_interface source_infos info then
        None
      else
        match (info.source.Action.kind, module_of_source_info info) with
        | (Action.LibraryImplementation, Some mod_) ->
            Some (
              Action.CompileByteImplementation {
                source = info.source;
                outputs = byte_implementation_outputs mod_;
                output = byte_implementation_compiler_output info;
                includes = source_includes input;
                flags = bytecode_flags_for_source input info;
              }
            )
        | _ -> None

let native_implementation_action = fun input info ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ -> (
      match info.source.kind with
      | Action.LibraryImplementation ->
          Some (
            Action.CompileNativeImplementation {
              source = info.source;
              outputs = info.outputs;
              output = native_implementation_output info;
              cmi_file = cmi_file_for_source info;
              includes = source_includes input;
              flags = flags_for_source input info;
            }
          )
      | Action.LibraryInterface -> None
    )

let final_library_action = fun input native_actions c_actions ->
  match input.package.Riot_model.Package.library with
  | None -> None
  | Some _ ->
      let module_objects =
        native_actions
        |> List.reverse
        |> List.flat_map ~fn:Action.outputs
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
            includes = archive_includes input;
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

let source_dependencies = fun input source_refs info ->
  Dep_analysis.compile_dependency_ids input.dep_analysis input.module_graph info.node
  |> List.filter_map ~fn:(fun dep_id -> HashMap.get source_refs ~key:dep_id)

let ref_key = fun (ref_: Action_execution.ref_) ->
  Riot_model.Package_name.to_string ref_.package
  ^ ":"
  ^ ref_.profile.Riot_model.Profile.name
  ^ ":"
  ^ Riot_model.Target.to_string ref_.target
  ^ ":"
  ^ Crypto.Digest.hex ref_.hash

let dedup_refs = fun refs ->
  let seen = HashSet.create () in
  List.filter_map
    refs
    ~fn:(fun ref_ ->
      if HashSet.insert seen ~value:(ref_key ref_) then
        Some ref_
      else
        None)

let action_ref = fun input action ->
  Action_execution.ref_from_action
    ~package:input.package
    ~profile:input.profile
    ~target:input.target
    ~toolchain:input.toolchain
    action

let add_cmi_ref = fun cmi_refs input info action ->
  let ref_ = action_ref input action in
  ignore (HashMap.insert cmi_refs ~key:(G.id info.node) ~value:ref_);
  ref_

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
      in
      let interface_actions =
        source_infos
        |> List.filter_map
          ~fn:(fun info ->
            interface_action input info
            |> Option.map ~fn:(fun action -> (info, action)))
      in
      let byte_implementation_actions =
        source_infos
        |> List.filter_map
          ~fn:(fun info ->
            byte_implementation_action input source_infos info
            |> Option.map ~fn:(fun action -> (info, action)))
      in
      let native_implementation_actions =
        source_infos
        |> List.filter_map
          ~fn:(fun info ->
            native_implementation_action input info
            |> Option.map ~fn:(fun action -> (info, action)))
      in
      let cmi_refs = HashMap.create () in
      List.for_each
        interface_actions
        ~fn:(fun (info, action) ->
          ignore (add_cmi_ref cmi_refs input info action));
      List.for_each
        byte_implementation_actions
        ~fn:(fun (info, action) ->
          ignore (add_cmi_ref cmi_refs input info action));
      let interface_executions =
        interface_actions
        |> List.map
          ~fn:(fun (info, action) ->
            make_execution
              input
              ~dependencies:(source_dependencies input cmi_refs info)
              action)
      in
      let byte_implementation_executions =
        byte_implementation_actions
        |> List.map
          ~fn:(fun (info, action) ->
            make_execution
              input
              ~dependencies:(source_dependencies input cmi_refs info)
              action)
      in
      let native_implementation_executions =
        native_implementation_actions
        |> List.map
          ~fn:(fun (info, action) ->
            let own_cmi_dependency =
              HashMap.get cmi_refs ~key:(G.id info.node)
              |> Option.to_list
            in
            make_execution
              input
              ~dependencies:(dedup_refs (source_dependencies input cmi_refs info @ own_cmi_dependency))
              action)
      in
      let source_executions =
        interface_executions
        @ byte_implementation_executions
        @ native_implementation_executions
      in
      let native_actions = List.map native_implementation_actions ~fn:(fun (_info, action) -> action) in
      match final_library_action input native_actions c_actions with
      | None -> Ok (c_executions @ source_executions)
      | Some library_action ->
          let dependencies =
            (List.map c_executions ~fn:(fun action -> action.Action_execution.ref_))
            @ (
              List.map
                native_implementation_executions
                ~fn:(fun action -> action.Action_execution.ref_)
            )
          in
          Ok (c_executions
          @ source_executions
          @ [ make_execution input ~dependencies library_action ])
