open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let dependency_source =
  Riot_model.Package.{
    workspace = true;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = None;
  }

let package_manifest = fun ?(dependencies = []) name ->
  let name = package name in
  Riot_model.Package.make
    ~name
    ~path:Path.(Path.v "." / Path.v (Riot_model.Package_name.to_string name))
    ~relative_path:(Path.v (Riot_model.Package_name.to_string name))
    ~dependencies
    ()
  |> Riot_model.Package_manifest.from_package

let workspace =
  let dep_name = package "dep" in
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-build-services-tests")
    ~packages:[
      package_manifest "dep";
      package_manifest
        ~dependencies:[ Riot_model.Package.{ name = dep_name; source = dependency_source } ]
        "app";
    ]
    ()

let config = fun () -> Config.make ~workspace ~parallelism:1 ()

let build_package = fun name ->
  Goal.{
    package = package name;
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let build_goal = fun name -> Goal.BuildPackage (build_package name)

let request_key = Work_request.key

let has_goal_key = fun requests goal ->
  List.any
    requests
    ~fn:(fun request ->
      match request_key request with
      | Work_node.GoalKey got -> got = goal
      | _ -> false)

let has_toolchain_key = fun requests target ->
  List.any
    requests
    ~fn:(fun request ->
      match request_key request with
      | Work_node.ToolchainReadyKey toolchain -> Riot_model.Target.equal toolchain.target target
      | _ -> false)

let has_package_artifact_key = fun requests build ->
  List.any
    requests
    ~fn:(fun request ->
      match request_key request with
      | Work_node.PackageArtifactKey got -> got = build
      | _ -> false)

let key_name = fun request ->
  match request_key request with
  | Work_node.Intent _ -> "Intent"
  | Package _ -> "Package"
  | Module _ -> "Module"
  | Source _ -> "Source"
  | GoalKey _ -> "GoalKey"
  | ToolchainReadyKey _ -> "ToolchainReadyKey"
  | SourceAnalysisKey _ -> "SourceAnalysisKey"
  | PackageArtifactKey _ -> "PackageArtifactKey"
  | PackageFinalizeKey _ -> "PackageFinalizeKey"
  | ModulePlanKey _ -> "ModulePlanKey"
  | ActionPlanKey _ -> "ActionPlanKey"
  | ModuleDependenciesKey _ -> "ModuleDependenciesKey"
  | OCamlInterfaceKey _ -> "OCamlInterfaceKey"
  | OCamlByteImplementationKey _ -> "OCamlByteImplementationKey"
  | OCamlImplementationKey _ -> "OCamlImplementationKey"
  | OCamlGeneratedKey _ -> "OCamlGeneratedKey"
  | CObjectKey _ -> "CObjectKey"
  | OCamlLibraryKey _ -> "OCamlLibraryKey"
  | OCamlArchiveKey _ -> "OCamlArchiveKey"
  | ActionExecutionKey _ -> "ActionExecutionKey"

let materialize_request = fun registry request ->
  match Work_request.kind request with
  | Some kind ->
      Ok (Work_registry.intern registry ~key:(Work_request.key request) ~make:(fun () -> kind))
  | None -> Error ("request cannot be materialized: " ^ key_name request)

let source_analysis_requests = fun requests ->
  List.filter
    requests
    ~fn:(fun request ->
      match request_key request with
      | Work_node.SourceAnalysisKey _ -> true
      | _ -> false)

let rec execute_source_requests = fun services registry requests ->
  match requests with
  | [] -> Ok ()
  | request :: rest ->
      match materialize_request registry request with
      | Error error -> Error error
      | Ok source_node ->
          let* _ =
            Build_services.execute_node services registry source_node
            |> Result.map_err ~fn:Error.message
          in
          execute_source_requests services registry rest

let source_package_workspace = fun root ->
  let package_name = package "sourcepkg" in
  let package_path = Path.(root / Path.v "sourcepkg") in
  let source = Path.v "src/sourcepkg.ml" in
  let* () =
    Fs.create_dir_all Path.(package_path / Path.v "src")
    |> Result.map_err ~fn:IO.error_message
  in
  let* () =
    Fs.write "let value = 1\n" Path.(package_path / source)
    |> Result.map_err ~fn:IO.error_message
  in
  let sources =
    Riot_model.Package.{
      src = [ source ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
  in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_path
      ~relative_path:(Path.v "sourcepkg")
      ~library:{ path = source }
      ~sources
      ()
    |> Riot_model.Package_manifest.from_package
  in
  Ok (Riot_model.Workspace.make
    ~root
    ~target_dir:Path.(root / Path.v "target")
    ~packages:[ package ]
    ())

let interface_dependency_workspace = fun root ->
  let package_name = package "sourcepkg" in
  let package_path = Path.(root / Path.v "sourcepkg") in
  let src_dir = Path.(package_path / Path.v "src") in
  let root_source = Path.v "src/sourcepkg.ml" in
  let a_interface = Path.v "src/a.mli" in
  let a_implementation = Path.v "src/a.ml" in
  let b_implementation = Path.v "src/b.ml" in
  let c_implementation = Path.v "src/c.ml" in
  let write_source = fun path content ->
    Fs.write content Path.(package_path / path)
    |> Result.map_err ~fn:IO.error_message
  in
  let* () =
    Fs.create_dir_all src_dir
    |> Result.map_err ~fn:IO.error_message
  in
  let* () = write_source a_interface "val value : int\n" in
  let* () = write_source a_implementation "let value = 1\n" in
  let* () = write_source c_implementation "let value = 2\n" in
  let* () = write_source b_implementation "let from_a = A.value\nlet from_c = C.value\n" in
  let* () = write_source root_source "let value = B.from_a + B.from_c\n" in
  let sources =
    Riot_model.Package.{
      src = [ root_source; a_interface; a_implementation; b_implementation; c_implementation; ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
  in
  let package =
    Riot_model.Package.make
      ~name:package_name
      ~path:package_path
      ~relative_path:(Path.v "sourcepkg")
      ~library:{ path = root_source }
      ~sources
      ()
    |> Riot_model.Package_manifest.from_package
  in
  Ok (Riot_model.Workspace.make
    ~root
    ~target_dir:Path.(root / Path.v "target")
    ~packages:[ package ]
    ())

let source_analysis_task = fun ~source ~source_path ->
  Riot_planner.Module_graph.{
    task_node_id = G.Node_id.next ();
    task_file = Riot_planner.Module_node.Concrete source;
    task_path = source;
    task_display_path = source_path;
    task_module_path = Some [ "Sourcepkg" ];
    task_implicit_opens = [];
    task_implicit_open_paths = [];
  }

let require_module_plan = fun services build ->
  match Build_services.module_plan services build with
  | Some plan -> Ok plan
  | None -> Error "expected module plan to be stored"

let ref_equal = fun (left: Action_execution.ref_) (right: Action_execution.ref_) ->
  Riot_model.Package_name.equal left.package right.package
  && String.equal left.profile.Riot_model.Profile.name right.profile.Riot_model.Profile.name
  && Riot_model.Target.equal left.target right.target
  && Crypto.Hash.equal left.hash right.hash

let action_for_ref = fun (plan: Module_plan.t) ref_ ->
  List.find
    plan.action_executions
    ~fn:(fun action -> ref_equal action.Action_execution.ref_ ref_)

let dependencies_for_action = fun plan action ->
  action.Action_execution.dependencies
  |> List.filter_map ~fn:(action_for_ref plan)

let compile_source_is = fun path kind action ->
  match action.Action_execution.action with
  | Action.CompileSource { source; _ } -> Path.equal source.source path && source.kind = kind
  | Action.CompileInterface { source; _ } ->
      kind = Action.LibraryInterface && Path.equal source.source path
  | Action.CompileNativeImplementation { source; _ } ->
      kind = Action.LibraryImplementation && Path.equal source.source path
  | _ -> false

let compile_byte_implementation_is = fun path action ->
  match action.Action_execution.action with
  | Action.CompileByteImplementation { source; _ } -> Path.equal source.source path
  | _ -> false

let require_compile_source = fun (plan: Module_plan.t) path kind ->
  match List.find plan.action_executions ~fn:(compile_source_is path kind) with
  | Some action -> Ok action
  | None -> Error ("expected compile source action for " ^ Path.to_string path)

let require_compile_byte_implementation = fun (plan: Module_plan.t) path ->
  match List.find plan.action_executions ~fn:(compile_byte_implementation_is path) with
  | Some action -> Ok action
  | None -> Error ("expected byte implementation action for " ^ Path.to_string path)

let compile_action_flags_include_opaque = fun action ->
  match action.Action_execution.action with
  | Action.CompileSource { flags; _ }
  | Action.CompileInterface { flags; _ }
  | Action.CompileByteImplementation { flags; _ }
  | Action.CompileNativeImplementation { flags; _ }
  | Action.CompileSources { flags; _ } ->
      List.any
        flags
        ~fn:(fun flag ->
          match flag with
          | Riot_toolchain.Ocamlc.Raw "-opaque" -> true
          | _ -> false)
  | _ -> false

let flags_include_impl = fun flags path ->
  List.any
    flags
    ~fn:(fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Impl impl_path -> Path.equal impl_path path
      | _ -> false)

let generated_implementations_use_impl_flags = fun (plan: Module_plan.t) ->
  let generated =
    plan.action_executions
    |> List.filter_map
      ~fn:(fun action ->
        match action.Action_execution.action with
        | Action.CompileSource {
            source = {
              source;
              kind = Action.LibraryImplementation;
              content = Some _;
            };
            flags;
            _;
          }
        | Action.CompileByteImplementation {
            source = {
              source;
              kind = Action.LibraryImplementation;
              content = Some _;
            };
            flags;
            _;
          }
        | Action.CompileNativeImplementation {
            source = {
              source;
              kind = Action.LibraryImplementation;
              content = Some _;
            };
            flags;
            _;
          } -> Some (source, flags)
        | _ -> None)
  in
  not (List.is_empty generated)
  && List.all generated ~fn:(fun (source, flags) -> flags_include_impl flags source)

let action_depends_on_source = fun deps path kind -> List.any deps ~fn:(compile_source_is path kind)

let action_depends_on_byte_implementation = fun deps path ->
  List.any
    deps
    ~fn:(compile_byte_implementation_is path)

let action_output_extensions = fun action ->
  Action.outputs action.Action_execution.action
  |> List.map ~fn:Path.extension

let expect_output_extensions = fun label action expected ->
  let actual = action_output_extensions action in
  if actual = expected then
    Ok ()
  else
    Error (label
    ^ " expected output extensions "
    ^ Int.to_string (List.length expected)
    ^ " entries, got "
    ^ Int.to_string (List.length actual))

let test_build_package_plans_package_dependencies_before_execution = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let app_build = build_package "app" in
  let app_goal = Goal.BuildPackage app_build in
  let dep_goal = build_goal "dep" in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) app_goal in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_goal_key keys dep_goal then
        if List.length keys = 2 && has_package_artifact_key keys app_build then
          Ok ()
        else
          Error "expected app build goal to plan manifest package deps and package artifact"
      else
        Error "expected app build goal to plan dep build goal before execution")

let test_build_package_without_package_dependencies_plans_no_dependencies = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) (build_goal "dep") in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if List.length keys = 1 && has_package_artifact_key keys (build_package "dep") then
        Ok ()
      else
        Error "expected package planning to include only package artifact for package without deps")

let test_build_package_does_not_plan_toolchain_readiness = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let goal = build_goal "dep" in
  let target =
    match goal with
    | Goal.BuildPackage build -> build.target
    | _ -> Riot_model.Target.current
  in
  let node = Work_node.goal ~id:(Work_node.Node_id.from_int 1) goal in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_toolchain_key keys target then
        Error "expected package-level planning not to add toolchain readiness"
      else
        Ok ())

let test_module_plan_dependencies_are_stable_without_source_analysis_state = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_module_plan_stability"
    (fun root ->
      let* workspace = source_package_workspace root in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let build = build_package "sourcepkg" in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* first =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
      in
      let* () =
        let has_source_analysis =
          List.any
            first
            ~fn:(fun request ->
              match request_key request with
              | Work_node.SourceAnalysisKey _ -> true
              | _ -> false)
        in
        if has_source_analysis then
          Ok ()
        else
          Error (
            "expected stable module planning to declare source analysis dependencies, got "
            ^ Int.to_string (List.length first)
            ^ ": "
            ^ (
              List.map first ~fn:key_name
              |> String.concat ", "
            )
          )
      in
      let* () = execute_source_requests services registry (source_analysis_requests first) in
      let* _ =
        Build_services.execute_node services registry node
        |> Result.map_err ~fn:Error.message
      in
      let* second =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
      in
      if List.map first ~fn:request_key = List.map second ~fn:request_key then
        Ok ()
      else
        Error "expected module planning dependencies to ignore source-analysis cache state") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_module_plan_declares_package_dependency_provider_nodes = fun _ctx ->
  let services = Build_services.create ~config:(config ()) () in
  let registry = Work_registry.create () in
  let app = build_package "app" in
  let dep = build_goal "dep" in
  let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) app in
  Build_services.plan_dependencies services registry node
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun keys ->
      if has_goal_key keys dep then
        Ok ()
      else
        Error "expected module planning to depend on declared package provider")

let test_module_plan_execution_requires_planned_source_analysis = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_module_plan_requires_sources"
    (fun root ->
      let* workspace = source_package_workspace root in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let build = build_package "sourcepkg" in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      match Build_services.execute_node services registry node with
      | Error (Error.ExecutorInvariantViolated _) -> Ok ()
      | Error error -> Error (Error.message error)
      | Ok _ -> Error "expected module planning execution to require planned source analysis") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_module_plan_cache_key_uses_source_summary_not_package_hash = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_module_plan_summary_cache"
    (fun root ->
      let* workspace = source_package_workspace root in
      let build = build_package "sourcepkg" in
      let config = Config.make ~workspace ~parallelism:1 () in
      let services = Build_services.create ~config () in
      let registry = Work_registry.create () in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* source_keys =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
        |> Result.and_then
          ~fn:(fun keys ->
            let source_keys = source_analysis_requests keys in
            if List.is_empty source_keys then
              Error "expected cold module planning to plan source analysis dependencies"
            else
              Ok source_keys)
      in
      let* () = execute_source_requests services registry source_keys in
      let* () =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected module plan to complete after source analysis"
        | Error error -> Error (Error.message error)
      in
      let* first_plan = require_module_plan services build in
      let* () =
        Fs.write "let value = 2\n" Path.(root / Path.v "sourcepkg/src/sourcepkg.ml")
        |> Result.map_err ~fn:IO.error_message
      in
      let cached_services = Build_services.create ~config () in
      let cached_registry = Work_registry.create () in
      let cached_node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* cached_source_keys =
        Build_services.plan_dependencies cached_services cached_registry cached_node
        |> Result.map_err ~fn:Error.message
        |> Result.and_then
          ~fn:(fun keys ->
            let source_keys = source_analysis_requests keys in
            if List.is_empty source_keys then
              Error "expected cached module planning to plan source summaries"
            else
              Ok source_keys)
      in
      let* () = execute_source_requests cached_services cached_registry cached_source_keys in
      let* () =
        match Build_services.execute_node cached_services cached_registry cached_node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected cached module plan to complete after source summaries"
        | Error error -> Error (Error.message error)
      in
      let* second_plan = require_module_plan cached_services build in
      if not (Crypto.Hash.equal first_plan.package_hash second_plan.package_hash) then
        if Crypto.Hash.equal first_plan.module_plan_hash second_plan.module_plan_hash then
          if
            Int.equal
              (List.length first_plan.action_executions)
              (List.length second_plan.action_executions)
          then
            Ok ()
          else
            Error "expected cached module plan action count to stay stable"
        else
          Error "expected body-only source edit to keep the module plan cache key stable"
      else
        Error "expected body-only source edit to change the raw package hash") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_opaque_implementation_dependencies_use_cmi_producers = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_opaque_deps"
    (fun root ->
      let* workspace = interface_dependency_workspace root in
      let build = build_package "sourcepkg" in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* source_keys =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
        |> Result.map ~fn:source_analysis_requests
      in
      let* () = execute_source_requests services registry source_keys in
      let* () =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected module plan to complete after source analysis"
        | Error error -> Error (Error.message error)
      in
      let* plan = require_module_plan services build in
      let b_path = Path.v "src/b.ml" in
      let a_mli_path = Path.v "src/a.mli" in
      let a_ml_path = Path.v "src/a.ml" in
      let c_ml_path = Path.v "src/c.ml" in
      let* b_action = require_compile_source plan b_path Action.LibraryImplementation in
      let deps = dependencies_for_action plan b_action in
      if not (compile_action_flags_include_opaque b_action) then
        Error "expected library implementation compile to use -opaque"
      else if not (generated_implementations_use_impl_flags plan) then
        Error "expected generated implementation sources to compile through -impl flags"
      else if not (action_depends_on_source deps a_mli_path Action.LibraryInterface) then
        Error "expected opaque B.ml compile to depend on A.mli as the CMI producer"
      else if action_depends_on_source deps a_ml_path Action.LibraryImplementation then
        Error "expected opaque B.ml compile not to depend on A.ml implementation CMX"
      else if not (action_depends_on_byte_implementation deps c_ml_path) then
        Error "expected opaque B.ml compile to depend on C.ml byte CMI producer when C has no interface"
      else if action_depends_on_source deps c_ml_path Action.LibraryImplementation then
        Error "expected opaque B.ml compile not to depend on C.ml native implementation"
      else
        Ok ()) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_opaque_implementation_dependencies_use_direct_cmi_frontier = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_direct_cmi_frontier"
    (fun root ->
      let* workspace = interface_dependency_workspace root in
      let build = build_package "sourcepkg" in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* source_keys =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
        |> Result.map ~fn:source_analysis_requests
      in
      let* () = execute_source_requests services registry source_keys in
      let* () =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected module plan to complete after source analysis"
        | Error error -> Error (Error.message error)
      in
      let* plan = require_module_plan services build in
      let root_path = Path.v "src/sourcepkg.ml" in
      let a_mli_path = Path.v "src/a.mli" in
      let b_path = Path.v "src/b.ml" in
      let c_path = Path.v "src/c.ml" in
      let* root_action = require_compile_source plan root_path Action.LibraryImplementation in
      let deps = dependencies_for_action plan root_action in
      if not (action_depends_on_byte_implementation deps b_path) then
        Error "expected root implementation to depend on direct B.ml CMI producer"
      else if action_depends_on_byte_implementation deps c_path then
        Error "expected root implementation not to depend on transitive C.ml CMI producer"
      else if action_depends_on_source deps a_mli_path Action.LibraryInterface then
        Error "expected root implementation not to depend on transitive A.mli CMI producer"
      else
        Ok ()) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_ocaml_compile_actions_declare_only_graph_required_outputs = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_ocaml_required_outputs"
    (fun root ->
      let* workspace = interface_dependency_workspace root in
      let build = build_package "sourcepkg" in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let node = Work_node.module_plan ~id:(Work_node.Node_id.from_int 1) build in
      let* source_keys =
        Build_services.plan_dependencies services registry node
        |> Result.map_err ~fn:Error.message
        |> Result.map ~fn:source_analysis_requests
      in
      let* () = execute_source_requests services registry source_keys in
      let* () =
        match Build_services.execute_node services registry node with
        | Ok (Work_result.Complete []) -> Ok ()
        | Ok _ -> Error "expected module plan to complete after source analysis"
        | Error error -> Error (Error.message error)
      in
      let* plan = require_module_plan services build in
      let* interface_action =
        require_compile_source plan (Path.v "src/a.mli") Action.LibraryInterface
      in
      let* byte_action = require_compile_byte_implementation plan (Path.v "src/c.ml") in
      let* native_action =
        require_compile_source plan (Path.v "src/a.ml") Action.LibraryImplementation
      in
      let* () = expect_output_extensions "interface CMI producer" interface_action [ Some ".cmi" ] in
      let* () =
        expect_output_extensions "byte implementation CMI producer" byte_action [ Some ".cmi" ]
      in
      expect_output_extensions
        "native implementation compiler"
        native_action
        [ Some ".cmx"; Some ".o" ]) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_source_analyzer_refreshes_changed_source = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_source_analysis_refresh"
    (fun root ->
      let package = package "sourcepkg" in
      let package_path = Path.(root / Path.v "sourcepkg") in
      let source = Path.v "src/sourcepkg.ml" in
      let source_path = Path.(package_path / source) in
      let* () =
        Fs.create_dir_all Path.(package_path / Path.v "src")
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "let value = 1\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let analyzer = Source_analyzer.create ~store () in
      let task =
        Riot_planner.Module_graph.{
          task_node_id = G.Node_id.next ();
          task_file = Riot_planner.Module_node.Concrete source;
          task_path = source;
          task_display_path = source_path;
          task_module_path = Some [ "Sourcepkg" ];
          task_implicit_opens = [];
          task_implicit_open_paths = [];
        }
      in
      let source_analysis = Source_analysis.make ~package ~task in
      let* () =
        Source_analyzer.execute analyzer source_analysis
        |> Result.map_err ~fn:Error.message
      in
      let* first =
        match Source_analyzer.find analyzer source_analysis.key with
        | Some analysis -> Ok analysis.Riot_planner.Module_graph.analysis_source_hash
        | None -> Error "expected first source analysis to be stored"
      in
      let* () =
        Fs.write "let value = 2\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Source_analyzer.execute analyzer source_analysis
        |> Result.map_err ~fn:Error.message
      in
      match Source_analyzer.find analyzer source_analysis.key with
      | Some analysis when not
        (Crypto.Hash.equal first analysis.Riot_planner.Module_graph.analysis_source_hash) -> Ok ()
      | Some _ -> Error "expected changed source to refresh the stored source analysis"
      | None -> Error "expected refreshed source analysis to be stored") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_source_analyzer_cache_reader_requires_completed_source_node = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_source_analysis_cache_reader"
    (fun root ->
      let package = package "sourcepkg" in
      let package_path = Path.(root / Path.v "sourcepkg") in
      let source = Path.v "src/sourcepkg.ml" in
      let source_path = Path.(package_path / source) in
      let* () =
        Fs.create_dir_all Path.(package_path / Path.v "src")
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "let value = 1\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let analyzer = Source_analyzer.create ~store () in
      let task = source_analysis_task ~source ~source_path in
      let package_manifest =
        Riot_model.Package.make
          ~name:package
          ~path:package_path
          ~relative_path:(Path.v "sourcepkg")
          ()
      in
      let source_analysis = Source_analysis.make ~package ~task in
      let cache_read () =
        Source_analyzer.analyze_from_cache
          analyzer
          package_manifest
          ~on_source_analyzed:(fun _ -> ())
          [ task ]
      in
      let* () =
        match cache_read () with
        | [ Error (Riot_planner.Planning_error.DependencyAnalysisFailed _) ] -> Ok ()
        | [ Ok _ ] -> Error "expected cache-only source reader not to analyze missing source"
        | _ -> Error "expected one cache-only source reader result"
      in
      let* () =
        Source_analyzer.execute analyzer source_analysis
        |> Result.map_err ~fn:Error.message
      in
      match cache_read () with
      | [ Ok analysis ] -> (
          match Source_analyzer.find analyzer source_analysis.key with
          | Some cached when Crypto.Hash.equal
            analysis.Riot_planner.Module_graph.analysis_source_hash
            cached.Riot_planner.Module_graph.analysis_source_hash -> Ok ()
          | Some _ -> Error "expected cache-only source reader to return stored analysis"
          | None -> Error "expected executed source analysis to be stored"
        )
      | [ Error error ] -> Error (Riot_planner.Planning_error.to_string error)
      | _ -> Error "expected one cache-only source reader result") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_source_analysis_cache_roundtrips_reusable_summary = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_source_analysis_cache_roundtrip"
    (fun root ->
      let package = package "sourcepkg" in
      let package_path = Path.(root / Path.v "sourcepkg") in
      let source = Path.v "src/sourcepkg.ml" in
      let source_path = Path.(package_path / source) in
      let* () =
        Fs.create_dir_all Path.(package_path / Path.v "src")
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "let value = 1\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let cache = Source_analysis_cache.create_cache ~store in
      let task = source_analysis_task ~source ~source_path in
      let* analysis =
        Riot_planner.Module_graph.analyze_source task
        |> Result.map_err ~fn:Riot_planner.Planning_error.to_string
      in
      let* payload =
        match Source_analysis_cache.payload ~package analysis with
        | Some payload -> Ok payload
        | None -> Error "expected successful source analysis payload"
      in
      let input_hash = Source_analysis_cache.input_hash ~package analysis in
      let* () =
        Graph_cache.put cache input_hash payload
        |> Result.map_err ~fn:Error.message
      in
      match Graph_cache.get cache input_hash with
      | Some (Ok loaded_payload) ->
          let* loaded_analysis =
            Source_analysis_cache.analysis ~task loaded_payload
            |> Result.map_err ~fn:Riot_planner.Planning_error.to_string
          in
          let* original_summary_hash =
            Source_analysis_cache.summary_hash_of_analysis analysis
            |> Result.map_err ~fn:Error.message
          in
          let* loaded_summary_hash =
            Source_analysis_cache.summary_hash_of_analysis loaded_analysis
            |> Result.map_err ~fn:Error.message
          in
          if
            Crypto.Hash.equal
              analysis.Riot_planner.Module_graph.analysis_source_hash
              (Source_analysis_cache.source_hash loaded_payload)
            && Crypto.Hash.equal
              analysis.Riot_planner.Module_graph.analysis_source_hash
              loaded_analysis.Riot_planner.Module_graph.analysis_source_hash
            && Crypto.Hash.equal original_summary_hash loaded_summary_hash
          then
            Ok ()
          else
            Error "expected source analysis cache to restore the full reusable summary"
      | Some (Error error) -> Error (Error.message error)
      | None -> Error "expected source analysis payload cache hit") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_source_summary_hash_ignores_body_only_source_changes = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_source_summary_hash"
    (fun root ->
      let package_path = Path.(root / Path.v "sourcepkg") in
      let source = Path.v "src/sourcepkg.ml" in
      let source_path = Path.(package_path / source) in
      let* () =
        Fs.create_dir_all Path.(package_path / Path.v "src")
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "let value = 1\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let task = source_analysis_task ~source ~source_path in
      let* first =
        Riot_planner.Module_graph.analyze_source task
        |> Result.map_err ~fn:Riot_planner.Planning_error.to_string
      in
      let* first_summary_hash =
        Source_analysis_cache.summary_hash_of_analysis first
        |> Result.map_err ~fn:Error.message
      in
      let* () =
        Fs.write "let value = 2\n" source_path
        |> Result.map_err ~fn:IO.error_message
      in
      let* second =
        Riot_planner.Module_graph.analyze_source task
        |> Result.map_err ~fn:Riot_planner.Planning_error.to_string
      in
      let* second_summary_hash =
        Source_analysis_cache.summary_hash_of_analysis second
        |> Result.map_err ~fn:Error.message
      in
      if
        Crypto.Hash.equal
          first.Riot_planner.Module_graph.analysis_source_hash
          second.Riot_planner.Module_graph.analysis_source_hash
      then
        Error "expected body-only source edit to change source analysis input hash"
      else if Crypto.Hash.equal first_summary_hash second_summary_hash then
        Ok ()
      else
        Error "expected body-only source edit to keep source summary hash stable") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let action_package = fun root ->
  let name = package "action-pkg" in
  Riot_model.Package.make
    ~name
    ~path:Path.(root / Path.v "action-pkg")
    ~relative_path:(Path.v "action-pkg")
    ()

let action_execution = fun root ~action ->
  let target = Riot_model.Target.current in
  let package = action_package root in
  let toolchain =
    Riot_toolchain.from_config_for_target
      ~config:(Riot_model.Toolchain_config.from_root ~root)
      ~target
  in
  Action_execution.make
    ~package
    ~profile:Riot_model.Profile.debug
    ~target
    ~toolchain
    ~action
    ~dependencies:[]
    ~sandbox_dir:Path.(root / Path.v "sandbox")

let action_execution_with_dependencies = fun root ~action ~dependencies ->
  let target = Riot_model.Target.current in
  let package = action_package root in
  let toolchain =
    Riot_toolchain.from_config_for_target
      ~config:(Riot_model.Toolchain_config.from_root ~root)
      ~target
  in
  Action_execution.make
    ~package
    ~profile:Riot_model.Profile.debug
    ~target
    ~toolchain
    ~action
    ~dependencies
    ~sandbox_dir:Path.(root / Path.v "sandbox")

let write_action_execution = fun root ->
  let output = Path.v "out.txt" in
  action_execution root ~action:(Action.WriteFile { destination = output; content = "hello" })

let copy_file_action_execution = fun root ->
  let source = Path.v "data.txt" in
  let destination = Path.v "copied.txt" in
  action_execution root ~action:(Action.CopyFile { source; destination })

let compile_action_execution = fun root ->
  action_execution
    root
    ~action:(Action.CompileC {
      source = Path.v "example.c";
      outputs = [ Path.v "example.o" ];
      ccflags = [];
    })

let test_action_execution_plans_toolchain_readiness_for_compiler_action = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_planned_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let action = compile_action_execution root in
      let node = Work_node.action_execution ~id:(Work_node.Node_id.from_int 1) action in
      Build_services.plan_dependencies services registry node
      |> Result.map_err ~fn:Error.message
      |> Result.and_then
        ~fn:(fun keys ->
          if has_toolchain_key keys action.ref_.target then
            Ok ()
          else
            Error "expected compiler action planning to include toolchain readiness")) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_action_execution_does_not_plan_toolchain_for_noncompiler_action = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_no_planned_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let services = Build_services.create ~config:(Config.make ~workspace ~parallelism:1 ()) () in
      let registry = Work_registry.create () in
      let action = write_action_execution root in
      let node = Work_node.action_execution ~id:(Work_node.Node_id.from_int 1) action in
      Build_services.plan_dependencies services registry node
      |> Result.map_err ~fn:Error.message
      |> Result.and_then
        ~fn:(fun keys ->
          if has_toolchain_key keys action.ref_.target then
            Error "expected noncompiler action planning not to include toolchain readiness"
          else
            Ok ())) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_noncompiler_action_executes_without_toolchain_readiness = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_no_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = write_action_execution root in
      match Action_executor.execute executor action with
      | Ok (Work_result.Complete []) ->
          let result =
            match Action_executor.find_result executor action.ref_ with
            | Some { Action_execution.status = Action_execution.Executed _; _ } -> Ok ()
            | Some _ -> Error "expected uncached noncompiler action to execute"
            | None -> Error "expected uncached noncompiler action result"
          in
          result
      | Ok _ -> Error "expected uncached noncompiler action not to request dependencies"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_action_reads_concrete_package_sources_without_sandbox_copy = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_package_source"
    (fun root ->
      let package = action_package root in
      let* () =
        Fs.create_dir_all package.path
        |> Result.map_err ~fn:IO.error_message
      in
      let* () =
        Fs.write "package-source\n" Path.(package.path / Path.v "data.txt")
        |> Result.map_err ~fn:IO.error_message
      in
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = copy_file_action_execution root in
      match Action_executor.execute executor action with
      | Ok (Work_result.Complete []) -> (
          match Fs.read Path.(action.sandbox_dir / Path.v "copied.txt") with
          | Ok "package-source\n" -> Ok ()
          | Ok _ -> Error "expected copied package source content"
          | Error error -> Error ("expected copied package source: " ^ IO.error_message error)
        )
      | Ok _ -> Error "expected package-source copy action not to request dependencies"
      | Error error -> Error (Error.message error)) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_uncached_compiler_action_requires_planned_toolchain_readiness = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_toolchain"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let action = compile_action_execution root in
      match Action_executor.execute executor action with
      | Error (Error.ExecutorInvariantViolated _) -> Ok ()
      | Error error -> Error (Error.message error)
      | Ok _ -> Error "expected uncached compiler action to require planned toolchain readiness") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_action_execution_requires_planned_action_dependencies = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build2_action_dependency"
    (fun root ->
      let workspace =
        Riot_model.Workspace.make ~root ~target_dir:Path.(root / Path.v "target") ~packages:[] ()
      in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let executor = Action_executor.create ~store ~toolchains () in
      let dependency = write_action_execution root in
      let dependent =
        action_execution_with_dependencies
          root
          ~dependencies:[ dependency.ref_ ]
          ~action:(Action.WriteFile { destination = Path.v "dependent.txt"; content = "ok" })
      in
      match Action_executor.execute executor dependent with
      | Error (Error.ExecutorInvariantViolated _) -> Ok ()
      | Error error -> Error (Error.message error)
      | Ok _ -> Error "expected action execution to require planned action dependencies") with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let test_package_sandbox_prepares_package_scoped_layout = fun _ctx ->
  let expect_exists = fun path ->
    match Fs.exists path with
    | Ok true -> Ok ()
    | Ok false -> Error ("expected package sandbox path to exist: " ^ Path.to_string path)
    | Error error -> Error ("failed to check package sandbox path: " ^ IO.error_message error)
  in
  match Fs.with_tempdir
    ~prefix:"riot_build2_package_sandbox"
    (fun root ->
      let* workspace = source_package_workspace root in
      let catalog = Package_catalog.create workspace in
      let store = Riot_store.Store.create ~workspace in
      let toolchains = Toolchain_service.create ~root () in
      let package_planning =
        Package_planning.create
          ~workspace
          ~catalog
          ~store
          ~session_id:(Riot_model.Session_id.make ())
          ~parallelism:1
          ~toolchains
          ()
      in
      let package_sandbox = Package_sandbox.create ~workspace ~store () in
      let build = build_package "sourcepkg" in
      let* input =
        Package_planning.resolve package_planning build
        |> Result.map_err ~fn:Error.message
      in
      let* sandbox_root =
        Package_sandbox.prepare package_sandbox input ~depset:[]
        |> Result.map_err ~fn:Error.message
      in
      let sandbox_name = Path.basename sandbox_root in
      if not (String.starts_with ~prefix:"sourcepkg-" sandbox_name) then
        Error ("expected package sandbox name to start with sourcepkg-, got " ^ sandbox_name)
      else
        let* () = expect_exists Path.(sandbox_root / Package_sandbox.check_dir) in
        let* () = expect_exists Path.(sandbox_root / Package_sandbox.link_dir) in
        expect_exists Path.(sandbox_root / Path.v "src/sourcepkg.ml")) with
  | Ok result -> result
  | Error error -> Error ("tempdir failed: " ^ IO.error_message error)

let tests =
  Test.[
    case
      "build package plans package dependencies before execution"
      test_build_package_plans_package_dependencies_before_execution;
    case
      "build package without package dependencies plans no dependencies"
      test_build_package_without_package_dependencies_plans_no_dependencies;
    case
      "build package does not plan toolchain readiness"
      test_build_package_does_not_plan_toolchain_readiness;
    case
      "module plan dependencies are stable without source analysis state"
      test_module_plan_dependencies_are_stable_without_source_analysis_state;
    case
      "module plan declares package dependency provider nodes"
      test_module_plan_declares_package_dependency_provider_nodes;
    case
      "module plan execution requires planned source analysis"
      test_module_plan_execution_requires_planned_source_analysis;
    case
      "module plan cache key uses source summary not package hash"
      test_module_plan_cache_key_uses_source_summary_not_package_hash;
    case
      "opaque implementation dependencies use CMI producers"
      test_opaque_implementation_dependencies_use_cmi_producers;
    case
      "opaque implementation dependencies use direct CMI frontier"
      test_opaque_implementation_dependencies_use_direct_cmi_frontier;
    case
      "OCaml compile actions declare only graph-required outputs"
      test_ocaml_compile_actions_declare_only_graph_required_outputs;
    case "source analyzer refreshes changed source" test_source_analyzer_refreshes_changed_source;
    case
      "source analyzer cache reader requires completed source node"
      test_source_analyzer_cache_reader_requires_completed_source_node;
    case
      "source analysis cache roundtrips reusable summary"
      test_source_analysis_cache_roundtrips_reusable_summary;
    case
      "source summary hash ignores body-only source changes"
      test_source_summary_hash_ignores_body_only_source_changes;
    case
      "action execution plans toolchain readiness for compiler action"
      test_action_execution_plans_toolchain_readiness_for_compiler_action;
    case
      "action execution does not plan toolchain for noncompiler action"
      test_action_execution_does_not_plan_toolchain_for_noncompiler_action;
    case
      "uncached noncompiler action executes without toolchain readiness"
      test_uncached_noncompiler_action_executes_without_toolchain_readiness;
    case
      "uncached action reads concrete package sources without sandbox copy"
      test_uncached_action_reads_concrete_package_sources_without_sandbox_copy;
    case
      "uncached compiler action requires planned toolchain readiness"
      test_uncached_compiler_action_requires_planned_toolchain_readiness;
    case
      "action execution requires planned action dependencies"
      test_action_execution_requires_planned_action_dependencies;
    case
      "package sandbox prepares package-scoped layout"
      test_package_sandbox_prepares_package_scoped_layout;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_build_services_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
