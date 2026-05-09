open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type t = {
  store: Riot_store.Store.t;
  toolchains: Toolchain_service.t;
  results: (Action_execution.ref_, Action_execution.result) ConcurrentHashMap.t;
}

let create = fun ~store ~toolchains () -> {
  store;
  toolchains;
  results = ConcurrentHashMap.with_capacity ~size:4_096;
}

let find_result = fun t ref_ -> ConcurrentHashMap.get t.results ~key:ref_

let failure = fun t ref_ ->
  find_result t ref_
  |> Option.and_then
    ~fn:(fun result ->
      match result.Action_execution.status with
      | Action_execution.Failed reason -> Some reason
      | Cached _
      | Executed _ -> None)

let artifact = fun t ref_ ->
  find_result t ref_
  |> Option.and_then ~fn:Action_execution.artifact

let store_result = fun t result ->
  ignore
    (ConcurrentHashMap.insert t.results ~key:result.Action_execution.ref_ ~value:result)

let store_error = fun ?package reason -> Error.StoreFailed { package; reason }

let action_dependency_key = fun action_ref -> Work_node.ActionExecutionKey action_ref

let compute_action_input_hash = fun ~planned_hash ~dependency_output_hashes ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-build2-action-input:v1";
  Crypto.Sha256.write_hash hasher planned_hash;
  List.for_each dependency_output_hashes ~fn:(Crypto.Sha256.write_hash hasher);
  Crypto.Sha256.finish hasher

let resolve_include_paths = fun sandbox_dir includes ->
  List.map
    includes
    ~fn:(fun inc ->
      let inc_str = Path.to_string inc in
      if Path.is_absolute inc || String.starts_with ~prefix:"+" inc_str then
        inc
      else
        Path.join sandbox_dir inc)

let make_flags_absolute = fun sandbox_dir flags ->
  List.map
    flags
    ~fn:(fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Impl path -> Riot_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)

let ocamlc_success = fun message -> Riot_toolchain.Ocamlc.Success { message; diagnostics = [] }

let ocamlc_failed = fun message -> Riot_toolchain.Ocamlc.Failed { message; diagnostics = [] }

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some dir -> Fs.create_dir_all dir
  | None -> Ok ()

let run_action = fun ?c_compiler ocamlc sandbox_dir action ->
  match action with
  | Riot_planner.Action.CompileInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.compile_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CompileImplementation {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.compile_impl
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | GenerateInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      Riot_toolchain.Ocamlc.generate_interface
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~flags:(make_flags_absolute sandbox_dir flags)
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CompileC { source; outputs = output :: _; ccflags } ->
      let source_dir =
        match Path.parent source with
        | Some dir -> [ Path.join sandbox_dir dir ]
        | None -> [ sandbox_dir ]
      in
      Riot_toolchain.Ocamlc.compile_c
        ocamlc
        ~cwd:sandbox_dir
        ~includes:source_dir
        ?cc:c_compiler
        ~ccflags
        ~output:(Path.join sandbox_dir output)
        (Path.join sandbox_dir source)
      |> Riot_toolchain.Ocamlc.run
  | CreateLibrary { outputs = output :: _; objects; includes } ->
      Riot_toolchain.Ocamlc.create_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~output:(Path.join sandbox_dir output)
        objects
      |> Riot_toolchain.Ocamlc.run
  | CreateExecutable {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Riot_toolchain.Ocamlc.create_executable
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~libs:libraries
        ?cc:c_compiler
        ~cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:(Path.join sandbox_dir output)
        (List.map objects ~fn:(Path.join sandbox_dir))
      |> Riot_toolchain.Ocamlc.run
  | CreateSharedLibrary {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      Riot_toolchain.Ocamlc.create_shared_library
        ocamlc
        ~cwd:sandbox_dir
        ~includes:(resolve_include_paths sandbox_dir includes)
        ~libs:libraries
        ?cc:c_compiler
        ~cclibs
        ~ccopt_flags
        ~cclib_flags
        ~output:(Path.join sandbox_dir output)
        (List.map objects ~fn:(Path.join sandbox_dir))
      |> Riot_toolchain.Ocamlc.run
  | CopyFile { source; destination } ->
      let src =
        if Path.is_absolute source then
          source
        else
          Path.join sandbox_dir source
      in
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.copy ~src ~dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "copied")
        ~error:(fun error -> ocamlc_failed ("copy failed: " ^ IO.error_message error))
  | WriteFile { destination; content } ->
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.write content dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "written")
        ~error:(fun error -> ocamlc_failed ("write failed: " ^ IO.error_message error))
  | BuildForeignDependency { name; _ } ->
      ocamlc_failed ("foreign dependency builds are not supported yet: " ^ name)
  | CompileInterface { outputs = []; _ }
  | CompileImplementation { outputs = []; _ }
  | GenerateInterface { outputs = []; _ }
  | CompileC { outputs = []; _ }
  | CreateLibrary { outputs = []; _ }
  | CreateExecutable { outputs = []; _ }
  | CreateSharedLibrary { outputs = []; _ } -> ocamlc_failed "action has no outputs"

let resolve_source_for_copy = fun ~(package:Riot_model.Package.t) source ->
  if Path.is_absolute source then
    source
  else
    Path.join package.path source

let copy_sources = fun ~(package:Riot_model.Package.t) ~sandbox_dir sources ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | source :: rest ->
        let src = resolve_source_for_copy ~package source in
        let dst = Path.join sandbox_dir source in
        let* () =
          match Path.parent dst with
          | Some dir ->
              Fs.create_dir_all dir
              |> Result.map_err ~fn:IO.error_message
          | None -> Ok ()
        in
        let* () =
          Fs.copy ~src ~dst
          |> Result.map_err ~fn:IO.error_message
        in
        loop rest
  in
  loop sources

let verify_outputs = fun outputs ->
  let missing =
    List.filter
      outputs
      ~fn:(fun output ->
        match Fs.exists output with
        | Ok true -> false
        | Ok false
        | Error _ -> true)
  in
  if List.is_empty missing then
    Ok ()
  else
    Error missing

let execute_uncached = fun t (action: Action_execution.t) toolchain action_input_hash ->
  let node = action.action in
  let spec = Riot_planner.Action_node.value node in
  let package = spec.package in
  let sandbox_dir = action.sandbox_dir in
  let _ = Fs.create_dir_all sandbox_dir in
  let* () =
    copy_sources ~package ~sandbox_dir spec.srcs
    |> Result.map_err
      ~fn:(fun reason -> Error.ActionExecutionFailed { package = package.name; reason })
  in
  let ocamlc = Riot_toolchain.ocamlc toolchain in
  let c_compiler = Riot_toolchain.c_compiler toolchain in
  let rec run_all warnings = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok warnings
    | action :: rest ->
        match run_action ?c_compiler ocamlc sandbox_dir action with
        | Riot_toolchain.Ocamlc.Success _ as result ->
            run_all (warnings @ Riot_toolchain.Ocamlc.get_ocamlc_warnings result) rest
        | Riot_toolchain.Ocamlc.Failed _ as result ->
            Error (Error.ActionExecutionFailed {
              package = package.name;
              reason = Riot_toolchain.Ocamlc.get_output result;
            })
  in
  let* warnings = run_all [] spec.actions in
  let abs_outputs = List.map spec.outs ~fn:(Path.join sandbox_dir) in
  let* () =
    verify_outputs abs_outputs
    |> Result.map_err
      ~fn:(fun missing -> Error.ActionOutputsNotCreated { package = package.name; missing })
  in
  match Riot_store.Store.save_action
    t.store
    ~package:(Riot_model.Package_name.to_string package.name)
    ~ocamlc_warnings:warnings
    ~input_hash:action_input_hash
    ~sandbox_dir
    ~outs:abs_outputs with
  | Error error -> Error (store_error ~package:package.name (Riot_store.Store.error_message error))
  | Ok saved_artifact ->
      store_result
        t
        {
          Action_execution.ref_ = action.ref_;
          status = Action_execution.Executed saved_artifact;
          ocamlc_warnings = warnings;
        };
      Ok (Executor.Complete [])

let promote_cached = fun t (action: Action_execution.t) (artifact: Riot_store.Artifact.t) ->
  match Riot_store.Store.promote_action t.store artifact.input_hash ~target_dir:action.sandbox_dir with
  | Error error ->
      Error (store_error ~package:action.ref_.package (Riot_store.Store.error_message error))
  | Ok () ->
      store_result
        t
        {
          Action_execution.ref_ = action.ref_;
          status = Action_execution.Cached artifact;
          ocamlc_warnings = artifact.ocamlc_warnings;
        };
      Ok (Executor.Complete [])

let execute = fun t (action: Action_execution.t) ->
  let missing =
    action.dependencies
    |> List.filter ~fn:(fun ref_ -> Option.is_none (find_result t ref_))
  in
  if not (List.is_empty missing) then
    Ok (Executor.RequeueWithDependencies (List.map missing ~fn:action_dependency_key))
  else
    let failed =
      action.dependencies
      |> List.filter_map ~fn:(failure t)
    in
    match failed with
    | reason :: _ ->
        let package = action.ref_.package in
        store_result
          t
          {
            Action_execution.ref_ = action.ref_;
            status = Action_execution.Failed reason;
            ocamlc_warnings = [];
          };
        Error (Error.ActionExecutionFailed { package; reason })
    | [] ->
        match Toolchain_service.find t.toolchains action.ref_.target with
        | None ->
            Error (Error.ToolchainFailed {
              target = action.ref_.target;
              reason = "toolchain was not ready before action execution";
            })
        | Some toolchain ->
            let dependency_output_hashes =
              action.dependencies
              |> List.filter_map
                ~fn:(fun ref_ ->
                  artifact t ref_
                  |> Option.map ~fn:(fun artifact -> artifact.Riot_store.Artifact.output_hash))
            in
            let action_input_hash =
              compute_action_input_hash ~planned_hash:action.ref_.hash ~dependency_output_hashes
            in
            match Riot_store.Store.get_action t.store action_input_hash with
            | Some artifact -> promote_cached t action artifact
            | None -> execute_uncached t action toolchain action_input_hash
