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

let results = fun t -> ConcurrentHashMap.values t.results

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
      | Riot_toolchain.Ocamlc.Impl path ->
          if Path.is_absolute path then
            flag
          else
            Riot_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)

let ocamlc_success = fun message -> Riot_toolchain.Ocamlc.Success { message; diagnostics = [] }

let ocamlc_failed = fun message -> Riot_toolchain.Ocamlc.Failed { message; diagnostics = [] }

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some dir -> Fs.create_dir_all dir
  | None -> Ok ()

let requires_toolchain = fun (action: Action_execution.t) -> Action.requires_toolchain action.action

let with_toolchain = fun ocamlc fn ->
  match ocamlc with
  | None -> ocamlc_failed "toolchain was not ready before compiler action execution"
  | Some ocamlc -> fn ocamlc

let path_error_message = fun __tmp1 ->
  match __tmp1 with
  | Path.InvalidUtf8 { path } -> "invalid utf8 path: " ^ path
  | Path.SystemInvalidUtf8 { syscall; path } -> syscall ^ " returned invalid utf8 path: " ^ path
  | Path.SystemError message -> message

let absolute_path = fun path ->
  if Path.is_absolute path then
    Ok path
  else
    Env.current_dir ()
    |> Result.map ~fn:(fun cwd -> Path.normalize Path.(cwd / path))
    |> Result.map_err
      ~fn:(fun error ->
        Error.ExecutorInvariantViolated {
          message = "failed to resolve current directory: " ^ path_error_message error;
        })

let resolve_source = fun ~package_root ~sandbox_dir source ->
  if Path.is_absolute source then
    source
  else
    let package_source = Path.join package_root source in
    match Fs.exists package_source with
    | Ok true -> package_source
    | Ok false
    | Error _ -> Path.join sandbox_dir source

let source_include_dir = fun source ->
  match Path.parent source with
  | Some dir -> dir
  | None -> Path.v "."

let stage_compile_library_source = fun ~package ~package_root ~sandbox_dir source ->
  let dst = Path.join sandbox_dir source.Action.staged in
  let* () =
    ensure_parent_dir dst
    |> Result.map_err
      ~fn:(fun error ->
        Error.ActionExecutionFailed {
          package;
          reason = "failed to create compile-library staging parent: " ^ IO.error_message error;
        })
  in
  let source_path = resolve_source ~package_root ~sandbox_dir source.source in
  let* raw =
    match source.content with
    | Some content -> Ok content
    | None ->
        Fs.read source_path
        |> Result.map_err
          ~fn:(fun error ->
            Error.ExecutorInvariantViolated {
              message = "failed to read compile-library source "
              ^ Path.to_string source_path
              ^ ": "
              ^ IO.error_message error;
            })
  in
  let prefix =
    let opens =
      source.opens
      |> List.map ~fn:(fun module_name -> "open! " ^ module_name)
      |> String.concat "\n"
    in
    let directive = "# 1 \"" ^ String.escaped (Path.to_string source_path) ^ "\"" in
    if String.is_empty opens then
      directive ^ "\n"
    else
      opens ^ "\n" ^ directive ^ "\n"
  in
  Fs.write (prefix ^ raw) dst
  |> Result.map_err
    ~fn:(fun error ->
      Error.ExecutorInvariantViolated {
        message = "failed to stage compile-library source "
        ^ Path.to_string dst
        ^ ": "
        ^ IO.error_message error;
      })

let run_action = fun
  ~(package:Riot_model.Package_name.t)
  ~(package_root:Path.t)
  ?c_compiler
  ocamlc
  sandbox_dir
  action ->
  match action with
  | Action.CompileC { source; outputs = output :: _; ccflags } ->
      with_toolchain
        ocamlc
        (fun ocamlc ->
          let source = resolve_source ~package_root ~sandbox_dir source in
          let source_dir = [ source_include_dir source ] in
          Riot_toolchain.Ocamlc.compile_c
            ocamlc
            ~cwd:sandbox_dir
            ~includes:source_dir
            ?cc:c_compiler
            ~ccflags
            ~output:(Path.join sandbox_dir output)
            source
          |> Riot_toolchain.Ocamlc.run)
  | Action.CompileSource {
      source;
      outputs = _ :: _;
      output;
      includes;
      flags;
    } ->
      with_toolchain
        ocamlc
        (fun ocamlc ->
          match stage_compile_library_source ~package ~package_root ~sandbox_dir source with
          | Error (Error.ExecutorInvariantViolated { message }) -> ocamlc_failed message
          | Error (Error.ActionExecutionFailed { reason; _ }) -> ocamlc_failed reason
          | Error error -> ocamlc_failed (Error.message error)
          | Ok () ->
              let invocation =
                match source.kind with
                | Action.LibraryInterface ->
                    Riot_toolchain.Ocamlc.compile_interface
                      ocamlc
                      ~cwd:sandbox_dir
                      ~includes:(resolve_include_paths sandbox_dir includes)
                      ~flags:(make_flags_absolute sandbox_dir flags)
                      ~output
                      source.staged
                | Action.LibraryImplementation ->
                    Riot_toolchain.Ocamlc.compile_impl
                      ocamlc
                      ~cwd:sandbox_dir
                      ~includes:(resolve_include_paths sandbox_dir includes)
                      ~flags:(make_flags_absolute sandbox_dir flags)
                      ~output
                      source.staged
              in
              Riot_toolchain.Ocamlc.run invocation)
  | Action.CompileSources {
      sources;
      outputs = _ :: _;
      includes;
      flags;
    } ->
      with_toolchain
        ocamlc
        (fun ocamlc ->
          let staged =
            sources
            |> List.fold_left
              ~init:(Ok [])
              ~fn:(fun acc source ->
                let* acc = acc in
                let* () = stage_compile_library_source ~package ~package_root ~sandbox_dir source in
                Ok (source.Action.staged :: acc))
          in
          match staged with
          | Error (Error.ExecutorInvariantViolated { message }) -> ocamlc_failed message
          | Error (Error.ActionExecutionFailed { reason; _ }) -> ocamlc_failed reason
          | Error error -> ocamlc_failed (Error.message error)
          | Ok staged ->
              Riot_toolchain.Ocamlc.compile_sources
                ocamlc
                ~cwd:sandbox_dir
                ~includes:(resolve_include_paths sandbox_dir includes)
                ~flags:(make_flags_absolute sandbox_dir flags)
                (List.reverse staged)
              |> Riot_toolchain.Ocamlc.run)
  | Action.CompileLibrary {
      sources;
      objects;
      outputs = _ :: _;
      output;
      includes;
      flags;
    } ->
      with_toolchain
        ocamlc
        (fun ocamlc ->
          let staged =
            sources
            |> List.fold_left
              ~init:(Ok [])
              ~fn:(fun acc source ->
                let* acc = acc in
                let* () = stage_compile_library_source ~package ~package_root ~sandbox_dir source in
                Ok (source.Action.staged :: acc))
          in
          match staged with
          | Error (Error.ExecutorInvariantViolated { message }) -> ocamlc_failed message
          | Error (Error.ActionExecutionFailed { reason; _ }) -> ocamlc_failed reason
          | Error error -> ocamlc_failed (Error.message error)
          | Ok staged ->
              Riot_toolchain.Ocamlc.compile_library
                ocamlc
                ~cwd:sandbox_dir
                ~includes:(resolve_include_paths sandbox_dir includes)
                ~flags:(make_flags_absolute sandbox_dir flags)
                ~output
                (List.reverse staged @ objects)
              |> Riot_toolchain.Ocamlc.run)
  | Action.CopyFile { source; destination } ->
      let src = resolve_source ~package_root ~sandbox_dir source in
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.copy ~src ~dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "copied")
        ~error:(fun error -> ocamlc_failed ("copy failed: " ^ IO.error_message error))
  | Action.WriteFile { destination; content } ->
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      Fs.write content dst
      |> Result.fold
        ~ok:(fun () -> ocamlc_success "written")
        ~error:(fun error -> ocamlc_failed ("write failed: " ^ IO.error_message error))
  | Action.CompileC { outputs = []; _ }
  | Action.CompileSource { outputs = []; _ }
  | Action.CompileSources { outputs = []; _ }
  | Action.CompileLibrary { outputs = []; _ } -> ocamlc_failed "action has no outputs"

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
  let package = action.package in
  let* sandbox_dir = absolute_path action.sandbox_dir in
  let _ = Fs.create_dir_all sandbox_dir in
  let* package_root = absolute_path package.path in
  let ocamlc = Option.map toolchain ~fn:Riot_toolchain.ocamlc in
  let c_compiler = Option.and_then toolchain ~fn:Riot_toolchain.c_compiler in
  let* warnings =
    match run_action
      ~package:package.name
      ~package_root
      ?c_compiler
      ocamlc
      sandbox_dir
      action.action with
    | Riot_toolchain.Ocamlc.Success _ as result ->
        Ok (Riot_toolchain.Ocamlc.get_ocamlc_warnings result)
    | Riot_toolchain.Ocamlc.Failed _ as result ->
        Error (Error.ActionExecutionFailed {
          package = package.name;
          reason = Riot_toolchain.Ocamlc.get_output result;
        })
  in
  let abs_outputs = List.map (Action.outputs action.action) ~fn:(Path.join sandbox_dir) in
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
      Ok (Work_result.Complete [])

let promote_cached = fun t (action: Action_execution.t) (artifact: Riot_store.Artifact.t) ->
  let* target_dir = absolute_path action.sandbox_dir in
  match Riot_store.Store.promote_action t.store artifact.input_hash ~target_dir with
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
      Ok (Work_result.Complete [])

let dependency_output_hashes = fun t (action: Action_execution.t) ->
  let package = action.ref_.package in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | ref_ :: rest ->
        match find_result t ref_ with
        | None ->
            Error (Error.ExecutorInvariantViolated {
              message = "action execution for "
              ^ Riot_model.Package_name.to_string package
              ^ " started before planned action dependency completed";
            })
        | Some { Action_execution.status = Failed reason; _ } ->
            Error (Error.ActionExecutionFailed { package; reason })
        | Some result ->
            match Action_execution.artifact result with
            | None ->
                Error (Error.ExecutorInvariantViolated {
                  message = "completed action dependency had no artifact";
                })
            | Some artifact -> loop (artifact.Riot_store.Artifact.output_hash :: acc) rest
  in
  loop [] action.dependencies

let execute = fun t (action: Action_execution.t) ->
  match find_result t action.ref_ with
  | Some _ -> Ok (Work_result.Complete [])
  | None ->
      let missing =
        action.dependencies
        |> List.filter ~fn:(fun ref_ -> Option.is_none (find_result t ref_))
      in
      if not (List.is_empty missing) then
        Ok (Work_result.RequeueWithDependencies (List.map missing ~fn:action_dependency_key))
      else
        let* dependency_output_hashes = dependency_output_hashes t action in
        let action_input_hash =
          compute_action_input_hash ~planned_hash:action.ref_.hash ~dependency_output_hashes
        in
        match Riot_store.Store.get_action t.store action_input_hash with
        | Some artifact -> promote_cached t action artifact
        | None ->
            if Action.requires_toolchain action.action then
              match Toolchain_service.find t.toolchains action.ref_.target with
              | Some toolchain -> execute_uncached t action (Some toolchain) action_input_hash
              | None ->
                  Ok (Work_result.RequeueWithDependencies [
                    Work_node.ToolchainReadyKey { target = action.ref_.target };
                  ])
            else
              execute_uncached t action None action_input_hash
