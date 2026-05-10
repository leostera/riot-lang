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

type action_run = {
  result: Riot_toolchain.Ocamlc.result;
  source_staging_duration: Time.Duration.t;
  command_execution_duration: Time.Duration.t;
}

let measured_duration = fun duration ->
  if Time.Duration.is_zero duration then
    Time.Duration.from_nanos 1
  else
    duration

let timed = fun fn ->
  let result, duration = Timer.measure fn in
  (result, measured_duration duration)

let action_run_result = fun
  ?(source_staging_duration = Time.Duration.zero)
  ?(command_execution_duration = Time.Duration.zero)
  result -> {
  result;
  source_staging_duration;
  command_execution_duration;
}

let action_run_failed = fun
  ?(source_staging_duration = Time.Duration.zero)
  ?(command_execution_duration = Time.Duration.zero)
  message ->
  action_run_result
    ~source_staging_duration
    ~command_execution_duration
    (ocamlc_failed message)

let ensure_parent_dir = fun path ->
  match Path.parent path with
  | Some dir -> Fs.create_dir_all dir
  | None -> Ok ()

let requires_toolchain = fun (action: Action_execution.t) -> Action.requires_toolchain action.action

let with_toolchain = fun ocamlc fn ->
  match ocamlc with
  | None -> action_run_failed "toolchain was not ready before compiler action execution"
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

let stage_single_compile_library_source = fun ~package ~package_root ~sandbox_dir source ->
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

let stage_compile_library_interface_if_present = fun ~package ~package_root ~sandbox_dir source ->
  match (source.Action.kind, source.content, Path.extension source.source) with
  | (Action.LibraryImplementation, None, Some ".ml") ->
      let interface_source = Path.replace_extension source.source ~ext:"mli" in
      let interface_path = resolve_source ~package_root ~sandbox_dir interface_source in
      (
        match Fs.exists interface_path with
        | Ok true ->
            let interface =
              {
                source with
                source = interface_source;
                staged = Path.replace_extension source.staged ~ext:"mli";
                kind = Action.LibraryInterface;
              }
            in
            stage_single_compile_library_source ~package ~package_root ~sandbox_dir interface
        | Ok false -> Ok ()
        | Error error ->
            Error (Error.ExecutorInvariantViolated {
              message = "failed to check compile-library interface source "
              ^ Path.to_string interface_path
              ^ ": "
              ^ IO.error_message error;
            })
      )
  | _ -> Ok ()

let stage_compile_library_source = fun ~package ~package_root ~sandbox_dir source ->
  let* () = stage_single_compile_library_source ~package ~package_root ~sandbox_dir source in
  stage_compile_library_interface_if_present ~package ~package_root ~sandbox_dir source

let stage_compile_library_sources = fun ~package ~package_root ~sandbox_dir sources ->
  if List.is_empty sources then
    (Ok [], Time.Duration.zero)
  else
    timed
      (fun () ->
        sources
        |> List.fold_left
          ~init:(Ok [])
          ~fn:(fun acc source ->
            let* acc = acc in
            let* () = stage_compile_library_source ~package ~package_root ~sandbox_dir source in
            Ok (source.Action.staged :: acc)))

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
          let invocation =
            Riot_toolchain.Ocamlc.compile_c
              ocamlc
              ~cwd:sandbox_dir
              ~includes:source_dir
              ?cc:c_compiler
              ~ccflags
              ~output:(Path.join sandbox_dir output)
              source
          in
          let result, command_execution_duration =
            timed (fun () -> Riot_toolchain.Ocamlc.run invocation)
          in
          action_run_result ~command_execution_duration result)
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
          let stage_result, source_staging_duration =
            timed (fun () -> stage_compile_library_source ~package ~package_root ~sandbox_dir source)
          in
          match stage_result with
          | Error (Error.ExecutorInvariantViolated { message }) ->
              action_run_failed ~source_staging_duration message
          | Error (Error.ActionExecutionFailed { reason; _ }) ->
              action_run_failed ~source_staging_duration reason
          | Error error -> action_run_failed ~source_staging_duration (Error.message error)
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
              let result, command_execution_duration =
                timed (fun () -> Riot_toolchain.Ocamlc.run invocation)
              in
              action_run_result ~source_staging_duration ~command_execution_duration result)
  | Action.CompileSources {
      sources;
      outputs = _ :: _;
      includes;
      flags;
    } ->
      with_toolchain
        ocamlc
        (fun ocamlc ->
          let staged, source_staging_duration =
            stage_compile_library_sources ~package ~package_root ~sandbox_dir sources
          in
          match staged with
          | Error (Error.ExecutorInvariantViolated { message }) ->
              action_run_failed ~source_staging_duration message
          | Error (Error.ActionExecutionFailed { reason; _ }) ->
              action_run_failed ~source_staging_duration reason
          | Error error -> action_run_failed ~source_staging_duration (Error.message error)
          | Ok staged ->
              let invocation =
                Riot_toolchain.Ocamlc.compile_sources
                  ocamlc
                  ~cwd:sandbox_dir
                  ~includes:(resolve_include_paths sandbox_dir includes)
                  ~flags:(make_flags_absolute sandbox_dir flags)
                  (List.reverse staged)
              in
              let result, command_execution_duration =
                timed (fun () -> Riot_toolchain.Ocamlc.run invocation)
              in
              action_run_result ~source_staging_duration ~command_execution_duration result)
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
          let staged, source_staging_duration =
            stage_compile_library_sources ~package ~package_root ~sandbox_dir sources
          in
          match staged with
          | Error (Error.ExecutorInvariantViolated { message }) ->
              action_run_failed ~source_staging_duration message
          | Error (Error.ActionExecutionFailed { reason; _ }) ->
              action_run_failed ~source_staging_duration reason
          | Error error -> action_run_failed ~source_staging_duration (Error.message error)
          | Ok staged ->
              let invocation =
                Riot_toolchain.Ocamlc.compile_library
                  ocamlc
                  ~cwd:sandbox_dir
                  ~includes:(resolve_include_paths sandbox_dir includes)
                  ~flags:(make_flags_absolute sandbox_dir flags)
                  ~output
                  (List.reverse staged @ objects)
              in
              let result, command_execution_duration =
                timed (fun () -> Riot_toolchain.Ocamlc.run invocation)
              in
              action_run_result ~source_staging_duration ~command_execution_duration result)
  | Action.CopyFile { source; destination } ->
      let src = resolve_source ~package_root ~sandbox_dir source in
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      let result, command_execution_duration =
        timed
          (fun () ->
            Fs.copy ~src ~dst
            |> Result.fold
              ~ok:(fun () -> ocamlc_success "copied")
              ~error:(fun error -> ocamlc_failed ("copy failed: " ^ IO.error_message error)))
      in
      action_run_result ~command_execution_duration result
  | Action.WriteFile { destination; content } ->
      let dst = Path.join sandbox_dir destination in
      let _ = ensure_parent_dir dst in
      let result, command_execution_duration =
        timed
          (fun () ->
            Fs.write content dst
            |> Result.fold
              ~ok:(fun () -> ocamlc_success "written")
              ~error:(fun error -> ocamlc_failed ("write failed: " ^ IO.error_message error)))
      in
      action_run_result ~command_execution_duration result
  | Action.CompileC { outputs = []; _ }
  | Action.CompileSource { outputs = []; _ }
  | Action.CompileSources { outputs = []; _ }
  | Action.CompileLibrary { outputs = []; _ } -> action_run_failed "action has no outputs"

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

let finish_timing = fun started_at (timing: Action_execution.timing) ->
  { timing with total = Time.Instant.duration_since ~earlier:started_at (Time.Instant.now ()) }

let is_final_archive_action = fun __tmp1 ->
  match __tmp1 with
  | Action.CompileLibrary { sources = []; outputs; _ } ->
      List.any outputs ~fn:(fun output -> Path.extension output = Some ".cmxa")
  | _ -> false

let same_relative_path = fun left right ->
  String.equal (Path.to_string (Path.normalize left)) (Path.to_string (Path.normalize right))

let artifact_file_path = fun t (artifact: Riot_store.Artifact.t) path ->
  if Path.is_absolute path then
    Ok (Some path)
  else
    artifact.files
    |> List.find ~fn:(fun (entry: Riot_store.Manifest.file_entry) ->
      same_relative_path entry.path path)
    |> Option.map
      ~fn:(fun (entry: Riot_store.Manifest.file_entry) ->
        Path.(Riot_store.Store.action_hash_dir_of t.store artifact.input_hash / entry.path))
    |> Option.map ~fn:absolute_path
    |> Option.transpose

let dependency_artifact_for_ref = fun t (ref_: Action_execution.ref_) ->
  match find_result t ref_ with
  | None -> None
  | Some result -> Action_execution.artifact result

let dependency_file_path = fun t (action: Action_execution.t) path ->
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok None
    | ref_ :: rest -> (
        match dependency_artifact_for_ref t ref_ with
        | None -> loop rest
        | Some artifact -> (
            match artifact_file_path t artifact path with
            | Ok (Some path) -> Ok (Some path)
            | Ok None -> loop rest
            | Error error -> Error error
          )
      )
  in
  loop action.dependencies

let resolve_archive_objects_from_dependencies = fun t (action: Action_execution.t) objects ->
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | object_ :: rest -> (
        match dependency_file_path t action object_ with
        | Ok (Some path) -> loop (path :: acc) rest
        | Ok None ->
            Error (Error.ExecutorInvariantViolated {
              message = "archive action for "
              ^ Riot_model.Package_name.to_string action.ref_.package
              ^ " could not find dependency object "
              ^ Path.to_string object_
              ^ " in completed action artifacts";
            })
        | Error error -> Error error
      )
  in
  loop [] objects

let action_for_execution = fun t (action: Action_execution.t) ->
  match action.action with
  | Action.CompileLibrary {
      sources = [];
      objects;
      outputs;
      output;
      includes;
      flags;
    } when is_final_archive_action action.action ->
      let* objects = resolve_archive_objects_from_dependencies t action objects in
      Ok (Action.CompileLibrary {
        sources = [];
        objects;
        outputs;
        output;
        includes;
        flags;
      })
  | _ -> Ok action.action

let artifact_paths = fun (artifact: Riot_store.Artifact.t) ->
  artifact.files
  |> List.map ~fn:(fun (entry: Riot_store.Manifest.file_entry) -> entry.path)

let artifact_paths_with_extension = fun (artifact: Riot_store.Artifact.t) extension ->
  artifact.files
  |> List.filter_map
    ~fn:(fun (entry: Riot_store.Manifest.file_entry) ->
      if Path.extension entry.path = Some extension then
        Some entry.path
      else
        None)

let artifact_paths_with_extensions = fun artifact extensions ->
  extensions
  |> List.flat_map ~fn:(artifact_paths_with_extension artifact)

let dependency_outputs_for_consumer = fun (consumer: Action_execution.t) artifact ->
  match consumer.action with
  | Action.CompileSource _
  | Action.CompileSources _ -> artifact_paths_with_extensions artifact [ ".cmi"; ".cmx" ]
  | Action.CompileLibrary { sources = []; outputs; _ }
    when List.any outputs ~fn:(fun output -> Path.extension output = Some ".cmxa") -> []
  | Action.CompileC _
  | Action.CompileLibrary _
  | Action.CopyFile _
  | Action.WriteFile _ -> artifact_paths artifact

let dependency_promotion_preserves_permissions = fun (consumer: Action_execution.t) ->
  match consumer.action with
  | Action.CompileC _
  | Action.CompileSource _
  | Action.CompileSources _
  | Action.CompileLibrary _ -> false
  | Action.CopyFile _
  | Action.WriteFile _ -> true

let materialize_cached_dependencies_for_execution = fun t (action: Action_execution.t) ~sandbox_dir ->
  let preserve_permissions = dependency_promotion_preserves_permissions action in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | ref_ :: rest -> (
        match find_result t ref_ with
        | None ->
            Error (Error.ExecutorInvariantViolated {
              message = "action execution for "
              ^ Riot_model.Package_name.to_string action.ref_.package
              ^ " started before planned action dependency completed";
            })
        | Some { Action_execution.status = Failed reason; _ } ->
            Error (Error.ActionExecutionFailed { package = action.ref_.package; reason })
        | Some { Action_execution.status = Executed artifact; _ }
        | Some { Action_execution.status = Cached artifact; _ } ->
            let files = dependency_outputs_for_consumer action artifact in
            if List.is_empty files then
              loop rest
            else
              Riot_store.Store.promote_action_artifact_files
                ~preserve_permissions
                t.store
                artifact
                ~files
                ~target_dir:sandbox_dir
              |> Result.map_err
                ~fn:(fun error ->
                  store_error ~package:action.ref_.package (Riot_store.Store.error_message error))
              |> Result.and_then ~fn:(fun () -> loop rest)
      )
  in
  loop action.dependencies

let execute_uncached = fun
  t
  (action: Action_execution.t)
  toolchain
  action_input_hash
  ~started_at
  ~(timing: Action_execution.timing) ->
  let package = action.package in
  let sandbox_result, sandbox_prepare_duration =
    timed
      (fun () ->
        let* sandbox_dir = absolute_path action.sandbox_dir in
        let _ = Fs.create_dir_all sandbox_dir in
        let* package_root = absolute_path package.path in
        Ok (sandbox_dir, package_root))
  in
  let* (sandbox_dir, package_root) = sandbox_result in
  let ocamlc = Option.map toolchain ~fn:Riot_toolchain.ocamlc in
  let c_compiler = Option.and_then toolchain ~fn:Riot_toolchain.c_compiler in
  let dependency_materialization_result, dependency_materialization_duration =
    timed (fun () -> materialize_cached_dependencies_for_execution t action ~sandbox_dir)
  in
  let* () = dependency_materialization_result in
  let* action_to_run = action_for_execution t action in
  let action_run =
    run_action
      ~package:package.name
      ~package_root
      ?c_compiler
      ocamlc
      sandbox_dir
      action_to_run
  in
  let timing: Action_execution.timing = {
    timing with
    cache_promotion = dependency_materialization_duration;
    sandbox_prepare = sandbox_prepare_duration;
    source_staging = action_run.source_staging_duration;
    command_execution = action_run.command_execution_duration;
  }
  in
  let* warnings =
    match action_run.result with
    | Riot_toolchain.Ocamlc.Success _ as result ->
        Ok (Riot_toolchain.Ocamlc.get_ocamlc_warnings result)
    | Riot_toolchain.Ocamlc.Failed _ as result ->
        Error (Error.ActionExecutionFailed {
          package = package.name;
          reason = Riot_toolchain.Ocamlc.get_output result;
        })
  in
  let abs_outputs = List.map (Action.outputs action.action) ~fn:(Path.join sandbox_dir) in
  let output_result, output_verification_duration =
    timed (fun () -> verify_outputs abs_outputs)
  in
  let timing: Action_execution.timing =
    { timing with output_verification = output_verification_duration }
  in
  let* () =
    output_result
    |> Result.map_err
      ~fn:(fun missing -> Error.ActionOutputsNotCreated { package = package.name; missing })
  in
  let save_result, store_save_duration =
    timed
      (fun () ->
        Riot_store.Store.save_action
          t.store
          ~package:(Riot_model.Package_name.to_string package.name)
          ~ocamlc_warnings:warnings
          ~input_hash:action_input_hash
          ~sandbox_dir
          ~outs:abs_outputs)
  in
  let timing: Action_execution.timing = { timing with store_save = store_save_duration } in
  match save_result with
  | Error error -> Error (store_error ~package:package.name (Riot_store.Store.error_message error))
  | Ok saved_artifact ->
      let timing = finish_timing started_at timing in
      store_result
        t
        {
          Action_execution.ref_ = action.ref_;
          action_kind = Action.kind action.action;
          status = Action_execution.Executed saved_artifact;
          ocamlc_warnings = warnings;
          timing;
        };
      Ok (Work_result.Complete [])

let promote_cached = fun
  t
  (action: Action_execution.t)
  (artifact: Riot_store.Artifact.t)
  ~started_at
  ~(timing: Action_execution.timing) ->
  let timing = finish_timing started_at timing in
  store_result
    t
    {
      Action_execution.ref_ = action.ref_;
      action_kind = Action.kind action.action;
      status = Action_execution.Cached artifact;
      ocamlc_warnings = artifact.ocamlc_warnings;
      timing;
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
        Error (Error.ExecutorInvariantViolated {
          message = "action execution for "
          ^ Riot_model.Package_name.to_string action.ref_.package
          ^ " started before planned action dependencies completed";
        })
      else
        let started_at = Time.Instant.now () in
        let dependency_hashes_result, dependency_hashing =
          timed (fun () -> dependency_output_hashes t action)
        in
        let* dependency_output_hashes = dependency_hashes_result in
        let action_input_hash, input_hashing =
          timed
            (fun () ->
              compute_action_input_hash ~planned_hash:action.ref_.hash ~dependency_output_hashes)
        in
        let artifact, store_lookup =
          timed (fun () -> Riot_store.Store.get_action t.store action_input_hash)
        in
        let timing: Action_execution.timing = {
          Action_execution.empty_timing with
          dependency_hashing;
          input_hashing;
          store_lookup;
        }
        in
        match artifact with
        | Some artifact -> promote_cached t action artifact ~started_at ~timing
        | None ->
            if Action.requires_toolchain action.action then
              match Toolchain_service.find t.toolchains action.ref_.target with
              | Some toolchain ->
                  execute_uncached t action (Some toolchain) action_input_hash ~started_at ~timing
              | None ->
                  Error (Error.ExecutorInvariantViolated {
                    message = "action execution for "
                    ^ Riot_model.Package_name.to_string action.ref_.package
                    ^ " started before toolchain readiness completed";
                  })
            else
              execute_uncached t action None action_input_hash ~started_at ~timing
