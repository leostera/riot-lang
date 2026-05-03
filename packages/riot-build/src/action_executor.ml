open Std
open Std.Collections
open Std.Time
open Riot_planner
open Riot_store

module G = Graph.SimpleGraph

type action_error =
  | ExecutionFailed of { message: string }
  | OutputsNotCreated of {
      missing: Path.t list;
    }
  | DependenciesFailed of {
      failed: G.Node_id.t list;
    }

type action_status =
  | Cached of Riot_store.Artifact.t
  | Executed of Riot_store.Artifact.t
  | Failed of action_error
  | Skipped

type execution_result = {
  node_id: G.Node_id.t;
  status: action_status;
  ocamlc_warnings: string list;
  duration: Duration.t;
  started_at: Instant.t;
  completed_at: Instant.t;
}

let make_flags_absolute = fun sandbox_dir flags ->
  List.map
    flags
    ~fn:(fun flag ->
      match flag with
      | Riot_toolchain.Ocamlc.Impl path -> Riot_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)

let resolve_include_paths = fun sandbox_dir includes ->
  List.map
    includes
    ~fn:(fun inc ->
      let inc_str = Path.to_string inc in
      if Path.is_absolute inc then
        inc
      else if String.starts_with ~prefix:"+" inc_str then
        inc
      else
        Path.join sandbox_dir inc)

let emit_action_command = fun ~session_id ~package ~node command ->
  Telemetry.emit
    (
      Telemetry_events.ActionCommandStarted {
        session_id;
        package;
        action = node;
        command;
      }
    )

let ocamlc_success = fun message -> Riot_toolchain.Ocamlc.Success { message; diagnostics = [] }

let ocamlc_failed = fun message -> Riot_toolchain.Ocamlc.Failed { message; diagnostics = [] }

let run_ocamlc_invocation = fun ~session_id ~package ~node ~sandbox_dir invocation ->
  emit_action_command ~session_id ~package ~node (Riot_toolchain.Ocamlc.to_string invocation);
  Riot_toolchain.Ocamlc.run invocation
  |> Diagnostic_rewrite.rewrite_ocamlc_result ~package ~sandbox_dir

let run_action = fun ~session_id ~package ~node ?c_compiler ocamlc sandbox_dir action ->
  match action with
  | Action.CompileInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation =
        Riot_toolchain.Ocamlc.compile_interface
          ocamlc
          ~cwd:sandbox_dir
          ~includes:abs_includes
          ~flags:abs_flags
          ~output:abs_output
          abs_source
      in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CompileImplementation {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation =
        Riot_toolchain.Ocamlc.compile_impl
          ocamlc
          ~cwd:sandbox_dir
          ~includes:abs_includes
          ~flags:abs_flags
          ~output:abs_output
          abs_source
      in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.GenerateInterface {
      source;
      outputs = output :: _;
      includes;
      flags;
    } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      let invocation =
        Riot_toolchain.Ocamlc.generate_interface
          ocamlc
          ~cwd:sandbox_dir
          ~includes:abs_includes
          ~flags:abs_flags
          ~output:abs_output
          abs_source
      in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CompileC { source; outputs = output :: _; ccflags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let source_dir =
        match Path.parent source with
        | Some dir -> [ Path.join sandbox_dir dir ]
        | None -> [ sandbox_dir ]
      in
      Log.debug ("[ACTION_EXECUTOR] CompileC ccflags: " ^ String.concat " " ccflags);
      let invocation =
        Riot_toolchain.Ocamlc.compile_c
          ocamlc
          ~cwd:sandbox_dir
          ~includes:source_dir
          ?cc:c_compiler
          ~ccflags
          ~output:abs_output
          abs_source
      in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CreateLibrary { outputs = output :: _; objects; includes } ->
      let abs_output = Path.join sandbox_dir output in
      let rel_objects = objects in
      let abs_includes = resolve_include_paths sandbox_dir includes in
      let invocation =
        Riot_toolchain.Ocamlc.create_library
          ocamlc
          ~cwd:sandbox_dir
          ~includes:abs_includes
          ~output:abs_output
          rel_objects
      in
      run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
  | Action.CreateExecutable {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      (
          Log.debug
            ("[ACTION_EXECUTOR] CreateExecutable: output="
            ^ Path.to_string output
            ^ ", "
            ^ Int.to_string (List.length objects)
            ^ " objects, "
            ^ Int.to_string (List.length libraries)
            ^ " libraries: ["
            ^ String.concat ", " (List.map libraries ~fn:Path.to_string)
            ^ "], cclibs: ["
            ^ String.concat ", " (List.map cclibs ~fn:Path.to_string)
            ^ "], ccopt_flags: ["
            ^ String.concat ", " ccopt_flags
            ^ "], cclib_flags: ["
            ^ String.concat ", " cclib_flags
            ^ "]");
          let abs_output = Path.join sandbox_dir output in
          let abs_objects = List.map objects ~fn:(Path.join sandbox_dir) in
          let abs_libraries = libraries in
          let abs_includes = resolve_include_paths sandbox_dir includes in
          let abs_cclibs = cclibs in
          let invocation =
            Riot_toolchain.Ocamlc.create_executable
              ocamlc
              ~cwd:sandbox_dir
              ~includes:abs_includes
              ~libs:abs_libraries
              ?cc:c_compiler
              ~cclibs:abs_cclibs
              ~ccopt_flags
              ~cclib_flags
              ~output:abs_output
              abs_objects
          in
          let result = run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation in
          match result with
          | Riot_toolchain.Ocamlc.Success _ -> (
              match Fs.set_permissions abs_output (Fs.Permissions.of_mode 0o755) with
              | Ok () -> result
              | Error err ->
                  Log.warn
                    ("Failed to set executable permissions on "
                    ^ Path.to_string abs_output
                    ^ ": "
                    ^ IO.error_message err);
                  result
            )
          | _ -> result
        )
  | Action.CreateSharedLibrary {
      outputs = output :: _;
      objects;
      libraries;
      includes;
      cclibs;
      ccopt_flags;
      cclib_flags;
    } ->
      (
          Log.debug
            ("[ACTION_EXECUTOR] CreateSharedLibrary: output="
            ^ Path.to_string output
            ^ ", "
            ^ Int.to_string (List.length objects)
            ^ " objects, "
            ^ Int.to_string (List.length libraries)
            ^ " libraries: ["
            ^ String.concat ", " (List.map libraries ~fn:Path.to_string)
            ^ "], cclibs: ["
            ^ String.concat ", " (List.map cclibs ~fn:Path.to_string)
            ^ "], ccopt_flags: ["
            ^ String.concat ", " ccopt_flags
            ^ "], cclib_flags: ["
            ^ String.concat ", " cclib_flags
            ^ "]");
          let abs_output = Path.join sandbox_dir output in
          let abs_objects = List.map objects ~fn:(Path.join sandbox_dir) in
          let abs_libraries = libraries in
          let abs_includes = resolve_include_paths sandbox_dir includes in
          let abs_cclibs = cclibs in
          let invocation =
            Riot_toolchain.Ocamlc.create_shared_library
              ocamlc
              ~cwd:sandbox_dir
              ~includes:abs_includes
              ~libs:abs_libraries
              ?cc:c_compiler
              ~cclibs:abs_cclibs
              ~ccopt_flags
              ~cclib_flags
              ~output:abs_output
              abs_objects
          in
          run_ocamlc_invocation ~session_id ~package ~node ~sandbox_dir invocation
        )
  | Action.CompileInterface { outputs = []; _ }
  | Action.CompileImplementation { outputs = []; _ }
  | Action.GenerateInterface { outputs = []; _ }
  | Action.CompileC { outputs = []; _ }
  | Action.CreateLibrary { outputs = []; _ }
  | Action.CreateExecutable { outputs = []; _ }
  | Action.CreateSharedLibrary { outputs = []; _ } -> ocamlc_failed "Action has no outputs"
  | Action.CopyFile { source; destination } -> (
      let src_path =
        if Path.is_absolute source then
          source
        else
          Path.join sandbox_dir source
      in
      let dst_path = Path.join sandbox_dir destination in
      match Fs.copy ~src:src_path ~dst:dst_path with
      | Ok () -> ocamlc_success "Copied"
      | Error _ ->
          ocamlc_failed
            ("Copy failed: " ^ Path.to_string source ^ " -> " ^ Path.to_string destination)
    )
  | Action.WriteFile { destination; content } -> (
      let dest_path = Path.join sandbox_dir destination in
      Log.debug
        ("WriteFile: writing to "
        ^ Path.to_string dest_path
        ^ " ("
        ^ Int.to_string (String.length content)
        ^ " bytes)");
      match Fs.write content dest_path with
      | Ok () ->
          Log.debug ("WriteFile: success - file written to " ^ Path.to_string dest_path);
          ocamlc_success "Written"
      | Error err ->
          let msg = IO.error_message err in
          Log.error ("WriteFile: failed - " ^ msg);
          ocamlc_failed ("Write failed: " ^ Path.to_string destination ^ " - " ^ msg)
    )
  | Action.BuildForeignDependency {
      name;
      path;
      build_cmd;
      outputs;
      env;
    } ->
      (
          let build_cmd_str = String.concat " " build_cmd in
          Log.info ("   \027[1;32mCompiling\027[0m " ^ name ^ " (" ^ build_cmd_str ^ ")");
          match build_cmd with
          | [] -> ocamlc_failed "BuildForeignDependency: empty build_cmd"
          | cmd_name :: cmd_args ->
              let normalized_path = Path.normalize path in
              let cmd =
                Command.make ~cwd:(Path.to_string normalized_path) ~env ~args:cmd_args cmd_name
              in
              let cmd_str = Command.to_string cmd in
              emit_action_command ~session_id ~package ~node cmd_str;
              Log.debug ("Executing: " ^ cmd_str);
              match Command.output cmd with
              | Ok output when output.Command.status = 0 ->
                  Log.debug ("Foreign build succeeded: " ^ name);
                  if String.length output.Command.stdout > 0 then
                    Log.debug ("stdout: " ^ output.Command.stdout);
                  let abs_outputs =
                    List.map outputs ~fn:(fun out -> Path.normalize (Path.join path out))
                  in
                  let missing =
                    List.filter
                      abs_outputs
                      ~fn:(fun out ->
                        match Fs.exists out with
                        | Ok true -> false
                        | Ok false
                        | Error _ -> true)
                  in
                  if List.length missing > 0 then
                    ocamlc_failed
                      ("Foreign build succeeded but outputs not created: "
                      ^ String.concat ", " (List.map missing ~fn:Path.to_string))
                  else
                    ocamlc_success ("Built foreign dependency: " ^ name)
              | Ok output ->
                  Log.error
                    ("Foreign build failed: "
                    ^ name
                    ^ " - exit code "
                    ^ Int.to_string output.Command.status);
                  if String.length output.Command.stderr > 0 then
                    Log.error ("stderr: " ^ output.Command.stderr);
                  ocamlc_failed
                    ("Foreign build failed: "
                    ^ name
                    ^ " - exit code "
                    ^ Int.to_string output.Command.status)
              | Error (Command.SystemError msg) ->
                  Log.error ("Failed to execute foreign build: " ^ msg);
                  ocamlc_failed ("Failed to execute foreign build command: " ^ msg)
        )

let execute_actions = fun ~session_id ~(node:Action_node.t) toolchain sandbox_dir actions ->
  let ocamlc = Riot_toolchain.ocamlc toolchain in
  let c_compiler = Riot_toolchain.c_compiler toolchain in
  let package = node.value.package in
  let rec execute_next ocamlc_warnings = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ocamlc_warnings
    | action :: rest -> (
        let result = run_action ~session_id ~package ~node ?c_compiler ocamlc sandbox_dir action in
        match result with
        | Riot_toolchain.Ocamlc.Success _ ->
            let action_warnings = Riot_toolchain.Ocamlc.get_ocamlc_warnings result in
            execute_next (ocamlc_warnings @ action_warnings) rest
        | Riot_toolchain.Ocamlc.Failed _ -> Error (Riot_toolchain.Ocamlc.get_output result)
      )
  in
  execute_next [] actions

let verify_outputs = fun outputs ->
  let missing =
    List.filter
      outputs
      ~fn:(fun out ->
        match Fs.exists out with
        | Ok true -> false
        | Ok false
        | Error _ -> true)
  in
  if List.length missing > 0 then
    Error missing
  else
    Ok ()

let save_action_artifact = fun ~store ~package ~input_hash ~ocamlc_warnings ~sandbox_dir ~outputs ->
  Riot_store.Store.save store ~package ~ocamlc_warnings ~input_hash ~sandbox_dir ~outs:outputs
  |> Result.map_err ~fn:Riot_store.Store.error_message

let successful_artifact = fun (result: execution_result) ->
  match result.status with
  | Cached artifact
  | Executed artifact -> Some artifact
  | Failed _
  | Skipped -> None

let compute_action_input_hash = fun ~planned_hash ~dependency_output_hashes ->
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write hasher "riot-action-input:v1";
  Crypto.Sha256.write_hash hasher planned_hash;
  List.for_each dependency_output_hashes ~fn:(Crypto.Sha256.write_hash hasher);
  Crypto.Sha256.finish hasher

let dependency_output_hashes = fun completed (node: Action_node.t) ->
  let sorted_deps =
    List.sort
      node.deps
      ~compare:(fun left right -> Int.compare (G.Node_id.to_int left) (G.Node_id.to_int right))
  in
  (* Action graphs can carry ordering dependencies that do not materialize an
     artifact in the action scheduler. The planned action hash still covers the
     graph shape; completed producer artifacts strengthen the runtime cache key
     when they are available. Package dependencies are mandatory and flow
     through the package input hash.
  *)
  List.filter_map
    sorted_deps
    ~fn:(fun dep_id ->
      match HashMap.get completed ~key:dep_id with
      | Some result ->
          Option.map
            (successful_artifact result)
            ~fn:(fun artifact -> artifact.Riot_store.Artifact.output_hash)
      | None -> None)

let has_failed_dependencies = fun completed (node: Action_node.t) ->
  List.any
    node.deps
    ~fn:(fun dep_id ->
      match HashMap.get completed ~key:dep_id with
      | Some { status = Failed _
        | Skipped; _ } ->
          true
      | _ -> false)

let resolve_source_for_copy = fun ~(package:Riot_model.Package.t) ~src_path ->
  let pkg_dir = package.path in
  let workspace_root_candidate =
    let pkg_path_str = Path.to_string package.path in
    let rel_path_str = Path.to_string package.relative_path in
    if String.length rel_path_str > 0 && String.ends_with ~suffix:rel_path_str pkg_path_str then
      let root_len = String.length pkg_path_str - String.length rel_path_str in
      let raw_root = String.sub pkg_path_str ~offset:0 ~len:root_len in
      let normalized_root =
        if String.ends_with ~suffix:"/" raw_root then
          String.sub raw_root ~offset:0 ~len:(String.length raw_root - 1)
        else
          raw_root
      in
      if String.length normalized_root = 0 then
        Some (Path.v ".")
      else
        Some (Path.v normalized_root)
    else
      None
  in
  let candidates =
    if Path.is_absolute src_path then
      [ src_path ]
    else
      let base = [ Path.join pkg_dir src_path ] in
      let with_workspace =
        match workspace_root_candidate with
        | Some root -> base @ [ Path.join root src_path ]
        | None -> base
      in
      with_workspace @ [ src_path ]
  in
  let rec first_existing = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | p :: rest -> (
        match Fs.exists p with
        | Ok true -> Some p
        | Ok false
        | Error _ -> first_existing rest
      )
  in
  match first_existing candidates with
  | Some p -> Ok p
  | None ->
      Error ("source not found for "
      ^ Path.to_string src_path
      ^ " (checked "
      ^ String.concat ", " (List.map candidates ~fn:Path.to_string)
      ^ ")")

let execute_node = fun ~completed ~store ~session_id toolchain sandbox_dir (node: Action_node.t) ->
  let start = Instant.now () in
  if has_failed_dependencies completed node then (
    let now = Instant.now () in
    {
      node_id = node.id;
      status = Skipped;
      ocamlc_warnings = [];
      duration = Instant.duration_since ~earlier:start now;
      started_at = start;
      completed_at = now;
    }
  ) else (
    let planned_hash = Action_node.get_hash node in
    let action_input_hash =
      compute_action_input_hash
        ~planned_hash
        ~dependency_output_hashes:(dependency_output_hashes completed node)
    in
    (
      match Riot_store.Store.get store action_input_hash with
      | Some artifact ->
          Telemetry.emit
            (
              Telemetry_events.CacheHit {
                session_id;
                package = node.value.package;
                action = node;
                hash = action_input_hash;
              }
            );
          let _ =
            Riot_store.Store.promote
              store
              artifact.Riot_store.Artifact.input_hash
              ~target_dir:sandbox_dir
            |> Result.expect
              ~msg:("Failed to materialize cached action artifact: " ^ G.Node_id.to_string node.id)
          in
          let completed_at = Instant.now () in
          let duration = Instant.duration_since ~earlier:start completed_at in
          Telemetry.emit
            (
              Telemetry_events.ActionCompleted {
                session_id;
                package = node.value.package;
                action = node;
                artifact;
                status = `Cached;
                duration;
              }
            );
          {
            node_id = node.id;
            status = Cached artifact;
            ocamlc_warnings = artifact.Riot_store.Artifact.ocamlc_warnings;
            duration;
            started_at = start;
            completed_at;
          }
      | None ->
          Telemetry.emit
            (
              Telemetry_events.CacheMiss {
                session_id;
                package = node.value.package;
                action = node;
                hash = action_input_hash;
              }
            );
          let actions = node.value.actions in
          let outputs = node.value.outs in
          let sources = node.value.srcs in
          Telemetry.emit
            (Telemetry_events.ActionStarted {
              session_id;
              package = node.value.package;
              action = node;
            });
          let copy_result: (unit, string) Result.t =
            List.fold_left
              sources
              ~init:(Ok ())
              ~fn:(fun acc src_path ->
                match acc with
                | Error _ -> acc
                | Ok () ->
                    match resolve_source_for_copy ~package:node.value.package ~src_path with
                    | Error msg -> Error msg
                    | Ok abs_src ->
                        let abs_dst = Path.join sandbox_dir src_path in
                        Log.debug
                          ("[EXECUTOR] Copying source: "
                          ^ Path.to_string src_path
                          ^ " from "
                          ^ Path.to_string abs_src);
                        (
                          match Path.parent abs_dst with
                          | Some dst_dir -> (
                              match Fs.create_dir_all dst_dir with
                              | Ok ()
                              | Error _ -> ()
                            )
                          | None -> ()
                        );
                        (
                          match Fs.copy ~src:abs_src ~dst:abs_dst with
                          | Ok () -> Ok ()
                          | Error err -> Error (IO.error_message err)
                        ))
          in
          let completed_at = Instant.now () in
          let duration = Instant.duration_since ~earlier:start completed_at in
          match copy_result with
          | Error msg ->
              {
                node_id = node.id;
                status = Failed (ExecutionFailed { message = "Failed to copy sources: " ^ msg });
                ocamlc_warnings = [];
                duration;
                started_at = start;
                completed_at;
              }
          | Ok () -> (
              match execute_actions ~session_id ~node toolchain sandbox_dir actions with
              | Error msg ->
                  Telemetry.emit
                    (
                      Telemetry_events.ActionFailed {
                        session_id;
                        package = node.value.package;
                        action = node;
                        error = msg;
                      }
                    );
                  {
                    node_id = node.id;
                    status = Failed (ExecutionFailed { message = msg });
                    ocamlc_warnings = [];
                    duration;
                    started_at = start;
                    completed_at;
                  }
              | Ok ocamlc_warnings ->
                  let needs_output_verification =
                    List.any
                      actions
                      ~fn:(fun __tmp1 ->
                        match __tmp1 with
                        | Action.BuildForeignDependency _ -> false
                        | _ -> true)
                  in
                  if not needs_output_verification then
                    match save_action_artifact
                      ~store
                      ~package:(Riot_model.Package_name.to_string node.value.package.name)
                      ~input_hash:action_input_hash
                      ~ocamlc_warnings
                      ~sandbox_dir
                      ~outputs:(List.map outputs ~fn:(Path.join sandbox_dir)) with
                    | Error message ->
                        {
                          node_id = node.id;
                          status = Failed (ExecutionFailed { message });
                          ocamlc_warnings = [];
                          duration;
                          started_at = start;
                          completed_at;
                        }
                    | Ok artifact ->
                        Telemetry.emit
                          (
                            Telemetry_events.ActionCompleted {
                              session_id;
                              package = node.value.package;
                              action = node;
                              artifact;
                              status = `Fresh;
                              duration;
                            }
                          );
                        {
                          node_id = node.id;
                          status = Executed artifact;
                          ocamlc_warnings;
                          duration;
                          started_at = start;
                          completed_at;
                        }
                  else
                    let abs_outputs = List.map outputs ~fn:(Path.join sandbox_dir) in
                    match verify_outputs abs_outputs with
                    | Error missing ->
                        {
                          node_id = node.id;
                          status = Failed (OutputsNotCreated { missing });
                          ocamlc_warnings = [];
                          duration;
                          started_at = start;
                          completed_at;
                        }
                    | Ok () ->
                        match save_action_artifact
                          ~store
                          ~package:(Riot_model.Package_name.to_string node.value.package.name)
                          ~input_hash:action_input_hash
                          ~ocamlc_warnings
                          ~sandbox_dir
                          ~outputs:abs_outputs with
                        | Error message ->
                            {
                              node_id = node.id;
                              status = Failed (ExecutionFailed { message });
                              ocamlc_warnings = [];
                              duration;
                              started_at = start;
                              completed_at;
                            }
                        | Ok artifact ->
                            Telemetry.emit
                              (
                                Telemetry_events.ActionCompleted {
                                  session_id;
                                  package = node.value.package;
                                  action = node;
                                  artifact;
                                  status = `Fresh;
                                  duration;
                                }
                              );
                            {
                              node_id = node.id;
                              status = Executed artifact;
                              ocamlc_warnings;
                              duration;
                              started_at = start;
                              completed_at;
                            }
            )
    )
  )
