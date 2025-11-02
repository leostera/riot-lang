open Std
open Std.Collections
open Std.Time

open Tusk_planner
open Tusk_store
module G = Graph.SimpleGraph

type action_error = Action_queue.action_error =
  | ExecutionFailed of { message : string }
  | OutputsNotCreated of { missing : Path.t list }
  | DependenciesFailed of { failed : G.Node_id.t list }

type action_status = Action_queue.action_status =
  | Cached of Crypto.hash
  | Executed
  | Failed of action_error
  | Skipped

type execution_result = Action_queue.execution_result = {
  node_id : G.Node_id.t;
  status : action_status;
  duration : Duration.t;
  started_at : Instant.t;
  completed_at : Instant.t;
}

type t = { completed : (G.Node_id.t, execution_result) HashMap.t }

let make_flags_absolute sandbox_dir flags =
  List.map
    (fun flag ->
      match flag with
      | Tusk_toolchain.Ocamlc.Impl path ->
          Tusk_toolchain.Ocamlc.Impl (Path.join sandbox_dir path)
      | other -> other)
    flags

let run_action ocamlc sandbox_dir action =
  match action with
  | Action.CompileInterface { source; outputs = output :: _; includes; flags }
    ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_interface ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.CompileImplementation
      { source; outputs = output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_impl ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.GenerateInterface { source; outputs = output :: _; includes; flags }
    ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.generate_interface ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.CompileC { source; outputs = output :: _ } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in
      (* Include the directory containing the source file so headers can be found *)
      let source_dir =
        match Path.parent source with
        | Some dir -> [ Path.join sandbox_dir dir ]
        | None -> [ sandbox_dir ]
      in
      Tusk_toolchain.Ocamlc.compile_c ocamlc ~cwd:sandbox_dir
        ~includes:source_dir ~output:abs_output abs_source
  | Action.CreateLibrary { outputs = output :: _; objects; includes } ->
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in
      let abs_includes = List.map (Path.join sandbox_dir) includes in
      Tusk_toolchain.Ocamlc.create_library ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~output:abs_output abs_objects
  | Action.CreateExecutable
      { outputs = output :: _; objects; libraries; includes } -> (
      Log.debug
        "[ACTION_EXECUTOR] CreateExecutable: output=%s, %d objects, %d \
         libraries: [%s]"
        (Path.to_string output) (List.length objects) (List.length libraries)
        (String.concat ", " (List.map Path.to_string libraries));
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in

      (* Libraries are now found via -I includes pointing to cache, keep as filenames *)
      let abs_libraries = libraries in

      (* Includes can be absolute (cache dirs) or relative (sandbox dir) - make relative ones absolute *)
      let abs_includes =
        List.map
          (fun inc ->
            if Path.is_absolute inc then inc else Path.join sandbox_dir inc)
          includes
      in
      Log.debug "[ACTION_EXECUTOR] Absolute libraries: [%s]"
        (String.concat ", " (List.map Path.to_string abs_libraries));
      let result =
        Tusk_toolchain.Ocamlc.create_executable ocamlc ~cwd:sandbox_dir
          ~includes:abs_includes ~libs:abs_libraries ~output:abs_output
          abs_objects
      in
      match result with
      | Tusk_toolchain.Ocamlc.Success _ ->
          (* Make the executable file executable *)
          (match Fs.set_permissions abs_output (Fs.Permissions.of_mode 0o755) with
          | Ok () -> ()
          | Error err ->
              Log.warn "Failed to set executable permissions on %s: %s"
                (Path.to_string abs_output) (IO.error_message err));
          result
      | _ -> result)
  | Action.CompileInterface { outputs = []; _ }
  | Action.CompileImplementation { outputs = []; _ }
  | Action.GenerateInterface { outputs = []; _ }
  | Action.CompileC { outputs = []; _ }
  | Action.CreateLibrary { outputs = []; _ }
  | Action.CreateExecutable { outputs = []; _ } ->
      Tusk_toolchain.Ocamlc.Failed "Action has no outputs"
  | Action.CopyFile { source; destination } -> (
      let src_path = Path.join sandbox_dir source in
      let dst_path = Path.join sandbox_dir destination in
      match Fs.copy ~src:src_path ~dst:dst_path with
      | Ok () -> Tusk_toolchain.Ocamlc.Success "Copied"
      | Error _ ->
          Tusk_toolchain.Ocamlc.Failed
            (format "Copy failed: %s -> %s" (Path.to_string source)
               (Path.to_string destination)))
  | Action.WriteFile { destination; content } -> (
      let dest_path = Path.join sandbox_dir destination in
      Log.debug "WriteFile: writing to %s (%d bytes)" (Path.to_string dest_path)
        (String.length content);
      match Fs.write content dest_path with
      | Ok () ->
          Log.debug "WriteFile: success - file written to %s"
            (Path.to_string dest_path);
          Tusk_toolchain.Ocamlc.Success "Written"
      | Error err ->
          let msg = IO.error_message err in
          Log.error "WriteFile: failed - %s" msg;
          Tusk_toolchain.Ocamlc.Failed
            (format "Write failed: %s - %s" (Path.to_string destination) msg))

let execute_actions toolchain sandbox_dir actions =
  let ocamlc = Tusk_toolchain.ocamlc toolchain in
  let rec execute_next = function
    | [] -> Ok ()
    | action :: rest -> (
        let result = run_action ocamlc sandbox_dir action in
        match result with
        | Tusk_toolchain.Ocamlc.Success _ -> execute_next rest
        | Tusk_toolchain.Ocamlc.Failed err ->
            Error err
        )
  in
  execute_next actions

let verify_outputs outputs =
  let missing =
    List.filter
      (fun out ->
        match Fs.exists out with Ok true -> false | Ok false | Error _ -> true)
      outputs
  in
  if List.length missing > 0 then Error missing else Ok ()

let has_failed_dependencies completed (node : Action_node.t) =
  List.exists
    (fun dep_id ->
      match HashMap.get completed dep_id with
      | Some { status = Failed _ | Skipped; _ } -> true
      | _ -> false)
    node.deps

let execute_node ~completed toolchain sandbox_dir (node : Action_node.t) =
  let start = Instant.now () in

  if has_failed_dependencies completed node then (
    let now = Instant.now () in
    {
      node_id = node.id;
      status = Skipped;
      duration = Instant.duration_since ~earlier:start now;
      started_at = start;
      completed_at = now;
    })
  else (
    let actions = node.value.actions in
    let outputs = node.value.outs in
    let sources = node.value.srcs in

    Telemetry.emit
      Telemetry_events.(
        ActionStarted { package = node.value.package; action = node });

    let copy_result =
      List.fold_left
        (fun acc src_path ->
          match acc with
          | Error _ -> acc
          | Ok () ->
              let pkg_dir = node.value.package.path in
              let abs_src = Path.join pkg_dir src_path in
              let abs_dst = Path.join sandbox_dir src_path in
              (match Path.parent abs_dst with
              | Some dst_dir -> (
                  match Fs.create_dir_all dst_dir with Ok () | Error _ -> ())
              | None -> ());
              Fs.copy ~src:abs_src ~dst:abs_dst)
        (Ok ()) sources
    in

    let completed_at = Instant.now () in
    let duration = Instant.duration_since ~earlier:start completed_at in

    match copy_result with
    | Error err ->
        let msg = IO.error_message err in
        {
          node_id = node.id;
          status =
            Failed
              (ExecutionFailed
                 { message = format "Failed to copy sources: %s" msg });
          duration;
          started_at = start;
          completed_at;
        }
    | Ok () -> (
        match execute_actions toolchain sandbox_dir actions with
        | Error msg ->
            Telemetry.emit
              Telemetry_events.(
                ActionFailed
                  { package = node.value.package; action = node; error = msg });

            {
              node_id = node.id;
              status = Failed (ExecutionFailed { message = msg });
              duration;
              started_at = start;
              completed_at;
            }
        | Ok () -> (
            let abs_outputs = List.map (Path.join sandbox_dir) outputs in
            match verify_outputs abs_outputs with
            | Error missing ->
                {
                  node_id = node.id;
                  status = Failed (OutputsNotCreated { missing });
                  duration;
                  started_at = start;
                  completed_at;
                }
            | Ok () ->
                {
                  node_id = node.id;
                  status = Executed;
                  duration;
                  started_at = start;
                  completed_at;
                })))

let execute ~action_graph ~sandbox ~store:_ toolchain ~concurrency:_ =
  let sandbox_dir = Sandbox.get_dir sandbox in
  let sorted_nodes = Action_graph.nodes action_graph in
  let total_nodes = List.length sorted_nodes in

  Log.info "Starting action executor (sequential, no caching): total_nodes=%d"
    total_nodes;

  let completed = HashMap.create () in

  List.iter
    (fun node ->
      let result = execute_node ~completed toolchain sandbox_dir node in
      let _ = HashMap.insert completed node.id result in

      let status_str =
        match result.status with
        | Cached _ -> "cached"
        | Executed -> "executed"
        | Failed _ -> "failed"
        | Skipped -> "skipped"
      in
      Log.info "Action node %s completed: %s (%dms)"
        (G.Node_id.to_string result.node_id)
        status_str
        (Duration.to_millis result.duration);

      match result.status with
      | Failed (ExecutionFailed { message }) ->
          Log.error "Action failed: %s" message
      | Failed (OutputsNotCreated { missing }) ->
          Log.error "Expected outputs not created: %s"
            (String.concat ", " (List.map Path.to_string missing))
      | Failed (DependenciesFailed { failed }) ->
          Log.error "Action dependencies failed: %s"
            (String.concat ", " (List.map G.Node_id.to_string failed))
      | Skipped -> Log.warn "Action skipped due to failed dependencies"
      | _ -> ())
    sorted_nodes;

  let succeeded =
    completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter ~fn:(fun (_, result) ->
           match result.status with Cached _ | Executed -> true | _ -> false)
    |> Iter.Iterator.count
  in
  let failed =
    completed
    |> HashMap.into_iter
    |> Iter.Iterator.filter ~fn:(fun (_, result) ->
           match result.status with Failed _ | Skipped -> true | _ -> false)
    |> Iter.Iterator.count
  in

  Log.info
    "Action executor: all done, completed=%d succeeded=%d failed=%d total=%d"
    (HashMap.len completed) succeeded failed total_nodes;

  { completed }
