open Std
open Std.Collections
open Std.Time
open Std.Type

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
type Message.t += ActionCompleted of execution_result

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

      (* Includes can be absolute (cache dirs), relative (sandbox dir), or special (+unix) *)
      let abs_includes =
        List.map
          (fun inc ->
            let inc_str = Path.to_string inc in
            if Path.is_absolute inc then inc
            else if String.starts_with ~prefix:"+" inc_str then inc
              (* Keep +unix, +threads as-is *)
            else Path.join sandbox_dir inc)
          includes
      in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_interface ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.CompileImplementation
      { source; outputs = output :: _; includes; flags } ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in

      (* Includes can be absolute (cache dirs), relative (sandbox dir), or special (+unix) *)
      let abs_includes =
        List.map
          (fun inc ->
            let inc_str = Path.to_string inc in
            if Path.is_absolute inc then inc
            else if String.starts_with ~prefix:"+" inc_str then inc
            else Path.join sandbox_dir inc)
          includes
      in
      let abs_flags = make_flags_absolute sandbox_dir flags in
      Tusk_toolchain.Ocamlc.compile_impl ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~flags:abs_flags ~output:abs_output abs_source
  | Action.GenerateInterface { source; outputs = output :: _; includes; flags }
    ->
      let abs_source = Path.join sandbox_dir source in
      let abs_output = Path.join sandbox_dir output in

      (* Includes can be absolute (cache dirs), relative (sandbox dir), or special (+unix) *)
      let abs_includes =
        List.map
          (fun inc ->
            let inc_str = Path.to_string inc in
            if Path.is_absolute inc then inc
            else if String.starts_with ~prefix:"+" inc_str then inc
            else Path.join sandbox_dir inc)
          includes
      in
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
      (* Keep objects as relative paths (basenames) since they're in cwd (sandbox_dir).
         This ensures the .cmxa doesn't bake in absolute paths to .o files. *)
      let rel_objects = objects in

      (* Includes can be absolute (cache dirs), relative (sandbox dir), or special (+unix) *)
      let abs_includes =
        List.map
          (fun inc ->
            let inc_str = Path.to_string inc in
            if Path.is_absolute inc then inc
            else if String.starts_with ~prefix:"+" inc_str then inc
            else Path.join sandbox_dir inc)
          includes
      in
      Tusk_toolchain.Ocamlc.create_library ocamlc ~cwd:sandbox_dir
        ~includes:abs_includes ~output:abs_output rel_objects
  | Action.CreateExecutable
      { outputs = output :: _; objects; libraries; includes; cclibs; ccflags } -> (
      Log.debug
        ("[ACTION_EXECUTOR] CreateExecutable: output="
        ^ Path.to_string output ^ ", "
        ^ Int.to_string (List.length objects)
        ^ " objects, "
        ^ Int.to_string (List.length libraries)
        ^ " libraries: ["
        ^ String.concat ", " (List.map Path.to_string libraries)
        ^ "], cclibs: ["
        ^ String.concat ", " (List.map Path.to_string cclibs)
        ^ "], ccflags: ["
        ^ String.concat ", " ccflags
        ^ "]");
      let abs_output = Path.join sandbox_dir output in
      let abs_objects = List.map (Path.join sandbox_dir) objects in

      (* Libraries are now found via -I includes pointing to cache, keep as filenames *)
      let abs_libraries = libraries in

      (* Includes can be absolute (cache dirs), relative (sandbox dir), or special (+unix) *)
      let abs_includes =
        List.map
          (fun inc ->
            let inc_str = Path.to_string inc in
            if Path.is_absolute inc then inc
            else if String.starts_with ~prefix:"+" inc_str then inc
            else Path.join sandbox_dir inc)
          includes
      in
      
      (* Foreign cclibs are absolute paths, keep them as-is *)
      let abs_cclibs = cclibs in

      Log.debug
        ("[ACTION_EXECUTOR] Absolute libraries: ["
        ^ String.concat ", " (List.map Path.to_string abs_libraries)
        ^ "]");
      let result =
        Tusk_toolchain.Ocamlc.create_executable ocamlc ~cwd:sandbox_dir
          ~includes:abs_includes ~libs:abs_libraries ~cclibs:abs_cclibs ~ccflags
          ~output:abs_output abs_objects
      in
      match result with
      | Tusk_toolchain.Ocamlc.Success _ ->
          (* Make the executable file executable *)
          (match Fs.set_permissions abs_output (Fs.Permissions.of_mode 0o755) with
          | Ok () -> result
          | Error err ->
              Log.warn
                ("Failed to set executable permissions on "
                ^ Path.to_string abs_output ^ ": " ^ IO.error_message err);
              result)
      | _ -> result)
  | Action.CompileInterface { outputs = []; _ }
  | Action.CompileImplementation { outputs = []; _ }
  | Action.GenerateInterface { outputs = []; _ }
  | Action.CompileC { outputs = []; _ }
  | Action.CreateLibrary { outputs = []; _ }
  | Action.CreateExecutable { outputs = []; _ } ->
      Tusk_toolchain.Ocamlc.Failed "Action has no outputs"
  | Action.CopyFile { source; destination } -> (
      (* Source can be absolute (from cache) or relative (from sandbox) *)
      let src_path =
        if Path.is_absolute source then source else Path.join sandbox_dir source
      in
      let dst_path = Path.join sandbox_dir destination in
      match Fs.copy ~src:src_path ~dst:dst_path with
      | Ok () -> Tusk_toolchain.Ocamlc.Success "Copied"
      | Error _ ->
          Tusk_toolchain.Ocamlc.Failed
            ("Copy failed: " ^ Path.to_string source ^ " -> " ^
               (Path.to_string destination)))
  | Action.WriteFile { destination; content } -> (
      let dest_path = Path.join sandbox_dir destination in
      Log.debug
        ("WriteFile: writing to " ^ Path.to_string dest_path ^ " ("
        ^ Int.to_string (String.length content)
        ^ " bytes)");
      match Fs.write content dest_path with
      | Ok () ->
          Log.debug
            ("WriteFile: success - file written to " ^ Path.to_string dest_path);
          Tusk_toolchain.Ocamlc.Success "Written"
      | Error err ->
          let msg = IO.error_message err in
          Log.error ("WriteFile: failed - " ^ msg);
          Tusk_toolchain.Ocamlc.Failed
            ("Write failed: " ^ Path.to_string destination ^ " - " ^ msg))
  | Action.BuildForeignDependency { name; path; build_cmd; outputs; env } -> (
      (* Execute foreign build command in the foreign package directory *)
      let build_cmd_str = String.concat " " build_cmd in
      (* Print a Cargo-style "Compiling" message for foreign builds *)
      Log.info ("   \027[1;32mCompiling\027[0m " ^ name ^ " (" ^ build_cmd_str ^ ")");
      
      match build_cmd with
      | [] -> Tusk_toolchain.Ocamlc.Failed "BuildForeignDependency: empty build_cmd"
      | cmd_name :: cmd_args ->
          let normalized_path = Path.normalize path in
          Log.debug ("Executing: " ^ build_cmd_str ^ " in " ^ Path.to_string normalized_path);
          
          (* Execute the build command in the foreign directory *)
          let cmd = Command.make ~cwd:(Path.to_string normalized_path) ~env ~args:cmd_args cmd_name in
          match Command.output cmd with
          | Ok output when output.Command.status = 0 ->
              Log.debug ("Foreign build succeeded: " ^ name);
              if String.length output.Command.stdout > 0 then 
                Log.debug ("stdout: " ^ output.Command.stdout);
              
              (* Verify that all expected outputs were created *)
              let abs_outputs = List.map (fun out -> Path.normalize (Path.join path out)) outputs in
              let missing = List.filter (fun out ->
                match Fs.exists out with
                | Ok true -> false
                | Ok false | Error _ -> true
              ) abs_outputs in
              
              if List.length missing > 0 then
                Tusk_toolchain.Ocamlc.Failed 
                  ("Foreign build succeeded but outputs not created: " ^ 
                   String.concat ", " (List.map Path.to_string missing))
              else
                Tusk_toolchain.Ocamlc.Success ("Built foreign dependency: " ^ name)
          | Ok output ->
              Log.error ("Foreign build failed: " ^ name ^ " - exit code " ^ 
                        Int.to_string output.Command.status);
              if String.length output.Command.stderr > 0 then 
                Log.error ("stderr: " ^ output.Command.stderr);
              Tusk_toolchain.Ocamlc.Failed 
                ("Foreign build failed: " ^ name ^ " - exit code " ^ 
                 Int.to_string output.Command.status)
          | Error (Command.SystemError msg) ->
              Log.error ("Failed to execute foreign build: " ^ msg);
              Tusk_toolchain.Ocamlc.Failed 
                ("Failed to execute foreign build command: " ^ msg)
      )

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
              Log.debug ("[EXECUTOR] Copying source: " ^ Path.to_string src_path ^ " from " ^ Path.to_string abs_src);
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
                 { message = "Failed to copy sources: " ^ msg });
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
            (* Foreign dependencies verify their own outputs, so skip verification for those *)
            let needs_output_verification = 
              List.exists (fun action ->
                match action with
                | Action.BuildForeignDependency _ -> false
                | _ -> true
              ) actions
            in
            
            if not needs_output_verification then
              (* All actions are foreign dependencies, skip output verification *)
              {
                node_id = node.id;
                status = Executed;
                duration;
                started_at = start;
                completed_at;
              }
            else (
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
                  }))))

let execute ~action_graph ~sandbox ~store:_ toolchain ~concurrency =
  let sandbox_dir = Sandbox.get_dir sandbox in
  let sorted_nodes = Action_graph.nodes action_graph in
  let total_nodes = List.length sorted_nodes in

  Log.info
    ("Starting parallel action executor: total_nodes="
    ^ Int.to_string total_nodes ^ " concurrency=" ^ Int.to_string concurrency);

  let queue = Action_queue.create () in

  (* Queue all action nodes BEFORE starting pool *)
  List.iter (fun node -> Action_queue.queue queue node) sorted_nodes;

  let pool : Action_node.t WorkerPool.DynamicWorkerPool.t =
    WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:(self ())
      ~worker_fn:(fun ~owner ~task ->
        let (node : Action_node.t) = task in
        let result =
          execute_node ~completed:queue.completed toolchain sandbox_dir node
        in
        send owner (ActionCompleted result))
      ()
  in

  let task_ref = pool.task_ref in

  let selector msg =
    match msg with
    | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
        let worker_ref =
          WorkerPool.DynamicWorkerPool.get_worker_task_ref worker
        in
        match Ref.type_equal task_ref worker_ref with
        | Some Type.Equal ->
            `select
              (`WorkerReady
                 (worker : Action_node.t WorkerPool.DynamicWorkerPool.worker))
        | None -> `skip)
    | ActionCompleted result -> `select (`ActionCompleted result)
    | _ -> `skip
  in

  let rec dispatch_loop () =
    if Action_queue.is_complete queue ~total_nodes then (
      let _, _, _, completed, succeeded, failed = Action_queue.stats queue in
      Log.info
        ("Action executor: all done, completed="
        ^ Int.to_string completed
        ^ " succeeded=" ^ Int.to_string succeeded ^ " failed="
        ^ Int.to_string failed ^ " total=" ^ Int.to_string total_nodes);
      ())
    else
      match receive ~selector () with
      | `WorkerReady worker -> (
          match Action_queue.next queue with
          | None ->
              let ready, waiting, busy, _, _, _ = Action_queue.stats queue in
              Log.debug
                ("No work available for worker (ready="
                ^ Int.to_string ready ^ " waiting=" ^ Int.to_string waiting
                ^ " busy=" ^ Int.to_string busy ^ ")");
              dispatch_loop ()
          | Some node ->
              Log.debug
                ("Dispatching action node " ^ G.Node_id.to_string node.id
                ^ " to worker");
              WorkerPool.DynamicWorkerPool.send_task pool worker node;
              dispatch_loop ())
      | `ActionCompleted result ->
          Action_queue.mark_completed queue result;

          let status_str =
            match result.status with
            | Cached _ -> "cached"
            | Executed -> "executed"
            | Failed _ -> "failed"
            | Skipped -> "skipped"
          in

          Log.info
            ("Action node " ^ G.Node_id.to_string result.node_id
            ^ " completed: " ^ status_str ^ " ("
            ^ Int.to_string (Duration.to_millis result.duration)
            ^ "ms)");

          (match result.status with
          | Failed (ExecutionFailed { message }) ->
              Log.error ("Action failed: " ^ message)
          | Failed (OutputsNotCreated { missing }) ->
              Log.error
                ("Expected outputs not created: "
                ^ String.concat ", " (List.map Path.to_string missing))
          | Failed (DependenciesFailed { failed }) ->
              Log.error
                ("Action dependencies failed: "
                ^ String.concat ", " (List.map G.Node_id.to_string failed))
          | Skipped -> Log.warn "Action skipped due to failed dependencies"
          | _ -> ());

          dispatch_loop ()
  in

  dispatch_loop ();

  { completed = queue.completed }
