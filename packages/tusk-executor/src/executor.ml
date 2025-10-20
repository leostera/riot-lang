open Std
open Std.Collections
open Miniriot
open Tusk_planner
module G = Graph.SimpleGraph

type execution_result = {
  node_id : G.Node_id.t;
  status : [ `Built | `Cached | `Failed ];
  duration_ms : int;
  error : string option;
}

type t = { completed : (G.Node_id.t, execution_result) HashMap.t }
type task = Action_node.t
type Message.t += TaskCompleted of execution_result | TaskRequeued of task

let deps_satisfied completed (node : Action_node.t) =
  List.for_all
    (fun dep_id ->
      match HashMap.get completed dep_id with
      | Some { status = `Built | `Cached; _ } -> true
      | Some { status = `Failed; _ } -> false
      | None -> false)
    node.deps

let execute_actions toolchain execution_root actions =
  let ocamlc = Tusk_toolchain.ocamlc toolchain in
  List.iter
    (fun action ->
      let result =
        match action with
        | Action.CompileInterface { source; output; includes; flags } ->
            Tusk_toolchain.Ocamlc.compile_interface ocamlc ~cwd:execution_root
              ~includes ~flags ~output source
        | Action.CompileImplementation { source; output; includes; flags } ->
            Tusk_toolchain.Ocamlc.compile_impl ocamlc ~cwd:execution_root
              ~includes ~flags ~output source
        | Action.GenerateInterface { source; output; includes; flags } ->
            Tusk_toolchain.Ocamlc.generate_interface ocamlc ~cwd:execution_root
              ~includes ~flags ~output source
        | Action.CompileC { source; output } ->
            Tusk_toolchain.Ocamlc.compile_c ocamlc ~cwd:execution_root
              ~includes:[] ~output source
        | Action.CreateLibrary { output; objects; includes } ->
            Tusk_toolchain.Ocamlc.create_library ocamlc ~cwd:execution_root
              ~includes ~output objects
        | Action.CreateExecutable { output; objects; libraries; includes } ->
            Tusk_toolchain.Ocamlc.create_executable ocamlc ~cwd:execution_root
              ~includes ~libs:libraries ~output objects
        | Action.CopyFile { source; destination } -> (
            match Fs.copy ~src:source ~dst:destination with
            | Ok () -> Tusk_toolchain.Ocamlc.Success "Copied"
            | Error _ ->
                Tusk_toolchain.Ocamlc.Failed
                  (format "Copy failed: %s -> %s" (Path.to_string source)
                     (Path.to_string destination)))
        | Action.WriteFile { destination; content } -> (
            match Fs.write content destination with
            | Ok () -> Tusk_toolchain.Ocamlc.Success "Written"
            | Error _ ->
                Tusk_toolchain.Ocamlc.Failed
                  (format "Write failed: %s" (Path.to_string destination)))
      in
      match result with
      | Tusk_toolchain.Ocamlc.Success _ -> ()
      | Tusk_toolchain.Ocamlc.Failed err ->
          panic (format "Action failed: %s\n%s" (Action.to_string action) err))
    actions

let verify_outputs outputs =
  List.iter
    (fun out ->
      match Fs.exists out with
      | Ok true -> ()
      | Ok false | Error _ ->
          panic (format "Expected output not created: %s" (Path.to_string out)))
    outputs

let execute_node toolchain execution_root (node : Action_node.t) =
  let start = Time.Instant.now () in

  execute_actions toolchain execution_root node.value.actions;
  verify_outputs node.value.outs;

  let duration_ms =
    Time.Instant.duration_since ~earlier:start (Time.Instant.now ())
    |> Time.Duration.to_millis
  in

  { node_id = node.id; status = `Built; duration_ms; error = None }

let worker_loop ~owner ~completed toolchain ~execution_root ~work_queue () =
  let rec loop () =
    match Queue.dequeue work_queue with
    | None -> loop ()
    | Some task ->
        if deps_satisfied completed task then (
          match
            Fun.protect
              (fun () -> execute_node toolchain execution_root task)
              ~finally:(fun () -> ())
          with
          | result ->
              send owner (TaskCompleted result);
              loop ()
          | exception exn ->
              let error = format "Exception: %s" (Printexc.to_string exn) in
              send owner
                (TaskCompleted
                   {
                     node_id = task.id;
                     status = `Failed;
                     duration_ms = 0;
                     error = Some error;
                   });
              loop ())
        else (
          Queue.enqueue work_queue task;
          send owner (TaskRequeued task);
          loop ())
  in
  loop ()

let execute ~action_graph ~execution_root toolchain ~concurrency =
  let completed = HashMap.create () in
  let work_queue = Queue.create () in

  let nodes = Action_graph.nodes action_graph in
  List.iter (fun node -> Queue.enqueue work_queue node) nodes;
  let total_nodes = List.length nodes in

  let rec spawn_workers n acc =
    if n = 0 then acc
    else
      let worker =
        spawn (fun () ->
            worker_loop ~owner:(self ()) ~completed toolchain ~execution_root
              ~work_queue ())
      in
      spawn_workers (n - 1) (worker :: acc)
  in
  let workers = spawn_workers concurrency [] in

  let rec collect_results remaining =
    if remaining = 0 then { completed }
    else
      match receive_any () with
      | TaskCompleted result ->
          let _ = HashMap.insert completed result.node_id result in
          Log.info "Node %s completed: %s (%dms)"
            (G.Node_id.to_string result.node_id)
            (match result.status with
            | `Built -> "built"
            | `Cached -> "cached"
            | `Failed -> "failed")
            result.duration_ms;
          collect_results (remaining - 1)
      | TaskRequeued task ->
          Log.debug "Node %s requeued (deps not ready)"
            (G.Node_id.to_string task.id);
          collect_results remaining
      | _ -> collect_results remaining
  in

  collect_results total_nodes
