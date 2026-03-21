open Std
open Std.Collections

type config = {
  files : Path.t list;
  concurrency : int;
  mode : Runner.mode;
  scope : Fix_config.scope option;
  owner : Pid.t;
}

type state = {
  file_queue : Path.t Queue.t;
  busy_workers : (Pid.t, Path.t) HashMap.t;
  mutable results_rev : Runner.file_result list;
  mutable stop_requested : bool;
  mutable stopped_workers : int;
  total_workers : int;
  mode : Runner.mode;
  scope : Fix_config.scope option;
  owner : Pid.t;
}

let rec loop state =
  let selector msg =
    match msg with
    | Messages.WorkerReady worker -> `select (`WorkerReady worker)
    | Messages.StopRequested -> `select `StopRequested
    | Messages.FileResult r -> `select (`FileResult r)
    | _ -> `skip
  in

  if is_complete state then handle_complete state
  else
    match receive ~selector () with
    | `WorkerReady worker -> handle_worker_ready state worker
    | `StopRequested -> handle_stop_requested state
    | `FileResult r -> handle_file_result state r

and handle_worker_ready state worker =
  if state.stop_requested || Queue.is_empty state.file_queue then (
    send worker Messages.Stop;
    state.stopped_workers <- state.stopped_workers + 1;
    if is_complete state then
      handle_complete state
    else
      loop state)
  else
    match Queue.pop state.file_queue with
    | None -> loop state
    | Some file_path ->
        send worker (Messages.RunTask file_path);
        let _ = HashMap.insert state.busy_workers worker file_path in
        loop state

and handle_file_result state r =
  ignore (HashMap.remove state.busy_workers r.worker);
  state.results_rev <- r.result :: state.results_rev;
  send state.owner (Messages.FileResult r);
  if is_complete state then
    handle_complete state
  else
    loop state

and handle_stop_requested state =
  state.stop_requested <- true;
  if is_complete state then
    handle_complete state
  else
    loop state

and handle_complete state =
  let summary = Runner.summarize (List.rev state.results_rev) in
  send state.owner (Messages.AllComplete summary);
  Ok ()

and is_complete state =
  HashMap.is_empty state.busy_workers
  && state.stopped_workers = state.total_workers
  && (state.stop_requested || Queue.is_empty state.file_queue)

let init config () =
  let file_queue = Queue.create () in
  List.iter (fun f -> Queue.push file_queue f) config.files;
  for _ = 1 to config.concurrency do
    ignore
      (Worker.start
         { mode = config.mode; scope = config.scope; coordinator = self () })
  done;
  let state =
    {
      file_queue;
      busy_workers = HashMap.create ();
      results_rev = [];
      stop_requested = false;
      stopped_workers = 0;
      total_workers = config.concurrency;
      mode = config.mode;
      scope = config.scope;
      owner = config.owner;
    }
  in
  loop state

let start config = spawn (init config)
