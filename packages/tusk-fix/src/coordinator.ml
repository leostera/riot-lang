open Std
open Std.Collections

type input =
  | Files of Path.t list
  | Roots of Path.t list

type config = {
  input: input;
  concurrency: int;
  limit: int option;
  mode: Runner.mode;
  scope: Fix_config.scope option;
  owner: Pid.t;
}

type state = {
  file_queue: Path.t Queue.t;
  idle_workers: Pid.t Queue.t;
  busy_workers: (Pid.t, Path.t) HashMap.t;
  mutable results_rev: Runner.file_result list;
  mutable diagnostics_seen: int;
  mutable stop_requested: bool;
  mutable stopped_workers: int;
  mutable discovery_complete: bool;
  limit: int option;
  total_workers: int;
  mode: Runner.mode;
  scope: Fix_config.scope option;
  owner: Pid.t;
}

let diagnostic_count = fun result ->
  Runner.(List.length result.parse_diagnostics + List.length result.diagnostics)

let should_ignore_file = Fix_config.should_ignore_file

let stop_worker = fun state worker ->
  send worker Messages.Stop;
  state.stopped_workers <- state.stopped_workers + 1

let rec stop_idle_workers = fun state ->
  match Queue.pop state.idle_workers with
  | Some worker ->
      stop_worker state worker;
      stop_idle_workers state
  | None -> ()

let rec dispatch_ready_workers = fun state ->
  match Queue.pop state.idle_workers with
  | Some worker -> (
      match Queue.pop state.file_queue with
      | Some file_path ->
          send state.owner (Messages.FileStarted file_path);
          send worker (Messages.RunTask file_path);
          let _ = HashMap.insert state.busy_workers worker file_path in
          dispatch_ready_workers state
      | None -> Queue.push state.idle_workers worker
    )
  | None -> ()

let maybe_stop_waiting_workers = fun state ->
  if state.stop_requested || (state.discovery_complete && Queue.is_empty state.file_queue) then
    stop_idle_workers state

let is_complete = fun state ->
  HashMap.is_empty state.busy_workers
  && state.stopped_workers = state.total_workers
  && state.discovery_complete
  && (state.stop_requested || Queue.is_empty state.file_queue)

let handle_complete = fun state ->
  let summary = Runner.summarize (List.rev state.results_rev) in
  send state.owner (Messages.AllComplete summary);
  Ok ()

let rec loop = fun state ->
  let selector msg =
    match msg with
    | Messages.ScannerDiscovered file -> `select (`ScannerDiscovered file)
    | Messages.ScannerComplete -> `select `ScannerComplete
    | Messages.WorkerReady worker -> `select (`WorkerReady worker)
    | Messages.StopRequested -> `select `StopRequested
    | Messages.FileProgress progress -> `select (`FileProgress progress)
    | Messages.FileResult r -> `select (`FileResult r)
    | _ -> `skip
  in
  if is_complete state then
    handle_complete state
  else
    match receive ~selector () with
    | `ScannerDiscovered file -> handle_scanner_discovered state file
    | `ScannerComplete -> handle_scanner_complete state
    | `WorkerReady worker -> handle_worker_ready state worker
    | `StopRequested -> handle_stop_requested state
    | `FileProgress progress -> handle_file_progress state progress
    | `FileResult r -> handle_file_result state r

and handle_scanner_discovered = fun state file ->
  if not state.stop_requested then
    (
      Queue.push state.file_queue file;
      dispatch_ready_workers state
    );
  loop state

and handle_scanner_complete = fun state ->
  state.discovery_complete <- true;
  maybe_stop_waiting_workers state;
  if is_complete state then
    handle_complete state
  else
    loop state

and handle_worker_ready = fun state worker ->
  if state.stop_requested then
    (
      stop_worker state worker;
      if is_complete state then
        handle_complete state
      else
        loop state
    )
  else
    match Queue.pop state.file_queue with
    | Some file_path ->
        send state.owner (Messages.FileStarted file_path);
        send worker (Messages.RunTask file_path);
        let _ = HashMap.insert state.busy_workers worker file_path in
        loop state
    | None ->
        if state.discovery_complete then
          (
            stop_worker state worker;
            if is_complete state then
              handle_complete state
            else
              loop state
          )
        else (
          Queue.push state.idle_workers worker;
          loop state
        )

and handle_file_result = fun state r ->
  ignore (HashMap.remove state.busy_workers r.worker);
  state.results_rev <- r.result :: state.results_rev;
  state.diagnostics_seen <- state.diagnostics_seen + diagnostic_count r.result;
  (
    match state.limit with
    | Some max_diagnostics when state.diagnostics_seen >= max_diagnostics -> state.stop_requested <- true
    | _ -> ()
  );
  send state.owner (Messages.FileResult r);
  dispatch_ready_workers state;
  maybe_stop_waiting_workers state;
  if is_complete state then
    handle_complete state
  else
    loop state

and handle_file_progress = fun state progress ->
  send state.owner (Messages.FileProgress progress);
  loop state

and handle_stop_requested = fun state ->
  state.stop_requested <- true;
  maybe_stop_waiting_workers state;
  if is_complete state then
    handle_complete state
  else
    loop state

let init = fun config () ->
  let file_queue = Queue.create () in
  let idle_workers = Queue.create () in
  let discovery_complete =
    match config.input with
    | Files files ->
        List.iter
          (fun f ->
            Queue.push file_queue f)
          files;
        true
    | Roots roots ->
        ignore
          (File_scanner.start
            ~owner:(self ())
            (File_scanner.create_many ~roots ~should_ignore:(should_ignore_file config.scope) ()));
        false
  in
  for _ = 1 to config.concurrency do
    yield ();
    ignore (Worker.start { mode = config.mode; scope = config.scope; coordinator = self () })
  done;
  let state = {
    file_queue;
    idle_workers;
    busy_workers = HashMap.create ();
    results_rev = [];
    diagnostics_seen = 0;
    stop_requested = false;
    stopped_workers = 0;
    discovery_complete;
    limit = config.limit;
    total_workers = config.concurrency;
    mode = config.mode;
    scope = config.scope;
    owner = config.owner;
  }
  in
  loop state

let start = fun config -> spawn (init config)
