open Std
open Std.Collections

type config = {
  files : Path.t list;
  concurrency : int;
  format : Reporter.format;
  owner : Pid.t;
}

type state = {
  file_queue : Path.t Queue.t;
  idle_workers : Pid.t Queue.t;
  busy_workers : (Pid.t, Path.t) HashMap.t;
  total_files : int;
  mutable processed_files : int;
  mutable total_diagnostics : int;
  mutable failed_files : int;
  format : Reporter.format;
  owner : Pid.t;
  pipeline : Pipeline.t;
}

let rec loop state =
  let selector msg =
    match msg with
    | Messages.WorkerReady worker -> `select (`WorkerReady worker)
    | Messages.LintResult r -> `select (`LintResult r)
    | Messages.WorkerFailed f -> `select (`WorkerFailed f)
    | _ -> `skip
  in

  if is_complete state then handle_complete state
  else
    match receive ~selector () with
    | `WorkerReady worker -> handle_worker_ready state worker
    | `LintResult r -> handle_lint_result state r
    | `WorkerFailed f -> handle_worker_failed state f

and handle_worker_ready state worker =
  match Queue.pop state.file_queue with
  | None ->
      (* No more files, keep worker idle *)
      Queue.push state.idle_workers worker;
      loop state
  | Some file_path ->
      (* Assign work to worker *)
      send worker (Messages.LintTask file_path);
      let _ = HashMap.insert state.busy_workers worker file_path in
      loop state

and handle_lint_result state r =
  let file = r.file in
  let diagnostics = r.diagnostics in
  let source = r.source in
  (* Remove worker from busy map *)
  let worker_opt =
    HashMap.into_iter state.busy_workers
    |> Iter.Iterator.find ~fn:(fun (_w, f) -> Path.equal f file)
    |> Option.map fst
  in

  (match worker_opt with
  | Some worker -> ignore (HashMap.remove state.busy_workers worker)
  | None -> ());

  (* Print diagnostics immediately for text format *)
  (match state.format with
  | Reporter.Text ->
      if List.length diagnostics > 0 then (
        let has_parse_errors =
          List.exists (fun d -> Diagnostic.rule_id d = "parse_error") diagnostics
        in
        if not has_parse_errors then (
          let grouped = Diagnostic.group_diagnostics diagnostics in
          List.iter
            (fun grouped_diag ->
              print
                (Diagnostic.grouped_to_formatted_output ~file ~source
                   grouped_diag))
            grouped))
  | Reporter.Json ->
      (* For JSON mode, we'd need to buffer results - TODO *)
      ());

  (* Update state *)
  state.processed_files <- state.processed_files + 1;
  state.total_diagnostics <- state.total_diagnostics + List.length diagnostics;

  (* Worker now ready for more work *)
  (match worker_opt with
  | Some worker -> Queue.push state.idle_workers worker
  | None -> ());

  loop state

and handle_worker_failed state f =
  let file = f.file in
  let worker = f.worker in
  let reason = f.reason in
  Log.warn ("Failed to lint " ^ Path.to_string file ^ ": " ^ reason);

  ignore (HashMap.remove state.busy_workers worker);
  state.processed_files <- state.processed_files + 1;
  state.failed_files <- state.failed_files + 1;

  (* Spawn new worker to replace crashed one *)
  let new_worker =
    Worker.start { pipeline = state.pipeline; coordinator = self () }
  in
  Queue.push state.idle_workers new_worker;
  loop state

and handle_complete state =
  let result : Messages.completion_result =
    {
      total_files = state.total_files;
      total_diagnostics = state.total_diagnostics;
      failed_files = state.failed_files;
    }
  in
  send state.owner (Messages.AllComplete result);
  Ok ()

and is_complete state =
  Queue.is_empty state.file_queue && HashMap.is_empty state.busy_workers

let init config () =
  let file_queue = Queue.create () in
  List.iter (fun f -> Queue.push file_queue f) config.files;

  let idle_workers = Queue.create () in
  let pipeline = Pipeline.default () in

  (* Spawn N workers *)
  for _ = 1 to config.concurrency do
    let worker = Worker.start { pipeline; coordinator = self () } in
    Queue.push idle_workers worker
  done;

  let state =
    {
      file_queue;
      idle_workers;
      busy_workers = HashMap.create ();
      total_files = List.length config.files;
      processed_files = 0;
      total_diagnostics = 0;
      failed_files = 0;
      format = config.format;
      owner = config.owner;
      pipeline;
    }
  in

  loop state

let start config = spawn (init config)
