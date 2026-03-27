open Std
open Std.Iter
open Std.Collections

type file_result = {
  file : Path.t;
  needs_formatting : bool;
  error : string option;
  duration : Time.Duration.t;
}

type summary = {
  total_files : int;
  already_formatted : int;
  needs_formatting : int;
  failed_files : int;
  duration : Time.Duration.t;
}

type run_result = { files : file_result list; summary : summary }

type Message.t +=
  | ScannerRequest of unit Ref.t
  | ScannerStop of unit Ref.t
  | ScannerDiscovered of { scanner_ref : unit Ref.t; file : Path.t }
  | ScannerComplete of unit Ref.t
  | DispatchFileChecked of {
      result_ref : file_result Ref.t;
      result : file_result;
    }
  | StreamFileResult of { run_ref : unit Ref.t; result : file_result }
  | StreamCompleted of unit Ref.t

let is_ocaml_source path =
  let path = Path.to_string path in
  String.ends_with ~suffix:".ml" path || String.ends_with ~suffix:".mli" path

let should_skip_directory path =
  let basename = Path.basename path in
  String.starts_with ~prefix:"." basename
  || String.equal basename "_build"
  || String.equal basename "target"

let compare_paths left right =
  String.compare (Path.to_string left) (Path.to_string right)

let rec walk_dir dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> MutIterator.to_list
      |> List.concat_map (fun entry ->
             let entry_path = Path.(dir / entry) in
             match Fs.is_dir entry_path with
             | Ok true ->
                 if should_skip_directory entry_path then
                   []
                 else
                   walk_dir entry_path
             | Ok false | Error _ ->
                 if is_ocaml_source entry_path then
                   [ entry_path ]
                 else
                   [])

let collect_ocaml_files ~roots =
  roots
  |> List.concat_map (fun root ->
         match Fs.is_dir root with
         | Ok true -> walk_dir root
         | Ok false | Error _ ->
             if is_ocaml_source root then
               [ root ]
             else
               [])
  |> List.sort_uniq compare_paths

let check_file file =
  let start = Time.Instant.now () in
  let finalize ~needs_formatting ~error =
    {
      file;
      needs_formatting;
      error;
      duration = Time.Instant.elapsed start;
    }
  in
  match Fs.read file with
  | Error _ ->
      finalize ~needs_formatting:false
        ~error:(Some ("Failed to read " ^ Path.to_string file))
  | Ok source ->
      let parsed = Syn.parse ~filename:file source in
      (match Format_core.format parsed with
      | Ok formatted ->
          finalize ~needs_formatting:(not (String.equal source formatted))
            ~error:None
      | Error err ->
          finalize ~needs_formatting:false
            ~error:(Some (Format_core.format_error_to_string err)))

type scanner_state = {
  owner : Pid.t;
  scanner_ref : unit Ref.t;
  seen : string HashSet.t;
  mutable pending : Path.t list;
}

let sorted_directory_entries dir =
  match Fs.read_dir dir with
  | Error _ -> []
  | Ok entries ->
      entries
      |> MutIterator.to_list
      |> List.map (fun entry -> Path.(dir / entry))
      |> List.sort compare_paths

let rec next_discovered_file state =
  match state.pending with
  | [] -> None
  | path :: rest ->
      state.pending <- rest;
      let path_string = Path.to_string path in
      if HashSet.contains state.seen path_string then
        next_discovered_file state
      else (
        let _ = HashSet.insert state.seen path_string in
        match Fs.is_dir path with
        | Ok true ->
            if should_skip_directory path then
              next_discovered_file state
            else (
              state.pending <- sorted_directory_entries path @ state.pending;
              next_discovered_file state)
        | Ok false | Error _ ->
            if is_ocaml_source path then
              Some path
            else
              next_discovered_file state)

let rec scanner_loop state =
  let selector : [ `Request | `Stop ] selector = function
    | ScannerRequest scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
        `select `Request
    | ScannerStop scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
        `select `Stop
    | _ -> `skip
  in
  match receive ~selector () with
  | `Stop -> Ok ()
  | `Request -> (
      match next_discovered_file state with
      | Some file ->
          send state.owner
            (ScannerDiscovered { scanner_ref = state.scanner_ref; file });
          scanner_loop state
      | None ->
          send state.owner (ScannerComplete state.scanner_ref);
          scanner_done_loop state)

and scanner_done_loop state =
  let selector : [ `Request | `Stop ] selector = function
    | ScannerRequest scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
        `select `Request
    | ScannerStop scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
        `select `Stop
    | _ -> `skip
  in
  match receive ~selector () with
  | `Stop -> Ok ()
  | `Request ->
      send state.owner (ScannerComplete state.scanner_ref);
      scanner_done_loop state

let start_scanner ~owner ~roots ~scanner_ref =
  let seen = HashSet.create () in
  let state = { owner; scanner_ref; seen; pending = List.sort compare_paths roots } in
  spawn (fun () -> scanner_loop state)

type dispatch_state = {
  owner : Pid.t;
  run_ref : unit Ref.t;
  scanner : Pid.t;
  scanner_ref : unit Ref.t;
  pool : Path.t WorkerPool.DynamicWorkerPool.t;
  result_ref : file_result Ref.t;
  pending_files : Path.t Queue.t;
  idle_workers : Path.t WorkerPool.DynamicWorkerPool.worker Queue.t;
  buffer_limit : int;
  mutable pending_requests : int;
  mutable tasks_in_flight : int;
  mutable discovery_complete : bool;
}

let dispatch_ready_workers state =
  let rec loop () =
    match (Queue.front state.idle_workers, Queue.front state.pending_files) with
    | Some _, Some _ ->
        let worker =
          Queue.pop state.idle_workers
          |> Option.expect ~msg:"idle worker should exist"
        in
        let file =
          Queue.pop state.pending_files
          |> Option.expect ~msg:"pending file should exist"
        in
        state.tasks_in_flight <- state.tasks_in_flight + 1;
        WorkerPool.DynamicWorkerPool.send_task state.pool worker file;
        loop ()
    | _ -> ()
  in
  loop ()

let refill_scanner state =
  if state.discovery_complete then
    ()
  else
    let buffered =
      Queue.len state.pending_files + state.tasks_in_flight + state.pending_requests
    in
    let missing = max 0 (state.buffer_limit - buffered) in
    for _ = 1 to missing do
      state.pending_requests <- state.pending_requests + 1;
      send state.scanner (ScannerRequest state.scanner_ref)
    done

let is_dispatch_complete state =
  state.discovery_complete
  && state.pending_requests = 0
  && state.tasks_in_flight = 0
  && Queue.is_empty state.pending_files

let rec dispatch_loop state =
  if is_dispatch_complete state then (
    send state.scanner (ScannerStop state.scanner_ref);
    send state.owner (StreamCompleted state.run_ref);
    Ok ())
  else
    let selector :
        [
          `WorkerReady of Path.t WorkerPool.DynamicWorkerPool.worker
        | `ScannerDiscovered of Path.t
        | `ScannerComplete
        | `FileChecked of file_result
        ]
        selector =
     function
      | WorkerPool.DynamicWorkerPool.WorkerReady worker -> (
          match
            Ref.type_equal state.pool.task_ref
              (WorkerPool.DynamicWorkerPool.get_worker_task_ref worker)
          with
          | Some Type.Equal -> `select (`WorkerReady worker)
          | None -> `skip)
      | ScannerDiscovered { scanner_ref; file }
        when Ref.equal state.scanner_ref scanner_ref ->
          `select (`ScannerDiscovered file)
      | ScannerComplete scanner_ref when Ref.equal state.scanner_ref scanner_ref ->
          `select `ScannerComplete
      | DispatchFileChecked { result_ref; result }
        when Ref.equal state.result_ref result_ref ->
          `select (`FileChecked result)
      | _ -> `skip
    in
    match receive ~selector () with
    | `WorkerReady worker ->
        Queue.push state.idle_workers worker;
        dispatch_ready_workers state;
        refill_scanner state;
        dispatch_loop state
    | `ScannerDiscovered file ->
        state.pending_requests <- max 0 (state.pending_requests - 1);
        Queue.push state.pending_files file;
        dispatch_ready_workers state;
        refill_scanner state;
        dispatch_loop state
    | `ScannerComplete ->
        state.pending_requests <- max 0 (state.pending_requests - 1);
        state.discovery_complete <- true;
        dispatch_loop state
    | `FileChecked result ->
        state.tasks_in_flight <- max 0 (state.tasks_in_flight - 1);
        send state.owner (StreamFileResult { run_ref = state.run_ref; result });
        dispatch_ready_workers state;
        refill_scanner state;
        dispatch_loop state

let start_dispatcher ~owner ~run_ref ~concurrency ~roots =
  let dispatcher_owner = self () in
  let scanner_ref = Ref.make () in
  let result_ref = Ref.make () in
  let worker_fn ~owner ~task =
    let result = check_file task in
    send owner (DispatchFileChecked { result_ref; result })
  in
  let scanner = start_scanner ~owner:dispatcher_owner ~roots ~scanner_ref in
  let pool =
    WorkerPool.DynamicWorkerPool.start ~concurrency ~owner:dispatcher_owner
      ~worker_fn ()
  in
  let state =
    {
      owner;
      run_ref;
      scanner;
      scanner_ref;
      pool;
      result_ref;
      pending_files = Queue.create ();
      idle_workers = Queue.create ();
      buffer_limit = max 1 (concurrency * 2);
      pending_requests = 0;
      tasks_in_flight = 0;
      discovery_complete = false;
    }
  in
  refill_scanner state;
  dispatch_loop state

let summarize ~duration files =
  List.fold_left
    (fun acc result ->
      match result.error, result.needs_formatting with
      | Some _, _ ->
          { acc with total_files = acc.total_files + 1; failed_files = acc.failed_files + 1 }
      | None, true ->
          {
            acc with
            total_files = acc.total_files + 1;
            needs_formatting = acc.needs_formatting + 1;
          }
      | None, false ->
          {
            acc with
            total_files = acc.total_files + 1;
            already_formatted = acc.already_formatted + 1;
          })
    {
      total_files = 0;
      already_formatted = 0;
      needs_formatting = 0;
      failed_files = 0;
      duration;
    }
    files

let run_checks_streaming ?(concurrency = System.available_parallelism) ~roots
    ~on_result () =
  let concurrency = max 1 concurrency in
  let run_ref = Ref.make () in
  let owner = self () in
  let start = Time.Instant.now () in
  let _dispatcher =
    spawn (fun () -> start_dispatcher ~owner ~run_ref ~concurrency ~roots)
  in
  let rec collect results_rev =
    let selector :
        [ `FileResult of file_result | `Completed ] selector = function
      | StreamFileResult { run_ref = msg_ref; result } when Ref.equal run_ref msg_ref
        -> `select (`FileResult result)
      | StreamCompleted msg_ref when Ref.equal run_ref msg_ref ->
          `select `Completed
      | _ -> `skip
    in
    match receive ~selector () with
    | `FileResult result ->
        on_result result;
        collect (result :: results_rev)
    | `Completed ->
        let files = List.rev results_rev in
        let duration = Time.Instant.elapsed start in
        { files; summary = summarize ~duration files }
  in
  collect []

let run_checks ?(concurrency = System.available_parallelism) files =
  let concurrency = max 1 concurrency in
  let start = Time.Instant.now () in
  let files = List.sort compare_paths files in
  let results =
    WorkerPool.SimpleWorkerPool.run ~concurrency ~tasks:files ~fn:check_file ()
    |> List.map snd
    |> List.sort (fun left right -> compare_paths left.file right.file)
  in
  let duration = Time.Instant.elapsed start in
  { files = results; summary = summarize ~duration results }
