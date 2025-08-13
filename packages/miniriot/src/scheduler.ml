module Pid_table = Hashtbl.Make (struct
  type t = Pid.t

  let equal = Pid.equal
  let hash = Hashtbl.hash
end)

type t = {
  mutable stop : bool;
  mutable status : int;
  run_queue : Process.t Queue.t;
  processes : Process.t Pid_table.t;
  mutable current_process : Process.t option;
  io_poll : Gluon.Poll.t;
}

let create () =
  match Gluon.Poll.make () with
  | Ok io_poll ->
      {
        stop = false;
        status = 0;
        run_queue = Queue.create ();
        processes = Pid_table.create 128;
        current_process = None;
        io_poll;
      }
  | Error err ->
      Printf.printf "[Scheduler] ERROR: Failed to create Gluon.Poll: %s\n%!" 
        (match err with 
        | `System_error s -> s 
        | `Noop -> "Unknown error"
        | `Unix_error e -> Unix.error_message e
        | `Closed -> "Closed"
        | `Connection_closed -> "Connection closed"
        | `Eof -> "End of file"
        | `Exn e -> Printexc.to_string e
        | `No_info -> "No info"
        | `Process_down -> "Process down"
        | `Timeout -> "Timeout"
        | `Would_block -> "Would block");
      failwith "Failed to create I/O polling system"

let current_scheduler = ref None
let has_run = ref false

let get_scheduler () =
  match !current_scheduler with
  | None -> failwith "No scheduler running"
  | Some s -> s

let add_to_run_queue t proc =
  Trace.trace "Adding process %s to run queue"
    (Pid.to_string (Process.pid proc));
  Queue.push proc t.run_queue

let spawn t fn =
  let proc = Process.make fn in
  let pid = Process.pid proc in
  Trace.trace "Spawning process %s" (Pid.to_string pid);
  Pid_table.add t.processes pid proc;
  add_to_run_queue t proc;
  pid

let self () =
  let t = get_scheduler () in
  match t.current_process with
  | None -> failwith "No process running"
  | Some proc -> Process.pid proc

let send pid msg =
  let t = get_scheduler () in
  Trace.trace "Sending message to %s" (Pid.to_string pid);
  match Pid_table.find_opt t.processes pid with
  | None -> Trace.trace "Process %s not found!" (Pid.to_string pid)
  | Some proc ->
      let was_waiting = Process.is_waiting proc in
      Process.send_message proc msg;
      if was_waiting then (
        Trace.trace "Process %s was waiting, now runnable" (Pid.to_string pid);
        add_to_run_queue t proc)

let shutdown t status =
  t.stop <- true;
  t.status <- status

let handle_receive k _t proc ~selector =
  let open Proc_state in
  let pid_str = Pid.to_string (Process.pid proc) in
  Trace.trace "Process %s receiving (mailbox empty? %b)" pid_str
    (Process.has_empty_mailbox proc);

  if Process.has_empty_mailbox proc then (
    Trace.trace "Process %s has empty mailbox, suspending" pid_str;
    Process.mark_as_awaiting_message proc;
    k Suspend)
  else
    let fuel = Process.message_count proc in
    Trace.trace "Process %s has %d messages" pid_str fuel;
    let rec go fuel =
      if fuel = 0 then (
        Trace.trace "Process %s out of fuel, delaying" pid_str;
        k Delay)
      else
        match Process.next_message proc with
        | None ->
            Trace.trace "Process %s no more messages, switching to save queue"
              pid_str;
            Process.read_save_queue proc;
            k Delay
        | Some msg -> (
            Trace.trace "Process %s got message" pid_str;
            match selector Message.(msg.msg) with
            | `select msg ->
                Trace.trace "Process %s selected message" pid_str;
                k (Continue msg)
            | `skip ->
                Trace.trace "Process %s skipped message" pid_str;
                Process.add_to_save_queue proc msg;
                go (fuel - 1))
    in
    go fuel

let handle_syscall k t proc name interest source _timeout =
  let open Proc_state in
  let pid_str = Pid.to_string (Process.pid proc) in
  Trace.trace "Process %s performing syscall %s" pid_str name;

  match Process.get_ready_token proc with
  | Some (_token, _source) ->
      Trace.trace "Process %s syscall %s ready" pid_str name;
      k (Continue ())
  | None ->
      let token = Gluon.Token.make proc in
      Trace.trace "Process %s registering for I/O" pid_str;
      Process.mark_as_awaiting_io proc name token source;
      (match Gluon.Poll.register t.io_poll token interest source with
      | Ok () ->
          Trace.trace "Process %s registered for I/O successfully" pid_str;
          k Suspend
      | Error err ->
          Printf.printf "[Scheduler] ERROR: Failed to register I/O for process %s: %s\n%!" 
            pid_str 
            (match err with 
            | `Unix_error e -> Unix.error_message e
            | `Noop -> "Unknown error"
            | _ -> "Other error");
          (* Don't continue - the I/O operation failed to register *)
          k (Discontinue (Failure "Failed to register I/O")))

let perform t proc =
  let open Proc_state in
  let open Proc_effect in
  let perform : type a b. (a, b) step_callback =
   fun k eff ->
    match eff with
    | Receive { selector } -> handle_receive k t proc ~selector
    | Yield -> k Yield
    | Syscall { name; interest; source; timeout } ->
        handle_syscall k t proc name interest source timeout
    | _ -> k Suspend
  in
  { perform }

let handle_exit_proc t proc reason =
  if Process.is_main proc then (
    let status = if reason = Process.Normal then 0 else 1 in
    shutdown t status;

    (* Clean up any I/O registrations *)
    Process.consume_ready_tokens proc (fun (_token, source) ->
        match Gluon.Poll.deregister t.io_poll source with
        | Ok () -> ()
        | Error err ->
            Printf.printf "[Scheduler] WARN: Failed to deregister I/O: %s\n%!"
              (match err with 
              | `Unix_error e -> Unix.error_message e
              | `Noop -> "Unknown error"
              | _ -> "Other error"));

    Pid_table.remove t.processes (Process.pid proc);
    Process.mark_as_finalized proc)

let handle_wait_proc t proc =
  if Process.has_messages proc then (
    Process.mark_as_runnable proc;
    add_to_run_queue t proc)

let handle_run_proc t proc =
  let pid_str = Pid.to_string (Process.pid proc) in
  Trace.trace "Running process %s" pid_str;
  Process.mark_as_running proc;
  t.current_process <- Some proc;
  let perform = perform t proc in
  let cont =
    match Proc_state.run ~reductions:100 ~perform (Process.cont proc) with
    | Some cont -> cont
    | None -> 
        Printf.printf "[Scheduler] ERROR: Proc_state.run returned None for process %s\n%!" pid_str;
        Printf.printf "[Scheduler] This should never happen - investigating...\n%!";
        failwith "Proc_state.run returned None"
  in
  Process.set_cont proc cont;
  match cont with
  | Proc_state.Finished (Ok reason) ->
      Trace.trace "Process %s finished with reason" pid_str;
      Process.mark_as_exited proc reason;
      add_to_run_queue t proc
  | Proc_state.Finished (Error exn) ->
      (* Always print exception details, not just when tracing is enabled *)
      Printf.printf "[Scheduler] Process %s finished with exception: %s\n%!" pid_str
        (Printexc.to_string exn);
      Printf.printf "[Scheduler] Backtrace:\n%s\n%!" 
        (Printexc.raw_backtrace_to_string (Printexc.get_raw_backtrace ()));
      Trace.trace "Process %s finished with exception: %s" pid_str
        (Printexc.to_string exn);
      Process.mark_as_exited proc (Exception exn);
      add_to_run_queue t proc
  | _ when Process.is_waiting proc ->
      Trace.trace "Process %s is waiting" pid_str
  | Proc_state.Suspended _ ->
      Trace.trace "Process %s suspended, re-queueing" pid_str;
      add_to_run_queue t proc
  | Proc_state.Unhandled _ ->
      Trace.trace "Process %s unhandled, re-queueing" pid_str;
      add_to_run_queue t proc

let handle_init_proc t proc =
  Process.init proc;
  handle_run_proc t proc

let handle_wait_io_proc _t _proc =
  (* Process will be woken up by I/O polling *)
  ()

let step_process t proc =
  match Process.state proc with
  | Uninitialized -> handle_init_proc t proc
  | Finalized -> failwith "finalized processes should never be stepped on"
  | Waiting_message -> handle_wait_proc t proc
  | Waiting_io _ -> handle_wait_io_proc t proc
  | Exited reason -> handle_exit_proc t proc reason
  | Running | Runnable -> handle_run_proc t proc

let poll_io t =
  Trace.trace "Polling for I/O events";
  let events = match Gluon.Poll.poll t.io_poll with
    | Ok events -> events
    | Error err ->
        Printf.printf "[Scheduler] ERROR: Failed to poll I/O: %s\n%!"
          (match err with 
          | `Unix_error e -> Unix.error_message e
          | `Noop -> "Unknown error"
          | _ -> "Other error");
        []
  in
  List.iter
    (fun event ->
      let token = Gluon.Event.token event in
      let proc : Process.t = Gluon.Token.unsafe_to_value token in
      Trace.trace "I/O ready for process %s" (Pid.to_string (Process.pid proc));
      match Process.state proc with
      | Waiting_io { source; _ } ->
          (match Gluon.Poll.deregister t.io_poll source with
          | Ok () -> ()
          | Error err ->
              Printf.printf "[Scheduler] WARN: Failed to deregister I/O for process %s: %s\n%!"
                (Pid.to_string (Process.pid proc))
                (match err with 
                | `Unix_error e -> Unix.error_message e
                | `Noop -> "Unknown error"
                | _ -> "Other error"));
          if Process.is_alive proc then (
            Process.add_ready_token proc token source;
            Process.mark_as_runnable proc;
            add_to_run_queue t proc)
      | _ -> ())
    events

let run_loop t =
  Trace.trace "Run loop starting";
  while (not (Queue.is_empty t.run_queue)) && not t.stop do
    let proc = Queue.pop t.run_queue in
    Trace.trace "Stepping process %s" (Pid.to_string (Process.pid proc));
    step_process t proc
  done;

  (* Poll for I/O events when run queue is empty *)
  if not t.stop then poll_io t;

  Trace.trace "Run loop done"

let shutdown ~status =
  match !current_scheduler with
  | None -> () (* No scheduler running, nothing to shutdown *)
  | Some t ->
      Trace.trace "Shutting down scheduler with status %d" status;
      (* Mark scheduler to stop *)
      t.stop <- true;
      t.status <- status;
      (* Clear the run queue *)
      Queue.clear t.run_queue;
      (* Clear all processes *)
      Pid_table.clear t.processes

let run ~main =
  if !has_run then
    failwith
      "Miniriot.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true;

  let t = create () in
  current_scheduler := Some t;

  (* Spawn main process with PID 0 *)
  let _ = spawn t main in

  (* Run until completion *)
  while not t.stop do
    run_loop t;
    (* If no processes are running and none are waiting for I/O, we're done *)
    if Queue.is_empty t.run_queue && Pid_table.length t.processes = 0 then
      t.stop <- true
    else if Queue.is_empty t.run_queue then
      (* Still have processes, they might be waiting for I/O *)
      let has_waiting_io =
        Pid_table.fold
          (fun _ proc acc -> acc || Process.is_waiting_io proc)
          t.processes false
      in
      if not has_waiting_io then t.stop <- true
  done;

  t.status
