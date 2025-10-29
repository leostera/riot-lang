open Kernel

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
  io_poll : Async.Poll.t;
  config : Config.t;
  timer_wheel : Timer_wheel.t;
  mutable last_timer_check : int64;
}

let create ~config =
  match Async.Poll.make () with
  | Ok io_poll ->
      let timer_wheel = Timer_wheel.create ~config in
      {
        stop = false;
        status = 0;
        run_queue = Queue.create ();
        processes = Pid_table.create 128;
        current_process = None;
        io_poll;
        config;
        timer_wheel;
        last_timer_check = 0L;
      }
  | Error err ->
      Printf.printf "[Scheduler] ERROR: Failed to create Async.Poll: %s\n%!"
        (match err with
        | `System_error s -> s
        | `Noop -> "Unknown error"
        | `IO_error e -> IO.error_message e
        | `Closed -> "Closed"
        | `Connection_closed -> "Connection closed"
        | `Eof -> "End of file"
        | `Exn e -> Exception.to_string e
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

let shutdown t ~status =
  t.stop <- true;
  t.status <- status

let handle_receive k t proc ~selector ~timeout =
  let open Proc_state in
  let pid_str = Pid.to_string (Process.pid proc) in
  Trace.trace "Process %s receiving (mailbox empty? %b)" pid_str
    (Process.has_empty_mailbox proc);

  (* Check for existing timeout - don't check here, let the timer expire naturally *)
  let should_timeout = false in

  if should_timeout then (
    Trace.trace "Process %s receive timed out" pid_str;
    k (Discontinue Effects.Receive_timeout))
  else if Process.has_empty_mailbox proc then (
    Trace.trace "Process %s has empty mailbox, suspending" pid_str;

    (* Set up timeout if specified *)
    (match timeout with
    | `infinity -> ()
    | `after secs ->
        let now = Time.monotonic_time_nanos () in
        let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
        let timer_id =
          Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos
            ~mode:Timer.One_shot ~action:(Timer.Wake_process proc)
        in
        Process.set_receive_timeout proc timer_id);

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

let handle_syscall k t proc name interest source timeout =
  let open Proc_state in
  let pid_str = Pid.to_string (Process.pid proc) in
  Trace.trace "Process %s performing syscall %s" pid_str name;

  (* Check for syscall timeout *)
  let should_timeout =
    match Process.syscall_timeout proc with
    | Some timer_id ->
        let expired = true in
        (* TODO: proper expiration check *)
        if expired then (
          Process.clear_syscall_timeout proc;
          Timer_wheel.cancel_timer t.timer_wheel timer_id);
        expired
    | None -> false
  in

  if should_timeout then (
    Trace.trace "Process %s syscall %s timed out" pid_str name;
    k (Discontinue Effects.Syscall_timeout))
  else
    match Process.get_ready_token proc with
    | Some (_token, _source) ->
        Trace.trace "Process %s syscall %s ready" pid_str name;
        (* Clear timeout if set *)
        (match Process.syscall_timeout proc with
        | Some timer_id ->
            Timer_wheel.cancel_timer t.timer_wheel timer_id;
            Process.clear_syscall_timeout proc
        | None -> ());
        k (Continue ())
    | None -> (
        let token = Async.Token.make proc in
        Trace.trace "Process %s registering for I/O" pid_str;
        Process.mark_as_awaiting_io proc ~name token source;
        match Async.Poll.register t.io_poll token interest source with
        | Ok () ->
            Trace.trace "Process %s registered for I/O successfully" pid_str;

            (* Set up timeout if specified *)
            (match timeout with
            | `infinity -> ()
            | `after secs ->
                let now = Time.monotonic_time_nanos () in
                let duration_nanos = Int64.of_float (secs *. 1_000_000_000.0) in
                let timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos
                    ~mode:Timer.One_shot ~action:(Timer.Wake_process proc)
                in
                Process.set_syscall_timeout proc timer_id);

            k Suspend
        | Error err ->
            Printf.printf
              "[Scheduler] ERROR: Failed to register I/O for process %s: %s\n%!"
              pid_str
              (match err with
              | `IO_error e -> IO.error_message e
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
    | Receive { selector; timeout } ->
        handle_receive k t proc ~selector ~timeout
    | Yield -> k Yield
    | Syscall { name; interest; source; timeout } ->
        handle_syscall k t proc name interest source timeout
    | _ -> k Suspend
  in
  { perform }

let handle_exit_proc t proc reason =
  if Process.is_main proc then (
    let status = match reason with Ok () -> 0 | Error _ -> 1 in
    shutdown t ~status;

    (* Clean up any I/O registrations *)
    Process.consume_ready_tokens proc (fun (_token, source) ->
        match Async.Poll.deregister t.io_poll source with
        | Ok () -> ()
        | Error err ->
            Printf.printf "[Scheduler] WARN: Failed to deregister I/O: %s\n%!"
              (match err with
              | `IO_error e -> IO.error_message e
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
        Printf.printf
          "[Scheduler] ERROR: Proc_state.run returned None for process %s\n%!"
          pid_str;
        Printf.printf
          "[Scheduler] This should never happen - investigating...\n%!";
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
      Printf.printf "[Scheduler] Process %s finished with exception: %s\n%!"
        pid_str (Exception.to_string exn);
      Printf.printf "[Scheduler] Backtrace:\n%s\n%!"
        (Exception.raw_backtrace_to_string (Exception.get_raw_backtrace ()));
      Trace.trace "Process %s finished with exception: %s" pid_str
        (Exception.to_string exn);
      Process.mark_as_exited proc (Error exn);
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

  (* Calculate timeout based on next timer expiration *)
  let timeout_nanos =
    if Timer_wheel.size t.timer_wheel > 0 then
      let now = Time.monotonic_time_nanos () in
      match Timer_wheel.next_expiration t.timer_wheel ~now with
      | Some next_expiry ->
          let delta = Int64.sub next_expiry now in
          Int64.max delta 0L (* Don't use negative timeout *)
      | None -> 10_000_000L (* 10ms default *)
    else 500_000_000L (* 500ms when no timers *)
  in

  let events =
    match Async.Poll.poll t.io_poll ~timeout:timeout_nanos with
    | Ok events -> events
    | Error err ->
        Printf.printf "[Scheduler] ERROR: Failed to poll I/O: %s\n%!"
          (match err with
          | `IO_error e -> IO.error_message e
          | `Noop -> "Unknown error"
          | _ -> "Other error");
        []
  in
  List.iter
    (fun event ->
      let token = Async.Event.token event in
      let proc : Process.t = Async.Token.unsafe_to_value token in
      Trace.trace "I/O ready for process %s" (Pid.to_string (Process.pid proc));
      match Process.state proc with
      | Waiting_io { source; _ } ->
          (match Async.Poll.deregister t.io_poll source with
          | Ok () -> ()
          | Error err ->
              Printf.printf
                "[Scheduler] WARN: Failed to deregister I/O for process %s: %s\n\
                 %!"
                (Pid.to_string (Process.pid proc))
                (match err with
                | `IO_error e -> IO.error_message e
                | `Noop -> "Unknown error"
                | _ -> "Other error"));
          if Process.is_alive proc then (
            Process.add_ready_token proc token source;
            Process.mark_as_runnable proc;
            add_to_run_queue t proc)
      | _ -> ())
    events

let process_timers t =
  (* Skip timer processing entirely if there are no timers *)
  if Timer_wheel.size t.timer_wheel = 0 then ()
  else
    let now = Time.monotonic_time_nanos () in
    let expired = Timer_wheel.tick t.timer_wheel ~now in

    Trace.trace "Processing %d expired timers" (List.length expired);

    List.iter
      (fun timer ->
        match timer.Timer.action with
        | Timer.Wake_process proc -> (
            Trace.trace "Timer expired, waking process %s"
              (Pid.to_string (Process.pid proc));
            if Process.is_alive proc then (
              Process.mark_as_runnable proc;
              add_to_run_queue t proc);

            (* Handle intervals *)
            match timer.mode with
            | Timer.One_shot -> ()
            | Timer.Interval interval ->
                let now = Time.monotonic_time_nanos () in
                let _new_timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now
                    ~duration_nanos:interval ~mode:timer.mode
                    ~action:timer.action
                in
                ())
        | Timer.Send_message (target_pid, msg) -> (
            Trace.trace "Timer expired, sending message to %s"
              (Pid.to_string target_pid);
            send target_pid msg;

            (* Handle intervals *)
            match timer.mode with
            | Timer.One_shot -> ()
            | Timer.Interval interval ->
                let now = Time.monotonic_time_nanos () in
                let _new_timer_id =
                  Timer_wheel.add_timer t.timer_wheel ~now
                    ~duration_nanos:interval ~mode:timer.mode
                    ~action:timer.action
                in
                ()))
      expired

let run_loop t =
  Trace.trace "Run loop starting";
  while (not (Queue.is_empty t.run_queue)) && not t.stop do
    let proc = Queue.pop t.run_queue in
    Trace.trace "Stepping process %s" (Pid.to_string (Process.pid proc));
    step_process t proc
  done;

  (* Process expired timers only if there are any *)
  if Timer_wheel.size t.timer_wheel > 0 then process_timers t;

  (* Check if we have processes waiting for I/O *)
  let has_waiting_io =
    Pid_table.fold
      (fun _ proc acc -> acc || Process.is_waiting_io proc)
      t.processes false
  in

  (* Poll for I/O events, or sleep until next timer if no I/O *)
  (if (not t.stop) && Pid_table.length t.processes > 0 then
     if has_waiting_io then poll_io t
     else if Timer_wheel.size t.timer_wheel > 0 then
       (* No I/O, but we have timers - sleep until next timer expiration *)
       let now = Time.monotonic_time_nanos () in
       match Timer_wheel.next_expiration t.timer_wheel ~now with
       | Some next_expiry ->
           let sleep_nanos = Int64.sub next_expiry now in
           if Int64.compare sleep_nanos 0L > 0 then
             let _ = Async.Poll.poll t.io_poll ~timeout:sleep_nanos in
             ()
       | None -> ());

  Trace.trace "Run loop done"

let shutdown t ~status =
  Trace.trace "Shutting down scheduler with status %d" status;
  (* Mark scheduler to stop *)
  t.stop <- true;
  t.status <- status;
  (* Clear the run queue *)
  Queue.clear t.run_queue;
  (* Clear all processes *)
  Pid_table.clear t.processes

let add_timer t ~now ~duration_nanos ~mode ~action =
  Timer_wheel.add_timer t.timer_wheel ~now ~duration_nanos ~mode ~action

let cancel_timer t timer_id = Timer_wheel.cancel_timer t.timer_wheel timer_id

let run ~config ~main =
  if !has_run then
    failwith
      "Miniriot.run can only be called once per process. Each test should be \
       in a separate executable.";
  has_run := true;

  let t = create ~config in
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
      (* Still have processes, they might be waiting for I/O or timers *)
      let has_waiting_io =
        Pid_table.fold
          (fun _ proc acc -> acc || Process.is_waiting_io proc)
          t.processes false
      in
      let has_active_timers = Timer_wheel.size t.timer_wheel > 0 in
      if (not has_waiting_io) && not has_active_timers then t.stop <- true
  done;

  t.status
