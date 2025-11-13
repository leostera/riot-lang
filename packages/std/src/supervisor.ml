open Global
open Sync
open Collections

(* # Supervisor - OTP-style process supervision *)

type t = Pid.t

(** {1 Supervision Strategies} *)

type strategy =
  | OneForOne
  | OneForAll
  | RestForOne
  | SimpleOneForOne

(** {1 Restart Policies} *)

type restart =
  | Permanent
  | Temporary
  | Transient

(** {1 Shutdown Behavior} *)

type shutdown =
  | BrutalKill
  | Timeout of Time.Duration.t
  | Infinity

(** {1 Child Types} *)

type child_type =
  | Worker
  | Supervisor

(** {1 Child Specification} *)

type child_spec = {
  id : string;
  start : unit -> Pid.t;
  restart : restart;
  shutdown : shutdown;
  child_type : child_type;
  significant : bool;
}

let child_spec ~id ~start ?(restart = Permanent) ?(shutdown = Timeout (Time.Duration.from_secs 5))
    ?(child_type = Worker) ?(significant = false) () =
  { id; start; restart; shutdown; child_type; significant }

(** {1 Intensity (Restart Limits)} *)

type intensity = {
  max_restarts : int;
  window : Time.Duration.t;
}

(** {1 Child State} *)

type child_state = {
  spec : child_spec;
  pid : Pid.t option;
  monitor : Process.Monitor.t option;
}

(** {1 Supervisor State} *)

type supervisor_state = {
  strategy : strategy;
  intensity : intensity;
  children : child_state list Cell.t;
  restarts : (Time.Instant.t * string) list Cell.t;  (* (timestamp, child_id) *)
}

(** {1 Public Types for Messages} *)

type child_info = {
  id : string;
  pid : Pid.t option;
  child_type : child_type;
  restart : restart;
}

type child_count = {
  specs : int;
  active : int;
  supervisors : int;
  workers : int;
}

type count = child_count

(** {1 Supervisor Messages} *)

type Message.t +=
  | Supervisor_which_children of { reply_to : Pid.t }
  | Supervisor_which_children_reply of { children : child_info list }
  | Supervisor_count_children of { reply_to : Pid.t }
  | Supervisor_count_children_reply of { count : child_count }
  | Supervisor_delete_child of { reply_to : Pid.t; id : string }
  | Supervisor_delete_child_reply of { result : (unit, string) result }
  | Supervisor_restart_child of { reply_to : Pid.t; id : string }
  | Supervisor_restart_child_reply of { result : (Pid.t, string) result }
  | Supervisor_terminate_child of { reply_to : Pid.t; id : string }
  | Supervisor_terminate_child_reply of { result : (unit, string) result }
  | Supervisor_stop of { reply_to : Pid.t }
  | Supervisor_stop_reply

let start_child spec =
  try
    let pid = spec.start () in
    let monitor = Process.monitor pid in
    Some (pid, monitor)
  with _exn -> None

let should_restart (spec : child_spec) reason =
  match (spec.restart, reason) with
  | Permanent, _ -> true
  | Temporary, _ -> false
  | Transient, Ok () -> false
  | Transient, Error _ -> true

let terminate_child_process (child_state : child_state) =
  match child_state.pid with
  | None -> ()
  | Some pid ->
      (match child_state.spec.shutdown with
      | BrutalKill ->
          (* TODO: Need Process.kill or Process.exit API in miniriot *)
          send pid (Process.EXIT { from = self (); reason = Error (Failure "killed") })
      | Timeout _timeout ->
          (* TODO: Implement graceful shutdown with timeout *)
          send pid (Process.EXIT { from = self (); reason = Error (Failure "shutdown") })
      | Infinity ->
          (* TODO: Implement graceful shutdown without timeout *)
          send pid (Process.EXIT { from = self (); reason = Error (Failure "shutdown") }))

let add_restart state child_id =
  let timestamp = Time.Instant.now () in
  let restarts = Cell.get state.restarts in
  Cell.set state.restarts ((timestamp, child_id) :: restarts)

let prune_old_restarts state =
  let current_time = Time.Instant.now () in
  let cutoff = Time.Instant.sub current_time state.intensity.window in
  let restarts = Cell.get state.restarts in
  let recent = List.filter (fun (ts, _) -> Time.Instant.compare ts cutoff >= 0) restarts in
  Cell.set state.restarts recent

let check_intensity state =
  prune_old_restarts state;
  let restart_count = List.length (Cell.get state.restarts) in
  restart_count <= state.intensity.max_restarts

(** {1 Restart Strategies} *)

let restart_one_for_one state child_id reason =
  let children : child_state list = Cell.get state.children in
  let updated =
    List.map
      (fun (child : child_state) ->
        let spec : child_spec = child.spec in
        if spec.id = child_id && should_restart spec reason then (
          (* Terminate old process if still alive *)
          (match child.monitor with
          | Some mon -> Process.demonitor mon
          | None -> ());
          
          (* Start new process *)
          match start_child child.spec with
          | Some (pid, monitor) ->
              add_restart state child_id;
              { child with pid = Some pid; monitor = Some monitor }
          | None ->
              { child with pid = None; monitor = None })
        else child)
      children
  in
  Cell.set state.children updated

let restart_one_for_all state _child_id reason =
  let children = Cell.get state.children in
  
  (* Terminate all children *)
  List.iter terminate_child_process children;
  List.iter (fun child ->
    match child.monitor with
    | Some mon -> Process.demonitor mon
    | None -> ()
  ) children;
  
  (* Restart all children *)
  let updated =
    List.map
      (fun child ->
        if should_restart child.spec reason then
          match start_child child.spec with
          | Some (pid, monitor) ->
              add_restart state child.spec.id;
              { child with pid = Some pid; monitor = Some monitor }
          | None ->
              { child with pid = None; monitor = None }
        else
          { child with pid = None; monitor = None })
      children
  in
  Cell.set state.children updated

let restart_rest_for_one state child_id reason =
  let children = Cell.get state.children in
  
  (* Find the failed child index *)
  let rec find_index idx = function
    | [] -> None
    | child :: _ when child.spec.id = child_id -> Some idx
    | _ :: rest -> find_index (idx + 1) rest
  in
  
  match find_index 0 children with
  | None -> ()
  | Some failed_idx ->
      (* Terminate failed child and all after it *)
      List.iteri (fun idx child ->
        if idx >= failed_idx then terminate_child_process child
      ) children;
      
      (* Restart failed child and all after it *)
      let updated =
        List.mapi (fun idx (child : child_state) ->
          if idx >= failed_idx && should_restart child.spec reason then
            match start_child child.spec with
            | Some (pid, monitor) ->
                add_restart state child.spec.id;
                { child with pid = Some pid; monitor = Some monitor }
            | None ->
                { child with pid = None; monitor = None }
          else if idx >= failed_idx then
            { child with pid = None; monitor = None }
          else
            child
        ) children
      in
      Cell.set state.children updated

let handle_child_exit state child_id reason =
  (* Check if child is significant *)
  let children = Cell.get state.children in
  let is_significant =
    List.exists
      (fun child -> child.spec.id = child_id && child.spec.significant)
      children
  in
  
  if is_significant then
    Error (Failure ("Significant child " ^ child_id ^ " terminated"))
  else (
    (* Apply restart strategy *)
    (match state.strategy with
    | OneForOne -> restart_one_for_one state child_id reason
    | OneForAll -> restart_one_for_all state child_id reason
    | RestForOne -> restart_rest_for_one state child_id reason
    | SimpleOneForOne -> restart_one_for_one state child_id reason);
    
    (* Check restart intensity *)
    if check_intensity state then
      Ok ()
    else
      Error (Failure "Max restart intensity reached"))

(** {1 Message Handlers} *)

let handle_which_children state reply_to =
  let children = Cell.get state.children in
  let child_infos =
    List.map
      (fun child ->
        {
          id = child.spec.id;
          pid = child.pid;
          child_type = child.spec.child_type;
          restart = child.spec.restart;
        })
      children
  in
  send reply_to (Supervisor_which_children_reply { children = child_infos });
  Ok ()

let handle_count_children state reply_to =
  let children : child_state list = Cell.get state.children in
  let specs = List.length children in
  let active = List.fold_left (fun acc (child : child_state) ->
    match child.pid with Some _ -> acc + 1 | None -> acc
  ) 0 children in
  let supervisors = List.fold_left (fun acc (child : child_state) ->
    match child.spec.child_type with Supervisor -> acc + 1 | _ -> acc
  ) 0 children in
  let workers = List.fold_left (fun acc (child : child_state) ->
    match child.spec.child_type with Worker -> acc + 1 | _ -> acc
  ) 0 children in
  
  send reply_to (Supervisor_count_children_reply {
    count = { specs; active; supervisors; workers }
  });
  Ok ()

let handle_delete_child state reply_to id =
  if state.strategy = SimpleOneForOne then (
    send reply_to (Supervisor_delete_child_reply {
      result = Error "Cannot delete child from SimpleOneForOne supervisor"
    });
    Ok ()
  ) else (
    let children = Cell.get state.children in
    let child_opt = List.find_opt (fun c -> c.spec.id = id) children in
    
    match child_opt with
    | None ->
        send reply_to (Supervisor_delete_child_reply {
          result = Error ("Child not found: " ^ id)
        });
        Ok ()
    | Some child when Option.is_some child.pid ->
        send reply_to (Supervisor_delete_child_reply {
          result = Error ("Child is still running: " ^ id)
        });
        Ok ()
    | Some _child ->
        let updated = List.filter (fun c -> c.spec.id != id) children in
        Cell.set state.children updated;
        send reply_to (Supervisor_delete_child_reply { result = Ok () });
        Ok ()
  )

let handle_restart_child state reply_to id =
  if state.strategy = SimpleOneForOne then (
    send reply_to (Supervisor_restart_child_reply {
      result = Error "Cannot restart child in SimpleOneForOne supervisor"
    });
    Ok ()
  ) else (
    let children = Cell.get state.children in
    let child_opt = List.find_opt (fun c -> c.spec.id = id) children in
    
    match child_opt with
    | None ->
        send reply_to (Supervisor_restart_child_reply {
          result = Error ("Child not found: " ^ id)
        });
        Ok ()
    | Some child when Option.is_some child.pid ->
        send reply_to (Supervisor_restart_child_reply {
          result = Error ("Child already running: " ^ id)
        });
        Ok ()
    | Some child ->
        (match start_child child.spec with
        | Some (pid, monitor) ->
            let updated = List.map (fun (c : child_state) ->
              if c.spec.id = id then
                { c with pid = Some pid; monitor = Some monitor }
              else c
            ) children in
            Cell.set state.children updated;
            send reply_to (Supervisor_restart_child_reply { result = Ok pid });
            Ok ()
        | None ->
            send reply_to (Supervisor_restart_child_reply {
              result = Error ("Failed to start child: " ^ id)
            });
            Ok ())
  )

let handle_terminate_child state reply_to id =
  if state.strategy = SimpleOneForOne then (
    send reply_to (Supervisor_terminate_child_reply {
      result = Error "Cannot terminate child in SimpleOneForOne supervisor"
    });
    Ok ()
  ) else (
    let children = Cell.get state.children in
    let child_opt = List.find_opt (fun c -> c.spec.id = id) children in
    
    match child_opt with
    | None ->
        send reply_to (Supervisor_terminate_child_reply {
          result = Error ("Child not found: " ^ id)
        });
        Ok ()
    | Some child ->
        terminate_child_process child;
        (match child.monitor with
        | Some mon -> Process.demonitor mon
        | None -> ());
        
        let updated = List.map (fun (c : child_state) ->
          if c.spec.id = id then
            { c with pid = None; monitor = None }
          else c
        ) children in
        Cell.set state.children updated;
        send reply_to (Supervisor_terminate_child_reply { result = Ok () });
        Ok ()
  )

let handle_stop state reply_to =
  (* Stop all children in reverse order *)
  let children = Cell.get state.children in
  List.iter terminate_child_process (List.rev children);
  send reply_to Supervisor_stop_reply;
  Ok ()

(** {1 Supervisor Loop} *)

let rec loop state =
  let selector msg =
    match msg with
    | Process.DOWN _ -> `select msg
    | Supervisor_which_children _ -> `select msg
    | Supervisor_count_children _ -> `select msg
    | Supervisor_delete_child _ -> `select msg
    | Supervisor_restart_child _ -> `select msg
    | Supervisor_terminate_child _ -> `select msg
    | Supervisor_stop _ -> `select msg
    | _ -> `skip
  in
  
  match receive ~selector () with
  | Process.DOWN { pid; reason; _ } ->
      (* Find which child died *)
      let children : child_state list = Cell.get state.children in
      let child_opt = List.find_opt (fun (c : child_state) -> c.pid = Some pid) children in
      (match child_opt with
      | Some child ->
          (match handle_child_exit state child.spec.id reason with
          | Ok () -> loop state
          | Error exn -> Error exn)
      | None ->
          (* UnkTime.Instant.nown child, ignore *)
          loop state)
  
  | Supervisor_which_children { reply_to } ->
      (match handle_which_children state reply_to with
      | Ok () -> loop state
      | Error exn -> Error exn)
  
  | Supervisor_count_children { reply_to } ->
      (match handle_count_children state reply_to with
      | Ok () -> loop state
      | Error exn -> Error exn)
  
  | Supervisor_delete_child { reply_to; id } ->
      (match handle_delete_child state reply_to id with
      | Ok () -> loop state
      | Error exn -> Error exn)
  
  | Supervisor_restart_child { reply_to; id } ->
      (match handle_restart_child state reply_to id with
      | Ok () -> loop state
      | Error exn -> Error exn)
  
  | Supervisor_terminate_child { reply_to; id } ->
      (match handle_terminate_child state reply_to id with
      | Ok () -> loop state
      | Error exn -> Error exn)
  
  | Supervisor_stop { reply_to } ->
      handle_stop state reply_to
  
  | _ ->
      (* Should never happen since selector filters *)
      loop state

(** {1 Starting Supervisors} *)

let init_supervisor strategy intensity children () =
  let intensity =
    match intensity with
    | Some i -> i
    | None -> { max_restarts = 3; window = Time.Duration.from_secs 5 }
  in
  
  (* Start all children *)
  let child_states =
    List.map
      (fun spec ->
        match start_child spec with
        | Some (pid, monitor) ->
            { spec; pid = Some pid; monitor = Some monitor }
        | None ->
            { spec; pid = None; monitor = None })
      children
  in
  
  let state = {
    strategy;
    intensity;
    children = cell child_states;
    restarts = cell [];
  } in
  
  loop state

let start_link ~strategy ?intensity ~children () =
  spawn (init_supervisor strategy intensity children)

let start ~strategy ?intensity ~children () =
  spawn (init_supervisor strategy intensity children)

(** {1 Child Management} *)

let which_children supervisor =
  send supervisor (Supervisor_which_children { reply_to = self () });
  let selector msg =
    match msg with
    | Supervisor_which_children_reply { children } -> `select children
    | _ -> `skip
  in
  receive ~selector ()

let count_children supervisor =
  send supervisor (Supervisor_count_children { reply_to = self () });
  let selector msg =
    match msg with
    | Supervisor_count_children_reply { count } -> `select count
    | _ -> `skip
  in
  receive ~selector ()

let delete_child supervisor ~id =
  send supervisor (Supervisor_delete_child { reply_to = self (); id });
  let selector msg =
    match msg with
    | Supervisor_delete_child_reply { result } -> `select result
    | _ -> `skip
  in
  receive ~selector ()

let restart_child supervisor ~id =
  send supervisor (Supervisor_restart_child { reply_to = self (); id });
  let selector msg =
    match msg with
    | Supervisor_restart_child_reply { result } -> `select result
    | _ -> `skip
  in
  receive ~selector ()

let terminate_child supervisor ~id =
  send supervisor (Supervisor_terminate_child { reply_to = self (); id });
  let selector msg =
    match msg with
    | Supervisor_terminate_child_reply { result } -> `select result
    | _ -> `skip
  in
  receive ~selector ()

(** {1 Stopping Supervisors} *)

let stop supervisor =
  send supervisor (Supervisor_stop { reply_to = self () });
  let selector msg =
    match msg with
    | Supervisor_stop_reply -> `select ()
    | _ -> `skip
  in
  receive ~selector ()

(** {1 Dynamic Supervision} *)

module Dynamic = struct
  type t = Pid.t

  let to_pid t = t

  type dynamic_child = {
    pid : Pid.t;
    monitor : Process.Monitor.t;
    restart : restart;
    shutdown : shutdown;
  }

  type dynamic_state = {
    intensity : intensity;
    max_children : int option;
    children : (Pid.t, dynamic_child) HashMap.t;
    restarts : (Time.Instant.t * Pid.t) list Cell.t;
  }

  type Message.t +=
    | Dynamic_start_child of {
        reply_to : Pid.t;
        start : unit -> Pid.t;
        restart : restart;
        shutdown : shutdown;
      }
    | Dynamic_start_child_reply of { result : (Pid.t, string) result }
    | Dynamic_terminate_child of { reply_to : Pid.t; pid : Pid.t }
    | Dynamic_terminate_child_reply of { result : (unit, string) result }
    | Dynamic_which_children of { reply_to : Pid.t }
    | Dynamic_which_children_reply of { children : Pid.t list }
    | Dynamic_count_children of { reply_to : Pid.t }
    | Dynamic_count_children_reply of { count : child_count }

  let rec dynamic_loop state =
    let selector msg =
      match msg with
      | Process.DOWN _ -> `select msg
      | Dynamic_start_child _ -> `select msg
      | Dynamic_terminate_child _ -> `select msg
      | Dynamic_which_children _ -> `select msg
      | Dynamic_count_children _ -> `select msg
      | _ -> `skip
    in
    
    match receive ~selector () with
    | Process.DOWN { pid; reason; _ } ->
        (match HashMap.get state.children pid with
        | None -> dynamic_loop state
        | Some child ->
            let _ = HashMap.remove state.children pid in
            
            if should_restart { id = ""; start = (fun () -> pid); restart = child.restart;
                                shutdown = child.shutdown; child_type = Worker;
                                significant = false } reason then (
              (* Add restart record *)
              let timestamp = Time.Instant.now () in
              let restarts = Cell.get state.restarts in
              Cell.set state.restarts ((timestamp, pid) :: restarts);
              
              (* Check intensity *)
              let current_time = Time.Instant.now () in
              let cutoff = Time.Instant.sub current_time state.intensity.window in
              let recent = List.filter (fun (ts, _) -> Time.Instant.compare ts cutoff >= 0) (Cell.get state.restarts) in
              Cell.set state.restarts recent;
              
              if List.length recent > state.intensity.max_restarts then
                Error (Failure "Max restart intensity reached")
              else
                dynamic_loop state
            ) else
              dynamic_loop state)
    
    | Dynamic_start_child { reply_to; start; restart; shutdown } ->
        (match state.max_children with
        | Some max when HashMap.len state.children >= max ->
            send reply_to (Dynamic_start_child_reply {
              result = Error "max_children_reached"
            });
            dynamic_loop state
        | _ ->
            (try
              let pid = start () in
              let monitor = Process.monitor pid in
              let _ = HashMap.insert state.children pid { pid; monitor; restart; shutdown } in
              send reply_to (Dynamic_start_child_reply { result = Ok pid });
              dynamic_loop state
            with exn ->
              send reply_to (Dynamic_start_child_reply {
                result = Error (Exception.to_string exn)
              });
              dynamic_loop state))
    
    | Dynamic_terminate_child { reply_to; pid } ->
        (match HashMap.get state.children pid with
        | None ->
            send reply_to (Dynamic_terminate_child_reply {
              result = Error "not_found"
            });
            dynamic_loop state
        | Some child ->
            Process.demonitor child.monitor;
            let _ = HashMap.remove state.children pid in
            (* TODO: Actually terminate the child based on shutdown spec *)
            send pid (Process.EXIT { from = self (); reason = Error (Failure "shutdown") });
            send reply_to (Dynamic_terminate_child_reply { result = Ok () });
            dynamic_loop state)
    
    | Dynamic_which_children { reply_to } ->
        let pids = HashMap.fold (fun pid _ acc -> pid :: acc) state.children [] in
        send reply_to (Dynamic_which_children_reply { children = pids });
        dynamic_loop state
    
    | Dynamic_count_children { reply_to } ->
        let total = HashMap.len state.children in
        send reply_to (Dynamic_count_children_reply {
          count = { specs = total; active = total; supervisors = 0; workers = total }
        });
        dynamic_loop state
    
    | _ ->
        (* Should never happen since selector filters *)
        dynamic_loop state

  let init_dynamic intensity max_children () =
    let intensity =
      match intensity with
      | Some i -> i
    | None -> { max_restarts = 3; window = Time.Duration.from_secs 5 }
    in
    
    let state = {
      intensity;
      max_children;
      children = HashMap.with_capacity 16;
      restarts = cell [];
    } in
    
    dynamic_loop state

  let start_link ?intensity ?max_children () =
    spawn (init_dynamic intensity max_children)

  let start ?intensity ?max_children () =
    spawn (init_dynamic intensity max_children)

  let start_child supervisor ~start ?(restart = Permanent) ?(shutdown = Timeout (Time.Duration.from_secs 5)) () =
    send supervisor (Dynamic_start_child { reply_to = self (); start; restart; shutdown });
    let selector msg =
      match msg with
      | Dynamic_start_child_reply { result } -> `select result
      | _ -> `skip
    in
    receive ~selector ()

  let terminate_child supervisor pid =
    send supervisor (Dynamic_terminate_child { reply_to = self (); pid });
    let selector msg =
      match msg with
      | Dynamic_terminate_child_reply { result } -> `select result
      | _ -> `skip
    in
    receive ~selector ()

  let which_children supervisor =
    send supervisor (Dynamic_which_children { reply_to = self () });
    let selector msg =
      match msg with
      | Dynamic_which_children_reply { children } -> `select children
      | _ -> `skip
    in
    receive ~selector ()

  let count_children supervisor =
    send supervisor (Dynamic_count_children { reply_to = self () });
    let selector msg =
      match msg with
      | Dynamic_count_children_reply { count } -> `select count
      | _ -> `skip
    in
    receive ~selector ()
end
