open Kernel

type exit_reason = exn

type state =
  | Uninitialized
  | Runnable
  | Waiting_message
  | Waiting_io of {
      name : string;
      token : Async.Token.t;
      source : Async.Source.t;
    }
  | Running
  | Exited of (unit, exit_reason) result
  | Finalized

type t = {
  pid : Pid.t;
  mutable state : state;
  mutable cont : (unit, exit_reason) result Proc_state.t option;
  mutable fn : (unit -> (unit, exit_reason) result) option;
  mailbox : Mailbox.t;
  save_queue : Mailbox.t;
  mutable read_save_queue : bool;
  mutable ready_tokens : (Async.Token.t * Async.Source.t) list;
}

let make fn =
  let pid = Pid.next () in
  {
    pid;
    cont = None;
    fn = Some fn;
    state = Uninitialized;
    mailbox = Mailbox.create ();
    save_queue = Mailbox.create ();
    read_save_queue = false;
    ready_tokens = [];
  }

let init t =
  let fn = Option.get t.fn in
  t.cont <- Some (Proc_state.make fn Proc_effect.Yield);
  t.fn <- None;
  t.state <- Runnable

let pid t = t.pid
let state t = t.state
let is_alive t = match t.state with Finalized | Exited _ -> false | _ -> true
let is_exited t = match t.state with Finalized | Exited _ -> true | _ -> false
let is_waiting t = match t.state with Waiting_message -> true | _ -> false
let is_waiting_io t = match t.state with Waiting_io _ -> true | _ -> false
let is_runnable t = t.state = Runnable
let is_running t = t.state = Running
let is_main t = Pid.equal t.pid Pid.main

let has_empty_mailbox t =
  Mailbox.is_empty t.save_queue && Mailbox.is_empty t.mailbox

let has_messages t = not (has_empty_mailbox t)
let message_count t = Mailbox.size t.mailbox + Mailbox.size t.save_queue
let mark_as_running t = t.state <- Running
let mark_as_runnable t = if is_alive t then t.state <- Runnable
let mark_as_awaiting_message t = if is_alive t then t.state <- Waiting_message
let mark_as_exited t reason = if not (is_exited t) then t.state <- Exited reason
let mark_as_finalized t = t.state <- Finalized
let cont t = Option.get t.cont
let set_cont t c = t.cont <- Some c

let next_message t =
  if t.read_save_queue then (
    match Mailbox.next t.save_queue with
    | Some m -> Some m
    | None ->
        t.read_save_queue <- false;
        None)
  else match Mailbox.next t.mailbox with Some m -> Some m | None -> None

let add_to_save_queue t msg = Mailbox.queue t.save_queue msg
let read_save_queue t = t.read_save_queue <- true

let send_message t msg =
  if is_alive t then (
    let envelope = Message.envelope msg in
    Mailbox.queue t.mailbox envelope;
    if is_waiting t then mark_as_runnable t)

(* I/O operations *)
let mark_as_awaiting_io t ~name token source =
  if is_alive t then t.state <- Waiting_io { name; token; source }

let add_ready_token t token source =
  t.ready_tokens <- (token, source) :: t.ready_tokens

let get_ready_token t =
  match t.ready_tokens with
  | [] -> None
  | token :: rest ->
      t.ready_tokens <- rest;
      Some token

let consume_ready_tokens t f =
  List.iter f t.ready_tokens;
  t.ready_tokens <- []

let pp ppf t =
  Format.fprintf ppf "Process %a { state = %s; messages = %d }" Pid.pp t.pid
    (match t.state with
    | Uninitialized -> "Uninitialized"
    | Runnable -> "Runnable"
    | Waiting_message -> "Waiting_message"
    | Waiting_io { name; _ } -> format "Waiting_io(%s)" name
    | Running -> "Running"
    | Exited _ -> "Exited"
    | Finalized -> "Finalized")
    (message_count t)
