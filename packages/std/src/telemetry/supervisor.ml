open Global
open Sync
open Collections

type event = Event.t

type handler_id = string

type handler = {
  id: handler_id;
  fn: event -> unit;
}

type server_ref = {
  pid: Pid.t;
}

module Server = struct
  type state = {
    handlers: (handler_id, handler) HashMap.t;
  }

  type reply_to = Pid.t

  type request_id = int

  type message =
    | AttachHandler of handler
    | DetachHandler of handler_id
    | DetachAll
    | ListHandlers of {
        reply_to: reply_to;
        request_id: request_id;
      }
    | Emit of event
    | Stop of {
        reply_to: reply_to;
        request_id: request_id;
      }

  type Message.t +=
    | Telemetry of message

  type Message.t +=
    | HandlerList of {
        request_id: request_id;
        ids: string list;
      }

  type Message.t +=
    | Stopped of {
        request_id: request_id;
      }

  let rec loop = fun state ->
    let selector msg =
      match msg with
      | Telemetry msg -> Select msg
      | _ -> Skip
    in
    match receive ~selector () with
    | AttachHandler handler ->
        let _ = HashMap.insert state.handlers ~key:handler.id ~value:handler in
        loop state
    | DetachHandler handler_id ->
        let _ = HashMap.remove state.handlers ~key:handler_id in
        loop state
    | DetachAll ->
        HashMap.clear state.handlers;
        loop state
    | ListHandlers { reply_to; request_id } ->
        let handler_ids = HashMap.keys state.handlers in
        send reply_to (HandlerList { request_id; ids = handler_ids });
        loop state
    | Emit event ->
        HashMap.for_each
          state.handlers
          ~fn:(fun _id handler ->
            try handler.fn event with
            | _ -> ());
        loop state
    | Stop { reply_to; request_id } ->
        send reply_to (Stopped { request_id });
        Ok ()

  let init = fun () ->
    let state = { handlers = HashMap.create () } in
    loop state

  let start = fun () -> spawn init
end

let pid: server_ref option Atomic.t = Atomic.make None

let request_counter = Atomic.make 0

let clear_pid_if_matches = fun candidate ->
  match Atomic.get pid with
  | Some current when Pid.equal current.pid candidate ->
      let _ = Atomic.compare_and_set pid (Some current) None in
      ()
  | _ -> ()

let next_request_id = fun () -> Atomic.fetch_and_add request_counter 1 + 1

let await_stopped = fun pid ->
  let request_id = next_request_id () in
  send pid Server.(Telemetry (Stop { reply_to = self (); request_id }));
  let selector msg =
    match msg with
    | Server.Stopped { request_id = got } when Kernel.Int.equal got request_id -> Select ()
    | _ -> Skip
  in
  (
    try receive ~selector ~timeout:(Time.Duration.from_millis 200) () with
    | Receive_timeout -> ()
  )

let rec start = fun () ->
  match Atomic.get pid with
  | Some current -> current.pid
  | None ->
      let current = { pid = Server.start () } in
      if Atomic.compare_and_set pid None (Some current) then
        current.pid
      else (
        await_stopped current.pid;
        start ()
      )

let emit = fun event ->
  match Atomic.get pid with
  | None -> ()
  | Some current -> send current.pid Server.(Telemetry (Emit event))

let attach = fun id fn ->
  match Atomic.get pid with
  | None -> ()
  | Some current -> send current.pid Server.(Telemetry (AttachHandler { id; fn }))

let detach = fun id ->
  match Atomic.get pid with
  | None -> ()
  | Some current -> send current.pid Server.(Telemetry (DetachHandler id))

let detach_all = fun () ->
  match Atomic.get pid with
  | None -> ()
  | Some current -> send current.pid Server.(Telemetry DetachAll)

let list_handlers = fun () ->
  match Atomic.get pid with
  | None -> []
  | Some current ->
      let request_id = next_request_id () in
      send current.pid Server.(Telemetry (ListHandlers { reply_to = self (); request_id }));
      let selector msg =
        match msg with
        | Server.HandlerList { request_id = got; ids } when Kernel.Int.equal got request_id ->
            Select ids
        | _ -> Skip
      in
      (
        try receive ~selector ~timeout:(Time.Duration.from_millis 100) () with
        | Receive_timeout ->
            clear_pid_if_matches current.pid;
            []
      )

let stop = fun () ->
  match Atomic.exchange pid None with
  | None -> ()
  | Some current -> await_stopped current.pid
