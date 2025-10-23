open Miniriot
open Global
open Collections

type event = ..

type event +=
  | SpanStarted of { started_at : Time.Instant.t; event : event }
  | SpanCompleted of { completed_at : Time.Instant.t; event : event }

type handler_id = string
type handler = { id : handler_id; fn : event -> unit }

module Server = struct
  type state = { handlers : (handler_id, handler) HashMap.t }
  type reply_to = Pid.t

  type message =
    | AttachHandler of handler
    | DetachHandler of handler_id
    | DetachAll
    | ListHandlers of reply_to
    | Emit of event
    | Stop of reply_to

  type Message.t += Telemetry of message
  type Message.t += HandlerList of string list
  type Message.t += Stopped

  let rec loop state =
    let selector msg =
      match msg with Telemetry msg -> `select msg | _ -> `skip
    in
    match receive ~selector () with
    | AttachHandler handler ->
        let _ = HashMap.insert state.handlers handler.id handler in
        loop state
    | DetachHandler handler_id ->
        let _ = HashMap.remove state.handlers handler_id in
        loop state
    | DetachAll ->
        HashMap.clear state.handlers;
        loop state
    | ListHandlers reply_to ->
        let handler_ids = HashMap.keys state.handlers in
        send reply_to (HandlerList handler_ids);
        loop state
    | Emit event ->
        HashMap.iter
          (fun _id handler -> try handler.fn event with _ -> ())
          state.handlers;
        loop state
    | Stop reply_to ->
        send reply_to Stopped;
        Ok ()

  let init () =
    let state = { handlers = HashMap.create () } in
    loop state

  let start () = spawn init
end

let pid : Pid.t option Cell.t = cell None

let start () =
  match !pid with
  | None ->
      let server_pid = Server.start () in
      pid := Some server_pid;
      server_pid
  | Some pid -> pid

let emit event =
  match !pid with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (Emit event))

let attach id fn =
  match !pid with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (AttachHandler { id; fn }))

let detach id =
  match !pid with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry (DetachHandler id))

let detach_all () =
  match !pid with
  | None -> ()
  | Some pid -> send pid Server.(Telemetry DetachAll)

let list_handlers () =
  match !pid with
  | None -> []
  | Some pid ->
      send pid Server.(Telemetry (ListHandlers (self ())));
      let selector msg =
        match msg with Server.HandlerList ids -> `select ids | _ -> `skip
      in
      receive ~selector ()

let stop () =
  match !pid with
  | None -> ()
  | Some pid ->
      send pid Server.(Telemetry (Stop (self ())));
      let selector msg =
        match msg with Server.Stopped -> `select () | _ -> `skip
      in
      receive ~selector ()
