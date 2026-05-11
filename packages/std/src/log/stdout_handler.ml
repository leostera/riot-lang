open Global
open Collections
open Sync
open Sync.Cell

let handler_id = "log_stdout"

type request_id = int

let request_counter = Atomic.make 0

let next_request_id = fun () -> Atomic.fetch_and_add request_counter 1 + 1

module Server = struct
  type state = {
    style: Log_config.format_style;
  }

  type message =
    | Write of Event.t
    | Flush of {
        reply_to: Pid.t;
        request_id: request_id;
      }

  type Message.t +=
    | StdoutHandler of message

  type Message.t +=
    | StdoutHandler_ready of {
        request_id: request_id;
      }

  type Message.t +=
    | StdoutHandler_flushed of {
        request_id: request_id;
      }

  let format_event = fun event style ->
    match style with
    | Log_config.Full ->
        let timestamp = DateTime.to_iso8601 event.Event.timestamp in
        let level_str = Level.to_string event.Event.level in
        let meta_str = Metadata.to_string event.Event.metadata in
        let meta_part =
          if meta_str = "" then
            ""
          else
            " [" ^ meta_str ^ "]"
        in
        timestamp ^ " | " ^ level_str ^ " | " ^ event.Event.message ^ meta_part ^ "\n"
    | Log_config.Compact ->
        let level_str = Level.to_string event.Event.level in
        level_str ^ " | " ^ event.Event.message ^ "\n"

  let rec loop = fun state ->
    let selector msg =
      match msg with
      | StdoutHandler msg -> Select msg
      | _ -> Skip
    in
    match receive ~selector () with
    | Write event ->
        let line = format_event event state.style in
        print line;
        loop state
    | Flush { reply_to; request_id } ->
        send reply_to (StdoutHandler_flushed { request_id });
        loop state

  let init = fun () ->
    let stdout_format =
      match Config.get (module Log_config) with
      | Ok config -> (
          match List.find
            config.handlers
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Log_config.Stdout _ -> true
              | _ -> false) with
          | Some (Log_config.Stdout { format }) -> format
          | Some _
          | None -> Log_config.Full
        )
      | Error (Config.NotFound _) -> Log_config.Full
      | Error err -> panic (Config.error_to_string err)
    in
    loop { style = stdout_format }
end

(** Shared process state - updated by supervised process on start *)
let handler_pid = Cell.create None

(** Attach handler callback - sends events to supervised process *)
let attach = fun () ->
  let handler event =
    match !handler_pid with
    | None -> ()
    | Some pid -> send pid Server.(StdoutHandler (Write event))
  in
  Handler.attach handler_id handler

let detach = fun () -> Handler.detach handler_id

let flush = fun () ->
  match !handler_pid with
  | None -> ()
  | Some pid ->
      let request_id = next_request_id () in
      send pid Server.(StdoutHandler (Flush { reply_to = self (); request_id }));
      let selector msg =
        match msg with
        | Server.StdoutHandler_flushed { request_id = got } when Int.equal got request_id ->
            Select ()
        | _ -> Skip
      in
      try receive ~selector ~timeout:(Time.Duration.from_millis 100) () with
      | Receive_timeout -> ()

(** Child spec for supervision *)
let child_spec = fun () ->
  Supervisor.child_spec
    ~id:handler_id
    ~start:(fun () ->
      let request_id = next_request_id () in
      let starter = self () in
      let pid =
        spawn_link
          (fun () ->
            (* Update shared state so handler callbacks can find us *)
            handler_pid := Some (self ());
            send starter Server.(StdoutHandler_ready { request_id });
            Server.init ())
      in
      let selector msg =
        match msg with
        | Server.StdoutHandler_ready { request_id = got } when Int.equal got request_id -> Select ()
        | _ -> Skip
      in
      receive ~selector ();
      attach ();
      pid)
    ~restart:Permanent
    ~shutdown:(Timeout (Time.Duration.from_secs 5))
    ()
