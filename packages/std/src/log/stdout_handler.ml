open Global
open Collections
open Sync
open Sync.Cell

let handler_id = "log_stdout"

module Server = struct
  type state = {
    style : Log_config.format_style;
  }
  
  type message = Write of Event.t

  type Message.t += StdoutHandler of message

  let format_event event style =
    match style with
    | Log_config.Full ->
        let timestamp = Datetime.to_iso8601 event.Event.timestamp in
        let level_str = Level.to_string event.Event.level in
        let meta_str = Metadata.to_string event.Event.metadata in
        let meta_part = if meta_str = "" then "" else " [" ^ meta_str ^ "]" in
        timestamp ^ " | " ^ level_str ^ " | " ^ event.Event.message ^ meta_part
        ^ "\n"
    | Log_config.Compact ->
        let level_str = Level.to_string event.Event.level in
        level_str ^ " | " ^ event.Event.message ^ "\n"

  let rec loop state =
    let selector msg =
      match msg with StdoutHandler msg -> `select msg | _ -> `skip
    in
    match receive ~selector () with
    | Write event ->
        let line = format_event event state.style in
        print line;
        loop state

  let init () =
    let config = 
      Config.get (module Log_config) 
      |> Result.expect ~msg:"Could not find log config"
    in
    
    (* Find the stdout handler in the list *)
    let stdout_format = List.find_map (function
      | Log_config.Stdout { format } -> Some format
      | _ -> None
    ) config.handlers in
    
    match stdout_format with
    | Some format -> loop { style = format }
    | None -> 
        (* No stdout handler configured, use default *)
        loop { style = Log_config.Full }
end

(** Shared process state - updated by supervised process on start *)
let handler_pid = Cell.create None

(** Attach handler callback - sends events to supervised process *)
let attach () =
  let handler event =
    match !handler_pid with
    | None -> () (* Not supervised yet, drop message *)
    | Some pid ->
        send pid Server.(StdoutHandler (Write event))
  in
  Handler.attach handler_id handler

let detach () = Handler.detach handler_id

(** Child spec for supervision *)
let child_spec () =
  Supervisor.child_spec ~id:handler_id
    ~start:(fun () ->
      let pid =
        spawn_link (fun () ->
            (* Update shared state so handler callbacks can find us *)
            handler_pid := Some (self ());
            Server.init ())
      in
      attach ();
      pid)
    ~restart:Permanent
    ~shutdown:(Timeout (Time.Duration.from_secs 5))
    ()

