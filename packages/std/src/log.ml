open Global
open Sync
open Sync.Cell

(** Log levels from least to most severe *)
type level = Trace | Debug | Info | Warn | Error

let level_to_int = function
  | Trace -> 0
  | Debug -> 1
  | Info -> 2
  | Warn -> 3
  | Error -> 4

let level_to_string = function
  | Trace -> "TRACE"
  | Debug -> "DEBUG"
  | Info -> "INFO"
  | Warn -> "WARN"
  | Error -> "ERROR"

(** Metadata attached to log events *)
module Metadata = struct
  type t = {
    module_name : string option;
    function_name : string option;
    file : string option;
    line : int option;
    pid : Pid.t option;
    custom : (string * string) list;
  }

  let empty =
    {
      module_name = None;
      function_name = None;
      file = None;
      line = None;
      pid = None;
      custom = [];
    }

  let make ?module_name ?function_name ?file ?line ?pid ?(custom = []) () =
    { module_name; function_name; file; line; pid; custom }

  let merge t1 t2 =
    {
      module_name = Option.or_ t2.module_name t1.module_name;
      function_name = Option.or_ t2.function_name t1.function_name;
      file = Option.or_ t2.file t1.file;
      line = Option.or_ t2.line t1.line;
      pid = Option.or_ t2.pid t1.pid;
      custom = t2.custom @ t1.custom;
    }

  let to_string t =
    let parts = [] in
    let parts =
      match t.module_name with
      | None -> parts
      | Some m -> ("module=" ^ m) :: parts
    in
    let parts =
      match t.function_name with
      | None -> parts
      | Some f -> ("function=" ^ f) :: parts
    in
    let parts =
      match t.file with None -> parts | Some f -> ("file=" ^ f) :: parts
    in
    let parts =
      match t.line with
      | None -> parts
      | Some l -> ("line=" ^ string_of_int l) :: parts
    in
    let parts =
      match t.pid with
      | None -> parts
      | Some p -> ("pid=" ^ Pid.to_string p) :: parts
    in
    (* Add custom fields *)
    let rec add_custom acc = function
      | [] -> acc
      | (k, v) :: rest -> add_custom ((k ^ "=" ^ v) :: acc) rest
    in
    let parts = add_custom parts t.custom in
    (* Reverse to get correct order *)
    let rec rev acc = function [] -> acc | x :: xs -> rev (x :: acc) xs in
    let parts = rev [] parts in
    if parts = [] then "" else String.concat " " parts
end

(** A log event with timestamp, level, message, and metadata *)
type event = {
  timestamp : Datetime.t;
  level : level;
  message : string;
  metadata : Metadata.t;
}

(** Handler system - similar to Telemetry *)
type handler_id = string
type handler = { id : handler_id; fn : event -> unit }

open Kernel
open Collections

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

  type Message.t += Log of message
  type Message.t += HandlerList of string list
  type Message.t += Stopped

  let rec loop state =
    let selector msg = match msg with Log msg -> `select msg | _ -> `skip in
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

(** Global server PID *)
let server_pid : Pid.t option Cell.t = cell None

(** Start the log server *)
let start () =
  match !server_pid with
  | None ->
      let pid = Server.start () in
      server_pid := Some pid;
      pid
  | Some pid -> pid

(** Emit event to all handlers *)
let emit event =
  match !server_pid with
  | None -> ()
  | Some pid -> send pid Server.(Log (Emit event))

(** Attach a handler *)
let attach id fn =
  match !server_pid with
  | None -> ()
  | Some pid -> send pid Server.(Log (AttachHandler { id; fn }))

(** Detach a handler by ID *)
let detach id =
  match !server_pid with
  | None -> ()
  | Some pid -> send pid Server.(Log (DetachHandler id))

(** Detach all handlers *)
let detach_all () =
  match !server_pid with
  | None -> ()
  | Some pid -> send pid Server.(Log DetachAll)

(** List all handler IDs *)
let list_handlers () =
  match !server_pid with
  | None -> []
  | Some pid ->
      send pid Server.(Log (ListHandlers (self ())));
      let selector msg =
        match msg with Server.HandlerList ids -> `select ids | _ -> `skip
      in
      receive ~selector ()

(** Stop the log server *)
let stop () =
  match !server_pid with
  | None -> ()
  | Some pid ->
      send pid Server.(Log (Stop (self ())));
      let selector msg =
        match msg with Server.Stopped -> `select () | _ -> `skip
      in
      receive ~selector ()

(** Current log level *)
let current_level = Cell.create Info

let set_level level = current_level := level
let get_level () = !current_level
let should_log level = level_to_int level >= level_to_int !current_level

(** Core logging function *)
let log level ?(meta = Metadata.empty) message =
  if should_log level then
    let event =
      { timestamp = Datetime.now (); level; message; metadata = meta }
    in
    emit event

(** Level-specific logging functions *)
let trace ?meta msg = log Trace ?meta msg
let debug ?meta msg = log Debug ?meta msg
let info ?meta msg = log Info ?meta msg
let warn ?meta msg = log Warn ?meta msg
let error ?meta msg = log Error ?meta msg

(** Built-in handlers *)

(** Stdout handler - logs to standard output *)
module StdoutHandler = struct
  let handler_id = "log_stdout"

  let format_event event =
    let timestamp = Datetime.to_iso8601 event.timestamp in
    let level_str = level_to_string event.level in
    let meta_str = Metadata.to_string event.metadata in
    let meta_part = if meta_str = "" then "" else " [" ^ meta_str ^ "]" in
    timestamp ^ " | " ^ level_str ^ " | " ^ event.message ^ meta_part ^ "\n"

  let handler event =
    let line = format_event event in
    print line

  let attach () = attach handler_id handler
  let detach () = detach handler_id
end

(** Attach stdout handler by default *)
let () =
  let _ = start () in
  StdoutHandler.attach ()
