open Global
open Sync
open Sync.Cell

module Level = Level
module Metadata = Metadata
module Event = Event
module Handler = Handler
module StdoutHandler = Stdout_handler

type level = Level.t =
  | Trace
  | Debug
  | Info
  | Warn
  | Error

(** Current log level *)
let current_level = Cell.create Level.Info

let set_level = fun level -> current_level := level

let get_level = fun () -> !current_level

let level_of_string = fun value ->
  match String.lowercase_ascii (String.trim value) with
  | "trace" -> Some Level.Trace
  | "debug" -> Some Level.Debug
  | "info" -> Some Level.Info
  | "warn" -> Some Level.Warn
  | "error" -> Some Level.Error
  | _ -> None

let configure_from_env = fun () ->
  match Env.get Env.String ~var:"RIOT_LOG" with
  | Some value -> (
      match level_of_string value with
      | Some level -> set_level level
      | None -> ()
    )
  | None -> ()

let should_log = fun level -> Level.to_int level >= Level.to_int !current_level

(** Core logging function *)
let log = fun level ?(meta = Metadata.empty) message ->
  if should_log level then
    let event = Event.make ~level ~message ~metadata:meta () in
    Handler.emit event

(** Level-specific logging functions *)
let trace = fun ?meta msg -> log Level.Trace ?meta msg

let debug = fun ?meta msg -> log Level.Debug ?meta msg

let info = fun ?meta msg -> log Level.Info ?meta msg

let warn = fun ?meta msg -> log Level.Warn ?meta msg

let error = fun ?meta msg -> log Level.Error ?meta msg

(** Handler management *)
let attach = Handler.attach

let detach = Handler.detach

let detach_all = Handler.detach_all

let list_handlers = Handler.list

let flush = StdoutHandler.flush

let start_link = fun () ->
  configure_from_env ();
  let sup =
    Supervisor.start_link ~strategy:OneForOne ~children:[ StdoutHandler.child_spec () ] ()
  in
  Supervisor.to_pid sup

(** Supervised logging infrastructure *)
let child_spec =
  Supervisor.child_spec
    ~id:"log_supervisor"
    ~start:start_link
    ~child_type:Supervisor
    ~shutdown:Infinity
    ()
