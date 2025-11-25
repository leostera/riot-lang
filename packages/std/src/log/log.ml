open Global
open Sync
open Sync.Cell

module Level = Level
module Metadata = Metadata
module Event = Event
module Handler = Handler
module StdoutHandler = Stdout_handler

type level = Level.t = Trace | Debug | Info | Warn | Error

(** Current log level *)
let current_level = Cell.create Level.Info

let set_level level = current_level := level
let get_level () = !current_level

let should_log level =
  Level.to_int level >= Level.to_int !current_level

(** Core logging function *)
let log level ?(meta = Metadata.empty) message =
  if should_log level then
    let event = Event.make ~level ~message ~metadata:meta () in
    Handler.emit event

(** Level-specific logging functions *)
let trace ?meta msg = log Level.Trace ?meta msg
let debug ?meta msg = log Level.Debug ?meta msg
let info ?meta msg = log Level.Info ?meta msg
let warn ?meta msg = log Level.Warn ?meta msg
let error ?meta msg = log Level.Error ?meta msg

(** Handler management *)
let attach = Handler.attach
let detach = Handler.detach
let detach_all = Handler.detach_all
let list_handlers = Handler.list

let start_link () =
  let sup =
    Supervisor.start_link ~strategy:OneForOne
      ~children:[ StdoutHandler.child_spec () ]
      ()
  in
  Supervisor.to_pid sup

(** Supervised logging infrastructure *)
let child_spec =
  Supervisor.child_spec ~id:"log_supervisor"
    ~start:start_link
    ~child_type:Supervisor ~shutdown:Infinity ()
