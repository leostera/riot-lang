(** # Log - Structured logging with handlers *)

open Global

module Level = Level
module Metadata = Metadata
module Event = Event
module Handler = Handler
module StdoutHandler = Stdout_handler

type level = Level.t = Trace | Debug | Info | Warn | Error

(** {1 Configuration} *)

val set_level : Level.t -> unit
(** Sets the minimum log level *)

val get_level : unit -> Level.t
(** Returns the current log level *)

(** {1 Logging Functions} *)

val trace : ?meta:Metadata.t -> string -> unit
val debug : ?meta:Metadata.t -> string -> unit
val info : ?meta:Metadata.t -> string -> unit
val warn : ?meta:Metadata.t -> string -> unit
val error : ?meta:Metadata.t -> string -> unit

(** {1 Handler Management} *)

val attach : string -> (Event.t -> unit) -> unit
(** Attach a custom handler *)

val detach : string -> unit
(** Detach a handler by ID *)

val detach_all : unit -> unit
(** Detach all handlers *)

val list_handlers : unit -> string list
(** List all registered handler IDs *)

(** {1 Supervision} *)

val start_link : unit -> Pid.t
val child_spec : Supervisor.child_spec
(** Get the supervisor child spec for the logging infrastructure *)
