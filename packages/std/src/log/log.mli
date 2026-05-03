(** Structured logging with handlers. *)
open Global

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

(** Sets the minimum log level *)
val set_level: Level.t -> unit

(** Returns the current log level *)
val get_level: unit -> Level.t

val trace: ?meta:Metadata.t -> string -> unit

val debug: ?meta:Metadata.t -> string -> unit

val info: ?meta:Metadata.t -> string -> unit

val warn: ?meta:Metadata.t -> string -> unit

val error: ?meta:Metadata.t -> string -> unit

(** Attach a custom handler *)
val attach: string -> (Event.t -> unit) -> unit

(** Detach a handler by ID *)
val detach: string -> unit

(** Detach all handlers *)
val detach_all: unit -> unit

(** List all registered handler IDs *)
val list_handlers: unit -> string list

val flush: unit -> unit

(** Wait until the stdout log handler has drained events emitted before this call. *)

(**
   Starts the logging infrastructure.

   If `RIOT_LOG` is set, its case-insensitive value configures the minimum log
   level before handlers start. Accepted values are `trace`, `debug`, `info`,
   `warn`, and `error`. Invalid values are ignored.
*)
val start_link: unit -> Pid.t

(** Get the supervisor child spec for the logging infrastructure *)
val child_spec: Supervisor.child_spec
