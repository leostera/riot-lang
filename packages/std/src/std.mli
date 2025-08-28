(** Riot's Standard library *)

module Buffer = Buffer
module Command = Command
module Data = Data
module Datetime = Datetime
module Env = Env
module File = File
module Fs = Fs
module List = List
module Net = Net
module Option = Option
module Path = Path
module Result = Result
(* module String = String (* TODO: fix iterator dependencies *) *)

val available_parallelism : unit -> int
(** Get the number of available CPU cores for parallelism *)

(* FIXME: this should be removed and use `availbale_parallelism` instaed *)
val cpu_count : unit -> int
(** Get the number of CPU cores (alias for available_parallelism) *)

(* FIXME: this should return a variant *)
val os_type : unit -> string
(** Get the OS type *)

(** FIXME: this should be removed in favor of Datetime.now () *)
val time : unit -> float
(** Get current time as float *)

(** FIXME: this should be removed in favor of Datetime.now () *)
val gettimeofday : unit -> float
(** Get current time with microsecond precision *)

(** FIXME: this should be removed in favor of Datetime.now () *)
val time_ms : unit -> int
(** Get current time in milliseconds *)

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)
