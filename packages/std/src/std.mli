(** Standard library extensions and utilities *)

module Buffer = Buffer
module Command = Command
module Env = Env
module Fs = Fs
module List = List
module Option = Option
module Path = Path
module Result = Result
module String = String

val available_parallelism : unit -> int
(** Get the number of available CPU cores for parallelism *)

val cpu_count : unit -> int
(** Get the number of CPU cores (alias for available_parallelism) *)

val os_type : unit -> string
(** Get the OS type *)

val time : unit -> float
(** Get current time as float *)

val gettimeofday : unit -> float
(** Get current time with microsecond precision *)

val time_ms : unit -> int
(** Get current time in milliseconds *)

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

module Datetime = Global.Datetime
module Process = Global.Process
module File = Global.File
