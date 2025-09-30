(** Riot's Standard library *)

module Buffer = Buffer
module Cell = Cell
module Collections = Collections
module Command = Command
module Crypto = Crypto
module Data = Data
module Datetime = Datetime
module Env = Env
module Fs = Fs
module List = List
module Net = Net
module Option = Option
module Path = Path
module Result = Result
module Time = Time
module Version = Version
module Iterator = Iterator
module MutIterator = MutIterator
module Graph = Graph
(* module String = String (* TODO: fix iterator dependencies *) *)

val available_parallelism : unit -> int
(** Get the number of available CPU cores for parallelism *)

(* FIXME: this should return a variant *)
val os_type : unit -> string
(** Get the OS type *)

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)
