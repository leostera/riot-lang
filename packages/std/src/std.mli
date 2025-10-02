(** Riot's Standard library *)

module Buffer = Buffer
module Cell = Cell
module Char = Char
module Collections = Collections
module Command = Command
module Crypto = Crypto
module Data = Data
module Datetime = Datetime
module Env = Env
module Fs = Fs
module Graph = Graph
module Iterator = Iterator
module List = List
module Log = Log
module MutIterator = MutIterator
module Net = Net
module Option = Option
module Path = Path
module Result = Result
module String = String
module System = System
module Task = Task
module Time = Time
module Version = Version
module WorkerPool = Worker_pool

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)

val format : ('a, unit, string, string) format4 -> 'a
(** Format string helper - alias for Printf.sprintf *)

val print : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with immediate flush *)

val println : ('a, unit, string, unit) format4 -> 'a
(** Print to stdout with newline and immediate flush *)
