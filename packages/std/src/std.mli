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
module Graph = Graph
module Iterator = Iterator
module List = List
module MutIterator = MutIterator
module Net = Net
module Option = Option
module Path = Path
module Result = Result
module System = System
module Task = Task
module Time = Time
module Version = Version
module Worker_pool = Worker_pool

val panic : string -> 'a
(** Panic with a message - raises an uncatchable exception *)

val cell : 'a -> 'a Cell.t
(** Create a mutable cell with the given value *)
