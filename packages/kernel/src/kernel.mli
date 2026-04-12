(** `Kernel` is Riot's platform abstraction layer.

    The top-level module intentionally exposes just the canonical type homes and the portable
    runtime, filesystem, network, time, environment, and process boundaries that higher layers
    build on. *)
module Prelude = Prelude

include module type of Prelude

(** Use `dangerously_cast_value value` only when a separate proof already establishes the target type. *)
val dangerously_cast_value: 'original -> 'casted

(** Foundational type homes. *)
module Bool = Bool

module Sync = Sync

module Atomic = Sync.Atomic

module Char = Char

module Condition = Sync.Condition

module Int = Int

module Int32 = Int32

module Int64 = Int64

module Float = Float

module List = List

module Mutex = Sync.Mutex

module String = String

module Bytes = Bytes

module Ptr = Ptr

module Random = Random

module Array = Array

module Option = Option

module Result = Result

module Regex = Regex

(** Package-wide error surface. *)
module SystemError = System_error

module Error = Error

module Exception = Exception

(** Portable runtime and path seams. *)
module Path = Path

module Effect = Effect

module IO = Io

module Async = Async

(** OS-facing domains. *)
module Fs = Fs

module Time = Time

module Unicode = Unicode

module Net = Net

module Env = Env

module System = System

module Thread = Thread

module Process = Process
