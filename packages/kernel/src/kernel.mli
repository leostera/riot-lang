(** `Kernel_new` is Riot's narrow platform layer.

    The top-level module intentionally exposes just the canonical type homes and the portable
    runtime, filesystem, network, time, environment, and process boundaries that higher layers
    build on. *)

include module type of Prelude

(** Foundational type homes. *)
module Bool = Bool

module Atomic = Atomic

module Char = Char

module Condition = Condition

module Domain = Domain

module Int = Int

module Int32 = Int32

module Int64 = Int64

module Float = Float

module List = List

module Mutex = Mutex

module String = String

module Bytes = Bytes

module Ptr = Ptr

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
