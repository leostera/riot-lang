(** `Kernel_new` is Riot's narrow platform layer.

    The top-level module intentionally exposes just the canonical type homes and the portable
    runtime, filesystem, network, time, environment, and process boundaries that higher layers
    build on. *)

(** Foundational type homes. *)
module Bool = Bool

module Char = Char

module Int = Int

module Int32 = Int32

module Int64 = Int64

module Float = Float

module String = String

module Bytes = Bytes

module Array = Array

module Option = Option

module Result = Result

(** Package-wide error surface. *)
module SystemError = System_error

module Error = Error

(** Portable runtime and path seams. *)
module Path = Path

module Effect = Effect

module IO = Io

module Async = Async

(** OS-facing domains. *)
module Fs = Fs

module Time = Time

module Net = Net

module Env = Env

module Process = Process
