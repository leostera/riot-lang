open Std
open Std.Collections

type t =
  | StartNode of Syntax_kind2.t option
  | FinishNode
  | Token of int
  | Missing of Syntax_kind2.t * int
  | Error of Diagnostic.t

type event = t

module Buffer: sig
  type t
  type marker
  type completed

  val create: ?event_capacity:int -> ?diagnostic_capacity:int -> unit -> t

  val start_node: t -> marker

  val complete: t -> marker -> Syntax_kind2.t -> completed

  val precede: t -> completed -> marker

  val token: t -> raw_index:int -> unit

  val missing: t -> kind:Syntax_kind2.t -> offset:int -> unit

  val error: t -> Diagnostic.t -> unit

  val length: t -> int

  val get_unchecked: t -> at:int -> event

  val iter: t -> event Iter.Iterator.t

  val diagnostics: t -> Diagnostic.t Vector.t
end
