open Std
open Std.Collections

(**
   Parser events.

   Events are the append-only grammar stream used by tests/tools that want to
   inspect parser output before tree construction. The production parser writes
   directly into `Syntax_tree.Builder` to avoid allocating a separate event
   buffer, but these types remain the explicit contract for the event form.
*)
type t =
  | StartNode of Syntax_kind.t option
  | FinishNode
  | Token of int
  | Missing of Syntax_kind.t * int
  | Error of Diagnostic.t
type event = t

(**
   Growable event buffer with the same marker/precede discipline as the tree
   builder.
*)
module Buffer: sig
  type t
  type marker
  type completed

  val create: ?event_capacity:int -> ?diagnostic_capacity:int -> unit -> t

  val start_node: t -> marker

  val complete: t -> marker -> Syntax_kind.t -> completed

  val precede: t -> completed -> marker

  val token: t -> raw_index:int -> unit

  val missing: t -> kind:Syntax_kind.t -> offset:int -> unit

  val error: t -> Diagnostic.t -> unit

  val length: t -> int

  val get_unchecked: t -> at:int -> event

  val iter: t -> event Iter.Iterator.t

  val diagnostics: t -> Diagnostic.t Vector.t
end
