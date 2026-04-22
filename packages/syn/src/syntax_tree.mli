open Std
open Std.Collections

type token_leaf = {
  kind: Syntax_kind2.t;
  raw_lo: int;
  raw_hi: int;
  body_raw: int;
}
type missing = {
  kind: Syntax_kind2.t;
  offset: int;
}
type child =
  | Node of int
  | Token of int
  | Missing of missing
type node = {
  kind: Syntax_kind2.t;
  first_child: int;
  child_count: int;
  raw_lo: int;
  raw_hi: int;
  full_width: int;
}
type t = {
  source: IO.IoVec.IoSlice.t;
  raw_tokens: Raw_token.t Vector.t;
  significant_tokens: int Vector.t;
  tokens: token_leaf Vector.t;
  nodes: node Vector.t;
  children: child Vector.t;
  root: int;
}
type tree = t
module Builder: sig
  type t
  type marker
  type completed
  val create:
    source:IO.IoVec.IoSlice.t ->
    token_stream:Raw_token.stream ->
    ?event_capacity:int ->
    ?diagnostic_capacity:int ->
    unit ->
    t

  val start_node: t -> marker

  val complete: t -> marker -> Syntax_kind2.t -> completed

  val precede: t -> completed -> marker

  val token: t -> raw_index:int -> unit

  val missing: t -> kind:Syntax_kind2.t -> offset:int -> unit

  val error: t -> Diagnostic.t -> unit

  val length: t -> int

  val diagnostics: t -> Diagnostic.t Vector.t

  val finish: t -> tree
end

val build: source:IO.IoVec.IoSlice.t -> token_stream:Raw_token.stream -> events:Event.Buffer.t -> t

val root: t -> node

val node: t -> int -> node

val token: t -> int -> token_leaf

val child: t -> int -> child

val child_at: t -> node -> int -> child option

val for_each_child: t -> node -> fn:(child -> unit) -> unit

val token_text: t -> token_leaf -> string

val raw_range_text: t -> raw_lo:int -> raw_hi:int -> string

val node_text: t -> node -> string

val to_json: t -> Std.Data.Json.t
