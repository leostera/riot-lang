open Std

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
  source: string;
  raw_tokens: Raw_token.t array;
  significant_tokens: int array;
  tokens: token_leaf array;
  nodes: node array;
  children: child array;
  root: int;
}
val build:
  source:string -> raw_tokens:Raw_token.t array -> significant_tokens:int array -> Event.t array -> t

val root: t -> node

val node: t -> int -> node

val token: t -> int -> token_leaf

val child: t -> int -> child

val children: t -> node -> child list

val token_text: t -> token_leaf -> string

val raw_range_text: t -> raw_lo:int -> raw_hi:int -> string

val node_text: t -> node -> string

val to_json: t -> Std.Data.Json.t
