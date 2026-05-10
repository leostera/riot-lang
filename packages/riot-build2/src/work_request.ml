open Std

type t =
  | Existing of Work_node.key
  | Materialize of Work_node.kind

let existing = fun key -> Existing key

let materialize = fun kind -> Materialize kind

let from_key = existing

let from_keys = fun keys -> List.map keys ~fn:existing

let key = fun __tmp1 ->
  match __tmp1 with
  | Existing key -> key
  | Materialize kind -> Work_node.key_from_kind kind

let kind = fun __tmp1 ->
  match __tmp1 with
  | Existing key -> Work_node.kind_from_key key
  | Materialize kind -> Some kind
