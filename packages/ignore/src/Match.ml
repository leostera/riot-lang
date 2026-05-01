type t =
  | Ignore
  | Whitelist
  | None_

let is_ignore = fun __tmp1 ->
  match __tmp1 with
  | Ignore -> true
  | Whitelist
  | None_ -> false

let is_whitelist = fun __tmp1 ->
  match __tmp1 with
  | Whitelist -> true
  | Ignore
  | None_ -> false

let is_none = fun __tmp1 ->
  match __tmp1 with
  | None_ -> true
  | Ignore
  | Whitelist -> false

let or_else = fun left right ->
  match left with
  | None_ -> right
  | Ignore
  | Whitelist -> left
