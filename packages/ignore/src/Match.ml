type t =
  | Ignore
  | Whitelist
  | None_

let is_ignore = function
  | Ignore -> true
  | Whitelist | None_ -> false

let is_whitelist = function
  | Whitelist -> true
  | Ignore | None_ -> false

let is_none = function
  | None_ -> true
  | Ignore | Whitelist -> false

let or_else = fun left right ->
  match left with
  | None_ -> right
  | Ignore | Whitelist -> left
