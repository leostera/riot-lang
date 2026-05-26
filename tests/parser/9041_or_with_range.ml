(* OR patterns with character ranges *)

let classify = function
  | '0' .. '9' -> "digit"
  | 'a' .. 'z' -> "lowercase"
  | 'A' .. 'Z' -> "uppercase"
  | _ -> "other"

(* OR combined with range in same pattern *)

let is_number_start = function
  | '-'
  | '+'
  | '0' .. '9' -> true
  | _ -> false

(* In constructor patterns *)

let parse_json_char = function
  | Some ('-' | '0' .. '9') -> "number"
  | Some ('"' | '\'') -> "string"
  | None -> "eof"
