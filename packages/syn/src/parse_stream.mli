open Std

(** Parse stream manages cursor position and error tracking *)
type t

(** Error information *)
type error = {
  message : string;
  position : int;
  expected : string list option;
}

(** Create a new parse stream from a token array *)
val create : Token.t array -> t

(** Get current position *)
val position : t -> int

(** Check if at end of stream *)
val is_empty : t -> bool

(** Peek at next token without consuming *)
val peek : t -> Token.t option

(** Peek at nth token without consuming *)
val peek_n : t -> int -> Token.t option

(** Consume and return next token *)
val next : t -> (Token.t * t) option

(** Check if next token matches predicate *)
val check : t -> (Token.t -> bool) -> bool

(** Parse a specific token, fail if not matched *)
val parse_token : t -> Token.token_kind -> (Token.t * t, error) result

(** Parse a keyword *)
val parse_keyword : t -> Token.keyword -> (Token.keyword * t, error) result

(** Parse an identifier *)
val parse_ident : t -> (string * t, error) result

(** Create a checkpoint for backtracking *)
val checkpoint : t -> t

(** Fork for trying alternatives *)
val fork : t -> t

(** Get span from start position to current position *)
val span : int -> t -> int * int

(** Add error and continue *)
val with_error : t -> error -> t

(** Get accumulated errors *)
val errors : t -> error list