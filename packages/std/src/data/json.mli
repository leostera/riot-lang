(** Simple JSON library for RPC communication *)

(** JSON value type *)
type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

(** JSON parsing errors *)
type error =
  | Unterminated_string of { position : int }
  | Invalid_literal of { expected : string; position : int; found : string }
  | Invalid_number of { position : int; text : string }
  | Expected_comma_or_bracket of {
      kind : string;
      position : int;
      found : char option;
    }
  | Expected_string_key of { position : int; found : char option }
  | Expected_colon of { position : int; found : char option }
  | Unexpected_end_of_input of { expected : string }
  | Unexpected_character of {
      position : int;
      character : char;
      expected : string;
    }
  | Extra_input_after_value of { position : int }
  | Unknown_error of string

val error_to_string : error -> string
(** Convert error to human-readable string *)

val to_string : t -> string
(** Serialization *)

val of_string : string -> (t, error) result
(** Deserialization *)

val null : t
(** Helper functions for building JSON *)

val bool : bool -> t
val int : int -> t
val float : float -> t
val string : string -> t
val array : t list -> t
val obj : (string * t) list -> t

val get_field : string -> t -> t option
(** Helper functions for extracting values *)

val get_string : t -> string option
val get_int : t -> int option
val get_bool : t -> bool option
val get_array : t -> t list option
val get_object : t -> (string * t) list option
