(** S-expression parsing and printing library *)

(** S-expression type *)
type t = Atom of string | List of t list

exception Parse_error of string
(** Parse error exception *)

(** {1 Parsing} *)

val of_string : string -> (t, string) result
(** Parse a string into an S-expression *)

val parse_exn : string -> t
(** Parse a string, raising exception on error *)

val parse_many : string -> (t list, string) result
(** Parse multiple S-expressions from a string *)

val parse_file : string -> (t list, string) result
(** Read S-expressions from a file *)

(** {1 Printing} *)

val to_string : t -> string
(** Convert S-expression to string *)

val pretty_print : t -> string
(** Pretty print S-expression with indentation *)

val to_file : string -> t -> (unit, string) result
(** Write S-expression to a file *)

(** {1 Constructors} *)

val atom : string -> t
(** Create an atom *)

val list : t list -> t
(** Create a list *)

(** {1 Accessors} *)

val is_atom : t -> bool
(** Check if S-expression is an atom *)

val is_list : t -> bool
(** Check if S-expression is a list *)

val to_atom : t -> string option
(** Extract atom value if it's an atom *)

val to_list : t -> t list option
(** Extract list if it's a list *)

val find_atom : string -> t list -> t option
(** Find an atom by name in a nested structure *)

val assoc : string -> t list -> t option
(** Association list lookup - find value for a key in list of lists *)
