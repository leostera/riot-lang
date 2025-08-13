(** S-expression parsing and printing library *)

(** S-expression type *)
type t =
  | Atom of string
  | List of t list

(** Parse error exception *)
exception Parse_error of string

(** {1 Parsing} *)

(** Parse a string into an S-expression *)
val of_string : string -> (t, string) result

(** Parse a string, raising exception on error *)
val parse_exn : string -> t

(** Parse multiple S-expressions from a string *)
val parse_many : string -> (t list, string) result

(** Read S-expressions from a file *)
val parse_file : string -> (t list, string) result

(** {1 Printing} *)

(** Convert S-expression to string *)
val to_string : t -> string

(** Pretty print S-expression with indentation *)
val pretty_print : t -> string

(** Write S-expression to a file *)
val to_file : string -> t -> (unit, string) result

(** {1 Constructors} *)

(** Create an atom *)
val atom : string -> t

(** Create a list *)
val list : t list -> t

(** {1 Accessors} *)

(** Check if S-expression is an atom *)
val is_atom : t -> bool

(** Check if S-expression is a list *)
val is_list : t -> bool

(** Extract atom value if it's an atom *)
val to_atom : t -> string option

(** Extract list if it's a list *)
val to_list : t -> t list option

(** Find an atom by name in a nested structure *)
val find_atom : string -> t list -> t option

(** Association list lookup - find value for a key in list of lists *)
val assoc : string -> t list -> t option