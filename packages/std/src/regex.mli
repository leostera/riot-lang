(**
   Tree-shaped regular-expression DSL layered over {!Kernel.Regex}.

   `Std.Regex` owns the syntax tree, simple optimization passes, and the
   rendering step to a concrete pattern string. Compilation and execution stay
   delegated to {!Kernel.Regex}.
*)

(** Character-class items. *)
type char_class_item =
  | Single of char
  | Range of char * char
(** Regular-expression syntax tree. *)
type t =
  | Empty
  | Start_of_text
  | End_of_text
  | Literal of string
  | Any_char
  | Char_class of {
      negated: bool;
      items: char_class_item list;
    }
  | Seq of t list
  | Alt of t list
  | Repeat of {
      expr: t;
      min: int;
      max: int option;
    }
(** A compiled regular expression. *)
type regex
(** Regex compile errors are surfaced directly from `Kernel.Regex`. *)
type compile_error = Kernel.Regex.compile_error = {
  message: string;
  offset: int option;
}
(** The first match span returned by `find`. *)
type match_ = Kernel.Regex.match_ = { start: int; stop: int }

(** Empty regex fragment. *)
val empty: t

(** Match the start of the haystack. *)
val start_of_text: t

(** Match the end of the haystack. *)
val end_of_text: t

(** Match a literal string. *)
val literal: string -> t

(** Match any single character. *)
val any_char: t

(** Match a character class. *)
val char_class: ?negated:bool -> char_class_item list -> t

(** Concatenate regex fragments. *)
val seq: t list -> t

(** Match any of the alternatives. *)
val alt: t list -> t

(** Repeat a fragment between [min] and [max] times. *)
val repeat: min:int -> ?max:int -> t -> t

(** Make a fragment optional. *)
val optional: t -> t

(** Repeat a fragment zero or more times. *)
val zero_or_more: t -> t

(** Repeat a fragment one or more times. *)
val one_or_more: t -> t

(** Flatten nested structure and remove trivial nodes. *)
val optimize: t -> t

(** Render the regex AST to a concrete pattern string. *)
val to_string: t -> string

(** Compile a regex AST through {!Kernel.Regex}. *)
val compile: t -> (regex, compile_error) Result.t

(** Compile a raw regex string through {!Kernel.Regex}. *)
val from_string: string -> (regex, compile_error) Result.t

(** Recover the compiled pattern string. *)
val source: regex -> string

(** Test whether a compiled regex matches anywhere in the haystack. *)
val is_match: regex -> string -> bool

(** Find the first match span in the haystack, if any. *)
val find: regex -> string -> match_ option
