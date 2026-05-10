(**
   # Shared dotenv types

   Internal representation shared by the parser, environment applier, and
   loaders.
*)

(** A parsed dotenv assignment. *)
type binding = {
  (** Environment variable name, such as `DATABASE_URL`. *)
  key: string;
  (** Parsed value after unescaping and substitution. *)
  value: string;
  (** 1-based source line where the assignment started. *)
  line: int;
}
(** How loaders handle variables already present in the process environment. *)
type existing =
  (** Keep existing process environment values. *)
  | PreserveExisting
  (** Replace process environment values with dotenv bindings. *)
  | OverwriteExisting
(** How file-loading functions handle missing files. *)
type missing =
  (** Skip missing files. *)
  | SkipMissing
  (** Return a `ReadError` for the first missing file. *)
  | FailMissing
(** Errors returned by parsing and loading functions. *)
type error =
  (** A dotenv file could not be read. *)
  | ReadError of {
      (** Requested dotenv path. *)
      path: Std.Path.t;
      (** Human-readable IO failure. *)
      reason: string;
    }
  (** Dotenv source text could not be parsed. *)
  | ParseError of {
      (** 1-based source line where parsing failed. *)
      line: int;
      (** Human-readable parse failure. *)
      message: string;
    }

(** Convert an error into a stable human-readable message. *)
val error_to_string: error -> string
