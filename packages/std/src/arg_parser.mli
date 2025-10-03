type action = Set | SetTrue | SetFalse | Append | Count
type 'a arg
type command
type matches

type error =
  | UnknownArgument of string
  | MissingRequired of string
  | InvalidValue of string * string
  | UnknownSubcommand of string
  | MissingSubcommand
  | ConflictingArguments of string * string
  | TooManyValues of string
  | TooFewValues of string

module Arg : sig
  type 'a t = 'a arg

  val flag : string -> unit t
  val option : string -> unit t
  val positional : string -> unit t
  val trailing : string -> unit t
  val short : char -> 'a t -> 'a t
  val long : string -> 'a t -> 'a t
  val help : string -> 'a t -> 'a t
  val value_name : string -> 'a t -> 'a t
  val default : string -> 'a t -> 'a t
  val required : bool -> 'a t -> 'a t
  val env : string -> 'a t -> 'a t
  val action : action -> 'a t -> 'a t
  val multiple : 'a t -> 'a t
  val count : 'a t -> 'a t
  val possible_values : string list -> 'a t -> 'a t
  val conflicts_with : string -> 'a t -> 'a t
  val requires : string -> 'a t -> 'a t
end

val command :
  ?version:string option -> ?about:string option -> string -> command

val version : string -> command -> command
val about : string -> command -> command
val author : string -> command -> command
val arg : unit arg -> command -> command
val subcommand : command -> command -> command
val get_matches : command -> string list -> (matches, error) result
val get_one : matches -> string -> string option
val get_flag : matches -> string -> bool
val get_count : matches -> string -> int
val get_many : matches -> string -> string list
val get_int : matches -> string -> int option
val get_float : matches -> string -> float option
val get_path : matches -> string -> Path.t option
val subcommand : matches -> (string * matches) option
val subcommand_name : matches -> string option
val subcommand_matches : matches -> string -> matches option
val error_message : error -> string
val print_error : error -> unit
