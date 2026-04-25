open Std
open Std.Data

module Env: sig
  type t
  val empty: t

  val add_path: t -> path:string list -> free_names:string list -> t

  val add_binding: t -> path:string list -> free_names:string list -> exports:t -> t

  val add_scoped_binding: t -> path:string list -> free_names:string list -> exports:t -> t

  val open_path: t -> path:string list -> t
end

type t
type parse_error =
  | Parse_diagnostics of Diagnostic.t list
val modules: t -> string list

val env: t -> Env.t

val exports: t -> Env.t

val to_json: t -> Json.t

val of_parse2_result: ?env:Env.t -> Parser2.parse_result -> (t, parse_error) result

val of_parse_result: ?env:Env.t -> Parser2.parse_result -> (t, parse_error) result
