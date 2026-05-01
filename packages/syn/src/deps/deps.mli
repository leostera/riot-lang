open Std
open Std.Data

(**
   Syntactic module dependency extraction.

   `Deps` walks the typed Ast views, records free module names used by a source
   file, and tracks exported module aliases so downstream build planning can
   resolve implicit opens and generated alias modules. It is syntax-only:
   unresolved or ambiguous names are reported as conservative module roots.
*)
module Env: sig
  type t

  val empty: t

  (** Add an exported path and the free module names needed to reference it. *)
  val add_path: t -> path:string list -> free_names:string list -> t

  (** Add a binding with nested exports, for example a module alias. *)
  val add_binding: t -> path:string list -> free_names:string list -> exports:t -> t

  (** Add a binding that becomes visible only after `open_path`. *)
  val add_scoped_binding: t -> path:string list -> free_names:string list -> exports:t -> t

  (** Bring a scoped binding/export path into the visible environment. *)
  val open_path: t -> path:string list -> t
end

type t
type parse_error =
  | Parse_diagnostics of Diagnostic.t list

(** Sorted module roots required by the parsed source file. *)
val modules: t -> string list

(** Environment after processing the source file. *)
val env: t -> Env.t

(** Public exports discovered in the source file. *)
val exports: t -> Env.t

val to_json: t -> Json.t

(**
   Extract dependencies from a clean parse result. Diagnostics are returned as
   an error because dependency output from malformed syntax is not stable.
*)
val from_parse_result: ?env:Env.t -> Parser.parse_result -> (t, parse_error) result
