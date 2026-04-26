open Std

(** Rule definition. *)
type t
(** Syntax tree passed through rule infrastructure when needed. *)
type syntax_tree = Syn.SyntaxTree.t
(** Root Ast node used during rule traversal. *)
type syntax_root = Syn.Ast.Node.t
(** Source context made available to a rule run. *)
type context = {
  (** Path of the file being checked. *)
  file_path: string;
  (** Original source text. *)
  source: string;
  (** Parsed source file view. *)
  source_file: Syn.Ast.SourceFile.t;
}

(**
   Create a lint rule.

   Use [run] to inspect the Ast root and return diagnostics. [explain] should
   provide the longer markdown explanation shown when the rule is documented.
*)
val make:
  id:Rule_id.t ->
  description:string ->
  explain:string ->
  ?enabled:bool ->
  run:(context -> syntax_root -> Diagnostic.t list) ->
  unit ->
  t

(** Return the stable rule identifier. *)
val id: t -> Rule_id.t

(** Return the short rule description. *)
val description: t -> string

(** Return the longer explanation body as markdown text. *)
val explain: t -> string

(** Return the structured explanation record for the rule. *)
val explanation: t -> Explanation.t

(** Return `true` if the rule is enabled by default. *)
val enabled: t -> bool

(** Run the rule over a parsed source tree and return diagnostics. *)
val run: t -> context -> syntax_root -> Diagnostic.t list
