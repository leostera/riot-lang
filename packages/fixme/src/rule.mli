open Std

(** Rule definition. *)
type t
(** Green tree passed through rule infrastructure when needed. *)
type green_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Green.node
(** Red tree used during rule traversal. *)
type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
(** Source context made available to a rule run. *)
type context = {
  (** Path of the file being checked. *)
  file_path: string;
  (** Original source text. *)
  source: string;
  (** Parsed source file CST. *)
  cst: Syn.Cst.source_file;
}

(** Create a lint rule.

    Use [run] to inspect the red tree and return diagnostics. [explain] should
    provide the longer markdown explanation shown when the rule is documented.
*)
val make:
  id:Rule_id.t ->
  description:string ->
  explain:string ->
  ?enabled:bool ->
  run:(context -> red_tree -> Diagnostic.t list) ->
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
val run: t -> context -> red_tree -> Diagnostic.t list
