open Std

type t

type result = Fixme.Source_runner.result = {
  tree: Rule.syntax_tree;
  diagnostics: Diagnostic.t list;
  parse_diagnostics: Syn.Diagnostic.t list;
}

val make: rules:Rule.t list -> unit -> t

val run: t -> ?filename:Path.t -> ?on_progress:(Fixme.Source_runner.progress_event -> unit) -> string -> result

val builtin_rules: unit -> Rule.t list

val builtin_rule_category: Rule_id.t -> string option

val default_rules: unit -> Rule.t list

val default_rule_ids: unit -> Rule_id.t list

val rules_by_id: Rule_id.t list -> Rule.t list

val default: unit -> t
