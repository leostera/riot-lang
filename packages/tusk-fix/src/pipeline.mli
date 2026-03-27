open Std

type t
type result = Tusk_fix_api.Source_runner.result = {
  tree : Rule.green_tree;
  diagnostics : Diagnostic.t list;
  parse_diagnostics : Syn.Diagnostic.t list;
}

val make : rules:Rule.t list -> unit -> t
val run : t -> ?filename:Path.t -> string -> result
val builtin_rules : unit -> Rule.t list
val builtin_rule_category : string -> string option
val default_rules : unit -> Rule.t list
val default_rule_ids : unit -> string list
val rules_by_id : string list -> Rule.t list
val default : unit -> t
