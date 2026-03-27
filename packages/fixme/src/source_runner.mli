open Std

type result = {
  tree : Rule.green_tree;
  diagnostics : Diagnostic.t list;
  parse_diagnostics : Syn.Diagnostic.t list;
}

val run : rules:Rule.t list -> ?filename:Path.t -> string -> result
val run_rule : rule:Rule.t -> ?filename:Path.t -> string -> result
val has_parse_errors : result -> bool
val has_errors : result -> bool
val safe_fixes : result -> Fix.fix list
val can_apply_safe_fixes : result -> bool

val apply_safe_fixes :
  source:string -> result -> ((string * Fix.fix list) option, string) Result.t
