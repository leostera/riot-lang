open Std

(** Result of running one or more rules against a source fixture. *)
type result = {
  (** Initial run before applying any fixes. *)
  initial: Source_runner.result;
  (** Source after applying fixes, if any fix was applied successfully. *)
  fixed_source: string option;
  (** Fixes produced during the initial run. *)
  applied_fixes: Fix.fix list;
  (** Result after applying fixes and re-running the rules, if available. *)
  after: Source_runner.result option;
}

(** Run a rule set against source text and capture before/after results.

    Use this in rule tests when you want to assert both diagnostics and the
    effect of any produced fixes.
*)
val run: rules:Rule.t list -> ?filename:Path.t -> string -> (result, string) Result.t

(** Run a single rule against source text.

    This is a convenience wrapper around [run] for one-rule tests.
*)
val run_rule: rule:Rule.t -> ?filename:Path.t -> string -> (result, string) Result.t
