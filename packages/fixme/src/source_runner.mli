open Std

(** Progress phase emitted while running rules against one source file. *)
type progress_phase =
  | Parsed of { parse_diagnostics: int }
  | AstReady
  | RuleStarted of { rule_id: Rule_id.t }
  | RuleFinished of { rule_id: Rule_id.t; diagnostics: int }
(** Timestamped progress event emitted by [run] or [run_rule]. *)
type progress_event = {
  timestamp_ms: int;
  phase: progress_phase;
}
(** Result of running rules against one source file. *)
type result = {
  (** Syntax tree produced from the parsed source. *)
  tree: Rule.syntax_tree;
  (** Diagnostics produced by all executed rules. *)
  diagnostics: Diagnostic.t list;
  (** Parse diagnostics produced before rule execution. *)
  parse_diagnostics: Syn.Diagnostic.t list;
}

(** Run a rule set against source text.

    Use [`on_progress`] when you want streaming visibility into parse and
    per-rule execution phases.
*)
val run:
  rules:Rule.t list -> ?filename:Path.t -> ?on_progress:(progress_event -> unit) -> string -> result

(** Run a single rule against source text. *)
val run_rule: rule:Rule.t -> ?filename:Path.t -> ?on_progress:(progress_event -> unit) -> string -> result

(** Return `true` if parsing produced any diagnostics. *)
val has_parse_errors: result -> bool

(** Return `true` if the result contains any error-severity diagnostics. *)
val has_errors: result -> bool

(** Return the subset of fixes considered safe to apply automatically. *)
val safe_fixes: result -> Fix.fix list

(** Return `true` if there are safe fixes that can be applied. *)
val can_apply_safe_fixes: result -> bool

(** Apply all safe fixes to the original source, if any exist.

    Returns [Ok None] when there is nothing safe to apply.
*)
val apply_safe_fixes: source:string -> result -> ((string * Fix.fix list) option, string) Result.t
