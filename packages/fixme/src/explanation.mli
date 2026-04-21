open Std

(** Expanded guidance for a lint or fix rule. *)
type t = {
  (** Rule identifier this explanation belongs to. *)
  rule_id: Rule_id.t;
  (** Longer explanation body, usually rendered as markdown. *)
  body: string;
  (** Short summary message shown alongside the rule. *)
  message: string;
}

(** Format an explanation as user-facing text.

    Use this when showing rule help in a terminal or structured report.
*)
val format: t -> string
