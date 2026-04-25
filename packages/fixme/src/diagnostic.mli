open Std

(** Diagnostic severity level. *)
type severity =
  | Error
  | Warning
  | Info
  | Hint

(**
   Diagnostic classification.

   [Known] is used for diagnostics backed by a rule with a stable message.
   [Generic] is the fallback for ad-hoc diagnostics.
*)
type kind =
  | Known of { rule_id: Rule_id.t; message: string }
  | Generic of { rule_id: Rule_id.t; message: string }

(** Diagnostic reported by a rule or traversal. *)
type t

(**
   Create a diagnostic.

   Use [`fix`] to attach an autofix and [`suggestion`] for a short next-step
   hint shown to the user.
*)
val make: severity:severity -> kind:kind -> span:Syn.Ceibo.Span.t -> ?suggestion:string -> ?fix:Fix.fix -> unit -> t

(** Return the diagnostic kind. *)
val kind: t -> kind

(** Return the diagnostic severity. *)
val severity: t -> severity

(** Return the diagnostic message. *)
val message: t -> string

(** Return the source span covered by the diagnostic. *)
val span: t -> Syn.Ceibo.Span.t

(** Return the rule identifier associated with the diagnostic. *)
val rule_id: t -> Rule_id.t

(** Return the optional suggestion attached to the diagnostic. *)
val suggestion: t -> string option

(** Return the optional autofix attached to the diagnostic. *)
val fix: t -> Fix.fix option
