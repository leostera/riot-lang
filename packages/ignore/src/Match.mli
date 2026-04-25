(** Outcome of evaluating ignore rules for one path. *)
type t =
  | Ignore
  | Whitelist
  | None_

(**
   Return `true` when the path should be skipped.

   Example:
   ```ocaml
   Match.is_ignore Match.Ignore = true;
   Match.is_ignore Match.None_ = false
   ```
*)
val is_ignore: t -> bool

(**
   Return `true` when the path was explicitly re-included by an allow rule.

   Use this to distinguish "matched an allow rule" from "no rule matched".
*)
val is_whitelist: t -> bool

(** Return `true` when no rule matched the path. *)
val is_none: t -> bool

(**
   Combine two match outcomes, preferring the left-hand side when it already
   made a decision.

   Example:
   ```ocaml
   Match.or_else Match.Ignore Match.Whitelist = Match.Ignore;
   Match.or_else Match.None_ Match.Whitelist = Match.Whitelist
   ```
*)
val or_else: t -> t -> t
