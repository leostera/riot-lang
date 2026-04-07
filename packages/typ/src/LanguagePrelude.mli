open Std
open Model

(** Minimal language-level bindings visible in every typing query.

    This prelude is intentionally small. It should only contain bindings that
    behave like intrinsic or pervasive language constructs for the current
    prototype, not ordinary module APIs that should instead arrive through
    persisted module summaries. *)
type env = (IdentPath.t * TypeScheme.t) list

(** Minimal intrinsic bindings used by the prototype checker. *)
val bindings: env

(** Minimal intrinsic type declarations used by the prototype checker. *)
val type_decls: FileSummary.type_decl list
