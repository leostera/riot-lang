open Std
open Model

(** Minimal language-level bindings visible in every typing query.

    This prelude is intentionally small. It should only contain bindings backed
    by explicit source syntax or core language forms, not ordinary module APIs
    that should instead arrive through persisted module summaries. *)
type env = (IdentPath.t * TypeScheme.t) list

(** Minimal syntax-backed intrinsic bindings used by the prototype checker. *)
val bindings: env

(** Minimal syntax-backed type declarations used by the prototype checker. *)
val type_decls: FileSummary.type_decl list
