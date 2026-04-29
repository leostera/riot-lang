(** Interface rendering helpers for the `Typ.Infer` path. *)
open Std

(**
   Render exported type declarations and values as an inferred interface.

   Types are currently rendered before values. The individual type and value
   streams are expected to be source-ordered by the caller.
*)
val from_exports:
  types:(Ast.ident * Ast.type_declaration) Iter.Iterator.t ->
  values:(Ast.ident * Ast.Type.t) Iter.Iterator.t ->
  string

(**
   Render exported values as an inferred interface.

   The caller owns the ordering of the iterator, so environments that need
   source-order output should provide a source-ordered stream.
*)
val from_values: (Ast.ident * Ast.Type.t) Iter.Iterator.t -> string
