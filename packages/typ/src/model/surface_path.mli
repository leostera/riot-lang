(**
   Source-level names and dotted paths.

   A `Surface_path.t` is the path shape the source program wrote or the checker
   wants to print back to a user: `x`, `Result.t`, `A.B.value`, and so on.

   Key properties:

   - It is about **surface spelling**, not binding resolution.
   - Two paths with the same text can resolve to different bindings in
     different environments.
   - The only construction path is `from_syn_ident`, so source-originated names
     stay tied to Syn's structured identifier view instead of being rebuilt from
     strings or raw token lists.
*)
type t

(**
   `from_syn_ident ident` builds a surface path from Syn's structured
   identifier view.

   This preserves Syn's identifier segments directly. It does **not** render the
   identifier to text and parse it again.
*)
val from_syn_ident: Syn.Ast.Ident.t -> t

(**
   `to_segments path` returns the path segments from left to right.
*)
val to_segments: t -> string list

(**
   `to_string path` renders the path by joining segments with dots.
*)
val to_string: t -> string

(** Structural equality over path segments. *)
val equal: t -> t -> bool

(**
   Structural ordering over path segments, suitable for deterministic maps,
   sets, and snapshots.
*)
val compare: t -> t -> Std.Order.t

(** Serializer for persisting surface paths in checker summaries and snapshots. *)
val serializer: t Serde.Ser.t
