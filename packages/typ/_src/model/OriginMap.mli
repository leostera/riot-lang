open Std

(** Source origins for one semantic snapshot. *)
type semantic_id =
  (** Top-level item semantic identity. *)
  | Item of ItemArenaId.t
  (** Lowered binding semantic identity. *)
  | Binding of BindingArenaId.t
  (** Lowered expression semantic identity. *)
  | Expr of ExprArenaId.t
  (** Lowered pattern semantic identity. *)
  | Pattern of PatternArenaId.t

type kind =
  (** Origin attached to one top-level item. *)
  | ItemKind
  (** Origin attached to one lowered binding. *)
  | BindingKind
  (** Origin attached to one lowered expression. *)
  | ExprKind
  (** Origin attached to one lowered pattern. *)
  | PatternKind

type origin = {
  (** Snapshot-local origin identifier. *)
  origin_id: OriginId.t;
  (** Logical source that owns this origin. *)
  source_id: SourceId.t;
  (** Source revision used to compute this origin. *)
  source_revision: int;
  (** Semantic node mapped by this origin. *)
  semantic_id: semantic_id;
  (** Lowering-stage label describing how the node was introduced. *)
  label: string;
  (** Syntax kind of the source node that produced this origin. *)
  syntax_kind: Syn.SyntaxKind.t;
  (** Primary source span for this semantic node. *)
  span: Syn.Ceibo.Span.t;
}

(** Snapshot-local origin table. *)
type t

(** Empty origin map. *)
val empty: t

(** Build an origin map from a fully prepared list. *)
val of_list: origin list -> t

(** Enumerate every stored origin in insertion order. *)
val origins: t -> origin list

(** Classify one semantic identity by its origin kind. *)
val kind_of_semantic_id: semantic_id -> kind

(** Find one origin by [OriginId]. *)
val find: t -> OriginId.t -> origin option

(** Find one origin by the semantic node it maps. *)
val find_by_semantic_id: t -> semantic_id -> origin option

(** Convenience lookup for item origins. *)
val find_item: t -> ItemArenaId.t -> origin option

(** Convenience lookup for binding origins. *)
val find_binding: t -> BindingArenaId.t -> origin option

(** Convenience lookup for expression origins. *)
val find_expr: t -> ExprArenaId.t -> origin option

(** Convenience lookup for pattern origins. *)
val find_pattern: t -> PatternArenaId.t -> origin option

(** Encode all origins as structured JSON for snapshot tests and tooling. *)
val to_json: t -> Data.Json.t

(** Render the origin map as debug text. *)
val to_string: t -> string
