open Std

(** One record label recovered from a lowered type declaration. *)
type label = {
  (** Stable descriptor identity for this label. *)
  label_id: LabelId.t;
  (** Surface label name as it appears in record syntax. *)
  name: string;
  (** Field scheme derived from the declaration. *)
  field_type: TypeScheme.t;
  (** Whether the field was declared mutable. *)
  mutable_: bool;
}

(** One exported constructor recovered from a lowered type declaration. *)
type constructor = {
  (** Stable descriptor identity for this constructor. *)
  constructor_id: ConstructorId.t;
  (** Stable constructor name as it will appear in the term environment. *)
  name: string;
  (** Constructor scheme derived from the declaration payload. *)
  scheme: TypeScheme.t;
  (**
     Whether the declaration carried an explicit result type and therefore
     behaves as a generalized constructor. 
  *)
  generalized: bool;
  (**
     Inline-record payload labels when the constructor was declared as
     [Ctor of { ... }]. 
  *)
  inline_record_labels: label list option;
}

(** Bound kind carried by lowered polymorphic-variant declarations. *)
type variance =
  | Covariant
  | Contravariant
  | Invariant

(** Flip variance when moving across an arrow parameter. *)
val flip_variance: variance -> variance

(** Join two observations of the same parameter variance. *)
val join_variance: variance -> variance -> variance

(** Compose outer and inner variances through a type constructor argument. *)
val compose_variance: variance -> variance -> variance

(** One declared parameter variance, ordered to match [param_ids]. *)
val variance_to_string: variance -> string

(** Bound kind carried by lowered polymorphic-variant declarations. *)
type poly_variant_bound =
  | Exact
  | UpperBound
  | LowerBound

(** One tag recovered from a lowered polymorphic-variant declaration. *)
type poly_variant_tag = {
  (** Surface tag name without the backtick prefix. *)
  name: string;
  (** Optional payload type carried by the tag. *)
  payload_type: TypeRepr.t option;
}

(**
   Manifest payload preserved by lowering for non-abstract declarations that do
   not elaborate into ordinary constructors or labels yet. 
*)
type manifest =
  | Alias of TypeRepr.t
  | PolyVariant of {
    bound: poly_variant_bound;
    tags: poly_variant_tag list;
    inherited: TypeRepr.t list;
  }

(**
   Lowered semantic summary for one type declaration item.

   The current prototype consumes constructor and record-label declarations
   during term inference, while preserving manifest alias and
   polymorphic-variant declaration detail explicitly for later slices. 
*)
type t = {
  (** Stable descriptor identity for this type constructor. *)
  type_constructor_id: TypeConstructorId.t;
  (** Declared type name. *)
  type_name: string;
  (** Whether lowering marked this declaration as [nonrec]. *)
  nonrec_: bool;
  (** Prototype-local type parameter identifiers used for later instantiation. *)
  param_ids: int list;
  (** Precomputed declaration variance for each parameter. *)
  param_variances: variance list;
  (** Constructors introduced by the declaration. *)
  constructors: constructor list;
  (** Record labels introduced by the declaration. *)
  labels: label list;
  (** Explicit lowered manifest payload when the declaration was not abstract. *)
  manifest: manifest option;
}

(** Extract constructor entries in environment form. *)
val constructor_entries: t -> (string * TypeScheme.t) list

(** Encode the lowered declaration as structured JSON for snapshots and tools. *)
val to_json: t -> Data.Json.t

(** Render the declaration as debug text. *)
val to_string: t -> string
