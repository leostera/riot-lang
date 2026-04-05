open Std

(** One exported constructor recovered from a lowered type declaration. *)
type constructor = {
  (** Stable constructor name as it will appear in the term environment. *)
  name: string;
  (** Constructor scheme derived from the declaration payload. *)
  scheme: TypeScheme.t;
}
(** Lowered semantic summary for one type declaration item.

    The current prototype only consumes enough declaration detail to surface
    constructor schemes to later term inference. The representation is kept
    explicit so future work can grow type-head and manifest support here rather
    than smuggling it through ad hoc environment entries. *)
type t = {
  (** Declared type name. *)
  type_name: string;
  (** Constructors introduced by the declaration. *)
  constructors: constructor list;
}

(** Extract constructor entries in environment form. *)
val constructor_entries: t -> (string * TypeScheme.t) list

(** Encode the lowered declaration as structured JSON for snapshots and tools. *)
val to_json: t -> Data.Json.t

(** Render the declaration as debug text. *)
val to_string: t -> string
