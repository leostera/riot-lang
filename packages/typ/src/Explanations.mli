open Std

(** Catalog of human-oriented explanations for prototype [typ] diagnostic ids.

    The checker itself emits structured diagnostics through {!Diagnostic}. This
    module is a separate, stable lookup table for explaining diagnostic ids on
    demand in CLI and editor-facing surfaces such as [riot check --explain]. *)
(** One explanation entry for one diagnostic id. *)
type t = {
  (** Stable diagnostic id such as ["TYP2001"]. *)
  diagnostic_id: string;
  (** Stable machine-friendly diagnostic name. *)
  name: string;
  (** Short summary of what the diagnostic means. *)
  summary: string;
  (** Longer guidance and caveats for the current prototype behavior. *)
  details: string list;
}

(** Return every explanation known by the current [typ] prototype. *)
val all: unit -> t list

(** Look up one explanation by diagnostic id.

    Matching is case-insensitive so callers can pass ["typ2001"] or
    ["TYP2001"]. *)
val explain: string -> t option

(** Encode one explanation as structured JSON. *)
val to_json: t -> Data.Json.t

(** Render one explanation as human-readable text for command-line use. *)
val format: t -> string
