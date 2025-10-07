(** Poneglyph - In-memory EAV graph store for build metadata *)

type t
(** Graph store instance *)

(** {1 Initialization} *)

val create : unit -> t
(** Create new empty graph store *)

(** {1 URIs} *)

module Uri : sig
  type t
  (** Opaque URI - automatically interned *)

  val of_string : string -> t
  (** Create URI from string. Same string always returns same URI (interned). *)

  val to_string : t -> string
  (** Convert URI back to string *)

  val equal : t -> t -> bool
  (** Fast URI equality (integer comparison) *)

  val compare : t -> t -> int
  (** Fast URI comparison (integer comparison) *)
end

(** {1 Values} *)

module Value : sig
  type t =
    | String of string
    | Int of int
    | Bool of bool
    | Float of float
    | Uri of Uri.t
    | DateTime of Datetime.t
    | List of t list

  val to_string : t -> string
  (** Human-readable representation *)
end

(** {1 Facts} *)

module Fact : sig
  type t = {
    entity : Uri.t;
    attribute : Uri.t;
    value : Value.t;
  }

  val fact : Uri.t -> Uri.t -> Value.t -> t
  (** Create a fact: fact entity attribute value *)

  val ( let+ ) : Uri.t -> Uri.t * Value.t -> t
  (** Infix: let+ entity = (attribute, value) *)
end

(** {1 State Management} *)

val state : t -> Fact.t list -> unit
(** Assert facts into the graph. Replaces existing values for same entity+attribute. *)

(** {1 Queries} *)

val get : t -> entity:Uri.t -> attr:Uri.t -> Value.t option
(** Get value for entity+attribute *)

val exists : t -> Uri.t -> bool
(** Check if entity has any facts *)
