module Attributes: sig
  type 'a key = 'a Collections.TypedKeyHashMap.key
  type binding = Collections.TypedKeyHashMap.binding =
    | Binding: 'a key * 'a -> binding
  type t

  val create: unit -> t

  val key: unit -> 'a key

  val of_list: binding list -> t

  val copy: t -> t

  val get: t -> key:'a key -> 'a option

  val insert: t -> key:'a key -> value:'a -> t

  val remove: t -> key:'a key -> 'a option

  val has_key: t -> key:'a key -> bool

  val length: t -> int

  val is_empty: t -> bool
end

type id = Uuid.t
type attribute = Attributes.binding
type attributes = Attributes.t
type status =
  | Succeeded
  | Failed of exn
type t
type lifecycle =
  | Started of t
  | Completed of {
      span: t;
      completed_at: Time.Instant.t;
      duration: Time.Duration.t;
      status: status;
    }

val set_emitter: (lifecycle -> unit) -> unit

val id: t -> id

val id_to_string: id -> string

val equal_id: id -> id -> bool

val parent_id: t -> id option

val name: t -> string

val attributes: t -> attributes

val get_attribute: t -> key:'a Attributes.key -> 'a option

val started_at: t -> Time.Instant.t

val start: ?span:t -> ?attributes:attributes -> string -> t

val finish: ?status:status -> t -> unit
