(** Use `available_parallelism` as Riot's concurrency budget hint for the current runtime. *)
val available_parallelism: int

(** Thread primitives for multicore runtimes. *)
type 'value t

val spawn: (unit -> 'value) -> 'value t

val join: 'value t -> 'value

(** Block the current system thread for at least the given nanoseconds. *)
val sleep_ns: int64 -> unit

module DLS : sig
  type 'value key

  val new_key: ?split_from_parent:('value -> 'value) -> (unit -> 'value) -> 'value key

  val get: 'value key -> 'value

  val set: 'value key -> 'value -> unit
end
