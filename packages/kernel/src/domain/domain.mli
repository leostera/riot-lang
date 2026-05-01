type 'value t = 'value Thread.t

val spawn: (unit -> 'value) -> 'value t

val join: 'value t -> 'value

module DLS: sig
  type 'value key = 'value Thread.DLS.key

  val new_key: ?split_from_parent:('value -> 'value) -> (unit -> 'value) -> 'value key

  val get: 'value key -> 'value

  val set: 'value key -> 'value -> unit
end
