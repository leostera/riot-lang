include module type of Queue_core

module MPSC: sig
  type 'value t
  val create: unit -> 'value t

  val with_capacity: size:int -> 'value t

  val from_list: 'value list -> 'value t

  val push: 'value t -> value:'value -> unit

  val pop: 'value t -> 'value option

  val length: 'value t -> int

  val is_empty: 'value t -> bool

  val clear: 'value t -> unit
end

module SPMC: sig
  type 'value t
  val create: unit -> 'value t

  val with_capacity: size:int -> 'value t

  val from_list: 'value list -> 'value t

  val push: 'value t -> value:'value -> unit

  val pop: 'value t -> 'value option

  val length: 'value t -> int

  val is_empty: 'value t -> bool

  val clear: 'value t -> unit
end

module MPMC: sig
  type 'value t
  val create: unit -> 'value t

  val with_capacity: size:int -> 'value t

  val from_list: 'value list -> 'value t

  val push: 'value t -> value:'value -> unit

  val pop: 'value t -> 'value option

  val length: 'value t -> int

  val is_empty: 'value t -> bool

  val clear: 'value t -> unit
end
