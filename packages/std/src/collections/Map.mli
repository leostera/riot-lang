module type S = sig
  type key

  type +'value t

  val empty: 'value t

  val is_empty: 'value t -> bool

  val is_singleton: 'value t -> bool

  val singleton: key:key -> value:'value -> 'value t

  val insert: 'value t -> key:key -> value:'value -> 'value t

  val insert_to_list: 'value list t -> key:key -> value:'value -> 'value list t

  val update: 'value t -> key:key -> fn:('value option -> 'value option) -> 'value t

  val remove: 'value t -> key:key -> 'value t

  val merge: left:'left t -> right:'right t -> fn:(key:key -> left:'left option -> right:'right option -> 'merged option) -> 'merged t

  val union: left:'value t -> right:'value t -> fn:(key:key -> left:'value -> right:'value -> 'value option) -> 'value t

  val length: 'value t -> int

  val to_list: 'value t -> (key * 'value) list

  val from_list: (key * 'value) list -> 'value t

  val minimum: 'value t -> (key * 'value) option

  val minimum_unchecked: 'value t -> key * 'value

  val maximum: 'value t -> (key * 'value) option

  val maximum_unchecked: 'value t -> key * 'value

  val choose: 'value t -> (key * 'value) option

  val choose_unchecked: 'value t -> key * 'value

  val get: 'value t -> key:key -> 'value option

  val get_unchecked: 'value t -> key:key -> 'value

  val get_first: 'value t -> fn:(key -> bool) -> (key * 'value) option

  val get_first_unchecked: 'value t -> fn:(key -> bool) -> key * 'value

  val get_last: 'value t -> fn:(key -> bool) -> (key * 'value) option

  val get_last_unchecked: 'value t -> fn:(key -> bool) -> key * 'value

  val has_key: 'value t -> key:key -> bool

  val for_each: 'value t -> fn:(key -> 'value -> unit) -> unit

  val fold_left: 'value t -> init:'acc -> fn:('acc -> key -> 'value -> 'acc) -> 'acc

  val map: 'value t -> fn:('value -> 'mapped) -> 'mapped t

  val map_with_key: 'value t -> fn:(key -> 'value -> 'mapped) -> 'mapped t

  val filter: 'value t -> fn:(key -> 'value -> bool) -> 'value t

  val filter_map: 'value t -> fn:(key -> 'value -> 'mapped option) -> 'mapped t

  val partition: 'value t -> fn:(key -> 'value -> bool) -> 'value t * 'value t

  val split: 'value t -> key:key -> 'value t * 'value option * 'value t

  val equal: left:'value t -> right:'value t -> fn:('value -> 'value -> bool) -> bool

  val compare: left:'value t -> right:'value t -> fn:('value -> 'value -> Order.t) -> Order.t

  val all: 'value t -> fn:(key -> 'value -> bool) -> bool

  val any: 'value t -> fn:(key -> 'value -> bool) -> bool
end

module Make (Order: Order.Ordered): S with type key = Order.t
