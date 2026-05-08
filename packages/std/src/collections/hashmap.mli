type ('key, 'value) t
type ('key, 'value) entry =
  | Occupied of 'value
  | Vacant
type ('value, 'result) operation =
  | Insert of 'value * 'result
  | Remove of 'result
  | Abort of 'result

val create: unit -> ('key, 'value) t

val with_capacity: size:int -> ('key, 'value) t

val from_list: ('key * 'value) list -> ('key, 'value) t

val bucket_count: ('key, 'value) t -> int

val insert: ('key, 'value) t -> key:'key -> value:'value -> 'value option

val get: ('key, 'value) t -> key:'key -> 'value option

val remove: ('key, 'value) t -> key:'key -> 'value option

val has_key: ('key, 'value) t -> key:'key -> bool

val length: ('key, 'value) t -> int

val is_empty: ('key, 'value) t -> bool

val clear: ('key, 'value) t -> unit

val keys: ('key, 'value) t -> 'key list

val values: ('key, 'value) t -> 'value list

val for_each: ('key, 'value) t -> fn:('key -> 'value -> unit) -> unit

val fold_left: ('key, 'value) t -> init:'acc -> fn:('acc -> 'key -> 'value -> 'acc) -> 'acc

val to_list: ('key, 'value) t -> ('key * 'value) list

val entry: ('key, 'value) t -> key:'key -> ('key, 'value) entry

val compute:
  ('key, 'value) t ->
  key:'key ->
  fn:('value option -> ('value, 'result) operation) ->
  'result

val iter: ('key, 'value) t -> ('key * 'value) Iter.Iterator.t

val mut_iter: ('key, 'value) t -> ('key * 'value) Iter.MutIterator.t
