type ('key, 'value) t

val create: unit -> ('key, 'value) t

val with_capacity: size:int -> ('key, 'value) t

val insert: ('key, 'value) t -> key:'key -> value:'value -> 'value option

val get: ('key, 'value) t -> key:'key -> 'value option

val remove: ('key, 'value) t -> key:'key -> 'value option

val for_each: ('key, 'value) t -> fn:('key -> 'value -> unit) -> unit

val length: ('key, 'value) t -> int
