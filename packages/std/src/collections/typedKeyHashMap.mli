type 'a key
type any_key =
  | Key: 'a key -> any_key
type binding =
  | Binding: 'a key * 'a -> binding
type t
type 'value entry =
  | Occupied of 'value
  | Vacant

val create: unit -> t

val with_capacity: size:int -> t

val key: unit -> 'a key

val equal_key: 'a key -> 'b key -> bool

val from_list: binding list -> t

val insert: t -> key:'a key -> value:'a -> 'a option

val get: t -> key:'a key -> 'a option

val remove: t -> key:'a key -> 'a option

val has_key: t -> key:'a key -> bool

val length: t -> int

val is_empty: t -> bool

val clear: t -> unit

val keys: t -> any_key list

val values: t -> binding list

val for_each: t -> fn:(any_key -> binding -> unit) -> unit

val fold_left: t -> init:'acc -> fn:('acc -> any_key -> binding -> 'acc) -> 'acc

val to_list: t -> (any_key * binding) list

val entry: t -> key:'a key -> 'a entry

val iter: t -> (any_key * binding) Iter.Iterator.t

val mut_iter: t -> (any_key * binding) Iter.MutIterator.t
