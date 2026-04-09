type 'value t = 'value option =
  | None
  | Some of 'value
val map: ('value -> 'mapped) -> 'value t -> 'mapped t

val is_some: 'value t -> bool

val is_none: 'value t -> bool

val unwrap_or: 'value t -> default:'value -> 'value
