type ('value, 'error) t =
  | Ok of 'value
  | Error of 'error
val map: ('value -> 'mapped) -> ('value, 'error) t -> ('mapped, 'error) t

val map_error: ('error -> 'mapped_error) -> ('value, 'error) t -> ('value, 'mapped_error) t

val and_then: ('value, 'error) t -> ('value -> ('next, 'error) t) -> ('next, 'error) t
