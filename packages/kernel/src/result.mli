open Prelude

type ('value, 'error) t = ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

(** Use `map fn value` to transform the `Ok` branch while leaving `Error` untouched. *)
val map: ('value, 'error) t -> fn:('value -> 'mapped) -> ('mapped, 'error) t

(** Use `map_error fn value` to transform the `Error` branch while leaving `Ok` untouched. *)
val map_err: ('value, 'error) t -> fn:('error -> 'mapped_error) -> ('value, 'mapped_error) t

(** Use `and_then value next` to sequence another fallible step from the `Ok` branch. *)
val and_then: ('value, 'error) t -> fn:('value -> ('next, 'error) t) -> ('next, 'error) t
