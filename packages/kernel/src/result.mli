open Prelude

type ('value, 'error) t = ('value, 'error) result =
  | Ok of 'value
  | Error of 'error

(** Use `map fn value` to transform the `Ok` branch while leaving `Error` untouched. *)
val map: ('value -> 'mapped) -> ('value, 'error) t -> ('mapped, 'error) t

(** Use `map_error fn value` to transform the `Error` branch while leaving `Ok` untouched. *)
val map_error: ('error -> 'mapped_error) -> ('value, 'error) t -> ('value, 'mapped_error) t

(** Use `and_then value next` to sequence another fallible step from the `Ok` branch. *)
val and_then: ('value, 'error) t -> ('value -> ('next, 'error) t) -> ('next, 'error) t
