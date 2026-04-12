type 'value t = 'value option =
  | None
  | Some of 'value

(** Use `map fn value` to transform the `Some` branch while leaving `None` untouched. *)
val map: ('value -> 'mapped) -> 'value t -> 'mapped t

(** Use `is_some value` to check whether `value` carries a payload. *)
val is_some: 'value t -> bool

(** Use `is_none value` to check whether `value` is empty. *)
val is_none: 'value t -> bool

(** Use `unwrap_or value ~default` to recover the payload or fall back to `default`. *)
val unwrap_or: 'value t -> default:'value -> 'value
