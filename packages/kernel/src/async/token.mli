type t
val make: 'value -> t

(** Recover the value stored in a token when the caller already owns the registration site and
    therefore knows the token's payload type. *)
val unsafe_value: t -> 'value

(** Use `id token` when you need a stable process-local identity for maps, sets, or debugging. *)
val id: t -> int

val hash: t -> int

val equal: t -> t -> bool
