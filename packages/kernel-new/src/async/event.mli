module type Intf = sig
  type t
  val is_error: t -> bool

  val is_priority: t -> bool

  val is_read_closed: t -> bool

  val is_readable: t -> bool

  val is_writable: t -> bool

  val is_write_closed: t -> bool

  val token: t -> Token.t
end

type t
val make: (module Intf with type t = 'state) -> 'state -> t

val token: t -> Token.t

val is_error: t -> bool

val is_priority: t -> bool

val is_read_closed: t -> bool

val is_readable: t -> bool

val is_writable: t -> bool

val is_write_closed: t -> bool
