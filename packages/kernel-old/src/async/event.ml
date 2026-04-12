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

type t =
  E: (module Intf with type t = 'state) * 'state -> t

let make = fun m e -> E (m, e)

let token = fun (E ((module Ev), state)) -> Ev.token state

let is_readable = fun (E ((module Ev), state)) -> Ev.is_readable state

let is_writable = fun (E ((module Ev), state)) -> Ev.is_writable state

let is_error = fun (E ((module Ev), state)) -> Ev.is_error state

let is_read_closed = fun (E ((module Ev), state)) -> Ev.is_read_closed state

let is_write_closed = fun (E ((module Ev), state)) -> Ev.is_write_closed state

let is_priority = fun (E ((module Ev), state)) -> Ev.is_priority state
