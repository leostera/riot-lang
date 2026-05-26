module type S = sig
  type t
  val create: unit -> t

  val get: t -> int

  val set: t -> int -> unit
end
