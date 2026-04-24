module type Ordered = sig
  type t
  val compare: t -> t -> Kernel.Order.t
end

type t = Kernel.Order.t =
  | LT
  | EQ
  | GT
val compare: 'value -> 'value -> t
