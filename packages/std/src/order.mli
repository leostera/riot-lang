module type Ordered = sig
  type t

  val compare: t -> t -> Kernel.Order.t
end

type t = Kernel.Order.t =
  | LT
  | EQ
  | GT

val compare: 'value -> 'value -> t

val is_lt: t -> bool

val is_lte: t -> bool

val is_eq: t -> bool

val is_gte: t -> bool

val is_gt: t -> bool
