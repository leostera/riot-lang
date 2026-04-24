include Kernel.Order

module type Ordered = sig
  type t
  val compare: t -> t -> Kernel.Order.t
end
