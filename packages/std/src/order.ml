include Kernel.Order

module type Ordered = sig
  type t

  val compare: t -> t -> Kernel.Order.t
end

let is_lt = function
  | LT -> true
  | EQ | GT -> false

let is_lte = function
  | LT | EQ -> true
  | GT -> false

let is_eq = function
  | EQ -> true
  | LT | GT -> false

let is_gte = function
  | LT -> false
  | EQ | GT -> true

let is_gt = function
  | GT -> true
  | LT | EQ -> false
