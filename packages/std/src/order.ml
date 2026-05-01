include Kernel.Order

module type Ordered = sig
  type t

  val compare: t -> t -> Kernel.Order.t
end

let is_lt = fun __tmp1 ->
  match __tmp1 with
  | LT -> true
  | EQ
  | GT -> false

let is_lte = fun __tmp1 ->
  match __tmp1 with
  | LT
  | EQ -> true
  | GT -> false

let is_eq = fun __tmp1 ->
  match __tmp1 with
  | EQ -> true
  | LT
  | GT -> false

let is_gte = fun __tmp1 ->
  match __tmp1 with
  | LT -> false
  | EQ
  | GT -> true

let is_gt = fun __tmp1 ->
  match __tmp1 with
  | GT -> true
  | LT
  | EQ -> false
