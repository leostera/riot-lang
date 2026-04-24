type t =
  | LT
  | EQ
  | GT

module type Ordered = sig
  type t
  val compare: t -> t -> int
end
