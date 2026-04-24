module type Ordered = sig
  type t
  val compare: t -> t -> int
end

type t =
  | LT
  | EQ
  | GT
