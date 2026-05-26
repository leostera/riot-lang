module type F = functor (X : S) -> sig
  val x: int
end
