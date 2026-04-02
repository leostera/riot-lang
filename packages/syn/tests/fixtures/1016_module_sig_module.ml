module type S = sig
  module M: sig
    val x: int
  end
end
