module M: sig
  type t = private int
  val make: int -> t
end = struct
  type t = int

  let make x = x
end
