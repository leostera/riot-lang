module M = struct
  type t = int

  exception Error

  let make x = x

  let get t = t
end
