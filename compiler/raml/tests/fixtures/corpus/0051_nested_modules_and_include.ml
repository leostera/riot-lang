(* Nested modules and include. *)
module Base = struct
  let x = 41
  let show n = string_of_int n
end

module Extended = struct
  include Base
  let x = Base.x + 1
end

let () = print_endline (Extended.show Extended.x)
