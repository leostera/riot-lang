(* Module aliases and local opens. *)
module M = struct
  let twice x = x * 2
end

module N = M

let value =
  let open N in
  twice 21

let () = Printf.printf "%d\n" value
