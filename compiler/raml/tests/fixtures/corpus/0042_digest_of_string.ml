(* Message digests. *)
let h = Digest.string "raml"

let () = print_endline (Digest.to_hex h)
