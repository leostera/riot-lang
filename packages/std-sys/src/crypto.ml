(** Low-level cryptographic hash functions via C bindings *)

(* Placeholder implementations until C stubs are written *)
(* These would be replaced by actual external declarations when C code is added *)

let md5 s =
  (* Temporary: use OCaml's built-in Digest *)
  Digest.string s

let sha1 _s = failwith "sha1: C binding not yet implemented"
let sha256 _s = failwith "sha256: C binding not yet implemented"
let sha512 _s = failwith "sha512: C binding not yet implemented"

let bytes_to_hex s =
  (* Temporary: use OCaml's Digest.to_hex *)
  Digest.to_hex s
