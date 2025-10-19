external add_native : int -> int -> int = "caml_add_native"

let add a b = add_native a b
