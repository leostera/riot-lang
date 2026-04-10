(* Fixed-width integer modules. *)
let a = Int32.add 10l 20l
let b = Int64.shift_left 1L 40
let c = Nativeint.logxor 0xF0n 0xAAn

let () =
  Printf.printf "%ld %Ld %s\n" a b (Nativeint.to_string c)
