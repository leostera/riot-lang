(* Tuple construction, destructuring, and swapping. *)
let pair = ("raml", 5)

let swap (a, b) = (b, a)

let () =
  let name, n = pair in
  let n2, name2 = swap pair in
  Printf.printf "%s %d %d %s\n" name n n2 name2
