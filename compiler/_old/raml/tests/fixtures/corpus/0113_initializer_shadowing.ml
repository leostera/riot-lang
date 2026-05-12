(* Declaration initializer flattening must preserve the exported binding name
   even when the initializer shadows it locally. *)
let outer = 10

let result =
  let result = outer + 1 in
  result

let () = Printf.printf "%d %d\n" outer result
