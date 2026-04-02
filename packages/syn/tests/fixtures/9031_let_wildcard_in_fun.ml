(* Test: let _ = expr in body inside fun expressions *)

(* This currently FAILS - documenting for future fix *)

let f x =
  g
    (fun y ->
      let _ = h y in
      ())
    z
