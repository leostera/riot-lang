(* Test: extension nodes *)

let x = [%test 42]

let y = [%derive show, eq]

let z = [%expect {| some output |}] [%%toplevel_eval (print_endline "hello")]
