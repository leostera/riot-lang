(* Test: First-class module unpacking with let module *)

(* Simple unpacking without type constraint *)

let test1 driver =
  let module D = (val driver) in
  D.execute ()

(* Unpacking with type constraint *)

let test2 driver =
  let module D = (val driver : Driver) in
  D.execute ()

(* Real-world example from sqlx *)

let fetch cursor =
  let module D = (val cursor.driver) in
  match D.fetch_row cursor.result_set with
  | Some row -> Some row
  | None -> None
