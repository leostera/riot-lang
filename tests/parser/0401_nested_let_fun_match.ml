let f = fun x ->
  let y =
    match x with
    | 0 -> 1
    | _ -> 2
  in
  y + 1
