let x =
  match f (fun y -> y + 1) 5 with
  | Some z -> z
  | None -> 0
