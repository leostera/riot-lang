let x =
  match [ [ 1 ]; [ 2; 3 ] ] with
  | [ y ] :: ys -> y
  | _ -> []
