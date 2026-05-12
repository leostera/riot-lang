let rec filter = fun f lst ->
  match lst with
  | [] -> []
  | x :: xs ->
      if f x then
        x :: filter f xs
      else
        filter f xs
