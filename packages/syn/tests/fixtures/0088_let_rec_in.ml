let x =
  let rec f n =
    if n = 0 then
      1
    else
      n * f (n - 1)
  in
  f 5
