let x =
  try risky () with
  | Failure msg -> 0
