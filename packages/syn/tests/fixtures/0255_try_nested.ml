let x =
  try
    try e1 with
    | E -> e2
  with
  | F -> e3
