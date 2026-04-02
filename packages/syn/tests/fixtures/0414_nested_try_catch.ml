let x =
  try
    try f y with
    | E1 -> g y
  with
  | E2 -> h y
