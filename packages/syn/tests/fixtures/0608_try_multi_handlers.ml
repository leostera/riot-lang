let x =
  try f () with
  | Not_found -> 0
  | Failure _ -> 1
