let x = fun a ->
  if a < 0 then
    raise (Invalid_argument "negative")
  else
    a
