let rec gcd = fun a b ->
  if b = 0 then
    a
  else
    gcd b (a mod b)
