let rec factorial = fun n ->
  if n = 0 then
    1
  else
    n * factorial (n - 1)
