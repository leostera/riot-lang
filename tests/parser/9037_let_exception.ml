let panic msg =
  let exception Panic of string in
  raise (Panic msg)

let test x =
  let exception MyError in
  if x < 0 then
    raise MyError
  else
    x
