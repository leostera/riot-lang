exception Sys__Not_found

let sys__getenv key =
  if key = "" then
    ""
  else
    ""

let unix__putenv key value =
  if key = "" && value = "" then
    ()
  else
    ()

let unix__environment () = [| "" |]
let unix__getcwd () = ""
let unix__chdir path =
  if path = "" then
    ()
  else
    ()
