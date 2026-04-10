open Global0

let getenv var =
  try Some (sys__getenv var) with
  | Sys__Not_found -> None

let getenv_exn var = sys__getenv var

let putenv var value = unix__putenv var value

let unsetenv var = unix__putenv var ""

let environment () = unix__environment ()

let getcwd () = unix__getcwd ()

let chdir path = unix__chdir path
