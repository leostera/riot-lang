open Global0
(** Environment operations for Kernel *)
let getenv = fun var ->
  try Some (sys__getenv var) with
  | Sys__Not_found -> None

let getenv_exn = fun var -> sys__getenv var

let putenv = fun var value -> unix__putenv var value

let unsetenv = fun var ->
  (* Use putenv with empty string to unset, as unsetenv may not be available *)
  unix__putenv var ""

let environment = fun () -> unix__environment ()

let getcwd = fun () -> unix__getcwd ()

let chdir = fun path -> unix__chdir path
