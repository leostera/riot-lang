(** Environment operations for Kernel *)

let getenv var = try Some (Sys.getenv var) with Not_found -> None
let getenv_exn var = Sys.getenv var
let putenv var value = Unix.putenv var value

let unsetenv var =
  (* Use putenv with empty string to unset, as unsetenv may not be available *)
  Unix.putenv var ""

let environment () = Unix.environment ()
let getcwd () = Unix.getcwd ()
let chdir path = Unix.chdir path
