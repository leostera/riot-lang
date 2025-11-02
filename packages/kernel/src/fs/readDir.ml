open Async

type t = Unix.dir_handle

let open_ path =
  try Ok (Unix.opendir path)
  with Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)

let read handle =
  syscall @@ fun () ->
  try Ok (Unix.readdir handle) with
  | End_of_file -> Error IO.End_of_file
  | Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)

let close handle =
  syscall @@ fun () ->
  try
    Unix.closedir handle;
    Ok ()
  with Unix.Unix_error (e, _, _) -> Error (IO.error_of_unix e)
