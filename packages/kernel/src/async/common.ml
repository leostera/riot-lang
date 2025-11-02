let ( let* ) = fun x f -> match x with Ok v -> f v | Error e -> Error e
let log = Format.printf

(* Async now uses IO.error for all I/O errors *)

let rec syscall fn =
  match fn () with
  | ok -> ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      Error IO.Operation_would_block
  | exception Unix.(Unix_error (reason, _, _)) ->
      Error (IO.error_of_unix reason)
