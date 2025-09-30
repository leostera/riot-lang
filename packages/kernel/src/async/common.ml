let ( let* ) = fun x f -> match x with Ok v -> f v | Error e -> Error e
let log = Format.printf

type io_error =
  [ `Connection_closed
  | `Exn of exn
  | `No_info
  | `Unix_error of Unix.error
  | `Noop
  | `Eof
  | `Closed
  | `Process_down
  | `Timeout
  | `Would_block ]

type ('ok, 'err) io_result = ('ok, ([> io_error ] as 'err)) Stdlib.result

let pp_err fmt = function
  | `Noop -> Format.fprintf fmt "Noop"
  | `Eof -> Format.fprintf fmt "End of file"
  | `Timeout -> Format.fprintf fmt "Timeout"
  | `Process_down -> Format.fprintf fmt "Process_down"
  | `System_limit -> Format.fprintf fmt "System_limit"
  | `Closed -> Format.fprintf fmt "Closed"
  | `Connection_closed -> Format.fprintf fmt "Connection closed"
  | `Exn exn ->
      Format.fprintf fmt "Unexpected exceptoin: %s" (Printexc.to_string exn)
  | `No_info -> Format.fprintf fmt "No info"
  | `Would_block -> Format.fprintf fmt "Would block"
  | `Unix_error err ->
      Format.fprintf fmt "Unix_error(%s)" (Unix.error_message err)

let rec syscall fn =
  match fn () with
  | ok -> ok
  | exception Unix.(Unix_error (EINTR, _, _)) -> syscall fn
  | exception Unix.(Unix_error ((EAGAIN | EWOULDBLOCK), _, _)) ->
      Error `Would_block
  | exception Unix.(Unix_error (reason, _, _)) -> Error (`Unix_error reason)
