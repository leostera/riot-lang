open Global0
open IO
open Async

type t = Socket.stream_socket

type connect_result =
[
  `Connected of t
  | `In_progress of t
]

let of_fd = fun fd -> fd

let close = Socket.close

(* Helper: retry on EINTR *)

let rec retry_eintr = fun fn ->
  try fn () with
  | Unix.(Unix_error (EINTR, _, _)) -> retry_eintr fn

let connect = fun addr ->
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = Socket.make sock_domain sock_type in
  try
    retry_eintr
      (fun () ->
        Unix.connect (Fd.to_unix fd) sock_addr);
    Unix.setsockopt (Fd.to_unix fd) Unix.TCP_NODELAY true;
    Ok (`Connected fd)
  with
  | Unix.(Unix_error (EINPROGRESS, _, _)) -> Ok (`In_progress fd)
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(((((Bytes.length buf - 1))))) in
  try Ok (retry_eintr (fun () -> UnixLabels.read (Fd.to_unix fd) ~buf ~pos ~len)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:(((((Bytes.length buf - 1))))) in
  try Ok (retry_eintr (fun () -> UnixLabels.write (Fd.to_unix fd) ~buf ~pos ~len)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_readv: Unix.file_descr -> Iovec.t -> int = "kernel_unix_readv"

let read_vectored = fun fd iov ->
  try Ok (retry_eintr (fun () -> std_sys_readv (Fd.to_unix fd) iov)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_writev: Unix.file_descr -> Iovec.t -> int = "kernel_unix_writev"

let write_vectored = fun fd iov ->
  try Ok (retry_eintr (fun () -> std_sys_writev (Fd.to_unix fd) iov)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

external std_sys_sendfile: Unix.file_descr -> Unix.file_descr -> int -> int -> int = "kernel_unix_sendfile"

let sendfile = fun fd ~file ~off ~len ->
  try Ok (retry_eintr (fun () -> std_sys_sendfile (Fd.to_unix file) (Fd.to_unix fd) off len)) with
  | Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let to_source = fun t ->
  let module Src = struct
    type nonrec t = t

    let register = fun t selector token interest -> Adapter.Selector.register
    selector
    ~fd:t
    ~token
    ~interest

    let reregister = fun t selector token interest -> Adapter.Selector.reregister
    selector
    ~fd:t
    ~token
    ~interest

    let deregister = fun t selector -> Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
