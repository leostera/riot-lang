open Async

type t = Socket.stream_socket
type connect_result = [ `Connected of t | `In_progress of t ]

let of_fd fd = fd
let to_string = Socket.to_string
let close = Socket.close

let connect addr =
  let sock_domain = Addr.to_domain addr in
  let sock_type, sock_addr = Addr.to_unix addr in
  let fd = Socket.make sock_domain sock_type in
  syscall @@ fun () ->
  try
    Unix.connect (Fd.to_unix fd) sock_addr;
    Unix.setsockopt (Fd.to_unix fd) Unix.TCP_NODELAY true;
    Ok (`Connected fd)
  with Unix.(Unix_error (EINPROGRESS, _, _)) -> Ok (`In_progress fd)

let read fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.read (Fd.to_unix fd) ~buf ~pos ~len)

let write fd ?(pos = 0) ?len buf =
  let len = Option.value len ~default:(Bytes.length buf - 1) in
  syscall @@ fun () -> Ok (UnixLabels.write (Fd.to_unix fd) ~buf ~pos ~len)

external std_sys_readv : Unix.file_descr -> Iovec.t -> int = "kernel_unix_readv"

let read_vectored fd iov =
  syscall @@ fun () -> Ok (std_sys_readv (Fd.to_unix fd) iov)

external std_sys_writev : Unix.file_descr -> Iovec.t -> int
  = "kernel_unix_writev"

let write_vectored fd iov =
  syscall @@ fun () -> Ok (std_sys_writev (Fd.to_unix fd) iov)

external std_sys_sendfile :
  Unix.file_descr -> Unix.file_descr -> int -> int -> int
  = "kernel_unix_sendfile"

let sendfile fd ~file ~off ~len =
  syscall @@ fun () ->
  Ok (std_sys_sendfile (Fd.to_unix file) (Fd.to_unix fd) off len)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register t selector token interest =
      Adapter.Selector.register selector ~fd:t ~token ~interest

    let reregister t selector token interest =
      Adapter.Selector.reregister selector ~fd:t ~token ~interest

    let deregister t selector = Adapter.Selector.deregister selector ~fd:t
  end in
  Source.make (module Src) t
