(** IO - Generic I/O abstractions *)

module Iovec = Iovec
module Reader = Reader
module Writer = Writer

(* Standard file descriptors *)
let stdin = Kernel.IO.stdin
let stdout = Kernel.IO.stdout
let stderr = Kernel.IO.stderr

(* Async-safe stdout writer *)
let stdout_writer =
  let module Write = struct
    type t = Kernel.Fd.t
    type err = Kernel.Async.io_error

    let write fd ~buf =
      let bytes = Bytes.unsafe_of_string buf in
      let len = String.length buf in
      Kernel.Fs.File.write fd ~pos:0 ~len bytes

    let write_owned_vectored fd ~bufs =
      Kernel.Fs.File.write_vectored fd bufs

    let flush _fd = Ok ()
  end in
  Writer.of_write_src (module Write) stdout

(* Convenience functions *)
let read = Reader.read
let read_vectored = Reader.read_vectored
let read_to_end = Reader.read_to_end
let write = Writer.write
let write_all = Writer.write_all
let write_owned_vectored = Writer.write_owned_vectored
let write_all_vectored = Writer.write_all_vectored
let flush = Writer.flush
