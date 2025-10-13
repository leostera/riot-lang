(** IO - Generic I/O abstractions *)

module Iovec = Iovec
module Reader = Reader
module Writer = Writer

(* Standard file descriptors *)
let stdin = Kernel.IO.stdin
let stdout = Kernel.IO.stdout
let stderr = Kernel.IO.stderr

(* Convenience functions *)
let read = Reader.read
let read_vectored = Reader.read_vectored
let read_to_end = Reader.read_to_end
let write = Writer.write
let write_all = Writer.write_all
let write_owned_vectored = Writer.write_owned_vectored
let write_all_vectored = Writer.write_all_vectored
let flush = Writer.flush
