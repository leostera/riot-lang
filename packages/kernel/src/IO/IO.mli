(** `IO` intentionally stays small in `kernel-new`.

    Its public role is `Iovec`, the shared vectored-I/O segment surface used by file and socket
    operations. *)
module Iovec = Iovec
