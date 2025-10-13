(** TCP stream for connected sockets *)

type t

type error = [ `Connection_refused | `Closed | `System_error of string ]

val connect : Kernel.Net.Addr.stream_addr -> (t, error) result
(** Connect to a TCP endpoint. This will suspend the process until the
    connection is established. *)

val read : t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result
(** Read data from the stream. This will suspend the process until data is
    available. Returns the number of bytes read. *)

val write : t -> bytes -> ?pos:int -> ?len:int -> unit -> (int, error) result
(** Write data to the stream. This will suspend the process until the socket
    is ready for writing. Returns the number of bytes written. *)

val close : t -> unit
(** Close the stream *)
