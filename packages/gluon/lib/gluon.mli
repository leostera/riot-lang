(** Gluon - Minimal kqueue-based I/O event notification for macOS
    
    This library provides a clean, minimal interface for I/O event notification
    using kqueue on macOS. It focuses on simplicity and performance. *)

(** {1 Core Types} *)

(** File descriptor type - just the descriptor, no operations *)
module Fd : sig
  type t = Unix.file_descr
  
  val to_int : t -> int
  val pp : Format.formatter -> t -> unit
end

(** I/O result type *)
type ('a, 'e) io_result = ('a, ([> `Noop ] as 'e)) result

(** Token for identifying events. Can be any value. *)
module Token : sig
  type t
  
  val make : 'a -> t
  val unsafe_to_value : t -> 'a
  val equal : ?eq:('a -> 'a -> bool) -> t -> t -> bool
  val pp : Format.formatter -> t -> unit
end

(** I/O interests that can be registered *)
module Interest : sig
  type t
  
  val readable : t
  val writable : t
  
  val ( + ) : t -> t -> t
  (** [a + b] combines interests [a] and [b] *)
  
  val ( - ) : t -> t -> t option
  (** [a - b] removes interest [b] from [a], returns [None] if result is empty *)
  
  val is_readable : t -> bool
  val is_writable : t -> bool
  val pp : Format.formatter -> t -> unit
end

(** Events returned by polling *)
module Event : sig
  type t
  
  val token : t -> Token.t
  val is_readable : t -> bool
  val is_writable : t -> bool
  val is_error : t -> bool
  val is_eof : t -> bool
  val pp : Format.formatter -> t -> unit
end

(** {1 I/O Sources} *)

(** Source abstraction for registerable I/O objects *)
module Source : sig
  type t
  
  val fd : t -> Fd.t
end

(** {1 I/O Vectors} *)

module Iovec : sig
  type t
  
  val create : bytes -> pos:int -> len:int -> t
  val create_array : (bytes * int * int) array -> t array
end

(** {1 File I/O} *)

module File : sig
  type t = Fd.t

  val pp : Format.formatter -> t -> unit
  val close : t -> unit
  val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  val write : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  val read_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  val write_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  val to_source : t -> Source.t
  
  (** Open a file for reading *)
  val open_read : string -> (t, [> `Noop ]) io_result
  
  (** Open a file for writing *)
  val open_write : ?create:bool -> ?truncate:bool -> string -> (t, [> `Noop ]) io_result
end

(** {1 Network I/O} *)

module Net : sig
  module Addr : sig
    type 't raw_addr = string
    type tcp_addr = [ `v4 | `v6 ] raw_addr
    type stream_addr

    val get_info : stream_addr -> (stream_addr list, [> `Noop ]) io_result
    val ip : stream_addr -> string
    val loopback : tcp_addr
    val of_addr_info : Unix.addr_info -> stream_addr option
    val of_unix : Unix.sockaddr -> stream_addr
    val parse : string -> (stream_addr, [> `Noop ]) io_result
    val port : stream_addr -> int
    val pp : Format.formatter -> stream_addr -> unit
    val tcp : tcp_addr -> int -> stream_addr
    val to_domain : stream_addr -> Unix.socket_domain
    val to_string : tcp_addr -> string
    val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
  end

  module Socket : sig
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket

    val pp : Format.formatter -> _ socket -> unit
    val close : _ socket -> unit
  end

  module TcpStream : sig
    type t = Socket.stream_socket

    val connect :
      Addr.stream_addr ->
      ([ `Connected of t | `In_progress of t ], [> `Noop ]) io_result

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
    val read_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result

    val sendfile :
      t -> file:Fd.t -> off:int -> len:int -> (int, [> `Noop ]) io_result

    val to_source : t -> Source.t

    val write :
      t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result

    val write_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  end

  module TcpListener : sig
    type t = Socket.listen_socket

    val accept : t -> (TcpStream.t * Addr.stream_addr, [> `Noop ]) io_result

    val bind :
      ?reuse_addr:bool ->
      ?reuse_port:bool ->
      ?backlog:int ->
      Addr.stream_addr ->
      (t, [> `Noop ]) io_result

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val to_source : t -> Source.t
  end
end

(** {1 Kqueue Polling} *)

(** Poll instance (wraps a kqueue) *)
type t

(** Create a new kqueue instance *)
val create : unit -> (t, [> `System_error of string ]) result

(** Poll for events
    
    @param timeout Timeout in milliseconds. None means block indefinitely, Some 0 means non-blocking
    @param max_events Maximum number of events to return (default: 1024)
    @return Array of events or error *)
val poll : 
  ?timeout:int -> 
  ?max_events:int -> 
  t -> 
  (Event.t array, [> `System_error of string ]) result

(** {1 Registration} *)

(** Register a file descriptor with interests
    
    @param fd File descriptor to monitor
    @param token Token to identify events from this fd
    @param interests I/O interests to monitor *)
val register : 
  t -> 
  fd:Fd.t -> 
  token:Token.t -> 
  interests:Interest.t -> 
  (unit, [> `System_error of string ]) result

(** Re-register a file descriptor with new interests *)
val reregister : 
  t -> 
  fd:Fd.t -> 
  token:Token.t -> 
  interests:Interest.t -> 
  (unit, [> `System_error of string ]) result

(** Deregister a file descriptor *)
val deregister : 
  t -> 
  fd:Fd.t -> 
  (unit, [> `System_error of string ]) result

(** {1 Utilities} *)

(** Create a pipe with both ends set to non-blocking mode *)
val pipe : unit -> (Fd.t * Fd.t, [> `System_error of string ]) result

(** Set a file descriptor to non-blocking mode *)
val set_nonblocking : Fd.t -> (unit, [> `System_error of string ]) result

(** Pretty-print poll instance info *)
val pp : Format.formatter -> t -> unit