(** Gluon - High-performance I/O event notification library for macOS!

    Gluon provides a clean, efficient interface for asynchronous I/O event notification
    using kqueue on macOS. It's designed for building high-performance network applications
    and concurrent I/O systems.

    {1 Core Concepts}

    - {b Non-blocking I/O}: All operations return immediately, using [`Would_block] to signal retry
    - {b Event-driven}: Register interest in file descriptors, poll for events when ready  
    - {b Token-based}: Associate arbitrary data with file descriptors for event correlation
    - {b Zero-copy}: Vectored I/O and sendfile support for efficient data transfer

    The typical usage pattern is:
    1. Create a {!Poll.t} instance
    2. Register file descriptors with {!Interest.t} and {!Token.t}
    3. Poll for events and handle them based on tokens
    4. Perform I/O operations when ready *)

type io_error =
  [ `Connection_closed  (** TCP connection was closed by peer *)
  | `Exn of exn        (** Unexpected exception occurred *)
  | `No_info          (** No address information available *)
  | `Unix_error of Unix.error [@config not (target_arch = "js")]  (** System call error *)
  | `Noop             (** No operation performed (legacy) *)
  | `Eof              (** End of file/stream reached *)
  | `Closed           (** File descriptor was closed *)
  | `Process_down     (** Associated process terminated *)
  | `Timeout          (** Operation timed out *)
  | `Would_block ]    (** Operation would block, retry later *)
(** Common I/O error conditions. Most operations return [`Would_block] when 
    they cannot complete immediately - this is the normal case for async I/O. *)

type ('ok, 'err) io_result = ('ok, ([> io_error ] as 'err)) Stdlib.result
(** Result type for I/O operations. Success returns the expected value,
    failure returns an {!io_error} variant. *)

val pp_err : Format.formatter -> [< io_error ] -> unit
(** Pretty-print I/O errors for debugging. *)

module Iovec : sig
  (** I/O vectors for scatter-gather operations.
      
      I/O vectors allow reading/writing multiple buffers in a single system call,
      significantly improving performance for structured data like protocol headers. *)
      
  type iov = { ba : bytes; off : int; len : int }
  (** Individual buffer segment with offset and length. *)
  
  type t = iov array
  (** Array of buffer segments for vectored I/O. *)

  val with_capacity : int -> t
  (** Create single I/O vector with given capacity. *)
  
  val create : ?count:int -> size:int -> unit -> t
  (** Create I/O vector array. [count] segments of [size/count] bytes each.
      @param count Number of segments (default: 1)
      @param size Total size to distribute across segments *)
  
  val sub : ?pos:int -> len:int -> t -> t
  (** Extract sub-vector starting at [pos] with [len] total bytes.
      Spans multiple segments as needed. *)
  
  val length : t -> int
  (** Total length of all segments in the vector. *)
  
  val iter : t -> (iov -> unit) -> unit
  (** Iterate over all segments in the vector. *)
  
  val of_bytes : bytes -> t
  (** Create vector from single byte buffer. *)
  
  val from_string : string -> t
  (** Create vector from string. *)
  
  val from_buffer : Buffer.t -> t
  (** Create vector from buffer contents. *)
  
  val into_string : t -> string
  (** Convert vector contents to string. *)
end

module Fd : sig
  (** File descriptor operations.
      
      Thin wrapper around Unix file descriptors with utility functions. *)
      
  type t = Unix.file_descr
  (** File descriptor type. *)

  val close : t -> unit
  (** Close the file descriptor. *)
  
  val equal : t -> t -> bool
  (** Test file descriptor equality. *)
  
  val make : Unix.file_descr -> t
  (** Wrap Unix file descriptor. *)
  
  val pp : Format.formatter -> t -> unit
  (** Pretty-print file descriptor. *)
  
  val seek : t -> int -> Unix.seek_command -> int
  (** Seek to position in file. *)
  
  val to_int : t -> int
  (** Convert to integer representation. *)
end

module Non_zero_int : sig
  (** Utility for non-zero integers. *)
  
  type t = int
  (** Non-zero integer type. *)

  val make : int -> int option
  (** Create non-zero int, returns [None] if zero. *)
end

module Token : sig
  (** Event correlation tokens.
      
      Tokens allow associating arbitrary OCaml values with file descriptors.
      When events occur, the associated token is returned, enabling efficient
      event dispatching without hash table lookups. *)
      
  type t
  (** Opaque token type. *)

  val hash : t -> int
  (** Hash token for use in hash tables. *)
  
  val equal : ?eq:('a -> 'a -> bool) -> t -> t -> bool
  (** Test token equality. Provide custom equality for complex values. *)
  
  val make : 'value -> t
  (** Create token from any OCaml value. *)
  
  val pp : Format.formatter -> t -> unit
  (** Pretty-print token (shows internal representation). *)
  
  val unsafe_to_value : t -> 'value
  (** Extract original value. {b Warning}: Type-unsafe, must match creation type. *)
end

module Interest : sig
  (** I/O readiness interests.
      
      Interests specify what type of I/O readiness to monitor for a file descriptor.
      Use bitwise operations to combine multiple interests. *)
      
  type t
  (** Interest bitmask type. *)

  val add : t -> t -> t
  (** Combine interests (bitwise OR). *)
  
  val is_readable : t -> bool
  (** Check if readable interest is set. *)
  
  val is_writable : t -> bool
  (** Check if writable interest is set. *)
  
  val readable : t
  (** Interest in read readiness. *)
  
  val remove : t -> t -> t option
  (** Remove interest, returns [None] if result would be empty. *)
  
  val writable : t
  (** Interest in write readiness. *)
end

module Event : sig
  (** I/O readiness events.
      
      Events are returned by polling operations and indicate what type of
      I/O is ready on a file descriptor. Each event contains the token
      that was registered with the file descriptor. *)
      
  module type Intf = sig
    (** Interface for platform-specific event implementations. *)
    type t

    val is_error : t -> bool
    val is_priority : t -> bool
    val is_read_closed : t -> bool
    val is_readable : t -> bool
    val is_writable : t -> bool
    val is_write_closed : t -> bool
    val token : t -> Token.t
  end

  type t
  (** Platform-agnostic event type. *)

  val is_error : t -> bool
  (** Check if error condition occurred. *)
  
  val is_priority : t -> bool
  (** Check if priority data available (rare). *)
  
  val is_read_closed : t -> bool
  (** Check if read end was closed (EOF). *)
  
  val is_readable : t -> bool
  (** Check if read operation would not block. *)
  
  val is_writable : t -> bool
  (** Check if write operation would not block. *)
  
  val is_write_closed : t -> bool
  (** Check if write end was closed. *)
  
  val make : (module Intf with type t = 'state) -> 'state -> t
  (** Create platform-agnostic event from platform-specific event. *)
  
  val token : t -> Token.t
  (** Get the token associated with this event. *)
end

module Adapter : sig
  (** Low-level platform adapter.
      
      Provides direct access to the underlying kqueue implementation.
      Most users should use {!Poll} instead. *)
      
  module Selector : sig
    (** Low-level event selector (kqueue). *)
    type t

    val name : string
    (** Platform name: "kqueue" *)
    
    val make : unit -> (t, [> `Noop ]) io_result
    (** Create new kqueue instance. *)

    val select :
      ?timeout:int64 ->
      ?max_events:int ->
      t ->
      (Event.t list, [> `Noop ]) io_result
    (** Wait for I/O events.
        @param timeout Timeout in nanoseconds (default: 500ms)
        @param max_events Maximum events to return (default: 1000) *)

    val register :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      (unit, [> `Noop ]) io_result
    (** Register file descriptor for event monitoring. *)

    val reregister :
      t ->
      fd:Fd.t ->
      token:Token.t ->
      interest:Interest.t ->
      (unit, [> `Noop ]) io_result
    (** Change interests for already-registered file descriptor. *)

    val deregister : t -> fd:Fd.t -> (unit, [> `Noop ]) io_result
    (** Stop monitoring file descriptor. *)
  end

  module Event : sig
    (** Platform-specific event type. *)
    type t
  end
end

module Source : sig
  (** Event source abstraction.
      
      Sources represent any I/O object that can be monitored for events.
      This abstraction allows files, sockets, and other I/O objects to be
      used uniformly with the polling system. *)
      
  module type Intf = sig
    (** Interface for types that can be event sources. *)
    type t

    val deregister : t -> Adapter.Selector.t -> (unit, [> `Noop ]) io_result
    val register :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result
    val reregister :
      t ->
      Adapter.Selector.t ->
      Token.t ->
      Interest.t ->
      (unit, [> `Noop ]) io_result
  end

  type t = S : ((module Intf with type t = 'state) * 'state) -> t
  (** Existential source type that can hold any source implementation. *)

  val deregister : t -> Adapter.Selector.t -> (unit, [> `Noop ]) io_result
  (** Deregister source from event monitoring. *)
  
  val make : (module Intf with type t = 'a) -> 'a -> t
  (** Create source from implementation and state. *)

  val register :
    t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result
  (** Register source for event monitoring. *)

  val reregister :
    t -> Adapter.Selector.t -> Token.t -> Interest.t -> (unit, [> `Noop ]) io_result
  (** Change monitoring interests for source. *)
end

module File : sig
  (** Non-blocking file I/O operations.
      
      All operations are non-blocking and will return [`Would_block] if they
      cannot complete immediately. Files should be opened in non-blocking mode. *)
      
  type t = Unix.file_descr
  (** File handle type. *)

  val pp : Format.formatter -> t -> unit
  (** Pretty-print file descriptor. *)
  
  val close : t -> unit
  (** Close file descriptor. *)
  
  val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  (** Read data from file into buffer. Returns bytes read or [`Would_block].
      @param pos Starting position in buffer (default: 0)
      @param len Maximum bytes to read (default: buffer length - 1) *)
  
  val write : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
  (** Write data from buffer to file. Returns bytes written or [`Would_block].
      @param pos Starting position in buffer (default: 0)  
      @param len Bytes to write (default: buffer length - 1) *)
  
  val read_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  (** Read data using scatter-gather I/O. Returns total bytes read. *)
  
  val write_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
  (** Write data using scatter-gather I/O. Returns total bytes written. *)
  
  val to_source : t -> Source.t
  (** Convert file to event source for polling. *)
end

module Net : sig
  (** Non-blocking TCP networking.
      
      All network operations are non-blocking and work with the event polling system.
      Sockets are automatically set to non-blocking mode. *)
      
  module Addr : sig
    (** Network address handling. *)
    type 't raw_addr = string
    type tcp_addr = [ `v4 | `v6 ] raw_addr  
    type stream_addr

    val get_info : stream_addr -> (stream_addr list, [> `Noop ]) io_result
    (** Resolve address to list of addresses. *)
    
    val ip : stream_addr -> string
    (** Extract IP address as string. *)
    
    val loopback : tcp_addr
    (** Loopback address (0.0.0.0). *)
    
    val of_addr_info : Unix.addr_info -> stream_addr option
    (** Convert from Unix address info. *)
    
    val of_unix : Unix.sockaddr -> stream_addr
    (** Convert from Unix socket address. *)
    
    val of_host_and_port : host:string -> port:int -> (stream_addr, [> `Noop ]) io_result
    (** Create address by resolving hostname and port. *)
    
    val port : stream_addr -> int
    (** Extract port number. *)
    
    val pp : Format.formatter -> stream_addr -> unit
    (** Pretty-print address as host:port. *)
    
    val tcp : tcp_addr -> int -> stream_addr
    (** Create TCP address from IP string and port. *)
    
    val to_domain : stream_addr -> Unix.socket_domain
    (** Get socket domain for address. *)
    
    val to_string : tcp_addr -> string
    (** Convert IP address to string. *)
    
    val to_unix : stream_addr -> Unix.socket_type * Unix.sockaddr
    (** Convert to Unix socket types. *)
  end

  module Socket : sig
    (** Low-level socket operations. *)
    type 'kind socket = Fd.t
    type listen_socket = [ `listen ] socket
    type stream_socket = [ `stream ] socket

    val pp : Format.formatter -> _ socket -> unit
    val close : _ socket -> unit
  end

  module TcpStream : sig
    (** TCP client connections. *)
    type t = Socket.stream_socket

    val connect :
      Addr.stream_addr ->
      ([ `Connected of t | `In_progress of t ], [> `Noop ]) io_result
    (** Connect to remote address. May complete immediately [`Connected] or
        return [`In_progress] for async completion. *)

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val read : t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
    (** Read data from stream. Returns 0 on EOF, [`Would_block] if not ready. *)
    
    val read_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
    (** Vectored read operation. *)

    val sendfile :
      t -> file:Fd.t -> off:int -> len:int -> (int, [> `Noop ]) io_result
    (** Zero-copy file transmission using sendfile(2). *)

    val to_source : t -> Source.t
    (** Convert to event source for polling. *)

    val write :
      t -> ?pos:int -> ?len:int -> bytes -> (int, [> `Noop ]) io_result
    (** Write data to stream. May write partial data. *)

    val write_vectored : t -> Iovec.t -> (int, [> `Noop ]) io_result
    (** Vectored write operation. *)
  end

  module TcpListener : sig
    (** TCP server listeners. *)
    type t = Socket.listen_socket

    val accept : t -> (TcpStream.t * Addr.stream_addr, [> `Noop ]) io_result
    (** Accept incoming connection. Returns stream and client address. *)

    val bind :
      ?reuse_addr:bool ->
      ?reuse_port:bool ->
      ?backlog:int ->
      Addr.stream_addr ->
      (t, [> `Noop ]) io_result
    (** Create and bind listener socket.
        @param reuse_addr Allow address reuse (default: true)
        @param reuse_port Allow port reuse (default: true)  
        @param backlog Connection queue size (default: 128) *)

    val close : t -> unit
    val pp : Format.formatter -> t -> unit
    val to_source : t -> Source.t
    (** Convert to event source for accepting connections. *)
  end
end

module Poll : sig
  (** High-level polling interface.
      
      This is the main interface for event-driven I/O. Create a Poll instance,
      register sources with tokens and interests, then poll for events. *)
      
  type t
  (** Poll instance wrapping platform event system. *)

  val make : unit -> (t, [> `Noop ]) io_result
  (** Create new poll instance. *)

  val poll :
    ?max_events:int ->
    ?timeout:int64 ->
    t ->
    (Event.t list, [> `Noop ]) io_result
  (** Wait for I/O events.
      @param max_events Maximum events to return (default: 1000)
      @param timeout Timeout in nanoseconds, [None] blocks indefinitely
      @return List of ready events *)

  val register :
    t -> Token.t -> Interest.t -> Source.t -> (unit, [> `Noop ]) io_result
  (** Register source for event monitoring with token and interests. *)

  val reregister :
    t -> Token.t -> Interest.t -> Source.t -> (unit, [> `Noop ]) io_result
  (** Change interests for already-registered source. *)

  val deregister : t -> Source.t -> (unit, [> `Noop ]) io_result
  (** Stop monitoring source. Call before closing file descriptors. *)
end
