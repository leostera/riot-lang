(** {1 IO - Generic I/O abstractions}

    Unified I/O interfaces for reading from and writing to various sources and
    destinations (files, sockets, buffers, etc.).

    {2 Overview}

    The IO module provides:
    - {!Reader} - Abstract interface for readable sources
    - {!Writer} - Abstract interface for writable destinations
    - {!Iovec} - IO vectors for efficient scatter/gather operations
    - Top-level convenience functions for common operations

    {2 Design Philosophy}

    The IO abstractions use first-class modules to achieve:
    - {b Type erasure}: Code can be polymorphic over I/O sources
    - {b Zero-cost}: No runtime overhead compared to direct calls
    - {b Composability}: Easy to wrap and chain I/O operations
    - {b Uniformity}: Same API whether reading from files, sockets, or memory
    - {b Flexible errors}: Each source/destination can define its own error
      types

    {2 Quick Start}

    Reading from a TCP connection:
    {[
      open Std

      let handle_connection stream =
        let reader = Net.TcpStream.to_reader stream in
        let buf = Bytes.create 4096 in

        match IO.read reader buf with
        | Ok n ->
            let data = Bytes.sub_string buf 0 n in
            process_data data
        | Error `Closed -> Log.info "Connection closed"
        | Error e -> Log.error "Read error"
    ]}

    Writing to a TCP connection:
    {[
      let send_response stream =
        let writer = Net.TcpStream.to_writer stream in
        let* () = IO.write_all writer ~buf:"HTTP/1.1 200 OK\r\n\r\n" in
        let* () = IO.write_all writer ~buf:"Hello, world!" in
        IO.flush writer
    ]}

    {2 Error Handling}

    Unlike traditional I/O libraries that force a single error hierarchy, this
    library allows each source to define its own error types. For example:
    - TCP streams might have
      [`Closed | `Connection_refused | `System_error of string]
    - Files might have [`Closed | `Permission_denied | `Disk_full]
    - In-memory buffers might have [`Buffer_overflow]

    This gives you precise error handling tailored to each I/O source.

    {2 Integration with Sources}

    Sources like {!Net.TcpStream} provide [to_reader] and [to_writer] functions
    to convert into these generic abstractions:

    {[
      (* From specific type to generic *)
      let stream : Net.TcpStream.t = ... in
      let reader = Net.TcpStream.to_reader stream in
      let writer = Net.TcpStream.to_writer stream in

      (* Now can use generic IO functions *)
      IO.read reader buf
      IO.write_all writer ~buf:"data"
    ]} *)

module Iovec : module type of Iovec
(** IO vectors for scatter/gather operations. See {!Iovec}. *)

module Reader : module type of Reader
(** Reader abstraction. See {!Reader}. *)

module Writer : module type of Writer
(** Writer abstraction. See {!Writer}. *)

(** {2 Standard File Descriptors} *)

val stdin : Kernel.Fd.t
(** Standard input file descriptor *)

val stdout : Kernel.Fd.t
(** Standard output file descriptor *)

val stderr : Kernel.Fd.t
(** Standard error file descriptor *)

(** {2 Convenience Functions}

    These are shortcuts to the corresponding functions in {!Reader} and
    {!Writer}. *)

val read :
  ('src, 'err) Reader.t -> ?timeout:int64 -> bytes -> (int, 'err) result
(** [read reader ?timeout buf] reads data into [buf]. Equivalent to
    [Reader.read reader ?timeout buf]. *)

val read_vectored : ('src, 'err) Reader.t -> Iovec.t -> (int, 'err) result
(** [read_vectored reader iov] reads into multiple buffers. Equivalent to
    [Reader.read_vectored reader iov]. *)

val read_to_end : ('src, 'err) Reader.t -> buf:Buffer.t -> (int, 'err) result
(** [read_to_end reader ~buf] reads until EOF. Equivalent to
    [Reader.read_to_end reader ~buf]. *)

val write : ('dst, 'err) Writer.t -> buf:string -> (int, 'err) result
(** [write writer ~buf] writes data (may be partial). Equivalent to
    [Writer.write writer ~buf]. *)

val write_all : ('dst, 'err) Writer.t -> buf:string -> (unit, 'err) result
(** [write_all writer ~buf] writes all data, retrying as needed. Equivalent to
    [Writer.write_all writer ~buf]. *)

val write_owned_vectored :
  ('dst, 'err) Writer.t -> bufs:Iovec.t -> (int, 'err) result
(** [write_owned_vectored writer ~bufs] writes from multiple buffers. Equivalent
    to [Writer.write_owned_vectored writer ~bufs]. *)

val write_all_vectored :
  ('dst, 'err) Writer.t -> bufs:Iovec.t -> (unit, 'err) result
(** [write_all_vectored writer ~bufs] writes all vectored data. Equivalent to
    [Writer.write_all_vectored writer ~bufs]. *)

val flush : ('dst, 'err) Writer.t -> (unit, 'err) result
(** [flush writer] flushes buffered data. Equivalent to [Writer.flush writer].
*)
