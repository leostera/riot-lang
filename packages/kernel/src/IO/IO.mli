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

open Global0

(** {2 Error Handling} *)

type error =
  | End_of_file
  | Timeout
  | Closed
  | Connection_closed
  | Process_down
  | No_info
  | Noop
  | Exception of exn
  | Permission_denied
  | No_such_file_or_directory
  | Interrupted_system_call
  | Input_output_error
  | Bad_file_descriptor
  | Resource_unavailable_try_again
  | Out_of_memory
  | Permission_denied_on_file
  | Bad_address
  | Resource_busy
  | File_exists
  | Cross_device_link
  | Invalid_argument
  | Too_many_open_files_in_system
  | Too_many_open_files
  | Invalid_operation_on_device
  | File_too_large
  | No_space_left_on_device
  | Illegal_seek
  | Read_only_filesystem
  | Too_many_links
  | Broken_pipe
  | Numerical_argument_out_of_domain
  | Numerical_result_out_of_range
  | Resource_deadlock_would_occur
  | Filename_too_long
  | No_locks_available
  | Function_not_implemented
  | Directory_not_empty
  | Too_many_symbolic_links
  | Operation_would_block
  | Socket_operation_on_non_socket
  | Destination_address_required
  | Message_too_long
  | Protocol_wrong_type_for_socket
  | Protocol_not_available
  | Protocol_not_supported
  | Socket_type_not_supported
  | Operation_not_supported
  | Protocol_family_not_supported
  | Address_family_not_supported
  | Address_already_in_use
  | Cannot_assign_requested_address
  | Network_is_down
  | Network_is_unreachable
  | Network_dropped_connection_on_reset
  | Software_caused_connection_abort
  | Connection_reset_by_peer
  | No_buffer_space_available
  | Transport_endpoint_already_connected
  | Transport_endpoint_not_connected
  | Cannot_send_after_transport_endpoint_shutdown
  | Too_many_references
  | Connection_timed_out
  | Connection_refused
  | Host_is_down
  | No_route_to_host
  | Operation_already_in_progress
  | Operation_now_in_progress
  | Unknown_error of string
type nonrec 'value io_result = ('value, error) result
val error_of_unix : Unix.error -> error

val error_to_unix : error -> Unix.error

val error_message : error -> string

(** {2 Unix Syscall Helpers} *)

(** [unix_syscall fn] wraps a Unix syscall to automatically retry on EINTR.
    EAGAIN/EWOULDBLOCK are returned as errors for async handling at the Std layer.
    
    Example:
    {[
      let read fd buf =
        IO.unix_syscall (fun () -> Unix.read fd buf 0 (Bytes.length buf))
    ]} *)
val unix_syscall : (unit -> 'a) -> ('a, error) result

(** {2 File Types} *)

type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket
val file_kind_of_unix : Unix.file_kind -> file_kind

val file_kind_to_unix : file_kind -> Unix.file_kind

(** {2 Standard File Descriptors} *)

val stdin : Fd.t

val stdout : Fd.t

val stderr : Fd.t

(** {2 Generic I/O Abstractions} *)

(** Growable byte buffers. See {!Buffer}. *)
(** Byte sequences. See {!Bytes}. *)
module Buffer : module type of Buffer

(** IO vectors for scatter/gather operations. See {!Iovec}. *)
module Bytes : module type of Bytes

(** Reader abstraction. See {!Reader}. *)
module Iovec : module type of Iovec

(** Writer abstraction. See {!Writer}. *)
module Reader : module type of Reader

module Writer : module type of Writer

(** {2 Standard File Descriptors} *)

(** Standard input file descriptor *)

val stdin : Fd.t

(** Standard output file descriptor *)
val stdout : Fd.t

(** Standard error file descriptor *)
val stderr : Fd.t

(** {2 Convenience Functions}

    These are shortcuts to the corresponding functions in {!Reader} and
    {!Writer}. *)

(** [read reader ?timeout buf] reads data into [buf]. Equivalent to
    [Reader.read reader ?timeout buf]. *)
val read : ('src, 'err) Reader.t -> ?timeout:int64 -> bytes -> (int, 'err) result

(** [read_vectored reader iov] reads into multiple buffers. Equivalent to
    [Reader.read_vectored reader iov]. *)
val read_vectored : ('src, 'err) Reader.t -> Iovec.t -> (int, 'err) result

(** [read_to_end reader ~buf] reads until EOF. Equivalent to
    [Reader.read_to_end reader ~buf]. *)
val read_to_end : ('src, 'err) Reader.t -> buf:Buffer.t -> (int, 'err) result

(** [write writer ~buf] writes data (may be partial). Equivalent to
    [Writer.write writer ~buf]. *)
val write : ('dst, 'err) Writer.t -> buf:string -> (int, 'err) result

(** [write_all writer ~buf] writes all data, retrying as needed. Equivalent to
    [Writer.write_all writer ~buf]. *)
val write_all : ('dst, 'err) Writer.t -> buf:string -> (unit, 'err) result

(** [write_owned_vectored writer ~bufs] writes from multiple buffers. Equivalent
    to [Writer.write_owned_vectored writer ~bufs]. *)
val write_owned_vectored : ('dst, 'err) Writer.t -> bufs:Iovec.t -> (int, 'err) result

(** [write_all_vectored writer ~bufs] writes all vectored data. Equivalent to
    [Writer.write_all_vectored writer ~bufs]. *)
val write_all_vectored : ('dst, 'err) Writer.t -> bufs:Iovec.t -> (unit, 'err) result

(** [flush writer] flushes buffered data. Equivalent to [Writer.flush writer].
*)
val flush : ('dst, 'err) Writer.t -> (unit, 'err) result
