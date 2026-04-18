open Prelude

type error = Error.t =
  | End_of_file
  | Timeout
  | Closed
  | Connection_closed
  | Process_down
  | No_info
  | Noop
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
type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket
val of_system_error: Kernel.SystemError.t -> error

val of_async_error: Kernel.Async.error -> error

val error_message: error -> string

module Buffer: module type of Buffer

module Bytes: module type of Bytes

module Iovec = Kernel.IO.Iovec

module IoBuffer: module type of Kernel.IO.Buffer

module StringView: module type of Kernel.IO.StringView

module Reader = Reader
module BufferedReader = Buffered_reader

module Writer = Writer

module Stdin: sig
  type t
  type nonrec error = error
  val open_: ?chunk_size:int -> unit -> t

  val read: t -> ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

  val read_vectored: t -> Iovec.t -> (int, error) result

  val to_reader: t -> (t, error) Reader.t
end

val stdin: ?chunk_size:int -> unit -> (Stdin.t, error) Reader.t

val stdout: unit -> (Stdout.t, error) Writer.t

val stderr: unit -> (Stderr.t, error) Writer.t

val buffered:
  ?chunk_size:int ->
  unit ->
  ('src, 'err) Reader.t ->
  ('src, 'err) BufferedReader.t

module Stdout: sig
  type nonrec error = error
  val write: ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

  val write_vectored: Iovec.t -> (int, error) result

  val flush: unit -> (unit, error) result
end

module Stderr: sig
  type nonrec error = error
  val write: ?offset:int -> ?len:int -> Bytes.t -> (int, error) result

  val write_vectored: Iovec.t -> (int, error) result

  val flush: unit -> (unit, error) result
end

val read:
  ('src, 'err) Reader.t ->
  ?timeout:int64 ->
  ?offset:int ->
  ?len:int ->
  bytes ->
  (int, 'err) result

val read_vectored: ('src, 'err) Reader.t -> Iovec.t -> (int, 'err) result

val read_char: ('src, 'err) Reader.t -> (char option, 'err) result

val read_line: ('src, 'err) Reader.t -> (string, 'err) result

val read_to_string: ('src, 'err) Reader.t -> len:int -> (string, 'err) result

val read_to_end: ('src, 'err) Reader.t -> buf:Buffer.t -> (int, 'err) result

val write: ('dst, 'err) Writer.t -> buf:string -> (int, 'err) result

val write_all: ('dst, 'err) Writer.t -> buf:string -> (unit, 'err) result

val write_owned_vectored: ('dst, 'err) Writer.t -> bufs:Iovec.t -> (int, 'err) result

val write_all_vectored: ('dst, 'err) Writer.t -> bufs:Iovec.t -> (unit, 'err) result

val flush: ('dst, 'err) Writer.t -> (unit, 'err) result
