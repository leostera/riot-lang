open Prelude

module Types = Types

type error = Error.t =
  | End_of_file
  | Unexpected_end_of_file
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
  | Buffer_full
  | Invalid_data
  | Unknown_error of string
type nonrec 'value result = ('value, error) Result.t
type nonrec 'value io_result = 'value result
type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket

val from_system_error: Kernel.SystemError.t -> error

val from_system_error_code: int -> error

val from_async_error: Kernel.Async.error -> error

val error_message: error -> string

module Error = Error

module Buffer: module type of Buffer

module Bytes: module type of Bytes

module IoSlice: module type of IoSlice

module IoVec: module type of IoVec

module IoBuffer: module type of Kernel.IO.Buffer

module Reader = Reader

module BufReader = Buf_reader

module Writer = Writer

module Stdin: sig
  type t
  type nonrec error = error

  val open_: ?chunk_size:int -> unit -> t

  val read: t -> into:Buffer.t -> int result

  val read_vectored: t -> into:IoVec.t -> int result

  val to_reader: t -> Reader.t
end

module Stdout: sig
  type nonrec error = error

  val write: from:Buffer.t -> int result

  val write_vectored: from:IoVec.t -> int result

  val flush: unit -> unit result
end

module Stderr: sig
  type nonrec error = error

  val write: from:Buffer.t -> int result

  val write_vectored: from:IoVec.t -> int result

  val flush: unit -> unit result
end

val stdin: ?chunk_size:int -> unit -> Reader.t

val stdout: unit -> Writer.t

val stderr: unit -> Writer.t

val read: Reader.t -> into:Buffer.t -> int result

val read_vectored: Reader.t -> into:IoVec.t -> int result

val is_read_vectored: Reader.t -> bool

val read_to_end: Reader.t -> into:Buffer.t -> int result

val read_to_string: Reader.t -> into:StringBuilder.t -> int result

val write: Writer.t -> from:Buffer.t -> int result

val write_vectored: Writer.t -> from:IoVec.t -> int result

val write_all: Writer.t -> from:Buffer.t -> unit result

val write_all_vectored: Writer.t -> from:IoVec.t -> unit result

val flush: Writer.t -> unit result
