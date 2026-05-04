open Prelude

module Types = Types
module Buffer = Buffer
module Bytes = Bytes
module IoSlice = IoSlice
module IoVec = IoVec
module IoBuffer = Kernel.IO.Buffer
module Reader = Reader
module BufReader = Buf_reader
module Writer = Writer
module Error = Error
module Stdin = Stdin
module Stdout = Stdout
module Stderr = Stderr

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

let from_system_error = Error.from_system_error

let from_system_error_code = Error.from_system_error_code

let from_async_error = Error.from_async_error

let error_message = Error.message

let stdin = fun ?chunk_size () ->
  Stdin.open_ ?chunk_size ()
  |> Stdin.to_reader

let stdout = fun () -> Stdout.to_writer ()

let stderr = fun () -> Stderr.to_writer ()

let read = Reader.read

let read_vectored = Reader.read_vectored

let is_read_vectored = Reader.is_read_vectored

let read_to_end = Reader.read_to_end

let read_to_string = Reader.read_to_string

let write = Writer.write

let write_vectored = Writer.write_vectored

let write_all = Writer.write_all

let write_all_vectored = Writer.write_all_vectored

let flush = Writer.flush
