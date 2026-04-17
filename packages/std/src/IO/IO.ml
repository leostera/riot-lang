open Prelude
let panic = Kernel.SystemError.panic
module Buffer = Buffer
module Bytes = Bytes
module Iovec = Kernel.IO.Iovec
module Reader = Reader
module BufferedReader = Buffered_reader
module Writer = Writer
module Error = Error
module Stdin = Stdin
module Stdout = Stdout
module Stderr = Stderr

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

let of_system_error = Error.of_system_error

let of_async_error = Error.of_async_error

let error_message = Error.message

let stdin = fun ?chunk_size () ->
  Stdin.open_ ?chunk_size () |> Stdin.to_reader

let buffered = fun ?chunk_size () reader ->
  BufferedReader.of_reader ?chunk_size reader

let read = fun reader ?timeout ?(offset = 0) ?len buffer ->
  let buffer_len = Bytes.length buffer in
  let len =
    match len with
    | Some len -> len
    | None -> buffer_len - offset
  in
  if offset < 0 || len < 0 || offset + len > buffer_len then
    panic "Std.IO.read: invalid buffer slice";
  if offset = 0 && len = buffer_len then
    Reader.read reader ?timeout buffer
  else
    match timeout with
    | Some timeout ->
        let tmp = Bytes.create ~size:len in
        (
          match Reader.read reader ~timeout tmp with
          | Ok count ->
              Bytes.blit_unchecked tmp ~src_offset:0 ~dst:buffer ~dst_offset:offset ~len:count;
              Ok count
          | Error _ as error -> error
        )
    | None ->
        Reader.read_vectored reader (Iovec.sub ~pos:offset ~len (Iovec.from_bytes buffer))

let read_vectored = Reader.read_vectored

let read_char = Reader.read_char

let read_line = Reader.read_line

let read_to_string = Reader.read_to_string

let read_to_end = Reader.read_to_end

let write = Writer.write

let write_all = Writer.write_all

let write_owned_vectored = Writer.write_owned_vectored

let write_all_vectored = Writer.write_all_vectored

let flush = Writer.flush
