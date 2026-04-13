open Kernel
module Buffer = Buffer
module Bytes = Bytes
module Iovec = Kernel.IO.Iovec
module Reader = Reader
module Writer = Writer

type error =
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

let of_system_error = function
  | Kernel.SystemError.EndOfFile -> End_of_file
  | Kernel.SystemError.PermissionDenied -> Permission_denied
  | Kernel.SystemError.NoSuchFileOrDirectory -> No_such_file_or_directory
  | Kernel.SystemError.Interrupted -> Interrupted_system_call
  | Kernel.SystemError.InputOutput -> Input_output_error
  | Kernel.SystemError.BadFileDescriptor -> Bad_file_descriptor
  | Kernel.SystemError.ResourceBusy -> Resource_busy
  | Kernel.SystemError.AlreadyExists -> File_exists
  | Kernel.SystemError.InvalidArgument -> Invalid_argument
  | Kernel.SystemError.NoSpaceLeft -> No_space_left_on_device
  | Kernel.SystemError.BrokenPipe -> Broken_pipe
  | Kernel.SystemError.WouldBlock -> Operation_would_block
  | Kernel.SystemError.NotDirectory -> Invalid_operation_on_device
  | Kernel.SystemError.IsDirectory -> Invalid_operation_on_device
  | Kernel.SystemError.NotSupported -> Operation_not_supported
  | Kernel.SystemError.AddressInUse -> Address_already_in_use
  | Kernel.SystemError.AddressNotAvailable -> Cannot_assign_requested_address
  | Kernel.SystemError.ConnectionRefused -> Connection_refused
  | Kernel.SystemError.ConnectionReset -> Connection_reset_by_peer
  | Kernel.SystemError.TimedOut -> Connection_timed_out
  | Kernel.SystemError.NetworkUnreachable -> Network_is_unreachable
  | Kernel.SystemError.DestinationAddressRequired -> Destination_address_required
  | Kernel.SystemError.NotConnected -> Transport_endpoint_not_connected
  | Kernel.SystemError.ConnectionAborted -> Software_caused_connection_abort
  | Kernel.SystemError.MessageTooLong -> Message_too_long
  | Kernel.SystemError.NoSuchProcess -> Process_down
  | Kernel.SystemError.DirectoryNotEmpty -> Directory_not_empty
  | Kernel.SystemError.Unknown code -> Unknown_error ("Unknown system error code "
  ^ Kernel.Int.to_string code)

let of_async_error = function
  | Kernel.Async.InvalidTimeoutNs _ -> Invalid_argument
  | Kernel.Async.InvalidMaxEvents _ -> Invalid_argument
  | Kernel.Async.System err -> of_system_error err

let error_message = function
  | End_of_file -> "End of file"
  | Timeout -> "Timeout"
  | Closed -> "Closed"
  | Connection_closed -> "Connection closed"
  | Process_down -> "Process down"
  | No_info -> "No info"
  | Noop -> "No operation"
  | Permission_denied -> "Permission denied"
  | No_such_file_or_directory -> "No such file or directory"
  | Interrupted_system_call -> "Interrupted system call"
  | Input_output_error -> "Input/output error"
  | Bad_file_descriptor -> "Bad file descriptor"
  | Resource_unavailable_try_again -> "Resource unavailable, try again"
  | Out_of_memory -> "Out of memory"
  | Permission_denied_on_file -> "Permission denied"
  | Bad_address -> "Bad address"
  | Resource_busy -> "Resource busy"
  | File_exists -> "File exists"
  | Cross_device_link -> "Cross-device link"
  | Invalid_argument -> "Invalid argument"
  | Too_many_open_files_in_system -> "Too many open files in system"
  | Too_many_open_files -> "Too many open files"
  | Invalid_operation_on_device -> "Invalid operation on device"
  | File_too_large -> "File too large"
  | No_space_left_on_device -> "No space left on device"
  | Illegal_seek -> "Illegal seek"
  | Read_only_filesystem -> "Read-only filesystem"
  | Too_many_links -> "Too many links"
  | Broken_pipe -> "Broken pipe"
  | Numerical_argument_out_of_domain -> "Numerical argument out of domain"
  | Numerical_result_out_of_range -> "Numerical result out of range"
  | Resource_deadlock_would_occur -> "Resource deadlock would occur"
  | Filename_too_long -> "Filename too long"
  | No_locks_available -> "No locks available"
  | Function_not_implemented -> "Function not implemented"
  | Directory_not_empty -> "Directory not empty"
  | Too_many_symbolic_links -> "Too many symbolic links"
  | Operation_would_block -> "Operation would block"
  | Socket_operation_on_non_socket -> "Socket operation on non-socket"
  | Destination_address_required -> "Destination address required"
  | Message_too_long -> "Message too long"
  | Protocol_wrong_type_for_socket -> "Protocol wrong type for socket"
  | Protocol_not_available -> "Protocol not available"
  | Protocol_not_supported -> "Protocol not supported"
  | Socket_type_not_supported -> "Socket type not supported"
  | Operation_not_supported -> "Operation not supported"
  | Protocol_family_not_supported -> "Protocol family not supported"
  | Address_family_not_supported -> "Address family not supported"
  | Address_already_in_use -> "Address already in use"
  | Cannot_assign_requested_address -> "Cannot assign requested address"
  | Network_is_down -> "Network is down"
  | Network_is_unreachable -> "Network is unreachable"
  | Network_dropped_connection_on_reset -> "Network dropped connection on reset"
  | Software_caused_connection_abort -> "Software caused connection abort"
  | Connection_reset_by_peer -> "Connection reset by peer"
  | No_buffer_space_available -> "No buffer space available"
  | Transport_endpoint_already_connected -> "Transport endpoint already connected"
  | Transport_endpoint_not_connected -> "Transport endpoint not connected"
  | Cannot_send_after_transport_endpoint_shutdown -> "Cannot send after transport endpoint shutdown"
  | Too_many_references -> "Too many references"
  | Connection_timed_out -> "Connection timed out"
  | Connection_refused -> "Connection refused"
  | Host_is_down -> "Host is down"
  | No_route_to_host -> "No route to host"
  | Operation_already_in_progress -> "Operation already in progress"
  | Operation_now_in_progress -> "Operation now in progress"
  | Unknown_error message -> message

module Stdin = struct
  type nonrec error = error

  let read = fun ?offset ?len buffer ->
    let source = Kernel.IO.Stdin.to_source () in
    let rec loop () =
      match Kernel.IO.Stdin.read ?pos:offset ?len buffer with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdin.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdin.read"
        ~interest:Kernel.Async.Interest.readable
        ~source
        loop
      | Error (Kernel.IO.Stdin.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let read_vectored = fun bufs ->
    let source = Kernel.IO.Stdin.to_source () in
    let rec loop () =
      match Kernel.IO.Stdin.read_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdin.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdin.read_vectored"
        ~interest:Kernel.Async.Interest.readable
        ~source
        loop
      | Error (Kernel.IO.Stdin.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdin.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()
end

module Stdout = struct
  type nonrec error = error

  let write = fun ?offset ?len buffer ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.write ?pos:offset ?len buffer with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.write"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let write_vectored = fun bufs ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.write_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.write_vectored"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let flush = fun () ->
    let source = Kernel.IO.Stdout.to_source () in
    let rec loop () =
      match Kernel.IO.Stdout.flush () with
      | Ok () -> Ok ()
      | Error (Kernel.IO.Stdout.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stdout.flush"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stdout.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stdout.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()
end

module Stderr = struct
  type nonrec error = error

  let write = fun ?offset ?len buffer ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write ?pos:offset ?len buffer with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.write"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let write_vectored = fun bufs ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.write_vectored bufs with
      | Ok value -> Ok value
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.write_vectored"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()

  let flush = fun () ->
    let source = Kernel.IO.Stderr.to_source () in
    let rec loop () =
      match Kernel.IO.Stderr.flush () with
      | Ok () -> Ok ()
      | Error (Kernel.IO.Stderr.System error) when Kernel.SystemError.would_block error -> Runtime.syscall
        ~name:"IO.Stderr.flush"
        ~interest:Kernel.Async.Interest.writable
        ~source
        loop
      | Error (Kernel.IO.Stderr.System error) -> Error (of_system_error error)
      | Error (Kernel.IO.Stderr.InvalidSlice _) -> Error Invalid_argument
    in
    loop ()
end

let read = Reader.read

let read_vectored = Reader.read_vectored

let read_to_end = Reader.read_to_end

let write = Writer.write

let write_all = Writer.write_all

let write_owned_vectored = Writer.write_owned_vectored

let write_all_vectored = Writer.write_all_vectored

let flush = Writer.flush
