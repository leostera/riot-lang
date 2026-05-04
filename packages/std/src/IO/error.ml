open Prelude

type t =
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

let from_system_error = fun __tmp1 ->
  match __tmp1 with
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
  | Kernel.SystemError.Unknown code ->
      Unknown_error ("Unknown system error code " ^ Kernel.Int.to_string code)

let from_system_error_code = fun code -> from_system_error (Kernel.SystemError.from_code code)

let from_async_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Async.InvalidTimeoutNs _ -> Invalid_argument
  | Kernel.Async.InvalidMaxEvents _ -> Invalid_argument
  | Kernel.Async.System err -> from_system_error err

let message = fun __tmp1 ->
  match __tmp1 with
  | End_of_file -> "End of file"
  | Unexpected_end_of_file -> "Unexpected end of file"
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
  | Buffer_full -> "Buffer full"
  | Invalid_data -> "Invalid data"
  | Unknown_error message -> message
