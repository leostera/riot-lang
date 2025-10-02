type dir_handle = Unix.dir_handle

type error =
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

let error_of_unix e =
  match e with
  | Unix.EACCES -> Permission_denied
  | Unix.ENOENT -> No_such_file_or_directory
  | Unix.EINTR -> Interrupted_system_call
  | Unix.EIO -> Input_output_error
  | Unix.EBADF -> Bad_file_descriptor
  | Unix.EAGAIN -> Resource_unavailable_try_again
  | Unix.ENOMEM -> Out_of_memory
  | Unix.EPERM -> Permission_denied_on_file
  | Unix.EFAULT -> Bad_address
  | Unix.EBUSY -> Resource_busy
  | Unix.EEXIST -> File_exists
  | Unix.EXDEV -> Cross_device_link
  | Unix.EINVAL -> Invalid_argument
  | Unix.ENFILE -> Too_many_open_files_in_system
  | Unix.EMFILE -> Too_many_open_files
  | Unix.ENOTTY -> Invalid_operation_on_device
  | Unix.EFBIG -> File_too_large
  | Unix.ENOSPC -> No_space_left_on_device
  | Unix.ESPIPE -> Illegal_seek
  | Unix.EROFS -> Read_only_filesystem
  | Unix.EMLINK -> Too_many_links
  | Unix.EPIPE -> Broken_pipe
  | Unix.EDOM -> Numerical_argument_out_of_domain
  | Unix.ERANGE -> Numerical_result_out_of_range
  | Unix.EDEADLK -> Resource_deadlock_would_occur
  | Unix.ENAMETOOLONG -> Filename_too_long
  | Unix.ENOLCK -> No_locks_available
  | Unix.ENOSYS -> Function_not_implemented
  | Unix.ENOTEMPTY -> Directory_not_empty
  | Unix.ELOOP -> Too_many_symbolic_links
  | Unix.EWOULDBLOCK -> Operation_would_block
  | Unix.ENOTSOCK -> Socket_operation_on_non_socket
  | Unix.EDESTADDRREQ -> Destination_address_required
  | Unix.EMSGSIZE -> Message_too_long
  | Unix.EPROTOTYPE -> Protocol_wrong_type_for_socket
  | Unix.ENOPROTOOPT -> Protocol_not_available
  | Unix.EPROTONOSUPPORT -> Protocol_not_supported
  | Unix.ESOCKTNOSUPPORT -> Socket_type_not_supported
  | Unix.EOPNOTSUPP -> Operation_not_supported
  | Unix.EPFNOSUPPORT -> Protocol_family_not_supported
  | Unix.EAFNOSUPPORT -> Address_family_not_supported
  | Unix.EADDRINUSE -> Address_already_in_use
  | Unix.EADDRNOTAVAIL -> Cannot_assign_requested_address
  | Unix.ENETDOWN -> Network_is_down
  | Unix.ENETUNREACH -> Network_is_unreachable
  | Unix.ENETRESET -> Network_dropped_connection_on_reset
  | Unix.ECONNABORTED -> Software_caused_connection_abort
  | Unix.ECONNRESET -> Connection_reset_by_peer
  | Unix.ENOBUFS -> No_buffer_space_available
  | Unix.EISCONN -> Transport_endpoint_already_connected
  | Unix.ENOTCONN -> Transport_endpoint_not_connected
  | Unix.ESHUTDOWN -> Cannot_send_after_transport_endpoint_shutdown
  | Unix.ETOOMANYREFS -> Too_many_references
  | Unix.ETIMEDOUT -> Connection_timed_out
  | Unix.ECONNREFUSED -> Connection_refused
  | Unix.EHOSTDOWN -> Host_is_down
  | Unix.EHOSTUNREACH -> No_route_to_host
  | Unix.EALREADY -> Operation_already_in_progress
  | Unix.EINPROGRESS -> Operation_now_in_progress
  | _ -> Unknown_error (Unix.error_message e)

let error_to_unix = function
  | Permission_denied -> Unix.EACCES
  | No_such_file_or_directory -> Unix.ENOENT
  | Interrupted_system_call -> Unix.EINTR
  | Input_output_error -> Unix.EIO
  | Bad_file_descriptor -> Unix.EBADF
  | Resource_unavailable_try_again -> Unix.EAGAIN
  | Out_of_memory -> Unix.ENOMEM
  | Permission_denied_on_file -> Unix.EPERM
  | Bad_address -> Unix.EFAULT
  | Resource_busy -> Unix.EBUSY
  | File_exists -> Unix.EEXIST
  | Cross_device_link -> Unix.EXDEV
  | Invalid_argument -> Unix.EINVAL
  | Too_many_open_files_in_system -> Unix.ENFILE
  | Too_many_open_files -> Unix.EMFILE
  | Invalid_operation_on_device -> Unix.ENOTTY
  | File_too_large -> Unix.EFBIG
  | No_space_left_on_device -> Unix.ENOSPC
  | Illegal_seek -> Unix.ESPIPE
  | Read_only_filesystem -> Unix.EROFS
  | Too_many_links -> Unix.EMLINK
  | Broken_pipe -> Unix.EPIPE
  | Numerical_argument_out_of_domain -> Unix.EDOM
  | Numerical_result_out_of_range -> Unix.ERANGE
  | Resource_deadlock_would_occur -> Unix.EDEADLK
  | Filename_too_long -> Unix.ENAMETOOLONG
  | No_locks_available -> Unix.ENOLCK
  | Function_not_implemented -> Unix.ENOSYS
  | Directory_not_empty -> Unix.ENOTEMPTY
  | Too_many_symbolic_links -> Unix.ELOOP
  | Operation_would_block -> Unix.EWOULDBLOCK
  | Socket_operation_on_non_socket -> Unix.ENOTSOCK
  | Destination_address_required -> Unix.EDESTADDRREQ
  | Message_too_long -> Unix.EMSGSIZE
  | Protocol_wrong_type_for_socket -> Unix.EPROTOTYPE
  | Protocol_not_available -> Unix.ENOPROTOOPT
  | Protocol_not_supported -> Unix.EPROTONOSUPPORT
  | Socket_type_not_supported -> Unix.ESOCKTNOSUPPORT
  | Operation_not_supported -> Unix.EOPNOTSUPP
  | Protocol_family_not_supported -> Unix.EPFNOSUPPORT
  | Address_family_not_supported -> Unix.EAFNOSUPPORT
  | Address_already_in_use -> Unix.EADDRINUSE
  | Cannot_assign_requested_address -> Unix.EADDRNOTAVAIL
  | Network_is_down -> Unix.ENETDOWN
  | Network_is_unreachable -> Unix.ENETUNREACH
  | Network_dropped_connection_on_reset -> Unix.ENETRESET
  | Software_caused_connection_abort -> Unix.ECONNABORTED
  | Connection_reset_by_peer -> Unix.ECONNRESET
  | No_buffer_space_available -> Unix.ENOBUFS
  | Transport_endpoint_already_connected -> Unix.EISCONN
  | Transport_endpoint_not_connected -> Unix.ENOTCONN
  | Cannot_send_after_transport_endpoint_shutdown -> Unix.ESHUTDOWN
  | Too_many_references -> Unix.ETOOMANYREFS
  | Connection_timed_out -> Unix.ETIMEDOUT
  | Connection_refused -> Unix.ECONNREFUSED
  | Host_is_down -> Unix.EHOSTDOWN
  | No_route_to_host -> Unix.EHOSTUNREACH
  | Operation_already_in_progress -> Unix.EALREADY
  | Operation_now_in_progress -> Unix.EINPROGRESS
  | Unknown_error _ -> Unix.EINVAL

let error_message e = Unix.error_message (error_to_unix e)

type file_kind =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket

let file_kind_of_unix = function
  | Unix.S_REG -> Regular
  | Unix.S_DIR -> Directory
  | Unix.S_LNK -> Symlink
  | Unix.S_BLK -> Block
  | Unix.S_CHR -> Character
  | Unix.S_FIFO -> Fifo
  | Unix.S_SOCK -> Socket

let file_kind_to_unix = function
  | Regular -> Unix.S_REG
  | Directory -> Unix.S_DIR
  | Symlink -> Unix.S_LNK
  | Block -> Unix.S_BLK
  | Character -> Unix.S_CHR
  | Fifo -> Unix.S_FIFO
  | Socket -> Unix.S_SOCK
