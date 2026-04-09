type t =
  | EndOfFile
  | PermissionDenied
  | NoSuchFileOrDirectory
  | Interrupted
  | InputOutput
  | BadFileDescriptor
  | ResourceBusy
  | AlreadyExists
  | InvalidArgument
  | NoSpaceLeft
  | BrokenPipe
  | WouldBlock
  | NotDirectory
  | IsDirectory
  | NotSupported
  | AddressInUse
  | AddressNotAvailable
  | ConnectionRefused
  | ConnectionReset
  | TimedOut
  | NetworkUnreachable
  | DestinationAddressRequired
  | NotConnected
  | ConnectionAborted
  | MessageTooLong
  | NoSuchProcess
  | DirectoryNotEmpty
  | Unknown of int

let code_end_of_file = 1

let code_permission_denied = 2

let code_no_such_file_or_directory = 3

let code_interrupted = 4

let code_input_output = 5

let code_bad_file_descriptor = 6

let code_resource_busy = 7

let code_already_exists = 8

let code_invalid_argument = 9

let code_no_space_left = 10

let code_broken_pipe = 11

let code_would_block = 12

let code_not_directory = 13

let code_is_directory = 14

let code_not_supported = 15

let code_address_in_use = 16

let code_address_not_available = 17

let code_connection_refused = 18

let code_connection_reset = 19

let code_timed_out = 20

let code_network_unreachable = 21

let code_destination_address_required = 22

let code_not_connected = 23

let code_connection_aborted = 24

let code_message_too_long = 25

let code_no_such_process = 26

let code_directory_not_empty = 27

let of_code = fun value ->
  match value with
  | 1 -> EndOfFile
  | 2 -> PermissionDenied
  | 3 -> NoSuchFileOrDirectory
  | 4 -> Interrupted
  | 5 -> InputOutput
  | 6 -> BadFileDescriptor
  | 7 -> ResourceBusy
  | 8 -> AlreadyExists
  | 9 -> InvalidArgument
  | 10 -> NoSpaceLeft
  | 11 -> BrokenPipe
  | 12 -> WouldBlock
  | 13 -> NotDirectory
  | 14 -> IsDirectory
  | 15 -> NotSupported
  | 16 -> AddressInUse
  | 17 -> AddressNotAvailable
  | 18 -> ConnectionRefused
  | 19 -> ConnectionReset
  | 20 -> TimedOut
  | 21 -> NetworkUnreachable
  | 22 -> DestinationAddressRequired
  | 23 -> NotConnected
  | 24 -> ConnectionAborted
  | 25 -> MessageTooLong
  | 26 -> NoSuchProcess
  | 27 -> DirectoryNotEmpty
  | code -> Unknown code

let to_string = fun value ->
  match value with
  | EndOfFile -> "end of file"
  | PermissionDenied -> "permission denied"
  | NoSuchFileOrDirectory -> "no such file or directory"
  | Interrupted -> "interrupted system call"
  | InputOutput -> "input/output error"
  | BadFileDescriptor -> "bad file descriptor"
  | ResourceBusy -> "resource busy"
  | AlreadyExists -> "already exists"
  | InvalidArgument -> "invalid argument"
  | NoSpaceLeft -> "no space left on device"
  | BrokenPipe -> "broken pipe"
  | WouldBlock -> "operation would block"
  | NotDirectory -> "not a directory"
  | IsDirectory -> "is a directory"
  | NotSupported -> "operation not supported"
  | AddressInUse -> "address already in use"
  | AddressNotAvailable -> "address not available"
  | ConnectionRefused -> "connection refused"
  | ConnectionReset -> "connection reset by peer"
  | TimedOut -> "timed out"
  | NetworkUnreachable -> "network unreachable"
  | DestinationAddressRequired -> "destination address required"
  | NotConnected -> "socket is not connected"
  | ConnectionAborted -> "connection aborted"
  | MessageTooLong -> "message too long"
  | NoSuchProcess -> "no such process"
  | DirectoryNotEmpty -> "directory not empty"
  | Unknown _ -> "unknown kernel error"

let is_would_block = fun value ->
  match value with
  | WouldBlock -> true
  | _ -> false

external panic: string -> 'a = "kernel_new_panic"
