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
