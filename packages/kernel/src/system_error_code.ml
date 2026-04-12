let from_system_error = fun value ->
  match value with
  | System_error.EndOfFile -> 1
  | System_error.PermissionDenied -> 2
  | System_error.NoSuchFileOrDirectory -> 3
  | System_error.Interrupted -> 4
  | System_error.InputOutput -> 5
  | System_error.BadFileDescriptor -> 6
  | System_error.ResourceBusy -> 7
  | System_error.AlreadyExists -> 8
  | System_error.InvalidArgument -> 9
  | System_error.NoSpaceLeft -> 10
  | System_error.BrokenPipe -> 11
  | System_error.WouldBlock -> 12
  | System_error.NotDirectory -> 13
  | System_error.IsDirectory -> 14
  | System_error.NotSupported -> 15
  | System_error.AddressInUse -> 16
  | System_error.AddressNotAvailable -> 17
  | System_error.ConnectionRefused -> 18
  | System_error.ConnectionReset -> 19
  | System_error.TimedOut -> 20
  | System_error.NetworkUnreachable -> 21
  | System_error.DestinationAddressRequired -> 22
  | System_error.NotConnected -> 23
  | System_error.ConnectionAborted -> 24
  | System_error.MessageTooLong -> 25
  | System_error.NoSuchProcess -> 26
  | System_error.DirectoryNotEmpty -> 27
  | System_error.Unknown code -> code

let broken_pipe = from_system_error System_error.BrokenPipe

let no_such_file_or_directory = from_system_error System_error.NoSuchFileOrDirectory
