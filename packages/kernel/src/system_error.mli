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

val from_code: int -> t

val to_string: t -> string

val would_block: t -> bool

(**
   Use `panic message` only for invariant violations or test/bench scaffolding where continuing
   would be meaningless. Normal kernel paths should return typed errors instead. 
*)
external panic: string -> 'a = "kernel_new_panic"
