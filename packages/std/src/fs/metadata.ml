open Global

type t = Kernel.Fs.File.Metadata.t

type file_type =
  | Regular
  | Directory
  | Symlink
  | Block
  | Character
  | Fifo
  | Socket
  | Unknown

let file_type = fun t ->
  match Kernel.Fs.File.Metadata.file_type t with
  | Kernel.Fs.File.RegularFile -> Regular
  | Kernel.Fs.File.Directory -> Directory
  | Kernel.Fs.File.SymbolicLink -> Symlink
  | Kernel.Fs.File.BlockDevice -> Block
  | Kernel.Fs.File.CharacterDevice -> Character
  | Kernel.Fs.File.NamedPipe -> Fifo
  | Kernel.Fs.File.Socket -> Socket
  | Kernel.Fs.File.Unknown -> Unknown

let is_file = Kernel.Fs.File.Metadata.is_file

let is_dir = Kernel.Fs.File.Metadata.is_dir

let is_symlink = Kernel.Fs.File.Metadata.is_symlink

let len = fun t -> Kernel.Int64.to_int (Kernel.Fs.File.Metadata.len t)

let permissions = fun t -> Permissions.from_mode (Kernel.Fs.File.Metadata.permissions t)

let accessed = fun t ->
  Kernel.Int64.to_float (Kernel.Fs.File.Metadata.accessed_ns t) /. 1_000_000_000.0

let modified = fun t ->
  Kernel.Int64.to_float (Kernel.Fs.File.Metadata.modified_ns t) /. 1_000_000_000.0

let created = fun _t -> None

let mode = Kernel.Fs.File.Metadata.mode

let uid = Kernel.Fs.File.Metadata.uid

let gid = Kernel.Fs.File.Metadata.gid

let nlink = Kernel.Fs.File.Metadata.nlink

let ino = Kernel.Fs.File.Metadata.ino

let dev = Kernel.Fs.File.Metadata.dev

let rdev = Kernel.Fs.File.Metadata.rdev
