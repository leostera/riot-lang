open Global

type t = Kernel.Fs.File.Metadata.t

let file_type = fun t ->
    match Kernel.Fs.File.Metadata.kind t with
    | Kernel.IO.Regular -> `Regular
    | Kernel.IO.Directory -> `Directory
    | Kernel.IO.Symlink -> `Symlink
    | Kernel.IO.Block -> `Block
    | Kernel.IO.Character -> `Character
    | Kernel.IO.Fifo -> `Fifo
    | Kernel.IO.Socket -> `Socket

let is_file = fun t -> Kernel.Fs.File.Metadata.kind t = Kernel.IO.Regular

let is_dir = fun t -> Kernel.Fs.File.Metadata.kind t = Kernel.IO.Directory

let is_symlink = fun t -> Kernel.Fs.File.Metadata.kind t = Kernel.IO.Symlink

let len = fun t -> Kernel.Fs.File.Metadata.size t

let permissions = fun t -> Permissions.of_mode (Kernel.Fs.File.Metadata.perm t)

let accessed = fun t -> Kernel.Fs.File.Metadata.atime t

let modified = fun t -> Kernel.Fs.File.Metadata.mtime t

let created = fun _t -> None

let mode = fun t -> Kernel.Fs.File.Metadata.perm t

let uid = fun t -> Kernel.Fs.File.Metadata.uid t

let gid = fun t -> Kernel.Fs.File.Metadata.gid t

let nlink = fun t -> Kernel.Fs.File.Metadata.nlink t

let ino = fun t -> Kernel.Fs.File.Metadata.ino t

let dev = fun t -> Kernel.Fs.File.Metadata.dev t

let rdev = fun t -> Kernel.Fs.File.Metadata.rdev t
