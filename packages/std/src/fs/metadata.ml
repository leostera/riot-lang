type t = Kernel.Fs.File.Metadata.t

let file_type t =
  match Kernel.IO.file_kind_of_unix (Kernel.Fs.File.Metadata.kind t) with
  | Kernel.IO.Regular -> `Regular
  | Kernel.IO.Directory -> `Directory
  | Kernel.IO.Symlink -> `Symlink
  | Kernel.IO.Block -> `Block
  | Kernel.IO.Character -> `Character
  | Kernel.IO.Fifo -> `Fifo
  | Kernel.IO.Socket -> `Socket

let is_file t =
  Kernel.IO.file_kind_of_unix (Kernel.Fs.File.Metadata.kind t)
  = Kernel.IO.Regular

let is_dir t =
  Kernel.IO.file_kind_of_unix (Kernel.Fs.File.Metadata.kind t)
  = Kernel.IO.Directory

let is_symlink t =
  Kernel.IO.file_kind_of_unix (Kernel.Fs.File.Metadata.kind t)
  = Kernel.IO.Symlink

let len t = Kernel.Fs.File.Metadata.size t
let permissions t = Permissions.of_mode (Kernel.Fs.File.Metadata.perm t)
let accessed t = Kernel.Fs.File.Metadata.atime t
let modified t = Kernel.Fs.File.Metadata.mtime t
let created _t = None
let mode t = Kernel.Fs.File.Metadata.perm t
let uid t = Kernel.Fs.File.Metadata.uid t
let gid t = Kernel.Fs.File.Metadata.gid t
let nlink t = Kernel.Fs.File.Metadata.nlink t
let ino t = Kernel.Fs.File.Metadata.ino t
let dev t = Kernel.Fs.File.Metadata.dev t
let rdev t = Kernel.Fs.File.Metadata.rdev t
