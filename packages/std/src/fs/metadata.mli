type t = Kernel.Fs.File.Metadata.t
(** File metadata *)

(** {1 File Properties} *)

val file_type :
  t ->
  [ `Regular | `Directory | `Symlink | `Block | `Character | `Fifo | `Socket ]
(** Get file type *)

val is_file : t -> bool
(** Returns true if this is a regular file *)

val is_dir : t -> bool
(** Returns true if this is a directory *)

val is_symlink : t -> bool
(** Returns true if this is a symbolic link *)

val len : t -> int
(** File size in bytes *)

val permissions : t -> Permissions.t
(** File permissions *)

(** {1 Timestamps} *)

val accessed : t -> float
(** Last access time (atime) as seconds since epoch *)

val modified : t -> float
(** Last modification time (mtime) as seconds since epoch *)

val created : t -> float option
(** Creation time (birth time, platform-specific). Returns None on Unix. *)

(** {1 Unix-specific} *)

val mode : t -> int
(** Unix mode bits *)

val uid : t -> int
(** User ID of owner *)

val gid : t -> int
(** Group ID of owner *)

val nlink : t -> int
(** Number of hard links *)

val ino : t -> int
(** Inode number *)

val dev : t -> int
(** Device ID *)

val rdev : t -> int
(** Device ID for special files *)
