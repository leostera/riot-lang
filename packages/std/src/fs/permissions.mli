type t
(** Unix permission bits *)

(** {1 Conversion} *)

val of_mode : int -> t
(** Create from Unix mode bits *)

val to_mode : t -> int
(** Convert to Unix mode bits *)

(** {1 Readonly Checks} *)

val readonly : t -> bool
(** Returns true if no write bits are set (owner, group, or others).

    Note: This does not consider ACLs or actual user permissions *)

val set_readonly : t -> bool -> t
(** Set or clear write permissions for owner, group, and others.

    Warning: set_readonly false makes file world-writable on Unix *)

(** {1 Permission Bits} *)

val user_read : t -> bool
val user_write : t -> bool
val user_execute : t -> bool

val group_read : t -> bool
val group_write : t -> bool
val group_execute : t -> bool

val other_read : t -> bool
val other_write : t -> bool
val other_execute : t -> bool

(** {1 Common Modes} *)

val read_write : t
(** rw-r--r-- (0644) - Owner read/write, group/others read-only *)

val executable : t
(** rwxr-xr-x (0755) - Owner read/write/execute, group/others read/execute *)

val private_read_write : t
(** rw------- (0600) - Owner read/write only, no access for others *)

val private_executable : t
(** rwx------ (0700) - Owner read/write/execute only, no access for others *)