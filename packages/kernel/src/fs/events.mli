(** Low-level file system event watching *)

open Global0
open IO

(** File system event watcher handle *)
(** Watch identifier *)
type t
type watch_id = int
type event = {
  path : string;
  flags : int32;
  event_id : int64;
}
(** Create a new file system watcher *)
type event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata
val create : unit -> (t, error) result

(** Watch a path for changes *)
val watch : t -> path:string -> latency:float -> (watch_id, error) result

(** Stop watching a path *)
val unwatch : t -> watch_id -> (unit, error) result

(** Get the underlying file descriptor for reading events *)
val get_fd : t -> Fd.t

(** Flag constants *)
val flag_created : int32

val flag_removed : int32

val flag_modified : int32

val flag_renamed : int32

val flag_metadata : int32

val flag_is_file : int32

val flag_is_dir : int32

val flag_is_symlink : int32

val flag_inode_meta_mod : int32

val flag_finder_info_mod : int32

val flag_xattr_mod : int32

val flag_own_event : int32

val flag_mount : int32

val flag_unmount : int32

val flag_root_changed : int32

val flag_must_scan_subdirs : int32

val flag_user_dropped : int32

val flag_kernel_dropped : int32

(** Decode event flags into kind *)
val decode_event_kind : int32 -> event_kind

(** Stop the watcher and release resources *)
val stop : t -> (unit, error) result

val to_source : t -> Async.Source.t
