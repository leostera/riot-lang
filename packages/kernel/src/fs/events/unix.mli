type t
type watch_id = int
type event = {
  path: Path.t;
  flags: int32;
  event_id: int64;
}
type event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata
type error =
  | Closed
  | AlreadyWatching
  | System of System_error.t
val error_to_string: error -> string

(**
   Use `create ()` to allocate a filesystem watcher handle.

   The watcher is inert until `watch` installs a root.
*)
val create: unit -> (t, error) Result.t

(**
   Use `watch watcher ~path ~latency` to begin watching one root path.

   The current Unix backend supports a single active root per watcher handle.
*)
val watch: t -> path:Path.t -> latency:float -> (watch_id, error) Result.t

(**
   Use `unwatch watcher watch_id` to stop the current watch root.

   Unknown watch identifiers are ignored so callers can treat cleanup as best-effort.
*)
val unwatch: t -> watch_id -> (unit, error) Result.t

module Flag: sig
  val created: int32

  val removed: int32

  val modified: int32

  val renamed: int32

  val metadata: int32

  val is_file: int32

  val is_dir: int32

  val is_symlink: int32

  val inode_meta_mod: int32

  val finder_info_mod: int32

  val xattr_mod: int32

  val own_event: int32

  val mount: int32

  val unmount: int32

  val root_changed: int32

  val must_scan_subdirs: int32

  val user_dropped: int32

  val kernel_dropped: int32
end

val decode_event_kind: int32 -> event_kind

(**
   Use `poll watcher` to read every currently buffered event without blocking.

   When no complete event is ready, it reports `System WouldBlock` so higher layers can wait
   through `to_source`.
*)
val poll: t -> (event list, error) Result.t

(** Use `stop watcher` to release watcher resources permanently. *)
val stop: t -> (unit, error) Result.t

(** Use `to_source watcher` to integrate watcher readiness with the async selector. *)
val to_source: t -> Async.Source.t
