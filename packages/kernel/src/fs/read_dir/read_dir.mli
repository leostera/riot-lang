type kind = File.kind =
  | RegularFile
  | Directory
  | SymbolicLink
  | CharacterDevice
  | BlockDevice
  | NamedPipe
  | Socket
  | Unknown
type entry = {
  path: Path.t;
  kind: kind;
}
type t
type error =
  | Closed
  | File of File.error

val error_to_string: error -> string

(**
   Use `open_dir path` to snapshot the current directory entry names for later iteration.

   The snapshot excludes `.` and `..`. Later filesystem mutations do not change which names the
   iterator returns.
*)
val open_dir: Path.t -> (t, error) Result.t

(**
   Use `read_name dir` to pull the next snapshotted entry name, if any.

   It returns `Ok None` once the iterator reaches the end of the snapshot.
*)
val read_name: t -> (string option, error) Result.t

(**
   Use `read_entry dir` to pull the next snapshotted relative path with its current kind.

   Entry kinds are resolved lazily through `Fs.File.symlink_metadata`, so this can fail if an
   entry disappears after `open_dir`.
*)
val read_entry: t -> (entry option, error) Result.t

(**
   Use `close dir` to end iteration explicitly.

   The current Unix implementation is snapshot-backed and does not hold an OS directory handle,
   but explicit close keeps the ownership contract stable across backends.
*)
val close: t -> (unit, error) Result.t
