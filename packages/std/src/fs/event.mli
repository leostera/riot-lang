type kind = Kernel.Fs.Events.event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

val kind_to_string: kind -> string

type file_type =
  | File
  | Directory
  | Symlink
  | Unknown

val file_type_to_string: file_type -> string

type metadata_change = { inode_meta: bool; finder_info: bool; owner: bool; xattr: bool }

type system_flags = {
  own_event: bool;
  mount: bool;
  unmount: bool;
  root_changed: bool;
  must_scan_subdirs: bool;
  user_dropped: bool;
  kernel_dropped: bool;
}

type t = {
  path: Path.t;
  kind: kind;
  event_id: int64;
  file_type: file_type;
  metadata: metadata_change;
  system: system_flags;
}

val from_kernel_event: Kernel.Fs.Events.event -> t

(** Convert an event to JSON representation with all metadata *)
val to_json: t -> Data.Json.t
