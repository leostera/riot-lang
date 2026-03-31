open Global
module Ev = Kernel.Fs.Events

type kind = Ev.event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

let kind_to_string = fun k ->
    match k with
    | Created -> "CREATED"
    | Modified -> "MODIFIED"
    | Deleted -> "DELETED"
    | Renamed -> "RENAMED"
    | Metadata -> "METADATA"

type file_type =
  | File
  | Directory
  | Symlink
  | Unknown

let file_type_to_string = function
  | File -> "FILE"
  | Directory -> "DIRECTORY"
  | Symlink -> "SYMLINK"
  | Unknown -> "UNKNOWN"

type metadata_change = {
  inode_meta: bool;  (* Permissions, timestamps *)
  finder_info: bool;  (* macOS Finder metadata *)
  owner: bool;  (* File ownership *)
  xattr: bool;  (* Extended attributes *)
}

type system_flags = {
  own_event: bool;  (* This process caused it *)
  mount: bool;  (* Volume mount *)
  unmount: bool;  (* Volume unmount *)
  root_changed: bool;  (* Watched root changed *)
  must_scan_subdirs: bool;  (* Too many events, rescan needed *)
  user_dropped: bool;  (* Events lost in userspace *)
  kernel_dropped: bool;  (* Events lost in kernel *)
}

type t = {
  path: Path.t;
  kind: kind;
  event_id: int64;  (* Monotonic sequence number *)
  file_type: file_type;  (* What kind of filesystem object *)
  metadata: metadata_change;
  system: system_flags;
}

(* Decode all flags from kernel event *)

let from_kernel_event : Ev.event -> t = fun ev ->
    let has_flag flag = Int32.logand ev.flags flag != Int32.zero in
    {
      path = Path.v ev.path;
      kind = Ev.decode_event_kind ev.flags;
      event_id = ev.event_id;
      file_type = if has_flag Ev.flag_is_file then
        File
      else if has_flag Ev.flag_is_dir then
        Directory
      else if has_flag Ev.flag_is_symlink then
        Symlink
      else
        Unknown;
        metadata
        = {
          inode_meta = has_flag Ev.flag_inode_meta_mod;
          finder_info = has_flag Ev.flag_finder_info_mod;
          owner = has_flag Ev.flag_metadata;
          xattr = has_flag Ev.flag_xattr_mod;

        };
        system
        = {
          own_event = has_flag Ev.flag_own_event;
          mount = has_flag Ev.flag_mount;
          unmount = has_flag Ev.flag_unmount;
          root_changed = has_flag Ev.flag_root_changed;
          must_scan_subdirs = has_flag Ev.flag_must_scan_subdirs;
          user_dropped = has_flag Ev.flag_user_dropped;
          kernel_dropped = has_flag Ev.flag_kernel_dropped;

        };
    }

(* Convert event to JSON *)

let to_json : t -> Data.Json.t = fun t ->
    Data.Json.(obj
      [
        ("path", string (Path.to_string t.path));
        ("kind", string (kind_to_string t.kind));
        ("event_id", string (Int64.to_string t.event_id));
        ("file_type", string (file_type_to_string t.file_type));
        (
          "metadata",
          obj
            [
              ("inode_meta", bool t.metadata.inode_meta);
              ("finder_info", bool t.metadata.finder_info);
              ("owner", bool t.metadata.owner);
              ("xattr", bool t.metadata.xattr);

            ]
        );
        (
          "system",
          obj
            [
              ("own_event", bool t.system.own_event);
              ("mount", bool t.system.mount);
              ("unmount", bool t.system.unmount);
              ("root_changed", bool t.system.root_changed);
              ("must_scan_subdirs", bool t.system.must_scan_subdirs);
              ("user_dropped", bool t.system.user_dropped);
              ("kernel_dropped", bool t.system.kernel_dropped);

            ]
        );

      ])
