open Prelude

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

let flag_created = 0x0000_0100l

let flag_removed = 0x0000_0200l

let flag_modified = 0x0000_1000l

let flag_renamed = 0x0000_0800l

let flag_metadata = 0x0000_4000l

let flag_is_file = 0x0001_0000l

let flag_is_dir = 0x0002_0000l

let flag_is_symlink = 0x0004_0000l

let flag_inode_meta_mod = 0x0000_0400l

let flag_finder_info_mod = 0x0000_2000l

let flag_xattr_mod = 0x0000_8000l

let flag_own_event = 0x0008_0000l

let flag_mount = 0x0000_0040l

let flag_unmount = 0x0000_0080l

let flag_root_changed = 0x0000_0020l

let flag_must_scan_subdirs = 0x0000_0001l

let flag_user_dropped = 0x0000_0002l

let flag_kernel_dropped = 0x0000_0004l

let decode_event_kind = fun flags ->
  let has_flag flag = Int32.logand flags flag <> Int32.zero in
  if has_flag flag_created then
    Created
  else if has_flag flag_removed then
    Deleted
  else if has_flag flag_modified then
    Modified
  else if has_flag flag_renamed then
    Renamed
  else
    Metadata

let error_to_string = fun value ->
  match value with
  | Closed -> "filesystem watcher is closed"
  | AlreadyWatching -> "filesystem watcher already has an active root"
  | System error -> System_error.to_string error

module FFI = struct
  type watcher

  type raw_event = string * int32 * int64

  external create: unit -> (watcher, int) Result.t = "kernel_new_fs_events_create"

  external watch: watcher -> string -> float -> (watch_id, int) Result.t = "kernel_new_fs_events_watch"

  external unwatch: watcher -> watch_id -> (unit, int) Result.t = "kernel_new_fs_events_unwatch"

  external poll: watcher -> (raw_event array, int) Result.t = "kernel_new_fs_events_poll"

  external stop: watcher -> (unit, int) Result.t = "kernel_new_fs_events_stop"

  external read_fd: watcher -> int = "kernel_new_fs_events_read_fd"
end

type t = {
  watcher: FFI.watcher;
}

let of_system_error = fun error ->
  match error with
  | System_error.BadFileDescriptor -> Closed
  | system_error -> System system_error

let of_code_error = fun code -> of_system_error (System_error.of_code code)

let create = fun () ->
  Result.map (fun watcher -> { watcher }) (Result.map_error of_code_error (FFI.create ()))

let watch = fun t ~path ~latency ->
  Result.map_error of_code_error (FFI.watch t.watcher (Path.to_string path) latency)

let unwatch = fun t watch_id ->
  Result.map_error of_code_error (FFI.unwatch t.watcher watch_id)

let event_of_raw = fun (path, flags, event_id) -> { path = Path.of_string path; flags; event_id }

let poll = fun t ->
  Result.map
    (fun raw_events ->
      let rec loop index acc =
        if index < 0 then
          acc
        else
          loop (index - 1) (event_of_raw (Array.get raw_events index) :: acc)
      in
      loop (Array.length raw_events - 1) [])
    (Result.map_error of_code_error (FFI.poll t.watcher))

let stop = fun t ->
  Result.map_error of_code_error (FFI.stop t.watcher)

let to_source = fun t ->
  let module Source = struct
    type nonrec t = t

    let register = fun state selector token interest ->
      Async.Adapter.Selector.register selector ~fd:(FFI.read_fd state.watcher) ~token ~interest

    let reregister = fun state selector token interest ->
      Async.Adapter.Selector.reregister selector ~fd:(FFI.read_fd state.watcher) ~token ~interest

    let deregister = fun state selector ->
      Async.Adapter.Selector.deregister selector ~fd:(FFI.read_fd state.watcher)
  end in
  Async.Source.make (module Source) t
