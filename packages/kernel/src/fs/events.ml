open Global0
  open IO
  open Collections
open Async

type t = int * Fd.t  (* context_ptr, read_fd *)
type watch_id = int

type event = {
  path : string;
  flags : int32;
  event_id : int64;
}

type event_kind =
  | Created
  | Modified
  | Deleted
  | Renamed
  | Metadata

(* Platform-specific FFI - macOS FSEvents *)
external fsevents_create : unit -> int * Fd.t = "kernel_fsevents_create"
external fsevents_watch : int -> string -> float -> unit = "kernel_fsevents_watch"
external fsevents_stop : int -> unit = "kernel_fsevents_stop"

(* FSEvents flag constants - Basic event types *)
let flag_created = 0x00000100l
let flag_removed = 0x00000200l
let flag_modified = 0x00001000l
let flag_renamed = 0x00000800l
let flag_metadata = 0x00004000l

(* File type flags *)
let flag_is_file = 0x00010000l
let flag_is_dir = 0x00020000l
let flag_is_symlink = 0x00040000l

(* Metadata change flags *)
let flag_inode_meta_mod = 0x00000400l
let flag_finder_info_mod = 0x00002000l
let flag_xattr_mod = 0x00008000l

(* System flags *)
let flag_own_event = 0x00080000l
let flag_mount = 0x00000040l
let flag_unmount = 0x00000080l
let flag_root_changed = 0x00000020l
let flag_must_scan_subdirs = 0x00000001l
let flag_user_dropped = 0x00000002l
let flag_kernel_dropped = 0x00000004l

let decode_event_kind flags =
  let has_flag flag = Int32.logand flags flag != Int32.zero in
  if has_flag flag_created then Created
  else if has_flag flag_removed then Deleted
  else if has_flag flag_modified then Modified
  else if has_flag flag_renamed then Renamed
  else Metadata

let create () =
  try
    let (ctx_ptr, read_fd) = fsevents_create () in
    Ok (ctx_ptr, read_fd)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let watch t ~path ~latency =
  try
    let (ctx_ptr, _read_fd) = t in
    fsevents_watch ctx_ptr path latency;
    Ok 0  (* FSEvents doesn't return per-path IDs *)
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let unwatch _t _watch_id =
  (* FSEvents watches are stopped when context is destroyed *)
  Ok ()

let get_fd t =
  let (_ctx_ptr, read_fd) = t in
  read_fd

let stop t =
  try
    let (ctx_ptr, read_fd) = t in
    fsevents_stop ctx_ptr;
    File.close_fd read_fd;
    Ok ()
  with Unix.Unix_error (err, _, _) -> Error (IO.error_of_unix err)

let to_source t =
  let module Src = struct
    type nonrec t = t

    let register (_ctx_ptr, fd) selector token interest =
      Adapter.Selector.register selector ~fd ~token ~interest

    let reregister (_ctx_ptr, fd) selector token interest =
      Adapter.Selector.reregister selector ~fd ~token ~interest

    let deregister (_ctx_ptr, fd) selector = Adapter.Selector.deregister selector ~fd
  end in
  Source.make (module Src) t
