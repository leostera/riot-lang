open Prelude

let ( let* ) = Result.and_then

type t = int

type error =
  | Invalid_slice of { pos: int; len: int; buffer_len: int }
  | System of System_error.t

type kind =
  | Regular_file
  | Directory
  | Symbolic_link
  | Character_device
  | Block_device
  | Named_pipe
  | Socket
  | Unknown

module Metadata = struct
  type t = {
    kind: kind;
    perm: int;
    size: int64;
    nlink: int;
    uid: int;
    gid: int;
    dev: int;
    ino: int;
    rdev: int;
    accessed_ns: int64;
    modified_ns: int64;
    changed_ns: int64;
  }

  let file_type = fun metadata -> metadata.kind

  let is_file = fun metadata -> metadata.kind = Regular_file

  let is_dir = fun metadata -> metadata.kind = Directory

  let is_symlink = fun metadata -> metadata.kind = Symbolic_link

  let permissions = fun metadata -> metadata.perm

  let mode = permissions

  let len = fun metadata -> metadata.size

  let nlink = fun metadata -> metadata.nlink

  let uid = fun metadata -> metadata.uid

  let gid = fun metadata -> metadata.gid

  let dev = fun metadata -> metadata.dev

  let ino = fun metadata -> metadata.ino

  let rdev = fun metadata -> metadata.rdev

  let accessed_ns = fun metadata -> metadata.accessed_ns

  let modified_ns = fun metadata -> metadata.modified_ns

  let changed_ns = fun metadata -> metadata.changed_ns
end

type open_flag =
  | Read_only
  | Write_only
  | Read_write
  | Create
  | Truncate
  | Append
  | Exclusive

type pipe = {
  read_end: t;
  write_end: t;
}

let flag_read_only = 1

let flag_write_only = 1 lsl 1

let flag_read_write = 1 lsl 2

let flag_create = 1 lsl 3

let flag_truncate = 1 lsl 4

let flag_append = 1 lsl 5

let flag_exclusive = 1 lsl 6

let kind_regular_file = 0

let kind_directory = 1

let kind_symbolic_link = 2

let kind_character_device = 3

let kind_block_device = 4

let kind_named_pipe = 5

let kind_socket = 6

let kind_unknown = 7

let kind_of_code = function
  | 0 -> Regular_file
  | 1 -> Directory
  | 2 -> Symbolic_link
  | 3 -> Character_device
  | 4 -> Block_device
  | 5 -> Named_pipe
  | 6 -> Socket
  | _ -> Unknown

let metadata_of_tuple = fun (kind_code, perm, size, link_count, owner_uid, owner_gid, device, inode, raw_device, accessed_time_ns, modified_time_ns, changed_time_ns) ->
  Metadata.{
    kind = kind_of_code kind_code;
    perm;
    size;
    nlink = link_count;
    uid = owner_uid;
    gid = owner_gid;
    dev = device;
    ino = inode;
    rdev = raw_device;
    accessed_ns = accessed_time_ns;
    modified_ns = modified_time_ns;
    changed_ns = changed_time_ns;
  }

let flags_to_mask = fun flags ->
  let rec loop acc = function
    | [] -> acc
    | Read_only :: rest -> loop (acc lor flag_read_only) rest
    | Write_only :: rest -> loop (acc lor flag_write_only) rest
    | Read_write :: rest -> loop (acc lor flag_read_write) rest
    | Create :: rest -> loop (acc lor flag_create) rest
    | Truncate :: rest -> loop (acc lor flag_truncate) rest
    | Append :: rest -> loop (acc lor flag_append) rest
    | Exclusive :: rest -> loop (acc lor flag_exclusive) rest
  in
  loop 0 flags

module FFI = struct
  external open_file: string -> int -> int -> (t, int) Result.t = "kernel_new_fs_file_open"

  external close: t -> (unit, int) Result.t = "kernel_new_fs_file_close"

  external read: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_read"

  external write: t -> bytes -> int -> int -> (int, int) Result.t = "kernel_new_fs_file_write"

  external readv: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_fs_file_readv"

  external writev: t -> IO.Iovec.t -> (int, int) Result.t = "kernel_new_fs_file_writev"

  external pipe: unit -> ((t * t), int) Result.t = "kernel_new_fs_file_pipe"

  external mkdir: string -> int -> (unit, int) Result.t = "kernel_new_fs_file_mkdir"

  external rmdir: string -> (unit, int) Result.t = "kernel_new_fs_file_rmdir"

  external remove: string -> (unit, int) Result.t = "kernel_new_fs_file_remove"

  external rename: string -> string -> (unit, int) Result.t = "kernel_new_fs_file_rename"

  external link: string -> string -> (unit, int) Result.t = "kernel_new_fs_file_link"

  external symlink: string -> string -> (unit, int) Result.t = "kernel_new_fs_file_symlink"

  external readlink: string -> (string, int) Result.t = "kernel_new_fs_file_readlink"

  external realpath: string -> (string, int) Result.t = "kernel_new_fs_file_realpath"

  external stat:
    string ->
    ((int * int * int64 * int * int * int * int * int * int * int64 * int64 * int64), int) Result.t
    = "kernel_new_fs_file_stat"

  external lstat:
    string ->
    ((int * int * int64 * int * int * int * int * int * int * int64 * int64 * int64), int) Result.t
    = "kernel_new_fs_file_lstat"

  external fstat:
    t ->
    ((int * int * int64 * int * int * int * int * int * int * int64 * int64 * int64), int) Result.t
    = "kernel_new_fs_file_fstat"

  external readdir: string -> (string array, int) Result.t = "kernel_new_fs_file_readdir"

  external getcwd: unit -> (string, int) Result.t = "kernel_new_fs_file_getcwd"

  external chdir: string -> (unit, int) Result.t = "kernel_new_fs_file_chdir"

  external is_tty: t -> bool = "kernel_new_fs_file_isatty"
end

let open_file = fun path flags ~perm ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.open_file (Path.to_string path) (flags_to_mask flags) perm)

let open_read = fun path -> open_file path [ Read_only ] ~perm:0

let open_write = fun ?(create = true) ?(truncate = true) ?(append = false) ?(perm = 0o644) path ->
  let flags =
    let flags =
      if create then
        Create :: [ Write_only ]
      else
        [ Write_only ]
    in
    let flags =
      if truncate then
        Truncate :: flags
      else
        flags
    in
    if append then
      Append :: flags
    else
      flags
  in
  open_file path flags ~perm

let close = fun fd ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.close fd)

let error_to_string = function
  | Invalid_slice { pos; len; buffer_len } -> String.concat
    ""
    [
      "invalid buffer slice: pos=";
      Int.to_string pos;
      ", len=";
      Int.to_string len;
      ", buffer_len=";
      Int.to_string buffer_len;
    ]
  | System error -> System_error.to_string error

let validate_slice = fun buf ~pos ~len ->
  if pos < 0 || len < 0 || pos + len > Bytes.length buf then
    Result.Error (Invalid_slice { pos; len; buffer_len = Bytes.length buf })
  else
    Result.Ok ()

let read = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.read fd buf pos len)

let write = fun fd ?(pos = 0) ?len buf ->
  let len = Option.unwrap_or len ~default:((Bytes.length buf - pos)) in
  let* () = validate_slice buf ~pos ~len in
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.write fd buf pos len)

let read_vectored = fun fd iov ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.readv fd iov)

let write_vectored = fun fd iov ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.writev fd iov)

let pipe = fun () ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map (fun (read_end, write_end) -> { read_end; write_end }) (FFI.pipe ()))

let create_dir = fun path ~perm ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.mkdir (Path.to_string path) perm)

let remove_dir = fun path ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.rmdir (Path.to_string path))

let remove_file = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.remove (Path.to_string path))

let rename = fun ~src ~dst ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.rename (Path.to_string src) (Path.to_string dst))

let hard_link = fun ~src ~dst ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.link (Path.to_string src) (Path.to_string dst))

let symlink = fun ~src ~dst ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.symlink (Path.to_string src) (Path.to_string dst))

let read_link = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map Path.v (FFI.readlink (Path.to_string path)))

let canonicalize = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map Path.v (FFI.realpath (Path.to_string path)))

let metadata = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map metadata_of_tuple (FFI.stat (Path.to_string path)))

let symlink_metadata = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map metadata_of_tuple (FFI.lstat (Path.to_string path)))

let fstat = fun fd ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map metadata_of_tuple (FFI.fstat fd))

let exists = fun path ->
  match metadata path with
  | Result.Ok _ -> Result.Ok true
  | Result.Error (System System_error.No_such_file_or_directory) -> Result.Ok false
  | Result.Error error -> Result.Error error

let is_directory = fun path ->
  match metadata path with
  | Result.Ok metadata -> Result.Ok (Metadata.is_dir metadata)
  | Result.Error (System System_error.No_such_file_or_directory) -> Result.Ok false
  | Result.Error error -> Result.Error error

let read_dir_names = fun path ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (FFI.readdir (Path.to_string path))

let current_dir = fun () ->
  Result.map_error
    (fun code -> System (System_error.of_code code))
    (Result.map Path.v (FFI.getcwd ()))

let set_current_dir = fun path ->
  Result.map_error (fun code -> System (System_error.of_code code)) (FFI.chdir (Path.to_string path))

let copy = fun ~src ~dst ->
  let rec write_all fd buffer pos len =
    if len <= 0 then
      Result.Ok ()
    else
      match write fd ~pos ~len buffer with
      | Result.Ok written ->
          if written <= 0 then
            Result.Error (System System_error.Broken_pipe)
          else
            write_all fd buffer (pos + written) (len - written)
      | Result.Error error -> Result.Error error
  in
  let rec copy_loop src_fd dst_fd buffer =
    match read src_fd buffer with
    | Result.Ok 0 ->
        Result.Ok ()
    | Result.Ok read_count -> (
        match write_all dst_fd buffer 0 read_count with
        | Result.Ok () -> copy_loop src_fd dst_fd buffer
        | Result.Error error -> Result.Error error
      )
    | Result.Error error ->
        Result.Error error
  in
  match open_read src with
  | Result.Error error -> Result.Error error
  | Result.Ok src_fd -> (
      match open_write ~create:true ~truncate:true dst with
      | Result.Error error ->
          let _ = close src_fd in
          Result.Error error
      | Result.Ok dst_fd ->
          let buffer = Bytes.create 65_536 in
          let outcome = copy_loop src_fd dst_fd buffer in
          let _ = close src_fd in
          let _ = close dst_fd in
          outcome
    )

let is_tty = FFI.is_tty

let to_source = fun fd ->
  let module Source = struct
    type nonrec t = t

    let register = fun fd selector token interest ->
      Async.Adapter.Selector.register selector ~fd ~token ~interest

    let reregister = fun fd selector token interest ->
      Async.Adapter.Selector.reregister selector ~fd ~token ~interest

    let deregister = fun fd selector -> Async.Adapter.Selector.deregister selector ~fd
  end in
  Async.Source.make (module Source) fd
