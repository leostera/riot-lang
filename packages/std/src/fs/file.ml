open Global
open IO
open Common

type t = {
  fd: Kernel.Fd.t;
  mutable closed: bool;
}

(* Re-export OpenFlags from Kernel.Fd *)

module OpenFlags = Kernel.Fd.OpenFlags

(* Helper to check if file is closed *)

let ensure_open = fun t ->
  if t.closed then
    Error (IO.Unknown_error "File is closed")
  else
    Ok ()

(* Opening files *)

let open_with_flags = fun path flags ~mode ->
  let path_str = Path.to_string path in
  let mode_int = Permissions.to_mode mode in
  try
    let fd = Kernel.Fd.open_file path_str flags mode_int in
    Ok { fd; closed = false }
  with
  | e -> Error (IO.Unknown_error ("Failed to open " ^ path_str ^ ": " ^ Exception.to_string e))

let create = fun path ->
  open_with_flags
    path
    [ Kernel.Fd.OpenFlags.WriteOnly; Kernel.Fd.OpenFlags.Create; Kernel.Fd.OpenFlags.Truncate; ]
    ~mode:Permissions.read_write

let create_new = fun path ->
  open_with_flags
    path
    [ Kernel.Fd.OpenFlags.WriteOnly; Kernel.Fd.OpenFlags.Create; Kernel.Fd.OpenFlags.Exclusive; ]
    ~mode:Permissions.read_write

let open_read = fun path -> open_with_flags path [ Kernel.Fd.OpenFlags.ReadOnly ] ~mode:Permissions.read_write

let open_write = fun path ->
  open_with_flags
    path
    [ Kernel.Fd.OpenFlags.WriteOnly; Kernel.Fd.OpenFlags.Create ]
    ~mode:Permissions.read_write

let open_append = fun path ->
  open_with_flags
    path
    [ Kernel.Fd.OpenFlags.ReadWrite; Kernel.Fd.OpenFlags.Append; Kernel.Fd.OpenFlags.Create; ]
    ~mode:Permissions.read_write

let open_read_write = fun path ->
  open_with_flags path [ Kernel.Fd.OpenFlags.ReadWrite ] ~mode:Permissions.read_write

(* Reading - with async/Miniriot support *)

let read = fun t buffer ~offset ~len ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let source = Kernel.Fs.File.to_source t.fd in
      let rec read_loop () =
        match Kernel.Fs.File.read t.fd buffer ~pos:offset ~len with
        | Ok bytes_read -> Ok bytes_read
        | Error IO.Operation_would_block
        | Error IO.Resource_unavailable_try_again -> Miniriot.syscall
          ~name:"File.read"
          ~interest:Kernel.Async.Interest.readable
          ~source
          read_loop
        | Error e -> Error e
      in
      read_loop ()

let read_to_end = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      (* Read in chunks until EOF - works for both files and pipes *)
      let buf = Buffer.create 4_096 in
      let chunk = Bytes.create 4_096 in
      let rec drain () =
        match read t chunk ~offset:0 ~len:4_096 with
        | Ok 0 ->
            Ok (Buffer.contents buf)
        | Ok n ->
            Buffer.add_subbytes buf chunk 0 n;
            drain ()
        | Error e ->
            Error e
      in
      drain ()

let read_exact = fun t buffer ~offset ~len ->
  let rec read_loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match read t buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Error (IO.Unknown_error "Unexpected EOF")
      | Ok n -> read_loop (pos + n) (remaining - n)
      | Error e -> Error e
  in
  read_loop offset len

let read_line = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let buf = Buffer.create 256 in
      let chunk = Bytes.create 1 in
      let rec read_until_newline () =
        match read t chunk ~offset:0 ~len:1 with
        | Ok 0 ->
            Ok (Buffer.contents buf)
        | Ok 1 ->
            let c = Bytes.get chunk 0 in
            Buffer.add_char buf c;
            if c = '\n' then
              Ok (Buffer.contents buf)
            else
              read_until_newline ()
        | Ok _ ->
            Error (IO.Unknown_error "Unexpected read result")
        | Error e ->
            Error e
      in
      read_until_newline ()

(* Writing - with async/Miniriot support *)

let write = fun t buffer ~offset ~len ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let source = Kernel.Fs.File.to_source t.fd in
      let rec write_loop () =
        match Kernel.Fs.File.write t.fd buffer ~pos:offset ~len with
        | Ok bytes_written -> Ok bytes_written
        | Error IO.Operation_would_block
        | Error IO.Resource_unavailable_try_again -> Miniriot.syscall
          ~name:"File.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          write_loop
        | Error e -> Error e
      in
      write_loop ()

let write_string = fun t str ->
  let buffer = Bytes.unsafe_of_string str in
  write t buffer ~offset:0 ~len:(String.length str)

let write_all = fun t str ->
  let buffer = Bytes.unsafe_of_string str in
  let len = String.length str in
  let rec write_loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match write t buffer ~offset:pos ~len:remaining with
      | Ok n -> write_loop (pos + n) (remaining - n)
      | Error e -> Error e
  in
  write_loop 0 len

(* Seeking *)

let seek = fun t pos ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lseek t.fd pos Kernel.Fs.File.SeekSet |> convert_kernel_result

let seek_from_current = fun t offset ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lseek t.fd offset Kernel.Fs.File.SeekCur |> convert_kernel_result

let seek_from_end = fun t offset ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lseek t.fd offset Kernel.Fs.File.SeekEnd |> convert_kernel_result

let tell = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lseek t.fd 0L Kernel.Fs.File.SeekCur |> convert_kernel_result

let rewind = fun t -> seek t 0L |> Result.map (fun _ -> ())

(* Synchronization *)

let sync_all = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.fsync t.fd |> convert_kernel_result

let sync_data = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      (* fdatasync not in OCaml Unix, fall back to fsync *)
      Kernel.Fs.File.fsync t.fd |> convert_kernel_result

(* Metadata *)

let metadata = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.fstat t.fd |> convert_kernel_result with
      | Ok m -> Ok ((m: Metadata.t))
      | Error e -> Error e
    )

let set_len = fun t ~len ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.ftruncate t.fd len |> convert_kernel_result

let set_permissions = fun t ~permissions ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.fchmod t.fd (Permissions.to_mode permissions) |> convert_kernel_result

(* File locking *)

let lock_exclusive = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lockf t.fd Kernel.Fs.File.LockExclusive 0 |> convert_kernel_result

let lock_shared = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lockf t.fd Kernel.Fs.File.LockShared 0 |> convert_kernel_result

let try_lock_exclusive = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.lockf t.fd Kernel.Fs.File.TryLockExclusive 0 with
      | Ok () -> Ok true
      | Error e when e = Kernel.IO.Resource_unavailable_try_again -> Ok false
      | Error e when e = Kernel.IO.Permission_denied -> Ok false
      | Error e -> Error e
    )

let try_lock_shared = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.lockf t.fd Kernel.Fs.File.TryLockShared 0 with
      | Ok () -> Ok true
      | Error e when e = Kernel.IO.Resource_unavailable_try_again -> Ok false
      | Error e when e = Kernel.IO.Permission_denied -> Ok false
      | Error e -> Error e
    )

let unlock = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.lockf t.fd Kernel.Fs.File.Unlock 0 |> convert_kernel_result

(* Advanced *)

let try_clone = fun t ->
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.dup t.fd
  |> convert_kernel_result
  |> Result.map (fun new_fd -> { fd = new_fd; closed = false })

let into_fd = fun t -> t.fd

let from_fd = fun fd -> { fd; closed = false }

(* Closing *)

let close = fun t ->
  if t.closed then
    Ok ()
  else
    try
      t.closed <- true;
      Kernel.Fs.File.close_fd t.fd |> convert_kernel_result
    with
    | e -> Error (IO.Unknown_error (Exception.to_string e))
