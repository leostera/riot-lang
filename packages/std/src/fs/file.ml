open Global
open Common

type t = { fd : Kernel.Fd.t; mutable closed : bool }

(* Helper to check if file is closed *)
let ensure_open t =
  if t.closed then Error (SystemError "File is closed") else Ok ()

(* Opening files *)

let open_with_flags path flags mode =
  let path_str = Path.to_string path in
  match Kernel.Fs.File.open_file path_str flags mode with
  | Ok fd -> Ok { fd; closed = false }
  | Error e -> Error (SystemError (kernel_error_to_string e))

let create path =
  open_with_flags path
    [ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create; Kernel.Fs.File.Truncate ]
    0o644

let create_new path =
  open_with_flags path
    [
      Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create; Kernel.Fs.File.Exclusive;
    ]
    0o644

let open_read path = open_with_flags path [ Kernel.Fs.File.ReadOnly ] 0o644

let open_write path =
  open_with_flags path [ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create ] 0o644

let open_append path =
  open_with_flags path
    [ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Append; Kernel.Fs.File.Create ]
    0o644

let open_read_write path =
  open_with_flags path [ Kernel.Fs.File.ReadWrite ] 0o644

(* Reading - with async/Miniriot support *)

let read t buffer ~offset ~len =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let source = Kernel.Fs.File.to_source t.fd in
      let rec read_loop () =
        match Kernel.Fs.File.read t.fd buffer ~pos:offset ~len with
        | Ok bytes_read -> Ok bytes_read
        | Error `Would_block ->
            Miniriot.syscall ~name:"File.read"
              ~interest:Kernel.Async.Interest.readable ~source read_loop
        | Error e -> Error (SystemError (kernel_error_to_string e))
      in
      read_loop ()

let read_to_end t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      (* Read in chunks until EOF - works for both files and pipes *)
      let buf = Buffer.create 4096 in
      let chunk = Bytes.create 4096 in
      let rec drain () =
        match read t chunk ~offset:0 ~len:4096 with
        | Ok 0 -> Ok (Buffer.contents buf) (* EOF *)
        | Ok n ->
            Buffer.add_subbytes buf chunk 0 n;
            drain ()
        | Error e -> Error e
      in
      drain ()

let read_exact t buffer ~offset ~len =
  let rec read_loop pos remaining =
    if remaining = 0 then Ok ()
    else
      match read t buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Error (SystemError "Unexpected EOF")
      | Ok n -> read_loop (pos + n) (remaining - n)
      | Error e -> Error e
  in
  read_loop offset len

let read_line t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let buf = Buffer.create 256 in
      let chunk = Bytes.create 1 in
      let rec read_until_newline () =
        match read t chunk ~offset:0 ~len:1 with
        | Ok 0 -> Ok (Buffer.contents buf)
        | Ok 1 ->
            let c = Bytes.get chunk 0 in
            Buffer.add_char buf c;
            if c = '\n' then Ok (Buffer.contents buf) else read_until_newline ()
        | Ok _ -> Error (SystemError "Unexpected read result")
        | Error e -> Error e
      in
      read_until_newline ()

(* Writing - with async/Miniriot support *)

let write t buffer ~offset ~len =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      let source = Kernel.Fs.File.to_source t.fd in
      let rec write_loop () =
        match Kernel.Fs.File.write t.fd buffer ~pos:offset ~len with
        | Ok bytes_written -> Ok bytes_written
        | Error `Would_block ->
            Miniriot.syscall ~name:"File.write"
              ~interest:Kernel.Async.Interest.writable ~source write_loop
        | Error e -> Error (SystemError (kernel_error_to_string e))
      in
      write_loop ()

let write_string t str =
  let buffer = Bytes.of_string str in
  write t buffer ~offset:0 ~len:(String.length str)

let write_all t str =
  let buffer = Bytes.of_string str in
  let len = String.length str in
  let rec write_loop pos remaining =
    if remaining = 0 then Ok ()
    else
      match write t buffer ~offset:pos ~len:remaining with
      | Ok n -> write_loop (pos + n) (remaining - n)
      | Error e -> Error e
  in
  write_loop 0 len

(* Seeking *)

let seek t pos =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lseek t.fd pos Kernel.Fs.File.SeekSet
      |> convert_kernel_result

let seek_from_current t offset =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lseek t.fd offset Kernel.Fs.File.SeekCur
      |> convert_kernel_result

let seek_from_end t offset =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lseek t.fd offset Kernel.Fs.File.SeekEnd
      |> convert_kernel_result

let tell t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lseek t.fd 0L Kernel.Fs.File.SeekCur
      |> convert_kernel_result

let rewind t = seek t 0L |> Result.map (fun _ -> ())

(* Synchronization *)

let sync_all t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.fsync t.fd |> convert_kernel_result

let sync_data t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      (* fdatasync not in OCaml Unix, fall back to fsync *)
      Kernel.Fs.File.fsync t.fd |> convert_kernel_result

(* Metadata *)

let metadata t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.fstat t.fd |> convert_kernel_result with
      | Ok m -> Ok (m : Metadata.t)
      | Error e -> Error e)

let set_len t ~len =
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> Kernel.Fs.File.ftruncate t.fd len |> convert_kernel_result

let set_permissions t ~permissions =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.fchmod t.fd (Permissions.to_mode permissions)
      |> convert_kernel_result

(* File locking *)

let lock_exclusive t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lockf t.fd Kernel.Fs.File.LockExclusive 0
      |> convert_kernel_result

let lock_shared t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lockf t.fd Kernel.Fs.File.LockShared 0
      |> convert_kernel_result

let try_lock_exclusive t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.lockf t.fd Kernel.Fs.File.TryLockExclusive 0 with
      | Ok () -> Ok true
      | Error (`IO_error e) when e = Kernel.IO.Resource_unavailable_try_again ->
          Ok false
      | Error (`IO_error e) when e = Kernel.IO.Permission_denied -> Ok false
      | Error e -> Error (SystemError (kernel_error_to_string e)))

let try_lock_shared t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () -> (
      match Kernel.Fs.File.lockf t.fd Kernel.Fs.File.TryLockShared 0 with
      | Ok () -> Ok true
      | Error (`IO_error e) when e = Kernel.IO.Resource_unavailable_try_again ->
          Ok false
      | Error (`IO_error e) when e = Kernel.IO.Permission_denied -> Ok false
      | Error e -> Error (SystemError (kernel_error_to_string e)))

let unlock t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.lockf t.fd Kernel.Fs.File.Unlock 0 |> convert_kernel_result

(* Advanced *)

let try_clone t =
  match ensure_open t with
  | Error e -> Error e
  | Ok () ->
      Kernel.Fs.File.dup t.fd |> convert_kernel_result
      |> Result.map (fun new_fd -> { fd = new_fd; closed = false })

let into_fd t = t.fd
let from_fd fd = { fd; closed = false }

(* Closing *)

let close t =
  if t.closed then Ok ()
  else
    try
      t.closed <- true;
      Kernel.Fs.File.close_fd t.fd |> convert_kernel_result
    with e -> Error (SystemError (Exception.to_string e))
