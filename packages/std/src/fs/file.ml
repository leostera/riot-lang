open Global

module Buffer = StringBuilder
module Bytes = Kernel.Bytes
module IoVec = IO.IoVec

type t = Kernel.Fs.File.t

type error = Kernel.Fs.File.error

module OpenFlags = struct
  type t = Kernel.Fs.File.open_flag =
    | ReadOnly
    | WriteOnly
    | ReadWrite
    | Create
    | Truncate
    | Append
    | Exclusive
end

let error_to_string = Kernel.Fs.File.error_to_string

let kernel_path = fun path -> Kernel.Path.from_string (Path.to_string path)

let wrap_result: type value error. (value, error) Kernel.Result.t -> (value, error) result = fun
  __tmp1 ->
  match __tmp1 with
  | Ok value -> Ok value
  | Error error -> Error error

let open_with_flags = fun path flags ~mode ->
  wrap_result
    (Kernel.Fs.File.open_file (kernel_path path) ~flags ~permissions:(Permissions.to_mode mode))

let create = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:true
      ~append:false
      ~perm:(Permissions.to_mode Permissions.read_write))

let create_new = fun path ->
  wrap_result
    (Kernel.Fs.File.open_file
      (kernel_path path)
      ~flags:[ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create; Kernel.Fs.File.Exclusive ]
      ~permissions:(Permissions.to_mode Permissions.read_write))

let open_read = fun path -> wrap_result (Kernel.Fs.File.open_read (kernel_path path))

let open_write = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:false
      ~append:false
      ~perm:(Permissions.to_mode Permissions.read_write))

let open_append = fun path ->
  wrap_result
    (Kernel.Fs.File.open_write
      (kernel_path path)
      ~create:true
      ~truncate:false
      ~append:true
      ~perm:(Permissions.to_mode Permissions.read_write))

let open_read_write = fun path ->
  wrap_result
    (Kernel.Fs.File.open_file
      (kernel_path path)
      ~flags:[ Kernel.Fs.File.ReadWrite ]
      ~permissions:(Permissions.to_mode Permissions.read_write))

let try_lock_exclusive = fun file -> wrap_result (Kernel.Fs.File.try_lock_exclusive file)

let unlock = fun file -> wrap_result (Kernel.Fs.File.unlock file)

let to_source = Kernel.Fs.File.to_source

let is_would_block = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.System error -> Kernel.SystemError.would_block error
  | Kernel.Fs.File.InvalidSlice _ -> false

let io_error_of_file_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.System error -> IO.from_system_error error
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument

let read = fun file buffer ~offset ~len ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.read file buffer ~pos:offset ~len with
    | Ok bytes_read -> Ok bytes_read
    | Error err when is_would_block err ->
        Runtime.syscall ~name:"Fs.File.read" ~interest:Kernel.Async.Interest.readable ~source loop
    | Error err -> Error err
  in
  loop ()

let read_to_end = fun file ->
  let buf = Buffer.create ~size:4_096 in
  let chunk = Bytes.create ~size:4_096 in
  let rec loop () =
    match read file chunk ~offset:0 ~len:4_096 with
    | Ok 0 -> Ok (Buffer.contents buf)
    | Ok n ->
        Buffer.add_subbytes buf chunk 0 n;
        loop ()
    | Error err -> Error err
  in
  loop ()

let read_exact = fun file buffer ~offset ~len ->
  let rec loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match read file buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Error (Kernel.Fs.File.System Kernel.SystemError.EndOfFile)
      | Ok n -> loop (pos + n) (remaining - n)
      | Error err -> Error err
  in
  loop offset len

let read_line = fun file ->
  let buf = Buffer.create ~size:256 in
  let chunk = Bytes.create ~size:1 in
  let rec loop () =
    match read file chunk ~offset:0 ~len:1 with
    | Ok 0 -> Ok (Buffer.contents buf)
    | Ok 1 ->
        let c = Bytes.get_unchecked chunk ~at:0 in
        Buffer.add_char buf c;
        if c = '\n' then
          Ok (Buffer.contents buf)
        else
          loop ()
    | Ok _ -> Ok (Buffer.contents buf)
    | Error err -> Error err
  in
  loop ()

let write = fun file buffer ~offset ~len ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.write file buffer ~pos:offset ~len with
    | Ok bytes_written -> Ok bytes_written
    | Error err when is_would_block err ->
        Runtime.syscall ~name:"Fs.File.write" ~interest:Kernel.Async.Interest.writable ~source loop
    | Error err -> Error err
  in
  loop ()

let write_vectored = fun file iovec ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.write_vectored file iovec with
    | Ok bytes_written -> Ok bytes_written
    | Error err when is_would_block err ->
        Runtime.syscall
          ~name:"Fs.File.write_vectored"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error err -> Error err
  in
  loop ()

let write_all_vectored = fun file iovec ->
  let rec loop remaining =
    let remaining_len = IoVec.length remaining in
    if remaining_len = 0 then
      Ok ()
    else
      match write_vectored file remaining with
      | Ok 0 -> Ok ()
      | Ok written -> (
          match IoVec.sub remaining ~pos:written ~len:(remaining_len - written) with
          | Ok next -> loop next
          | Error error ->
              Kernel.SystemError.panic
                ("Fs.File.write_all_vectored: " ^ Kernel.IO.Error.message error)
        )
      | Error err -> Error err
  in
  loop iovec

let write_string = fun file str ->
  let buffer = IO.Buffer.from_string str in
  write_vectored file (IO.Buffer.to_iovec buffer)

let write_all = fun file str ->
  let buffer = IO.Buffer.from_string str in
  write_all_vectored file (IO.Buffer.to_iovec buffer)

let metadata = fun file -> wrap_result (Kernel.Fs.File.fstat file)

let to_reader = fun file ->
  let module Read = struct
    type nonrec t = t

    let read = fun file ~into ->
      let writable =
        if IO.Buffer.writable_bytes into = 0 then (
          match IO.Buffer.ensure_free into 4_096 with
          | Ok () -> IO.Buffer.writable into
          | Error error ->
              Kernel.SystemError.panic
                ("Fs.File.to_reader.ensure_free: " ^ Kernel.IO.Error.message error)
        ) else
          IO.Buffer.writable into
      in
      match Kernel.Fs.File.read_vectored file (IO.IoVec.from_slices [|writable|]) with
      | Ok count -> (
          match IO.Buffer.commit into count with
          | Ok () -> Ok count
          | Error error ->
              Kernel.SystemError.panic
                ("Fs.File.to_reader.commit: " ^ Kernel.IO.Error.message error)
        )
      | Error err -> Error (io_error_of_file_error err)

    let read_vectored = fun file ~into ->
      match Kernel.Fs.File.read_vectored file into with
      | Ok count -> Ok count
      | Error err -> Error (io_error_of_file_error err)

    let is_read_vectored = fun _file -> true
  end in
  IO.Reader.from_source (module Read) file

let to_writer = fun file ->
  let module Write = struct
    type nonrec t = t

    let write = fun file ~from ->
      match write_vectored file (IO.Buffer.to_iovec from) with
      | Ok count -> Ok count
      | Error err -> Error (io_error_of_file_error err)

    let write_vectored = fun file ~from ->
      match write_vectored file from with
      | Ok count -> Ok count
      | Error err -> Error (io_error_of_file_error err)

    let flush = fun _file -> Ok ()
  end in
  IO.Writer.from_sink (module Write) file

let close = fun file -> wrap_result (Kernel.Fs.File.close file)
