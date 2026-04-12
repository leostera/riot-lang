open Global

module Buffer = IO.Buffer
module Bytes = Kernel.Bytes

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

let open_with_flags = fun path flags ~mode ->
  Kernel.Fs.File.open_file path flags ~perm:(Permissions.to_mode mode)

let create = fun path ->
  Kernel.Fs.File.open_write
    path
    ~create:true
    ~truncate:true
    ~append:false
    ~perm:(Permissions.to_mode Permissions.read_write)

let create_new = fun path ->
  Kernel.Fs.File.open_file
    path
    [ Kernel.Fs.File.WriteOnly; Kernel.Fs.File.Create; Kernel.Fs.File.Exclusive ]
    ~perm:(Permissions.to_mode Permissions.read_write)

let open_read = Kernel.Fs.File.open_read

let open_write = fun path ->
  Kernel.Fs.File.open_write
    path
    ~create:true
    ~truncate:false
    ~append:false
    ~perm:(Permissions.to_mode Permissions.read_write)

let open_append = fun path ->
  Kernel.Fs.File.open_write
    path
    ~create:true
    ~truncate:false
    ~append:true
    ~perm:(Permissions.to_mode Permissions.read_write)

let open_read_write = fun path ->
  Kernel.Fs.File.open_file
    path
    [ Kernel.Fs.File.ReadWrite ]
    ~perm:(Permissions.to_mode Permissions.read_write)

let to_source = Kernel.Fs.File.to_source

let is_would_block = function
  | Kernel.Fs.File.System error -> Kernel.SystemError.is_would_block error
  | Kernel.Fs.File.InvalidSlice _ -> false

let read = fun file buffer ~offset ~len ->
  let source = Kernel.Fs.File.to_source file in
  let rec loop () =
    match Kernel.Fs.File.read file buffer ~pos:offset ~len with
    | Ok bytes_read -> Ok bytes_read
    | Error err when is_would_block err ->
        Runtime.syscall
          ~name:"Fs.File.read"
          ~interest:Kernel.Async.Interest.readable
          ~source
          loop
    | Error err -> Error err
  in
  loop ()

let read_to_end = fun file ->
  let buf = Buffer.create 4_096 in
  let chunk = Bytes.create 4_096 in
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
      | Ok 0 ->
          Error (Kernel.Fs.File.System Kernel.SystemError.EndOfFile)
      | Ok n -> loop (pos + n) (remaining - n)
      | Error err -> Error err
  in
  loop offset len

let read_line = fun file ->
  let buf = Buffer.create 256 in
  let chunk = Bytes.create 1 in
  let rec loop () =
    match read file chunk ~offset:0 ~len:1 with
    | Ok 0 -> Ok (Buffer.contents buf)
    | Ok 1 ->
        let c = Bytes.get chunk 0 in
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
        Runtime.syscall
          ~name:"Fs.File.write"
          ~interest:Kernel.Async.Interest.writable
          ~source
          loop
    | Error err -> Error err
  in
  loop ()

let write_string = fun file str ->
  write file (Kernel.Bytes.of_string str) ~offset:0 ~len:(String.length str)

let write_all = fun file str ->
  let buffer = Kernel.Bytes.of_string str in
  let len = String.length str in
  let rec loop pos remaining =
    if remaining = 0 then
      Ok ()
    else
      match write file buffer ~offset:pos ~len:remaining with
      | Ok 0 -> Ok ()
      | Ok n -> loop (pos + n) (remaining - n)
      | Error err -> Error err
  in
  loop 0 len

let metadata = Kernel.Fs.File.fstat

let to_reader = fun file ->
  let module Read = struct
    type nonrec t = t

    type err = error

    let read = fun file ?timeout:_ buf ->
      read file buf ~offset:0 ~len:(Bytes.length buf)

    let read_vectored = fun file bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Bytes.create total_len in
      match read file scratch ~offset:0 ~len:total_len with
      | Error err -> Error err
      | Ok read_len ->
          let copied = ref 0 in
          Iovec.iter
            (fun { Kernel.IO.Iovec.buffer; offset; length } ->
              let remaining = read_len - !copied in
              if remaining > 0 then
                let chunk_len = min length remaining in
                Bytes.blit scratch !copied buffer offset chunk_len;
                copied := !copied + chunk_len)
            bufs;
          Ok read_len

    let direct_string = fun _file -> None
  end in
  IO.Reader.of_read_src (module Read) file

let to_writer = fun file ->
  let module Write = struct
    type nonrec t = t

    type err = error

    let write = fun file ~buf -> write_string file buf

    let write_owned_vectored = fun file ~bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Iovec.into_bytes bufs in
      write file scratch ~offset:0 ~len:total_len

    let flush = fun _file -> Ok ()
  end in
  IO.Writer.of_write_src (module Write) file

let close = Kernel.Fs.File.close
