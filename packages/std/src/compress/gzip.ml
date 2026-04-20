open Global
open IO

let ( let* ) = fun result fn -> Result.and_then result ~fn

module Engine = Gzip_engine

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

type error =
  | Engine_error of Engine.error
  | Truncated_input

type ('src, 'read_err) reader = {
  src: ('src, 'read_err) Reader.t;
  decoder: Engine.decoder;
  mutable input: Bytes.t;
  mutable input_pos: int;
  mutable input_len: int;
  output: Bytes.t;
  mutable output_pos: int;
  mutable output_len: int;
  mutable source_eof: bool;
  mutable finished: bool;
}

type 'read_err read_error =
  | Source_error of 'read_err
  | Gzip_error of error

type ('read_err, 'write_err) stream_error =
  | Stream_source_error of 'read_err
  | Stream_destination_error of 'write_err
  | Stream_gzip_error of error

type file_error =
  | File_io_error of Fs.error
  | File_gzip_error of error

let input_buffer_size = 32 * 1_024

let output_buffer_size = 256 * 1_024

let error_of_engine = fun err -> Engine_error err

let fs_error_of_file_error = function
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Fs.File.System error -> IO.of_system_error error

let string_of_engine_error = function
  | Engine.Invalid_data -> "invalid gzip data"
  | Engine.Need_dictionary -> "gzip stream requires a preset dictionary"
  | Engine.Buffer_error -> "gzip decoder buffer error"
  | Engine.Out_of_memory -> "gzip decoder out of memory"
  | Engine.Unknown_error msg -> msg

let error_to_string = function
  | Engine_error err -> string_of_engine_error err
  | Truncated_input -> "truncated gzip input"

let buffered_input = fun (t: (_, _) reader) -> t.input_len - t.input_pos

let buffered_output = fun (t: (_, _) reader) -> t.output_len - t.output_pos

let compact_input = fun (t: (_, _) reader) ->
  if t.input_pos = 0 then
    ()
  else if t.input_pos >= t.input_len then
    (
      t.input_pos <- 0;
      t.input_len <- 0
    )
  else
    let remaining = buffered_input t in
    (
      Bytes.blit_unchecked t.input ~src_offset:t.input_pos ~dst:t.input ~dst_offset:0 ~len:remaining;
      t.input_pos <- 0;
      t.input_len <- remaining
    )

let ensure_input_capacity = fun (t: (_, _) reader) ->
  let available = Bytes.length t.input - t.input_len in
  if available > 0 then
    ()
  else
    let buffered = buffered_input t in
    let next_capacity = max (Bytes.length t.input * 2) (buffered + input_buffer_size) in
    let next = Bytes.create ~size:next_capacity in
    Bytes.blit_unchecked t.input ~src_offset:t.input_pos ~dst:next ~dst_offset:0 ~len:buffered;
    t.input <- next;
    t.input_pos <- 0;
    t.input_len <- buffered

let refill_input = fun (t: (_, _) reader) ->
  compact_input t;
  ensure_input_capacity t;
  let available = Bytes.length t.input - t.input_len in
  let chunk = Bytes.create ~size:available in
  match IO.read t.src chunk with
  | Ok 0 ->
      t.source_eof <- true;
      Ok false
  | Ok bytes_read ->
      Bytes.blit_unchecked chunk ~src_offset:0 ~dst:t.input ~dst_offset:t.input_len ~len:bytes_read;
      t.input_len <- t.input_len + bytes_read;
      Ok true
  | Error err ->
      Error (Source_error err)

let read_into = fun (t: (_, _) reader) dst ->
  let copy_from_output () =
    let available = buffered_output t in
    let to_copy = min available (Bytes.length dst) in
    Bytes.blit_unchecked t.output ~src_offset:t.output_pos ~dst ~dst_offset:0 ~len:to_copy;
    t.output_pos <- t.output_pos + to_copy;
    if t.output_pos >= t.output_len then
      (
        t.output_pos <- 0;
        t.output_len <- 0
      );
    Ok to_copy
  in
  let rec loop () =
    if buffered_output t > 0 then
      copy_from_output ()
    else if t.finished then
      Ok 0
    else
      let dst_len = Bytes.length dst in
      if dst_len = 0 then
        Ok 0
      else
        let available_input = buffered_input t in
        if available_input = 0 then
          if t.source_eof then
            Error (Gzip_error Truncated_input)
          else
            match refill_input t with
            | Error err -> Error err
            | Ok false -> Error (Gzip_error Truncated_input)
            | Ok true -> loop ()
        else
          match Engine.decode
            t.decoder
            ~src:t.input
            ~src_pos:t.input_pos
            ~src_len:available_input
            ~dst:t.output
            ~dst_pos:0
            ~dst_len:(Bytes.length t.output) with
          | Error err -> Error (Gzip_error (error_of_engine err))
          | Ok step ->
              t.input_pos <- t.input_pos + step.consumed;
              if step.produced > 0 then
                (
                  t.output_pos <- 0;
                  t.output_len <- step.produced
                );
              if step.status = Engine.Finished then
                t.finished <- true;
              if step.produced > 0 then
                copy_from_output ()
              else
                (
                  match step.status with
                  | Engine.Finished ->
                      Ok 0
                  | Engine.Need_input ->
                      if t.source_eof then
                        Error (Gzip_error Truncated_input)
                      else
                        (
                          match refill_input t with
                          | Error err -> Error err
                          | Ok false -> Error (Gzip_error Truncated_input)
                          | Ok true -> loop ()
                        )
                  | Engine.Need_output ->
                      Error (Gzip_error (Engine_error Engine.Buffer_error))
                )
  in
  loop ()

let to_reader:
  type src read_err. (src, read_err) Reader.t ->
  ((src, read_err) reader, read_err read_error) Reader.t = fun src ->
  let decoder =
    match Engine.create_decoder () with
    | Ok decoder -> decoder
    | Error err -> panic ("failed to create gzip decoder: " ^ string_of_engine_error err)
  in
  let state = {
    src;
    decoder;
    input = Bytes.create ~size:input_buffer_size;
    input_pos = 0;
    input_len = 0;
    output = Bytes.create ~size:output_buffer_size;
    output_pos = 0;
    output_len = 0;
    source_eof = false;
    finished = false;
  }
  in
  let module Read = struct
    type t = (src, read_err) reader

    type err = read_err read_error

    let read = fun t ?timeout:_ buf -> read_into t buf

    let read_vectored = fun t bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Bytes.create ~size:total_len in
      match read_into t scratch with
      | Error err -> Error err
      | Ok read_len ->
          let copied = ref 0 in
          Iovec.for_each
            ~fn:(fun segment ->
              let remaining = read_len - !copied in
              if remaining > 0 then
                let length = Iovec.IoSlice.length segment in
                let chunk_len = min length remaining in
                Iovec.IoSlice.blit_from_bytes_unchecked
                  scratch
                  ~src_off:!copied
                  segment
                  ~dst_off:0
                  ~len:chunk_len;
                copied := !copied + chunk_len)
            bufs;
          Ok read_len

    let direct_string = fun _t -> None
  end in
  Reader.of_read_src (module Read) state

let buffer_writer =
  let module Write = struct
    type t = Buffer.t

    type err = unit

    let write = fun buffer ~buf ->
      Buffer.add_string buffer buf;
      Ok (String.length buf)

    let write_owned_vectored = fun buffer ~bufs ->
      let written = ref 0 in
      Iovec.for_each
        ~fn:(fun segment ->
          Buffer.add_string buffer (Iovec.IoSlice.to_string segment);
          written := !written + Iovec.IoSlice.length segment)
        bufs;
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer ->
    Writer.of_write_src (module Write) buffer

let compress = fun src dst ->
  let encoder =
    match Engine.create_encoder () with
    | Ok encoder -> encoder
    | Error err -> panic ("failed to create gzip encoder: " ^ string_of_engine_error err)
  in
  protect ~finally:(fun () -> Engine.close_encoder encoder)
    (fun () ->
      let src_buf = Bytes.create ~size:input_buffer_size in
      let dst_buf = Bytes.create ~size:input_buffer_size in
      let rec encode_loop ~source_eof ~src_pos ~src_len ~chunk_count =
        let available_src = src_len - src_pos in
        if not source_eof && available_src = 0 then
          match IO.read src src_buf with
          | Ok 0 -> encode_loop ~source_eof:true ~src_pos:0 ~src_len:0 ~chunk_count
          | Ok bytes_read -> encode_loop ~source_eof:false ~src_pos:0 ~src_len:bytes_read ~chunk_count
          | Error err -> Error (Stream_source_error err)
        else
          let flush =
            if source_eof then
              Engine.Finish
            else
              Engine.No_flush
          in
          match Engine.encode
            encoder
            ~src:src_buf
            ~src_pos
            ~src_len:available_src
            ~dst:dst_buf
            ~dst_pos:0
            ~dst_len:(Bytes.length dst_buf)
            ~flush with
          | Error err -> Error (Stream_gzip_error (error_of_engine err))
          | Ok step ->
              let next_src_pos = src_pos + step.consumed in
              let next_src_len = src_len in
              let* () =
                if step.produced = 0 then
                  Ok ()
                else
                  let buf = Bytes.sub_unchecked dst_buf ~offset:0 ~len:step.produced |> Bytes.to_string in
                  IO.write_all dst ~buf
                  |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
              in
              let next_chunk_count =
                if step.produced > 0 then
                  chunk_count + 1
                else
                  chunk_count
              in
              let () =
                if next_chunk_count > 0 && Int.rem next_chunk_count 32 = 0 then
                  yield ()
              in
              match step.status with
              | Engine.Finished -> IO.flush dst
              |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
              | Engine.Need_output -> encode_loop
                ~source_eof
                ~src_pos:next_src_pos
                ~src_len:next_src_len
                ~chunk_count:next_chunk_count
              | Engine.Need_input ->
                  if source_eof && step.consumed = 0 && step.produced = 0 then
                    Error (Stream_gzip_error (Engine_error Engine.Buffer_error))
                  else if source_eof then
                    encode_loop
                      ~source_eof
                      ~src_pos:next_src_pos
                      ~src_len:next_src_len
                      ~chunk_count:next_chunk_count
                  else
                    encode_loop ~source_eof:false ~src_pos:0 ~src_len:0 ~chunk_count:next_chunk_count
      in
      encode_loop ~source_eof:false ~src_pos:0 ~src_len:0 ~chunk_count:0)

let decompress = fun src dst ->
  let gzip_reader = to_reader src in
  let chunk = Bytes.create ~size:input_buffer_size in
  let rec loop chunk_count =
    match IO.read gzip_reader chunk with
    | Ok 0 ->
        IO.flush dst |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
    | Ok bytes_read ->
        let () =
          if chunk_count > 0 && Int.rem chunk_count 32 = 0 then
            yield ()
        in
        let buf = Bytes.sub_unchecked chunk ~offset:0 ~len:bytes_read |> Bytes.to_string in
        let* () = IO.write_all dst ~buf
        |> Result.map_err ~fn:(fun err -> Stream_destination_error err) in
        loop (chunk_count + 1)
    | Error (Source_error err) ->
        Error (Stream_source_error err)
    | Error (Gzip_error err) ->
        Error (Stream_gzip_error err)
  in
  loop 0

let with_open_input = fun path fn ->
  match Fs.File.open_read path with
  | Error err -> Error (File_io_error (fs_error_of_file_error err))
  | Ok file ->
      protect
        ~finally:(fun () ->
          let _ = Fs.File.close file in
          ())
        (fun () -> fn file)

let with_open_output = fun path fn ->
  match Fs.File.create path with
  | Error err -> Error (File_io_error (fs_error_of_file_error err))
  | Ok file ->
      protect
        ~finally:(fun () ->
          let _ = Fs.File.close file in
          ())
        (fun () -> fn file)

let decompress_file = fun ~src ~dst ->
  with_open_input src
    (fun src_file ->
      with_open_output dst
        (fun dst_file ->
          decompress (Fs.File.to_reader src_file) (Fs.File.to_writer dst_file) |> Result.map_err
            ~fn:(
              function
              | Stream_source_error err -> File_io_error (fs_error_of_file_error err)
              | Stream_destination_error err -> File_io_error (fs_error_of_file_error err)
              | Stream_gzip_error err -> File_gzip_error err
            )))

let compress_file = fun ~src ~dst ->
  with_open_input src
    (fun src_file ->
      with_open_output dst
        (fun dst_file ->
          compress (Fs.File.to_reader src_file) (Fs.File.to_writer dst_file) |> Result.map_err
            ~fn:(
              function
              | Stream_source_error err -> File_io_error (fs_error_of_file_error err)
              | Stream_destination_error err -> File_io_error (fs_error_of_file_error err)
              | Stream_gzip_error err -> File_gzip_error err
            )))

let compress_string = fun data ->
  let buffer = Buffer.create ~size:128 in
  let writer = buffer_writer buffer in
  match compress (Reader.from_string data) writer with
  | Ok () -> Ok (Buffer.contents buffer)
  | Error (Stream_source_error ()) -> Error (Engine_error (Engine.Unknown_error "unexpected in-memory source error"))
  | Error (Stream_destination_error ()) -> Error (Engine_error (Engine.Unknown_error "unexpected in-memory destination error"))
  | Error (Stream_gzip_error err) -> Error err

let decompress_string = fun data ->
  let gzip_reader = to_reader (Reader.from_string data) in
  let buffer = Buffer.create ~size:128 in
  match Reader.read_all_into_buffer gzip_reader ~buf:buffer with
  | Ok _ -> Ok (Buffer.contents buffer)
  | Error (Source_error ()) -> Error (Engine_error (Engine.Unknown_error "unexpected in-memory source error"))
  | Error (Gzip_error err) -> Error err
