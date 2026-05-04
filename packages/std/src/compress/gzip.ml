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

type reader = {
  src: Reader.t;
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

type read_error =
  | Source_error of IO.error
  | Gzip_error of error

type stream_error =
  | Stream_source_error of IO.error
  | Stream_destination_error of IO.error
  | Stream_gzip_error of error

type file_error =
  | File_io_error of IO.error
  | File_gzip_error of error

let input_buffer_size = 32 * 1_024

let output_buffer_size = 256 * 1_024

let error_of_engine = fun err -> Engine_error err

let fs_error_of_file_error = fun __tmp1 ->
  match __tmp1 with
  | Kernel.Fs.File.InvalidSlice _ -> IO.Invalid_argument
  | Kernel.Fs.File.System error -> IO.from_system_error error

let string_of_engine_error = fun __tmp1 ->
  match __tmp1 with
  | Engine.Invalid_data -> "invalid gzip data"
  | Engine.Need_dictionary -> "gzip stream requires a preset dictionary"
  | Engine.Buffer_error -> "gzip decoder buffer error"
  | Engine.Out_of_memory -> "gzip decoder out of memory"
  | Engine.Unknown_error msg -> msg

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Engine_error err -> string_of_engine_error err
  | Truncated_input -> "truncated gzip input"

let buffered_input = fun (t: reader) -> t.input_len - t.input_pos

let buffered_output = fun (t: reader) -> t.output_len - t.output_pos

let compact_input = fun (t: reader) ->
  if t.input_pos = 0 then
    ()
  else if t.input_pos >= t.input_len then (
    t.input_pos <- 0;
    t.input_len <- 0
  ) else
    let remaining = buffered_input t in
    (
      Bytes.blit_unchecked t.input ~src_offset:t.input_pos ~dst:t.input ~dst_offset:0 ~len:remaining;
      t.input_pos <- 0;
      t.input_len <- remaining
    )

let ensure_input_capacity = fun (t: reader) ->
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

let refill_input = fun (t: reader) ->
  compact_input t;
  ensure_input_capacity t;
  let available = Bytes.length t.input - t.input_len in
  let chunk = Buffer.create ~size:available in
  match IO.read t.src ~into:chunk with
  | Ok 0 ->
      t.source_eof <- true;
      Ok false
  | Ok bytes_read ->
      let chunk_bytes = Buffer.to_bytes chunk in
      Bytes.blit_unchecked
        chunk_bytes
        ~src_offset:0
        ~dst:t.input
        ~dst_offset:t.input_len
        ~len:bytes_read;
      t.input_len <- t.input_len + bytes_read;
      Ok true
  | Error err -> Error (Source_error err)

let read_into = fun (t: reader) dst ->
  let copy_from_output () =
    let available = buffered_output t in
    let to_copy = min available (Bytes.length dst) in
    Bytes.blit_unchecked t.output ~src_offset:t.output_pos ~dst ~dst_offset:0 ~len:to_copy;
    t.output_pos <- t.output_pos + to_copy;
    if t.output_pos >= t.output_len then (
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
              if step.produced > 0 then (
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
                  | Engine.Finished -> Ok 0
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
                  | Engine.Need_output -> Error (Gzip_error (Engine_error Engine.Buffer_error))
                )
  in
  loop ()

let io_error_of_read_error = fun __tmp1 ->
  match __tmp1 with
  | Source_error error -> error
  | Gzip_error Truncated_input -> IO.Unexpected_end_of_file
  | Gzip_error (Engine_error Engine.Invalid_data) -> IO.Invalid_data
  | Gzip_error (Engine_error Engine.Need_dictionary) ->
      IO.Unknown_error "gzip stream requires a preset dictionary"
  | Gzip_error (Engine_error Engine.Buffer_error) -> IO.Buffer_full
  | Gzip_error (Engine_error Engine.Out_of_memory) -> IO.Out_of_memory
  | Gzip_error (Engine_error (Engine.Unknown_error message)) -> IO.Unknown_error message

let make_reader = fun src ->
  let decoder =
    match Engine.create_decoder () with
    | Ok decoder -> decoder
    | Error err -> panic ("failed to create gzip decoder: " ^ string_of_engine_error err)
  in
  {
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

let to_reader = fun src ->
  let state = make_reader src in
  let module Read = struct
    type t = reader

    let read = fun t ~into ->
      let requested =
        let writable = Buffer.writable_bytes into in
        if writable = 0 then
          4_096
        else
          writable
      in
      let scratch = Bytes.create ~size:requested in
      match read_into t scratch with
      | Error err -> Error (io_error_of_read_error err)
      | Ok read_len ->
          let chunk = Bytes.sub_unchecked scratch ~offset:0 ~len:read_len in
          begin
            match Buffer.append_bytes into chunk with
            | Ok () -> Ok read_len
            | Error error ->
                panic ("Compress.Gzip.to_reader.read: " ^ Kernel.IO.Error.message error)
          end

    let read_vectored = fun t ~into:bufs ->
      let total_len = IoVec.length bufs in
      let scratch = Bytes.create ~size:total_len in
      match read_into t scratch with
      | Error err -> Error (io_error_of_read_error err)
      | Ok read_len ->
          let copied = ref 0 in
          IoVec.for_each
            ~fn:(fun segment ->
              let remaining = read_len - !copied in
              if remaining > 0 then
                let length = IoVec.IoSlice.length segment in
                let chunk_len = min length remaining in
                IoVec.IoSlice.blit_from_bytes_unchecked
                  scratch
                  ~src_off:!copied
                  segment
                  ~dst_off:0
                  ~len:chunk_len;
              copied := !copied + chunk_len)
            bufs;
          Ok read_len

    let is_read_vectored = fun _t -> true
  end in
  Reader.from_source (module Read) state

let buffer_writer =
  let module Write = struct
    type t = Buffer.t

    let write = fun buffer ~from ->
      match Buffer.append_slice buffer (Buffer.readable from) with
      | Ok () -> Ok (Buffer.readable_bytes from)
      | Error error -> panic ("Compress.Gzip.buffer_writer.write: " ^ Kernel.IO.Error.message error)

    let write_vectored = fun buffer ~from:bufs ->
      let written = ref 0 in
      IoVec.for_each
        ~fn:(fun segment ->
          Buffer.add_string buffer (IoVec.IoSlice.to_string segment);
          written := !written + IoVec.IoSlice.length segment)
        bufs;
      Ok !written

    let flush = fun _buffer -> Ok ()
  end in
  fun buffer -> Writer.from_sink (module Write) buffer

let compress = fun src dst ->
  let encoder =
    match Engine.create_encoder () with
    | Ok encoder -> encoder
    | Error err -> panic ("failed to create gzip encoder: " ^ string_of_engine_error err)
  in
  protect
    ~finally:(fun () -> Engine.close_encoder encoder)
    (fun () ->
      let src_buf = Bytes.create ~size:input_buffer_size in
      let dst_buf = Bytes.create ~size:input_buffer_size in
      let rec encode_loop ~source_eof ~src_pos ~src_len ~chunk_count =
        let available_src = src_len - src_pos in
        if not source_eof && available_src = 0 then
          let input = Buffer.create ~size:input_buffer_size in
          match IO.read src ~into:input with
          | Ok 0 -> encode_loop ~source_eof:true ~src_pos:0 ~src_len:0 ~chunk_count
          | Ok bytes_read ->
              let bytes = Buffer.to_bytes input in
              Bytes.blit_unchecked bytes ~src_offset:0 ~dst:src_buf ~dst_offset:0 ~len:bytes_read;
              encode_loop ~source_eof:false ~src_pos:0 ~src_len:bytes_read ~chunk_count
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
                  let buf =
                    Bytes.sub_unchecked dst_buf ~offset:0 ~len:step.produced
                    |> Buffer.from_bytes
                  in
                  IO.write_all dst ~from:buf
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
              | Engine.Finished ->
                  IO.flush dst
                  |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
              | Engine.Need_output ->
                  encode_loop
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
                    encode_loop
                      ~source_eof:false
                      ~src_pos:0
                      ~src_len:0
                      ~chunk_count:next_chunk_count
      in
      encode_loop ~source_eof:false ~src_pos:0 ~src_len:0 ~chunk_count:0)

let decompress = fun src dst ->
  let gzip_reader = make_reader src in
  let chunk = Buffer.create ~size:input_buffer_size in
  let rec loop chunk_count =
    Buffer.clear chunk;
    let requested =
      let writable = Buffer.writable_bytes chunk in
      if writable = 0 then
        input_buffer_size
      else
        writable
    in
    let scratch = Bytes.create ~size:requested in
    match read_into gzip_reader scratch with
    | Ok 0 ->
        IO.flush dst
        |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
    | Ok read_len ->
        let block = Bytes.sub_unchecked scratch ~offset:0 ~len:read_len in
        begin
          match Buffer.append_bytes chunk block with
          | Ok () -> ()
          | Error error ->
              panic ("Compress.Gzip.decompress.append: " ^ Kernel.IO.Error.message error)
        end;
        let () =
          if chunk_count > 0 && Int.rem chunk_count 32 = 0 then
            yield ()
        in
        let* () =
          IO.write_all dst ~from:chunk
          |> Result.map_err ~fn:(fun err -> Stream_destination_error err)
        in
        loop (chunk_count + 1)
    | Error (Source_error err) -> Error (Stream_source_error err)
    | Error (Gzip_error err) -> Error (Stream_gzip_error err)
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
  with_open_input
    src
    (fun src_file ->
      with_open_output
        dst
        (fun dst_file ->
          decompress (Fs.File.to_reader src_file) (Fs.File.to_writer dst_file)
          |> Result.map_err
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Stream_source_error err -> File_io_error err
              | Stream_destination_error err -> File_io_error err
              | Stream_gzip_error err -> File_gzip_error err)))

let compress_file = fun ~src ~dst ->
  with_open_input
    src
    (fun src_file ->
      with_open_output
        dst
        (fun dst_file ->
          compress (Fs.File.to_reader src_file) (Fs.File.to_writer dst_file)
          |> Result.map_err
            ~fn:(fun __tmp1 ->
              match __tmp1 with
              | Stream_source_error err -> File_io_error err
              | Stream_destination_error err -> File_io_error err
              | Stream_gzip_error err -> File_gzip_error err)))

let compress_string = fun data ->
  let buffer = Buffer.create ~size:128 in
  let writer = buffer_writer buffer in
  match compress (Reader.from_string data) writer with
  | Ok () -> Ok (Buffer.contents buffer)
  | Error (Stream_source_error err) ->
      Error (Engine_error (Engine.Unknown_error ("unexpected in-memory source error: "
      ^ IO.error_message err)))
  | Error (Stream_destination_error err) ->
      Error (Engine_error (Engine.Unknown_error ("unexpected in-memory destination error: "
      ^ IO.error_message err)))
  | Error (Stream_gzip_error err) -> Error err

let decompress_string = fun data ->
  let buffer = Buffer.create ~size:128 in
  let gzip_reader = make_reader (Reader.from_string data) in
  let rec loop () =
    let scratch = Bytes.create ~size:input_buffer_size in
    match read_into gzip_reader scratch with
    | Ok 0 -> Ok ()
    | Ok read_len ->
        let block = Bytes.sub_unchecked scratch ~offset:0 ~len:read_len in
        begin
          match Buffer.append_bytes buffer block with
          | Ok () -> Ok ()
          | Error error ->
              panic ("Compress.Gzip.decompress_string.append: " ^ Kernel.IO.Error.message error)
        end
        |> Result.and_then ~fn:(fun () -> loop ())
    | Error (Source_error err) ->
        Error (Engine_error (Engine.Unknown_error ("unexpected in-memory source error: "
        ^ IO.error_message err)))
    | Error (Gzip_error err) -> Error err
  in
  match loop () with
  | Ok _ -> Ok (Buffer.contents buffer)
  | Error err -> Error err
