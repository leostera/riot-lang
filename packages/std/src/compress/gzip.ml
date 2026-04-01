open Global
open IO

let ( let* ) = Result.and_then

type error =
  | Kernel_error of Kernel.Compress.Gzip.error
  | Truncated_input

type ('src, 'read_err) reader = {
  src: ('src, 'read_err) Reader.t;
  decoder: Kernel.Compress.Gzip.decoder;
  input: Bytes.t;
  mutable input_pos: int;
  mutable input_len: int;
  mutable source_eof: bool;
  mutable finished: bool;
}

type 'read_err read_error =
  | Source_error of 'read_err
  | Decode_error of error

type ('read_err, 'write_err) stream_error =
  | Stream_source_error of 'read_err
  | Stream_destination_error of 'write_err
  | Stream_decode_error of error

type file_error =
  | File_io_error of Fs.error
  | File_decode_error of error

let input_buffer_size = 32 * 1_024

let error_of_kernel = fun err -> Kernel_error err

let string_of_kernel_error = function
  | Kernel.Compress.Gzip.Invalid_data -> "invalid gzip data"
  | Kernel.Compress.Gzip.Need_dictionary -> "gzip stream requires a preset dictionary"
  | Kernel.Compress.Gzip.Buffer_error -> "gzip decoder buffer error"
  | Kernel.Compress.Gzip.Out_of_memory -> "gzip decoder out of memory"
  | Kernel.Compress.Gzip.Unknown_error msg -> msg

let buffered_input = fun (t: (_, _) reader) -> t.input_len - t.input_pos

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
      Bytes.blit t.input t.input_pos t.input 0 remaining;
      t.input_pos <- 0;
      t.input_len <- remaining
    )

let refill_input = fun (t: (_, _) reader) ->
  compact_input t;
  let available = Bytes.length t.input - t.input_len in
  if available = 0 then
    Error (Decode_error (Kernel_error Kernel.Compress.Gzip.Buffer_error))
  else
    let chunk = Bytes.create available in
    match IO.read t.src chunk with
    | Ok 0 ->
        t.source_eof <- true;
        Ok false
    | Ok bytes_read ->
        Bytes.blit chunk 0 t.input t.input_len bytes_read;
        t.input_len <- t.input_len + bytes_read;
        Ok true
    | Error err ->
        Error (Source_error err)

let read_into = fun (t: (_, _) reader) dst ->
  let rec loop () =
    if t.finished then
      Ok 0
    else
      let dst_len = Bytes.length dst in
      if dst_len = 0 then
        Ok 0
      else
        let available_input = buffered_input t in
        if available_input = 0 then
          if t.source_eof then
            Error (Decode_error Truncated_input)
          else
            match refill_input t with
            | Error err ->
                Error err
            | Ok false ->
                Error (Decode_error Truncated_input)
            | Ok true ->
                loop ()
        else
          match Kernel.Compress.Gzip.decode
                  t.decoder
                  ~src:t.input
                  ~src_pos:t.input_pos
                  ~src_len:available_input
                  ~dst
                  ~dst_pos:0
                  ~dst_len
          with
          | Error err ->
              Error (Decode_error (error_of_kernel err))
          | Ok step ->
              t.input_pos <- t.input_pos + step.consumed;
              if step.status = Kernel.Compress.Gzip.Finished then
                t.finished <- true;
              if step.produced > 0 then
                Ok step.produced
              else (
                match step.status with
                | Kernel.Compress.Gzip.Finished ->
                    Ok 0
                | Kernel.Compress.Gzip.Need_input ->
                    if t.source_eof then
                      Error (Decode_error Truncated_input)
                    else (
                      match refill_input t with
                      | Error err ->
                          Error err
                      | Ok false ->
                          Error (Decode_error Truncated_input)
                      | Ok true ->
                          loop ()
                    )
                | Kernel.Compress.Gzip.Need_output ->
                    Error (Decode_error (Kernel_error Kernel.Compress.Gzip.Buffer_error))
              )
  in
  loop ()

let to_reader : type src read_err.
  (src, read_err) Reader.t ->
  ((src, read_err) reader, read_err read_error) Reader.t = fun src ->
  let decoder =
    match Kernel.Compress.Gzip.create_decoder () with
    | Ok decoder ->
        decoder
    | Error err ->
        panic ("failed to create gzip decoder: " ^ string_of_kernel_error err)
  in
  let state = {
    src;
    decoder;
    input = Bytes.create input_buffer_size;
    input_pos = 0;
    input_len = 0;
    source_eof = false;
    finished = false;
  } in
  let module Read = struct
    type t = (src, read_err) reader

    type err = read_err read_error

    let read = fun t ?timeout:_ buf -> read_into t buf

    let read_vectored = fun t bufs ->
      let total_len = Iovec.length bufs in
      let scratch = Bytes.create total_len in
      match read_into t scratch with
      | Error err ->
          Error err
      | Ok read_len ->
          let copied = ref 0 in
          Iovec.iter bufs
            (fun { ba; off; len } ->
              let remaining = read_len - !copied in
              if remaining > 0 then
                let chunk_len = min len remaining in
                Bytes.blit scratch !copied ba off chunk_len;
                copied := !copied + chunk_len);
          Ok read_len
  end in
  Reader.of_read_src (module Read) state

let decompress = fun src dst ->
  let gzip_reader = to_reader src in
  let chunk = Bytes.create input_buffer_size in
  let rec loop chunk_count =
    match IO.read gzip_reader chunk with
    | Ok 0 ->
        IO.flush dst |> Result.map_err (fun err -> Stream_destination_error err)
    | Ok bytes_read ->
        let () =
          if chunk_count > 0 && Int.rem chunk_count 32 = 0 then
            yield ()
        in
        let buf = Bytes.sub_string chunk 0 bytes_read in
        let* () =
          IO.write_all dst ~buf
          |> Result.map_err (fun err -> Stream_destination_error err)
        in
        loop (chunk_count + 1)
    | Error (Source_error err) ->
        Error (Stream_source_error err)
    | Error (Decode_error err) ->
        Error (Stream_decode_error err)
  in
  loop 0

let with_open_input = fun path fn ->
  match Fs.File.open_read path with
  | Error err ->
      Error (File_io_error err)
  | Ok file ->
      Kernel.Fun.protect
        ~finally:(fun () -> ignore (Fs.File.close file))
        (fun () -> fn file)

let with_open_output = fun path fn ->
  match Fs.File.create path with
  | Error err ->
      Error (File_io_error err)
  | Ok file ->
      Kernel.Fun.protect
        ~finally:(fun () -> ignore (Fs.File.close file))
        (fun () -> fn file)

let decompress_file = fun ~src ~dst ->
  with_open_input src
    (fun src_file ->
      with_open_output dst
        (fun dst_file ->
          decompress (Fs.File.to_reader src_file) (Fs.File.to_writer dst_file)
          |> Result.map_err
               (function
                | Stream_source_error err ->
                    File_io_error err
                | Stream_destination_error err ->
                    File_io_error err
                | Stream_decode_error err ->
                    File_decode_error err)))

let decompress_string = fun data ->
  let gzip_reader = to_reader (Reader.from_string data) in
  let buffer = Buffer.create 128 in
  match Reader.read_to_end gzip_reader ~buf:buffer with
  | Ok _ ->
      Ok (Buffer.contents buffer)
  | Error (Source_error ()) ->
      Error (Kernel_error (Kernel.Compress.Gzip.Unknown_error "unexpected in-memory source error"))
  | Error (Decode_error err) ->
      Error err
