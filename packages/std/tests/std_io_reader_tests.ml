open Std

module IoVec = IO.IoVec
module Bytes = Kernel.Bytes

module FailingReader = struct
  type t = unit

  let read = fun () ~into:_ -> Error (IO.Unknown_error "boom")

  let read_vectored = fun () ~into:_ -> Error (IO.Unknown_error "boom")

  let is_read_vectored = fun () -> false
end

module CountingReader = struct
  type write_state = { mutable written: int }

  type t = {
    data: Bytes.t;
    mutable offset: int;
    mutable reads: int;
  }

  let create = fun input -> { data = Bytes.from_string input; offset = 0; reads = 0 }

  let read = fun t ~into ->
    t.reads <- t.reads + 1;
    let remaining = Bytes.length t.data - t.offset in
    let writable =
      if IO.Buffer.writable_bytes into = 0 then (
        match IO.Buffer.ensure_free into 4 with
        | Ok () -> IO.Buffer.writable into
        | Error error ->
            Kernel.SystemError.panic
              ("CountingReader.read.ensure_free: " ^ Kernel.IO.Error.message error)
      ) else
        IO.Buffer.writable into
    in
    let len = min remaining (IO.IoSlice.length writable) in
    if len > 0 then
      IO.IoSlice.blit_from_bytes_unchecked t.data ~src_off:t.offset writable ~dst_off:0 ~len;
    begin
      match IO.Buffer.commit into len with
      | Ok () -> ()
      | Error error ->
          Kernel.SystemError.panic ("CountingReader.read.commit: " ^ Kernel.IO.Error.message error)
    end;
    t.offset <- t.offset + len;
    Ok len

  let read_vectored = fun t ~into:bufs ->
    t.reads <- t.reads + 1;
    let remaining = Bytes.length t.data - t.offset in
    let total = min remaining (IoVec.length bufs) in
    let state: write_state = { written = 0 } in
    IoVec.for_each
      bufs
      ~fn:(fun segment ->
        if state.written < total then (
          let length = IoVec.IoSlice.length segment in
          let chunk_len = min length (total - state.written) in
          IoVec.IoSlice.blit_from_bytes_unchecked
            t.data
            ~src_off:(t.offset + state.written)
            segment
            ~dst_off:0
            ~len:chunk_len;
          state.written <- state.written + chunk_len
        ));
    t.offset <- t.offset + total;
    Ok total

  let is_read_vectored = fun _ -> true
end

let test_empty_reader_returns_zero = fun _ctx ->
  let buffer = IO.Buffer.create ~size:4 in
  match IO.read IO.Reader.empty ~into:buffer with
  | Ok 0 -> Ok ()
  | Ok _ -> Error "IO.Reader.empty should report EOF immediately"
  | Error _ -> Error "IO.Reader.empty should not fail"

let test_from_string_reads_small_buffers_sequentially = fun _ctx ->
  let reader = IO.Reader.from_string "hello" in
  let buffer = IO.Buffer.create ~size:2 in
  let rec loop acc =
    IO.Buffer.clear buffer;
    match IO.read reader ~into:buffer with
    | Ok 0 -> Ok (String.concat "" (List.reverse acc))
    | Ok _ -> loop (IO.Buffer.contents buffer :: acc)
    | Error _ -> Error "from_string should not fail"
  in
  match loop [] with
  | Ok actual when String.equal actual "hello" -> Ok ()
  | Ok _ -> Error "IO.Reader.from_string should return sequential chunks"
  | Error err -> Error err

let test_from_bytes_read_into_buffer_appends_available_content = fun _ctx ->
  let reader = IO.Reader.from_bytes (Bytes.from_string "hello") in
  let buffer = IO.Buffer.create ~size:2 in
  match IO.read reader ~into:buffer with
  | Ok read when Int.equal read 2 && String.equal (IO.Buffer.contents buffer) "he" -> Ok ()
  | Ok _ -> Error "IO.Reader.read should append one chunk into the destination buffer"
  | Error _ -> Error "IO.Reader.read should not fail for from_bytes"

let test_from_bytes_read_to_end_copies_entire_content = fun _ctx ->
  let reader = IO.Reader.from_bytes (Bytes.from_string "hello") in
  let buffer = IO.Buffer.create ~size:2 in
  match IO.read_to_end reader ~into:buffer with
  | Ok read when Int.equal read 5 && String.equal (IO.Buffer.contents buffer) "hello" -> Ok ()
  | Ok _ -> Error "IO.Reader.read_to_end should copy the full payload"
  | Error _ -> Error "IO.Reader.read_to_end should not fail for from_bytes"

let test_read_vectored_fills_segments_in_order = fun _ctx ->
  let reader = IO.Reader.from_string "hello" in
  let iov =
    IoVec.create ~count:2 ~size:5 ()
    |> Result.unwrap
  in
  match IO.read_vectored reader ~into:iov with
  | Ok read when Int.equal read 5 && String.equal (IoVec.to_string iov) "hello" -> Ok ()
  | Ok _ -> Error "IO.Reader.read_vectored should fill segments in order"
  | Error _ -> Error "IO.Reader.read_vectored should not fail for from_string"

let test_reader_propagates_io_errors = fun _ctx ->
  let reader = IO.Reader.from_source (module FailingReader) () in
  let buffer = IO.Buffer.create ~size:4 in
  match IO.read reader ~into:buffer with
  | Error (IO.Unknown_error "boom") -> Ok ()
  | Error _ -> Error "IO.Reader should preserve underlying IO.Error values"
  | Ok _ -> Error "IO.Reader should preserve failures"

let test_from_string_returns_zero_after_eof = fun _ctx ->
  let reader = IO.Reader.from_string "hi" in
  let buffer = IO.Buffer.create ~size:2 in
  match IO.read reader ~into:buffer with
  | Ok 2 ->
      IO.Buffer.clear buffer;
      (match IO.read reader ~into:buffer with
      | Ok 0 -> Ok ()
      | Ok _ -> Error "reads after EOF should keep returning 0"
      | Error _ -> Error "reads after EOF should not fail")
  | Ok _ -> Error "the first read should consume the full string"
  | Error _ -> Error "from_string should not fail"

let test_read_exact_reads_requested_bytes = fun _ctx ->
  let reader = IO.Reader.from_string "hello world" in
  let buffer = IO.Buffer.create ~size:5 in
  match IO.Reader.read_exact reader ~into:buffer ~len:5 with
  | Ok () when String.equal (IO.Buffer.contents buffer) "hello" -> Ok ()
  | Ok () -> Error "IO.Reader.read_exact should copy the requested prefix"
  | Error _ -> Error "IO.Reader.read_exact should not fail for in-memory readers"

let test_bufreader_amortizes_byte_reads = fun _ctx ->
  let src = CountingReader.create "hello" in
  let reader =
    IO.Reader.from_source (module CountingReader) src
    |> IO.BufReader.from_reader ~size:4
  in
  let rec loop acc =
    match IO.BufReader.read_byte reader with
    | Ok char -> loop (String.make ~len:1 ~char :: acc)
    | Error IO.End_of_file -> Ok (String.concat "" (List.reverse acc))
    | Error _ -> Error "buffered readers should not fail for counting reader"
  in
  match loop [] with
  | Ok actual when String.equal actual "hello" && src.reads <= 3 -> Ok ()
  | Ok _ -> Error "BufReader should serve small byte reads from local chunks"
  | Error err -> Error err

let test_bufreader_to_reader_exposes_generic_reads = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta"
    |> IO.BufReader.from_reader ~size:4
    |> IO.BufReader.to_reader
  in
  let buffer = IO.Buffer.create ~size:8 in
  match IO.Reader.read_to_end reader ~into:buffer with
  | Ok 10 when String.equal (IO.Buffer.contents buffer) "alpha\nbeta" -> Ok ()
  | Ok _ -> Error "BufReader.to_reader should preserve the underlying byte stream"
  | Error _ -> Error "BufReader.to_reader should not fail for in-memory strings"

let tests =
  Test.[
    case "empty readers return EOF immediately" test_empty_reader_returns_zero;
    case
      "from_string reads small buffers sequentially"
      test_from_string_reads_small_buffers_sequentially;
    case
      "from_bytes read appends one chunk"
      test_from_bytes_read_into_buffer_appends_available_content;
    case
      "from_bytes read_to_end copies the entire content"
      test_from_bytes_read_to_end_copies_entire_content;
    case "read_vectored fills segments in order" test_read_vectored_fills_segments_in_order;
    case "reader propagates io errors" test_reader_propagates_io_errors;
    case "from_string returns zero after EOF" test_from_string_returns_zero_after_eof;
    case "read_exact reads the requested bytes" test_read_exact_reads_requested_bytes;
    case "BufReader amortizes byte reads" test_bufreader_amortizes_byte_reads;
    case "BufReader.to_reader exposes generic reads" test_bufreader_to_reader_exposes_generic_reads;
  ]

let main ~args = Test.Cli.main ~name:"IO.Reader" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
