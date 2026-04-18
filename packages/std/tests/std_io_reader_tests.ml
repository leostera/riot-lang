open Std

module Iovec = IO.Iovec
module Bytes = Kernel.Bytes

module FailingReader = struct
  type t = unit
  type err = string

  let read = fun () ?timeout:_ _ -> Error "boom"
  let read_vectored = fun () _ -> Error "boom"
end

module CountingReader = struct
  type write_state = {
    mutable written: int;
  }

  type t = {
    data: Bytes.t;
    mutable offset: int;
    mutable reads: int;
  }

  type err = unit

  let create = fun input ->
    {
      data = Bytes.from_string input;
      offset = 0;
      reads = 0;
    }

  let read = fun t ?timeout:_ buffer ->
    t.reads <- t.reads + 1;
    let remaining = Bytes.length t.data - t.offset in
    let len = min remaining (Bytes.length buffer) in
    if len > 0 then
      Bytes.blit_unchecked t.data ~src_offset:t.offset ~dst:buffer ~dst_offset:0 ~len;
    t.offset <- t.offset + len;
    Ok len

  let read_vectored = fun t bufs ->
    t.reads <- t.reads + 1;
  let remaining = Bytes.length t.data - t.offset in
  let total = min remaining (Iovec.length bufs) in
  let state: write_state = { written = 0 } in
  Iovec.for_each bufs ~fn:(fun segment ->
    if state.written < total then (
      let length = Iovec.IoSlice.length segment in
      let chunk_len = min length (total - state.written) in
      Iovec.IoSlice.blit_from_bytes
        t.data
        ~src_offset:(t.offset + state.written)
        ~dst:segment
        ~dst_offset:0
        ~len:chunk_len;
      state.written <- state.written + chunk_len
    ));
    t.offset <- t.offset + total;
    Ok total
end

let test_empty_reader_returns_zero = fun _ctx ->
  let buffer = IO.Bytes.create ~size:4 in
  match IO.read IO.Reader.empty buffer with
  | Ok 0 -> Ok ()
  | Ok _ -> Error "IO.Reader.empty should report EOF immediately"
  | Error () -> Error "IO.Reader.empty should not fail"

let test_from_string_reads_small_buffers_sequentially = fun _ctx ->
  let reader = IO.Reader.from_string "hello" in
  let buffer = IO.Bytes.create ~size:2 in
  let rec loop acc =
    match IO.read reader buffer with
    | Ok 0 -> Ok (String.concat "" (List.reverse acc))
    | Ok len ->
        let chunk = Bytes.sub_string buffer ~offset:0 ~len in
        loop (chunk :: acc)
    | Error () -> Error "from_string should not fail"
  in
  match loop [] with
  | Ok actual when String.equal actual "hello" -> Ok ()
  | Ok _ -> Error "IO.Reader.from_string should return sequential chunks"
  | Error err -> Error err

let test_from_bytes_read_to_end_copies_entire_content = fun _ctx ->
  let reader = IO.Reader.from_bytes (Bytes.from_string "hello") in
  let buffer = IO.Buffer.create ~size:2 in
  match IO.read_to_end reader ~buf:buffer with
  | Ok read when Int.equal read 5 && String.equal (IO.Buffer.contents buffer) "hello" -> Ok ()
  | Ok _ -> Error "IO.Reader.read_to_end should copy the full payload"
  | Error () -> Error "IO.Reader.read_to_end should not fail for from_bytes"

let test_read_vectored_fills_segments_in_order = fun _ctx ->
  let reader = IO.Reader.from_string "hello" in
  let iov = Iovec.create ~count:2 ~size:5 () in
  match IO.read_vectored reader iov with
  | Ok read when Int.equal read 5 && String.equal (Iovec.to_string iov) "hello" ->
      Ok ()
  | Ok _ -> Error "IO.Reader.read_vectored should fill segments in order"
  | Error () -> Error "IO.Reader.read_vectored should not fail for from_string"

let test_map_err_transforms_reader_errors = fun _ctx ->
  let reader =
    IO.Reader.of_read_src (module FailingReader) ()
    |> IO.Reader.map_err ~fn:String.uppercase_ascii
  in
  let buffer = IO.Bytes.create ~size:4 in
  match IO.read reader buffer with
  | Error err when String.equal err "BOOM" -> Ok ()
  | Error _ -> Error "IO.Reader.map_err returned the wrong transformed error"
  | Ok _ -> Error "IO.Reader.map_err should preserve failures"

let test_from_string_returns_zero_after_eof = fun _ctx ->
  let reader = IO.Reader.from_string "hi" in
  let buffer = IO.Bytes.create ~size:2 in
  match IO.read reader buffer with
  | Ok 2 -> (
      match IO.read reader buffer with
      | Ok 0 -> Ok ()
      | Ok _ -> Error "reads after EOF should keep returning 0"
      | Error () -> Error "reads after EOF should not fail")
  | Ok _ -> Error "the first read should consume the full string"
  | Error () -> Error "from_string should not fail"

let test_zero_length_read_buffer_returns_zero = fun _ctx ->
  let reader = IO.Reader.from_string "hi" in
  let buffer = IO.Bytes.create ~size:0 in
  match IO.read reader buffer with
  | Ok 0 -> Ok ()
  | Ok _ -> Error "reading into a zero-length buffer should return 0"
  | Error () -> Error "reading into a zero-length buffer should not fail"

let test_buffered_reader_amortizes_char_reads = fun _ctx ->
  let src = CountingReader.create "hello" in
  let reader =
    IO.Reader.of_read_src (module CountingReader) src
    |> IO.buffered ~chunk_size:4 ()
  in
  let rec loop acc =
    match IO.read_char reader with
    | Ok None -> Ok (String.concat "" (List.reverse acc))
    | Ok (Some char) -> loop (String.make ~len:1 ~char :: acc)
    | Error () -> Error "buffered readers should not fail for counting reader"
  in
  match loop [] with
  | Ok actual when String.equal actual "hello" && src.reads <= 3 -> Ok ()
  | Ok _ -> Error "buffered readers should serve small char reads from local chunks"
  | Error err -> Error err

let test_buffered_reader_read_line_uses_generic_io_surface = fun _ctx ->
  let reader =
    IO.Reader.from_string "alpha\nbeta"
    |> IO.buffered ()
  in
  match IO.read_line reader with
  | Ok "alpha\n" -> (
      match IO.read_line reader with
      | Ok "beta" -> Ok ()
      | Ok _ -> Error "buffered readers should preserve remaining line content"
      | Error () -> Error "buffered readers should not fail for in-memory strings")
  | Ok _ -> Error "buffered readers should preserve newline-terminated lines"
  | Error () -> Error "buffered readers should not fail for in-memory strings"

let test_reader_make_derives_line_and_string_reads = fun _ctx ->
  let reader = IO.Reader.from_string "custom-line\ntext" in
  match IO.read_line reader with
  | Ok "custom-line\n" -> (
      match IO.read_to_string reader ~len:4 with
      | Ok "text" -> Ok ()
      | Ok _ -> Error "Reader.make should derive read_to_string from read"
      | Error () -> Error "Reader.make should derive read_to_string from read")
  | Ok _ -> Error "Reader.make should derive read_line from read"
  | Error () -> Error "Reader.make should derive read_line from read"

let tests = Test.[
  case "empty readers return EOF immediately" test_empty_reader_returns_zero;
  case "from_string reads small buffers sequentially" test_from_string_reads_small_buffers_sequentially;
  case "from_bytes read_to_end copies the entire content" test_from_bytes_read_to_end_copies_entire_content;
  case "read_vectored fills segments in order" test_read_vectored_fills_segments_in_order;
  case "map_err transforms reader errors" test_map_err_transforms_reader_errors;
  case "from_string returns zero after EOF" test_from_string_returns_zero_after_eof;
  case "reading into a zero-length buffer returns zero" test_zero_length_read_buffer_returns_zero;
  case "buffered readers amortize char reads" test_buffered_reader_amortizes_char_reads;
  case "buffered readers expose line reads through Std.IO" test_buffered_reader_read_line_uses_generic_io_surface;
  case "Reader.make derives line and string reads" test_reader_make_derives_line_and_string_reads;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"IO.Reader" ~tests ~args) ~args:Env.args ()
