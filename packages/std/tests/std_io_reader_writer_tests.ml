open Std

module Iovec = IO.Iovec
module Bytes = Kernel.Bytes

type sink = {
  mutable chunks: string list;
  mutable flushes: int;
  max_chunk: int option;
}

let create_sink = fun ?max_chunk () -> { chunks = []; flushes = 0; max_chunk }

let sink_contents = fun sink -> String.concat "" sink.chunks

module CollectWriter = struct
  type t = sink
  type err = string

  let write = fun sink ~buf ->
    let requested = String.length buf in
    let written =
      match sink.max_chunk with
      | Some max_chunk -> Int.min max_chunk requested
      | None -> requested
    in
    let chunk = String.sub buf ~offset:0 ~len:written in
    sink.chunks <- sink.chunks @ [ chunk ];
    Ok written

  let write_owned_vectored = fun sink ~bufs ->
    write sink ~buf:(Iovec.to_string bufs)

  let flush = fun sink ->
    sink.flushes <- sink.flushes + 1;
    Ok ()
end

module FailingReader = struct
  type t = unit
  type err = string

  let read = fun () ?timeout:_ _ -> Error "boom"
  let read_vectored = fun () _ -> Error "boom"
end

module FailingWriter = struct
  type t = unit
  type err = string

  let write = fun () ~buf:_ -> Error "boom"
  let write_owned_vectored = fun () ~bufs:_ -> Error "boom"
  let flush = fun () -> Error "boom"
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
  let first = Bytes.create ~size:2 in
  let second = Bytes.create ~size:3 in
  let iov = Iovec.from_bytes_array [| first; second |] in
  match IO.read_vectored reader iov with
  | Ok read when Int.equal read 5 && String.equal (Bytes.to_string first ^ Bytes.to_string second) "hello" ->
      Ok ()
  | Ok _ -> Error "IO.Reader.read_vectored should fill segments in order"
  | Error () -> Error "IO.Reader.read_vectored should not fail for from_string"

let test_map_err_transforms_reader_errors = fun _ctx ->
  let reader = IO.Reader.of_read_src (module FailingReader) () |> IO.Reader.map_err ~fn:String.uppercase_ascii in
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

let test_write_appends_exact_content = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  match IO.write writer ~buf:"hello" with
  | Ok written when Int.equal written 5 && String.equal (sink_contents sink) "hello" -> Ok ()
  | Ok _ -> Error "IO.Writer.write should append the exact content"
  | Error _ -> Error "IO.Writer.write should not fail for the collecting sink"

let test_write_all_handles_partial_writes = fun _ctx ->
  let sink = create_sink ~max_chunk:2 () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  match IO.write_all writer ~buf:"hello" with
  | Ok () when String.equal (sink_contents sink) "hello" -> Ok ()
  | Ok () -> Error "IO.Writer.write_all should keep writing until completion"
  | Error _ -> Error "IO.Writer.write_all should not fail for the collecting sink"

let test_write_owned_vectored_appends_segment_content = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  let iov = Iovec.from_string_array [| "ab"; "cd"; "ef" |] in
  match IO.write_owned_vectored writer ~bufs:iov with
  | Ok written when Int.equal written 6 && String.equal (sink_contents sink) "abcdef" -> Ok ()
  | Ok _ -> Error "IO.Writer.write_owned_vectored should append all segments"
  | Error _ -> Error "IO.Writer.write_owned_vectored should not fail for the collecting sink"

let test_write_all_vectored_handles_partial_writes = fun _ctx ->
  let sink = create_sink ~max_chunk:2 () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  let iov = Iovec.from_string_array [| "ab"; "cd"; "ef" |] in
  match IO.write_all_vectored writer ~bufs:iov with
  | Ok () when String.equal (sink_contents sink) "abcdef" -> Ok ()
  | Ok () -> Error "IO.Writer.write_all_vectored should keep writing until completion"
  | Error _ -> Error "IO.Writer.write_all_vectored should not fail for the collecting sink"

let test_map_err_transforms_writer_errors = fun _ctx ->
  let writer = IO.Writer.of_write_src (module FailingWriter) () |> IO.Writer.map_err ~fn:String.uppercase_ascii in
  match IO.write writer ~buf:"hello" with
  | Error err when String.equal err "BOOM" -> Ok ()
  | Error _ -> Error "IO.Writer.map_err returned the wrong transformed error"
  | Ok _ -> Error "IO.Writer.map_err should preserve failures"

let test_flush_forwards_to_the_underlying_sink = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  match IO.flush writer with
  | Ok () when Int.equal sink.flushes 1 -> Ok ()
  | Ok () -> Error "IO.Writer.flush should call the underlying sink"
  | Error _ -> Error "IO.Writer.flush should not fail for the collecting sink"

let test_reader_writer_copy_loop_reconstructs_payload = fun _ctx ->
  let reader = IO.Reader.from_string "hello world" in
  let sink = create_sink () in
  let writer = IO.Writer.of_write_src (module CollectWriter) sink in
  let buffer = IO.Bytes.create ~size:4 in
  let rec loop () =
    match IO.read reader buffer with
    | Ok 0 -> Ok ()
    | Ok len ->
        let chunk = Bytes.sub_string buffer ~offset:0 ~len in
        (match IO.write_all writer ~buf:chunk with
        | Ok () -> loop ()
        | Error err -> Error err)
    | Error () -> Error "reader unexpectedly failed"
  in
  match loop () with
  | Ok () when String.equal (sink_contents sink) "hello world" -> Ok ()
  | Ok () -> Error "copy loop should reconstruct the original payload"
  | Error err -> Error err

let tests = Test.[
  case "empty readers return EOF immediately" test_empty_reader_returns_zero;
  case "from_string reads small buffers sequentially" test_from_string_reads_small_buffers_sequentially;
  case "from_bytes read_to_end copies the entire content" test_from_bytes_read_to_end_copies_entire_content;
  case "read_vectored fills segments in order" test_read_vectored_fills_segments_in_order;
  case "map_err transforms reader errors" test_map_err_transforms_reader_errors;
  case "from_string returns zero after EOF" test_from_string_returns_zero_after_eof;
  case "reading into a zero-length buffer returns zero" test_zero_length_read_buffer_returns_zero;
  case "write appends exact content" test_write_appends_exact_content;
  case "write_all handles partial writes" test_write_all_handles_partial_writes;
  case "write_owned_vectored appends segment content" test_write_owned_vectored_appends_segment_content;
  case "write_all_vectored handles partial writes" test_write_all_vectored_handles_partial_writes;
  case "map_err transforms writer errors" test_map_err_transforms_writer_errors;
  case "flush forwards to the underlying sink" test_flush_forwards_to_the_underlying_sink;
  case "reader writer copy loops reconstruct payloads" test_reader_writer_copy_loop_reconstructs_payload;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"io_reader_writer" ~tests ~args) ~args:Env.args ()
