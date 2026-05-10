open Std

module IoVec = IO.IoVec

type sink = {
  mutable chunks: string list;
  mutable flushes: int;
  max_chunk: int option;
}

let create_sink = fun ?max_chunk () -> { chunks = []; flushes = 0; max_chunk }

let sink_contents = fun sink -> String.concat "" sink.chunks

module CollectWriter = struct
  type t = sink

  let write = fun sink ~from ->
    let requested = IO.Buffer.readable_bytes from in
    let written =
      match sink.max_chunk with
      | Some max_chunk -> Int.min max_chunk requested
      | None -> requested
    in
    let chunk =
      IO.Buffer.contents from
      |> String.sub ~offset:0 ~len:written
    in
    sink.chunks <- sink.chunks @ [ chunk ];
    Ok written

  let write_vectored = fun sink ~from ->
    let requested = IoVec.length from in
    let written =
      match sink.max_chunk with
      | Some max_chunk -> Int.min max_chunk requested
      | None -> requested
    in
    let chunk =
      IoVec.to_string from
      |> String.sub ~offset:0 ~len:written
    in
    sink.chunks <- sink.chunks @ [ chunk ];
    Ok written

  let flush = fun sink ->
    sink.flushes <- sink.flushes + 1;
    Ok ()
end

module FailingWriter = struct
  type t = unit

  let write = fun () ~from:_ -> Error (IO.Unknown_error "boom")

  let write_vectored = fun () ~from:_ -> Error (IO.Unknown_error "boom")

  let flush = fun () -> Error (IO.Unknown_error "boom")
end

let test_write_appends_exact_content = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  match IO.write writer ~from:(IO.Buffer.from_string "hello") with
  | Ok written when Int.equal written 5 && String.equal (sink_contents sink) "hello" -> Ok ()
  | Ok _ -> Error "IO.Writer.write should append the exact content"
  | Error _ -> Error "IO.Writer.write should not fail for the collecting sink"

let test_write_all_handles_partial_writes = fun _ctx ->
  let sink = create_sink ~max_chunk:2 () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  match IO.write_all writer ~from:(IO.Buffer.from_string "hello") with
  | Ok () when String.equal (sink_contents sink) "hello" -> Ok ()
  | Ok () -> Error "IO.Writer.write_all should keep writing until completion"
  | Error _ -> Error "IO.Writer.write_all should not fail for the collecting sink"

let test_write_vectored_appends_segment_content = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  let iov =
    IoVec.from_string_array [|"ab"; "cd"; "ef"|]
    |> Result.unwrap
  in
  match IO.write_vectored writer ~from:iov with
  | Ok written when Int.equal written 6 && String.equal (sink_contents sink) "abcdef" -> Ok ()
  | Ok _ -> Error "IO.Writer.write_vectored should append all segments"
  | Error _ -> Error "IO.Writer.write_vectored should not fail for the collecting sink"

let test_write_all_vectored_handles_partial_writes = fun _ctx ->
  let sink = create_sink ~max_chunk:2 () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  let iov =
    IoVec.from_string_array [|"ab"; "cd"; "ef"|]
    |> Result.unwrap
  in
  match IO.write_all_vectored writer ~from:iov with
  | Ok () when String.equal (sink_contents sink) "abcdef" -> Ok ()
  | Ok () -> Error "IO.Writer.write_all_vectored should keep writing until completion"
  | Error _ -> Error "IO.Writer.write_all_vectored should not fail for the collecting sink"

let test_writer_propagates_io_errors = fun _ctx ->
  let writer = IO.Writer.from_sink (module FailingWriter) () in
  match IO.write writer ~from:(IO.Buffer.from_string "hello") with
  | Error (IO.Unknown_error "boom") -> Ok ()
  | Error _ -> Error "IO.Writer should preserve underlying IO.Error values"
  | Ok _ -> Error "IO.Writer should preserve failures"

let test_flush_forwards_to_the_underlying_sink = fun _ctx ->
  let sink = create_sink () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  match IO.flush writer with
  | Ok () when Int.equal sink.flushes 1 -> Ok ()
  | Ok () -> Error "IO.Writer.flush should call the underlying sink"
  | Error _ -> Error "IO.Writer.flush should not fail for the collecting sink"

let test_reader_writer_copy_loop_reconstructs_payload = fun _ctx ->
  let reader = IO.Reader.from_string "hello world" in
  let sink = create_sink () in
  let writer = IO.Writer.from_sink (module CollectWriter) sink in
  let buffer = IO.Buffer.create ~size:4 in
  let rec loop () =
    IO.Buffer.clear buffer;
    match IO.read reader ~into:buffer with
    | Ok 0 -> Ok ()
    | Ok _ ->
        (match IO.write_all writer ~from:buffer with
        | Ok () -> loop ()
        | Error _ -> Error "writer unexpectedly failed")
    | Error _ -> Error "reader unexpectedly failed"
  in
  match loop () with
  | Ok () when String.equal (sink_contents sink) "hello world" -> Ok ()
  | Ok () -> Error "copy loop should reconstruct the original payload"
  | Error message -> Error message

let tests =
  Test.[
    case "write appends exact content" test_write_appends_exact_content;
    case "write_all handles partial writes" test_write_all_handles_partial_writes;
    case "write_vectored appends segment content" test_write_vectored_appends_segment_content;
    case "write_all_vectored handles partial writes" test_write_all_vectored_handles_partial_writes;
    case "writer propagates io errors" test_writer_propagates_io_errors;
    case "flush forwards to the underlying sink" test_flush_forwards_to_the_underlying_sink;
    case
      "reader writer copy loops reconstruct payloads"
      test_reader_writer_copy_loop_reconstructs_payload;
  ]

let main ~args = Test.Cli.main ~name:"IO.Writer" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
