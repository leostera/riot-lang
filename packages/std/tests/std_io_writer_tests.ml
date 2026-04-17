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

module FailingWriter = struct
  type t = unit
  type err = string

  let write = fun () ~buf:_ -> Error "boom"
  let write_owned_vectored = fun () ~bufs:_ -> Error "boom"
  let flush = fun () -> Error "boom"
end

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
  let writer =
    IO.Writer.of_write_src (module FailingWriter) ()
    |> IO.Writer.map_err ~fn:String.uppercase_ascii
  in
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
  case "write appends exact content" test_write_appends_exact_content;
  case "write_all handles partial writes" test_write_all_handles_partial_writes;
  case "write_owned_vectored appends segment content" test_write_owned_vectored_appends_segment_content;
  case "write_all_vectored handles partial writes" test_write_all_vectored_handles_partial_writes;
  case "map_err transforms writer errors" test_map_err_transforms_writer_errors;
  case "flush forwards to the underlying sink" test_flush_forwards_to_the_underlying_sink;
  case "reader writer copy loops reconstruct payloads" test_reader_writer_copy_loop_reconstructs_payload;
]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"IO.Writer" ~tests ~args) ~args:Env.args ()
