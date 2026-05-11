open Std

module Test = Std.Test
module H1 = Http.Http1
module H2 = Http.Http2
module Ws = Http.Ws

let accept = fun _ -> Ok ()

let check = fun label fn ->
  try
    fn ()
    |> ignore;
    Ok ()
  with
  | exn -> Error (label ^ ": " ^ Exception.to_string exn)

let ( let* ) = fun result fn ->
  match result with
  | Ok value -> fn value
  | Error _ as error -> error

let byte_mutator =
  Test.Fuzz.Mutator.(bytes
  |> with_max_len 4_096
  |> with_dictionary
    [
      "\r\n";
      "\r\n\r\n";
      "GET / HTTP/1.1\r\nHost: example.test\r\n\r\n";
      "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n";
      "Transfer-Encoding: chunked\r\n";
      "0\r\n\r\n";
      "data: hello\n\n";
      "Cookie: a=b; c=d";
      "\x00\x00\x00\x04\x00\x00\x00\x00\x00";
      "\x81\x05hello";
    ])

let test_http1_parsers_fuzz = fun _ctx input ->
  let* () =
    check
      "http1 request parse"
      (fun () ->
        H1.Request.parse
          ~max_request_line:1_024
          ~max_headers:32
          ~max_header_length:1_024
          ~max_header_block_length:4_096
          ~max_body_size:8_192
          ~max_chunk_size:4_096
          input)
  in
  let* () =
    check
      "http1 request head parse"
      (fun () ->
        H1.Request.parse_head
          ~max_request_line:1_024
          ~max_headers:32
          ~max_header_length:1_024
          ~max_header_block_length:4_096
          input)
  in
  let* () =
    check
      "http1 response parse"
      (fun () ->
        H1.Response.parse
          ~max_status_line:1_024
          ~max_headers:32
          ~max_header_length:1_024
          ~max_header_block_length:4_096
          ~max_body_size:8_192
          ~max_chunk_size:4_096
          input)
  in
  let* () =
    check
      "http1 response head parse"
      (fun () ->
        H1.Response.parse_head
          ~max_status_line:1_024
          ~max_headers:32
          ~max_header_length:1_024
          ~max_header_block_length:4_096
          input)
  in
  let* () = check "http1 chunk parse" (fun () -> H1.Chunk.parse input) in
  check
    "http1 chunk decode"
    (fun () ->
      H1.Chunk.decode ~max_chunk_size:4_096 ~max_body_size:8_192 input)

let test_http2_parsers_fuzz = fun _ctx input ->
  let* () = check "http2 frame header parse" (fun () -> H2.Parser.parse_frame_header input) in
  let* () = check "http2 frame parse" (fun () -> H2.Parser.parse_frame input) in
  check
    "http2 hpack decode"
    (fun () -> H2.Hpack.decode (H2.Hpack.create_decoder ()) (IO.Bytes.from_string input))

let test_ws_cookie_sse_parsers_fuzz = fun _ctx input ->
  let* () =
    check
      "websocket server parse"
      (fun () ->
        Ws.Parser.parse ~role:Ws.Parser.Server ~max_payload_length:65_536 input)
  in
  let* () =
    check
      "websocket client parse"
      (fun () ->
        Ws.Parser.parse ~role:Ws.Parser.Client ~max_payload_length:65_536 input)
  in
  let* () = check "cookie parse" (fun () -> H1.Cookie.parse input) in
  let* () = check "set-cookie parse" (fun () -> H1.Cookie.parse_set_cookie_result input) in
  let* () = check "sse parse" (fun () -> H1.Sse.parse input) in
  check "sse line parse" (fun () -> H1.Sse.parse_line input)

let tests =
  Test.[
    fuzz
      "http1 request response and chunk parsers accept arbitrary bytes"
      ~seeds:[
        "";
        "GET / HTTP/1.1\r\nHost: example.test\r\n\r\n";
        "POST / HTTP/1.1\r\nTransfer-Encoding: chunked\r\n\r\n0\r\n\r\n";
        "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nhello";
      ]
      ~mutator:byte_mutator
      test_http1_parsers_fuzz;
    fuzz
      "http2 frame and hpack parsers accept arbitrary bytes"
      ~seeds:[ ""; "\x00\x00\x00\x04\x00\x00\x00\x00\x00"; "\x82\x86\x84"; ]
      ~mutator:byte_mutator
      test_http2_parsers_fuzz;
    fuzz
      "websocket cookie and sse parsers accept arbitrary bytes"
      ~seeds:[ ""; "\x81\x05hello"; "a=b; c=d"; "data: hello\n\n"; ]
      ~mutator:byte_mutator
      test_ws_cookie_sse_parsers_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"http_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
