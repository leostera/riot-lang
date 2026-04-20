open Std
open Http

(** HTTP/1 parser tests *)
(* Import parse_result constructors *)

open Http1.Common

(* For the Std.Net.Http accessor functions *)

module NetRequest = Std.Net.Http.Request
module NetResponse = Std.Net.Http.Response
module NetMethod = Std.Net.Http.Method
module NetVersion = Std.Net.Http.Version
module NetStatus = Std.Net.Http.Status
module Uri = Std.Net.Uri

let build_request = fun ~method_ ~path ~headers ~body ->
  let head =
    method_ ^ " " ^ path ^ " HTTP/1.1\r\n"
    ^ String.concat ""
        (List.map headers ~fn:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n"))
    ^ "\r\n"
  in
  head ^ body

let expect_request_parse = fun input ->
  match Http1.Request.parse input with
  | Done { value; remaining } ->
      Result.Ok (value, remaining)
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error error ->
      Result.Error ("Parse error: " ^ error)

let expect_request_parse_slice = fun input ->
  match Http1.Request.parse_slice (IO.Iovec.IoSlice.from_string input |> Result.unwrap) with
  | Done { value; remaining } ->
      Result.Ok (value, remaining)
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error error ->
      Result.Error ("Parse error: " ^ error)

let expect_request_parse_slices = fun input ->
  match Http1.Request.parse_slices (IO.Iovec.IoSlice.from_string input |> Result.unwrap) with
  | Borrowed_done { value; remaining } ->
      Result.Ok (value, remaining)
  | Borrowed_need_more ->
      Result.Error "Unexpected Borrowed_need_more"
  | Borrowed_error error ->
      Result.Error ("Parse error: " ^ error)

(* HTTP/1 Request Tests *)

let test_request_simple_get = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Done { value=parsed; _ } -> (
      let method_ = NetRequest.method_ parsed |> NetMethod.to_string in
      let uri = NetRequest.uri parsed in
      let path = Uri.path uri in
      let version = NetRequest.version parsed in
      if method_ != "GET" then
        Result.Error ("Expected method GET, got " ^ method_)
      else if path != "/path" then
        Result.Error ("Expected path /path, got " ^ path)
      else if version != NetVersion.Http11 then
        Result.Error "Expected version HTTP/1.1"
      else
        match NetRequest.get_header parsed "host" with
        | Some host when host = "example.com" -> Result.Ok ()
        | Some host -> Result.Error ("Expected Host: example.com, got " ^ host)
        | None -> Result.Error "Expected Host header"
    )
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error e ->
      Result.Error ("Parse error: " ^ e)

let test_request_post_with_body = fun _ctx ->
  let req = "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     \r\n\
     {\"key\":\"value\"}"
  in
  match Http1.Request.parse req with
  | Done { value=parsed; remaining } ->
      let method_ = NetRequest.method_ parsed |> NetMethod.to_string in
      let uri = NetRequest.uri parsed in
      let path = Uri.path uri in
      if method_ != "POST" then
        Result.Error ("Expected method POST, got " ^ method_)
      else if path != "/api/data" then
        Result.Error ("Expected path /api/data, got " ^ path)
      else if remaining != "{\"key\":\"value\"}" then
        Result.Error ("Expected body in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error e ->
      Result.Error ("Parse error: " ^ e)

let test_request_incomplete = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: exa" in
  match Http1.Request.parse req with
  | Need_more -> Result.Ok ()
  | Done _ -> Result.Error "Should have returned Need_more"
  | Error e -> Error ("Unexpected error: " ^ e)

let test_request_with_1k_body = fun _ctx ->
  let body = String.make ~len:1_024 ~char:'x' in
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[
        ("Host", "example.com");
        ("Content-Type", "application/octet-stream");
        ("Content-Length", Int.to_string (String.length body));
      ]
      ~body
  in
  match expect_request_parse req with
  | Error error ->
      Result.Error error
  | Ok (parsed, remaining) ->
      let path = NetRequest.uri parsed |> Uri.path in
      if path != "/upload" then
        Result.Error ("Expected /upload path, got " ^ path)
      else if String.length remaining != 1_024 then
        Result.Error
          ("Expected 1024-byte remaining body, got " ^ Int.to_string (String.length remaining))
      else
        Result.Ok ()

let test_request_with_100k_body = fun _ctx ->
  let body = String.make ~len:100_000 ~char:'y' in
  let req =
    build_request
      ~method_:"PUT"
      ~path:"/bulk"
      ~headers:[
        ("Host", "example.com");
        ("Content-Length", Int.to_string (String.length body));
      ]
      ~body
  in
  match expect_request_parse req with
  | Error error ->
      Result.Error error
  | Ok (_, remaining) ->
      if String.length remaining != 100_000 then
        Result.Error
          ("Expected 100000-byte remaining body, got " ^ Int.to_string (String.length remaining))
      else
        Result.Ok ()

let test_request_with_1m_body = fun _ctx ->
  let body = String.make ~len:1_000_000 ~char:'z' in
  let req =
    build_request
      ~method_:"PATCH"
      ~path:"/large"
      ~headers:[
        ("Host", "example.com");
        ("Content-Length", Int.to_string (String.length body));
      ]
      ~body
  in
  match expect_request_parse req with
  | Error error ->
      Result.Error error
  | Ok (_, remaining) ->
      if String.length remaining != 1_000_000 then
        Result.Error
          ("Expected 1000000-byte remaining body, got " ^ Int.to_string (String.length remaining))
      else
        Result.Ok ()

let test_request_parse_slice = fun _ctx ->
  let req = "GET /view HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match expect_request_parse_slice req with
  | Error error ->
      Result.Error error
  | Ok (parsed, remaining) ->
      let method_ = NetRequest.method_ parsed |> NetMethod.to_string in
      let path = NetRequest.uri parsed |> Uri.path in
      if method_ != "GET" then
        Result.Error ("Expected GET method, got " ^ method_)
      else if path != "/view" then
        Result.Error ("Expected /view path, got " ^ path)
      else if remaining != "" then
        Result.Error ("Expected empty remaining body, got " ^ remaining)
      else
        Result.Ok ()

let test_request_parse_slices = fun _ctx ->
  let req = "GET /view HTTP/1.1\r\nHost: example.com\r\nX-Test: ok\r\n\r\nbody" in
  match expect_request_parse_slices req with
  | Error error ->
      Result.Error error
  | Ok (parsed, remaining) ->
      let method_ = IO.Iovec.IoSlice.to_string parsed.method_ in
      let path = IO.Iovec.IoSlice.to_string parsed.path in
      let version = IO.Iovec.IoSlice.to_string parsed.version in
      let headers =
        List.map parsed.headers ~fn:(fun (name, value) ->
          (IO.Iovec.IoSlice.to_string name, IO.Iovec.IoSlice.to_string value))
      in
      let body = IO.Iovec.IoSlice.to_string parsed.body in
      let remaining = IO.Iovec.IoSlice.to_string remaining in
      if method_ != "GET" then
        Result.Error ("Expected GET method, got " ^ method_)
      else if path != "/view" then
        Result.Error ("Expected /view path, got " ^ path)
      else if version != "HTTP/1.1" then
        Result.Error ("Expected HTTP/1.1 version, got " ^ version)
      else if not (List.exists (fun header -> header = ("Host", "example.com")) headers) then
        Result.Error "Expected Host header"
      else if not (List.exists (fun header -> header = ("X-Test", "ok")) headers) then
        Result.Error "Expected X-Test header"
      else if body != "body" then
        Result.Error ("Expected body slice, got " ^ body)
      else if remaining != "body" then
        Result.Error ("Expected remaining body slice, got " ^ remaining)
      else
        Result.Ok ()

let test_request_missing_lf_after_request_line_current_behavior = fun _ctx ->
  let req = "GET /path HTTP/1.1\rHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Done _ ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected current Done behavior, got error " ^ error)
  | Need_more ->
      Result.Error "Expected current Done behavior, got Need_more"

let test_request_rejects_too_many_headers = fun _ctx ->
  let headers =
    let rec loop index acc =
      if index < 0 then
        acc
      else
        loop (index - 1) (("X-Test-" ^ Int.to_string index, "value") :: acc)
    in
    loop 100 []
  in
  let req = build_request ~method_:"GET" ~path:"/" ~headers ~body:"" in
  match Http1.Request.parse req with
  | Error "Too many headers" ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected too many headers error, got " ^ error)
  | Need_more ->
      Result.Error "Expected too many headers error"
  | Done _ ->
      Result.Error "Expected too many headers error"

(* HTTP/1 Response Tests *)

let test_response_200_ok = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Done { value=parsed; remaining } ->
      let status = NetResponse.status parsed |> NetStatus.to_int in
      let version = NetResponse.version parsed in
      if status != 200 then
        Result.Error ("Expected status 200, got " ^ Int.to_string status)
      else if version != NetVersion.Http11 then
        Result.Error "Expected version HTTP/1.1"
      else if remaining != "Hello" then
        Result.Error ("Expected body Hello in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error e ->
      Result.Error ("Parse error: " ^ e)

let test_response_404 = fun _ctx ->
  let resp = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n" in
  match Http1.Response.parse resp with
  | Done { value=parsed; _ } ->
      let status = NetResponse.status parsed |> NetStatus.to_int in
      if status != 404 then
        Result.Error ("Expected status 404, got " ^ Int.to_string status)
      else
        Result.Ok ()
  | Need_more ->
      Result.Error "Unexpected Need_more"
  | Error e ->
      Result.Error ("Parse error: " ^ e)

(* Chunked Encoding Tests *)

let test_chunk_single = fun _ctx ->
  let chunk = "5\r\nHello\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value=chunk_result; _ } ->
      if chunk_result.data != "Hello" then
        Result.Error "Expected data Hello"
      else if chunk_result.remaining != "" then
        Result.Error "Expected empty remaining"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ e)

let test_chunk_last = fun _ctx ->
  let chunk = "0\r\n\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value=chunk_result; _ } ->
      if chunk_result.data != "" then
        Result.Error "Expected empty data"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ e)

let test_chunk_hex_size = fun _ctx ->
  let chunk = "1a\r\nabcdefghijklmnopqrstuvwxyz\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value=chunk_result; _ } ->
      if String.length chunk_result.data != 26 then
        Result.Error ("Expected length 26, got " ^ Int.to_string (String.length chunk_result.data))
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ e)

(* SSE Tests *)

let test_sse_data_line = fun _ctx ->
  match Http1.Sse.parse_line "data: Hello World" with
  | Some event ->
      if event.data != "Hello World" then
        Result.Error "Expected data Hello World"
      else if event.event_type != None then
        Result.Error "Expected no event type"
      else
        Result.Ok ()
  | None -> Result.Error "Failed to parse data line"

let test_sse_event_type = fun _ctx ->
  match Http1.Sse.parse_line "event: message" with
  | Some event ->
      if event.event_type != Some "message" then
        Result.Error "Expected event type message"
      else
        Result.Ok ()
  | None -> Result.Error "Failed to parse event type"

let test_sse_empty_line = fun _ctx ->
  match Http1.Sse.parse_line "" with
  | None -> Result.Ok ()
  | Some _ -> Result.Error "Should have ignored empty line"

let test_sse_comment = fun _ctx ->
  match Http1.Sse.parse_line ": this is a comment" with
  | None -> Result.Ok ()
  | Some _ -> Result.Error "Should have ignored comment"

let tests =
  Test.[
    case "request_simple_get" test_request_simple_get;
    case "request_post_with_body" test_request_post_with_body;
    case "request_incomplete" test_request_incomplete;
    case "request_with_1k_body" test_request_with_1k_body;
    case "request_with_100k_body" test_request_with_100k_body;
    case "request_with_1m_body" test_request_with_1m_body;
    case "request_parse_slice" test_request_parse_slice;
    case "request_parse_slices" test_request_parse_slices;
    case
      "request_missing_lf_after_request_line_current_behavior"
      test_request_missing_lf_after_request_line_current_behavior;
    case "request_rejects_too_many_headers" test_request_rejects_too_many_headers;
    case "response_200_ok" test_response_200_ok;
    case "response_404" test_response_404;
    case "chunk_single" test_chunk_single;
    case "chunk_last" test_chunk_last;
    case "chunk_hex_size" test_chunk_hex_size;
    case "sse_data_line" test_sse_data_line;
    case "sse_event_type" test_sse_event_type;
    case "sse_empty_line" test_sse_empty_line;
    case "sse_comment" test_sse_comment;
  ]

let () =
  Actors.run
    ~main:(fun ~args:_ -> Test.Cli.main ~name:"http:http1_parser" ~tests ~args:Env.args)
    ~args:Env.args
    ()
