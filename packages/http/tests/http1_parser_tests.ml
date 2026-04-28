open Std
open Http
(** HTTP/1 parser tests *)
(* Import parse_result constructors *)

open Http1.Common

(* For the Std.Net.Http accessor functions *)

module NetRequest = Std.Net.Http.Request
module NetResponse = Std.Net.Http.Response
module NetBody = Std.Net.Http.Body
module NetMethod = Std.Net.Http.Method
module NetVersion = Std.Net.Http.Version
module NetStatus = Std.Net.Http.Status
module Uri = Std.Net.Uri

let build_request = fun ~method_ ~path ~headers ~body ->
  let head =
    method_
    ^ " "
    ^ path
    ^ " HTTP/1.1\r\n"
    ^ String.concat "" (List.map headers ~fn:(fun (name, value) -> name ^ ": " ^ value ^ "\r\n"))
    ^ "\r\n"
  in
  head ^ body

let expect_request_parse = fun input ->
  match Http1.Request.parse input with
  | Done { value; remaining } -> Result.Ok (value, remaining)
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let expect_request_parse_slice = fun input ->
  match Http1.Request.parse_slice
    (
      IO.IoVec.IoSlice.from_string input
      |> Result.unwrap
    ) with
  | Done { value; remaining } -> Result.Ok (value, remaining)
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

(* HTTP/1 Request Tests *)

let test_request_simple_get = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Done { value = parsed; _ } -> (
      let method_ =
        NetRequest.method_ parsed
        |> NetMethod.to_string
      in
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
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_request_post_with_body = fun _ctx ->
  let req =
    "POST /api/data HTTP/1.1\r\n\
     Host: api.example.com\r\n\
     Content-Type: application/json\r\n\
     Content-Length: 15\r\n\
     \r\n\
     {\"key\":\"value\"}"
  in
  match Http1.Request.parse req with
  | Done { value = parsed; remaining } ->
      let method_ =
        NetRequest.method_ parsed
        |> NetMethod.to_string
      in
      let uri = NetRequest.uri parsed in
      let path = Uri.path uri in
      let body =
        NetRequest.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if method_ != "POST" then
        Result.Error ("Expected method POST, got " ^ method_)
      else if path != "/api/data" then
        Result.Error ("Expected path /api/data, got " ^ path)
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else if body != Some "{\"key\":\"value\"}" then
        Result.Error "Expected request body on parsed request"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_request_incomplete = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: exa" in
  match Http1.Request.parse req with
  | Need_more -> Result.Ok ()
  | Done _ -> Result.Error "Should have returned Need_more"
  | Error e -> Error ("Unexpected error: " ^ error_to_string e)

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
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let path =
        NetRequest.uri parsed
        |> Uri.path
      in
      let body_length =
        match NetRequest.body parsed with
        | None -> 0
        | Some body -> NetBody.length body
      in
      if path != "/upload" then
        Result.Error ("Expected /upload path, got " ^ path)
      else if remaining != "" then
        Result.Error ("Expected empty remaining body, got " ^ remaining)
      else if body_length != 1_024 then
        Result.Error ("Expected 1024-byte request body, got " ^ Int.to_string body_length)
      else
        Result.Ok ()

let test_request_with_100k_body = fun _ctx ->
  let body = String.make ~len:100_000 ~char:'y' in
  let req =
    build_request
      ~method_:"PUT"
      ~path:"/bulk"
      ~headers:[ ("Host", "example.com"); ("Content-Length", Int.to_string (String.length body)); ]
      ~body
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body_length =
        match NetRequest.body parsed with
        | None -> 0
        | Some body -> NetBody.length body
      in
      if remaining != "" then
        Result.Error ("Expected empty remaining body, got " ^ remaining)
      else if body_length != 100_000 then
        Result.Error ("Expected 100000-byte request body, got " ^ Int.to_string body_length)
      else
        Result.Ok ()

let test_request_with_1m_body = fun _ctx ->
  let body = String.make ~len:1_000_000 ~char:'z' in
  let req =
    build_request
      ~method_:"PATCH"
      ~path:"/large"
      ~headers:[ ("Host", "example.com"); ("Content-Length", Int.to_string (String.length body)); ]
      ~body
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body_length =
        match NetRequest.body parsed with
        | None -> 0
        | Some body -> NetBody.length body
      in
      if remaining != "" then
        Result.Error ("Expected empty remaining body, got " ^ remaining)
      else if body_length != 1_000_000 then
        Result.Error ("Expected 1000000-byte request body, got " ^ Int.to_string body_length)
      else
        Result.Ok ()

let test_request_incomplete_fixed_body = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ]
      ~body:"Hi"
  in
  match Http1.Request.parse req with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got error " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for incomplete fixed body"

let test_request_preserves_pipelined_bytes_after_fixed_body = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ]
      ~body:"HelloGET /next HTTP/1.1\r\nHost: example.com\r\n\r\n"
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body =
        NetRequest.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected body Hello on parsed request"
      else if remaining != "GET /next HTTP/1.1\r\nHost: example.com\r\n\r\n" then
        Result.Error ("Expected pipelined request in remaining, got " ^ remaining)
      else
        Result.Ok ()

let test_request_without_content_length_preserves_remaining = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ]
      ~body:"GET /next HTTP/1.1\r\nHost: example.com\r\n\r\n"
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body = NetRequest.body parsed in
      if body != None then
        Result.Error "Expected request without Content-Length to have no body"
      else if remaining != "GET /next HTTP/1.1\r\nHost: example.com\r\n\r\n" then
        Result.Error ("Expected remaining bytes to be preserved, got " ^ remaining)
      else
        Result.Ok ()

let test_request_accepts_matching_duplicate_content_length = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ("Content-Length", "5"); ]
      ~body:"Hello"
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body =
        NetRequest.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected body Hello on parsed request"
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else
        Result.Ok ()

let test_request_rejects_conflicting_content_length = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ("Content-Length", "7"); ]
      ~body:"Hello"
  in
  match Http1.Request.parse req with
  | Error ConflictingContentLength -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected conflicting Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected conflicting Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected conflicting Content-Length error"

let test_request_rejects_invalid_content_length = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "nope"); ]
      ~body:"Hello"
  in
  match Http1.Request.parse req with
  | Error InvalidContentLength -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected invalid Content-Length error"

let test_request_rejects_transfer_encoding_with_content_length = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[
        ("Host", "example.com");
        ("Transfer-Encoding", "chunked");
        ("Content-Length", "5");
      ]
      ~body:"Hello"
  in
  match Http1.Request.parse req with
  | Error TransferEncodingWithContentLength -> Result.Ok ()
  | Error error -> Result.Error ("Expected TE+CL framing error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected TE+CL framing error, got Need_more"
  | Done _ -> Result.Error "Expected TE+CL framing error"

let test_request_rejects_unsupported_transfer_encoding = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "chunked"); ]
      ~body:"5\r\nHello\r\n0\r\n\r\n"
  in
  match Http1.Request.parse req with
  | Error UnsupportedTransferEncoding -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected unsupported Transfer-Encoding error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected unsupported Transfer-Encoding error, got Need_more"
  | Done _ -> Result.Error "Expected unsupported Transfer-Encoding error"

let test_request_line_limit_applies_before_crlf = fun _ctx ->
  let req = "GET /very-long-path" in
  match Http1.Request.parse ~max_request_line:8 req with
  | Error (RequestLineTooLong { max_length = 8 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected request line too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected request line too long error, got Need_more"
  | Done _ -> Result.Error "Expected request line too long error"

let test_header_line_limit_applies_before_crlf = fun _ctx ->
  let req = "GET / HTTP/1.1\r\nX-Long: abcdefghijklmnop" in
  match Http1.Request.parse ~max_header_length:8 req with
  | Error (HeaderTooLong { max_length = 8 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected header too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected header too long error, got Need_more"
  | Done _ -> Result.Error "Expected header too long error"

let test_request_parse_slice = fun _ctx ->
  let req = "GET /view HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match expect_request_parse_slice req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let method_ =
        NetRequest.method_ parsed
        |> NetMethod.to_string
      in
      let path =
        NetRequest.uri parsed
        |> Uri.path
      in
      if method_ != "GET" then
        Result.Error ("Expected GET method, got " ^ method_)
      else if path != "/view" then
        Result.Error ("Expected /view path, got " ^ path)
      else if remaining != "" then
        Result.Error ("Expected empty remaining body, got " ^ remaining)
      else
        Result.Ok ()

let test_request_rejects_missing_lf_after_request_line = fun _ctx ->
  let req = "GET /path HTTP/1.1\rHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Error InvalidCrlf -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid CRLF error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid CRLF error, got Need_more"
  | Done _ -> Result.Error "Expected invalid CRLF error"

let test_request_rejects_missing_lf_after_header_line = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\r\n" in
  match Http1.Request.parse req with
  | Error InvalidCrlf -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid CRLF error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid CRLF error, got Need_more"
  | Done _ -> Result.Error "Expected invalid CRLF error"

let test_request_rejects_invalid_http_version = fun _ctx ->
  let req = "GET /path HTTP/9.9\r\nHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Error InvalidHttpVersion -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid HTTP version error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid HTTP version error, got Need_more"
  | Done { value; _ } ->
      let version =
        NetRequest.version value
        |> NetVersion.to_string
      in
      Result.Error ("Expected invalid HTTP version error, got parsed " ^ version)

let test_request_rejects_invalid_request_target = fun _ctx ->
  let target = "/" ^ String.make ~len:65_535 ~char:'a' in
  let req = "GET " ^ target ^ " HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match Http1.Request.parse ~max_request_line:70_000 req with
  | Error (InvalidRequestTarget Std.Net.Uri.TooLong) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid request target error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid request target error, got Need_more"
  | Done _ -> Result.Error "Expected invalid request target error"

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
  | Error (TooManyHeaders { max_count = 100 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected too many headers error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected too many headers error"
  | Done _ -> Result.Error "Expected too many headers error"

(* HTTP/1 Response Tests *)

let test_response_200_ok = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let status =
        NetResponse.status parsed
        |> NetStatus.to_int
      in
      let version = NetResponse.version parsed in
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if status != 200 then
        Result.Error ("Expected status 200, got " ^ Int.to_string status)
      else if version != NetVersion.Http11 then
        Result.Error "Expected version HTTP/1.1"
      else if body != Some "Hello" then
        Result.Error "Expected response body on parsed response"
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_response_404 = fun _ctx ->
  let resp = "HTTP/1.1 404 Not Found\r\nContent-Type: text/plain\r\n\r\n" in
  match Http1.Response.parse resp with
  | Done { value = parsed; _ } ->
      let status =
        NetResponse.status parsed
        |> NetStatus.to_int
      in
      if status != 404 then
        Result.Error ("Expected status 404, got " ^ Int.to_string status)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_response_incomplete_fixed_body = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHi" in
  match Http1.Response.parse resp with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got error " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for incomplete fixed body"

let test_response_preserves_pipelined_bytes_after_fixed_body = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHelloHTTP/1.1 204 No Content\r\n\r\n" in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected body Hello on parsed response"
      else if remaining != "HTTP/1.1 204 No Content\r\n\r\n" then
        Result.Error ("Expected pipelined response in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_no_body_status_preserves_following_bytes = fun _ctx ->
  let resp =
    "HTTP/1.1 204 No Content\r\nContent-Length: 5\r\n\r\nHTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n"
  in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body = NetResponse.body parsed in
      if body != None then
        Result.Error "Expected 204 response to have no body"
      else if remaining != "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" then
        Result.Error ("Expected following response in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_without_content_length_uses_close_delimited_body = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected close-delimited response body"
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_rejects_conflicting_content_length = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Length: 7\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Error ConflictingContentLength -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected conflicting Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected conflicting Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected conflicting Content-Length error"

let test_response_rejects_invalid_content_length = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: nope\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Error InvalidContentLength -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected invalid Content-Length error"

let test_response_rejects_invalid_http_version = fun _ctx ->
  let resp = "HTTP/9.9 200 OK\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error InvalidHttpVersion -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid HTTP version error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid HTTP version error, got Need_more"
  | Done { value; _ } ->
      let version =
        NetResponse.version value
        |> NetVersion.to_string
      in
      Result.Error ("Expected invalid HTTP version error, got parsed " ^ version)

(* Chunked Encoding Tests *)

let test_chunk_single = fun _ctx ->
  let chunk = "5\r\nHello\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value = chunk_result; _ } ->
      if chunk_result.data != "Hello" then
        Result.Error "Expected data Hello"
      else if chunk_result.remaining != "" then
        Result.Error "Expected empty remaining"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_chunk_last = fun _ctx ->
  let chunk = "0\r\n\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value = chunk_result; _ } ->
      if chunk_result.data != "" then
        Result.Error "Expected empty data"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_chunk_hex_size = fun _ctx ->
  let chunk = "1a\r\nabcdefghijklmnopqrstuvwxyz\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value = chunk_result; _ } ->
      if String.length chunk_result.data != 26 then
        Result.Error ("Expected length 26, got " ^ Int.to_string (String.length chunk_result.data))
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error e -> Result.Error ("Parse error: " ^ error_to_string e)

let test_chunk_preserves_remaining_after_data_crlf = fun _ctx ->
  let chunk = "5\r\nHello\r\nnext" in
  match Http1.Chunk.parse chunk with
  | Done { value = chunk_result; _ } ->
      if chunk_result.data != "Hello" then
        Result.Error "Expected data Hello"
      else if chunk_result.remaining != "next" then
        Result.Error ("Expected remaining next, got " ^ chunk_result.remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_rejects_invalid_size_line_crlf = fun _ctx ->
  let chunk = "5\rHello\r\n" in
  match Http1.Chunk.parse chunk with
  | Error InvalidChunkSizeLineEnding -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid chunk size line ending, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid chunk size line ending, got Need_more"
  | Done _ -> Result.Error "Expected invalid chunk size line ending"

let test_chunk_rejects_invalid_data_crlf = fun _ctx ->
  let chunk = "5\r\nHello\rX" in
  match Http1.Chunk.parse chunk with
  | Error InvalidChunkDataLineEnding -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid chunk data line ending, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid chunk data line ending, got Need_more"
  | Done _ -> Result.Error "Expected invalid chunk data line ending"

let test_chunk_incomplete_data_crlf_needs_more = fun _ctx ->
  let chunk = "5\r\nHello\r" in
  match Http1.Chunk.parse chunk with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for partial chunk data CRLF"

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
    case "request incomplete fixed body" test_request_incomplete_fixed_body;
    case
      "request preserves pipelined bytes after fixed body"
      test_request_preserves_pipelined_bytes_after_fixed_body;
    case
      "request without content length preserves remaining"
      test_request_without_content_length_preserves_remaining;
    case
      "request accepts matching duplicate content length"
      test_request_accepts_matching_duplicate_content_length;
    case
      "request rejects conflicting content length"
      test_request_rejects_conflicting_content_length;
    case "request rejects invalid content length" test_request_rejects_invalid_content_length;
    case
      "request rejects transfer encoding with content length"
      test_request_rejects_transfer_encoding_with_content_length;
    case
      "request rejects unsupported transfer encoding"
      test_request_rejects_unsupported_transfer_encoding;
    case "request line limit applies before crlf" test_request_line_limit_applies_before_crlf;
    case "header line limit applies before crlf" test_header_line_limit_applies_before_crlf;
    case "request_parse_slice" test_request_parse_slice;
    case
      "request rejects missing lf after request line"
      test_request_rejects_missing_lf_after_request_line;
    case
      "request rejects missing lf after header line"
      test_request_rejects_missing_lf_after_header_line;
    case "request rejects invalid http version" test_request_rejects_invalid_http_version;
    case "request rejects invalid request target" test_request_rejects_invalid_request_target;
    case "request_rejects_too_many_headers" test_request_rejects_too_many_headers;
    case "response_200_ok" test_response_200_ok;
    case "response_404" test_response_404;
    case "response incomplete fixed body" test_response_incomplete_fixed_body;
    case
      "response preserves pipelined bytes after fixed body"
      test_response_preserves_pipelined_bytes_after_fixed_body;
    case
      "response no-body status preserves following bytes"
      test_response_no_body_status_preserves_following_bytes;
    case
      "response without content length uses close-delimited body"
      test_response_without_content_length_uses_close_delimited_body;
    case
      "response rejects conflicting content length"
      test_response_rejects_conflicting_content_length;
    case "response rejects invalid content length" test_response_rejects_invalid_content_length;
    case "response rejects invalid http version" test_response_rejects_invalid_http_version;
    case "chunk_single" test_chunk_single;
    case "chunk_last" test_chunk_last;
    case "chunk_hex_size" test_chunk_hex_size;
    case "chunk preserves remaining after data crlf" test_chunk_preserves_remaining_after_data_crlf;
    case "chunk rejects invalid size line crlf" test_chunk_rejects_invalid_size_line_crlf;
    case "chunk rejects invalid data crlf" test_chunk_rejects_invalid_data_crlf;
    case "chunk incomplete data crlf needs more" test_chunk_incomplete_data_crlf_needs_more;
    case "sse_data_line" test_sse_data_line;
    case "sse_event_type" test_sse_event_type;
    case "sse_empty_line" test_sse_empty_line;
    case "sse_comment" test_sse_comment;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http1_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
