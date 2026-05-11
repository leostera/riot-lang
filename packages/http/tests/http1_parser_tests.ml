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

let test_common_slice_creation_errors_are_typed = fun _ctx ->
  match Http1.Common.slice_of_string ~off:(-1) "GET" with
  | Error (Http1.Common.InputSliceCreationFailed _) -> Result.Ok ()
  | Error error -> Result.Error ("Wrong slice creation error: " ^ error_to_string error)
  | Ok _ -> Result.Error "Negative slice offset was accepted"

let test_content_length_errors_are_typed = fun _ctx ->
  let expect input check =
    match Http1.Common.parse_content_length_value input with
    | Error error when check error -> Result.Ok ()
    | Error error ->
        Result.Error ("Wrong Content-Length error: "
        ^ Http1.Common.error_to_string (InvalidContentLength error))
    | Ok value -> Result.Error ("Content-Length parsed unexpectedly as " ^ Int.to_string value)
  in
  match expect
    ""
    (fun __tmp1 ->
      match __tmp1 with
      | EmptyContentLength -> true
      | _ -> false) with
  | Error _ as error -> error
  | Ok () ->
      match expect
        "-1"
        (fun __tmp1 ->
          match __tmp1 with
          | NegativeContentLength -> true
          | _ -> false) with
      | Error _ as error -> error
      | Ok () ->
          match expect
            "12x"
            (fun __tmp1 ->
              match __tmp1 with
              | InvalidContentLengthCharacter { code; index = 2 } when code = Char.to_int 'x' -> true
              | _ -> false) with
          | Error _ as error -> error
          | Ok () ->
              expect
                (String.make ~len:32 ~char:'9')
                (fun __tmp1 ->
                  match __tmp1 with
                  | ContentLengthOverflow -> true
                  | _ -> false)

(* HTTP/1 Request Tests *)

let test_request_simple_get = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Done { value = parsed; _ } ->
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
        (
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

let test_request_rejects_fixed_body_over_limit = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ]
      ~body:"Hello"
  in
  match Http1.Request.parse ~max_body_size:4 req with
  | Error (BodyTooLarge { size = 5; max_size = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected body too large error, got Need_more"
  | Done _ -> Result.Error "Expected body too large error"

let test_request_rejects_conflicting_content_length = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Content-Length", "5"); ("Content-Length", "7"); ]
      ~body:"Hello"
  in
  match Http1.Request.parse req with
  | Error (ConflictingContentLength { expected = 5; actual = 7 }) -> Result.Ok ()
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
  | Error (InvalidContentLength (
    InvalidContentLengthCharacter { code; index = 0 }
  )) when code = Char.to_int 'n' ->
      Result.Ok ()
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
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "gzip"); ]
      ~body:"Hello"
  in
  match Http1.Request.parse req with
  | Error UnsupportedTransferEncoding -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected unsupported Transfer-Encoding error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected unsupported Transfer-Encoding error, got Need_more"
  | Done _ -> Result.Error "Expected unsupported Transfer-Encoding error"

let test_request_parses_chunked_body = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "chunked"); ]
      ~body:"5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n"
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body =
        NetRequest.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello world" then
        Result.Error "Expected decoded chunked request body"
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else
        Result.Ok ()

let test_request_parses_chunked_body_with_trailers_and_remaining = fun _ctx ->
  let next = "GET /next HTTP/1.1\r\nHost: example.com\r\n\r\n" in
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "chunked"); ]
      ~body:("5\r\nHello\r\n0\r\nETag: abc\r\n\r\n" ^ next)
  in
  match expect_request_parse req with
  | Error error -> Result.Error error
  | Ok (parsed, remaining) ->
      let body =
        NetRequest.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected decoded chunked request body"
      else if remaining != next then
        Result.Error ("Expected pipelined request in remaining, got " ^ remaining)
      else
        Result.Ok ()

let test_request_incomplete_chunked_body_needs_more = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "chunked"); ]
      ~body:"5\r\nHello\r\n0\r\n"
  in
  match Http1.Request.parse req with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for incomplete chunked request body"

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

let test_request_header_block_limit = fun _ctx ->
  let req = "GET / HTTP/1.1\r\nA: 123\r\nB: 456\r\n\r\n" in
  match Http1.Request.parse ~max_header_block_length:12 req with
  | Error (HeaderBlockTooLong { max_length = 12 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected header block too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected header block too long error, got Need_more"
  | Done _ -> Result.Error "Expected header block too long error"

let test_request_header_block_limit_applies_before_crlf = fun _ctx ->
  let req = "GET / HTTP/1.1\r\nA: 123\r\nB: 456" in
  match Http1.Request.parse ~max_header_block_length:12 req with
  | Error (HeaderBlockTooLong { max_length = 12 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected header block too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected header block too long error, got Need_more"
  | Done _ -> Result.Error "Expected header block too long error"

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

let test_request_parse_head_preserves_body_bytes = fun _ctx ->
  let req = "POST /upload HTTP/1.1\r\nHost: example.com\r\nContent-Length: 5\r\n\r\nHi" in
  match Http1.Request.parse_head req with
  | Done { value = parsed; remaining } ->
      let body = NetRequest.body parsed in
      let path =
        NetRequest.uri parsed
        |> Uri.path
      in
      if path != "/upload" then
        Result.Error ("Expected /upload path, got " ^ path)
      else if body != None then
        Result.Error "Expected head parser to leave request body unset"
      else if remaining != "Hi" then
        Result.Error ("Expected partial body in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

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

let test_request_rejects_empty_header_name = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\n: value\r\n\r\n" in
  match Http1.Request.parse req with
  | Error (InvalidHeaderFormat EmptyName) -> Result.Ok ()
  | Error error -> Result.Error ("Expected empty header name error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected empty header name error, got Need_more"
  | Done _ -> Result.Error "Expected empty header name error"

let test_request_rejects_whitespace_before_colon = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost : example.com\r\n\r\n" in
  match Http1.Request.parse req with
  | Error (InvalidHeaderFormat WhitespaceBeforeColon) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected whitespace-before-colon error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected whitespace-before-colon error, got Need_more"
  | Done _ -> Result.Error "Expected whitespace-before-colon error"

let test_request_rejects_obsolete_line_folding = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nHost: example.com\r\n folded\r\n\r\n" in
  match Http1.Request.parse req with
  | Error (InvalidHeaderFormat ObsoleteLineFolding) -> Result.Ok ()
  | Error error -> Result.Error ("Expected obs-fold error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected obs-fold error, got Need_more"
  | Done _ -> Result.Error "Expected obs-fold error"

let test_request_rejects_invalid_header_name_character = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nBad@Name: value\r\n\r\n" in
  match Http1.Request.parse req with
  | Error (InvalidHeaderFormat (InvalidNameCharacter { code; index })) when code = 64 && index = 3 ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid header name character error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid header name character error, got Need_more"
  | Done _ -> Result.Error "Expected invalid header name character error"

let test_request_rejects_invalid_header_value_character = fun _ctx ->
  let req = "GET /path HTTP/1.1\r\nX-Test: value\nbad\r\n\r\n" in
  match Http1.Request.parse req with
  | Error (InvalidHeaderFormat (InvalidValueCharacter { code; index })) when code = 10 && index = 5 ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid header value character error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid header value character error, got Need_more"
  | Done _ -> Result.Error "Expected invalid header value character error"

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

let test_request_accepts_exact_max_headers = fun _ctx ->
  let req =
    build_request
      ~method_:"GET"
      ~path:"/"
      ~headers:[ ("Host", "example.com"); ("X-Test", "ok"); ]
      ~body:""
  in
  match Http1.Request.parse ~max_headers:2 req with
  | Done { value = parsed; remaining = "" } -> (
      match NetRequest.get_header parsed "x-test" with
      | Some "ok" -> Result.Ok ()
      | Some value -> Result.Error ("Expected X-Test: ok, got " ^ value)
      | None -> Result.Error "Expected X-Test header"
    )
  | Done _ -> Result.Error "Expected empty remaining bytes"
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

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

let test_response_parse_head_preserves_body_bytes = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHi" in
  match Http1.Response.parse_head resp with
  | Done { value = parsed; remaining } ->
      let body = NetResponse.body parsed in
      let status =
        NetResponse.status parsed
        |> NetStatus.to_int
      in
      if status != 200 then
        Result.Error ("Expected status 200, got " ^ Int.to_string status)
      else if body != None then
        Result.Error "Expected head parser to leave response body unset"
      else if remaining != "Hi" then
        Result.Error ("Expected partial body in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

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

let test_response_parses_chunked_body = fun _ctx ->
  let resp =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n"
  in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello world" then
        Result.Error "Expected decoded chunked response body"
      else if remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_parses_chunked_body_with_trailers_and_remaining = fun _ctx ->
  let next = "HTTP/1.1 204 No Content\r\n\r\n" in
  let resp =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n0\r\nETag: abc\r\n\r\n"
    ^ next
  in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected decoded chunked response body"
      else if remaining != next then
        Result.Error ("Expected pipelined response in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_incomplete_chunked_body_needs_more = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n0\r\n" in
  match Http1.Response.parse resp with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for incomplete chunked response body"

let test_response_rejects_unsupported_transfer_encoding = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nTransfer-Encoding: gzip\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Error UnsupportedTransferEncoding -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected unsupported Transfer-Encoding error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected unsupported Transfer-Encoding error, got Need_more"
  | Done _ -> Result.Error "Expected unsupported Transfer-Encoding error"

let test_response_rejects_conflicting_content_length = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Length: 7\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Error (ConflictingContentLength { expected = 5; actual = 7 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected conflicting Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected conflicting Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected conflicting Content-Length error"

let test_response_rejects_fixed_body_over_limit = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\n\r\nHello" in
  match Http1.Response.parse ~max_body_size:4 resp with
  | Error (BodyTooLarge { size = 5; max_size = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected body too large error, got Need_more"
  | Done _ -> Result.Error "Expected body too large error"

let test_response_rejects_close_delimited_body_over_limit = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\nHello" in
  match Http1.Response.parse ~max_body_size:4 resp with
  | Error (BodyTooLarge { size = 5; max_size = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected body too large error, got Need_more"
  | Done _ -> Result.Error "Expected body too large error"

let test_response_rejects_invalid_content_length = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: nope\r\n\r\nHello" in
  match Http1.Response.parse resp with
  | Error (InvalidContentLength (
    InvalidContentLengthCharacter { code; index = 0 }
  )) when code = Char.to_int 'n' ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid Content-Length error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected invalid Content-Length error"

let test_response_accepts_matching_duplicate_content_length = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 5\r\nContent-Length: 5\r\n\r\nHellonext" in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body =
        NetResponse.body parsed
        |> Option.map ~fn:NetBody.to_string
      in
      if body != Some "Hello" then
        Result.Error "Expected body Hello on parsed response"
      else if remaining != "next" then
        Result.Error ("Expected remaining next, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_rejects_transfer_encoding_with_content_length = fun _ctx ->
  let resp =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\nContent-Length: 5\r\n\r\n0\r\n\r\n"
  in
  match Http1.Response.parse resp with
  | Error TransferEncodingWithContentLength -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected Transfer-Encoding with Content-Length error, got "
      ^ error_to_string error)
  | Need_more -> Result.Error "Expected Transfer-Encoding with Content-Length error, got Need_more"
  | Done _ -> Result.Error "Expected Transfer-Encoding with Content-Length error"

let test_response_no_body_informational_preserves_following_bytes = fun _ctx ->
  let next = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
  let resp = "HTTP/1.1 103 Early Hints\r\nTransfer-Encoding: chunked\r\n\r\n" ^ next in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body = NetResponse.body parsed in
      let status =
        NetResponse.status parsed
        |> NetStatus.to_int
      in
      if status != 103 then
        Result.Error ("Expected status 103, got " ^ Int.to_string status)
      else if body != None then
        Result.Error "Expected informational response to have no body"
      else if remaining != next then
        Result.Error ("Expected following response in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_response_not_modified_preserves_following_bytes = fun _ctx ->
  let next = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
  let resp = "HTTP/1.1 304 Not Modified\r\nContent-Length: 8\r\n\r\n" ^ next in
  match Http1.Response.parse resp with
  | Done { value = parsed; remaining } ->
      let body = NetResponse.body parsed in
      let status =
        NetResponse.status parsed
        |> NetStatus.to_int
      in
      if status != 304 then
        Result.Error ("Expected status 304, got " ^ Int.to_string status)
      else if body != None then
        Result.Error "Expected 304 response to have no body"
      else if remaining != next then
        Result.Error ("Expected following response in remaining, got " ^ remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

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

let test_response_rejects_short_status_code = fun _ctx ->
  let resp = "HTTP/1.1 99 Weird\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error (InvalidStatusCode (StatusCodeLength { length = 2; expected = 3 })) -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid status code error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid status code error, got Need_more"
  | Done _ -> Result.Error "Expected invalid status code error"

let test_response_rejects_status_code_below_100 = fun _ctx ->
  let resp = "HTTP/1.1 099 Weird\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error (InvalidStatusCode (
    StatusCodeOutOfRange { code = 99; min = 100; max = 999 }
  )) ->
      Result.Ok ()
  | Error error -> Result.Error ("Expected invalid status code error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid status code error, got Need_more"
  | Done _ -> Result.Error "Expected invalid status code error"

let test_response_rejects_long_status_code = fun _ctx ->
  let resp = "HTTP/1.1 1000 Weird\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error (InvalidStatusCode (StatusCodeLength { length = 4; expected = 3 })) -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid status code error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid status code error, got Need_more"
  | Done _ -> Result.Error "Expected invalid status code error"

let test_response_rejects_status_code_character = fun _ctx ->
  let resp = "HTTP/1.1 2xx Weird\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error (InvalidStatusCode (InvalidStatusCodeCharacter { code; index = 1 })) when code
  = Char.to_int 'x' -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid status code error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid status code error, got Need_more"
  | Done _ -> Result.Error "Expected invalid status code error"

let test_response_rejects_invalid_status_line_crlf = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\rContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error InvalidCrlf -> Result.Ok ()
  | Error error -> Result.Error ("Expected invalid CRLF error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid CRLF error, got Need_more"
  | Done _ -> Result.Error "Expected invalid CRLF error"

let test_response_status_line_limit = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nContent-Length: 0\r\n\r\n" in
  match Http1.Response.parse ~max_status_line:8 resp with
  | Error (StatusLineTooLong { max_length = 8 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected status line too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected status line too long error, got Need_more"
  | Done _ -> Result.Error "Expected status line too long error"

let test_response_status_line_limit_applies_before_crlf = fun _ctx ->
  let resp = "HTTP/1.1 200" in
  match Http1.Response.parse ~max_status_line:8 resp with
  | Error (StatusLineTooLong { max_length = 8 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected status line too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected status line too long error, got Need_more"
  | Done _ -> Result.Error "Expected status line too long error"

let test_response_header_line_limit_applies_before_crlf = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nX-Long: value" in
  match Http1.Response.parse ~max_header_length:8 resp with
  | Error (HeaderTooLong { max_length = 8 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected header too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected header too long error, got Need_more"
  | Done _ -> Result.Error "Expected header too long error"

let test_response_rejects_invalid_header_name_character = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nBad@Name: value\r\n\r\n" in
  match Http1.Response.parse resp with
  | Error (InvalidHeaderFormat (InvalidNameCharacter { code; index })) when code = 64 && index = 3 ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid header name character error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid header name character error, got Need_more"
  | Done _ -> Result.Error "Expected invalid header name character error"

let test_response_header_block_limit = fun _ctx ->
  let resp = "HTTP/1.1 200 OK\r\nA: 123\r\nB: 456\r\n\r\n" in
  match Http1.Response.parse ~max_header_block_length:12 resp with
  | Error (HeaderBlockTooLong { max_length = 12 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected header block too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected header block too long error, got Need_more"
  | Done _ -> Result.Error "Expected header block too long error"

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

let test_chunk_rejects_empty_size = fun _ctx ->
  match Http1.Chunk.parse "\r\n" with
  | Error (InvalidChunkSize EmptyChunkSize) -> Result.Ok ()
  | Error error -> Result.Error ("Expected empty chunk size error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected empty chunk size error, got Need_more"
  | Done _ -> Result.Error "Expected empty chunk size error"

let test_chunk_rejects_invalid_size_character = fun _ctx ->
  match Http1.Chunk.parse "1z\r\nhello\r\n" with
  | Error (InvalidChunkSize (InvalidChunkSizeCharacter { code; index = 1 })) when code
  = Char.to_int 'z' -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid chunk size character, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid chunk size character, got Need_more"
  | Done _ -> Result.Error "Expected invalid chunk size character"

let test_chunk_rejects_size_overflow = fun _ctx ->
  let chunk = String.make ~len:32 ~char:'f' ^ "\r\nhello\r\n" in
  match Http1.Chunk.parse chunk with
  | Error (InvalidChunkSize ChunkSizeOverflow) -> Result.Ok ()
  | Error error -> Result.Error ("Expected chunk size overflow, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunk size overflow, got Need_more"
  | Done _ -> Result.Error "Expected chunk size overflow"

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

let test_chunk_rejects_overlong_size_line = fun _ctx ->
  let chunk = "12345\r\nHello\r\n" in
  match Http1.Chunk.parse ~max_chunk_size_line:4 chunk with
  | Error (ChunkSizeLineTooLong { max_length = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected chunk size line too long, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunk size line too long, got Need_more"
  | Done _ -> Result.Error "Expected chunk size line too long"

let test_chunk_size_line_limit_applies_before_crlf = fun _ctx ->
  match Http1.Chunk.parse ~max_chunk_size_line:4 "12345" with
  | Error (ChunkSizeLineTooLong { max_length = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected chunk size line too long, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunk size line too long, got Need_more"
  | Done _ -> Result.Error "Expected chunk size line too long"

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

let test_chunk_accepts_extensions = fun _ctx ->
  let chunk = "5;foo=bar; flag\r\nHello\r\n" in
  match Http1.Chunk.parse chunk with
  | Done { value = chunk_result; _ } ->
      if chunk_result.data != "Hello" then
        Result.Error "Expected data Hello"
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_rejects_invalid_extension_character = fun _ctx ->
  let chunk = "5;bad\next\r\nHello\r\n" in
  match Http1.Chunk.parse chunk with
  | Error (InvalidChunkExtensionCharacter { code; index }) when code = 10 && index = 5 ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid chunk extension character, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid chunk extension character, got Need_more"
  | Done _ -> Result.Error "Expected invalid chunk extension character"

let test_chunk_decode_multiple_chunks = fun _ctx ->
  let body = "5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n" in
  match Http1.Chunk.decode body with
  | Done { value = decoded; _ } ->
      if decoded.body != "Hello world" then
        Result.Error ("Expected decoded body Hello world, got " ^ decoded.body)
      else if decoded.trailers != [] then
        Result.Error "Expected no trailers"
      else if decoded.remaining != "" then
        Result.Error ("Expected empty remaining, got " ^ decoded.remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_decode_trailers_and_remaining = fun _ctx ->
  let body = "5;foo=bar\r\nHello\r\n0\r\nETag: abc\r\nX-Trace: 42\r\n\r\nnext" in
  match Http1.Chunk.decode body with
  | Done { value = decoded; _ } ->
      if decoded.body != "Hello" then
        Result.Error ("Expected decoded body Hello, got " ^ decoded.body)
      else if decoded.trailers != [ ("ETag", "abc"); ("X-Trace", "42"); ] then
        Result.Error "Expected decoded trailers"
      else if decoded.remaining != "next" then
        Result.Error ("Expected remaining next, got " ^ decoded.remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_decode_rejects_chunk_over_limit = fun _ctx ->
  match Http1.Chunk.decode ~max_chunk_size:4 "5\r\nHello\r\n0\r\n\r\n" with
  | Error (ChunkTooLarge { size = 5; max_size = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected chunk too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunk too large error, got Need_more"
  | Done _ -> Result.Error "Expected chunk too large error"

let test_chunk_decode_rejects_body_over_limit = fun _ctx ->
  match Http1.Chunk.decode ~max_body_size:10 "5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n" with
  | Error (ChunkedBodyTooLarge { size = 11; max_size = 10 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected chunked body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunked body too large error, got Need_more"
  | Done _ -> Result.Error "Expected chunked body too large error"

let test_request_rejects_chunked_body_over_limit = fun _ctx ->
  let req =
    build_request
      ~method_:"POST"
      ~path:"/upload"
      ~headers:[ ("Host", "example.com"); ("Transfer-Encoding", "chunked"); ]
      ~body:"5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n"
  in
  match Http1.Request.parse ~max_body_size:10 req with
  | Error (ChunkedBodyTooLarge { size = 11; max_size = 10 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected chunked body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunked body too large error, got Need_more"
  | Done _ -> Result.Error "Expected chunked body too large error"

let test_response_rejects_chunked_body_over_limit = fun _ctx ->
  let resp =
    "HTTP/1.1 200 OK\r\nTransfer-Encoding: chunked\r\n\r\n5\r\nHello\r\n6\r\n world\r\n0\r\n\r\n"
  in
  match Http1.Response.parse ~max_body_size:10 resp with
  | Error (ChunkedBodyTooLarge { size = 11; max_size = 10 }) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected chunked body too large error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected chunked body too large error, got Need_more"
  | Done _ -> Result.Error "Expected chunked body too large error"

let test_chunk_decode_rejects_invalid_trailer_name = fun _ctx ->
  let body = "0\r\nBad@Name: value\r\n\r\n" in
  match Http1.Chunk.decode body with
  | Error (InvalidHeaderFormat (InvalidNameCharacter { code; index })) when code = 64 && index = 3 ->
      Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid trailer name character, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid trailer name character, got Need_more"
  | Done _ -> Result.Error "Expected invalid trailer name character"

let test_chunk_decode_incomplete_trailer_needs_more = fun _ctx ->
  match Http1.Chunk.decode "0\r\nETag: abc\r\n" with
  | Need_more -> Result.Ok ()
  | Error error -> Result.Error ("Expected Need_more, got " ^ error_to_string error)
  | Done _ -> Result.Error "Expected Need_more for incomplete trailers"

let test_chunk_decode_allows_empty_trailers_at_zero_limit = fun _ctx ->
  match Http1.Chunk.decode ~max_trailers:0 "0\r\n\r\nnext" with
  | Done { value = decoded; _ } ->
      if decoded.body != "" then
        Result.Error "Expected empty decoded body"
      else if decoded.trailers != [] then
        Result.Error "Expected no trailers"
      else if decoded.remaining != "next" then
        Result.Error ("Expected remaining next, got " ^ decoded.remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_decode_accepts_exact_max_trailers = fun _ctx ->
  match Http1.Chunk.decode ~max_trailers:1 "0\r\nETag: abc\r\n\r\nnext" with
  | Done { value = decoded; _ } ->
      if decoded.trailers != [ ("ETag", "abc"); ] then
        Result.Error "Expected one decoded trailer"
      else if decoded.remaining != "next" then
        Result.Error ("Expected remaining next, got " ^ decoded.remaining)
      else
        Result.Ok ()
  | Need_more -> Result.Error "Unexpected Need_more"
  | Error error -> Result.Error ("Parse error: " ^ error_to_string error)

let test_chunk_decode_rejects_too_many_trailers = fun _ctx ->
  match Http1.Chunk.decode ~max_trailers:1 "0\r\nETag: abc\r\nX-Trace: 42\r\n\r\n" with
  | Error (TooManyHeaders { max_count = 1 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected too many trailers error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected too many trailers error, got Need_more"
  | Done _ -> Result.Error "Expected too many trailers error"

let test_chunk_decode_trailer_length_limit_applies_before_crlf = fun _ctx ->
  match Http1.Chunk.decode ~max_trailer_length:4 "0\r\nX-Long" with
  | Error (HeaderTooLong { max_length = 4 }) -> Result.Ok ()
  | Error error -> Result.Error ("Expected trailer too long error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected trailer too long error, got Need_more"
  | Done _ -> Result.Error "Expected trailer too long error"

let test_chunk_decode_rejects_invalid_trailer_value = fun _ctx ->
  match Http1.Chunk.decode "0\r\nX-Test: \001\r\n\r\n" with
  | Error (InvalidHeaderFormat (InvalidValueCharacter { code = 1; index = 0 })) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected invalid trailer value error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected invalid trailer value error, got Need_more"
  | Done _ -> Result.Error "Expected invalid trailer value error"

let test_chunk_decode_rejects_obsolete_trailer_folding = fun _ctx ->
  match Http1.Chunk.decode "0\r\n Folded: nope\r\n\r\n" with
  | Error (InvalidHeaderFormat ObsoleteLineFolding) -> Result.Ok ()
  | Error error ->
      Result.Error ("Expected obsolete trailer folding error, got " ^ error_to_string error)
  | Need_more -> Result.Error "Expected obsolete trailer folding error, got Need_more"
  | Done _ -> Result.Error "Expected obsolete trailer folding error"

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

let test_sse_line_does_not_skip_leading_whitespace = fun _ctx ->
  match Http1.Sse.parse_line " data: hidden" with
  | None -> Result.Ok ()
  | Some _ -> Result.Error "Leading whitespace should make the SSE field unknown"

let test_sse_parse_multiline_event = fun _ctx ->
  let input = "event: message\r\ndata: hello\r\ndata: world\r\nid: 42\r\nretry: 1000\r\n\r\n" in
  match Http1.Sse.parse input with
  | [
      {
        data = "hello\nworld";
        event_type = Some "message";
        id = Some "42";
        retry = Some 1_000;
      };
    ] -> Result.Ok ()
  | _ -> Result.Error "SSE parser did not accumulate a multiline event"

let test_sse_parse_ignores_comments_and_invalid_retry = fun _ctx ->
  let input = ": ignored\r\nretry: nope\r\ndata: ok\r\n\r\n" in
  match Http1.Sse.parse input with
  | [
      {
        data = "ok";
        event_type = None;
        id = None;
        retry = None;
      };
    ] -> Result.Ok ()
  | _ -> Result.Error "SSE parser did not ignore comments or invalid retry"

let test_sse_parse_dispatches_trailing_event = fun _ctx ->
  match Http1.Sse.parse "data: final" with
  | [
      {
        data = "final";
        event_type = None;
        id = None;
        retry = None;
      };
    ] -> Result.Ok ()
  | _ -> Result.Error "SSE parser did not dispatch the trailing event"

let test_sse_parse_does_not_dispatch_event_without_data = fun _ctx ->
  match Http1.Sse.parse "event: ping\r\nid: 42\r\nretry: 1000\r\n\r\n" with
  | [] -> Result.Ok ()
  | _ -> Result.Error "SSE parser dispatched an event with no data lines"

let test_sse_parse_invalid_utf8_bytes = fun _ctx ->
  match Http1.Sse.parse "\x2b\xf4\x3a" with
  | [] -> Result.Ok ()
  | _ -> Result.Error "SSE parser dispatched invalid UTF-8 bytes as an event"

let tests =
  Test.[
    case "common slice creation errors are typed" test_common_slice_creation_errors_are_typed;
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
    case "request rejects fixed body over limit" test_request_rejects_fixed_body_over_limit;
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
    case "request parses chunked body" test_request_parses_chunked_body;
    case
      "request parses chunked body with trailers and remaining"
      test_request_parses_chunked_body_with_trailers_and_remaining;
    case
      "request incomplete chunked body needs more"
      test_request_incomplete_chunked_body_needs_more;
    case "request line limit applies before crlf" test_request_line_limit_applies_before_crlf;
    case "header line limit applies before crlf" test_header_line_limit_applies_before_crlf;
    case "request header block limit" test_request_header_block_limit;
    case
      "request header block limit applies before crlf"
      test_request_header_block_limit_applies_before_crlf;
    case "request_parse_slice" test_request_parse_slice;
    case "request parse head preserves body bytes" test_request_parse_head_preserves_body_bytes;
    case
      "request rejects missing lf after request line"
      test_request_rejects_missing_lf_after_request_line;
    case
      "request rejects missing lf after header line"
      test_request_rejects_missing_lf_after_header_line;
    case "request rejects empty header name" test_request_rejects_empty_header_name;
    case "request rejects whitespace before colon" test_request_rejects_whitespace_before_colon;
    case "request rejects obsolete line folding" test_request_rejects_obsolete_line_folding;
    case
      "request rejects invalid header name character"
      test_request_rejects_invalid_header_name_character;
    case
      "request rejects invalid header value character"
      test_request_rejects_invalid_header_value_character;
    case "request rejects invalid http version" test_request_rejects_invalid_http_version;
    case "request rejects invalid request target" test_request_rejects_invalid_request_target;
    case "request accepts exact max headers" test_request_accepts_exact_max_headers;
    case "request_rejects_too_many_headers" test_request_rejects_too_many_headers;
    case "response_200_ok" test_response_200_ok;
    case "response_404" test_response_404;
    case "response incomplete fixed body" test_response_incomplete_fixed_body;
    case "response parse head preserves body bytes" test_response_parse_head_preserves_body_bytes;
    case
      "response preserves pipelined bytes after fixed body"
      test_response_preserves_pipelined_bytes_after_fixed_body;
    case
      "response no-body status preserves following bytes"
      test_response_no_body_status_preserves_following_bytes;
    case
      "response without content length uses close-delimited body"
      test_response_without_content_length_uses_close_delimited_body;
    case "response parses chunked body" test_response_parses_chunked_body;
    case
      "response parses chunked body with trailers and remaining"
      test_response_parses_chunked_body_with_trailers_and_remaining;
    case
      "response incomplete chunked body needs more"
      test_response_incomplete_chunked_body_needs_more;
    case
      "response rejects unsupported transfer encoding"
      test_response_rejects_unsupported_transfer_encoding;
    case
      "response rejects conflicting content length"
      test_response_rejects_conflicting_content_length;
    case "response rejects fixed body over limit" test_response_rejects_fixed_body_over_limit;
    case
      "response rejects close-delimited body over limit"
      test_response_rejects_close_delimited_body_over_limit;
    case "response rejects invalid content length" test_response_rejects_invalid_content_length;
    case
      "response accepts matching duplicate content length"
      test_response_accepts_matching_duplicate_content_length;
    case
      "response rejects transfer encoding with content length"
      test_response_rejects_transfer_encoding_with_content_length;
    case
      "response no-body informational preserves following bytes"
      test_response_no_body_informational_preserves_following_bytes;
    case
      "response not modified preserves following bytes"
      test_response_not_modified_preserves_following_bytes;
    case "response rejects invalid http version" test_response_rejects_invalid_http_version;
    case "response rejects short status code" test_response_rejects_short_status_code;
    case "response rejects status code below 100" test_response_rejects_status_code_below_100;
    case "response rejects long status code" test_response_rejects_long_status_code;
    case "response rejects status code character" test_response_rejects_status_code_character;
    case "response rejects invalid status line crlf" test_response_rejects_invalid_status_line_crlf;
    case "response status line limit" test_response_status_line_limit;
    case
      "response status line limit applies before crlf"
      test_response_status_line_limit_applies_before_crlf;
    case
      "response header line limit applies before crlf"
      test_response_header_line_limit_applies_before_crlf;
    case
      "response rejects invalid header name character"
      test_response_rejects_invalid_header_name_character;
    case "response header block limit" test_response_header_block_limit;
    case "chunk_single" test_chunk_single;
    case "chunk_last" test_chunk_last;
    case "chunk_hex_size" test_chunk_hex_size;
    case "chunk rejects empty size" test_chunk_rejects_empty_size;
    case "chunk rejects invalid size character" test_chunk_rejects_invalid_size_character;
    case "chunk rejects size overflow" test_chunk_rejects_size_overflow;
    case "chunk preserves remaining after data crlf" test_chunk_preserves_remaining_after_data_crlf;
    case "chunk rejects invalid size line crlf" test_chunk_rejects_invalid_size_line_crlf;
    case "chunk rejects overlong size line" test_chunk_rejects_overlong_size_line;
    case "chunk size line limit applies before crlf" test_chunk_size_line_limit_applies_before_crlf;
    case "chunk rejects invalid data crlf" test_chunk_rejects_invalid_data_crlf;
    case "chunk incomplete data crlf needs more" test_chunk_incomplete_data_crlf_needs_more;
    case "chunk accepts extensions" test_chunk_accepts_extensions;
    case "chunk rejects invalid extension character" test_chunk_rejects_invalid_extension_character;
    case "chunk decode multiple chunks" test_chunk_decode_multiple_chunks;
    case "chunk decode trailers and remaining" test_chunk_decode_trailers_and_remaining;
    case "chunk decode rejects chunk over limit" test_chunk_decode_rejects_chunk_over_limit;
    case "chunk decode rejects body over limit" test_chunk_decode_rejects_body_over_limit;
    case "request rejects chunked body over limit" test_request_rejects_chunked_body_over_limit;
    case "response rejects chunked body over limit" test_response_rejects_chunked_body_over_limit;
    case "chunk decode rejects invalid trailer name" test_chunk_decode_rejects_invalid_trailer_name;
    case
      "chunk decode incomplete trailer needs more"
      test_chunk_decode_incomplete_trailer_needs_more;
    case
      "chunk decode allows empty trailers at zero limit"
      test_chunk_decode_allows_empty_trailers_at_zero_limit;
    case "chunk decode accepts exact max trailers" test_chunk_decode_accepts_exact_max_trailers;
    case "chunk decode rejects too many trailers" test_chunk_decode_rejects_too_many_trailers;
    case
      "chunk decode trailer length limit applies before crlf"
      test_chunk_decode_trailer_length_limit_applies_before_crlf;
    case
      "chunk decode rejects invalid trailer value"
      test_chunk_decode_rejects_invalid_trailer_value;
    case
      "chunk decode rejects obsolete trailer folding"
      test_chunk_decode_rejects_obsolete_trailer_folding;
    case "sse_data_line" test_sse_data_line;
    case "sse_event_type" test_sse_event_type;
    case "sse_empty_line" test_sse_empty_line;
    case "sse_comment" test_sse_comment;
    case "sse line does not skip leading whitespace" test_sse_line_does_not_skip_leading_whitespace;
    case "sse parse multiline event" test_sse_parse_multiline_event;
    case
      "sse parse ignores comments and invalid retry"
      test_sse_parse_ignores_comments_and_invalid_retry;
    case "sse parse dispatches trailing event" test_sse_parse_dispatches_trailing_event;
    case
      "sse parse does not dispatch event without data"
      test_sse_parse_does_not_dispatch_event_without_data;
    case "sse parse invalid utf8 bytes" test_sse_parse_invalid_utf8_bytes;
  ]

let main ~args:_ = Test.Cli.main ~name:"http:http1_parser" ~tests ~args:Env.args ()

let () = Runtime.run ~main ~args:Env.args ()
