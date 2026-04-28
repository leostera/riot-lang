open Std

module Http2 = Suri.Testing.Internal.Http2

let hpack_header = fun name value -> { Http.Http2.Hpack.name; value }

let required_headers = [
  hpack_header ":method" "GET";
  hpack_header ":scheme" "https";
  hpack_header ":path" "/hello?name=suri";
  hpack_header "accept" "text/plain";
]

let test_http2_headers_to_request_uses_required_pseudo_headers = fun _ctx ->
  match Http2.headers_to_request required_headers "body" with
  | Error error -> Error (Http2.request_header_error_to_string error)
  | Ok req ->
      Test.assert_equal ~expected:Net.Http.Method.Get ~actual:(Suri.Request.method_ req);
      Test.assert_equal ~expected:"/hello?name=suri" ~actual:(Suri.Request.uri req);
      Test.assert_equal ~expected:"body" ~actual:(Suri.Request.body req);
      Test.assert_equal
        ~expected:(Some "text/plain")
        ~actual:(Net.Http.Header.get (Suri.Request.headers req) "accept");
      Ok ()

let test_http2_headers_to_request_rejects_missing_method = fun _ctx ->
  let headers = [ hpack_header ":scheme" "https"; hpack_header ":path" "/"; ] in
  match Http2.headers_to_request headers "" with
  | Error (Http2.MissingPseudoHeader Http2.Method) -> Ok ()
  | Ok _ -> Error "expected missing :method to be rejected"
  | Error error -> Error (Http2.request_header_error_to_string error)

let test_http2_headers_to_request_rejects_empty_scheme = fun _ctx ->
  let headers = [
    hpack_header ":method" "GET";
    hpack_header ":scheme" "";
    hpack_header ":path" "/";
  ]
  in
  match Http2.headers_to_request headers "" with
  | Error (Http2.EmptyPseudoHeader Http2.Scheme) -> Ok ()
  | Ok _ -> Error "expected empty :scheme to be rejected"
  | Error error -> Error (Http2.request_header_error_to_string error)

let test_http2_headers_to_request_rejects_missing_path = fun _ctx ->
  let headers = [ hpack_header ":method" "GET"; hpack_header ":scheme" "https"; ] in
  match Http2.headers_to_request headers "" with
  | Error (Http2.MissingPseudoHeader Http2.Path) -> Ok ()
  | Ok _ -> Error "expected missing :path to be rejected"
  | Error error -> Error (Http2.request_header_error_to_string error)

let test_http2_headers_to_request_rejects_invalid_path = fun _ctx ->
  let too_long_path = "/" ^ String.make ~len:65_536 ~char:'a' in
  let headers = [
    hpack_header ":method" "GET";
    hpack_header ":scheme" "https";
    hpack_header ":path" too_long_path;
  ]
  in
  match Http2.headers_to_request headers "" with
  | Error (Http2.InvalidPath { reason = Net.Uri.TooLong; _ }) -> Ok ()
  | Ok _ -> Error "expected invalid :path to be rejected"
  | Error error -> Error (Http2.request_header_error_to_string error)

let tests =
  Test.[
    case
      "http2 headers to request uses required pseudo headers"
      test_http2_headers_to_request_uses_required_pseudo_headers;
    case
      "http2 headers to request rejects missing method"
      test_http2_headers_to_request_rejects_missing_method;
    case
      "http2 headers to request rejects empty scheme"
      test_http2_headers_to_request_rejects_empty_scheme;
    case
      "http2 headers to request rejects missing path"
      test_http2_headers_to_request_rejects_missing_path;
    case
      "http2 headers to request rejects invalid path"
      test_http2_headers_to_request_rejects_invalid_path;
  ]

let main ~args = Test.Cli.main ~name:"suri:web_server_http2" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
