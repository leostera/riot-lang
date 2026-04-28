open Std

module Conditional_get = Suri.Middleware.Conditional_get
module Conn = Suri.Middleware.Conn

let response_headers = fun headers ->
  List.fold_left
    headers
    ~init:Net.Http.Header.empty
    ~fn:(fun acc (name, value) ->
      Net.Http.Header.add acc name value)

let test_conditional_get_reports_invalid_month = fun _ctx ->
  match Conditional_get.parse_http_date "Wed, 21 Foo 2015 07:28:00 GMT" with
  | Error (Conditional_get.InvalidMonth { value = "Foo" }) -> Ok ()
  | Ok _ -> Error "expected invalid HTTP date month"
  | Error error -> Error (Conditional_get.date_parse_error_to_string error)

let test_conditional_get_reports_invalid_request_date = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~headers:[ ("if-modified-since", "Wed, nope Oct 2015 07:28:00 GMT"); ] ()
    |> Result.unwrap
  in
  let headers = response_headers [ ("last-modified", "Wed, 21 Oct 2015 07:28:00 GMT"); ] in
  match Conditional_get.check_modified_since conn headers with
  | Error (Conditional_get.InvalidRequestDate (
    Conditional_get.InvalidDay { value = "nope" }
  )) ->
      Ok ()
  | Ok _ -> Error "expected invalid If-Modified-Since date"
  | Error error -> Error (Conditional_get.modified_since_error_to_string error)

let test_conditional_get_if_none_match_precedes_modified_since = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make
      ~headers:[
        ("if-none-match", "\"client\"");
        ("if-modified-since", "Wed, 21 Oct 2015 07:28:00 GMT");
      ]
      ()
    |> Result.unwrap
  in
  let response =
    Conditional_get.middleware
      ~conn
      ~next:(fun conn ->
        conn
        |> Conn.with_header "etag" "\"server\""
        |> Conn.with_header "last-modified" "Wed, 21 Oct 2015 07:28:00 GMT"
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"fresh"
        |> Conn.send)
    |> Conn.to_response
  in
  Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
  Test.assert_equal ~expected:"fresh" ~actual:response.body;
  Ok ()

let test_conditional_get_etag_match_returns_not_modified = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~headers:[ ("if-none-match", "\"server\""); ] ()
    |> Result.unwrap
  in
  let response =
    Conditional_get.middleware
      ~conn
      ~next:(fun conn ->
        conn
        |> Conn.with_header "etag" "\"server\""
        |> Conn.with_header "cache-control" "public, max-age=60"
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"cached"
        |> Conn.send)
    |> Conn.to_response
  in
  Test.assert_equal ~expected:Net.Http.Status.NotModified ~actual:response.status;
  Test.assert_equal ~expected:"" ~actual:response.body;
  Test.assert_equal
    ~expected:(Some "\"server\"")
    ~actual:(Net.Http.Header.get response.headers "etag");
  Test.assert_equal
    ~expected:(Some "public, max-age=60")
    ~actual:(Net.Http.Header.get response.headers "cache-control");
  Ok ()

let tests =
  Test.[
    case "conditional get reports invalid month" test_conditional_get_reports_invalid_month;
    case
      "conditional get reports invalid request date"
      test_conditional_get_reports_invalid_request_date;
    case
      "conditional get if none match precedes modified since"
      test_conditional_get_if_none_match_precedes_modified_since;
    case
      "conditional get etag match returns not modified"
      test_conditional_get_etag_match_returns_not_modified;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-conditional-get" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
