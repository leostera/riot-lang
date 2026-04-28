open Std

module Test = Std.Test
module Testing = Suri.Testing
module Conn = Suri.Conn

let ( let* ) value fn = Result.and_then value ~fn

let expect = fun result ->
  match result with
  | Ok () -> Ok ()
  | Error error -> Error (Testing.Expect.error_to_string error)

let test_app_get_runs_real_pipeline = fun _ctx ->
  let app = [
    fun ~conn ~next:_ ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Ok ~body:"hello"
      |> Conn.with_header "x-suri-test" "yes"
      |> Conn.send;
  ]
  in
  match Testing.App.get app "/" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      let* () = expect (Testing.Expect.status Net.Http.Status.Ok response) in
      let* () = expect (Testing.Expect.body "hello" response) in
      expect (Testing.Expect.header "x-suri-test" "yes" response)

let test_app_post_preserves_request_body = fun _ctx ->
  let app = [
    fun ~conn ~next:_ ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Created ~body:(Conn.body conn)
      |> Conn.send;
  ]
  in
  match Testing.App.post app ~body:"payload" "/messages" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      let* () = expect (Testing.Expect.status Net.Http.Status.Created response) in
      expect (Testing.Expect.body "payload" response)

let test_unsent_pipeline_returns_not_found = fun _ctx ->
  let app = [
    fun ~conn ~next -> next conn;
  ]
  in
  match Testing.App.get app "/missing" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      let* () = expect (Testing.Expect.status Net.Http.Status.NotFound response) in
      expect (Testing.Expect.body "Not Found" response)

let test_middleware_runner_exercises_middleware = fun _ctx ->
  let middleware = fun ~conn ~next ->
    conn
    |> Conn.with_header "x-before-next" "yes"
    |> next
    |> Conn.with_header "x-after-next" "yes"
  in
  let conn =
    Testing.Conn.make ~uri:"/" ~headers:[ ("x-request-id", "request-1"); ] ()
    |> Result.unwrap
  in
  let conn = Testing.Middleware.run middleware conn in
  Test.assert_equal
    ~expected:(Some "request-1")
    ~actual:(Net.Http.Header.get (Conn.headers conn) "x-request-id");
  Test.assert_equal
    ~expected:[ ("x-after-next", "yes"); ("x-before-next", "yes"); ]
    ~actual:(Conn.resp_headers conn);
  Ok ()

let invalid_uri = "/" ^ String.make ~len:66_000 ~char:'a'

let test_request_reports_invalid_uri = fun _ctx ->
  match Testing.Request.to_http (Testing.Request.get invalid_uri) with
  | Error (Testing.Request.InvalidUri { value; reason = Net.Uri.TooLong }) ->
      Test.assert_equal ~expected:invalid_uri ~actual:value;
      Ok ()
  | Ok _ -> Error "expected invalid testing URI"
  | Error error -> Error (Testing.Request.error_to_string error)

let test_conn_make_returns_typed_request_errors = fun _ctx ->
  match Testing.Conn.make ~uri:invalid_uri () with
  | Error (Testing.Request.InvalidUri { reason = Net.Uri.TooLong; _ }) -> Ok ()
  | Ok _ -> Error "expected invalid testing URI"
  | Error error -> Error (Testing.Request.error_to_string error)

let test_app_returns_typed_request_errors = fun _ctx ->
  let app = [
    fun ~conn ~next:_ ->
      conn
      |> Conn.respond ~status:Net.Http.Status.Ok ~body:"ok"
      |> Conn.send;
  ]
  in
  match Testing.App.get app invalid_uri with
  | Error (Testing.InvalidRequest (
    Testing.Request.InvalidUri { reason = Net.Uri.TooLong; _ }
  )) -> Ok ()
  | Ok _ -> Error "expected invalid testing URI"
  | Error error -> Error (Testing.response_error_to_string error)

let test_expect_status_reports_typed_mismatch = fun _ctx ->
  let response = Suri.Response.not_found () in
  match Testing.Expect.status Net.Http.Status.Ok response with
  | Ok () -> Error "expected status mismatch"
  | Error (Testing.Expect.StatusMismatch { expected; actual }) ->
      Test.assert_true (Net.Http.Status.equal Net.Http.Status.Ok expected);
      Test.assert_true (Net.Http.Status.equal Net.Http.Status.NotFound actual);
      Ok ()
  | Error error -> Error (Testing.Expect.error_to_string error)

let tests =
  Test.[
    case "Testing.App.get runs the real Suri pipeline" test_app_get_runs_real_pipeline;
    case "Testing.App.post preserves request body" test_app_post_preserves_request_body;
    case "Testing.App returns not found for unsent pipelines" test_unsent_pipeline_returns_not_found;
    case "Testing.Middleware.run exercises middleware" test_middleware_runner_exercises_middleware;
    case "Testing.Request reports invalid URIs" test_request_reports_invalid_uri;
    case
      "Testing.Conn.make returns typed request errors"
      test_conn_make_returns_typed_request_errors;
    case "Testing.App returns typed request errors" test_app_returns_typed_request_errors;
    case "Testing.Expect.status reports typed mismatches" test_expect_status_reports_typed_mismatch;
  ]

let main ~args = Test.Cli.main ~name:"testing_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
