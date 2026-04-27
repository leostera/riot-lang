open Std

module Conn = Suri.Middleware.Conn
module Etag = Suri.Middleware.Etag

let run = fun ?weak status body ->
  Etag.middleware
    ?weak
    ()
    ~conn:(Suri.Testing.Conn.make ())
    ~next:(fun conn ->
      conn
      |> Conn.respond ~status ~body
      |> Conn.send)
  |> Conn.to_response

let test_etag_generates_strong_tags_for_ok_bodies = fun _ctx ->
  let response = run Net.Http.Status.Ok "hello" in
  match Net.Http.Header.get response.headers "etag" with
  | Some value ->
      Test.assert_true (String.starts_with ~prefix:"\"" value);
      Test.assert_true (String.ends_with ~suffix:"\"" value);
      Test.assert_false (String.starts_with ~prefix:"W/" value);
      Ok ()
  | None -> Error "expected generated ETag"

let test_etag_generates_weak_tags_when_requested = fun _ctx ->
  let response = run ~weak:true Net.Http.Status.Ok "hello" in
  match Net.Http.Header.get response.headers "etag" with
  | Some value ->
      Test.assert_true (String.starts_with ~prefix:"W/\"" value);
      Ok ()
  | None -> Error "expected generated weak ETag"

let test_etag_preserves_existing_header = fun _ctx ->
  let response =
    Etag.middleware
      ()
      ~conn:(Suri.Testing.Conn.make ())
      ~next:(fun conn ->
        conn
        |> Conn.with_header "etag" "\"custom\""
        |> Conn.respond ~status:Net.Http.Status.Ok ~body:"hello"
        |> Conn.send)
    |> Conn.to_response
  in
  Test.assert_equal
    ~expected:(Some "\"custom\"")
    ~actual:(Net.Http.Header.get response.headers "etag");
  Ok ()

let test_etag_skips_no_body_statuses = fun _ctx ->
  let no_content = run Net.Http.Status.NoContent "not emitted" in
  let reset_content = run Net.Http.Status.ResetContent "not emitted" in
  let not_modified = run Net.Http.Status.NotModified "not emitted" in
  Test.assert_equal ~expected:None ~actual:(Net.Http.Header.get no_content.headers "etag");
  Test.assert_equal ~expected:None ~actual:(Net.Http.Header.get reset_content.headers "etag");
  Test.assert_equal ~expected:None ~actual:(Net.Http.Header.get not_modified.headers "etag");
  Ok ()

let tests =
  Test.[
    case "etag generates strong tags for ok bodies" test_etag_generates_strong_tags_for_ok_bodies;
    case "etag generates weak tags when requested" test_etag_generates_weak_tags_when_requested;
    case "etag preserves existing header" test_etag_preserves_existing_header;
    case "etag skips no body statuses" test_etag_skips_no_body_statuses;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-etag" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
