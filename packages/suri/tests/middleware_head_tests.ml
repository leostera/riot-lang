open Std

module Conn = Suri.Middleware.Conn
module Head = Suri.Middleware.Head
module Router = Suri.Middleware.Router
module Testing = Suri.Testing

let ok_handler = fun conn _req ->
  conn
  |> Conn.respond ~status:Net.Http.Status.Ok ~body:"content"
  |> Conn.set_header "content-length" "7"
  |> Conn.send

let test_head_routes_through_get_and_strips_body = fun _ctx ->
  let app = [ Head.middleware; Router.middleware [ Router.get "/resource" ok_handler; ]; ] in
  let request = Testing.Request.make ~method_:Net.Http.Method.Head ~uri:"/resource" () in
  match Testing.App.response app request with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"" ~actual:response.body;
      Test.assert_equal
        ~expected:(Some "7")
        ~actual:(Net.Http.Header.get response.headers "content-length");
      Ok ()

let test_head_preserves_non_head_requests = fun _ctx ->
  let app = [ Head.middleware; Router.middleware [ Router.get "/resource" ok_handler; ]; ] in
  match Testing.App.get app "/resource" with
  | Error error -> Error (Testing.response_error_to_string error)
  | Ok response ->
      Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
      Test.assert_equal ~expected:"content" ~actual:response.body;
      Ok ()

let tests =
  Test.[
    case "head routes through get and strips body" test_head_routes_through_get_and_strips_body;
    case "head preserves non-head requests" test_head_preserves_non_head_requests;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-head" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
