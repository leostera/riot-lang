open Std

module Conn = Suri.Middleware.Conn
module Method_override = Suri.Middleware.Method_override

let test_method_override_parses_allowed_methods = fun _ctx ->
  Test.assert_equal
    ~expected:(Ok Net.Http.Method.Patch)
    ~actual:(Method_override.parse_override_method " patch ");
  Ok ()

let test_method_override_rejects_empty_method = fun _ctx ->
  match Method_override.parse_override_method " " with
  | Error Method_override.MissingOverrideMethod -> Ok ()
  | Ok method_ ->
      Error ("expected empty override method to fail, got " ^ Net.Http.Method.to_string method_)
  | Error error -> Error (Method_override.override_error_to_string error)

let test_method_override_rejects_disallowed_method = fun _ctx ->
  match Method_override.parse_override_method "GET" with
  | Error (Method_override.MethodNotAllowed { method_; allowed }) ->
      Test.assert_equal ~expected:Net.Http.Method.Get ~actual:method_;
      Test.assert_equal ~expected:[ Net.Http.Method.Put; Patch; Delete; ] ~actual:allowed;
      Ok ()
  | Ok method_ ->
      Error ("expected disallowed override method to fail, got " ^ Net.Http.Method.to_string method_)
  | Error error -> Error (Method_override.override_error_to_string error)

let test_method_override_applies_allowed_method = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~method_:Net.Http.Method.Post ~body_params:[ ("_method", "DELETE"); ] ()
  in
  let conn' =
    Method_override.middleware
      ()
      ~conn
      ~next:(fun conn ->
        conn
        |> Conn.respond
          ~status:Net.Http.Status.Ok
          ~body:(Net.Http.Method.to_string (Conn.method_ conn))
        |> Conn.send)
  in
  let response = Conn.to_response conn' in
  Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
  Test.assert_equal ~expected:"DELETE" ~actual:response.body;
  Ok ()

let test_method_override_rejects_invalid_method = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~method_:Net.Http.Method.Post ~body_params:[ ("_method", "GET"); ] ()
  in
  let continued = ref false in
  let response =
    Method_override.middleware
      ()
      ~conn
      ~next:(fun conn ->
        continued := true;
        conn)
    |> Conn.to_response
  in
  Test.assert_false !continued;
  Test.assert_equal ~expected:Net.Http.Status.BadRequest ~actual:response.status;
  Test.assert_equal
    ~expected:"method override is not allowed: GET; allowed methods: PUT, PATCH, DELETE"
    ~actual:response.body;
  Ok ()

let test_method_override_ignores_non_post_requests = fun _ctx ->
  let conn =
    Suri.Testing.Conn.make ~method_:Net.Http.Method.Get ~body_params:[ ("_method", "DELETE"); ] ()
  in
  let conn' =
    Method_override.middleware
      ()
      ~conn
      ~next:(fun conn ->
        conn
        |> Conn.respond
          ~status:Net.Http.Status.Ok
          ~body:(Net.Http.Method.to_string (Conn.method_ conn))
        |> Conn.send)
  in
  let response = Conn.to_response conn' in
  Test.assert_equal ~expected:Net.Http.Status.Ok ~actual:response.status;
  Test.assert_equal ~expected:"GET" ~actual:response.body;
  Ok ()

let tests =
  Test.[
    case "method override parses allowed methods" test_method_override_parses_allowed_methods;
    case "method override rejects empty method" test_method_override_rejects_empty_method;
    case "method override rejects disallowed method" test_method_override_rejects_disallowed_method;
    case "method override applies allowed method" test_method_override_applies_allowed_method;
    case "method override rejects invalid method" test_method_override_rejects_invalid_method;
    case "method override ignores non-post requests" test_method_override_ignores_non_post_requests;
  ]

let main ~args = Test.Cli.main ~name:"suri:middleware-method-override" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
