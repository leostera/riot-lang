open Std

module Test = Std.Test
module Accepts = Suri.Middleware.Accepts
module Body_parser = Suri.Middleware.Body_parser
module Method_override = Suri.Middleware.Method_override

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "application/json";
      "text/html, application/json;q=0.8, */*;q=0.1";
      "application/x-www-form-urlencoded";
      "{\"name\":\"riot\"}";
      "name=riot&_method=DELETE";
      "DELETE";
      "PATCH";
    ])

let test_suri_fuzz = fun _ctx input ->
  Accepts.parse_accept input
  |> ignore;
  Accepts.parse_accept_or_empty input
  |> ignore;
  Accepts.get_base_content_type input
  |> ignore;
  Accepts.accept_header_matches ~types:[ "application/json"; "text/*"; "*/*"; ] input
  |> ignore;
  Body_parser.parse_body (Body_parser.default_config ()) ~content_type:input ~body:input
  |> ignore;
  Body_parser.parse_body
    {
      Body_parser.parsers = [ Body_parser.Urlencoded; Body_parser.Json; Body_parser.Multipart ];
      max_body_size = 4_096;
    }
    ~content_type:"application/x-www-form-urlencoded"
    ~body:input
  |> ignore;
  Method_override.parse_override_method input
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "suri middleware parsers accept arbitrary input"
      ~seeds:[ ""; "application/json"; "name=riot&_method=DELETE"; "{\"name\":\"riot\"}"; ]
      ~mutator
      test_suri_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"suri_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
