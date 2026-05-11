open Std

module Test = Std.Test
module Json = Std.Data.Json

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "{}";
      "[]";
      "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1}";
      "{\"jsonrpc\":\"2.0\",\"method\":\"ping\"}";
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":null}";
    ])

let test_jsonrpc_fuzz = fun _ctx input ->
  Jsonrpc.request ~method_:input ~id:(Jsonrpc.String input) ()
  |> Jsonrpc.request_to_json
  |> ignore;
  match Json.from_string input with
  | Error _ -> Ok ()
  | Ok json ->
      Jsonrpc.id_of_json json
      |> ignore;
      Jsonrpc.request_of_json json
      |> ignore;
      Ok ()

let tests =
  Test.[
    fuzz
      "jsonrpc request and id decoders accept arbitrary json text"
      ~seeds:[ ""; "{}"; "{\"jsonrpc\":\"2.0\",\"method\":\"ping\",\"id\":1}"; ]
      ~mutator
      test_jsonrpc_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"jsonrpc_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
