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
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}";
      "{\"jsonrpc\":\"2.0\",\"method\":\"notifications/progress\",\"params\":{}}";
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"result\":{}}";
    ])

let test_mcp_fuzz = fun _ctx input ->
  let request =
    Mcp.{
      jsonrpc = Jsonrpc.version;
      id = Mcp.String input;
      method_name = input;
      params = Some (CustomParams (Json.String input));
    }
  in
  Mcp.request_to_json request
  |> ignore;
  match Json.from_string input with
  | Error _ -> Ok ()
  | Ok json ->
      Mcp.request_of_json json
      |> ignore;
      Mcp.response_of_json json
      |> ignore;
      Mcp.notification_of_json json
      |> ignore;
      Ok ()

let tests =
  Test.[
    fuzz
      "mcp json codecs accept arbitrary json text"
      ~seeds:[ ""; "{}"; "{\"jsonrpc\":\"2.0\",\"id\":1,\"method\":\"ping\"}"; ]
      ~mutator
      test_mcp_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"mcp_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
