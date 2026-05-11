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
      "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{}}";
      "{\"jsonrpc\":\"2.0\",\"method\":\"initialized\",\"params\":{}}";
      "{\"jsonrpc\":\"2.0\",\"id\":1,\"error\":{\"code\":-32603,\"message\":\"x\"}}";
    ])

let test_utf16_fuzz = fun input ->
  let len = String.length input in
  let offset = Int.min len (len / 2) in
  Lsp.Utf16.position_of_offset input ~offset
  |> ignore;
  Lsp.Utf16.offset_of_position input { Lsp.Position.line = 0; character = offset }
  |> ignore;
  Lsp.Utf16.range_of_offsets input ~start_offset:0 ~end_offset:offset
  |> ignore

let test_lsp_json_fuzz = fun json ->
  Lsp.request_of_json Lsp.Initialize.request json
  |> ignore;
  Lsp.request_of_json Lsp.Shutdown.request json
  |> ignore;
  Lsp.request_of_json Lsp.Text_document_methods.Hover.request json
  |> ignore;
  Lsp.notification_of_json Lsp.Initialized.notification json
  |> ignore;
  Lsp.notification_of_json Lsp.Exit.notification json
  |> ignore;
  Lsp.error_response_of_json json
  |> ignore

let test_lsp_fuzz = fun _ctx input ->
  test_utf16_fuzz input;
  match Json.from_string input with
  | Error _ -> Ok ()
  | Ok json ->
      test_lsp_json_fuzz json;
      Ok ()

let tests =
  Test.[
    fuzz
      "lsp json codecs and utf16 helpers accept arbitrary input"
      ~seeds:[
        "";
        "{\"jsonrpc\":\"2.0\",\"method\":\"initialize\",\"id\":1,\"params\":{}}";
        "a\240\159\152\128b";
      ]
      ~mutator
      test_lsp_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"lsp_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
