open Std

module Test = Std.Test
module Json = Std.Data.Json

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [
      "";
      "data: hello\n\n";
      "event:update\nid:42\ndata:payload\n\n";
      "data: [DONE]\n\n";
      "{\"name\":\"fixture\",\"interactions\":[]}";
    ])

let test_blink_fuzz = fun _ctx input ->
  Blink.SSE.parse_event input
  |> ignore;
  Blink.Testing.Recording.sanitize_name input
  |> ignore;
  Blink.Testing.RecordMode.from_string input
  |> ignore;
  match Json.from_string input with
  | Error _ -> Ok ()
  | Ok json ->
      Blink.Testing.Recording.from_json
        ~fallback_name:input
        ~fallback_mode:Blink.Testing.RecordMode.RecordOnce
        json
      |> ignore;
      Ok ()

let tests =
  Test.[
    fuzz
      "blink sse and recording decoders accept arbitrary input"
      ~seeds:[ ""; "data: hello\n\n"; "{\"interactions\":[]}"; ]
      ~mutator
      test_blink_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"blink_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
