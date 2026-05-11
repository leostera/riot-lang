open Std

module Test = Std.Test

let printable_text = fun input ->
  let bytes = IO.Bytes.from_string input in
  let len = IO.Bytes.length bytes in
  let rec loop index =
    if index >= len then
      IO.Bytes.unsafe_to_string bytes
    else
      let ch = IO.Bytes.get_unchecked bytes ~at:index in
      let code = Char.code ch in
      if code < 32 || code > 126 then
        IO.Bytes.set_unchecked bytes ~at:index ~char:' ';
    loop (index + 1)
  in
  loop 0

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 4_096
  |> with_dictionary
    [ ""; "KEY=value\n"; "QUOTED=\"hello world\"\n"; "EXPAND=${KEY}\n"; "export KEY=value\n"; ])

let test_dotenv_fuzz = fun _ctx input ->
  let input = printable_text input in
  Dotenv.parse input
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "dotenv parser accepts arbitrary text"
      ~seeds:[ ""; "KEY=value\n"; "A=${B}\n"; ]
      ~mutator
      test_dotenv_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"dotenv_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
