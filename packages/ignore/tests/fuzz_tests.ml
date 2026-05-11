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
  |> with_max_len 2_048
  |> with_dictionary [ ""; "*.ml"; "!keep.ml"; "vendor/"; "/_build"; "[bad"; "\\#literal"; ])

let test_ignore_pattern_fuzz = fun _ctx input ->
  let input = printable_text input in
  let root = Path.v "." in
  Ignore.Walker.create
    ~roots:[ root ]
    ~hidden:false
    ~parents:false
    ~ignore_patterns:[ input ]
    ~overrides:[ input ]
    ()
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "ignore walker pattern parser accepts arbitrary text"
      ~seeds:[ ""; "*.ml"; "!keep.ml"; "vendor/"; "[bad"; ]
      ~mutator
      test_ignore_pattern_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"ignore_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
