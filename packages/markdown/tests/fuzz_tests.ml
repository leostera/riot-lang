open Std

module Test = Std.Test

let mutator =
  Test.Fuzz.Mutator.(text
  |> with_max_len 8_192
  |> with_dictionary
    [
      "";
      "# Heading\n\nText";
      "```ml\nlet x = 1\n```";
      "- [ ] task";
      "| a | b |\n| - | - |\n| 1 | 2 |";
      "<div>html</div>";
    ])

let test_markdown_fuzz = fun _ctx input ->
  let parsed = Markdown.parse input in
  Markdown.to_html parsed
  |> ignore;
  let parsed_gfm = Markdown.parse_gfm input in
  Markdown.to_html parsed_gfm
  |> ignore;
  let document = Markdown.Document.parse_gfm input in
  Markdown.Document.to_html document
  |> ignore;
  let len = String.length input in
  let start = Int.min len (len / 2) in
  Markdown.Document.update document ~edit:{ start; end_ = start; text = input }
  |> ignore;
  Ok ()

let tests =
  Test.[
    fuzz
      "markdown parser renderer and incremental update accept arbitrary text"
      ~seeds:[ ""; "# Heading\n"; "text\n\ntext"; "```ml\nlet x = 1\n"; ]
      ~mutator
      test_markdown_fuzz;
  ]

let main ~args = Test.Cli.main ~name:"markdown_fuzz_tests" ~tests ~args ()

let () = Std.Runtime.run ~main ~args:Std.Env.args ()
