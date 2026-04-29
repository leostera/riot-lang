open Std
open Markdown

let ( let* ) = fun result fn -> Result.and_then result ~fn

let find_required = fun source needle ->
  let needle_len = String.length needle in
  let source_len = String.length source in
  let rec loop index =
    if index + needle_len > source_len then
      panic ("missing substring: " ^ needle)
    else if String.sub source ~offset:index ~len:needle_len = needle then
      index
    else
      loop (index + 1)
  in
  loop 0

let expect_string = fun ~label ~expected ~actual ->
  if String.equal expected actual then
    Ok ()
  else
    Error (label ^ ": expected " ^ expected ^ " but got " ^ actual)

let expect_int = fun ~label ~expected ~actual ->
  if Int.equal expected actual then
    Ok ()
  else
    Error (label ^ ": expected " ^ Int.to_string expected ^ " but got " ^ Int.to_string actual)

let expect_bool = fun ~label ~expected ~actual ->
  if expected = actual then
    Ok ()
  else
    let render value =
      if value then
        "true"
      else
        "false"
    in
    Error (label ^ ": expected " ^ render expected ^ " but got " ^ render actual)

let expect_some = fun ~label value ->
  match value with
  | Some value -> Ok value
  | None -> Error (label ^ ": expected Some")

let test_single_line_paragraph_edit_reuses_surrounding_blocks = fun _ctx ->
  let source = "# Title\n\nhello world\n\n- item\n" in
  let doc = Document.parse source in
  let start = find_required source "world" in
  let updated =
    Document.update doc ~edit:{ start; end_ = start + String.length "world"; text = "markdown" }
  in
  let expected_source = "# Title\n\nhello markdown\n\n- item\n" in
  let* () =
    expect_string ~label:"source" ~expected:expected_source ~actual:(Document.source updated)
  in
  let* () =
    expect_string
      ~label:"html"
      ~expected:(Markdown.compile expected_source)
      ~actual:(Document.to_html updated)
  in
  let* stats = expect_some ~label:"last update" (Document.last_update updated) in
  let* () = expect_bool ~label:"full reparse" ~expected:false ~actual:stats.reparsed_full in
  let* () = expect_int ~label:"reused prefix" ~expected:1 ~actual:stats.reused_prefix_blocks in
  let* () = expect_int ~label:"reparsed blocks" ~expected:1 ~actual:stats.reparsed_blocks in
  expect_int ~label:"reused suffix" ~expected:1 ~actual:stats.reused_suffix_blocks

let test_structural_edit_falls_back_to_full_parse = fun _ctx ->
  let source = "# Title\n\nhello world\n\n- item\n" in
  let doc = Document.parse source in
  let start = find_required source "hello" in
  let updated = Document.update doc ~edit:{ start; end_ = start; text = "- inserted\n" } in
  let expected_source = "# Title\n\n- inserted\nhello world\n\n- item\n" in
  let* () =
    expect_string ~label:"source" ~expected:expected_source ~actual:(Document.source updated)
  in
  let* () =
    expect_string
      ~label:"html"
      ~expected:(Markdown.compile expected_source)
      ~actual:(Document.to_html updated)
  in
  let* stats = expect_some ~label:"last update" (Document.last_update updated) in
  expect_bool ~label:"full reparse" ~expected:true ~actual:stats.reparsed_full

let tests =
  Test.[
    case
      "single-line paragraph edit reuses surrounding blocks"
      test_single_line_paragraph_edit_reuses_surrounding_blocks;
    case "structural edit falls back to full parse" test_structural_edit_falls_back_to_full_parse;
  ]

let main ~args = Test.Cli.main ~name:"markdown-incremental" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
