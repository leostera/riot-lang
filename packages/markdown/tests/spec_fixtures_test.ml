open Std
open Markdown

module Markdown_fixture_db = Tests__Markdown_fixture_db

let normalize_label = fun label ->
  let normalized = String.lowercase_ascii label in
  let transformed =
    String.map
      ~fn:
      (fun ch ->
        if
          (ch >= 'a' && ch <= 'z')
          || (ch >= '0' && ch <= '9')
          || ch = '-'
          || ch = '_'
        then
          ch
        else
          '_'
      )
      normalized
  in
  let trim_underscores = fun value ->
    let len = String.length value in
    let rec skip_left idx =
      if idx >= len || not (Char.equal (String.get_unchecked value ~at:idx) '_') then
        idx
      else
        skip_left (idx + 1)
    in
    let rec skip_right idx =
      if idx < 0 || not (Char.equal (String.get_unchecked value ~at:idx) '_') then
        idx
      else
        skip_right (idx - 1)
    in
    let left = skip_left 0 in
    let right = skip_right (len - 1) in
    if left > right then
      "fixture"
    else
      String.sub value ~offset:left ~len:(right - left + 1)
  in
  trim_underscores transformed

let fixture_test_name = fun index (fixture: Markdown_fixture_db.fixture) ->
  let section =
    Option.unwrap_or ~default:"fixture" fixture.section
  in
  let example =
    Option.map ~fn:Int.to_string fixture.example
    |> Option.unwrap_or ~default:(Int.to_string (index + 1))
  in
  "markdown/spec/" ^ normalize_label section ^ "/" ^ example ^ "_" ^ Int.to_string index

let test_fixture = fun (fixture: Markdown_fixture_db.fixture) index ctx ->
  let actual = Markdown.compile fixture.markdown in
  Test.Snapshot.assert_inline_text
    ~ctx
    ~actual
    ~expected:fixture.html

let fixture_cases = fun () ->
  Markdown_fixture_db.all_spec_fixtures ()
  |> List.enumerate
  |> List.map ~fn:(fun (index, fixture) ->
    Test.case
      (fixture_test_name index fixture)
      (fun ctx -> test_fixture fixture index ctx))

let gfm_cases = fun () ->
  [
    Test.case "markdown/gfm/strikethrough" (fun ctx ->
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual:(Markdown.compile_gfm "~~gone~~\n")
        ~expected:"<p><del>gone</del></p>\n");
    Test.case "markdown/gfm/task-list" (fun ctx ->
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual:(Markdown.compile_gfm "- [ ] todo\n- [x] done\n")
        ~expected:
          "<ul>\n\
          <li class=\"task-list-item\">\n\
          <input type=\"checkbox\" disabled /><p>todo</p>\n\
          </li>\n\
          <li class=\"task-list-item\">\n\
          <input type=\"checkbox\" checked disabled /><p>done</p>\n\
          </li>\n\
          </ul>\n");
    Test.case "markdown/gfm/table" (fun ctx ->
      Test.Snapshot.assert_inline_text
        ~ctx
        ~actual:(Markdown.compile_gfm "| a | b |\n| --- | ---: |\n| c | d |\n")
        ~expected:
          "<table>\n\
          <thead>\n\
          <tr>\n\
          <th>a</th>\n\
          <th align=\"right\">b</th>\n\
          </tr>\n\
          </thead>\n\
          <tbody>\n\
          <tr>\n\
          <td>c</td>\n\
          <td align=\"right\">d</td>\n\
          </tr>\n\
          </tbody>\n\
          </table>\n");
  ]

let cases = fun () -> fixture_cases () @ gfm_cases ()

let main ~args =
      Test.Cli.main
        ~name:"markdown-spec-fixtures"
        ~tests:(cases ())
        ~args
        ()

let () = Runtime.run ~main ~args:Env.args ()
