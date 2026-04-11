open Std
open Pretext

let test_top_level_break_flattens_when_it_fits = fun _ctx ->
  Test.assert_equal
    ~expected:"hello world"
    ~actual:(format ~width:32 [ str "hello"; brk; str "world" ]);
  Ok ()

let test_top_level_break_wraps_when_it_does_not_fit = fun _ctx ->
  Test.assert_equal
    ~expected:"hello\nworld"
    ~actual:(format ~width:10 [ str "hello"; brk; str "world" ]);
  Ok ()

let test_nesting_indents_broken_groups = fun _ctx ->
  let doc = group [ str "["; nest 2 [ brk; str "hello"; brk; str "world"; ]; brk; str "]"; ] in
  Test.assert_equal ~expected:"[\n  hello\n  world\n]" ~actual:(format_doc ~width:6 doc);
  Ok ()

let test_hard_lines_always_break = fun _ctx ->
  Test.assert_equal ~expected:"hello\nworld" ~actual:(format [ str "hello"; line; str "world" ]);
  Ok ()

let test_unicode_width_uses_display_columns = fun _ctx ->
  Test.assert_equal ~expected:"你 好" ~actual:(format ~width:5 [ str "你"; brk; str "好" ]);
  Ok ()

let tests =
  Test.[
    case "top-level break flattens when it fits" test_top_level_break_flattens_when_it_fits;
    case "top-level break wraps when it does not fit" test_top_level_break_wraps_when_it_does_not_fit;
    case "nesting indents broken groups" test_nesting_indents_broken_groups;
    case "hard lines always break" test_hard_lines_always_break;
    case "unicode width uses display columns" test_unicode_width_uses_display_columns;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"pretext" ~tests ~args) ~args:Env.args ()
