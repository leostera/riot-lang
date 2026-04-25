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

let test_softline_disappears_in_flat_mode = fun _ctx ->
  Test.assert_equal
    ~expected:"alphabeta"
    ~actual:(format ~width:32 [ group [ str "alpha"; softline; str "beta" ] ]);
  Ok ()

let test_join_interleaves_separator = fun _ctx ->
  Test.assert_equal
    ~expected:"alpha, beta, gamma"
    ~actual:(format
      ~width:32
      [ join (concat [ str ","; brk ]) [ str "alpha"; str "beta"; str "gamma" ] ]);
  Ok ()

let test_multiline_text_honors_indent_after_break = fun _ctx ->
  let doc = group [ str "items:"; nest 2 [ line; str "alpha\nbeta" ] ] in
  Test.assert_equal ~expected:"items:\n  alpha\nbeta" ~actual:(format_doc ~width:12 doc);
  Ok ()

let tests =
  Test.[
    case "top-level break flattens when it fits" test_top_level_break_flattens_when_it_fits;
    case "top-level break wraps when it does not fit" test_top_level_break_wraps_when_it_does_not_fit;
    case "nesting indents broken groups" test_nesting_indents_broken_groups;
    case "hard lines always break" test_hard_lines_always_break;
    case "unicode width uses display columns" test_unicode_width_uses_display_columns;
    case "softline disappears in flat mode" test_softline_disappears_in_flat_mode;
    case "join interleaves separator" test_join_interleaves_separator;
    case "multiline text honors indent after break" test_multiline_text_honors_indent_after_break;
  ]

let main ~args = Test.Cli.main ~name:"pretext" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
