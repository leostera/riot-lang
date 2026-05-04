open Std

let test_plain_status_labels = fun _ctx ->
  let terminal = Jollyroger.Terminal.plain in
  Test.assert_equal
    ~expected:"[ok]"
    ~actual:(Jollyroger.Terminal.status_label terminal Jollyroger.Terminal.Success);
  Test.assert_equal
    ~expected:"[error] failed"
    ~actual:(Jollyroger.Terminal.status_line terminal Jollyroger.Terminal.Error "failed");
  Ok ()

let test_layout_fields_align_labels = fun _ctx ->
  let actual =
    Jollyroger.Layout.fields
      ~indent:2
      [ ("package", "std"); ("source", "src/std.ml"); ("fix", "riot add std") ]
    |> String.concat "\n"
  in
  Test.assert_equal
    ~expected:"  package: std\n  source : src/std.ml\n  fix    : riot add std"
    ~actual;
  Ok ()

let test_layout_bullets_are_plain_text_safe = fun _ctx ->
  Test.assert_equal
    ~expected:"    - add the missing package"
    ~actual:(Jollyroger.Layout.bullet ~indent:4 "add the missing package");
  Ok ()

let test_color_can_be_disabled = fun _ctx ->
  let terminal = Jollyroger.Terminal.make ~color:false () in
  Test.assert_false (Jollyroger.Terminal.color_enabled terminal);
  Test.assert_equal ~expected:"plain" ~actual:(Jollyroger.Terminal.success terminal "plain");
  Ok ()

let tests =
  Test.[
    case "plain status labels" test_plain_status_labels;
    case "layout fields align labels" test_layout_fields_align_labels;
    case "layout bullets are plain text safe" test_layout_bullets_are_plain_text_safe;
    case "color can be disabled" test_color_can_be_disabled;
  ]

let main ~args = Test.Cli.main ~name:"jollyroger_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
