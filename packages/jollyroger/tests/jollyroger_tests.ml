open Std

let assert_color = fun ~expected actual ->
  Test.assert_equal
    ~expected
    ~actual:(Tty.Color.to_string actual)

let test_palette_matches_jolly_roger_tokens = fun _ctx ->
  assert_color ~expected:"RGB(255,248,237)" Jollyroger.Palette.paper;
  assert_color ~expected:"RGB(245,236,220)" Jollyroger.Palette.paper_2;
  assert_color ~expected:"RGB(21,19,23)" Jollyroger.Palette.ink;
  assert_color ~expected:"RGB(14,13,16)" Jollyroger.Palette.coal;
  assert_color ~expected:"RGB(239,35,60)" Jollyroger.Palette.riot;
  assert_color ~expected:"RGB(36,192,141)" Jollyroger.Palette.mint;
  assert_color ~expected:"RGB(240,180,41)" Jollyroger.Palette.amber;
  assert_color ~expected:"RGB(39,119,255)" Jollyroger.Palette.blue;
  assert_color ~expected:"RGB(201,31,56)" Jollyroger.Palette.brand_hover;
  assert_color ~expected:"RGB(159,23,44)" Jollyroger.Palette.brand_active;
  assert_color ~expected:"RGB(91,84,98)" Jollyroger.Palette.text_muted;
  assert_color ~expected:"RGB(154,160,170)" Jollyroger.Palette.text_subtle;
  assert_color ~expected:"RGB(255,253,247)" Jollyroger.Palette.text_inverse;
  assert_color ~expected:"RGB(230,226,214)" Jollyroger.Palette.syntax_text;
  Ok ()

let test_surface_palettes_are_lifted_for_light_and_dark_modes = fun _ctx ->
  assert_color ~expected:"RGB(255,111,135)" Jollyroger.Palette.LightMode.action;
  assert_color ~expected:"RGB(54,209,159)" Jollyroger.Palette.LightMode.success;
  assert_color ~expected:"RGB(246,185,31)" Jollyroger.Palette.LightMode.warning;
  assert_color ~expected:"RGB(255,111,135)" Jollyroger.Palette.LightMode.danger;
  assert_color ~expected:"RGB(77,152,255)" Jollyroger.Palette.LightMode.reference;
  assert_color ~expected:"RGB(154,160,170)" Jollyroger.Palette.LightMode.muted;
  assert_color ~expected:"RGB(255,138,160)" Jollyroger.Palette.DarkMode.action;
  assert_color ~expected:"RGB(101,231,117)" Jollyroger.Palette.DarkMode.success;
  assert_color ~expected:"RGB(255,196,61)" Jollyroger.Palette.DarkMode.warning;
  assert_color ~expected:"RGB(255,138,160)" Jollyroger.Palette.DarkMode.danger;
  assert_color ~expected:"RGB(155,210,255)" Jollyroger.Palette.DarkMode.reference;
  assert_color ~expected:"RGB(184,180,171)" Jollyroger.Palette.DarkMode.muted;
  Ok ()

let test_plain_status_labels = fun _ctx ->
  let terminal = Jollyroger.Terminal.plain in
  Test.assert_equal
    ~expected:"ok"
    ~actual:(Jollyroger.Terminal.status_label terminal Jollyroger.Terminal.Success);
  Test.assert_equal
    ~expected:"error failed"
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
    case "palette matches Jolly Roger tokens" test_palette_matches_jolly_roger_tokens;
    case
      "surface palettes are lifted for light and dark modes"
      test_surface_palettes_are_lifted_for_light_and_dark_modes;
    case "plain status labels" test_plain_status_labels;
    case "layout fields align labels" test_layout_fields_align_labels;
    case "layout bullets are plain text safe" test_layout_bullets_are_plain_text_safe;
    case "color can be disabled" test_color_can_be_disabled;
  ]

let main ~args = Test.Cli.main ~name:"jollyroger_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
