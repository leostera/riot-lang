open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let make_config = fun ?(width = 80.0) ?(height = 24.0) () ->
  Config.make
    ~viewport:(Viewport.make ~width ~height)
    ~text_measurer:Config.default_text_measurer
    ()

let text_commands = fun commands ->
  List.filter_map
    commands
    ~fn:(fun command ->
      match command.Render.command_type with
      | Render.Text data -> Some (data, command.bounding_box)
      | _ -> None)

let test_default_text_measurer_multiline = fun _ctx ->
  let measurement =
    Config.default_text_measurer ~constraints:(Config.constraints ()) "Hi\nthere" Style.empty
  in
  if approx_eq measurement.size.width 5.0 && approx_eq measurement.size.height 2.0 then
    Ok ()
  else
    Error "default_text_measurer should use longest visible line and line count"

let test_default_text_measurer_cjk_width = fun _ctx ->
  let measurement =
    Config.default_text_measurer ~constraints:(Config.constraints ()) "中" Style.empty
  in
  if approx_eq measurement.size.width 2.0 then
    Ok ()
  else
    Error "CJK characters should measure to their terminal display width"

let test_default_text_measurer_emoji_width = fun _ctx ->
  let measurement =
    Config.default_text_measurer ~constraints:(Config.constraints ()) "👍" Style.empty
  in
  if approx_eq measurement.size.width 2.0 then
    Ok ()
  else
    Error "Emoji should measure to their terminal display width"

let test_default_text_measurer_combining_sequence = fun _ctx ->
  let measurement =
    Config.default_text_measurer ~constraints:(Config.constraints ()) "é" Style.empty
  in
  if approx_eq measurement.size.width 1.0 then
    Ok ()
  else
    Error "Combining mark sequences should occupy a single display cell"

let test_default_text_measurer_respects_available_width = fun _ctx ->
  let measurement =
    Config.default_text_measurer
      ~constraints:(Config.constraints ~available_width:3.0 ())
      "aa bb"
      Style.empty
  in
  if
    measurement.lines = [ "aa"; "bb" ]
    && approx_eq measurement.size.width 2.0
    && approx_eq measurement.size.height 2.0
  then
    Ok ()
  else
    Error "default_text_measurer should honor the available width when wrapping"

let test_word_wrap_breaks_text_into_multiple_lines = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> width (Fixed 4.0)
      |> text_wrap Words)
      "aa bb cc"
  in
  let contents =
    List.map
      (text_commands (layout ~config:(make_config ()) ui))
      ~fn:(fun ({ Render.content; _ }, _) -> content)
  in
  if contents = [ "aa"; "bb"; "cc" ] then
    Ok ()
  else
    Error "Word wrapping should break text on word boundaries"

let test_character_wrap_breaks_long_words = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> width (Fixed 3.0)
      |> text_wrap Character)
      "abcdefg"
  in
  let contents =
    List.map
      (text_commands (layout ~config:(make_config ()) ui))
      ~fn:(fun ({ Render.content; _ }, _) -> content)
  in
  if contents = [ "abc"; "def"; "g" ] then
    Ok ()
  else
    Error "Character wrapping should break long text at grapheme boundaries"

let test_no_wrap_preserves_single_line_content = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> width (Fixed 3.0)
      |> text_wrap NoWrap)
      "abcdef"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.content = "abcdef"; _ }, _) ] -> Ok ()
  | _ -> Error "NoWrap text should stay as a single render command"

let test_text_align_center_offsets_text = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> width (Fixed 10.0)
      |> text_align TextCenter)
      "Hi"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.content = "Hi"; _ }, box) ] when approx_eq box.x 4.0 -> Ok ()
  | _ -> Error "Centered text should shift within the element's content box"

let test_text_align_right_offsets_text = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> width (Fixed 10.0)
      |> text_align TextRight)
      "Hi"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.content = "Hi"; _ }, box) ] when approx_eq box.x 8.0 -> Ok ()
  | _ -> Error "Right-aligned text should move to the end of the content box"

let test_underline_survives_into_render_commands = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> underline)
      "underline"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.decoration = Style.Underline; _ }, _) ] -> Ok ()
  | _ -> Error "Underline should survive command generation"

let test_strikethrough_survives_into_render_commands = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> strikethrough)
      "strike"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.decoration = Style.Strikethrough; _ }, _) ] -> Ok ()
  | _ -> Error "Strikethrough should survive command generation"

let test_bold_survives_into_render_commands = fun _ctx ->
  let ui =
    Element.text
      ~style:Style.(empty
      |> bold)
      "bold"
  in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.weight = Style.Bold; _ }, _) ] -> Ok ()
  | _ -> Error "Bold should survive command generation"

let test_layout_uses_the_configured_text_measurer = fun _ctx ->
  let text_measurer: Config.text_measurer = fun ~constraints:_ _text _style -> {
    size = Viewport.make ~width:5.0 ~height:2.0;
    lines = [ "left"; "right" ];
  }
  in
  let config =
    Config.make
      ~viewport:(Viewport.make ~width:80.0 ~height:24.0)
      ~text_measurer
      ()
  in
  let contents =
    List.map
      (text_commands (layout ~config (Element.text "ignored")))
      ~fn:(fun ({ Render.content; _ }, _) -> content)
  in
  if contents = [ "left"; "right" ] then
    Ok ()
  else
    Error "Layout should use the configured text measurer's wrapped lines"

let test_text_size_is_a_terminal_layout_no_op = fun _ctx ->
  let normal =
    layout
      ~config:(make_config ())
      (
        Element.text
          ~style:Style.(empty
          |> text_size 12)
          "same"
      )
    |> text_commands
  in
  let large =
    layout
      ~config:(make_config ())
      (
        Element.text
          ~style:Style.(empty
          |> text_size 72)
          "same"
      )
    |> text_commands
  in
  match (normal, large) with
  | ([ (_, normal_box) ], [ (_, large_box) ]) when approx_eq normal_box.width large_box.width
  && approx_eq normal_box.height large_box.height -> Ok ()
  | _ -> Error "Terminal layout should ignore text_size when measuring text"

let tests =
  Test.[
    case "default text measurer multiline" test_default_text_measurer_multiline;
    case "default text measurer cjk width" test_default_text_measurer_cjk_width;
    case "default text measurer emoji width" test_default_text_measurer_emoji_width;
    case "default text measurer combining sequence" test_default_text_measurer_combining_sequence;
    case
      "default text measurer respects available width"
      test_default_text_measurer_respects_available_width;
    case "word wrap breaks text into multiple lines" test_word_wrap_breaks_text_into_multiple_lines;
    case "character wrap breaks long words" test_character_wrap_breaks_long_words;
    case "no wrap preserves single line content" test_no_wrap_preserves_single_line_content;
    case "text align center offsets text" test_text_align_center_offsets_text;
    case "text align right offsets text" test_text_align_right_offsets_text;
    case "underline survives into render commands" test_underline_survives_into_render_commands;
    case
      "strikethrough survives into render commands"
      test_strikethrough_survives_into_render_commands;
    case "bold survives into render commands" test_bold_survives_into_render_commands;
    case "layout uses the configured text measurer" test_layout_uses_the_configured_text_measurer;
    case "text size is a terminal layout no op" test_text_size_is_a_terminal_layout_no_op;
  ]

let main ~args = Test.Cli.main ~name:"text" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
