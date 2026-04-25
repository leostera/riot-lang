open Std
open Gooey

let rect = fun ~x ~y ~width ~height -> Geometry.Rect.make ~x ~y ~width ~height

let text_command = fun ?(z = 0) ?(size = 12) ?(weight = Style.Normal) ?(decoration = Style.NoDecoration) ?(color = `rgb (
  255,
  255,
  255
)) ~x ~y ~width content ->
  {
    Render.bounding_box = rect ~x ~y ~width ~height:1.0;
    command_type =
      Render.Text {
        content;
        color;
        size;
        weight;
        decoration;
      };
    z_index = z;
  }

let custom_command = fun ?(z = 0) ~x ~y ~width data ->
  {
    Render.bounding_box = rect ~x ~y ~width ~height:1.0;
    command_type = Render.Custom { data };
    z_index = z
  }

let scissor_start = fun ~x ~y ~width ~height ->
  {
    Render.bounding_box = rect ~x ~y ~width ~height;
    command_type = Render.ScissorStart (rect ~x ~y ~width ~height);
    z_index = 0
  }

let scissor_end = {
  Render.bounding_box = rect ~x:0.0 ~y:0.0 ~width:0.0 ~height:0.0;
  command_type = Render.ScissorEnd;
  z_index = 0
}

let test_inline_renderer_handles_multiple_custom_segments_on_one_row = fun _ctx ->
  let output = Terminal_renderer_inline.render_to_string
    [ custom_command ~x:0.0 ~y:0.0 ~width:3.0 "AAA"; custom_command ~x:3.0 ~y:0.0 ~width:3.0 "BBB"; ] in
  let visible = Tty.Escape_seq.strip output in
  if String.contains visible "AAABBB" then
    Ok ()
  else
    Error "Inline renderer should support multiple custom segments on the same row"

let test_fullscreen_renderer_handles_custom_commands = fun _ctx ->
  let output = Terminal_renderer_fullscreen.render_to_string
    [ custom_command ~x:2.0 ~y:1.0 ~width:4.0 "demo"; ] in
  if
    String.contains output "demo" && String.contains output (Tty.Escape_seq.cursor_position_seq 2 3)
  then
    Ok ()
  else
    Error "Fullscreen renderer should position and emit custom commands"

let test_inline_renderer_clips_custom_commands_to_the_bounding_box = fun _ctx ->
  let visible = Terminal_renderer_inline.render_to_string
    [ custom_command ~x:0.0 ~y:0.0 ~width:3.0 "abcdef"; ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "abc" && not (String.contains visible "abcd") then
    Ok ()
  else
    Error "Inline renderer should clip custom content to the command bounding box"

let test_fullscreen_renderer_clips_custom_commands_to_the_bounding_box = fun _ctx ->
  let visible = Terminal_renderer_fullscreen.render_to_string
    [ custom_command ~x:0.0 ~y:0.0 ~width:3.0 "abcdef"; ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "abc" && not (String.contains visible "abcd") then
    Ok ()
  else
    Error "Fullscreen renderer should clip custom content to the command bounding box"

let test_inline_renderer_emits_bold_and_underline_sequences = fun _ctx ->
  let output = Terminal_renderer_inline.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:4.0 ~weight:Style.Bold ~decoration:Style.Underline "text"; ] in
  if
    String.contains output Tty.Escape_seq.bold_seq && String.contains output Tty.Escape_seq.underline_seq
  then
    Ok ()
  else
    Error "Inline renderer should preserve bold and underline formatting"

let test_fullscreen_renderer_emits_strikethrough_sequences = fun _ctx ->
  let output = Terminal_renderer_fullscreen.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:6.0 ~decoration:Style.Strikethrough "strike"; ] in
  if String.contains output Tty.Escape_seq.cross_out_seq then
    Ok ()
  else
    Error "Fullscreen renderer should preserve strikethrough formatting"

let test_inline_renderer_keeps_unicode_display_width = fun _ctx ->
  let output = Terminal_renderer_inline.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:2.0 "中"; text_command ~x:2.0 ~y:0.0 ~width:2.0 "👍"; ] in
  let visible = Tty.Escape_seq.strip output in
  if Tty.Escape_seq.width visible >= 4 then
    Ok ()
  else
    Error "Inline renderer should keep wide grapheme display widths intact"

let test_inline_renderer_clips_text_to_the_bounding_box = fun _ctx ->
  let visible = Terminal_renderer_inline.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:3.0 "abcdef"; ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "abc" && not (String.contains visible "abcd") then
    Ok ()
  else
    Error "Inline renderer should clip text to the command bounding box"

let test_fullscreen_renderer_clips_text_to_the_bounding_box = fun _ctx ->
  let visible = Terminal_renderer_fullscreen.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:3.0 "abcdef"; ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "abc" && not (String.contains visible "abcd") then
    Ok ()
  else
    Error "Fullscreen renderer should clip text to the command bounding box"

let test_inline_renderer_respects_scissor_for_text = fun _ctx ->
  let visible = Terminal_renderer_inline.render_to_string
    [
      scissor_start ~x:1.0 ~y:0.0 ~width:2.0 ~height:1.0;
      text_command ~x:0.0 ~y:0.0 ~width:4.0 "abcd";
      scissor_end;
    ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "bc" && not (String.contains visible "abc") then
    Ok ()
  else
    Error "Inline renderer should clip text to the active scissor box"

let test_fullscreen_renderer_respects_scissor_for_text = fun _ctx ->
  let visible = Terminal_renderer_fullscreen.render_to_string
    [
      scissor_start ~x:1.0 ~y:0.0 ~width:2.0 ~height:1.0;
      text_command ~x:0.0 ~y:0.0 ~width:4.0 "abcd";
      scissor_end;
    ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "bc" && not (String.contains visible "abc") then
    Ok ()
  else
    Error "Fullscreen renderer should clip text to the active scissor box"

let test_inline_renderer_respects_scissor_for_custom = fun _ctx ->
  let visible = Terminal_renderer_inline.render_to_string
    [
      scissor_start ~x:1.0 ~y:0.0 ~width:2.0 ~height:1.0;
      custom_command ~x:0.0 ~y:0.0 ~width:4.0 "abcd";
      scissor_end;
    ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "bc" && not (String.contains visible "abc") then
    Ok ()
  else
    Error "Inline renderer should clip custom content to the active scissor box"

let test_fullscreen_renderer_respects_scissor_for_custom = fun _ctx ->
  let visible = Terminal_renderer_fullscreen.render_to_string
    [
      scissor_start ~x:1.0 ~y:0.0 ~width:2.0 ~height:1.0;
      custom_command ~x:0.0 ~y:0.0 ~width:4.0 "abcd";
      scissor_end;
    ]
  |> Tty.Escape_seq.strip in
  if String.contains visible "bc" && not (String.contains visible "abc") then
    Ok ()
  else
    Error "Fullscreen renderer should clip custom content to the active scissor box"

let test_terminal_renderers_treat_text_size_as_a_render_no_op = fun _ctx ->
  let small_inline = Terminal_renderer_inline.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:4.0 ~size:12 "text" ]
  |> Tty.Escape_seq.strip in
  let large_inline = Terminal_renderer_inline.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:4.0 ~size:72 "text" ]
  |> Tty.Escape_seq.strip in
  let small_fullscreen = Terminal_renderer_fullscreen.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:4.0 ~size:12 "text" ]
  |> Tty.Escape_seq.strip in
  let large_fullscreen = Terminal_renderer_fullscreen.render_to_string
    [ text_command ~x:0.0 ~y:0.0 ~width:4.0 ~size:72 "text" ]
  |> Tty.Escape_seq.strip in
  if small_inline = large_inline && small_fullscreen = large_fullscreen then
    Ok ()
  else
    Error "Built-in terminal renderers should treat text_size as a metadata-only field"

let tests =
  Test.[
    case "inline renderer handles multiple custom segments on one row" test_inline_renderer_handles_multiple_custom_segments_on_one_row;
    case "fullscreen renderer handles custom commands" test_fullscreen_renderer_handles_custom_commands;
    case "inline renderer clips custom commands to the bounding box" test_inline_renderer_clips_custom_commands_to_the_bounding_box;
    case "fullscreen renderer clips custom commands to the bounding box" test_fullscreen_renderer_clips_custom_commands_to_the_bounding_box;
    case "inline renderer emits bold and underline sequences" test_inline_renderer_emits_bold_and_underline_sequences;
    case "fullscreen renderer emits strikethrough sequences" test_fullscreen_renderer_emits_strikethrough_sequences;
    case "inline renderer keeps unicode display width" test_inline_renderer_keeps_unicode_display_width;
    case "inline renderer clips text to the bounding box" test_inline_renderer_clips_text_to_the_bounding_box;
    case "fullscreen renderer clips text to the bounding box" test_fullscreen_renderer_clips_text_to_the_bounding_box;
    case "inline renderer respects scissor for text" test_inline_renderer_respects_scissor_for_text;
    case "fullscreen renderer respects scissor for text" test_fullscreen_renderer_respects_scissor_for_text;
    case "inline renderer respects scissor for custom" test_inline_renderer_respects_scissor_for_custom;
    case "fullscreen renderer respects scissor for custom" test_fullscreen_renderer_respects_scissor_for_custom;
    case "terminal renderers treat text size as a render no op" test_terminal_renderers_treat_text_size_as_a_render_no_op;
  ]

let main ~args = Test.Cli.main ~name:"renderer_backends" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
