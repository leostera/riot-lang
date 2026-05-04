open Std

module Test = Std.Test

let test_color_make_long_hex = fun _ctx ->
  match Tty.Color.make "#FF0080" with
  | Tty.Color.RGB (255, 0, 128) -> Ok ()
  | value -> Error ("Expected RGB(255,0,128), got " ^ Tty.Color.to_string value)

let test_color_make_long_hex_lowercase = fun _ctx ->
  match Tty.Color.make "#00ff7f" with
  | Tty.Color.RGB (0, 255, 127) -> Ok ()
  | value -> Error ("Expected RGB(0,255,127), got " ^ Tty.Color.to_string value)

let test_color_make_short_hex = fun _ctx ->
  match Tty.Color.make "#F0A" with
  | Tty.Color.RGB (255, 0, 170) -> Ok ()
  | value -> Error ("Expected RGB(255,0,170), got " ^ Tty.Color.to_string value)

let test_color_make_short_hex_lowercase = fun _ctx ->
  match Tty.Color.make "#0f8" with
  | Tty.Color.RGB (0, 255, 136) -> Ok ()
  | value -> Error ("Expected RGB(0,255,136), got " ^ Tty.Color.to_string value)

let test_color_make_invalid_length_rejected = fun _ctx ->
  try
    let _ = Tty.Color.make "#12345" in
    Error "Expected invalid hex length to be rejected"
  with
  | Tty.Color.Invalid_color "#12345" -> Ok ()
  | _ -> Error "Expected Invalid_color for invalid hex length"

let test_color_make_invalid_hex_rejected = fun _ctx ->
  try
    let _ = Tty.Color.make "#GG0000" in
    Error "Expected invalid hex digit to be rejected"
  with
  | Tty.Color.Invalid_color_param "GG" -> Ok ()
  | _ -> Error "Expected Invalid_color_param for invalid hex digits"

let test_color_ansi_accepts_bounds = fun _ctx ->
  match (Tty.Color.ansi 0, Tty.Color.ansi 15) with
  | (Tty.Color.ANSI 0, Tty.Color.ANSI 15) -> Ok ()
  | _ -> Error "Expected ansi color bounds 0 and 15 to be accepted"

let test_color_ansi_rejects_negative = fun _ctx ->
  try
    let _ = Tty.Color.ansi (-1) in
    Error "Expected ansi -1 to be rejected"
  with
  | Tty.Color.Invalid_color_num ("ansi", -1) -> Ok ()
  | _ -> Error "Expected Invalid_color_num for negative ansi input"

let test_color_ansi_range_validation = fun _ctx ->
  try
    let _ = Tty.Color.ansi 16 in
    Error "Expected ansi 16 to be rejected"
  with
  | Tty.Color.Invalid_color_num ("ansi", 16) -> Ok ()
  | _ -> Error "Expected Invalid_color_num for ansi range overflow"

let test_color_ansi256_range_validation = fun _ctx ->
  try
    let _ = Tty.Color.ansi256 256 in
    Error "Expected ansi256 256 to be rejected"
  with
  | Tty.Color.Invalid_color_num ("ansi256", 256) -> Ok ()
  | _ -> Error "Expected Invalid_color_num for ansi256 range overflow"

let test_color_ansi256_rejects_negative = fun _ctx ->
  try
    let _ = Tty.Color.ansi256 (-1) in
    Error "Expected ansi256 -1 to be rejected"
  with
  | Tty.Color.Invalid_color_num ("ansi256", -1) -> Ok ()
  | _ -> Error "Expected Invalid_color_num for negative ansi256 input"

let test_color_ansi256_accepts_bounds = fun _ctx ->
  match (Tty.Color.ansi256 0, Tty.Color.ansi256 255) with
  | (Tty.Color.ANSI256 0, Tty.Color.ANSI256 255) -> Ok ()
  | _ -> Error "Expected ansi256 bounds 0 and 255 to be accepted"

let test_color_of_rgb_clamps = fun _ctx ->
  match Tty.Color.from_rgb (300, (-10), 128) with
  | Tty.Color.RGB (255, 0, 128) -> Ok ()
  | value -> Error ("Expected clamped RGB(255,0,128), got " ^ Tty.Color.to_string value)

let test_color_to_escape_seq = fun _ctx ->
  let rgb =
    Tty.Color.make "#010203"
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  let ansi =
    Tty.Color.ansi 4
    |> Tty.Color.to_escape_seq ~mode:`bg
  in
  let ansi256 =
    Tty.Color.ansi256 196
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  if rgb = "38;2;1;2;3" && ansi = "44" && ansi256 = "38;5;196" then
    Ok ()
  else
    Error ("Unexpected color escape sequences: rgb=" ^ rgb ^ " ansi=" ^ ansi ^ " ansi256=" ^ ansi256)

let test_color_to_escape_seq_bright_ansi = fun _ctx ->
  let fg =
    Tty.Color.ansi 9
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  let bg =
    Tty.Color.ansi 12
    |> Tty.Color.to_escape_seq ~mode:`bg
  in
  if fg = "91" && bg = "104" then
    Ok ()
  else
    Error ("Unexpected bright ANSI escape sequences: fg=" ^ fg ^ " bg=" ^ bg)

let test_color_to_escape_seq_basic_ansi = fun _ctx ->
  let fg =
    Tty.Color.ansi 1
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  let bg =
    Tty.Color.ansi 4
    |> Tty.Color.to_escape_seq ~mode:`bg
  in
  if fg = "31" && bg = "44" then
    Ok ()
  else
    Error ("Unexpected basic ANSI escape sequences: fg=" ^ fg ^ " bg=" ^ bg)

let test_color_to_escape_seq_rgb_modes = fun _ctx ->
  let fg =
    Tty.Color.from_rgb (1, 2, 3)
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  let bg =
    Tty.Color.from_rgb (1, 2, 3)
    |> Tty.Color.to_escape_seq ~mode:`bg
  in
  if fg = "38;2;1;2;3" && bg = "48;2;1;2;3" then
    Ok ()
  else
    Error ("Unexpected RGB escape sequences: fg=" ^ fg ^ " bg=" ^ bg)

let test_color_to_escape_seq_ansi256_modes = fun _ctx ->
  let fg =
    Tty.Color.ansi256 196
    |> Tty.Color.to_escape_seq ~mode:`fg
  in
  let bg =
    Tty.Color.ansi256 46
    |> Tty.Color.to_escape_seq ~mode:`bg
  in
  if fg = "38;5;196" && bg = "48;5;46" then
    Ok ()
  else
    Error ("Unexpected ANSI256 escape sequences: fg=" ^ fg ^ " bg=" ^ bg)

let test_color_to_escape_seq_no_color = fun _ctx ->
  let value = Tty.Color.to_escape_seq ~mode:`fg Tty.Color.no_color in
  if value = "" then
    Ok ()
  else
    Error ("Expected no_color to emit no sequence, got " ^ value)

let test_style_default_escape_seq_is_empty = fun _ctx ->
  if String.equal (Tty.Style.to_escape_seq Tty.Style.default) "" then
    Ok ()
  else
    Error "Expected default style escape sequence to be empty"

let test_style_default_is_noop = fun _ctx ->
  let styled = Tty.Style.styled Tty.Style.default "plain" in
  if styled = "plain" then
    Ok ()
  else
    Error ("Expected default style to leave text unchanged, got " ^ styled)

let test_style_styled_wraps_text = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.bold
    |> Tty.Style.underline
    |> Tty.Style.fg (Tty.Color.make "#FF0000")
    |> Tty.Style.bg (Tty.Color.ansi 4)
  in
  let seq = Tty.Style.to_escape_seq style in
  let styled = Tty.Style.styled style "hi" in
  if seq = "1;4;38;2;255;0;0;44" && styled = "\x1b[1;4;38;2;255;0;0;44mhi\x1b[0m" then
    Ok ()
  else
    Error ("Unexpected styled rendering: seq=" ^ seq ^ " styled=" ^ styled)

let test_style_attribute_order_is_deterministic = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.underline
    |> Tty.Style.italic
    |> Tty.Style.bold
  in
  let seq = Tty.Style.to_escape_seq style in
  if seq = "1;3;4" then
    Ok ()
  else
    Error ("Expected deterministic attribute order, got " ^ seq)

let test_style_fg_no_color_is_noop = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.fg Tty.Color.no_color
  in
  if
    String.equal (Tty.Style.to_escape_seq style) ""
    && String.equal (Tty.Style.styled style "hello") "hello"
  then
    Ok ()
  else
    Error "Expected no_color foreground style to be a no-op"

let test_style_ansi_foreground_render = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.fg (Tty.Color.ansi 1)
  in
  let styled = Tty.Style.styled style "hi" in
  if styled = "\x1b[31mhi\x1b[0m" then
    Ok ()
  else
    Error ("Expected ANSI foreground rendering, got " ^ styled)

let test_style_ansi_background_render = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.bg (Tty.Color.ansi 4)
  in
  let styled = Tty.Style.styled style "hi" in
  if styled = "\x1b[44mhi\x1b[0m" then
    Ok ()
  else
    Error ("Expected ANSI background rendering, got " ^ styled)

let test_style_ansi256_render = fun _ctx ->
  let style =
    Tty.Style.default
    |> Tty.Style.fg (Tty.Color.ansi256 196)
    |> Tty.Style.bg (Tty.Color.ansi256 46)
  in
  let styled = Tty.Style.styled style "hi" in
  if styled = "\x1b[38;5;196;48;5;46mhi\x1b[0m" then
    Ok ()
  else
    Error ("Expected ANSI256 rendering, got " ^ styled)

let test_style_empty_string_policy = fun _ctx ->
  let styled = Tty.Style.styled (Tty.Style.bold Tty.Style.default) "" in
  if styled = "\x1b[1m\x1b[0m" then
    Ok ()
  else
    Error ("Expected styled empty string to preserve wrapper policy, got " ^ styled)

let test_style_nested_policy = fun _ctx ->
  let inner = Tty.Style.styled (Tty.Style.fg (Tty.Color.ansi 1) Tty.Style.default) "inner" in
  let outer = Tty.Style.styled (Tty.Style.bold Tty.Style.default) ("before " ^ inner) in
  if outer = "\x1b[1mbefore \x1b[31minner\x1b[0m\x1b[0m" then
    Ok ()
  else
    Error ("Unexpected nested styling policy: " ^ outer)

let test_style_preserves_unicode_width = fun _ctx ->
  let plain = "Cafe\u{0301} 🙂" in
  let style =
    Tty.Style.default
    |> Tty.Style.bold
    |> Tty.Style.fg (Tty.Color.make "#FF0000")
  in
  let styled = Tty.Style.styled style plain in
  if Int.equal (Tty.Escape_seq.width styled) (String.width plain) then
    Ok ()
  else
    Error "Expected styling to preserve displayed width for unicode text"

let test_input_parse_csi_arrow = fun _ctx ->
  match Tty.Input.parse_escape "\x1b[A" with
  | Some event when Tty.Input.event_to_string event = "up" -> Ok ()
  | Some event -> Error ("Expected up, got " ^ Tty.Input.event_to_string event)
  | None -> Error "Expected parsed up-arrow event"

let test_input_parse_modified_arrow = fun _ctx ->
  match Tty.Input.parse_escape "\x1b[1;5A" with
  | Some event when Tty.Input.event_to_string event = "ctrl+up" -> Ok ()
  | Some event -> Error ("Expected ctrl+up, got " ^ Tty.Input.event_to_string event)
  | None -> Error "Expected parsed modified up-arrow event"

let test_input_parse_ss3_function_key = fun _ctx ->
  match Tty.Input.parse_escape "\x1bOP" with
  | Some event when Tty.Input.event_to_string event = "f1" -> Ok ()
  | Some event -> Error ("Expected f1, got " ^ Tty.Input.event_to_string event)
  | None -> Error "Expected parsed SS3 F1 event"

let test_input_parse_focus_events = fun _ctx ->
  match (Tty.Input.parse_escape "\x1b[I", Tty.Input.parse_escape "\x1b[O") with
  | (Some focus_in, Some focus_out) when Tty.Input.event_to_string focus_in = "focus-gained"
  && Tty.Input.event_to_string focus_out = "focus-lost" -> Ok ()
  | _ -> Error "Expected focus gained and focus lost events"

let test_input_parse_mouse_press = fun _ctx ->
  match Tty.Input.parse_escape "\x1b[<0;10;20M" with
  | Some (`Mouse {
    button = Tty.Input.Left;
    action = Tty.Input.Mouse_press;
    x = 10;
    y = 20;
    modifiers = [];
  }) ->
      Ok ()
  | Some event -> Error ("Expected left mouse press, got " ^ Tty.Input.event_to_string event)
  | None -> Error "Expected parsed mouse press event"

let test_input_parse_mouse_release_with_modifiers = fun _ctx ->
  match Tty.Input.parse_escape "\x1b[<20;7;9m" with
  | Some (`Mouse {
    button = Tty.Input.Left;
    action = Tty.Input.Mouse_release;
    x = 7;
    y = 9;
    modifiers = [ Tty.Input.Shift; Tty.Input.Ctrl ];
  }) ->
      Ok ()
  | Some event -> Error ("Expected modified mouse release, got " ^ Tty.Input.event_to_string event)
  | None -> Error "Expected parsed mouse release event"

let test_input_parse_home_end_variants = fun _ctx ->
  match (
    Tty.Input.parse_escape "\x1b[H",
    Tty.Input.parse_escape "\x1b[F",
    Tty.Input.parse_escape "\x1b[1~",
    Tty.Input.parse_escape "\x1b[4~"
  ) with
  | (Some home, Some ending, Some legacy_home, Some legacy_end) when Tty.Input.event_to_string home
  = "home"
  && Tty.Input.event_to_string ending = "end"
  && Tty.Input.event_to_string legacy_home = "home"
  && Tty.Input.event_to_string legacy_end = "end" -> Ok ()
  | _ -> Error "Expected home/end variants to parse"

let test_input_parse_insert_delete_paging = fun _ctx ->
  match (
    Tty.Input.parse_escape "\x1b[2~",
    Tty.Input.parse_escape "\x1b[3~",
    Tty.Input.parse_escape "\x1b[5~",
    Tty.Input.parse_escape "\x1b[6~"
  ) with
  | (Some insert, Some delete, Some page_up, Some page_down) when Tty.Input.event_to_string insert
  = "insert"
  && Tty.Input.event_to_string delete = "delete"
  && Tty.Input.event_to_string page_up = "pageup"
  && Tty.Input.event_to_string page_down = "pagedown" -> Ok ()
  | _ -> Error "Expected insert/delete/page navigation variants to parse"

let test_input_event_to_string_text = fun _ctx ->
  let rendered = Tty.Input.event_to_string (`Text "🙂") in
  if rendered = "text(\"\\240\\159\\153\\130\")" then
    Ok ()
  else
    Error ("Expected text event rendering, got " ^ rendered)

let test_input_event_to_string_repeat = fun _ctx ->
  let event = `Key {
    Tty.Input.code = Tty.Input.Char 'x';
    modifiers = [ Tty.Input.Alt ];
    kind = Tty.Input.Repeat;
  }
  in
  let rendered = Tty.Input.event_to_string event in
  if rendered = "alt+x:repeat" then
    Ok ()
  else
    Error ("Expected alt+x:repeat, got " ^ rendered)

let test_tty_make_self_equal = fun _ctx ->
  match Tty.make () with
  | Ok tty ->
      if Tty.equal tty tty then
        Ok ()
      else
        Error "Expected a tty value to be equal to itself"
  | Error _ -> Ok ()

let test_tty_to_string_has_prefix = fun _ctx ->
  match Tty.make () with
  | Ok tty ->
      let rendered = Tty.to_string tty in
      if String.starts_with ~prefix:"TTY { size=" rendered then
        Ok ()
      else
        Error ("Expected tty string representation, got " ^ rendered)
  | Error _ -> Ok ()

let test_size_to_string = fun _ctx ->
  let rendered = Tty.Size.to_string Tty.Size.{ rows = 20; cols = 80 } in
  if rendered = "{ rows = 20; cols = 80 }" then
    Ok ()
  else
    Error ("Unexpected size rendering: " ^ rendered)

let tests =
  Test.[
    case "color_make_long_hex" test_color_make_long_hex;
    case "color_make_long_hex_lowercase" test_color_make_long_hex_lowercase;
    case "color_make_short_hex" test_color_make_short_hex;
    case "color_make_short_hex_lowercase" test_color_make_short_hex_lowercase;
    case "color_make_invalid_length_rejected" test_color_make_invalid_length_rejected;
    case "color_make_invalid_hex_rejected" test_color_make_invalid_hex_rejected;
    case "color_ansi_accepts_bounds" test_color_ansi_accepts_bounds;
    case "color_ansi_rejects_negative" test_color_ansi_rejects_negative;
    case "color_ansi_range_validation" test_color_ansi_range_validation;
    case "color_ansi256_rejects_negative" test_color_ansi256_rejects_negative;
    case "color_ansi256_range_validation" test_color_ansi256_range_validation;
    case "color_ansi256_accepts_bounds" test_color_ansi256_accepts_bounds;
    case "color_of_rgb_clamps" test_color_of_rgb_clamps;
    case "color_to_escape_seq" test_color_to_escape_seq;
    case "color_to_escape_seq_bright_ansi" test_color_to_escape_seq_bright_ansi;
    case "color_to_escape_seq_basic_ansi" test_color_to_escape_seq_basic_ansi;
    case "color_to_escape_seq_rgb_modes" test_color_to_escape_seq_rgb_modes;
    case "color_to_escape_seq_ansi256_modes" test_color_to_escape_seq_ansi256_modes;
    case "color_to_escape_seq_no_color" test_color_to_escape_seq_no_color;
    case "style_default_escape_seq_is_empty" test_style_default_escape_seq_is_empty;
    case "style_default_is_noop" test_style_default_is_noop;
    case "style_styled_wraps_text" test_style_styled_wraps_text;
    case "style_attribute_order_is_deterministic" test_style_attribute_order_is_deterministic;
    case "style_fg_no_color_is_noop" test_style_fg_no_color_is_noop;
    case "style_ansi_foreground_render" test_style_ansi_foreground_render;
    case "style_ansi_background_render" test_style_ansi_background_render;
    case "style_ansi256_render" test_style_ansi256_render;
    case "style_empty_string_policy" test_style_empty_string_policy;
    case "style_nested_policy" test_style_nested_policy;
    case "style_preserves_unicode_width" test_style_preserves_unicode_width;
    case "input_parse_csi_arrow" test_input_parse_csi_arrow;
    case "input_parse_modified_arrow" test_input_parse_modified_arrow;
    case "input_parse_ss3_function_key" test_input_parse_ss3_function_key;
    case "input_parse_focus_events" test_input_parse_focus_events;
    case "input_parse_mouse_press" test_input_parse_mouse_press;
    case "input_parse_mouse_release_with_modifiers" test_input_parse_mouse_release_with_modifiers;
    case "input_parse_home_end_variants" test_input_parse_home_end_variants;
    case "input_parse_insert_delete_paging" test_input_parse_insert_delete_paging;
    case "input_event_to_string_text" test_input_event_to_string_text;
    case "input_event_to_string_repeat" test_input_event_to_string_repeat;
    case "tty_make_self_equal" test_tty_make_self_equal;
    case "tty_to_string_has_prefix" test_tty_to_string_has_prefix;
    case "size_to_string" test_size_to_string;
  ]

let main ~args = Test.Cli.main ~name:"tty_pure_modules" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
