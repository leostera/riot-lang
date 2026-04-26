open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let test_empty_style = fun _ctx ->
  let style = Style.empty in
  if
    style.direction = Style.LeftToRight
    && style.grow_weight = 1.0
    && style.child_gap = 0
    && style.overflow = Style.Visible
    && style.text_wrap = Style.Words
    && style.text_align = Style.TextLeft
    && style.text_decoration = Style.NoDecoration
  then
    Ok ()
  else
    Error "Style.empty should expose the documented defaults"

let test_size_builder_updates_both_axes = fun _ctx ->
  let style =
    Style.(empty
    |> size ~width:(Fixed 12.0) ~height:Grow)
  in
  if style.sizing.width = Style.Fixed 12.0 && style.sizing.height = Style.Grow then
    Ok ()
  else
    Error "Style.size should update width and height together"

let test_min_max_builders = fun _ctx ->
  let style =
    Style.(empty
    |> min_width 2.0
    |> max_width 8.0
    |> min_height 3.0
    |> max_height 9.0)
  in
  if
    style.sizing.min_width = Some 2.0
    && style.sizing.max_width = Some 8.0
    && style.sizing.min_height = Some 3.0
    && style.sizing.max_height = Some 9.0
  then
    Ok ()
  else
    Error "min/max builders should store all sizing clamps"

let test_padding_helpers = fun _ctx ->
  let p1 = Style.Padding.all 4 in
  let p2 = Style.Padding.symmetric ~h:3 ~v:2 in
  let p3 = Style.Padding.make ~left:1 ~right:2 ~top:3 ~bottom:4 () in
  if
    p1.left = 4
    && p1.bottom = 4
    && p2.left = 3
    && p2.top = 2
    && p3.left = 1
    && p3.right = 2
    && p3.top = 3
    && p3.bottom = 4
  then
    Ok ()
  else
    Error "Padding helpers should populate every side correctly"

let test_margin_helpers = fun _ctx ->
  let m1 = Style.Margin.all 5 in
  let m2 = Style.Margin.symmetric ~h:7 ~v:2 in
  if m1.left = 5 && m1.bottom = 5 && m2.left = 7 && m2.top = 2 then
    Ok ()
  else
    Error "Margin helpers should populate every side correctly"

let test_direction_helpers = fun _ctx ->
  let row =
    Style.(empty
    |> row)
  in
  let column =
    Style.(empty
    |> column)
  in
  if row.direction = Style.LeftToRight && column.direction = Style.TopToBottom then
    Ok ()
  else
    Error "Direction helpers should set the expected axis"

let test_grow_and_grow_weight = fun _ctx ->
  let style =
    Style.(empty
    |> grow
    |> grow_weight 2.5)
  in
  if
    style.sizing.width = Style.Grow
    && style.sizing.height = Style.Grow
    && approx_eq style.grow_weight 2.5
  then
    Ok ()
  else
    Error "grow and grow_weight should cooperate"

let test_text_style_builders = fun _ctx ->
  let style =
    Style.(empty
    |> text_wrap Character
    |> text_align TextRight
    |> strikethrough
    |> bold)
  in
  if
    style.text_wrap = Style.Character
    && style.text_align = Style.TextRight
    && style.text_decoration = Style.Strikethrough
    && style.font_weight = Style.Bold
  then
    Ok ()
  else
    Error "Text builders should store wrapping, alignment, decoration, and weight"

let test_alignment_builder = fun _ctx ->
  let style =
    Style.(empty
    |> align ~x:Center ~y:Middle)
  in
  if style.alignment.x = Style.Center && style.alignment.y = Style.Middle then
    Ok ()
  else
    Error "Style.align should store both alignment axes"

let test_overflow_builders = fun _ctx ->
  let clipped =
    Style.(empty
    |> clip)
  in
  let visible =
    Style.(empty
    |> overflow Visible)
  in
  if clipped.overflow = Style.Clip && visible.overflow = Style.Visible then
    Ok ()
  else
    Error "Overflow builders should store clipping semantics explicitly"

let test_border_builder = fun _ctx ->
  let radius = Style.CornerRadius.make ~top_left:1.0 ~bottom_right:3.0 () in
  let style =
    Style.(empty
    |> border ~width:3 ~color:(`rgb (10, 20, 30)) ~radius ())
  in
  if style.border_width = 3 && style.border_color = Some (`rgb (10, 20, 30)) && style.corner_radius
  = radius then
    Ok ()
  else
    Error "Border builder should preserve width, color, and radius"

let test_color_parsing = fun _ctx ->
  if Style.color "#f00" = (`rgb (255, 0, 0)) && Style.color "#ff0000" = (`rgb (255, 0, 0)) && Style.color
    "00ff00" = (`rgb (0, 255, 0)) then
    Ok ()
  else
    Error "Style.color should parse both short and long RGB hex forms"

let test_color_parser_integrates_with_style_api = fun _ctx ->
  let red = Style.color "#ff0000" in
  let style =
    Style.(empty
    |> fg red
    |> bg red
    |> border ~color:red ())
  in
  if
    style.foreground = Some red && style.background = Some red && style.border_color = Some red
  then
    Ok ()
  else
    Error "Parsed colors should plug into fg/bg/border without conversion"

let tests =
  Test.[
    case "empty style defaults" test_empty_style;
    case "size builder updates both axes" test_size_builder_updates_both_axes;
    case "min/max builders" test_min_max_builders;
    case "padding helpers" test_padding_helpers;
    case "margin helpers" test_margin_helpers;
    case "direction helpers" test_direction_helpers;
    case "grow and grow_weight" test_grow_and_grow_weight;
    case "text style builders" test_text_style_builders;
    case "alignment builder" test_alignment_builder;
    case "overflow builders" test_overflow_builders;
    case "border builder" test_border_builder;
    case "color parsing" test_color_parsing;
    case "color parser integrates with style api" test_color_parser_integrates_with_style_api;
  ]

let main ~args = Test.Cli.main ~name:"style" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
