open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let make_config = fun ?(width = 80.0) ?(height = 24.0) () ->
  Config.make ~viewport:(Viewport.make ~width ~height) ~text_measurer:Config.default_text_measurer ()

let text_commands = fun commands ->
  List.filter_map commands
    ~fn:(fun command ->
      match command.Render.command_type with
      | Render.Text { content; _ } -> Some (content, command.bounding_box)
      | _ -> None)

let text_contents = fun commands ->
  List.map (text_commands commands) ~fn:(fun (content, _) -> content)

let rectangle_boxes = fun commands ->
  List.filter_map commands
    ~fn:(fun command ->
      match command.Render.command_type with
      | Render.Rectangle _ -> Some command.bounding_box
      | _ -> None)

let find_text_box = fun commands expected ->
  text_commands commands
  |> List.find ~fn:(fun (content, _) -> content = expected)
  |> Option.map ~fn:(fun (_, box) -> box)

let test_single_text_uses_terminal_cell_measurement = fun _ctx ->
  let commands = layout ~config:(make_config ()) (Element.text "Hello") in
  match commands with
  | [ { Render.command_type=Render.Text { content="Hello"; _ }; bounding_box; _ } ] when approx_eq
    bounding_box.width
    5.0
  && approx_eq bounding_box.height 1.0 -> Ok ()
  | _ -> Error "Single-line text should measure to visible terminal cells"

let test_multiline_text_uses_longest_line_and_line_count = fun _ctx ->
  let commands = layout ~config:(make_config ()) (Element.text "Hi\nthere") in
  let boxes = text_commands commands in
  match boxes with
  | [("Hi", first);("there", second)] when approx_eq first.width 2.0
  && approx_eq second.width 5.0
  && approx_eq first.y 0.0
  && approx_eq second.y 1.0 -> Ok ()
  | _ -> Error "Multiline text should emit one line command per rendered line"

let test_padding_offsets_first_child = fun _ctx ->
  let ui = Element.container
    ~style:Style.(empty |> padding (Style.Padding.make ~left:3 ~top:2 ()))
    [ Element.text "Hi" ] in
  match find_text_box (layout ~config:(make_config ()) ui) "Hi" with
  | Some box when approx_eq box.x 3.0 && approx_eq box.y 2.0 -> Ok ()
  | _ -> Error "Padding should offset children by its exact per-side values"

let test_row_preserves_child_order = fun _ctx ->
  let ui = Element.row [ Element.text "A"; Element.text "B"; Element.text "C" ] in
  let texts = text_contents (layout ~config:(make_config ()) ui) in
  if texts = [ "A"; "B"; "C" ] then
    Ok ()
  else
    Error "Rows should preserve child order"

let test_column_preserves_child_order = fun _ctx ->
  let ui = Element.column [ Element.text "A"; Element.text "B" ] in
  let texts = text_contents (layout ~config:(make_config ()) ui) in
  if texts = [ "A"; "B" ] then
    Ok ()
  else
    Error "Columns should preserve child order"

let test_child_gap_applies_once_between_row_children = fun _ctx ->
  let ui = Element.row ~style:Style.(empty |> child_gap 2) [ Element.text "A"; Element.text "B" ] in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [("A", first);("B", second)] when approx_eq first.x 0.0 && approx_eq second.x 3.0 -> Ok ()
  | _ -> Error "Rows should add the child gap exactly once between siblings"

let test_fixed_row_positions_children_exactly = fun _ctx ->
  let ui = Element.row
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 3.0) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [first;second] when approx_eq first.x 0.0 && approx_eq second.x 3.0 -> Ok ()
  | _ -> Error "Fixed-width row children should be placed one after the other"

let test_fixed_column_positions_children_exactly = fun _ctx ->
  let ui = Element.column
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 2.0) |> height (Fixed 2.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 2.0) |> height (Fixed 3.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [first;second] when approx_eq first.y 0.0 && approx_eq second.y 2.0 -> Ok ()
  | _ -> Error "Fixed-height column children should stack exactly"

let test_percent_width_uses_parent_inner_width = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> width (Percent 0.25) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Percent 0.75) |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [left;right] when approx_eq left.width 20.0 && approx_eq right.width 60.0 -> Ok ()
  | _ -> Error "Percent sizing should resolve against the parent inner width"

let test_single_grow_child_gets_remaining_width = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 10.0) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [left;middle] when approx_eq left.width 10.0 && approx_eq middle.width 70.0 -> Ok ()
  | _ -> Error "A single grow child should consume all remaining main-axis space"

let test_weighted_grow_splits_remaining_space = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty
        |> width Grow
        |> height (Fixed 1.0)
        |> grow_weight 1.0
        |> bg (`rgb (0, 255, 0)))
        [];
      Element.container
        ~style:Style.(empty
        |> width Grow
        |> height (Fixed 1.0)
        |> grow_weight 2.0
        |> bg (`rgb (0, 0, 255)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [_fixed;grow1;grow2] when approx_eq grow1.width 20.0 && approx_eq grow2.width 40.0 -> Ok ()
  | _ -> Error "Grow widths should split proportionally by grow_weight"

let test_spacer_consumes_remaining_width_in_rows = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [ Element.text "Left"; Element.spacer (); Element.text "Right"; ] in
  match find_text_box (layout ~config:(make_config ()) ui) "Right" with
  | Some box when approx_eq box.x 75.0 -> Ok ()
  | _ -> Error "Row spacers should consume the remaining width"

let test_spacer_consumes_remaining_height_in_columns = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> height Grow)
    [ Element.text "Top"; Element.spacer (); Element.text "Bottom"; ] in
  match find_text_box (layout ~config:(make_config ()) ui) "Bottom" with
  | Some box when approx_eq box.y 23.0 -> Ok ()
  | _ -> Error "Column spacers should consume the remaining height"

let test_non_fit_parent_still_measures_fit_children = fun _ctx ->
  let ui = Element.container
    ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 4.0))
    [
      Element.row
        ~style:Style.(empty |> bg (`rgb (255, 0, 0)))
        [ Element.text "Hi"; Element.text "!" ]
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [ box ] when approx_eq box.width 3.0 && approx_eq box.height 1.0 -> Ok ()
  | _ -> Error "Fixed parents should still recursively measure fit descendants"

let test_non_fit_parent_still_measures_percent_children = fun _ctx ->
  let ui = Element.container
    ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 4.0))
    [
      Element.container
        ~style:Style.(empty |> width (Percent 0.5) |> height (Fixed 2.0) |> bg (`rgb (255, 0, 0)))
        [ Element.text "Hi" ]
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [ box ] when approx_eq box.width 10.0 && approx_eq box.height 2.0 -> Ok ()
  | _ -> Error "Fixed parents should still recursively measure percent descendants"

let test_margins_affect_sibling_spacing = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty
        |> width (Fixed 10.0)
        |> height (Fixed 1.0)
        |> margin (Style.Margin.make ~right:2 ())
        |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 5.0) |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [first;second] when approx_eq first.x 0.0 && approx_eq second.x 12.0 -> Ok ()
  | _ -> Error "Margins should contribute to sibling spacing"

let test_margins_affect_fit_parent_size = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> bg (`rgb (40, 40, 40)))
    [
      Element.container
        ~style:Style.(empty
        |> width (Fixed 5.0)
        |> height (Fixed 1.0)
        |> margin (Style.Margin.symmetric ~h:1 ~v:0)
        |> bg (`rgb (255, 0, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | parent :: _ when approx_eq parent.width 7.0 -> Ok ()
  | _ -> Error "Fit parents should include child margins in their measured size"

let test_alignment_centers_children_inside_extra_space = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 10.0) |> align ~x:Center ~y:Middle)
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 2.0) |> bg (`rgb (255, 0, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [ box ] when approx_eq box.x 8.0 && approx_eq box.y 4.0 -> Ok ()
  | _ -> Error "Container alignment should center children within leftover space"

let test_alignment_can_place_children_at_bottom_right = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 10.0) |> align ~x:Right ~y:Bottom)
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 2.0) |> bg (`rgb (255, 0, 0)))
        [];
    ] in
  match rectangle_boxes (layout ~config:(make_config ()) ui) with
  | [ box ] when approx_eq box.x 16.0 && approx_eq box.y 8.0 -> Ok ()
  | _ -> Error "Container alignment should support bottom-right placement"

let test_border_and_padding_define_the_content_box = fun _ctx ->
  let ui = Element.container
    ~style:Style.(empty
    |> width (Fixed 12.0)
    |> height (Fixed 5.0)
    |> border ~width:1 ~color:(`rgb (255, 255, 255)) ()
    |> padding (Style.Padding.all 1))
    [ Element.text "Hi" ] in
  match find_text_box (layout ~config:(make_config ()) ui) "Hi" with
  | Some box when approx_eq box.x 2.0 && approx_eq box.y 2.0 -> Ok ()
  | _ -> Error "Border thickness and padding should both inset child content"

let test_zero_sized_children_do_not_advance_siblings = fun _ctx ->
  let ui = Element.row
    [
      Element.text "A";
      Element.container ~style:Style.(empty |> width (Fixed 0.0) |> height (Fixed 0.0)) [];
      Element.text "B";
    ] in
  match text_commands (layout ~config:(make_config ()) ui) with
  | [("A", first);("B", second)] when approx_eq first.x 0.0 && approx_eq second.x 1.0 -> Ok ()
  | _ -> Error "Zero-sized children should not move later siblings"

let tests =
  Test.[
    case "single text uses terminal cell measurement" test_single_text_uses_terminal_cell_measurement;
    case "multiline text uses longest line and line count" test_multiline_text_uses_longest_line_and_line_count;
    case "padding offsets first child" test_padding_offsets_first_child;
    case "row preserves child order" test_row_preserves_child_order;
    case "column preserves child order" test_column_preserves_child_order;
    case "child gap applies once between row children" test_child_gap_applies_once_between_row_children;
    case "fixed row positions children exactly" test_fixed_row_positions_children_exactly;
    case "fixed column positions children exactly" test_fixed_column_positions_children_exactly;
    case "percent width uses parent inner width" test_percent_width_uses_parent_inner_width;
    case "single grow child gets remaining width" test_single_grow_child_gets_remaining_width;
    case "weighted grow splits remaining space" test_weighted_grow_splits_remaining_space;
    case "spacer consumes remaining width in rows" test_spacer_consumes_remaining_width_in_rows;
    case "spacer consumes remaining height in columns" test_spacer_consumes_remaining_height_in_columns;
    case "non-fit parent still measures fit children" test_non_fit_parent_still_measures_fit_children;
    case "non-fit parent still measures percent children" test_non_fit_parent_still_measures_percent_children;
    case "margins affect sibling spacing" test_margins_affect_sibling_spacing;
    case "margins affect fit parent size" test_margins_affect_fit_parent_size;
    case "alignment centers children inside extra space" test_alignment_centers_children_inside_extra_space;
    case "alignment can place children at bottom right" test_alignment_can_place_children_at_bottom_right;
    case "border and padding define the content box" test_border_and_padding_define_the_content_box;
    case "zero-sized children do not advance siblings" test_zero_sized_children_do_not_advance_siblings;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"layout" ~tests ~args ()) ~args:Env.args ()
