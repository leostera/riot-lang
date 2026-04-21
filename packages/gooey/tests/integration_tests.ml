open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let make_config = fun ?(width = 80.0) ?(height = 24.0) () ->
  Config.make ~viewport:(Viewport.make ~width ~height) ~text_measurer:Config.default_text_measurer ()

let rectangles = fun commands ->
  List.filter_map commands
    ~fn:(fun command ->
      match command.Render.command_type with
      | Render.Rectangle { color; _ } -> Some (color, command.bounding_box)
      | _ -> None)

let texts = fun commands ->
  List.filter_map commands
    ~fn:(fun command ->
      match command.Render.command_type with
      | Render.Text { content; _ } -> Some (content, command.bounding_box)
      | _ -> None)

let text_contents = fun commands -> List.map (texts commands) ~fn:(fun (content, _) -> content)

let test_complex_nested_layout = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> bg (`rgb (50, 50, 50)) |> padding (Padding.all 2))
        [ Element.text ~style:Style.(empty |> bold) "Header" ];
      Element.row [ Element.text "A"; Element.text "B" ];
      Element.text "Footer";
    ] in
  let contents = text_contents (layout ~config:(make_config ()) ui) in
  if contents = [ "Header"; "A"; "B"; "Footer" ] then
    Ok ()
  else
    Error "Nested containers should preserve text ordering through command generation"

let test_card_pattern_has_consistent_content_offsets = fun _ctx ->
  let card = Element.column
    ~style:Style.(empty
    |> width (Fixed 20.0)
    |> bg (`rgb (255, 255, 255))
    |> border ~width:1 ~color:(`rgb (200, 200, 200)) ()
    |> padding (Padding.all 1))
    [
      Element.container ~style:Style.(empty |> height (Fixed 3.0) |> bg (`rgb (220, 220, 220))) [];
      Element.text "Title";
      Element.text "Description";
    ] in
  match texts (layout ~config:(make_config ()) card) with
  | [("Title", title);("Description", body)] when approx_eq title.x 2.0
  && approx_eq title.y 5.0
  && approx_eq body.x 2.0
  && approx_eq body.y 6.0 -> Ok ()
  | _ -> Error "Card layouts should use the same content box for nested children"

let test_grid_like_layout_uses_grow_cells = fun _ctx ->
  let grid = Element.column
    ~style:Style.(empty |> grow)
    [
      Element.row
        ~style:Style.(empty |> width Grow |> height Grow)
        [
          Element.container
            ~style:Style.(empty |> width Grow |> height Grow |> bg (`rgb (255, 0, 0)))
            [];
          Element.container
            ~style:Style.(empty |> width Grow |> height Grow |> bg (`rgb (0, 255, 0)))
            [];
        ];
      Element.row
        ~style:Style.(empty |> width Grow |> height Grow)
        [
          Element.container
            ~style:Style.(empty |> width Grow |> height Grow |> bg (`rgb (0, 0, 255)))
            [];
          Element.container
            ~style:Style.(empty |> width Grow |> height Grow |> bg (`rgb (255, 255, 0)))
            [];
        ];
    ] in
  match rectangles (layout ~config:(make_config ()) grid) with
  | [(c1, r1);(c2, r2);(c3, r3);(c4, r4)] when c1 = `rgb (255, 0, 0)
  && c2 = `rgb (0, 255, 0)
  && c3 = `rgb (0, 0, 255)
  && c4 = `rgb (255, 255, 0)
  && approx_eq r1.width 40.0
  && approx_eq r2.width 40.0
  && approx_eq r3.height 12.0
  && approx_eq r4.height 12.0 -> Ok ()
  | _ -> Error "Grow-based grids should split the viewport evenly across cells"

let test_column_alignment_can_center_page_content = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> width (Fixed 40.0) |> height (Fixed 12.0) |> align ~x:Center ~y:Middle)
    [ Element.text "Clock"; Element.text "Press q"; ] in
  match texts (layout ~config:(make_config ()) ui) with
  | [("Clock", title);("Press q", footer)] when approx_eq title.x 17.5
  && approx_eq title.y 5.0
  && approx_eq footer.x 16.5
  && approx_eq footer.y 6.0 -> Ok ()
  | _ -> Error "Container alignment should center a whole column of text"

let test_responsive_sidebar_layout = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow |> height (Fixed 10.0))
    [
      Element.container
        ~style:Style.(empty |> width (Percent 0.25) |> height Grow |> bg (`rgb (30, 30, 30)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height Grow |> bg (`rgb (240, 240, 240)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, sidebar);(_, content)] when approx_eq sidebar.width 20.0
  && approx_eq content.width 60.0
  && approx_eq content.x 20.0 -> Ok ()
  | _ -> Error "Sidebar/content layouts should combine percent and grow sizing cleanly"

let test_margin_and_gap_stack_cleanly = fun _ctx ->
  let ui = Element.column
    ~style:Style.(empty |> height Grow |> child_gap 1)
    [
      Element.container
        ~style:Style.(empty
        |> width (Fixed 10.0)
        |> height (Fixed 2.0)
        |> margin (Margin.make ~bottom:1 ())
        |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 10.0) |> height (Fixed 2.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, first);(_, second)] when approx_eq first.y 0.0 && approx_eq second.y 4.0 -> Ok ()
  | _ -> Error "Explicit margins and child_gap should both contribute to vertical placement"

let tests =
  Test.[
    case "complex nested layout" test_complex_nested_layout;
    case "card pattern has consistent content offsets" test_card_pattern_has_consistent_content_offsets;
    case "grid like layout uses grow cells" test_grid_like_layout_uses_grow_cells;
    case "column alignment can center page content" test_column_alignment_can_center_page_content;
    case "responsive sidebar layout" test_responsive_sidebar_layout;
    case "margin and gap stack cleanly" test_margin_and_gap_stack_cleanly;
  ]

let () =
  Actors.run ~main:(fun ~args -> Test.Cli.main ~name:"integration" ~tests ~args ()) ~args:Env.args ()
