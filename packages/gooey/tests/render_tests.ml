open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let make_config = fun ?(width = 80.0) ?(height = 24.0) () -> Config.make ~viewport:(Viewport.make ~width ~height) ~text_measurer:Config.default_text_measurer ()

let find_text_commands = fun commands -> List.filter_map commands ~fn:(
  fun command ->
    match command.Render.command_type with
    | Render.Text data -> Some (data, command.bounding_box, command.z_index)
    | _ -> None
)

let find_rectangles = fun commands -> List.filter_map commands ~fn:(
  fun command ->
    match command.Render.command_type with
    | Render.Rectangle data -> Some (data, command.bounding_box, command.z_index)
    | _ -> None
)

let find_borders = fun commands -> List.filter_map commands ~fn:(
  fun command ->
    match command.Render.command_type with
    | Render.Border data -> Some (data, command.bounding_box, command.z_index)
    | _ -> None
)

let find_scissors = fun commands -> List.filter_map commands ~fn:(
  fun command ->
    match command.Render.command_type with
    | Render.ScissorStart _ | Render.ScissorEnd -> Some ()
    | _ -> None
)

let test_text_command_preserves_style_metadata = fun _ctx ->
  let ui = Element.text ~style:Style.(empty |> fg (`rgb (255, 0, 0)) |> bold |> underline) "Test" in
  match find_text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.content = "Test"; color = `rgb (255, 0, 0); weight = Style.Bold; decoration = Style.Underline; _ }, _, _) ] -> Ok ()
  | _ -> Error "Text commands should carry color, weight, and decoration"

let test_text_command_preserves_text_size_metadata = fun _ctx ->
  let ui = Element.text ~style:Style.(empty |> text_size 42) "Sized" in
  match find_text_commands (layout ~config:(make_config ()) ui) with
  | [ ({ Render.size = 42; _ }, _, _) ] -> Ok ()
  | _ -> Error "Text commands should preserve text_size as render metadata"

let test_background_requires_a_real_box = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 6.0) |> height (Fixed 2.0) |> bg (`rgb (100, 150, 200))) [] in
  match find_rectangles (layout ~config:(make_config ()) ui) with
  | [ ({ Render.color = `rgb (100, 150, 200); _ }, box, _) ] when approx_eq box.width 6.0 && approx_eq box.height 2.0 -> Ok ()
  | _ -> Error "Background rectangles should use the element's computed box"

let test_border_width_is_clamped_to_terminal_thickness = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 6.0) |> height (Fixed 3.0) |> border ~width:3 ~color:(`rgb (50, 50, 50)) ()) [] in
  match find_borders (layout ~config:(make_config ()) ui) with
  | [ ({ Render.width = { left = 1; right = 1; top = 1; bottom = 1 }; color = `rgb (50, 50, 50); _ }, _, _) ] -> Ok ()
  | _ -> Error "Terminal borders should clamp border_width to a single cell"

let test_background_and_border_both_emit = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 2.0) |> bg (`rgb (255, 255, 255)) |> border ~width:1 ~color:(`rgb (0, 0, 0)) ()) [] in
  let commands = layout ~config:(make_config ()) ui in
  if List.length (find_rectangles commands) = 1 && List.length (find_borders commands) = 1 then
    Ok ()
  else Error "Background and border should each produce a render command"

let test_equal_z_index_preserves_tree_order = fun _ctx ->
  let ui = Element.row [ Element.text "First"; Element.text "Second"; Element.text "Third" ] in
  let contents = List.map (find_text_commands (layout ~config:(make_config ()) ui)) ~fn:(
    fun ({ Render.content; _ }, _, _) -> content
  ) in
  if contents = [ "First"; "Second"; "Third" ] then
    Ok ()
  else Error "Equal z-index commands should preserve tree order"

let test_z_index_sorting_still_applies = fun _ctx ->
  let ui = Element.container [ Element.text ~style:Style.(empty |> z_index 2) "Top"; Element.text ~style:Style.(empty |> z_index 0) "Bottom"; Element.text ~style:Style.(empty |> z_index 1) "Middle" ] in
  let z_indices = List.map (layout ~config:(make_config ()) ui) ~fn:(
    fun command -> command.Render.z_index
  ) in
  if z_indices = [ 0; 1; 2 ] then
    Ok ()
  else Error "z_index should still sort commands from low to high"

let test_corner_radius_is_preserved_on_rectangles = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 2.0) |> bg (`rgb (255, 0, 0)) |> border ~radius:(Style.CornerRadius.all 8.0) ()) [] in
  match find_rectangles (layout ~config:(make_config ()) ui) with
  | [ ({ Render.corner_radius; _ }, _, _) ] when approx_eq corner_radius.top_left 8.0 && approx_eq corner_radius.bottom_right 8.0 -> Ok ()
  | _ -> Error "Corner radius should pass through to rectangle commands"

let test_empty_visual_container_emits = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 3.0) |> height (Fixed 1.0) |> bg (`rgb (1, 2, 3))) [] in
  if List.length (layout ~config:(make_config ()) ui) = 1 then
    Ok ()
  else Error "A visual empty container should still emit its own render command"

let test_empty_non_visual_container_emits_nothing = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 3.0) |> height (Fixed 1.0)) [] in
  if layout ~config:(make_config ()) ui = [] then
    Ok ()
  else Error "A non-visual empty container should emit nothing"

let test_parent_background_and_child_text_both_appear = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 8.0) |> height (Fixed 3.0) |> bg (`rgb (30, 30, 30))) [ Element.text "Hi" ] in
  let commands = layout ~config:(make_config ()) ui in
  if List.length (find_rectangles commands) = 1 && List.length (find_text_commands commands) = 1 then
    Ok ()
  else Error "Parent visuals and child content should both survive command generation"

let test_multiline_text_emits_one_text_command_per_line = fun _ctx ->
  let commands = layout ~config:(make_config ()) (Element.text "A\nB\nC") in
  let contents = List.map (find_text_commands commands) ~fn:(
    fun ({ Render.content; _ }, _, _) -> content
  ) in
  if contents = [ "A"; "B"; "C" ] then
    Ok ()
  else Error "Multiline text should render as one command per line"

let test_custom_commands_pass_through = fun _ctx ->
  let ui = Element.custom ~measure:(
    fun ~constraints:_ -> Viewport.make ~width:4.0 ~height:1.0
  ) ~render:(
    fun box -> [ { Render.bounding_box = box; command_type = Render.Custom { data = "demo" }; z_index = 3 } ]
  ) () in
  match layout ~config:(make_config ()) ui with
  | [ { Render.command_type = Render.Custom { data = "demo" }; z_index = 3; _ } ] -> Ok ()
  | _ -> Error "Custom render commands should pass through untouched"

let test_clipped_container_emits_scissor_commands = fun _ctx ->
  let ui = Element.container ~style:Style.(empty |> width (Fixed 4.0) |> height (Fixed 2.0) |> clip) [ Element.text "abcdef" ] in
  let scissors = find_scissors (layout ~config:(make_config ()) ui) in
  if List.length scissors = 2 then
    Ok ()
  else Error "Clipped containers should emit scissor commands around their children"

let tests = Test.[
  case "text command preserves style metadata" test_text_command_preserves_style_metadata;
  case "text command preserves text size metadata" test_text_command_preserves_text_size_metadata;
  case "background requires a real box" test_background_requires_a_real_box;
  case "border width is clamped to terminal thickness" test_border_width_is_clamped_to_terminal_thickness;
  case "background and border both emit" test_background_and_border_both_emit;
  case "equal z-index preserves tree order" test_equal_z_index_preserves_tree_order;
  case "z-index sorting still applies" test_z_index_sorting_still_applies;
  case "corner radius is preserved on rectangles" test_corner_radius_is_preserved_on_rectangles;
  case "empty visual container emits" test_empty_visual_container_emits;
  case "empty non-visual container emits nothing" test_empty_non_visual_container_emits_nothing;
  case "parent background and child text both appear" test_parent_background_and_child_text_both_appear;
  case "multiline text emits one text command per line" test_multiline_text_emits_one_text_command_per_line;
  case "custom commands pass through" test_custom_commands_pass_through;
  case "clipped container emits scissor commands" test_clipped_container_emits_scissor_commands;
]

let main ~args = Test.Cli.main ~name:"render" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
