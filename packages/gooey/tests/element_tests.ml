open Std
open Gooey

let approx_eq = fun left right -> Float.abs (left -. right) < 0.001

let test_text_element = fun _ctx ->
  match Element.text "Hello" with
  | Element.Text { content; _ } when content = "Hello" -> Ok ()
  | _ -> Error "Element.text should preserve the provided content"

let test_text_element_preserves_style = fun _ctx ->
  let style = Style.(empty |> bold |> fg (`rgb (255, 0, 0)) |> underline) in
  match Element.text ~style "Styled" with
  | Element.Text { content; style } when content = "Styled" && style.font_weight = Style.Bold && style.text_decoration = Style.Underline && style.foreground = Some (`rgb (255, 0, 0)) -> Ok ()
  | _ -> Error "Text elements should retain the full style record"

let test_container_preserves_child_order = fun _ctx ->
  let child1 = Element.text "A" in
  let child2 = Element.text "B" in
  match Element.container [ child1; child2 ] with
  | Element.Container { children; _ } when children = [ child1; child2 ] -> Ok ()
  | _ -> Error "Container children should keep insertion order"

let test_row_sets_direction_only = fun _ctx ->
  let style = Style.(empty |> width (Fixed 12.0) |> height Fit) in
  match Element.row ~style [ Element.text "A" ] with
  | Element.Container { style; _ } when style.direction = Style.LeftToRight && style.sizing.width = Style.Fixed 12.0 && style.sizing.height = Style.Fit -> Ok ()
  | _ -> Error "row should set direction without overriding caller sizing"

let test_column_sets_direction_only = fun _ctx ->
  let style = Style.(empty |> width Fit |> height (Fixed 6.0)) in
  match Element.column ~style [ Element.text "A" ] with
  | Element.Container { style; _ } when style.direction = Style.TopToBottom && style.sizing.width = Style.Fit && style.sizing.height = Style.Fixed 6.0 -> Ok ()
  | _ -> Error "column should set direction without overriding caller sizing"

let test_spacer_is_weighted_grow = fun _ctx ->
  match Element.spacer ~flex:2.0 () with
  | Element.Container { children; style } when children = [] && style.sizing.width = Style.Grow && style.sizing.height = Style.Grow && approx_eq style.grow_weight 2.0 -> Ok ()
  | _ -> Error "spacer should be an empty grow container that carries its grow weight"

let test_spacer_clamps_negative_weight = fun _ctx ->
  match Element.spacer ~flex:(-3.0) () with
  | Element.Container { style; _ } when approx_eq style.grow_weight 0.0 -> Ok ()
  | _ -> Error "spacer should clamp negative grow weights to zero"

let test_custom_preserves_callbacks = fun _ctx ->
  let measured = Viewport.make ~width:4.0 ~height:2.0 in
  let box = Geometry.Rect.make ~x:1.0 ~y:2.0 ~width:3.0 ~height:4.0 in
  let render rect = [ { Render.bounding_box = rect; command_type = Render.Custom { data = "custom" }; z_index = 7 } ] in
  match Element.custom ~measure:(
    fun ~constraints:_ -> measured
  ) ~render () with
  | Element.Custom { measure; render; _ } ->
      let commands = render box in
      if measure ~constraints:(Config.constraints ()) = measured && List.length commands = 1 then
        Ok ()
      else Error "Custom callbacks should be retained unchanged"
  | _ -> Error "Element.custom should produce the Custom variant"

let test_custom_measure_receives_constraints = fun _ctx ->
  let seen = ref None in
  let element = Element.custom ~measure:(
    fun ~constraints ->
      seen := Some constraints;
      Viewport.make ~width:3.0 ~height:1.0
  ) ~render:(
    fun _ -> []
  ) () in
  let _ = Gooey.layout ~config:(Config.make ~viewport:(Viewport.make ~width:8.0 ~height:4.0) ~text_measurer:Config.default_text_measurer ()) element in
  match !seen with
  | Some { Config.available_width = Some width; available_height = Some height } when approx_eq width 8.0 && approx_eq height 4.0 -> Ok ()
  | _ -> Error "Custom elements should receive available-space constraints during measurement"

let test_empty_element = fun _ctx ->
  match Element.empty with
  | Element.Empty -> Ok ()
  | _ -> Error "Element.empty should be the Empty variant"

let tests = Test.[
  case "text element" test_text_element;
  case "text preserves style" test_text_element_preserves_style;
  case "container preserves child order" test_container_preserves_child_order;
  case "row sets direction only" test_row_sets_direction_only;
  case "column sets direction only" test_column_sets_direction_only;
  case "spacer is weighted grow" test_spacer_is_weighted_grow;
  case "spacer clamps negative weight" test_spacer_clamps_negative_weight;
  case "custom preserves callbacks" test_custom_preserves_callbacks;
  case "custom measure receives constraints" test_custom_measure_receives_constraints;
  case "empty element" test_empty_element;
]

let main ~args = Test.Cli.main ~name:"element" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
