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

let test_fixed_grow_fixed = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 20.0) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 15.0) |> height (Fixed 1.0) |> bg (`rgb (0, 0, 255)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, left);(_, middle);(_, right)] when approx_eq left.width 20.0
  && approx_eq middle.width 45.0
  && approx_eq right.width 15.0 -> Ok ()
  | _ -> Error "Fixed-grow-fixed arithmetic should match the remaining-space model"

let test_equal_grow_children_split_evenly = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, left);(_, right)] when approx_eq left.width 40.0 && approx_eq right.width 40.0 -> Ok ()
  | _ -> Error "Equal grow children should split the remaining width evenly"

let test_grow_children_respect_margins = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width Grow)
    [
      Element.container
        ~style:Style.(empty
        |> width Grow
        |> height (Fixed 1.0)
        |> margin (Margin.make ~right:2 ())
        |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, left);(_, right)] when approx_eq left.width 39.0
  && approx_eq right.x 41.0
  && approx_eq right.width 39.0 -> Ok ()
  | _ -> Error "Grow allocation should leave room for grow-child margins"

let test_negative_remaining_space_clamps_to_zero = fun _ctx ->
  let ui = Element.row
    ~style:Style.(empty |> width (Fixed 10.0))
    [
      Element.container
        ~style:Style.(empty |> width (Fixed 8.0) |> height (Fixed 1.0) |> bg (`rgb (255, 0, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width Grow |> height (Fixed 1.0) |> bg (`rgb (0, 255, 0)))
        [];
      Element.container
        ~style:Style.(empty |> width (Fixed 8.0) |> height (Fixed 1.0) |> bg (`rgb (0, 0, 255)))
        [];
    ] in
  match rectangles (layout ~config:(make_config ()) ui) with
  | [(_, left);(_, right)] when approx_eq left.width 8.0
  && approx_eq right.x 8.0
  && approx_eq right.width 8.0 -> Ok ()
  | _ -> Error "Grow children should clamp to zero when fixed content already overflows"

let tests =
  Test.[
    case "fixed grow fixed arithmetic" test_fixed_grow_fixed;
    case "equal grow children split evenly" test_equal_grow_children_split_evenly;
    case "grow children respect margins" test_grow_children_respect_margins;
    case "negative remaining space clamps to zero" test_negative_remaining_space_clamps_to_zero;
  ]

let () =
  Actors.run
    ~main:(fun ~args -> Test.Cli.main ~name:"grow_layout_tests" ~tests ~args)
    ~args:Env.args
    ()
