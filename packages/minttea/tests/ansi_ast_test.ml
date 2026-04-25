open Std

let make_config = fun () -> Gooey.Config.make ~viewport:(Gooey.Viewport.make ~width:40.0 ~height:10.0) ~text_measurer:Gooey.Config.default_text_measurer ()

let render = fun element -> Gooey.layout ~config:(make_config ()) element |> Gooey.Terminal_renderer_inline.render_to_string

let assert_contains = fun label output expected ->
  if String.contains output expected then
    ()
  else panic (label ^ " expected rendered output to contain: " ^ expected)

let test_public_text_rendering = fun () ->
  let output = render (Minttea.Element.text "hello from minttea") in
  assert_contains "text rendering" output "hello from minttea";
  println "Minttea public text rendering works"

let test_public_style_rendering = fun () ->
  let style = Minttea.Style.empty |> Minttea.Style.fg (`rgb (255, 0, 0)) |> Minttea.Style.bold in
  let output = render (Minttea.Element.text ~style "styled") in
  assert_contains "styled text rendering" output "styled";
  assert_contains "styled text rendering" output "\x1b[";
  println "Minttea public style rendering works"

let test_public_layout_rendering = fun () ->
  let ui = Minttea.Element.column [ Minttea.Element.text "first"; Minttea.Element.text "second" ] in
  let output = render ui in
  assert_contains "layout rendering" output "first";
  assert_contains "layout rendering" output "second";
  println "Minttea public layout rendering works"

let main ~args:_ =
  test_public_text_rendering ();
  test_public_style_rendering ();
  test_public_layout_rendering ();
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
