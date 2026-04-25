open Std
open Gooey

(* Static layout example: Three columns with different colors *)
let text_measurer = fun ~constraints text style -> Config.default_text_measurer ~constraints text style

let main ~args:_ =
  (* Get terminal dimensions - use default if TTY not available *)
  let size =
    match Tty.make () with
    | Ok tty -> Tty.size tty
    | Error _ -> { Tty.rows = 24; cols = 80 }
  in
  (* Create UI: Three columns (sidebar, content, sidebar) *)
  let ui = Element.row [ Element.container ~style:(Style.empty |> Style.width (Style.Fixed 20.0) |> Style.bg (`rgb (0, 0, 255)) |> Style.padding (Style.Padding.all 1)) [ Element.text ~style:(Style.empty |> Style.fg (`rgb (255, 255, 255))) "LEFT SIDEBAR" ]; Element.container ~style:(Style.empty |> Style.width Style.Grow |> Style.bg (`rgb (0, 255, 0)) |> Style.padding (Style.Padding.all 1)) [ Element.text ~style:(Style.empty |> Style.fg (`rgb (0, 0, 0))) "MAIN CONTENT (flexible)" ]; Element.container ~style:(Style.empty |> Style.width (Style.Fixed 20.0) |> Style.bg (`rgb (255, 0, 0)) |> Style.padding (Style.Padding.all 1)) [ Element.text ~style:(Style.empty |> Style.fg (`rgb (255, 255, 255))) "RIGHT" ] ] in
  (* Layout and render *)
  let viewport = Viewport.make ~width:(float_of_int size.cols) ~height:(float_of_int size.rows) in
  let config = Config.make ~viewport ~text_measurer () in
  let commands = Gooey.layout ~config ui in
  Terminal_renderer_fullscreen.render commands;
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
