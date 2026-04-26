open Std
open Gooey

(* Static layout example: Three rows with different colors *)

let text_measurer = fun ~constraints text style ->
  Config.default_text_measurer
    ~constraints
    text
    style

let main ~args:_ =
  (* Get terminal dimensions *)
  let tty =
    Tty.make ()
    |> Result.unwrap
  in
  let size = Tty.size tty in
  (* Create UI: Three rows (header, content, footer) *)
  let ui =
    Element.column
      [
        Element.container
          ~style:(
            Style.empty
            |> Style.height (Style.Fixed 3.0)
            |> Style.bg (`rgb (0, 0, 255))
            |> Style.padding (Style.Padding.symmetric ~h:2 ~v:0)
          )
          [
            Element.text
              ~style:(
                Style.empty
                |> Style.fg (`rgb (255, 255, 255))
              )
              "HEADER (fixed 3 lines)";
          ];
        Element.container
          ~style:(
            Style.empty
            |> Style.height Style.Grow
            |> Style.bg (`rgb (0, 255, 0))
            |> Style.padding (Style.Padding.symmetric ~h:2 ~v:0)
          )
          [
            Element.text
              ~style:(
                Style.empty
                |> Style.fg (`rgb (0, 0, 0))
              )
              "CONTENT (flexible - fills remaining space)";
          ];
        Element.container
          ~style:(
            Style.empty
            |> Style.height (Style.Fixed 1.0)
            |> Style.bg (`rgb (255, 0, 0))
          )
          [
            Element.text
              ~style:(
                Style.empty
                |> Style.fg (`rgb (255, 255, 255))
              )
              " FOOTER (fixed 1 line)";
          ];
      ]
  in
  (* Layout and render *)
  let viewport = Viewport.make ~width:(float_of_int size.cols) ~height:(float_of_int size.rows) in
  let config = Config.make ~viewport ~text_measurer () in
  let commands = Gooey.layout ~config ui in
  Terminal_renderer_fullscreen.render commands;
  sleep (Time.Duration.from_secs 3);
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
