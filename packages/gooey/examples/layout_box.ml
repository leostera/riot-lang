open Std
open Gooey

(* Static layout example: Blue box with white text and padding *)

let text_measurer = fun ~constraints text style ->
  Config.default_text_measurer ~constraints text style

let () =
  Actors.run
    ~main:(fun ~args:_ ->
      (* Get terminal dimensions *)
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      (* Create UI: Blue box filling terminal with white text *)
      let ui = Element.container
        ~style:(Style.empty
        |> Style.width Style.Grow
        |> Style.height Style.Grow
        |> Style.bg (`rgb (0, 0, 255))
        |> Style.padding (Style.Padding.all 2))
        [
          Element.text
            ~style:(Style.empty |> Style.fg (`rgb (255, 255, 255)))
            ("Terminal: " ^ Int.to_string size.cols ^ "x" ^ Int.to_string size.rows ^ " | Blue box demo")
        ] in
      (* Layout and render *)
      let viewport = Viewport.make ~width:(float_of_int size.cols) ~height:(float_of_int size.rows) in
      let config = Config.make ~viewport ~text_measurer () in
      let commands = Gooey.layout ~config ui in
      Terminal_renderer_fullscreen.render commands;
      sleep (Time.Duration.from_secs 3);
      Ok ())
    ~args:Env.args
    ()
