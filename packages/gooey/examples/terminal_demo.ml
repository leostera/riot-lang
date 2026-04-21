open Std
open Gooey

(* Use Gooey's default terminal-cell text measurement and wrapping. *)

let text_measurer = fun ~constraints text style -> Config.default_text_measurer ~constraints text style

let () =
  Actors.run
    ~main:(fun ~args:_ ->
      (* Create a simple UI with text, colored boxes, and borders *)
      let ui = Element.column
        ~style:(Style.empty |> Style.padding (Style.Padding.all 2) |> Style.child_gap 1)
        [
          Element.container
            ~style:(Style.empty
            |> Style.padding (Style.Padding.all 1)
            |> Style.border ~width:1 ~color:(`rgb (100, 150, 255)) ()
            |> Style.bg (`rgb (20, 20, 40)))
            [
              Element.text ~style:(Style.empty |> Style.fg (`rgb (255, 255, 255))) "Gooey Terminal Demo"
            ];
          Element.row
            ~style:(Style.empty |> Style.child_gap 2)
            [
              Element.container
                ~style:(Style.empty
                |> Style.width (Style.Fixed 10.0)
                |> Style.height (Style.Fixed 3.0)
                |> Style.bg (`rgb (255, 100, 100)))
                [];
              Element.container
                ~style:(Style.empty
                |> Style.width (Style.Fixed 10.0)
                |> Style.height (Style.Fixed 3.0)
                |> Style.bg (`rgb (100, 255, 100)))
                [];
              Element.container
                ~style:(Style.empty
                |> Style.width (Style.Fixed 10.0)
                |> Style.height (Style.Fixed 3.0)
                |> Style.bg (`rgb (100, 100, 255)))
                [];
            ];
          Element.text ~style:(Style.empty |> Style.fg (`rgb (200, 200, 200))) "This is rendered to your terminal!";
          Element.container
            ~style:(Style.empty
            |> Style.padding (Style.Padding.all 2)
            |> Style.border ~width:1 ~color:(`rgb (255, 200, 100)) ()
            |> Style.bg (`rgb (50, 40, 30)))
            [
              Element.text ~style:(Style.empty |> Style.fg (`rgb (255, 255, 200))) "Box with border and background"
            ];
        ] in
      (* Create config with text measurer *)
      (* Get actual terminal dimensions *)
      let tty = Tty.make () |> Result.unwrap in
      let size = Tty.size tty in
      let viewport = Viewport.make ~width:(float_of_int size.cols) ~height:(float_of_int size.rows) in
      let config = Config.make ~viewport ~text_measurer () in
      (* Layout the UI *)
      let commands = Gooey.layout ~config ui in
      (* Render to terminal *)
      Gooey.Terminal_renderer_fullscreen.render commands;
      sleep (Time.Duration.from_secs 3);
      Ok ())
    ~args:Env.args
    ()
