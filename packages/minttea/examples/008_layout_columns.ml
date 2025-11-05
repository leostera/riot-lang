open Std

(* Example 008: Four columns with flex distribution *)

type model = unit

let init model = (model, Minttea.Command.Enter_alt_screen)

let update event model =
  match event with
  | Minttea.Event.KeyDown (Minttea.Event.Key "q", _)
  | Minttea.Event.KeyDown (Minttea.Event.Escape, _) ->
      (model, Minttea.Command.Quit)
  | _ -> (model, Minttea.Command.Noop)

let view _model =
  let module S = Minttea.Style in
  let module E = Minttea.Element in
  
  E.box ~style:(S.default
    |> S.width_flex 1.0
    |> S.height_flex 1.0)
    (E.row [
      (* Column 1: flex 1.0 *)
      E.box ~style:(S.default
        |> S.width_flex 1.0
        |> S.bg (S.color "#FF0000")
        |> S.padding_left 1)
        (E.text ~style:(S.default |> S.fg (S.color "#FFFFFF"))
          "Col 1\nflex 1.0");
      
      (* Column 2: flex 2.0 (twice as wide) *)
      E.box ~style:(S.default
        |> S.width_flex 2.0
        |> S.bg (S.color "#00FF00")
        |> S.padding_left 1)
        (E.text ~style:(S.default |> S.fg (S.color "#000000"))
          "Col 2\nflex 2.0\n(2x wider)");
      
      (* Column 3: flex 1.0 *)
      E.box ~style:(S.default
        |> S.width_flex 1.0
        |> S.bg (S.color "#0000FF")
        |> S.padding_left 1)
        (E.text ~style:(S.default |> S.fg (S.color "#FFFFFF"))
          "Col 3\nflex 1.0");
      
      (* Column 4: fixed 20 *)
      E.box ~style:(S.default
        |> S.width_fixed 20
        |> S.bg (S.color "#FFFF00")
        |> S.padding_left 1)
        (E.text ~style:(S.default |> S.fg (S.color "#000000"))
          "Col 4\nfixed 20");
    ])
  

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start ~config:(Minttea.config ()) app ()
