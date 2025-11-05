open Std

(* Example 007: Three rows with different colors *)

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
    (E.column [
    (* Row 1: Fixed height header *)
    E.box ~style:(S.default
      |> S.height_fixed 3
      |> S.bg (S.color "#0000FF")
      |> S.padding_left 2)
      (E.text ~style:(S.default |> S.fg (S.color "#FFFFFF"))
        "HEADER (fixed 3 lines)");
    
    (* Row 2: Flexible content area *)
    E.box ~style:(S.default
      |> S.height_flex 1.0
      |> S.bg (S.color "#00FF00")
      |> S.padding_left 2)
      (E.text ~style:(S.default |> S.fg (S.color "#000000"))
        "CONTENT (flexible - fills remaining space)");
    
    (* Row 3: Fixed height footer *)
    E.box ~style:(S.default
      |> S.height_fixed 1
      |> S.bg (S.color "#FF0000"))
      (E.text ~style:(S.default |> S.fg (S.color "#FFFFFF"))
        " FOOTER (fixed 1 line) | Press Q to quit");
  ])

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start ~config:(Minttea.config ()) app ()
