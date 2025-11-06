open Std

(* Example 009: Classic sticky footer layout (header + flexible content + footer) *)

type model = { counter : int }

let init model = (model, Minttea.Command.EnterAltScreen)

let update event model =
  match event with
  | Minttea.Event.KeyDown (Minttea.Event.Key "q", _)
  | Minttea.Event.KeyDown (Minttea.Event.Escape, _) ->
      (model, Minttea.Command.Quit)
  | Minttea.Event.KeyDown (Minttea.Event.Space, _) ->
      ({ counter = model.counter + 1 }, Minttea.Command.Noop)
  | _ -> (model, Minttea.Command.Noop)

let view model =
  let module S = Minttea.Style in
  let module E = Minttea.Element in
  
  E.box ~style:(S.default
    |> S.width_flex 1.0
    |> S.height_flex 1.0)
    (E.column [
      (* Header: fixed height *)
      E.box ~style:(S.default
        |> S.height_fixed 3
        |> S.bg (S.color "#2E3440")
        |> S.fg (S.color "#ECEFF4")
        |> S.padding_left 2
        |> S.padding_top 1)
        (E.text "Sticky Footer Example");
      
      (* Content: flexible (grows to fill space) *)
      E.box ~style:(S.default
        |> S.height_flex 1.0
        |> S.bg (S.color "#3B4252")
        |> S.fg (S.color "#D8DEE9")
        |> S.padding_left 2
        |> S.padding_top 1)
        (E.text (format "Content area (flexible)\n\nCounter: %d\n\nPress SPACE to increment" model.counter));
      
      (* Footer: fixed height, always at bottom *)
      E.box ~style:(S.default
        |> S.height_fixed 1
        |> S.bg (S.color "#4C566A")
        |> S.fg (S.color "#ECEFF4")
        |> S.padding_left 2)
        (E.text "Footer (fixed) | Press Q to quit");
    ])

let app = Minttea.app ~init ~update ~view ()
let () = Minttea.start ~config:(Minttea.config ()) app { counter = 0 }
