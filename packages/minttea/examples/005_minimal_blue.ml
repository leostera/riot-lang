open Std

(* Minimal example - just fill screen with blue *)

type model = { width : int; height : int }

let init model = 
  (model, Minttea.Command.EnterAltScreen)

let update event model =
  match event with
  | Minttea.Event.Resize { width; height } ->
      ({ width; height }, Minttea.Command.Noop)
  | Minttea.Event.KeyDown (Minttea.Event.Key "q", _)
  | Minttea.Event.KeyDown (Minttea.Event.Escape, _) ->
      (model, Minttea.Command.Quit)
  | _ -> (model, Minttea.Command.Noop)

let view model =
  let module S = Minttea.Style in
  let module E = Minttea.Element in

  (* Just a simple blue box, no padding, no text *)
  let style = (S.default
    |> S.width_flex 1.0
    |> S.height_flex 1.0
    |> S.bg (S.color "#0000FF"))
  in
  
  E.box ~style (E.text "")

let app = Minttea.app ~init ~update ~view ()
let () = 
  Log.(set_log_file Path.(v "./stdout.log"));
  Log.(set_level Trace);
  Minttea.start ~config:(Minttea.config ()) app { width = 0; height = 0 }

