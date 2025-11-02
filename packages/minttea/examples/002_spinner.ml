open Std
open Minttea

(* Model: contains a spinner sprite *)
type model = { spinner : Minttea.Component.Sprite.t }

(* Initialize with a spinner *)
let init _model = Command.Noop

(* Update: handle keyboard events and frame updates *)
let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _) 
  | Event.KeyDown (Event.Key "Q", _)
  | Event.KeyDown (Event.Escape, _) 
  | Event.KeyDown (Event.Key "c", Event.Ctrl) ->
      (model, Command.Quit)
  
  | Event.Frame now ->
      (* Update spinner animation *)
      let spinner = Minttea.Component.Sprite.update ~now model.spinner in
      ({ spinner }, Command.Noop)
  
  | _ -> (model, Command.Noop)

(* View: render the spinner with some text *)
let view model =
  let spinner_view = Minttea.Component.Sprite.view model.spinner in
  let styled_message = 
    Style.(default
    |> fg (color "#00FFFF")
    |> bold true
    |> padding_top 1
    |> padding_left 2
    |> render)
  in
  
  styled_message (spinner_view ^ " Loading...") ^ "\n\nPress 'q' or Ctrl+C to quit"

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Initial model with a dot spinner *)
let initial_model = 
  { spinner = Minttea.Component.Spinner.dot }

(* Run it *)
let () = Minttea.start app initial_model
