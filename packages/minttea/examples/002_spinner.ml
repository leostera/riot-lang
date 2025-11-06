open Std
open Minttea

(* Model: contains a spinner sprite *)
type model = { spinner : Component.Sprite.t }

(* Initialize with a spinner *)
let init model = (model, Command.Noop)

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
      let spinner = Component.Sprite.update ~now model.spinner in
      ({ spinner }, Command.Noop)
  
  | _ -> (model, Command.Noop)

(* View: render the spinner with some text *)
let view model =
  let spinner_view = Component.Sprite.view model.spinner in
  Element.column [
    Element.text 
      ~style:(Style.default
        |> Style.fg (Style.color "#00FFFF")
        |> Style.bold true
        |> Style.padding_top 1
        |> Style.padding_left 2)
      (spinner_view ^ " Loading...");
    Element.text "\n\nPress 'q' or Ctrl+C to quit";
  ]

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Initial model with a dot spinner *)
let initial_model = 
  { spinner = Component.Spinner.dot }

(* Run it *)
let () = start app initial_model
