open Std
open Minttea

(* Model: Just a simple message *)
type model = string

(* Initialize: Takes initial model, returns updated model and command *)
let init model = (model, Command.Noop)

(* Update: Handle keyboard events *)
let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _) 
  | Event.KeyDown (Event.Key "Q", _)
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  
  | _ -> (model, Command.Noop)

(* View: Render the message *)
let view model =
  Element.column [
    Element.text 
      ~style:(Style.default
        |> Style.fg (Style.color "#00FFFF")  (* Cyan *)
        |> Style.bold true
        |> Style.padding_top 1
        |> Style.padding_left 2)
      model;
    Element.text "\n\nPress 'q' to quit";
  ]

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Run it with initial model *)
let () = Minttea.start app "Hello, World!"
