open Std
open Minttea

(* Model: Just a simple message *)

type model = string

(* Initialize: Takes initial model, returns command *)
let init _model = Command.Noop

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
  let styled_message = 
    Style.(default
    |> fg (color "#00FFFF")  (* Cyan *)
    |> bold true
    |> padding_top 1
    |> padding_left 2
    |> render)
  in
  
  styled_message model ^ "\n\nPress 'q' to quit"

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Run it with initial model *)
let () = Minttea.start app "Hello, World!"
