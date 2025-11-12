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
  | Event.KeyDown (Event.Escape, _) -> (model, Command.Quit)
  | _ -> (model, Command.Noop)

(* View: Render the message using Gooey's API *)
let view model =
  let open Element in
  column [
    text 
      ~style:Style.(empty
        |> fg (`rgb (0, 255, 255))  (* Cyan *)
        |> bold
        |> padding (Padding.make ~top:1 ~left:2 ()))
      model;
    text "\n\nPress 'q' to quit";
  ]

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Run it with initial model *)
let () = Minttea.start app "Hello, World!"
