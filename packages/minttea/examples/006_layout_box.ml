open Std
open Minttea

(* Example 006: Basic blue box with white text and padding *)

type model = { debug_width : int; debug_height : int }

let init model = 
  (model, Command.Seq [
    Command.EnterAltScreen;
    Command.HideCursor;
  ])

let update event model =
  match event with
  | Event.Resize { width; height } -> ({ debug_width=width; debug_height=width }, Command.Noop)
  | Event.KeyDown (Key "q", _)
  | Event.KeyDown (Escape, _) -> (model, Command.Quit)
  | _ -> (model, Command.Noop)


let view model =
  let open Element in
  
  let text = 
    let str = format "Terminal: %dx%d | Press 'q' to quit" model.debug_width model.debug_height in
    let style = Style.(default |> fg (color "#FFFFFF")) in
    text ~style str
  in
  
  (* Blue box filling the screen with padding and white text *)
  let style = Style.(default
    |> width_flex 1.0
    |> height_flex 1.0
    |> bg (color "#0000FF")  (* Blue background *)
    |> padding_left 2
    |> padding_right 2
    |> padding_top 2
    |> padding_bottom 2)
  in
  box ~style text
    

let app = app ~init ~update ~view ()
let () = 
  Std.Log.(set_level Error);
  start ~config:(config ()) app { debug_width = 0; debug_height = 0 }
