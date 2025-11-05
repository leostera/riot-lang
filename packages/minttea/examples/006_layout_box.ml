open Std
open Minttea

(* Example 006: Basic blue box with white text and padding *)

type model = { width : int; height : int }

let init model = 
  (model, Command.Seq [
    Command.Enter_alt_screen;
    Command.Hide_cursor;
  ])

let update event model =
  match event with
  | Event.Resize { width; height } -> ({ width; height }, Command.Noop)
  | Event.KeyDown (Key "q", _)
  | Event.KeyDown (Escape, _) -> (model, Command.Quit)
  | _ -> (model, Command.Noop)


let view model =
  let open Element in
  
  let text = 
    let str = format "Terminal: %dx%d | Press 'q' to quit" model.width model.height in
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
  start ~config:(config ()) app { width = 0; height = 0 }
