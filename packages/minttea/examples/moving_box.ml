open Std
open Minttea

(** Simple full-screen example with a moving blue box to test rendering and input *)

type direction = Up | Down | Left | Right

type model = {
  x : int;
  y : int;
  width : int;
  height : int;
}

let init _ = 
  ({ x = 10; y = 5; width = 80; height = 24 }, Command.EnterAltScreen)

let random_direction () =
  match Random.int 4 with
  | 0 -> Up
  | 1 -> Down
  | 2 -> Left
  | _ -> Right

let update msg model =
  match msg with
  | Event.KeyDown ((Event.Key "q" | Event.Escape), _) -> 
      (model, Command.Quit)
  | Event.Frame _ ->
      (* Move the box randomly *)
      let dir = random_direction () in
      let (new_x, new_y) = match dir with
        | Up -> (model.x, Int.max 0 (model.y - 1))
        | Down -> (model.x, Int.min (model.height - 3) (model.y + 1))
        | Left -> (Int.max 0 (model.x - 1), model.y)
        | Right -> (Int.min (model.width - 5) (model.x + 1), model.y)
      in
      ({ model with x = new_x; y = new_y }, Command.Noop)
  | Event.Resize { width; height } ->
      ({ model with width; height }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  let open Element in
  let open Style in
  
  (* Create a blue box using styled text *)
  let blue_color = Tty.Color.of_rgb (0, 0, 255) in
  let blue_style = default |> bg blue_color in
  let box = text ~style:blue_style "  " in
  
  (* Position the box using absolute positioning *)
  let positioned_box = layer ~pos:(Absolute (model.x, model.y)) [box] in
  
  (* Info text at bottom *)
  let info = column
    [ text "\n\n\n"
    ; text "Blue Box Demo - Full Screen Test"
    ; text "Press 'q' to quit"
    ] in
  
  (* Stack the positioned box and info *)
  layer [positioned_box; info]

let () =
  Log.(set_log_file Path.(v "./stdout.log"));
  Log.(set_level Trace);
  Random.self_init ();
  let config = 
    Config.make 
      ~fps:10  (* 10 FPS so we can see it move *)
      ~render_mode:Clear
      ()
  in
  let app = Minttea.app ~init ~update ~view () in
  let initial_model = { x = 10; y = 5; width = 80; height = 24 } in
  Minttea.start ~config app initial_model
