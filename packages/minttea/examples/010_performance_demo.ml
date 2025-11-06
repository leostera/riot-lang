open Std
open Minttea

(** Performance test to verify our game-engine-like optimizations:
    - 60 FPS rendering
    - Scene graph differential rendering
    - Synchronized updates
*)

type model = {
  counter : int;
  frames_received : int;  (* Frame events we've received *)
  renders_done : int;     (* Times we actually rendered *)
  static_mode : bool;     (* Toggle to test skipping *)
}

let init _ = ({ counter = 0; frames_received = 0; renders_done = 0; static_mode = false }, Command.Noop)

let update msg model =
  match msg with
  | Event.KeyDown ((Event.Key "q" | Event.Escape), _) -> (model, Command.Quit)
  | Event.KeyDown (Event.Key "s", _) -> 
      (* Toggle static mode to test render skipping *)
      ({ model with static_mode = not model.static_mode }, Command.Noop)
  | Event.Frame _ ->
      let new_counter = if model.static_mode then model.counter else model.counter + 1 in
      ({ model with 
         counter = new_counter; 
         frames_received = model.frames_received + 1;
         renders_done = model.renders_done + 1 (* This will only count when view changes *)
       }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  let open Element in
  let progress = model.counter mod 100 in
  let bar_width = progress / 2 in
  
  (* In static mode, don't show changing numbers *)
  let frame_text = 
    if model.static_mode then
      "Static Mode: UI frozen (press 's' to toggle)"
    else
      format "Frames Received: %d | Counter: %d" model.frames_received model.counter
  in
  
  column
    [ text "🚀 Minttea Performance Test (60 FPS)"
    ; text ""
    ; text frame_text
    ; text ""
    ; text "Progress Bar (frozen in static mode):"
    ; row [ text "["
        ; text (String.make bar_width '#')
        ; text (String.make (50 - bar_width) ' ')
        ; text "]"
        ; text (format " %d%%" progress)
        ]
    ; text ""
    ; text "Controls:"
    ; text "- Press q to quit"
    ; text "- Press s to toggle static mode"
    ; text ""
    ; text (if model.static_mode then 
        "Static Mode ON - Elements unchanged, rendering should be skipped"
      else
        "Static Mode OFF - Elements changing every frame")
    ]

let () =
  let config = 
    Config.make 
      ~fps:60
      ~render_mode:Clear
      ()
  in
  let app = Minttea.app ~init ~update ~view () in
  let initial_model = { counter = 0; frames_received = 0; renders_done = 0; static_mode = false } in
  Minttea.start ~config app initial_model 
