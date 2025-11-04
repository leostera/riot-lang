open Std
open Minttea

type model = {
  width : int;
  height : int;
  viewport_height : int;
}

let init model = Command.Enter_alt_screen

let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _) 
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  | Event.Resize { width; height } ->
      let viewport_height = height - 3 in
      ({ width; height; viewport_height }, Command.Noop)
  | _ -> (model, Command.Noop)

let view model =
  let lines = ref [] in
  
  (* Header showing detected size *)
  lines := (format "Terminal Size: %dx%d" model.width model.height) :: !lines;
  lines := (format "Viewport Height: %d" model.viewport_height) :: !lines;
  lines := "" :: !lines;
  
  (* Fill viewport with numbered lines *)
  for i = 1 to model.viewport_height do
    lines := (format "Line %d of %d" i model.viewport_height) :: !lines
  done;
  
  (* Footer *)
  lines := "" :: !lines;
  lines := "[Input area - press 'q' to quit]" :: !lines;
  lines := "[Status: OK]" :: !lines;
  
  String.concat "\n" (List.rev !lines)

let initial_model = { width = 1; height = 1; viewport_height = 1 }

let app = Minttea.app ~init ~update ~view ()

let () = 
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
