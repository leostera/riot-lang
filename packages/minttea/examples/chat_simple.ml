open Std
open Minttea
open Minttea.Component

(* Simplified chat app to test the viewport and layout *)

type model = {
  viewport : Viewport.t;
  messages : string;
  width : int;
  height : int;
}

let init model =
  Command.Enter_alt_screen

let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Key "Q", _)
  | Event.KeyDown (Event.Escape, _)
  | Event.KeyDown (Event.Key "c", Event.Ctrl) ->
      (model, Command.Quit)
      
  | Event.KeyDown (Event.Up, _) ->
      let viewport = Viewport.scroll_up model.viewport ~lines:1 in
      ({ model with viewport }, Command.Noop)
      
  | Event.KeyDown (Event.Down, _) ->
      let viewport = Viewport.scroll_down model.viewport ~lines:1 in
      ({ model with viewport }, Command.Noop)
      
  | Event.KeyDown (Event.Page_up, _) ->
      let viewport = Viewport.page_up model.viewport in
      ({ model with viewport }, Command.Noop)
      
  | Event.KeyDown (Event.Page_down, _) ->
      let viewport = Viewport.page_down model.viewport in
      ({ model with viewport }, Command.Noop)
      
  | Event.Resize { width; height } ->
      (* Log resize for debugging *)
      let _ = Std.Log.trace "Resize event: %dx%d" width height in
      let viewport_height = height - 3 in
      let viewport = model.viewport
        |> Viewport.set_width ~width
        |> Viewport.set_height ~height:viewport_height
        |> Viewport.set_content ~content:model.messages
      in
      ({ model with viewport; width; height }, Command.Noop)
      
  | _ -> (model, Command.Noop)

let view model =
  let messages = Viewport.view model.viewport in
  let footer = 
    (* Simple footer with styled backgrounds *)
    let input_text = "ostera: Type a message...█" in
    let padding = String.make (Int.max 0 (model.width - String.length input_text)) ' ' in
    let input_style = Style.default
      |> Style.bg (Style.color "#0064FF")
    in
    let input_line = Style.render input_style (input_text ^ padding) in
    
    let status_text = "Status: Ready" in
    let status_padding = String.make (Int.max 0 (model.width - String.length status_text)) ' ' in
    let status_style = Style.default
      |> Style.bg (Style.color "#323232")
    in
    let status_line = Style.render status_style (status_text ^ status_padding) in
    input_line ^ "\n" ^ status_line
  in
  messages ^ "\n\n" ^ footer

let app = Minttea.app ~init ~update ~view ()

let initial_model =
  (* Start with minimal defaults - Program will send Resize event with actual size *)
  let width, height = 1, 1 in  (* Placeholder - will be updated immediately *)
  let viewport_height = 1 in
  
  (* Create viewport with soft-wrap enabled *)
  let viewport = Viewport.make ~width ~height:viewport_height
    |> Viewport.set_wrap_mode ~mode:`Soft
  in
  
  (* Sample messages *)
  let messages = 
    "ostera @ 22:40:27\n" ^
    "  Hello world! This is a test of the chat interface.\n" ^
    "\n" ^
    "ollama/deepseek-r1:8b @ 22:40:30\n" ^
    "  Thinking: The user said hello world which is a common greeting used in\n" ^
    "  programming tutorials and also as a casual way to start a conversation so I\n" ^
    "  need to respond warmly and offer assistance with their coding questions.\n" ^
    "\n" ^
    "ollama/deepseek-r1:8b @ 22:40:32\n" ^
    "  Hello! How can I help you today?"
  in
  
  let viewport = viewport
    |> Viewport.set_content ~content:messages
    |> Viewport.goto_bottom
  in
  
  { viewport; messages; width; height }

let config = Minttea.config ()
let () = Minttea.start ~config app initial_model
