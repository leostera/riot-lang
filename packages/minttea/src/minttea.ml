(* Don't open Std here to avoid conflict with Std.Command *)
type Std.Message.t += | ProgramDone

module App = App
module Command = Command
module Component = Component
module Config = Config
module Element = Element
module Event = Event
module Program = Program
module Render = Render
module Style = Style

let config = Config.make
let app = App.make

let run ?(config = config ()) initial_model app =
  let prog = Program.make ~app ~config in
  Program.run prog initial_model
  |> Std.Result.map_err (fun reason -> Failure reason)

let start ?(config = config ()) (app : 'model App.t) initial_model =
  let main ~args:_ =
    let open Std in
    (* Detect terminal size in main process before spawning *)
    (* Create a temporary TTY just to get the size *)
    let cols, rows = match Tty.make () with
      | Ok tty -> 
          let size = Tty.size tty in
          (size.cols, size.rows)
      | Error _ -> (80, 24)
    in
    
    (* Update initial model with detected size *)
    let initial_model_with_size =
      let updated_model, _ = app.App.update (Event.Resize { width = cols; height = rows }) initial_model in
      updated_model
    in
    
    (* Update config with detected terminal size *)
    let config_with_size = Config.make 
      ~render_mode:config.Config.render_mode 
      ~fps:config.Config.fps
      ~initial_width:cols
      ~initial_height:rows
      ()
    in
    
    let main_pid = self () in
    let _prog_pid = spawn (fun () ->
      run ~config:config_with_size initial_model_with_size app
      |> Std.Result.expect ~msg:"Program finished";
      Log.trace "program finished";
      send main_pid ProgramDone;
      Ok ()
    ) in
    (* Wait for the program to finish *)
    let rec wait () =
      match receive_any () with
      | ProgramDone ->
          Log.trace "main finished";
          Ok ()
      | _ -> wait ()
    in
    wait ()
  in
  let _ = Miniriot.run ~main ~args:[] () in
  ()
