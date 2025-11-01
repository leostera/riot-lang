(* Don't open Std here to avoid conflict with Std.Command *)

type Std.Message.t += | ProgramDone

(* Core modules *)
module Timer = Timer_ref
module Config = Config
module Event = Event
module Command = Command
module App = App
module Program = Program

(* Styles and utilities *)
module Style = Style
module Layout = Style.Layout

(* Components *)
module Component = Component

let config ?(render_mode = `clear) ?(fps = 60) () = Config.make ~render_mode ~fps ()

let app = App.make

let run ?(config = config ()) initial_model app =
  let prog = Program.make ~app ~config in
  Program.run prog initial_model;
  Std.Log.trace "terminating"

let start ?(config = config ()) app initial_model =
  let main ~args:_ =
    let open Std in
    let main_pid = self () in
    let _prog_pid = spawn (fun () ->
      run ~config initial_model app;
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
  Miniriot.run ~main ~args:[] ()
