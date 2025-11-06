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
  Program.run ~app ~config ~initial_model
  |> Std.Result.map_err (fun reason -> Failure reason)

let start ?(config = config ()) (app : 'model App.t) initial_model =
  let main ~args:_ =
    let open Std in
    let main_pid = self () in
    let _prog_pid = spawn (fun () ->
      run ~config initial_model app
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
