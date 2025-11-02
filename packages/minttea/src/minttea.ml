(* Don't open Std here to avoid conflict with Std.Command *)
type Std.Message.t += | ProgramDone

module App = App
module Command = Command
module Component = Component
module Config = Config
module Event = Event
module Program = Program
module Style = Style

let config = Config.make
let app = App.make

let run ?(config = config ()) initial_model app =
  let prog = Program.make ~app ~config in
  Program.run prog initial_model |> Std.Result.expect ~msg:"Program failed"

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
  let _ = Miniriot.run ~main ~args:[] () in
  ()
