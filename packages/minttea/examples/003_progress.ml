open Std
open Minttea
open Minttea.Component

(* Model: contains a progress bar and completion state *)

type model = {
  progress: Progress.t;
  waiting_to_quit: bool;
}

(* Initialize with a progress bar and start a timer *)

let init = fun model ->
    let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 0.1) in
    (model, cmd)

(* Update: handle timer events to increment progress *)

let update = fun event model ->
    match event with
    | Event.KeyDown (Event.Key "q", _)
    | Event.KeyDown (Event.Key "Q", _)
    | Event.KeyDown (Event.Escape, _)
    | Event.KeyDown (Event.Key "c", Event.Ctrl) ->
        (model, Command.Quit)
    | Event.Timer _timer ->
        if model.waiting_to_quit then
          (model, Command.Quit)
        else
          let progress = Progress.increment model.progress ~delta:0.02 in
          if Progress.is_finished progress then
            let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 1.0) in
            ({progress; waiting_to_quit = true}, cmd)
          else
            (* Continue incrementing *)
            let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 0.1) in
            ({progress; waiting_to_quit = false}, cmd)
    | _ ->
        (model, Command.Noop)

(* View: render the progress bar *)

let view = fun model ->
    let progress_view = Progress.view model.progress in
    let instructions =
      if model.waiting_to_quit then
        "Completed! Exiting..."
      else
        "Press 'q' or Ctrl+C to quit"
    in
    Element.column
      [
        Element.text ~style:((Style.empty |> Style.fg (`rgb (0, 255, 255)) |> Style.bold)) "Downloading...";
        progress_view;
        Element.text instructions;

      ]

(* Create the app *)

let app = App.make ~init ~update ~view ()

(* Initial model with a progress bar *)

let initial_model = {
  progress = Progress.make
    ~percent:0.0
    ~width:40
    ~show_percentage:true
    ~color:(`Gradient (Style.color "#FF00FF", Style.color "#00FFFF"))
    ();
  waiting_to_quit = false;

}

(* Run it *)

let () =
  (* TODO: Implement FileHandler for logging to file *)
  (* Log.(set_log_file (Path.v "./stdout.log")); *)
  Log.(set_level Trace);
  let config = Minttea.config () in
  Minttea.start ~config app initial_model
