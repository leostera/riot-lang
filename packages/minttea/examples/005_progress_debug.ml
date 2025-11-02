open Std
open Minttea
open Minttea.Component

(* Model: contains a progress bar and update counter *)
type model = {
  progress : Progress.t;
  update_count : int;
}

(* Initialize with a progress bar and start a timer *)
let init model =
  let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 0.1) in
  cmd

(* Update: handle timer events to increment progress *)
let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _) 
  | Event.KeyDown (Event.Key "Q", _)
  | Event.KeyDown (Event.Escape, _) 
  | Event.KeyDown (Event.Key "c", Event.Ctrl) ->
      (model, Command.Quit)
  
  | Event.Timer _timer ->
      let update_count = model.update_count + 1 in
      
      Log.info "Timer #%d fired!" update_count;
      
      let progress = Progress.increment model.progress 0.02 in
      if Progress.is_finished progress then
        (* Progress complete, quit *)
        ({ progress; update_count }, Command.Quit)
      else
        (* Continue incrementing *)
        let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 0.1) in
        ({ progress; update_count }, cmd)
  
  | _ -> (model, Command.Noop)

(* View: render the progress bar *)
let view model =
  let progress_view = Progress.view model.progress in
  let title = 
    Style.(default
    |> fg (color "#00FFFF")
    |> bold true
    |> render)
    "Downloading..."
  in
  
  let instructions = "\n\nPress 'q' or Ctrl+C to quit" in
  
  title ^ "\n" ^ progress_view ^ instructions

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Initial model with a progress bar *)
let initial_model = 
  {
    progress = Progress.make 
      ~percent:0.0
      ~width:40
      ~show_percentage:true
      ~color:(`Gradient (Style.color "#FF00FF", Style.color "#00FFFF"))
      ();
    update_count = 0;
  }

(* Run it *)
let config = Minttea.config ~render_mode:Persist ~fps:1 ()
let () = Minttea.start ~config app initial_model
