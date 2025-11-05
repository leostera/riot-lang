open Std
open Minttea

(* Model: just a counter *)
type model = { count : int }

(* Initialize and start a timer *)
let init model =
  let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 0.5) in
  (model, cmd)

(* Update: increment counter on timer, quit after 5 *)
let update event model =
  match event with
  | Event.KeyDown (Event.Key "q", _) 
  | Event.KeyDown (Event.Escape, _) ->
      (model, Command.Quit)
  
  | Event.Timer _timer ->
      let count = model.count + 1 in
      if count >= 5 then
        ({ count }, Command.Quit)
      else
        let _timer, cmd = Command.timer ~after:(Time.Duration.from_secs_float 1.0) in
        ({ count }, cmd)
  
  | _ -> (model, Command.Noop)

(* View: show the counter *)
let view model =
  Element.column [
    Element.text 
      ~style:(Style.default
        |> Style.fg (Style.color "#00FFFF")
        |> Style.bold true
        |> Style.padding_top 1
        |> Style.padding_left 2)
      (format "Count: %d" model.count);
    Element.text "\n\nWill quit after 5 ticks (press 'q' to quit early)";
  ]

(* Create the app *)
let app = App.make ~init ~update ~view ()

(* Run it *)
let () = Minttea.start app { count = 0 }
