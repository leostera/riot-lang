(**
   * Example: FPS Counter
   *
   * This example demonstrates:
   * - Using the FPS component
   * - Frame-based animations
   * - Performance monitoring
   *
   * Key concepts:
   * - Frame events for smooth animations
   * - FPS calculation and display
   * - Performance optimization
   *
   * Controls:
   * - Space - Toggle animation
   * - +/- - Increase/decrease speed
   * - r - Reset position
   * - q/Escape - Quit
*)
open Std
open Minttea
open Minttea.Component

(* Model *)

type model = {
  fps: Fps.t;
  box_x: float;
  box_y: float;
  velocity_x: float;
  velocity_y: float;
  animating: bool;
  speed: float;
  width: int;
  height: int;
}

(* Initialize *)

let init = fun model -> (model, Command.Noop)

(* Update *)

let update = fun event model ->
  match event with
  | Event.KeyDown (Event.Key "q", _)
  | Event.KeyDown (Event.Escape, _) -> (model, Command.Quit)
  | Event.KeyDown (Event.Space, _) ->
      (* Toggle animation *)
      ({ model with animating = not model.animating }, Command.Noop)
  | Event.KeyDown (Event.Key "+", _) ->
      (* Increase speed *)
      ({ model with speed = min 5.0 (model.speed +. 0.5) }, Command.Noop)
  | Event.KeyDown (Event.Key "-", _) ->
      (* Decrease speed *)
      ({ model with speed = max 0.5 (model.speed -. 0.5) }, Command.Noop)
  | Event.KeyDown (Event.Key "r", _) ->
      (* Reset position *)
      ({ model with box_x = 10.0; box_y = 5.0 }, Command.Noop)
  | Event.Frame _instant when model.animating ->
      (* Update FPS counter *)
      let _frame_status = Fps.tick model.fps in
      (* Update box position *)
      let new_x = model.box_x +. (model.velocity_x *. model.speed) in
      let new_y = model.box_y +. (model.velocity_y *. model.speed) in
      (* Bounce off walls *)
      let (final_x, vel_x) =
        if new_x <= 0.0 || new_x >= float_of_int (model.width - 10) then
          (model.box_x, -.(model.velocity_x))
        else
          (new_x, model.velocity_x)
      in
      let (final_y, vel_y) =
        if new_y <= 0.0 || new_y >= float_of_int (model.height - 5) then
          (model.box_y, -.(model.velocity_y))
        else
          (new_y, model.velocity_y)
      in
      (
        {
          model with
          box_x = final_x;
          box_y = final_y;
          velocity_x = vel_x;
          velocity_y = vel_y;
        },
        Command.Noop
      )
  | Event.Frame _instant ->
      (* Just update FPS even when not animating *)
      let _frame_status = Fps.tick model.fps in
      (model, Command.Noop)
  | Event.Resize size ->
      (* Update window dimensions *)
      ({ model with width = size.width; height = size.height }, Command.Noop)
  | _ -> (model, Command.Noop)

(* View *)

let view = fun model ->
  let open Element in
  let box_style =
    Style.(empty
    |> bg (`rgb (255, 100, 100))
    |> fg (`rgb (255, 255, 255))
    |> width (Fixed 10.0)
    |> height (Fixed 3.0)
    |> padding (Style.Padding.all 1))
  in
  column
    ~style:Style.(empty
    |> padding (Style.Padding.all 1))
    [
      row
        ~style:Style.(empty
        |> bg (`rgb (40, 40, 40))
        |> padding (Style.Padding.symmetric ~h:2 ~v:1))
        [
          text
            ~style:Style.(empty
            |> bold
            |> fg (`rgb (0, 255, 127)))
            "FPS: 60";
          spacer ~flex:1.0 ();
          text
            ~style:Style.(empty
            |> fg (`rgb (255, 200, 100)))
            ("Speed: " ^ Float.to_string model.speed ^ "x");
          text " | ";
          text
            ~style:Style.(empty
            |> fg
              (
                if model.animating then
                  `rgb (0, 255, 0)
                else
                  `rgb (255, 100, 100)
              ))
            (
              if model.animating then
                "▶ PLAYING"
              else
                "⏸ PAUSED"
            );
        ];
      text "";
      container
        ~style:Style.(empty
        |> border ~width:1 ~color:(`rgb (100, 100, 200)) ()
        |> min_height (float_of_int (model.height - 10))
        |> min_width (float_of_int (model.width - 4)))
        [
          container
            ~style:Style.(empty
            |> margin
              (Style.Margin.make ~left:(int_of_float model.box_x) ~top:(int_of_float model.box_y) ()))
            [ container ~style:box_style [ text "BOX" ] ];
        ];
      text "";
      row
        ~style:Style.(empty
        |> fg (`rgb (100, 100, 100)))
        [
          text "Space: Play/Pause";
          text " • ";
          text "+/-: Speed";
          text " • ";
          text "r: Reset";
          text " • ";
          text "q: Quit";
        ];
    ]

(* Create and run the app *)

let app = App.make ~init ~update ~view ()

(* Run it *)

let main ~args:_ =
  let initial_model = {
    fps = Fps.from_int 60;
    box_x = 10.0;
    box_y = 5.0;
    velocity_x = 1.0;
    velocity_y = 0.5;
    animating = true;
    speed = 1.0;
    width = 80;
    height = 24;
  }
  in
  let config = Minttea.config ~fps:60 () in
  Minttea.run ~config initial_model app

let () = Runtime.run ~main ~args:Env.args ()
