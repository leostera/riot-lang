open Std

type t = {
  (* cursor style -- defaults to inverted default *)
  style: Style.t;
  (* whether cursor is visible/active *)
  focus: bool;
  (* whether cursor should blink *)
  blink: bool;
  (* blink state of cursor *)
  show: bool;
  (* blink rate *)
  fps: Fps.t;
}

let default_style =
  Style.(default
  |> reverse true)

let default_fps = Fps.from_float 2.5

let make = fun ?(style = default_style) ?(blink = true) ?(fps = default_fps) () ->
  {
    focus = true;
    blink;
    fps;
    show = true;
    style;
  }

let update = fun t (e: Event.t) ->
  match e with
  | Frame now when t.blink ->
      if Fps.tick ~now t.fps = Fps.Frame then
        let show = not t.show in
        { t with show }
      else
        t
  | _ -> t

let view = fun t ~text_style str ->
  if t.show && t.focus then
    Style.render t.style str
  else
    Style.render text_style str

let focus = fun t -> { t with focus = true; show = true }

let unfocus = fun t -> { t with focus = false }

let disable_blink = fun t -> { t with blink = false; show = true }

let enable_blink = fun t -> { t with blink = true; show = true }
