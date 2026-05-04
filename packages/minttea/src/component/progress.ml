open Std
open Std.Collections

type t = {
  color: [`Plain of Style.color | `Gradient of Style.color array];
  width: int;
  mutable percent: float;
  mutable finished: bool;
  full_char: string;
  empty_char: string;
  trail_char: string;
  show_percentage: bool;
}

let default_full_char = "█"

let default_empty_char = " "

let default_trail_char = ""

let default_color = `Plain (Style.color "#00FFA3")

let default_show_percentage = true

let make = fun
  ?(percent = 0.)
  ?(full_char = default_full_char)
  ?(trail_char = default_trail_char)
  ?(empty_char = default_empty_char)
  ?(color = default_color)
  ?(show_percentage = default_show_percentage)
  ~width
  () ->
  {
    width;
    percent;
    full_char;
    empty_char;
    trail_char;
    show_percentage;
    finished = false;
    color =
      (
        match color with
        | `Plain c -> `Plain c
        | `Gradient Style.((No_color, No_color)) -> `Plain (Tty.Color.from_rgb (127, 127, 127))
        | `Gradient Style.((No_color, c)) -> `Plain c
        | `Gradient Style.((c, No_color)) -> `Plain c
        | `Gradient (start, finish) -> `Gradient (Style.gradient ~start ~finish ~steps:width)
      );
  }

let is_finished = fun t -> t.finished

let reset = fun t ->
  t.percent <- 0.;
  t.finished <- false;
  t

let set_progress = fun t ~progress ->
  t.percent <- if progress < 0.0 then
    0.0
  else if progress > 1.0 then
    1.0
  else
    progress;
  if t.percent = 1.0 then
    t.finished <- true;
  t

let increment = fun t ~delta:amount ->
  if t.percent +. amount < 1.0 then
    t.percent <- t.percent +. amount
  else (
    t.percent <- 1.0;
    t.finished <- true
  );
  t

let view = fun t ->
  let percent =
    if t.percent < 0.0 then
      0.0
    else if t.percent > 1.0 then
      1.0
    else
      t.percent
  in
  let full_size = Int.from_float (Float.floor (Float.from_int t.width *. t.percent)) in
  (* Build progress bar as a pre-rendered string with ANSI codes using old Style module *)
  let color char =
    match t.color with
    | `Plain c ->
        fun _ ->
          Style.(render
            (
              default
              |> fg c
            )
            char)
    | `Gradient color_ramp ->
        fun i ->
          let shade =
            Array.get color_ramp ~at:i
            |> Option.expect ~msg:"gradient color index out of bounds"
          in
          Style.(render
            (
              default
              |> fg shade
            )
            char)
  in
  let full_part =
    if String.length t.full_char = 0 then
      ""
    else
      List.init ~count:full_size ~fn:(color t.full_char)
      |> String.concat ""
  in
  (* Only show trail if we're not at 100% and have space remaining *)
  let has_trail = full_size < t.width && String.length t.trail_char > 0 in
  let trail_part =
    if has_trail then
      color t.trail_char full_size
    else
      ""
  in
  (* Calculate empty size based on whether we have a trail *)
  let empty_size =
    let used =
      full_size + (
        if has_trail then
          1
        else
          0
      )
    in
    Int.max 0 (t.width - used)
  in
  let empty_part =
    if String.length t.empty_char = 0 then
      ""
    else
      String.make
        ~len:empty_size
        ~char:(
          String.get t.empty_char ~at:0
          |> Option.expect ~msg:"empty progress char"
        )
  in
  let percentage_part =
    if t.show_percentage then
      " " ^ Float.to_string (percent *. 100.) ^ "%"
    else
      ""
  in
  let progress_string = full_part ^ trail_part ^ empty_part ^ percentage_part in
  (* Use Element.custom with Custom render command to output raw ANSI *)
  let measure ~constraints:_ =
    let visible_width =
      float_of_int
        (
          t.width + (
            if t.show_percentage then
              7
            else
              0
          )
        )
    in
    Gooey.Viewport.make ~width:visible_width ~height:1.0
  in
  let render box = [
    {
      Gooey.Render.bounding_box = box;
      command_type = Gooey.Render.Custom { data = progress_string };
      z_index = 0;
    };
  ]
  in
  Gooey.Element.custom ~measure ~render ()
