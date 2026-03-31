open Std

type text_measurer = string -> Style.t -> Viewport.t

type t = {
  viewport: Viewport.t;
  text_measurer: text_measurer;
}

let default_text_measurer = fun text style ->
    let char_width = 8.0 in
    let width = float_of_int (String.length text) *. char_width in
    let height = float_of_int style.Style.text_size in
    Viewport.make ~width ~height

let make = fun ~viewport ~text_measurer () -> {viewport; text_measurer}
