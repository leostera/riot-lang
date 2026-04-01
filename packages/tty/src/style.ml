open Std

type t = {
  fg: Color.t option;
  bg: Color.t option;
  bold: bool;
  faint: bool;
  italic: bool;
  underline: bool;
  blink: bool;
  reverse: bool;
  strikethrough: bool;
  overline: bool;
}

let default = {
  fg = None;
  bg = None;
  bold = false;
  faint = false;
  italic = false;
  underline = false;
  blink = false;
  reverse = false;
  strikethrough = false;
  overline = false;
}

let fg = fun color t -> { t with fg = Some color }

let bg = fun color t -> { t with bg = Some color }

let bold = fun t -> { t with bold = true }

let faint = fun t -> { t with faint = true }

let italic = fun t -> { t with italic = true }

let underline = fun t -> { t with underline = true }

let blink = fun t -> { t with blink = true }

let reverse = fun t -> { t with reverse = true }

let strikethrough = fun t -> { t with strikethrough = true }

let overline = fun t -> { t with overline = true }

let to_escape_seq = fun t ->
  let codes = [] in
  let codes =
    if t.bold then
      Escape_seq.bold_seq :: codes
    else
      codes
  in
  let codes =
    if t.faint then
      Escape_seq.faint_seq :: codes
    else
      codes
  in
  let codes =
    if t.italic then
      Escape_seq.italics_seq :: codes
    else
      codes
  in
  let codes =
    if t.underline then
      Escape_seq.underline_seq :: codes
    else
      codes
  in
  let codes =
    if t.blink then
      Escape_seq.blink_seq :: codes
    else
      codes
  in
  let codes =
    if t.reverse then
      Escape_seq.reverse_seq :: codes
    else
      codes
  in
  let codes =
    if t.strikethrough then
      Escape_seq.cross_out_seq :: codes
    else
      codes
  in
  let codes =
    if t.overline then
      Escape_seq.overline_seq :: codes
    else
      codes
  in
  let codes =
    match t.fg with
    | Some color when not (Color.is_no_color color) ->
        let seq = "38;" ^ Color.to_escape_seq ~mode:`fg color in
        seq :: codes
    | _ -> codes
  in
  let codes =
    match t.bg with
    | Some color when not (Color.is_no_color color) ->
        let seq = "48;" ^ Color.to_escape_seq ~mode:`bg color in
        seq :: codes
    | _ -> codes
  in
  String.concat ";" (List.rev codes)

let styled = fun t text ->
  if t = default then
    text
  else
    let seq = to_escape_seq t in
    if seq = "" then
      text
    else
      "\027[" ^ seq ^ "m" ^ text ^ "\027[0m"
