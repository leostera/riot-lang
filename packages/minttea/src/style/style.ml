open Std
open Std.Collections

module Layout = Layout

type color = Tty.Color.t = private
  | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color

let color ?(profile = Tty.Profile.default) raw =
  let color = Tty.Color.make raw in
  let color = Tty.Profile.convert profile color in
  color

let gradient = Gradient.make

module Border = Border

type style = {
  background : color option;
  blink : bool;
  bold : bool;
  faint : bool;
  foreground : color option;
  height : int option;
  italic : bool;
  margin_bottom : int;
  margin_left : int;
  margin_right : int;
  margin_top : int;
  max_height : int option;
  max_width : int option;
  padding_bottom : int;
  padding_left : int;
  padding_right : int;
  padding_top : int;
  reverse : bool;
  strikethrough : bool;
  underline : bool;
  width : int option;
  border : Border.t option;
  align_horizontal : [`Left | `Center | `Right] option;
  align_vertical : [`Top | `Center | `Bottom] option;
}

let default =
  {
    background = None;
    blink = false;
    bold = false;
    faint = false;
    foreground = None;
    height = None;
    italic = false;
    margin_bottom = 0;
    margin_left = 0;
    margin_right = 0;
    margin_top = 0;
    max_height = None;
    max_width = None;
    padding_bottom = 0;
    padding_left = 0;
    padding_right = 0;
    padding_top = 0;
    reverse = false;
    strikethrough = false;
    underline = false;
    width = None;
    border = None;
    align_horizontal = None;
    align_vertical = None;
  }

let bg x t = { t with background = Some x }
let blink x t = { t with blink = x }
let bold x t = { t with bold = x }
let faint x t = { t with faint = x }
let fg x t = { t with foreground = Some x }
let height x t = { t with height = Some x }
let italic x t = { t with italic = x }
let margin_bottom x t = { t with margin_bottom = x }
let margin_left x t = { t with margin_left = x }
let margin_right x t = { t with margin_right = x }
let margin_top x t = { t with margin_top = x }
let max_height x t = { t with max_height = Some x }
let max_width x t = { t with max_width = Some x }
let padding_bottom x t = { t with padding_bottom = x }
let padding_left x t = { t with padding_left = x }
let padding_right x t = { t with padding_right = x }
let padding_top x t = { t with padding_top = x }
let reverse x t = { t with reverse = x }
let strikethrough x t = { t with strikethrough = x }
let underline x t = { t with underline = x }
let width x t = { t with width = x }
let border x t = { t with border = Some x }
let align_horizontal x t = { t with align_horizontal = Some x }
let align_vertical x t = { t with align_vertical = Some x }

let do_render t str =
  (* Pre-process padding *)
  let apply_padding str =
    let pad_left = String.make t.padding_left ' ' in
    let pad_right = String.make t.padding_right ' ' in
    let pad_top = String.init t.padding_top (fun _ -> '\n') in
    let pad_bottom = String.init t.padding_bottom (fun _ -> '\n') in
    pad_top ^ pad_left ^ str ^ pad_right ^ pad_bottom
  in

  let str = apply_padding str in
  
  (* Apply horizontal alignment if width is set *)
  let str = match (t.align_horizontal, t.width) with
    | (Some align, Some target_width) ->
        let lines = Util.Ansi.split_lines str in
        let aligned_lines = List.map (fun line ->
          match align with
          | `Left -> Util.Ansi.pad_right ~width:target_width ' ' line
          | `Right -> Util.Ansi.pad_left ~width:target_width ' ' line
          | `Center -> Util.Ansi.pad_center ~width:target_width ' ' line
        ) lines in
        String.concat "\n" aligned_lines
    | _ -> str
  in
  
  (* Apply vertical alignment if height is set *)
  let str = match (t.align_vertical, t.height) with
    | (Some align, Some target_height) ->
        let lines = Util.Ansi.split_lines str in
        let current_height = List.length lines in
        if current_height >= target_height then
          (* Truncate if needed *)
          let lines = lines |> List.to_seq |> Seq.take target_height |> List.of_seq in
          String.concat "\n" lines
        else
          let padding_needed = target_height - current_height in
          (match align with
          | `Top -> 
              lines @ List.make ~len:padding_needed ~fn:(fun _ -> "")
          | `Bottom -> 
              List.make ~len:padding_needed ~fn:(fun _ -> "") @ lines
          | `Center ->
              let top_pad = padding_needed / 2 in
              let bottom_pad = padding_needed - top_pad in
              List.make ~len:top_pad ~fn:(fun _ -> "") @ lines @ List.make ~len:bottom_pad ~fn:(fun _ -> ""))
          |> String.concat "\n"
    | _ -> str
  in

  (* build formatting sequence *)
  let format_seq =
    Formatter.
      [
        (if t.blink then [ Blink ] else []);
        (if t.bold then [ Bold ] else []);
        (if t.faint then [ Faint ] else []);
        (if t.italic then [ Italic ] else []);
        (if t.reverse then [ Reverse ] else []);
        (if t.strikethrough then [ Cross_out ] else []);
        (if t.underline then [ Underline ] else []);
        (match t.foreground with
        | Some color when Tty.Color.is_no_color color -> []
        | Some color -> [ Foreground color ]
        | None -> []);
        (match t.background with
        | Some color when Tty.Color.is_no_color color -> []
        | Some color -> [ Background color ]
        | None -> []);
      ]
    |> List.flatten
  in

  (* render core text *)
  let str =
    let lines = String.split_on_char '\n' str in
    List.map (fun line -> Formatter.format_string format_seq line) lines
    |> String.concat "\r\n"
  in

  (* handle border *)
  let str =
    match t.border with
    | Some border -> Border.build_border border str
    | None -> str
  in

  (* handle margin *)
  let str = Cell.create str in
  if t.margin_left > 0 then Cell.set str (String.make t.margin_left ' ' ^ Cell.get str);
  if t.margin_right > 0 then Cell.set str (Cell.get str ^ String.make t.margin_right ' ');
  if t.margin_top > 0 then Cell.set str (String.make t.margin_top '\n' ^ Cell.get str);
  if t.margin_bottom > 0 then Cell.set str (Cell.get str ^ String.make t.margin_bottom '\n');

  (match t.max_height with
  | Some max_height when max_height > 0 ->
      let lines = String.split_on_char '\n' (Cell.get str) in
      let lines = lines |> List.to_seq |> Seq.take max_height |> List.of_seq in
      Cell.set str (String.concat "\n" lines)
  | _ -> ());

  (match t.max_width with
  | Some max_width when max_width > 0 ->
      let lines = Util.Ansi.split_lines (Cell.get str) in
      let truncated = List.map (fun line ->
        if Util.Ansi.width line > max_width then
          Util.Ansi.truncate ~width:max_width ~ellipsis:"…" line
        else line
      ) lines in
      Cell.set str (String.concat "\n" truncated)
  | _ -> ());

  Cell.get str

let render t str = do_render t str
