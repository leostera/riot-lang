open Std
open Std.Sync
open Std.Collections

type color = Tty.Color.t =
  private | RGB of int * int * int
  | ANSI of int
  | ANSI256 of int
  | No_color

let color = fun ?(profile = Tty.Profile.default) raw ->
  let color = Tty.Color.make raw in
  let color = Tty.Profile.convert profile color in
  color

let gradient = Gradient.make

module Border = Border

(* Size specification for width/height *)

type size =
  | Auto
  (* Measure content, use intrinsic size *)
  | Fixed of int
  (* Explicit size in cells *)
  | Flex of float

(* Flexible unit, shares remaining space *)

(* Overflow behavior *)

type overflow =
  | Visible
  (* Don't clip (default) *)
  | Hidden
  (* Clip content that exceeds bounds *)
  | Scroll

(* Future: scrollable (not implemented yet) *)

(* Constraints for Auto/Flex *)

type constraints = {
  min_width: int option;
  max_width: int option;
  min_height: int option;
  max_height: int option;
}

type t = {
  background: color option;
  blink: bool;
  bold: bool;
  faint: bool;
  foreground: color option;
  height: size;
  italic: bool;
  margin_bottom: int;
  margin_left: int;
  margin_right: int;
  margin_top: int;
  padding_bottom: int;
  padding_left: int;
  padding_right: int;
  padding_top: int;
  reverse: bool;
  strikethrough: bool;
  underline: bool;
  width: size;
  border: Border.t option;
  align_horizontal:
    ([
      `Left
      | `Center
      | `Right
    ]) option;
  align_vertical:
    ([
      `Top
      | `Center
      | `Bottom
    ]) option;
  overflow: overflow;
  constraints: constraints;
}

(* Structural equality for styles *)

let equal : t -> t -> bool = fun a b -> a = b

let default = {
  background = None;
  blink = false;
  bold = false;
  faint = false;
  foreground = None;
  height = Auto;
  italic = false;
  margin_bottom = 0;
  margin_left = 0;
  margin_right = 0;
  margin_top = 0;
  padding_bottom = 0;
  padding_left = 0;
  padding_right = 0;
  padding_top = 0;
  reverse = false;
  strikethrough = false;
  underline = false;
  width = Auto;
  border = None;
  align_horizontal = None;
  align_vertical = None;
  overflow = Visible;
  constraints = {min_width = None; max_width = None; min_height = None; max_height = None; };

}

let bg = fun x t -> {t with background = Some x}

let blink = fun x t -> {t with blink = x}

let bold = fun x t -> {t with bold = x}

let faint = fun x t -> {t with faint = x}

let fg = fun x t -> {t with foreground = Some x}

let italic = fun x t -> {t with italic = x}

let margin_bottom = fun x t -> {t with margin_bottom = x}

let margin_left = fun x t -> {t with margin_left = x}

let margin_right = fun x t -> {t with margin_right = x}

let margin_top = fun x t -> {t with margin_top = x}

let padding_bottom = fun x t -> {t with padding_bottom = x}

let padding_left = fun x t -> {t with padding_left = x}

let padding_right = fun x t -> {t with padding_right = x}

let padding_top = fun x t -> {t with padding_top = x}

let reverse = fun x t -> {t with reverse = x}

let strikethrough = fun x t -> {t with strikethrough = x}

let underline = fun x t -> {t with underline = x}

let border = fun x t -> {t with border = Some x}

let align_horizontal = fun x t -> {t with align_horizontal = Some x}

let align_vertical = fun x t -> {t with align_vertical = Some x}

(* Legacy API - kept for compatibility *)

let height = fun x t -> {t with height = Fixed x}

let width = fun x t ->
  match x with
  | Some w -> {t with width = Fixed w}
  | None -> {t with width = Auto}

let max_height = fun x t -> {t with constraints = {t.constraints with max_height = Some x}}

let max_width = fun x t -> {t with constraints = {t.constraints with max_width = Some x}}

(* New size API *)

let width_auto = fun t -> {t with width = Auto}

let width_fixed = fun x t -> {t with width = Fixed x}

let width_flex = fun x t -> {t with width = Flex x}

let height_auto = fun t -> {t with height = Auto}

let height_fixed = fun x t -> {t with height = Fixed x}

let height_flex = fun x t -> {t with height = Flex x}

(* Constraint API *)

let min_width = fun x t -> {t with constraints = {t.constraints with min_width = Some x}}

let min_height = fun x t -> {t with constraints = {t.constraints with min_height = Some x}}

(* Overflow API *)

let overflow = fun x t -> {t with overflow = x}

let do_render = fun t str ->
  (* Pre-process padding *)
  let apply_padding = fun str ->
    let pad_left = String.make t.padding_left ' ' in
    let pad_right = String.make t.padding_right ' ' in
    (* Apply horizontal padding to each line *)
    let lines = Util.Ansi.split_lines str in
    let padded_lines =
      List.map (fun line -> pad_left ^ line ^ pad_right) lines
    in
    let str_with_h_padding = String.concat "\n" padded_lines in
    (* Apply vertical padding (top and bottom) *)
    let pad_top =
      String.concat "\n" (List.init t.padding_top (fun _ -> ""))
    in
    let pad_bottom =
      String.concat "\n" (List.init t.padding_bottom (fun _ -> ""))
    in
    let result = (
      if t.padding_top > 0 then
        pad_top ^ "\n"
      else
        ""
    ) ^ str_with_h_padding ^ (
      if t.padding_bottom > 0 then
        "\n" ^ pad_bottom
      else
        ""
    )
    in
    result
  in
  let str = apply_padding str in
  (* Extract target width/height from size spec *)
  let target_width =
    match t.width with
    | Fixed w -> Some w
    | Auto
    | Flex _ -> None
  in
  let target_height =
    match t.height with
    | Fixed h -> Some h
    | Auto
    | Flex _ -> None
  in
  (* Apply horizontal alignment/padding if width is set *)
  let str =
    match target_width with
    | Some w ->
        let align = Option.unwrap_or ~default:`Left t.align_horizontal in
        let lines = Util.Ansi.split_lines str in
        let aligned_lines =
          List.map
            (fun line ->
              match align with
              | `Left -> Util.Ansi.pad_right ~width:w ' ' line
              | `Right -> Util.Ansi.pad_left ~width:w ' ' line
              | `Center -> Util.Ansi.pad_center ~width:w ' ' line)
            lines
        in
        String.concat "\n" aligned_lines
    | None -> str
  in
  (* Apply vertical alignment/padding if height is set *)
  let str =
    match target_height with
    | Some h ->
        let align = Option.unwrap_or ~default:`Top t.align_vertical in
        let lines = Util.Ansi.split_lines str in
        let current_height = List.length lines in
        if current_height >= h then
          let lines = List.take h lines in
          String.concat "\n" lines
        else
          let padding_needed = h - current_height in
          (* Create empty lines that match the target width (if set) so they show background color *)
          let empty_line =
            match target_width with
            | Some w -> String.make w ' '
            | None -> ""
          in
          (
            match align with
            | `Top ->
                lines @ List.make ~len:padding_needed ~fn:(fun _ -> empty_line)
            | `Bottom ->
                List.make ~len:padding_needed ~fn:(fun _ -> empty_line) @ lines
            | `Center ->
                let top_pad = padding_needed / 2 in
                let bottom_pad = padding_needed - top_pad in
                List.make ~len:top_pad ~fn:(fun _ -> empty_line)
                @ lines
                @ List.make ~len:bottom_pad ~fn:(fun _ -> empty_line)
          ) |> String.concat "\n"
    | None -> str
  in
  (* build formatting sequence *)
  let format_seq =
    Formatter.
      [ (
          if t.blink then
            [ Blink ]
          else
            []
        ); (
          if t.bold then
            [ Bold ]
          else
            []
        ); (
          if t.faint then
            [ Faint ]
          else
            []
        ); (
          if t.italic then
            [ Italic ]
          else
            []
        ); (
          if t.reverse then
            [ Reverse ]
          else
            []
        ); (
          if t.strikethrough then
            [ CrossOut ]
          else
            []
        ); (
          if t.underline then
            [ Underline ]
          else
            []
        ); (
          match t.foreground with
          | Some color when Tty.Color.is_no_color color -> []
          | Some color -> [ Foreground color ]
          | None -> []
        ); (
          match t.background with
          | Some color when Tty.Color.is_no_color color -> []
          | Some color -> [ Background color ]
          | None -> []
        );  ]
    |> List.flatten
  in
  (* render core text *)
  let str =
    let lines = String.split_on_char '\n' str in
    List.map
      (fun line ->
        Formatter.format_string format_seq line)
      lines |> String.concat "\n"
  in
  (* handle border *)
  let str =
    match t.border with
    | Some border -> Border.build_border border str
    | None -> str
  in
  (* handle margin *)
  let str = Cell.create str in
  if t.margin_left > 0 then
    Cell.set str (String.make t.margin_left ' ' ^ Cell.get str);
  if t.margin_right > 0 then
    Cell.set str (Cell.get str ^ String.make t.margin_right ' ');
  if t.margin_top > 0 then
    Cell.set str (String.make t.margin_top '\n' ^ Cell.get str);
  if t.margin_bottom > 0 then
    Cell.set str (Cell.get str ^ String.make t.margin_bottom '\n');
  (
    match t.constraints.max_height with
    | Some max_height when max_height > 0 ->
        let lines = String.split_on_char '\n' (Cell.get str) in
        let lines = List.take max_height lines in
        Cell.set str (String.concat "\n" lines)
    | _ -> ()
  );
  (
    match t.constraints.max_width with
    | Some max_width when max_width > 0 ->
        let lines = Util.Ansi.split_lines (Cell.get str) in
        let truncated =
          List.map
            (fun line ->
              if Util.Ansi.width line > max_width then
                Util.Ansi.truncate ~width:max_width ~ellipsis:"…" line
              else
                line)
            lines
        in
        Cell.set str (String.concat "\n" truncated)
    | _ -> ()
  );
  (
    match t.overflow with
    | Hidden ->
        (* Clip to target dimensions if set *)
        (
          match target_height with
          | Some h ->
              let lines = String.split_on_char '\n' (Cell.get str) in
              let lines = List.take h lines in
              Cell.set str (String.concat "\n" lines)
          | None -> ()
        );
        (
          match target_width with
          | Some w ->
              let lines = Util.Ansi.split_lines (Cell.get str) in
              let clipped =
                List.map
                  (fun line ->
                    if Util.Ansi.width line > w then
                      Util.Ansi.truncate ~width:w line
                    else
                      line)
                  lines
              in
              Cell.set str (String.concat "\n" clipped)
          | None -> ()
        )
    | Visible
    | Scroll -> ()
  );
  Cell.get str

let render = fun t str -> do_render t str

(** Accessors for layout system *)
let get_padding_left = fun t -> t.padding_left

let get_padding_right = fun t -> t.padding_right

let get_padding_top = fun t -> t.padding_top

let get_padding_bottom = fun t -> t.padding_bottom

let get_width = fun t -> t.width

let get_height = fun t -> t.height

(** Accessors for rendering system *)
let get_foreground = fun t -> t.foreground

let get_background = fun t -> t.background

let get_bold = fun t -> t.bold

let get_italic = fun t -> t.italic

let get_underline = fun t -> t.underline

let get_strikethrough = fun t -> t.strikethrough

let get_reverse = fun t -> t.reverse
