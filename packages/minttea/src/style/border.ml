open Std

let remove_color_sequences = Tty.Escape_seq.strip

let rec create_string = fun n s ->
  if n = 0 then
    ""
  else
    let str = create_string (n - 1) s in
    str ^ s

let utf8_len = fun str ->
  (* TODO: Implement proper grapheme cluster counting *)
  (* For now, use byte length as approximation *)
  String.length (remove_color_sequences str)

let split_lines = fun text ->
  (* Split on \n, handling optional \r before it *)
  String.split_on_char '\n' text |> List.map
    (fun line ->
      if String.length line > 0 && line.[String.length line - 1] = '\r' then
        String.sub line 0 (String.length line - 1)
      else
        line)

let get_width = fun text ->
  List.fold_left
    (fun acc line ->
      let len = utf8_len (remove_color_sequences line) in
      if acc < len then
        len
      else
        acc)
    0
    (split_lines text)

let get_height = fun text -> List.length (split_lines text)

type t = {
  top: string option;
  left: string option;
  bottom: string option;
  right: string option;
  top_left: string option;
  top_right: string option;
  bottom_left: string option;
  bottom_right: string option;
  middle_left: string option;
  middle_right: string option;
  middle: string option;
  middle_top: string option;
  middle_bottom: string option;
}

let make = fun ?top ?left ?bottom ?right ?top_left ?top_right ?bottom_left ?bottom_right ?middle_left ?middle_right ?middle ?middle_top ?middle_bottom () -> {
  top;
  left;
  bottom;
  right;
  top_left;
  top_right;
  bottom_left;
  bottom_right;
  middle_left;
  middle_right;
  middle;
  middle_top;
  middle_bottom;

}

let build_border = fun (border:t) text ->
  let top = Option.unwrap_or ~default:"" border.top in
  let left = Option.unwrap_or ~default:"" border.left in
  let bottom = Option.unwrap_or ~default:"" border.bottom in
  let right = Option.unwrap_or ~default:"" border.right in
  let top_left = Option.unwrap_or ~default:"" border.top_left in
  let top_right = Option.unwrap_or ~default:"" border.top_right in
  let bottom_left = Option.unwrap_or ~default:"" border.bottom_left in
  let bottom_right = Option.unwrap_or ~default:"" border.bottom_right in
  let width = get_width text in
  let top_border = top_left ^ create_string width top ^ top_right in
  let bottom_border = bottom_left ^ create_string width bottom ^ bottom_right in
  let l = split_lines text in
  let l =
    List.map
      (fun x ->
        let x_w = get_width x in
        let extra_right_spacing = create_string (width - x_w) " " in
        let res = left ^ x ^ extra_right_spacing ^ right in
        res)
      l
  in
  let text = String.concat "\n" l in
  top_border ^ "\n" ^ text ^ "\n" ^ bottom_border

let normal = {
  top = Some "─";
  bottom = Some "─";
  left = Some "│";
  right = Some "│";
  top_left = Some "┌";
  top_right = Some "┐";
  bottom_left = Some "└";
  bottom_right = Some "┘";
  middle_left = Some "├";
  middle_right = Some "┤";
  middle = Some "┼";
  middle_top = Some "┬";
  middle_bottom = Some "┴";

}

let rounded = {
  top = Some "─";
  bottom = Some "─";
  left = Some "│";
  right = Some "│";
  top_left = Some "╭";
  top_right = Some "╮";
  bottom_left = Some "╰";
  bottom_right = Some "╯";
  middle_left = Some "├";
  middle_right = Some "┤";
  middle = Some "┼";
  middle_top = Some "┬";
  middle_bottom = Some "┴";

}

let block = {
  top = Some "█";
  bottom = Some "█";
  left = Some "█";
  right = Some "█";
  top_left = Some "█";
  top_right = Some "█";
  bottom_left = Some "█";
  bottom_right = Some "█";
  middle_left = None;
  middle_right = None;
  middle = None;
  middle_top = None;
  middle_bottom = None;

}

let outer_half_block = {
  top = Some "▀";
  bottom = Some "▄";
  left = Some "▌";
  right = Some "▐";
  top_left = Some "▛";
  top_right = Some "▜";
  bottom_left = Some "▙";
  bottom_right = Some "▟";
  middle_left = None;
  middle_right = None;
  middle = None;
  middle_top = None;
  middle_bottom = None;

}

let inner_half_block = {
  top = Some "▄";
  bottom = Some "▀";
  left = Some "▐";
  right = Some "▌";
  top_left = Some "▗";
  top_right = Some "▖";
  bottom_left = Some "▝";
  bottom_right = Some "▘";
  middle_left = None;
  middle_right = None;
  middle = None;
  middle_top = None;
  middle_bottom = None;

}

let thick = {
  top = Some "━";
  bottom = Some "━";
  left = Some "┃";
  right = Some "┃";
  top_left = Some "┏";
  top_right = Some "┓";
  bottom_left = Some "┗";
  bottom_right = Some "┛";
  middle_left = Some "┣";
  middle_right = Some "┫";
  middle = Some "╋";
  middle_top = Some "┳";
  middle_bottom = Some "┻";

}

let double = {
  top = Some "═";
  bottom = Some "═";
  left = Some "║";
  right = Some "║";
  top_left = Some "╔";
  top_right = Some "╗";
  bottom_left = Some "╚";
  bottom_right = Some "╝";
  middle_left = Some "╠";
  middle_right = Some "╣";
  middle = Some "╬";
  middle_top = Some "╦";
  middle_bottom = Some "╩";

}

let hidden = {
  top = Some " ";
  bottom = Some " ";
  left = Some " ";
  right = Some " ";
  top_left = Some " ";
  top_right = Some " ";
  bottom_left = Some " ";
  bottom_right = Some " ";
  middle_left = Some " ";
  middle_right = Some " ";
  middle = Some " ";
  middle_top = Some " ";
  middle_bottom = Some " ";

}
