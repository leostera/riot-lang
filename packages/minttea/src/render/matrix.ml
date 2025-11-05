open Std

type cell = {
  char : string;
  fg : Tty.Color.t option;
  bg : Tty.Color.t option;
  bold : bool;
  italic : bool;
  underline : bool;
  strikethrough : bool;
  reverse : bool;
}

let empty_cell = {
  char = " ";
  fg = None;
  bg = None;
  bold = false;
  italic = false;
  underline = false;
  strikethrough = false;
  reverse = false;
}

type t = {
  width : int;
  height : int;
  cells : cell array array;
}

let create ~width ~height =
  Log.debug "[MATRIX] create() called with %dx%d" width height;
  let cells = Array.make_matrix height width empty_cell in
  { width; height; cells }

let get t ~x ~y =
  if x >= 0 && x < t.width && y >= 0 && y < t.height then
    Some t.cells.(y).(x)
  else
    None

let set t ~x ~y cell =
  if x >= 0 && x < t.width && y >= 0 && y < t.height then
    t.cells.(y).(x) <- cell

let fill_rect t ~x ~y ~width ~height cell =
  for row = y to Int.min (y + height - 1) (t.height - 1) do
    for col = x to Int.min (x + width - 1) (t.width - 1) do
      if row >= 0 && col >= 0 then
        t.cells.(row).(col) <- cell
    done
  done

let write_text t ~x ~y ~max_width text cell =
  (* Split text into grapheme clusters for proper UTF-8 handling *)
  let chars = String.to_seq text |> List.of_seq |> List.map (String.make 1) in
  let rec write_chars col chars_left =
    match chars_left with
    | [] -> ()
    | c :: rest ->
        if col >= x + max_width || col >= t.width then ()
        else begin
          set t ~x:col ~y { cell with char = c };
          write_chars (col + 1) rest
        end
  in
  if y >= 0 && y < t.height then
    write_chars x chars

let clear t =
  for row = 0 to t.height - 1 do
    for col = 0 to t.width - 1 do
      t.cells.(row).(col) <- empty_cell
    done
  done

let copy t =
  let cells = Array.map Array.copy t.cells in
  { width = t.width; height = t.height; cells }
