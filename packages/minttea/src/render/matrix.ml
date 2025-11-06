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

(** Helper: Create a cell with just a character *)
let char c = { empty_cell with char = c }

(** Helper: Create a cell with character and foreground color *)
let char_fg c fg = { empty_cell with char = c; fg = Some fg }

(** Helper: Create a cell with character and background color *)
let char_bg c bg = { empty_cell with char = c; bg = Some bg }

(** Helper: Create a cell with character and both colors *)
let char_fg_bg c fg bg = { empty_cell with char = c; fg = Some fg; bg = Some bg }

(** Helper: Create a cell with character and style attributes *)
let char_styled c ?(fg = None) ?(bg = None) ?(bold = false) ?(italic = false) 
    ?(underline = false) ?(strikethrough = false) ?(reverse = false) () =
  { char = c; fg; bg; bold; italic; underline; strikethrough; reverse }

(** Create a matrix from a 2D array of strings (each string is a character) *)
let of_char_array arr =
  let height = Array.length arr in
  if height = 0 then create ~width:0 ~height:0
  else
    let width = Array.length arr.(0) in
    let matrix = create ~width ~height in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if x < Array.length arr.(y) then
          matrix.cells.(y).(x) <- { empty_cell with char = arr.(y).(x) }
      done
    done;
    matrix

(** Create a matrix from a 2D array of cells *)
let of_cell_array arr =
  let height = Array.length arr in
  if height = 0 then create ~width:0 ~height:0
  else
    let width = Array.length arr.(0) in
    let matrix = create ~width ~height in
    for y = 0 to height - 1 do
      for x = 0 to width - 1 do
        if x < Array.length arr.(y) then
          matrix.cells.(y).(x) <- arr.(y).(x)
      done
    done;
    matrix

(** Compare two cells for equality *)
let cell_equal c1 c2 =
  c1.char = c2.char &&
  c1.fg = c2.fg &&
  c1.bg = c2.bg &&
  c1.bold = c2.bold &&
  c1.italic = c2.italic &&
  c1.underline = c2.underline &&
  c1.strikethrough = c2.strikethrough &&
  c1.reverse = c2.reverse

(** Compare two matrices for equality *)
let equal t1 t2 =
  if t1.width <> t2.width || t1.height <> t2.height then false
  else
    let rec check_cells y x =
      if y >= t1.height then true
      else if x >= t1.width then check_cells (y + 1) 0
      else
        let c1 = t1.cells.(y).(x) in
        let c2 = t2.cells.(y).(x) in
        if cell_equal c1 c2 then check_cells y (x + 1)
        else false
    in
    check_cells 0 0

(** Get a human-readable diff between two matrices *)
let diff t1 t2 =
  if t1.width <> t2.width || t1.height <> t2.height then
    format "Dimension mismatch: %dx%d vs %dx%d" t1.width t1.height t2.width t2.height
  else
    let buf = Buffer.create 256 in
    let has_diff = ref false in
    for y = 0 to t1.height - 1 do
      for x = 0 to t1.width - 1 do
        let c1 = t1.cells.(y).(x) in
        let c2 = t2.cells.(y).(x) in
        if not (cell_equal c1 c2) then begin
          has_diff := true;
          Buffer.add_string buf (format "\nAt (%d,%d): expected '%s' but got '%s'"
            x y c2.char c1.char);
          if c1.fg <> c2.fg then
            Buffer.add_string buf (format " [fg mismatch]");
          if c1.bg <> c2.bg then
            Buffer.add_string buf (format " [bg mismatch]");
          if c1.bold <> c2.bold then
            Buffer.add_string buf (format " [bold: %b vs %b]" c1.bold c2.bold);
        end
      done
    done;
    if !has_diff then Buffer.contents buf
    else "Matrices are equal"
