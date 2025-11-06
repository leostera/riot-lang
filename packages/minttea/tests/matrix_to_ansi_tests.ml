open Std

(** Tests for Matrix -> ANSI conversion (Emitter phase) with exact output assertions *)

module Matrix = Minttea.Render.Matrix
module Ansi = Minttea.Render.Ansi_emitter

(** Test 1: Empty 3x1 matrix produces 3 spaces + reset *)
let test_empty_matrix_3x1 () =
  let matrix = Matrix.create ~width:3 ~height:1 in
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: "   " + reset *)
  let expected = "   \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 2: Single character "X" at (0,0) in 3x1 matrix *)
let test_single_char_x () =
  let matrix = Matrix.create ~width:3 ~height:1 in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char "X");
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: "X  " + reset *)
  let expected = "X  \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 3: Text "Hi" in 2x1 matrix *)
let test_text_hi () =
  let matrix = Matrix.create ~width:2 ~height:1 in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char "H");
  Matrix.set matrix ~x:1 ~y:0 (Matrix.char "i");
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  let expected = "Hi\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 4: Two rows "A" and "B" *)
let test_two_rows () =
  let matrix = Matrix.create ~width:2 ~height:2 in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char "A");
  Matrix.set matrix ~x:0 ~y:1 (Matrix.char "B");
  let output = Ansi.emit matrix ~mode:Ansi.Fullscreen in
  
  (* Expected: "A " + CRLF + "B " + reset *)
  let expected = "A \r\nB \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 5: Bold "B" produces bold ANSI code *)
let test_bold_b () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let cell = Matrix.char_styled "B" ~bold:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + bold + "B" + final reset *)
  let expected = "\x1b[0m\x1b[1mB\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 6: Italic "I" produces italic ANSI code *)
let test_italic_i () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let cell = Matrix.char_styled "I" ~italic:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + italic + "I" + final reset *)
  let expected = "\x1b[0m\x1b[3mI\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 7: Underline "U" produces underline ANSI code *)
let test_underline_u () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let cell = Matrix.char_styled "U" ~underline:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + underline + "U" + final reset *)
  let expected = "\x1b[0m\x1b[4mU\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 8: Strikethrough "S" produces strikethrough ANSI code *)
let test_strikethrough_s () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let cell = Matrix.char_styled "S" ~strikethrough:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + strikethrough + "S" + final reset *)
  let expected = "\x1b[0m\x1b[9mS\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 9: Reverse "R" produces reverse ANSI code *)
let test_reverse_r () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let cell = Matrix.char_styled "R" ~reverse:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + reverse + "R" + final reset *)
  let expected = "\x1b[0m\x1b[7mR\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 10: Red foreground produces RGB foreground code *)
let test_red_foreground () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let red = Minttea.Style.color "#FF0000" in
  let cell = Matrix.char_fg "X" red in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + red fg + "X" + final reset *)
  let expected = "\x1b[0m\x1b[38;2;255;0;0mX\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 11: Blue background produces RGB background code *)
let test_blue_background () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let blue = Minttea.Style.color "#0000FF" in
  let cell = Matrix.char_bg "X" blue in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + blue bg + "X" + final reset *)
  let expected = "\x1b[0m\x1b[48;2;0;0;255mX\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 12: Red fg + Blue bg together *)
let test_red_fg_blue_bg () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let red = Minttea.Style.color "#FF0000" in
  let blue = Minttea.Style.color "#0000FF" in
  let cell = Matrix.char_fg_bg "X" red blue in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + red fg + blue bg + "X" + final reset *)
  let expected = "\x1b[0m\x1b[38;2;255;0;0m\x1b[48;2;0;0;255mX\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 13: Bold + Red foreground *)
let test_bold_red () =
  let matrix = Matrix.create ~width:1 ~height:1 in
  let red = Minttea.Style.color "#FF0000" in
  let cell = Matrix.char_styled "X" ~fg:(Some red) ~bold:true () in
  Matrix.set matrix ~x:0 ~y:0 cell;
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: reset + red fg + bold + "X" + final reset *)
  let expected = "\x1b[0m\x1b[38;2;255;0;0m\x1b[1mX\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 14: ContentFit with 3 rows emits 2 newlines *)
let test_contentfit_3_rows () =
  let matrix = Matrix.create ~width:1 ~height:3 in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char "A");
  Matrix.set matrix ~x:0 ~y:1 (Matrix.char "B");
  Matrix.set matrix ~x:0 ~y:2 (Matrix.char "C");
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Expected: "A" + CRLF + "B" + CRLF + "C" + reset *)
  let expected = "A\r\nB\r\nC\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 15: Fullscreen 2x2 emits all rows *)
let test_fullscreen_2x2 () =
  let matrix = Matrix.create ~width:2 ~height:2 in
  let output = Ansi.emit matrix ~mode:Ansi.Fullscreen in
  
  (* Expected: 2 spaces + CRLF + 2 spaces + reset *)
  let expected = "  \r\n  \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 16: ContentFit with only row 2 having content emits rows 0-2 *)
let test_contentfit_skips_to_last_content () =
  let matrix = Matrix.create ~width:1 ~height:5 in
  Matrix.set matrix ~x:0 ~y:2 (Matrix.char "X");
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Should emit rows 0,1,2 (space, space, X) *)
  let expected = " \r\n \r\nX\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 17: Two cells with different colors *)
let test_two_colored_cells () =
  let matrix = Matrix.create ~width:2 ~height:1 in
  let red = Minttea.Style.color "#FF0000" in
  let blue = Minttea.Style.color "#0000FF" in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char_fg "R" red);
  Matrix.set matrix ~x:1 ~y:0 (Matrix.char_fg "B" blue);
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* First cell: reset + red fg + "R" *)
  (* Second cell: reset + blue fg + "B" (new reset because color changed) *)
  (* Final: reset *)
  let expected = "\x1b[0m\x1b[38;2;255;0;0mR\x1b[0m\x1b[38;2;0;0;255mB\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 18: Style change only emits new codes *)
let test_style_change_optimization () =
  let matrix = Matrix.create ~width:2 ~height:1 in
  let red = Minttea.Style.color "#FF0000" in
  Matrix.set matrix ~x:0 ~y:0 (Matrix.char_fg "A" red);
  Matrix.set matrix ~x:1 ~y:0 (Matrix.char_fg "B" red);
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* First cell: reset + red + "A" *)
  (* Second cell: "B" (no reset needed, same color) *)
  (* Final: reset *)
  let expected = "\x1b[0m\x1b[38;2;255;0;0mAB\x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 19: Empty 5x1 matrix *)
let test_empty_5x1 () =
  let matrix = Matrix.create ~width:5 ~height:1 in
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  let expected = "     \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 20: ContentFit with empty matrix emits 1 line *)
let test_contentfit_empty_emits_one_line () =
  let matrix = Matrix.create ~width:3 ~height:5 in
  let output = Ansi.emit matrix ~mode:Ansi.ContentFit in
  
  (* Even with no content, ContentFit emits at least 1 line *)
  let expected = "   \x1b[0m" in
  if output = expected then Ok ()
  else Error (format "Expected %S, got %S" expected output)

(** Test 21: Blue box full screen 40x50 - Fullscreen mode emits 50 lines with blue background *)
let test_blue_box_full_screen () =
  let matrix = Matrix.create ~width:40 ~height:50 in
  let blue = Minttea.Style.color "#0000FF" in
  
  (* Fill entire matrix with blue background *)
  for y = 0 to 49 do
    for x = 0 to 39 do
      Matrix.set matrix ~x ~y (Matrix.char_bg " " blue)
    done
  done;
  
  let output = Ansi.emit matrix ~mode:Ansi.Fullscreen in
  
  (* Expected pattern:
     - Line 0: \x1b[0m\x1b[48;2;0;0;255m + 40 spaces + \r\n
     - Lines 1-48: 40 spaces + \r\n (style doesn't change, so no codes)
     - Line 49: 40 spaces + \x1b[0m (final reset, no CRLF)
  *)
  let blue_bg_code = "\x1b[0m\x1b[48;2;0;0;255m" in
  let spaces_40 = String.make 40 ' ' in
  let first_line = blue_bg_code ^ spaces_40 ^ "\r\n" in
  let middle_lines = List.init 48 (fun _ -> spaces_40 ^ "\r\n") in
  let last_line = spaces_40 ^ "\x1b[0m" in
  let expected = first_line ^ String.concat "" middle_lines ^ last_line in
  
  if output = expected then Ok ()
  else 
    let output_len = String.length output in
    let expected_len = String.length expected in
    Error (format "Expected length %d, got %d. First 100 chars: %S" 
      expected_len output_len (String.sub output 0 (min 100 output_len)))

let tests =
  Test.[
    case "empty 3x1" test_empty_matrix_3x1;
    case "single char x" test_single_char_x;
    case "text hi" test_text_hi;
    case "two rows" test_two_rows;
    case "bold b" test_bold_b;
    case "italic i" test_italic_i;
    case "underline u" test_underline_u;
    case "strikethrough s" test_strikethrough_s;
    case "reverse r" test_reverse_r;
    case "red foreground" test_red_foreground;
    case "blue background" test_blue_background;
    case "red fg blue bg" test_red_fg_blue_bg;
    case "bold red" test_bold_red;
    case "contentfit 3 rows" test_contentfit_3_rows;
    case "fullscreen 2x2" test_fullscreen_2x2;
    case "contentfit last content" test_contentfit_skips_to_last_content;
    case "two colored cells" test_two_colored_cells;
    case "style change optimization" test_style_change_optimization;
    case "empty 5x1" test_empty_5x1;
    case "contentfit empty one line" test_contentfit_empty_emits_one_line;
    case "blue box full screen" test_blue_box_full_screen;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"matrix-to-ansi" ~tests ~args)
    ~args:Env.args ()
