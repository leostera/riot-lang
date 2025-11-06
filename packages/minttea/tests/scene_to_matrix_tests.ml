open Std

(** Structural tests for Scene -> Matrix (Painter phase) with precise matrix assertions *)

module Scene = Minttea.Render.Scene
module Matrix = Minttea.Render.Matrix  
module Painter = Minttea.Render.Painter
module S = Minttea.Style

(** Helper: Create a scene rect *)
let make_rect ~x ~y ~width ~height =
  Scene.{x; y; width; height}

(** Helper: Create default scene style *)
let default_style = Scene.{
  fg = None;
  bg = None;
  bold = false;
  italic = false;
  underline = false;
  strikethrough = false;
  reverse = false;
}

(** Test 1: Empty container paints nothing *)
let test_empty_container () =
  let rect = make_rect ~x:0 ~y:0 ~width:3 ~height:2 in
  let node = Scene.container ~rect ~z_index:0 [] in
  let matrix = Matrix.create ~width:3 ~height:2 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| " "; " "; " " |];
    [| " "; " "; " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 2: Container with blue background fills 3x2 rect *)
let test_container_with_background () =
  let blue = S.color "#0000FF" in
  let style = {default_style with bg = Some blue} in
  let rect = make_rect ~x:0 ~y:0 ~width:3 ~height:2 in
  let node = Scene.container ~rect ~z_index:0 ~style [] in
  let matrix = Matrix.create ~width:3 ~height:2 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let b = Matrix.char_bg " " blue in
  let expected = Matrix.of_cell_array [|
    [| b; b; b |];
    [| b; b; b |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 3: Container with partial rect at offset *)
let test_container_partial_rect () =
  let blue = S.color "#0000FF" in
  let style = {default_style with bg = Some blue} in
  let rect = make_rect ~x:2 ~y:1 ~width:2 ~height:1 in
  let node = Scene.container ~rect ~z_index:0 ~style [] in
  let matrix = Matrix.create ~width:5 ~height:3 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let s = " " in
  let b = Matrix.char_bg " " blue in
  let expected = Matrix.of_cell_array [|
    [| Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s |];
    [| Matrix.char s; Matrix.char s; b; b; Matrix.char s |];
    [| Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 4: Simple text "Hello" *)
let test_simple_text () =
  let rect = make_rect ~x:0 ~y:0 ~width:5 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "Hello" in
  let matrix = Matrix.create ~width:5 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| "H"; "e"; "l"; "l"; "o" |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 5: Text "Hi" at offset (3,2) *)
let test_text_offset () =
  let rect = make_rect ~x:3 ~y:2 ~width:3 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "Hi" in
  let matrix = Matrix.create ~width:6 ~height:3 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| " "; " "; " "; " "; " "; " " |];
    [| " "; " "; " "; " "; " "; " " |];
    [| " "; " "; " "; "H"; "i"; " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 6: Text with red foreground *)
let test_text_with_foreground () =
  let red = S.color "#FF0000" in
  let style = {default_style with fg = Some red} in
  let rect = make_rect ~x:0 ~y:0 ~width:3 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "X" in
  let matrix = Matrix.create ~width:3 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_fg "X" red; Matrix.char " "; Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 7: Text with blue background *)
let test_text_with_background () =
  let blue = S.color "#0000FF" in
  let style = {default_style with bg = Some blue} in
  let rect = make_rect ~x:0 ~y:0 ~width:2 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "X" in
  let matrix = Matrix.create ~width:2 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_bg "X" blue; Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 8: Two text nodes "AAA" and "BBB" stacked *)
let test_multiple_text_nodes () =
  let rect1 = make_rect ~x:0 ~y:0 ~width:3 ~height:1 in
  let rect2 = make_rect ~x:0 ~y:1 ~width:3 ~height:1 in
  let node1 = Scene.text_node ~rect:rect1 ~z_index:0 ~style:default_style "AAA" in
  let node2 = Scene.text_node ~rect:rect2 ~z_index:0 ~style:default_style "BBB" in
  let matrix = Matrix.create ~width:3 ~height:2 in
  
  let flattened = Scene.sort_by_z [node1; node2] in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| "A"; "A"; "A" |];
    [| "B"; "B"; "B" |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 9: Overlapping containers - blue (z=1) over red (z=0) *)
let test_overlapping_containers () =
  let red = S.color "#FF0000" in
  let blue = S.color "#0000FF" in
  let style_red = {default_style with bg = Some red} in
  let style_blue = {default_style with bg = Some blue} in
  
  let rect1 = make_rect ~x:0 ~y:0 ~width:4 ~height:2 in
  let node1 = Scene.container ~rect:rect1 ~z_index:0 ~style:style_red [] in
  
  let rect2 = make_rect ~x:1 ~y:0 ~width:2 ~height:2 in
  let node2 = Scene.container ~rect:rect2 ~z_index:1 ~style:style_blue [] in
  
  let matrix = Matrix.create ~width:4 ~height:2 in
  let flattened = Scene.sort_by_z [node1; node2] in
  Painter.paint ~matrix ~scene:flattened;
  
  let r = Matrix.char_bg " " red in
  let b = Matrix.char_bg " " blue in
  let expected = Matrix.of_cell_array [|
    [| r; b; b; r |];
    [| r; b; b; r |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 10: Text wrapping "HelloWorld" in 5-wide rect *)
let test_text_wrapping () =
  let rect = make_rect ~x:0 ~y:0 ~width:5 ~height:2 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "HelloWorld" in
  let matrix = Matrix.create ~width:5 ~height:2 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| "H"; "e"; "l"; "l"; "o" |];
    [| "W"; "o"; "r"; "l"; "d" |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 11: Full 40x50 blue container *)
let test_container_full_40x50 () =
  let blue = S.color "#0000FF" in
  let style = {default_style with bg = Some blue} in
  let rect = make_rect ~x:0 ~y:0 ~width:40 ~height:50 in
  let node = Scene.container ~rect ~z_index:0 ~style [] in
  let matrix = Matrix.create ~width:40 ~height:50 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  (* Create expected matrix with all blue cells *)
  let blue_row = Array.make 40 (Matrix.char_bg " " blue) in
  let expected_arr = Array.make 50 blue_row in
  let expected = Matrix.of_cell_array expected_arr in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 12: Text with newlines "A\nB\nC" *)
let test_text_with_newlines () =
  let rect = make_rect ~x:0 ~y:0 ~width:3 ~height:3 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "A\nB\nC" in
  let matrix = Matrix.create ~width:3 ~height:3 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| "A"; " "; " " |];
    [| "B"; " "; " " |];
    [| "C"; " "; " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 13: Bold text *)
let test_bold_text () =
  let style = {default_style with bold = true} in
  let rect = make_rect ~x:0 ~y:0 ~width:2 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "B" in
  let matrix = Matrix.create ~width:2 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_styled "B" ~bold:true (); Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 14: Italic text *)
let test_italic_text () =
  let style = {default_style with italic = true} in
  let rect = make_rect ~x:0 ~y:0 ~width:2 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "I" in
  let matrix = Matrix.create ~width:2 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_styled "I" ~italic:true (); Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 15: Underline text *)
let test_underline_text () =
  let style = {default_style with underline = true} in
  let rect = make_rect ~x:0 ~y:0 ~width:2 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "U" in
  let matrix = Matrix.create ~width:2 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_styled "U" ~underline:true (); Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 16: Container with no style paints nothing *)
let test_container_no_style () =
  let rect = make_rect ~x:0 ~y:0 ~width:3 ~height:2 in
  let node = Scene.container ~rect ~z_index:0 [] in
  let matrix = Matrix.create ~width:3 ~height:2 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| " "; " "; " " |];
    [| " "; " "; " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 17: Text out of bounds doesn't crash *)
let test_text_out_of_bounds () =
  let rect = make_rect ~x:15 ~y:15 ~width:5 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "Out" in
  let matrix = Matrix.create ~width:10 ~height:10 in
  let flattened = Scene.flatten node in
  
  Painter.paint ~matrix ~scene:flattened;
  
  (* Just verify it doesn't crash - matrix should be all spaces *)
  let expected = Matrix.create ~width:10 ~height:10 in
  if Matrix.equal matrix expected then Ok ()
  else Error "Out of bounds painting produced unexpected result"

(** Test 18: Text "A B C" preserves spaces *)
let test_text_with_spaces () =
  let rect = make_rect ~x:0 ~y:0 ~width:5 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style:default_style "A B C" in
  let matrix = Matrix.create ~width:5 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_char_array [|
    [| "A"; " "; "B"; " "; "C" |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 19: Container at specific position (2,1) size 2x2 *)
let test_container_at_position () =
  let green = S.color "#00FF00" in
  let style = {default_style with bg = Some green} in
  let rect = make_rect ~x:2 ~y:1 ~width:2 ~height:2 in
  let node = Scene.container ~rect ~z_index:0 ~style [] in
  let matrix = Matrix.create ~width:5 ~height:3 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let s = " " in
  let g = Matrix.char_bg " " green in
  let expected = Matrix.of_cell_array [|
    [| Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s; Matrix.char s |];
    [| Matrix.char s; Matrix.char s; g; g; Matrix.char s |];
    [| Matrix.char s; Matrix.char s; g; g; Matrix.char s |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 20: Text with foreground and background *)
let test_text_fg_and_bg () =
  let red = S.color "#FF0000" in
  let blue = S.color "#0000FF" in
  let style = {default_style with fg = Some red; bg = Some blue} in
  let rect = make_rect ~x:0 ~y:0 ~width:2 ~height:1 in
  let node = Scene.text_node ~rect ~z_index:0 ~style "X" in
  let matrix = Matrix.create ~width:2 ~height:1 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  let expected = Matrix.of_cell_array [|
    [| Matrix.char_fg_bg "X" red blue; Matrix.char " " |];
  |] in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

(** Test 21: Blue box full screen 40x50 - container with blue background fills entire matrix *)
let test_blue_box_full_screen () =
  let blue = S.color "#0000FF" in
  let style = {default_style with bg = Some blue} in
  let rect = make_rect ~x:0 ~y:0 ~width:40 ~height:50 in
  let node = Scene.container ~rect ~z_index:0 ~style [] in
  let matrix = Matrix.create ~width:40 ~height:50 in
  let flattened = Scene.flatten node in
  Painter.paint ~matrix ~scene:flattened;
  
  (* Create expected matrix: all cells with blue background *)
  let blue_row = Array.init 40 (fun _ -> Matrix.char_bg " " blue) in
  let expected_arr = Array.make 50 blue_row in
  let expected = Matrix.of_cell_array expected_arr in
  
  if Matrix.equal matrix expected then Ok ()
  else Error (format "Matrix mismatch:%s" (Matrix.diff matrix expected))

let tests =
  Test.[
    case "empty container" test_empty_container;
    case "container with background" test_container_with_background;
    case "container partial rect" test_container_partial_rect;
    case "simple text" test_simple_text;
    case "text offset" test_text_offset;
    case "text with foreground" test_text_with_foreground;
    case "text with background" test_text_with_background;
    case "multiple text nodes" test_multiple_text_nodes;
    case "overlapping containers" test_overlapping_containers;
    case "text wrapping" test_text_wrapping;
    case "container full 40x50" test_container_full_40x50;
    case "text with newlines" test_text_with_newlines;
    case "bold text" test_bold_text;
    case "italic text" test_italic_text;
    case "underline text" test_underline_text;
    case "container no style" test_container_no_style;
    case "text out of bounds" test_text_out_of_bounds;
    case "text with spaces" test_text_with_spaces;
    case "container at position" test_container_at_position;
    case "text fg and bg" test_text_fg_and_bg;
    case "blue box full screen" test_blue_box_full_screen;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"scene-to-matrix" ~tests ~args)
    ~args:Env.args ()
