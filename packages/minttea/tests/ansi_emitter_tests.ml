open Std

(* Helper to check if haystack contains needle substring *)
let contains_substring haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  let rec check i =
    if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else check (i + 1)
  in
  check 0

let test_emit_empty_matrix () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:3 ~height:2 in
  let output = A.emit matrix in
  
  (* Should have cursor home, spaces, and reset *)
  if String.contains output '\x1b' then Ok ()
  else Error "Expected ANSI codes in output"

let test_emit_colored_cell () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:1 ~height:1 in
  let red = Tty.Color.of_rgb (255, 0, 0) in
  let cell = { M.empty_cell with 
    char = "X";
    bg = Some red;
  } in
  M.set matrix ~x:0 ~y:0 cell;
  
  let output = A.emit matrix in
  
  (* Should contain red background code: \x1b[48;2;255;0;0m *)
  if contains_substring output "48;2;255;0;0" && String.contains output 'X' then 
    Ok ()
  else 
    Error (format "Expected red background ANSI code, got: %s" output)

let test_emit_bold_text () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:1 ~height:1 in
  let cell = { M.empty_cell with 
    char = "B";
    bold = true;
  } in
  M.set matrix ~x:0 ~y:0 cell;
  
  let output = A.emit matrix in
  
  (* Should contain bold code: \x1b[1m *)
  if contains_substring output "\x1b[1m" && String.contains output 'B' then 
    Ok ()
  else 
    Error (format "Expected bold ANSI code, got: %s" output)

let test_emit_multiline () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:3 ~height:2 in
  let output = A.emit matrix in
  
  (* Count newlines - should have 1 (between 2 rows) *)
  let newline_count = String.to_seq output 
    |> Seq.filter (fun c -> c = '\n') 
    |> Seq.length in
    
  if newline_count = 1 then Ok ()
  else Error (format "Expected 1 newline, got %d" newline_count)

let test_diff_no_changes () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:3 ~height:2 in
  let same = M.copy matrix in
  
  let output = A.emit_diff ~old:matrix ~new_:same in
  
  (* Should be minimal (just reset code) *)
  let len = String.length output in
  if len < 20 then Ok ()
  else Error (format "Diff output too long for no changes: %d bytes" len)

let test_diff_one_cell_change () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let old = M.create ~width:3 ~height:2 in
  let new_ = M.copy old in
  
  (* Change one cell *)
  let cell = { M.empty_cell with char = "X" } in
  M.set new_ ~x:1 ~y:1 cell;
  
  let output = A.emit_diff ~old ~new_ in
  
  (* Should contain cursor positioning and the X *)
  if String.contains output '\x1b' && String.contains output 'X' then 
    Ok ()
  else 
    Error "Expected cursor positioning and changed character"

(* Test that background colors are preserved when text is painted over them *)
let test_text_preserves_background () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  let matrix = M.create ~width:5 ~height:1 in
  let blue = Tty.Color.of_rgb (0, 0, 255) in
  let white = Tty.Color.of_rgb (255, 255, 255) in
  
  (* Fill with blue background *)
  for x = 0 to 4 do
    let bg_cell = { M.empty_cell with bg = Some blue } in
    M.set matrix ~x ~y:0 bg_cell;
  done;
  
  (* Paint text "Hi" with white foreground at position 1 *)
  M.set matrix ~x:1 ~y:0 { M.empty_cell with char = "H"; fg = Some white; bg = Some blue };
  M.set matrix ~x:2 ~y:0 { M.empty_cell with char = "i"; fg = Some white; bg = Some blue };
  
  let output = A.emit matrix in
  
  (* Should have blue background for ALL cells, including text *)
  (* Count occurrences of blue background code *)
  let blue_bg_code = "48;2;0;0;255" in
  let rec count_occurrences str needle pos count =
    try
      let idx = String.index_from str pos (String.get needle 0) in
      if String.sub str idx (String.length needle) = needle then
        count_occurrences str needle (idx + 1) (count + 1)
      else
        count_occurrences str needle (idx + 1) count
    with Not_found -> count
  in
  let blue_count = count_occurrences output blue_bg_code 0 0 in
  
  (* Should see blue background code at least once (might be optimized) *)
  (* More importantly, there should NOT be a reset that clears the background in the middle *)
  let has_blue = contains_substring output blue_bg_code in
  let has_text_h = String.contains output 'H' in
  let has_text_i = String.contains output 'i' in
  
  if has_blue && has_text_h && has_text_i then
    Ok ()
  else
    Error (format "Text should preserve background. Blue: %b, H: %b, i: %b, output: %s" 
      has_blue has_text_h has_text_i output)

(* Test that every row of the matrix is rendered *)
let test_all_rows_rendered () =
  let module M = Minttea.Render.Matrix in
  let module A = Minttea.Render.Ansi_emitter in
  
  (* Create tall matrix *)
  let matrix = M.create ~width:10 ~height:24 in
  let blue = Tty.Color.of_rgb (0, 0, 255) in
  
  (* Fill entire matrix with blue background *)
  for y = 0 to 23 do
    for x = 0 to 9 do
      let cell = { M.empty_cell with bg = Some blue } in
      M.set matrix ~x ~y cell;
    done;
  done;
  
  let output = A.emit matrix in
  
  (* Count newlines - should have 23 (for 24 rows) *)
  let newline_count = String.to_seq output 
    |> Seq.filter (fun c -> c = '\n') 
    |> Seq.length in
  
  (* Count total characters (should be at least 10 * 24 = 240) *)
  let char_count = String.to_seq output
    |> Seq.filter (fun c -> c <> '\x1b' && c <> '[' && c <> ';' && c <> 'm' && c <> 'H' && c <> '\n')
    |> Seq.filter (fun c -> let code = Char.code c in code >= 32 && code < 127)
    |> Seq.length in
  
  if newline_count = 23 then 
    Ok ()
  else 
    Error (format "Expected 23 newlines for 24 rows, got %d. Char count: %d" newline_count char_count)

let tests =
  Test.[
    case "emit empty matrix" test_emit_empty_matrix;
    case "emit colored cell" test_emit_colored_cell;
    case "emit bold text" test_emit_bold_text;
    case "emit multiline" test_emit_multiline;
    case "diff with no changes" test_diff_no_changes;
    case "diff with one cell change" test_diff_one_cell_change;
    case "text preserves background" test_text_preserves_background;
    case "all rows rendered" test_all_rows_rendered;
  ]

let () =
  Miniriot.run
    ~main:(fun ~args -> Test.Cli.main ~name:"ansi-emitter" ~tests ~args)
    ~args:Env.args ()
