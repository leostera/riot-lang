(** Word Navigation Example - Demonstrates Ctrl+Arrow style navigation *)

open Std

let () =
  print_endline "=== Word Navigation Example ===\n";
  
  (* Example 1: Basic English text *)
  print_endline "1. Basic English text:";
  let s1 = "Hello world, how are you?" in
  Printf.printf "   Text: %s\n" s1;
  let boundaries = String.word_boundaries s1 in
  Printf.printf "   Word boundaries at byte positions: [%s]\n"
    (String.concat "; " (List.map string_of_int boundaries));
  let words = String.split_words s1 in
  Printf.printf "   Words: [%s]\n\n"
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words));
  
  (* Example 2: Contractions *)
  print_endline "2. Contractions (don't split apostrophes):";
  let s2 = "don't it's can't" in
  Printf.printf "   Text: %s\n" s2;
  let words2 = String.split_words s2 in
  Printf.printf "   Words: [%s]\n\n"
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words2));
  
  (* Example 3: snake_case identifiers *)
  print_endline "3. Programming identifiers (keep underscores):";
  let s3 = "hello_world foo_bar_baz" in
  Printf.printf "   Text: %s\n" s3;
  let words3 = String.split_words s3 in
  Printf.printf "   Words: [%s]\n\n"
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words3));
  
  (* Example 4: Numbers and hex codes *)
  print_endline "4. Numbers (don't break letters and numbers):";
  let s4 = "123 abc123 0xDEADBEEF" in
  Printf.printf "   Text: %s\n" s4;
  let words4 = String.split_words s4 in
  Printf.printf "   Words: [%s]\n\n"
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words4));
  
  (* Example 5: CJK text (each character is a word) *)
  print_endline "5. CJK text (each character is typically a word):";
  let s5 = "你好世界" in
  Printf.printf "   Text: %s\n" s5;
  let words5 = String.split_words s5 in
  Printf.printf "   Words: [%s]\n" 
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words5));
  Printf.printf "   (Each CJK character is treated as a separate word)\n\n";
  
  (* Example 6: Mixed content *)
  print_endline "6. Mixed content:";
  let s6 = "Hello 世界! foo_bar 123" in
  Printf.printf "   Text: %s\n" s6;
  let words6 = String.split_words s6 in
  Printf.printf "   Words: [%s]\n\n"
    (String.concat "; " (List.map (fun w -> "\"" ^ w ^ "\"") words6));
  
  (* Example 7: Simulating Ctrl+Right navigation *)
  print_endline "7. Simulating Ctrl+Right arrow navigation:";
  let text = "The quick brown fox" in
  Printf.printf "   Text: \"%s\"\n" text;
  Printf.printf "   Cursor positions for Ctrl+Right from start:\n";
  let rec show_navigation pos =
    if pos >= String.length text then ()
    else
      let next = Unicode.Segmentation.find_next_word_start text pos in
      let word_part = String.sub text pos (min (next - pos) (String.length text - pos)) in
      Printf.printf "     Position %2d -> %2d: \"%s\"\n" pos next word_part;
      show_navigation next
  in
  show_navigation 0;
  print_endline "";
  
  (* Example 8: Simulating Ctrl+Left navigation *)
  print_endline "8. Simulating Ctrl+Left arrow navigation:";
  Printf.printf "   Text: \"%s\"\n" text;
  Printf.printf "   Cursor positions for Ctrl+Left from end:\n";
  let rec show_back_navigation pos =
    if pos <= 0 then ()
    else
      let prev = Unicode.Segmentation.find_prev_word_start text pos in
      let word_part = String.sub text prev (pos - prev) in
      Printf.printf "     Position %2d -> %2d: \"%s\"\n" pos prev word_part;
      show_back_navigation prev
  in
  show_back_navigation (String.length text);
  print_endline "";
  
  print_endline "Word navigation example completed!"
