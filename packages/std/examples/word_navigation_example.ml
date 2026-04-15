(** Word Navigation Example - Demonstrates Ctrl+Arrow style navigation *)
open Std
open Std.Collections

let () =
  println "=== Word Navigation Example ===\n";
  (* Example 1: Basic English text *)
  println "1. Basic English text:";
  let s1 = "Hello world, how are you?" in
  println ("   Text: " ^ s1);
  let boundaries = String.word_boundaries s1 in
  println
    ("   Word boundaries at byte positions: ["
    ^ String.concat "; " (List.map boundaries ~fn:Int.to_string)
    ^ "]");
  let words = String.split_words s1 in
  println
    ("   Words: [" ^ String.concat "; " (List.map words ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]\n");
  (* Example 2: Contractions *)
  println "2. Contractions (don't split apostrophes):";
  let s2 = "don't it's can't" in
  println ("   Text: " ^ s2);
  let words2 = String.split_words s2 in
  println
    ("   Words: [" ^ String.concat "; " (List.map words2 ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]\n");
  (* Example 3: snake_case identifiers *)
  println "3. Programming identifiers (keep underscores):";
  let s3 = "hello_world foo_bar_baz" in
  println ("   Text: " ^ s3);
  let words3 = String.split_words s3 in
  println
    ("   Words: [" ^ String.concat "; " (List.map words3 ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]\n");
  (* Example 4: Numbers and hex codes *)
  println "4. Numbers (don't break letters and numbers):";
  let s4 = "123 abc123 0xDEADBEEF" in
  println ("   Text: " ^ s4);
  let words4 = String.split_words s4 in
  println
    ("   Words: [" ^ String.concat "; " (List.map words4 ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]\n");
  (* Example 5: CJK text (each character is a word) *)
  println "5. CJK text (each character is typically a word):";
  let s5 = "你好世界" in
  println ("   Text: " ^ s5);
  let words5 = String.split_words s5 in
  println ("   Words: [" ^ String.concat "; " (List.map words5 ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]");
  println "   (Each CJK character is treated as a separate word)\n";
  (* Example 6: Mixed content *)
  println "6. Mixed content:";
  let s6 = "Hello 世界! foo_bar 123" in
  println ("   Text: " ^ s6);
  let words6 = String.split_words s6 in
  println
    ("   Words: [" ^ String.concat "; " (List.map words6 ~fn:(fun w -> "\"" ^ w ^ "\"")) ^ "]\n");
  (* Example 7: Simulating Ctrl+Right navigation *)
  println "7. Simulating Ctrl+Right arrow navigation:";
  let text = "The quick brown fox" in
  println ("   Text: \"" ^ text ^ "\"");
  println "   Cursor positions for Ctrl+Right from start:";
  let rec show_navigation pos =
    if pos >= String.length text then
      ()
    else
      let next = Unicode.Segmentation.find_next_word_start text pos in
      let word_part = String.sub text ~offset:pos ~len:(min (next - pos) (String.length text - pos)) in
      let pos_str =
        if pos < 10 then
          " " ^ Int.to_string pos
        else
          Int.to_string pos
      in
      let next_str =
        if next < 10 then
          " " ^ Int.to_string next
        else
          Int.to_string next
      in
      println ("     Position " ^ pos_str ^ " -> " ^ next_str ^ ": \"" ^ word_part ^ "\"");
      show_navigation next
  in
  show_navigation 0;
  println "";
  (* Example 8: Simulating Ctrl+Left navigation *)
  println "8. Simulating Ctrl+Left arrow navigation:";
  println ("   Text: \"" ^ text ^ "\"");
  println "   Cursor positions for Ctrl+Left from end:";
  let rec show_back_navigation pos =
    if pos <= 0 then
      ()
    else
      let prev = Unicode.Segmentation.find_prev_word_start text pos in
      let word_part = String.sub text ~offset:prev ~len:(pos - prev) in
      let pos_str =
        if pos < 10 then
          " " ^ Int.to_string pos
        else
          Int.to_string pos
      in
      let prev_str =
        if prev < 10 then
          " " ^ Int.to_string prev
        else
          Int.to_string prev
      in
      println ("     Position " ^ pos_str ^ " -> " ^ prev_str ^ ": \"" ^ word_part ^ "\"");
      show_back_navigation prev
  in
  show_back_navigation (String.length text);
  println "";
  println "Word navigation example completed!"
