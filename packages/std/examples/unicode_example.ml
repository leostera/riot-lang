(** Unicode Example - Demonstrates Unicode text processing capabilities *)

open Std

let () =
  print_endline "=== Std.Unicode Example ===\n";
  
  (* Example 1: Basic ASCII *)
  print_endline "1. Basic ASCII string:";
  let s1 = "Hello" in
  Printf.printf "   String: %s\n" s1;
  Printf.printf "   Display width: %d\n" (String.width s1);
  Printf.printf "   Grapheme count: %d\n" (String.grapheme_count s1);
  Printf.printf "   Rune count: %d\n\n" (String.rune_count s1);
  
  (* Example 2: CJK characters *)
  print_endline "2. CJK (Chinese/Japanese/Korean) characters:";
  let s2 = "你好世界" in
  Printf.printf "   String: %s\n" s2;
  Printf.printf "   Display width: %d (each CJK char is width 2)\n" (String.width s2);
  Printf.printf "   Grapheme count: %d\n" (String.grapheme_count s2);
  Printf.printf "   Rune count: %d\n\n" (String.rune_count s2);
  
  (* Example 3: Emoji *)
  print_endline "3. Emoji characters:";
  let s3 = "👍🎉🚀" in
  Printf.printf "   String: %s\n" s3;
  Printf.printf "   Display width: %d (each emoji is width 2)\n" (String.width s3);
  Printf.printf "   Grapheme count: %d\n" (String.grapheme_count s3);
  Printf.printf "   Rune count: %d\n\n" (String.rune_count s3);
  
  (* Example 4: Mixed content *)
  print_endline "4. Mixed ASCII, CJK, and emoji:";
  let s4 = "Hello 世界! 👋" in
  Printf.printf "   String: %s\n" s4;
  Printf.printf "   Display width: %d\n" (String.width s4);
  Printf.printf "   Grapheme count: %d\n" (String.grapheme_count s4);
  Printf.printf "   Rune count: %d\n\n" (String.rune_count s4);
  
  (* Example 5: Individual rune width *)
  print_endline "5. Individual character widths:";
  let test_char c_code name =
    let r = Unicode.Rune.unsafe_of_int c_code in
    let w = Unicode.Rune.width r in
    Printf.printf "   U+%04X (%s): width=%d\n" c_code name w
  in
  test_char 0x0041 "Latin A";
  test_char 0x4E00 "CJK Ideograph";
  test_char 0x0301 "Combining acute";
  test_char 0x1F44D "Thumbs up emoji";
  test_char 0x200D "Zero-width joiner";
  print_endline "";
  
  (* Example 6: UTF-8 operations *)
  print_endline "6. UTF-8 encoding/decoding:";
  let s = "Hello" in
  (match Unicode.Utf8.decode_rune s 0 with
   | Some (r, next_pos) ->
       Printf.printf "   First rune of '%s': U+%04X at byte position %d\n" 
         s (Unicode.Rune.to_int r) next_pos
   | None -> print_endline "   Failed to decode");
  Printf.printf "   Is valid UTF-8: %b\n\n" (Unicode.Utf8.is_valid s);
  
  (* Example 7: String truncation with width *)
  print_endline "7. Truncating strings to display width:";
  let s7 = "Hello 世界 World!" in
  Printf.printf "   Original: '%s' (width: %d)\n" s7 (String.width s7);
  let truncated = String.truncate_width ~width:10 s7 in
  Printf.printf "   Truncated to width 10: '%s' (width: %d)\n\n" 
    truncated (String.width truncated);
  
  (* Example 8: Padding with width awareness *)
  print_endline "8. Padding strings (width-aware):";
  let s8 = "你好" in  (* Width 4 *)
  Printf.printf "   Original: '%s' (width: %d)\n" s8 (String.width s8);
  Printf.printf "   Right-padded to 10: '%s'\n" (String.pad_right ~width:10 ' ' s8);
  Printf.printf "   Center-padded to 10: '%s'\n\n" (String.pad_center ~width:10 ' ' s8);
  
  print_endline "Example completed!"
