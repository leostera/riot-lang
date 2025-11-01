(** Line Wrapping Example - Demonstrates text wrapping for terminal display *)

open Std

let () =
  print_endline "=== Line Wrapping Example ===\n";
  
  (* Example 1: Basic English text *)
  print_endline "1. Wrapping English text to 40 characters:";
  let text1 = "The quick brown fox jumps over the lazy dog. This is a sample sentence to demonstrate line wrapping." in
  Printf.printf "   Original: %s\n" text1;
  Printf.printf "   Width: %d characters\n\n" (String.width text1);
  let wrapped1 = Unicode.Segmentation.wrap_lines ~width:40 text1 in
  print_endline "   Wrapped lines:";
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s (width: %d)\n" (i+1) line (String.width line)
  ) wrapped1;
  print_endline "";
  
  (* Example 2: Text with punctuation *)
  print_endline "2. Wrapping text with punctuation (width 30):";
  let text2 = "Hello, world! How are you? I'm fine, thank you. What about you?" in
  let wrapped2 = Unicode.Segmentation.wrap_lines ~width:30 text2 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s\n" (i+1) line
  ) wrapped2;
  print_endline "";
  
  (* Example 3: CJK text *)
  print_endline "3. Wrapping CJK text (width 20):";
  let text3 = "你好世界！这是一个测试句子。" in
  Printf.printf "   Original: %s (width: %d)\n" text3 (String.width text3);
  let wrapped3 = Unicode.Segmentation.wrap_lines ~width:20 text3 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s (width: %d)\n" (i+1) line (String.width line)
  ) wrapped3;
  print_endline "";
  
  (* Example 4: Mixed content *)
  print_endline "4. Wrapping mixed English and CJK (width 35):";
  let text4 = "Hello 世界! This is mixed content with 中文 and English text." in
  let wrapped4 = Unicode.Segmentation.wrap_lines ~width:35 text4 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s (width: %d)\n" (i+1) line (String.width line)
  ) wrapped4;
  print_endline "";
  
  (* Example 5: Code with identifiers *)
  print_endline "5. Wrapping code (width 50):";
  let text5 = "let my_function_name = calculate_something(foo_bar_baz, another_parameter) in result" in
  let wrapped5 = Unicode.Segmentation.wrap_lines ~width:50 text5 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s\n" (i+1) line
  ) wrapped5;
  print_endline "";
  
  (* Example 6: Text with mandatory breaks *)
  print_endline "6. Text with newlines (preserves mandatory breaks):";
  let text6 = "First line\nSecond line with more text that needs wrapping\nThird line" in
  let wrapped6 = Unicode.Segmentation.wrap_lines ~width:30 text6 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s\n" (i+1) line
  ) wrapped6;
  print_endline "";
  
  (* Example 7: Log output *)
  print_endline "7. Wrapping log output (width 60):";
  let log = "[2024-11-01 08:15:23] INFO: Processing request from user@example.com with parameters: foo=bar, baz=qux, very_long_parameter_name=some_value" in
  let wrapped_log = Unicode.Segmentation.wrap_lines ~width:60 log in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s\n" (i+1) line
  ) wrapped_log;
  print_endline "";
  
  (* Example 8: Very narrow width *)
  print_endline "8. Narrow width wrapping (width 20):";
  let text8 = "supercalifragilisticexpialidocious" in
  Printf.printf "   Original: %s\n" text8;
  let wrapped8 = Unicode.Segmentation.wrap_lines ~width:20 text8 in
  List.iteri (fun i line ->
    Printf.printf "   %2d| %s\n" (i+1) line
  ) wrapped8;
  print_endline "";
  
  (* Example 9: Simulating terminal pager *)
  print_endline "9. Simulating terminal pager view (width 70):";
  let doc = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris." in
  let pager_lines = Unicode.Segmentation.wrap_lines ~width:70 doc in
  print_endline "   ┌──────────────────────────────────────────────────────────────────────┐";
  List.iter (fun line ->
    Printf.printf "   │ %-68s │\n" line
  ) pager_lines;
  print_endline "   └──────────────────────────────────────────────────────────────────────┘";
  print_endline "";
  
  print_endline "Line wrapping example completed!"
