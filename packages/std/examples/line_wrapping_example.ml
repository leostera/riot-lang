(** Line Wrapping Example - Demonstrates text wrapping for terminal display *)

open Std
open Std.Collections

let () =
  println "=== Line Wrapping Example ===\n";
  
  (* Example 1: Basic English text *)
  println "1. Wrapping English text to 40 characters:";
  let text1 = "The quick brown fox jumps over the lazy dog. This is a sample sentence to demonstrate line wrapping." in
  println ("   Original: " ^ text1);
  println ("   Width: " ^ Int.to_string (String.width text1) ^ " characters\n");
  let wrapped1 = Unicode.Segmentation.wrap_lines ~width:40 text1 in
  println "   Wrapped lines:";
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line ^ " (width: " ^ Int.to_string (String.width line) ^ ")")
  ) wrapped1;
  println "";
  
  (* Example 2: Text with punctuation *)
  println "2. Wrapping text with punctuation (width 30):";
  let text2 = "Hello, world! How are you? I'm fine, thank you. What about you?" in
  let wrapped2 = Unicode.Segmentation.wrap_lines ~width:30 text2 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line)
  ) wrapped2;
  println "";
  
  (* Example 3: CJK text *)
  println "3. Wrapping CJK text (width 20):";
  let text3 = "你好世界！这是一个测试句子。" in
  println ("   Original: " ^ text3 ^ " (width: " ^ Int.to_string (String.width text3) ^ ")");
  let wrapped3 = Unicode.Segmentation.wrap_lines ~width:20 text3 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line ^ " (width: " ^ Int.to_string (String.width line) ^ ")")
  ) wrapped3;
  println "";
  
  (* Example 4: Mixed content *)
  println "4. Wrapping mixed English and CJK (width 35):";
  let text4 = "Hello 世界! This is mixed content with 中文 and English text." in
  let wrapped4 = Unicode.Segmentation.wrap_lines ~width:35 text4 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line ^ " (width: " ^ Int.to_string (String.width line) ^ ")")
  ) wrapped4;
  println "";
  
  (* Example 5: Code with identifiers *)
  println "5. Wrapping code (width 50):";
  let text5 = "let my_function_name = calculate_something(foo_bar_baz, another_parameter) in result" in
  let wrapped5 = Unicode.Segmentation.wrap_lines ~width:50 text5 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line)
  ) wrapped5;
  println "";
  
  (* Example 6: Text with mandatory breaks *)
  println "6. Text with newlines (preserves mandatory breaks):";
  let text6 = "First line\nSecond line with more text that needs wrapping\nThird line" in
  let wrapped6 = Unicode.Segmentation.wrap_lines ~width:30 text6 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line)
  ) wrapped6;
  println "";
  
  (* Example 7: Log output *)
  println "7. Wrapping log output (width 60):";
  let log = "[2024-11-01 08:15:23] INFO: Processing request from user@example.com with parameters: foo=bar, baz=qux, very_long_parameter_name=some_value" in
  let wrapped_log = Unicode.Segmentation.wrap_lines ~width:60 log in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line)
  ) wrapped_log;
  println "";
  
  (* Example 8: Very narrow width *)
  println "8. Narrow width wrapping (width 20):";
  let text8 = "supercalifragilisticexpialidocious" in
  println ("   Original: " ^ text8);
  let wrapped8 = Unicode.Segmentation.wrap_lines ~width:20 text8 in
  List.iteri (fun i line ->
    let num = if i+1 < 10 then " " ^ Int.to_string (i+1) else Int.to_string (i+1) in
    println ("   " ^ num ^ "| " ^ line)
  ) wrapped8;
  println "";
  
  (* Example 9: Simulating terminal pager *)
  println "9. Simulating terminal pager view (width 70):";
  let doc = "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris." in
  let pager_lines = Unicode.Segmentation.wrap_lines ~width:70 doc in
  println "   ┌──────────────────────────────────────────────────────────────────────┐";
  List.iter (fun line ->
    let padding = String.make (68 - String.length line) ' ' in
    println ("   │ " ^ line ^ padding ^ " │")
  ) pager_lines;
  println "   └──────────────────────────────────────────────────────────────────────┘";
  println "";
  
  println "Line wrapping example completed!"
