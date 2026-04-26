(** Line Wrapping Example - Demonstrates text wrapping for terminal display *)
open Std
open Std.Collections

let main ~args:_ =
  let print_numbered_lines lines ~show_width =
    List.for_each
      (List.enumerate lines)
      ~fn:(fun (i, line) ->
        let num =
          if i + 1 < 10 then
            " " ^ Int.to_string (i + 1)
          else
            Int.to_string (i + 1)
        in
        let suffix =
          if show_width then
            " (width: " ^ Int.to_string (String.width line) ^ ")"
          else
            ""
        in
        println ("   " ^ num ^ "| " ^ line ^ suffix))
  in
  println "=== Line Wrapping Example ===\n";
  (* Example 1: Basic English text *)
  println "1. Wrapping English text to 40 characters:";
  let text1 =
    "The quick brown fox jumps over the lazy dog. This is a sample sentence to demonstrate line wrapping."
  in
  println ("   Original: " ^ text1);
  println ("   Width: " ^ Int.to_string (String.width text1) ^ " characters\n");
  let wrapped1 = Unicode.Segmentation.wrap_lines ~width:40 text1 in
  println "   Wrapped lines:";
  print_numbered_lines wrapped1 ~show_width:true;
  println "";
  (* Example 2: Text with punctuation *)
  println "2. Wrapping text with punctuation (width 30):";
  let text2 = "Hello, world! How are you? I'm fine, thank you. What about you?" in
  let wrapped2 = Unicode.Segmentation.wrap_lines ~width:30 text2 in
  print_numbered_lines wrapped2 ~show_width:false;
  println "";
  (* Example 3: CJK text *)
  println "3. Wrapping CJK text (width 20):";
  let text3 = "你好世界！这是一个测试句子。" in
  println ("   Original: " ^ text3 ^ " (width: " ^ Int.to_string (String.width text3) ^ ")");
  let wrapped3 = Unicode.Segmentation.wrap_lines ~width:20 text3 in
  print_numbered_lines wrapped3 ~show_width:true;
  println "";
  (* Example 4: Mixed content *)
  println "4. Wrapping mixed English and CJK (width 35):";
  let text4 = "Hello 世界! This is mixed content with 中文 and English text." in
  let wrapped4 = Unicode.Segmentation.wrap_lines ~width:35 text4 in
  print_numbered_lines wrapped4 ~show_width:true;
  println "";
  (* Example 5: Code with identifiers *)
  println "5. Wrapping code (width 50):";
  let text5 = "let my_function_name = calculate_something(foo_bar_baz, another_parameter) in result" in
  let wrapped5 = Unicode.Segmentation.wrap_lines ~width:50 text5 in
  print_numbered_lines wrapped5 ~show_width:false;
  println "";
  (* Example 6: Text with mandatory breaks *)
  println "6. Text with newlines (preserves mandatory breaks):";
  let text6 = "First line\nSecond line with more text that needs wrapping\nThird line" in
  let wrapped6 = Unicode.Segmentation.wrap_lines ~width:30 text6 in
  print_numbered_lines wrapped6 ~show_width:false;
  println "";
  (* Example 7: Log output *)
  println "7. Wrapping log output (width 60):";
  let log =
    "[2024-11-01 08:15:23] INFO: Processing request from user@example.com with parameters: foo=bar, baz=qux, very_long_parameter_name=some_value"
  in
  let wrapped_log = Unicode.Segmentation.wrap_lines ~width:60 log in
  print_numbered_lines wrapped_log ~show_width:false;
  println "";
  (* Example 8: Very narrow width *)
  println "8. Narrow width wrapping (width 20):";
  let text8 = "supercalifragilisticexpialidocious" in
  println ("   Original: " ^ text8);
  let wrapped8 = Unicode.Segmentation.wrap_lines ~width:20 text8 in
  print_numbered_lines wrapped8 ~show_width:false;
  println "";
  (* Example 9: Simulating terminal pager *)
  println "9. Simulating terminal pager view (width 70):";
  let doc =
    "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris."
  in
  let pager_lines = Unicode.Segmentation.wrap_lines ~width:70 doc in
  println
    "   ┌──────────────────────────────────────────────────────────────────────┐";
  List.for_each
    pager_lines
    ~fn:(fun line ->
      let padding = String.make ~len:(68 - String.length line) ~char:' ' in
      println ("   │ " ^ line ^ padding ^ " │"));
  println
    "   └──────────────────────────────────────────────────────────────────────┘";
  println "";
  println "Line wrapping example completed!";
  Ok ()

let () = Runtime.run ~main ~args:Env.args ()
