(** Unicode Example - Demonstrates Unicode text processing capabilities *)
open Std
open Std.IO

let () =
  println "=== Std.Unicode Example ===\n";
  (* Example 1: Basic ASCII *)
  println "1. Basic ASCII string:";
  let s1 = "Hello" in
  println ("   String: " ^ s1);
  println ("   Display width: " ^ string_of_int (String.width s1));
  println ("   Grapheme count: " ^ string_of_int (String.grapheme_count s1));
  println ("   Rune count: " ^ string_of_int (String.rune_count s1));
  println "";
  (* Example 2: CJK characters *)
  println "2. CJK (Chinese/Japanese/Korean) characters:";
  let s2 = "你好世界" in
  println ("   String: " ^ s2);
  println ("   Display width: " ^ string_of_int (String.width s2) ^ " (each CJK char is width 2)");
  println ("   Grapheme count: " ^ string_of_int (String.grapheme_count s2));
  println ("   Rune count: " ^ string_of_int (String.rune_count s2));
  println "";
  (* Example 3: Emoji *)
  println "3. Emoji characters:";
  let s3 = "👍🎉🚀" in
  println ("   String: " ^ s3);
  println ("   Display width: " ^ string_of_int (String.width s3) ^ " (each emoji is width 2)");
  println ("   Grapheme count: " ^ string_of_int (String.grapheme_count s3));
  println ("   Rune count: " ^ string_of_int (String.rune_count s3));
  println "";
  (* Example 4: Mixed content *)
  println "4. Mixed ASCII, CJK, and emoji:";
  let s4 = "Hello 世界! 👋" in
  println ("   String: " ^ s4);
  println ("   Display width: " ^ string_of_int (String.width s4));
  println ("   Grapheme count: " ^ string_of_int (String.grapheme_count s4));
  println ("   Rune count: " ^ string_of_int (String.rune_count s4));
  println "";
  (* Example 5: Individual rune width *)
  println "5. Individual character widths:";
  let test_char c_code name =
    let r = Unicode.Rune.unsafe_of_int c_code in
    let w = Unicode.Rune.width r in
    let hex_str =
      let hex_chars = "0123456789ABCDEF" in
      let s = Bytes.create 4 in
      let n = ref c_code in
      for i = 3 downto 0 do
        Bytes.set s i hex_chars.[!n land 0xf];
        n := !n lsr 4
      done;
      Bytes.to_string s
    in
    println ("   U+" ^ hex_str ^ " (" ^ name ^ "): width=" ^ string_of_int w)
  in
  test_char 0x0041 "Latin A";
  test_char 0x4e00 "CJK Ideograph";
  test_char 0x0301 "Combining acute";
  test_char 0x1_f44d "Thumbs up emoji";
  test_char 0x200d "Zero-width joiner";
  println "";
  (* Example 6: UTF-8 operations *)
  println "6. UTF-8 encoding/decoding:";
  let s = "Hello" in
  (
    match Unicode.Utf8.decode_rune s 0 with
    | Some (r, next_pos) ->
        let code = Unicode.Rune.to_int r in
        let hex_str =
          let hex_chars = "0123456789ABCDEF" in
          let s = Bytes.create 4 in
          let n = ref code in
          for i = 3 downto 0 do
            Bytes.set s i hex_chars.[!n land 0xf];
            n := !n lsr 4
          done;
          Bytes.to_string s
        in
        println
          ("   First rune of '" ^ s ^ "': U+" ^ hex_str ^ " at byte position " ^ string_of_int next_pos)
    | None -> println "   Failed to decode"
  );
  println ("   Is valid UTF-8: " ^ Bool.to_string (Unicode.Utf8.is_valid s));
  println "";
  (* Example 7: String truncation with width *)
  println "7. Truncating strings to display width:";
  let s7 = "Hello 世界 World!" in
  println ("   Original: '" ^ s7 ^ "' (width: " ^ string_of_int (String.width s7) ^ ")");
  let truncated = String.truncate_width ~width:10 s7 in
  println
    ("   Truncated to width 10: '"
    ^ truncated
    ^ "' (width: "
    ^ string_of_int (String.width truncated)
    ^ ")");
  println "";
  (* Example 8: Padding with width awareness *)
  println "8. Padding strings (width-aware):";
  let s8 = "你好" in
  (* Width 4 *)
  println ("   Original: '" ^ s8 ^ "' (width: " ^ string_of_int (String.width s8) ^ ")");
  println ("   Right-padded to 10: '" ^ String.pad_right ~width:10 ' ' s8 ^ "'");
  println ("   Center-padded to 10: '" ^ String.pad_center ~width:10 ' ' s8 ^ "'");
  println "";
  println "Example completed!"
