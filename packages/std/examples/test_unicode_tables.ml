open Std

let () =
  Miniriot.run ~main:(fun ~args:_ ->
  (* Test Greek letter *)
  let alpha = match Unicode.Utf8.decode_rune "α" 0 with
    | Some (r, _) -> r
    | None -> failwith "Failed to decode"
  in
  
  (* Test Cyrillic letter *)
  let cyrillic_a = match Unicode.Utf8.decode_rune "А" 0 with
    | Some (r, _) -> r
    | None -> failwith "Failed to decode"
  in
  
  (* Test CJK ideograph *)
  let zhong = match Unicode.Utf8.decode_rune "中" 0 with
    | Some (r, _) -> r
    | None -> failwith "Failed to decode"
  in
  
  (* Test Arabic digit *)
  let arabic_5 = match Unicode.Utf8.decode_rune "٥" 0 with
    | Some (r, _) -> r
    | None -> failwith "Failed to decode"
  in
  
  println "Testing Unicode character classification:";
  println "";
  
  Printf.printf "Greek α (U+%04X):\n" (Unicode.Rune.to_int alpha);
  Printf.printf "  is_letter: %b (should be true)\n" (Unicode.Rune.is_letter alpha);
  Printf.printf "  is_lower: %b (should be true)\n" (Unicode.Rune.is_lower alpha);
  println "";
  
  Printf.printf "Cyrillic А (U+%04X):\n" (Unicode.Rune.to_int cyrillic_a);
  Printf.printf "  is_letter: %b (should be true)\n" (Unicode.Rune.is_letter cyrillic_a);
  Printf.printf "  is_upper: %b (should be true)\n" (Unicode.Rune.is_upper cyrillic_a);
  println "";
  
  Printf.printf "CJK 中 (U+%04X):\n" (Unicode.Rune.to_int zhong);
  Printf.printf "  is_letter: %b (should be true)\n" (Unicode.Rune.is_letter zhong);
  println "";
  
  Printf.printf "Arabic digit ٥ (U+%04X):\n" (Unicode.Rune.to_int arabic_5);
  Printf.printf "  is_digit: %b (should be true)\n" (Unicode.Rune.is_digit arabic_5);
  println "";
  
  (* Test ASCII for regression *)
  let a = Unicode.Rune.of_char 'A' in
  println "ASCII 'A':";
  Printf.printf "  is_letter: %b (should be true)\n" (Unicode.Rune.is_letter a);
  Printf.printf "  is_upper: %b (should be true)\n" (Unicode.Rune.is_upper a);
  
  println "";
  println "✅ All tests completed!";
  Ok ()
  ) ~args:Env.args ()
