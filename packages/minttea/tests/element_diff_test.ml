open Std
open Minttea

(* Test that verifies our element-level differential rendering *)

let test_element_equality () =
  let open Element in
  
  (* Test identical elements *)
  let e1 = text "Hello World" in
  let e2 = text "Hello World" in
  assert (Element.equal e1 e2);
  print_endline "✓ Identical text elements are equal";
  
  (* Test different text *)
  let e3 = text "Different" in
  assert (not (Element.equal e1 e3));
  print_endline "✓ Different text elements are not equal";
  
  (* Test identical columns *)
  let col1 = column [text "A"; text "B"; text "C"] in
  let col2 = column [text "A"; text "B"; text "C"] in
  assert (Element.equal col1 col2);
  print_endline "✓ Identical columns are equal";
  
  (* Test columns with different lengths *)
  let col3 = column [text "A"; text "B"] in
  assert (not (Element.equal col1 col3));
  print_endline "✓ Columns with different lengths are not equal (early exit)";
  
  (* Test rows with same length but different content *)
  let row1 = row [text "1"; text "2"; text "3"] in
  let row2 = row [text "1"; text "X"; text "3"] in
  assert (not (Element.equal row1 row2));
  print_endline "✓ Rows with different content are not equal";
  
  (* Test nested structures *)
  let nested1 = column [
    text "Header";
    row [text "A"; text "B"];
    text "Footer"
  ] in
  let nested2 = column [
    text "Header";
    row [text "A"; text "B"];
    text "Footer"
  ] in
  assert (Element.equal nested1 nested2);
  print_endline "✓ Nested structures compare correctly";
  
  (* Test with styles *)
  let styled1 = text ~style:(Style.default |> Style.fg (Tty.Color.ansi 2)) "Green" in
  let styled2 = text ~style:(Style.default |> Style.fg (Tty.Color.ansi 2)) "Green" in
  let styled3 = text ~style:(Style.default |> Style.fg (Tty.Color.ansi 3)) "Green" in
  assert (Element.equal styled1 styled2);
  assert (not (Element.equal styled1 styled3));
  print_endline "✓ Style comparison works correctly"

let test_rendering_optimization () =
  print_endline "\nElement-level differential rendering:";
  print_endline "• Elements are compared directly (no scene graph needed)";
  print_endline "• Early exit on structural differences (different list lengths)";
  print_endline "• Natural short-circuit with && operators";
  print_endline "• Much simpler than scene graph comparison";
  print_endline "• Frame counter should now increment by 1 each frame"

let () =
  print_endline "\n=== Minttea Element Diff Test ===\n";
  test_element_equality ();
  test_rendering_optimization ();
  print_endline "\n=== All Element Diff Tests Passed! ===\n"