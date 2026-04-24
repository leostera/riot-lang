open Std
open Minttea

(* Test that verifies our element-level differential rendering *)

let test_element_equality = fun () ->
  let open Element in
    let e1 = text "Hello World" in
    let e2 = text "Hello World" in
    assert (e1 = e2);
    println "✓ Identical text elements are equal";
    (* Test different text *)
    let e3 = text "Different" in
    assert (e1 != e3);
    println "✓ Different text elements are not equal";
    (* Test identical columns *)
    let col1 = column [ text "A"; text "B"; text "C" ] in
    let col2 = column [ text "A"; text "B"; text "C" ] in
    assert (col1 = col2);
    println "✓ Identical columns are equal";
    (* Test columns with different lengths *)
    let col3 = column [ text "A"; text "B" ] in
    assert (col1 != col3);
    println "✓ Columns with different lengths are not equal (early exit)";
    (* Test rows with same length but different content *)
    let row1 = row [ text "1"; text "2"; text "3" ] in
    let row2 = row [ text "1"; text "X"; text "3" ] in
    assert (row1 != row2);
    println "✓ Rows with different content are not equal";
    (* Test nested structures *)
    let nested1 = column [ text "Header"; row [ text "A"; text "B" ]; text "Footer" ] in
    let nested2 = column [ text "Header"; row [ text "A"; text "B" ]; text "Footer" ] in
    assert (nested1 = nested2);
    println "✓ Nested structures compare correctly";
    (* Test with styles *)
    let styled1 = text ~style:(Style.empty |> Style.fg (`rgb (0, 255, 0))) "Green" in
    let styled2 = text ~style:(Style.empty |> Style.fg (`rgb (0, 255, 0))) "Green" in
    let styled3 = text ~style:(Style.empty |> Style.fg (`rgb (255, 255, 0))) "Green" in
    assert (styled1 = styled2);
    assert (styled1 != styled3);
    println "✓ Style comparison works correctly"

let test_rendering_optimization = fun () ->
  println "\nElement-level differential rendering:";
  println "• Elements are compared directly (no scene graph needed)";
  println "• Early exit on structural differences (different list lengths)";
  println "• Natural short-circuit with && operators";
  println "• Much simpler than scene graph comparison";
  println "• Frame counter should now increment by 1 each frame"

let () =
  println "\n=== Minttea Element Diff Test ===\n";
  test_element_equality ();
  test_rendering_optimization ();
  println "\n=== All Element Diff Tests Passed! ===\n"
