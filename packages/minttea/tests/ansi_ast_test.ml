open Std
open Minttea.Render.Ansi_ast

let test_text_merging = fun () ->
  let input = Seq [ Text "hello"; Text " "; Text "world" ] in
  let expected = Seq [ Text "hello world" ] in
  let result = optimize input in
  assert (result = expected);
  print_endline "✓ Text merging works"

let test_movement_merging = fun () ->
  let input = Seq [ MoveUp 5; MoveUp 3; MoveDown 2; MoveDown 1 ] in
  let expected = Seq [ MoveUp 8; MoveDown 3 ] in
  let result = optimize input in
  assert (result = expected);
  print_endline "✓ Movement merging works"

let test_zero_movement_elimination = fun () ->
  let input = Seq [ MoveUp 0; Text "hi"; MoveDown 0; MoveLeft 0 ] in
  let expected = Seq [ Text "hi" ] in
  let result = optimize input in
  assert (result = expected);
  print_endline "✓ Zero movement elimination works"

let test_nested_sequence_flattening = fun () ->
  let input = Seq [ Text "a"; Seq [ Text "b"; Text "c" ]; Text "d" ] in
  let expected = Seq [ Text "abcd" ] in
  let result = optimize input in
  assert (result = expected);
  print_endline "✓ Nested sequence flattening works"

let test_cursor_deduplication = fun () ->
  let input = Seq [ HideCursor; HideCursor; ShowCursor; ShowCursor ] in
  let expected = Seq [ HideCursor; ShowCursor ] in
  let result = optimize input in
  assert (result = expected);
  print_endline "✓ Cursor operation deduplication works"

let test_style_composition = fun () ->
  let input = Fg (
    Tty.Color.ANSI 1,
    [ Bg (Tty.Color.ANSI 4, [ Bold [ Text "Hello " ]; Italic [ Text "World" ] ]) ]
  ) in
  (* The optimizer should preserve the structure but optimize internal sequences *)
  let result = optimize input in
  (* Check it still has the same structure *)
  match result with
  | Fg (Tty.Color.ANSI 1, [ Bg (Tty.Color.ANSI 4, [Bold [ Text "Hello " ];Italic [ Text "World" ]]) ]) -> print_endline
  "✓ Style composition preserved"
  | _ -> panic "Style composition not preserved correctly"

let test_complex_optimization = fun () ->
  let input = Seq [
    MoveCursor (10, 20);
    Fg (Tty.Color.ANSI 2, [ Text "Status: "; Text "OK" ]);
    MoveUp 0;
    Text " - ";
    Text "Done";
    HideCursor;
    HideCursor;

  ] in
  let result = optimize input in
  match result with
  | Seq [MoveCursor (10, 20);Fg (Tty.Color.ANSI 2, [ Text "Status: OK" ]);Text " - Done";HideCursor] -> print_endline
  "✓ Complex optimization works"
  | _ ->
      (* Debug output *)
      let rec ast_to_string =
        function
        | Text s -> format "Text(%S)" s
        | MoveCursor (x, y) -> format "MoveCursor(%d,%d)" x y
        | MoveUp n -> format "MoveUp(%d)" n
        | HideCursor -> "HideCursor"
        | ShowCursor -> "ShowCursor"
        | Fg (_, children) -> format
        "Fg([%s])"
        (String.concat "; " (List.map ast_to_string children))
        | Bg (_, children) -> format
        "Bg([%s])"
        (String.concat "; " (List.map ast_to_string children))
        | Seq ops -> format "Seq([%s])" (String.concat "; " (List.map ast_to_string ops))
        | _ -> "..."
      in
      panic (format "Complex optimization failed. Got: %s" (ast_to_string result))

let test_ansi_output = fun () ->
  (* Test that our AST produces correct ANSI codes *)
  let ast = Seq [
    MoveCursor (0, 0);
    Clear;
    Fg (Tty.Color.ANSI 1, [ Text "Error: " ]);
    Bold [ Text "Failed" ];
    Text "\n"
  ] in
  let output = render ast in
  (* Check for expected ANSI sequences *)
  assert (String.contains_s output "\x1b[1;1H");
  (* MoveCursor(0,0) *)
  assert (String.contains_s output "\x1b[2J");
  (* Clear *)
  assert (String.contains_s output "\x1b[31m");
  (* Red foreground *)
  assert (String.contains_s output "\x1b[1m");
  (* Bold *)
  assert (String.contains_s output "Error: ");
  assert (String.contains_s output "Failed");
  print_endline "✓ ANSI output generation works"

let test_color_reset = fun () ->
  (* Ensure colors are properly reset after scope *)
  let ast = Seq [ Fg (Tty.Color.ANSI 2, [ Text "Green" ]); Text " Normal" ] in
  let output = render ast in
  (* Should contain green color, the text, reset, then normal text *)
  assert (String.contains_s output "\x1b[32m");
  (* Green *)
  assert (String.contains_s output "\x1b[39m");
  (* Reset fg color *)
  assert (String.contains_s output "Green Normal");
  print_endline "✓ Color reset works correctly"

let test_nested_style_resets = fun () ->
  (* Test that nested styles properly reset *)
  let ast = Bold [ Text "Bold "; Italic [ Text "Bold+Italic " ]; Text "Bold again" ] in
  let output = render ast in
  assert (String.contains_s output "\x1b[1m");
  (* Bold on *)
  assert (String.contains_s output "\x1b[3m");
  (* Italic on *)
  assert (String.contains_s output "\x1b[23m");
  (* Italic off *)
  assert (String.contains_s output "\x1b[22m");
  (* Bold off at the end *)
  print_endline "✓ Nested style resets work correctly"

let test_synchronized_updates = fun () ->
  let ast = Seq [ BeginSync; Text "Atomic update"; EndSync ] in
  let output = render ast in
  assert (String.contains_s output "\x1b[?2026h");
  (* Begin sync *)
  assert (String.contains_s output "\x1b[?2026l");
  (* End sync *)
  assert (String.contains_s output "Atomic update");
  print_endline "✓ Synchronized updates work"

let test_empty_sequences = fun () ->
  (* Empty sequences should be handled gracefully *)
  let ast = Seq [] in
  let result = optimize ast in
  assert (result = Seq []);
  let ast2 = Fg (Tty.Color.ANSI 1, []) in
  let result2 = optimize ast2 in
  assert (result2 = Fg (Tty.Color.ANSI 1, []));
  print_endline "✓ Empty sequences handled correctly"

let test_rgb_colors = fun () ->
  let ast = Seq [
    Fg (Tty.Color.RGB (255, 0, 0), [ Text "Red" ]);
    Bg (Tty.Color.RGB (0, 0, 255), [ Text "Blue bg" ])
  ] in
  let output = render ast in
  assert (String.contains_s output "\x1b[38;2;255;0;0m");
  (* RGB red fg *)
  assert (String.contains_s output "\x1b[48;2;0;0;255m");
  (* RGB blue bg *)
  print_endline "✓ RGB colors work"

let test_ansi256_colors = fun () ->
  let ast = Seq [ Fg (Tty.Color.ANSI256 196, [ Text "256-color red" ]);  ] in
  let output = render ast in
  assert (String.contains_s output "\x1b[38;5;196m");
  (* 256-color *)
  print_endline "✓ ANSI256 colors work"

let run_all_tests = fun () ->
  print_endline "\n=== Running ANSI AST Optimizer Tests ===\n";
  test_text_merging ();
  test_movement_merging ();
  test_zero_movement_elimination ();
  test_nested_sequence_flattening ();
  test_cursor_deduplication ();
  test_style_composition ();
  test_complex_optimization ();
  test_ansi_output ();
  test_color_reset ();
  test_nested_style_resets ();
  test_synchronized_updates ();
  test_empty_sequences ();
  test_rgb_colors ();
  test_ansi256_colors ();
  print_endline "\n=== All ANSI AST Tests Passed! ===\n"

let () = run_all_tests ()
