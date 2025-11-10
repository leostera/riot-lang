open Std
open Tty

(* Extract a code snippet from source showing the diagnostic location.
    
    Returns `(code_line, pointer_line, line_number)` tuple where:
    - `code_line` - The line of code containing the error
    - `pointer_line` - A line with spaces and `^` pointing to the error column
    - `line_number` - The 1-based line number in the source *)
let extract_code_snippet source (span : Ceibo.Span.t) =
  let lines = String.split_on_char '\n' source in
  let line_lengths = List.map String.length lines in

  (* Find which line contains the given position in the source *)
  let rec find_line line_lengths_list pos line_num acc_len =
    match line_lengths_list with
    | [] -> (line_num, 0)
    | len :: rest ->
        if pos <= acc_len + len then (line_num, pos - acc_len)
        else find_line rest pos (line_num + 1) (acc_len + len + 1)
    (* `+1` for newline character *)
  in

  let start_pos = span.start in
  let start_line, start_col = find_line line_lengths start_pos 1 0 in
  let line_idx = start_line - 1 in

  if line_idx >= 0 && line_idx < List.length lines then
    let code_line = List.nth lines line_idx in
    let pointer_line = String.make start_col ' ' ^ "^" in
    Some (code_line, pointer_line, start_line)
  else None

(* Extract just the message from a diagnostic, without span info *)
let diagnostic_message diag = Diagnostic.main_message diag
let format_hint text = text

let format_diagnostic ~source diag =
  let fix = Diagnostic.fix_message diag in
  let error_id = diag |> Diagnostic.error_id |> Error.id_to_string in
  let bold_style = Style.default |> Style.bold in
  let error_color = Color.make "#E06C75" in
  let error_style = Style.default |> Style.fg error_color |> Style.bold in
  let fix_style =
    Style.default |> Style.fg (Color.make "#98C379") |> Style.bold
  in
  let error_label = Style.styled error_style "error:" in
  let fix_label = Style.styled fix_style " fix:" in
  let main_msg = Diagnostic.main_message diag in
  let expected = Diagnostic.expected_message diag in
  let explain_msg =
    "  For more information about this error, try `syn explain " ^ error_id ^ "`"
  in
  match extract_code_snippet source diag.Diagnostic.span with
  | Some (code_line, pointer_line, line_num) -> (
      let styled_pointer = Style.styled error_style pointer_line in
      let styled_msg =
        Style.styled error_style ("expected " ^ expected)
      in
      match fix with
      | Some fix ->
          error_label ^ " " ^ main_msg ^ "\n  |\n" ^ Int.to_string line_num ^ " | " ^ code_line ^ "\n  | " ^ styled_pointer ^ " " ^ styled_msg ^ "\n  |\n\n" ^ fix_label ^ " " ^ fix ^ "\n\n" ^ explain_msg ^ "\n"
      | None ->
          error_label ^ " " ^ main_msg ^ "\n  |\n" ^ Int.to_string line_num ^ " | " ^ code_line ^ "\n  | " ^ styled_pointer ^ " " ^ styled_msg ^ "\n  |\n\n" ^ explain_msg ^ "\n")
  | None ->
      panic
        "This is a bug! We couldn't show the error message because we couldn't \
         find the snippet in the code :("

(* Print diagnostics in a nice formatted way *)
let print ~file ~source diagnostics =
  println (file ^ "\n");
  List.iter
    (fun diag -> println (format_diagnostic ~source diag))
    diagnostics;
  println ""
