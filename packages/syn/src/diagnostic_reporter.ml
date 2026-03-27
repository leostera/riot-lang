open Std
open Std.Collections
open Tty

module Array = Std.Collections.Array

type source_layout = {
  lines : string array;
  line_starts : int array;
}

(* Extract a code snippet from source showing the diagnostic location.
    
    Returns `(code_line, pointer_line, line_number)` tuple where:
    - `code_line` - The line of code containing the error
    - `pointer_line` - A line with spaces and `^` pointing to the error column
    - `line_number` - The 1-based line number in the source *)
let make_source_layout source =
  let lines = String.split_on_char '\n' source |> Array.of_list in
  let line_starts = Array.make (Array.length lines) 0 in
  let offset = ref 0 in
  for index = 0 to Array.length lines - 1 do
    line_starts.(index) <- !offset;
    offset := !offset + String.length lines.(index) + 1
  done;
  { lines; line_starts }

let line_for_pos layout pos =
  let rec search low high best =
    if low > high then
      best
    else
      let mid = (low + high) / 2 in
      if layout.line_starts.(mid) <= pos then
        search (mid + 1) high mid
      else
        search low (mid - 1) best
  in
  let last_index = Array.length layout.line_starts - 1 in
  let line_idx = if last_index < 0 then 0 else search 0 last_index 0 in
  let start_offset =
    if Array.length layout.line_starts = 0 then 0 else layout.line_starts.(line_idx)
  in
  line_idx, Int.max 0 (pos - start_offset)

let extract_code_snippet_from_layout layout (span : Ceibo.Span.t) =
  if Array.length layout.lines = 0 then
    None
  else
    let start_pos = span.start in
    let line_idx, start_col = line_for_pos layout start_pos in
    if line_idx >= 0 && line_idx < Array.length layout.lines then
      let code_line = layout.lines.(line_idx) in
      let pointer_line = String.make start_col ' ' ^ "^" in
      Some (code_line, pointer_line, line_idx + 1)
    else
      None

(* Extract just the message from a diagnostic, without span info *)
let diagnostic_message diag = Diagnostic.main_message diag
let format_hint text = text

let format_diagnostic ~layout diag =
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
  match extract_code_snippet_from_layout layout diag.Diagnostic.span with
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

let format ~file ~source diagnostics =
  let layout = make_source_layout source in
  file ^ "\n\n"
  ^ (diagnostics
    |> List.map (format_diagnostic ~layout)
    |> String.concat "")
  ^ "\n"

(* Print diagnostics in a nice formatted way *)
let print ~file ~source diagnostics =
  print (format ~file ~source diagnostics)
