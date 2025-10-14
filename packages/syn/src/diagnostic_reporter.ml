open Std

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
let diagnostic_message (diag : Diagnostic.t) =
  match diag.kind with
  | Diagnostic.UnexpectedToken { expected; found } ->
      let found_kind = found.kind in
      format "expected %s but found %s" expected found_kind
  | Diagnostic.MissingToken { expected } ->
      format "expected %s" expected

(* Format a single diagnostic with source context *)
let format_diagnostic ~source diag =
  match extract_code_snippet source diag.Diagnostic.span with
  | Some (code_line, pointer_line, line_num) ->
      let msg = diagnostic_message diag in
      format "%03d | %s\n    | %s %s\n    |" line_num code_line pointer_line msg
  | None ->
      let msg = diagnostic_message diag in
      format "    | %s" msg

(* Print diagnostics in a nice formatted way *)
let print ~file ~source diagnostics =
  println "%s" file;
  List.iter
    (fun diag ->
      println "%s" (format_diagnostic ~source diag))
    diagnostics;
  println ""
