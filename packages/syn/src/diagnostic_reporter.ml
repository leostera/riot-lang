open Std
open Std.Collections
open Tty

module Array = Std.Collections.Array

type source_layout = {
  lines: string array;
  line_starts: int array;
}

let make_source_layout = fun source ->
  let lines =
    String.split source ~by:"\n"
    |> Array.from_list
  in
  let line_starts = Array.make ~count:(Array.length lines) ~value:0 in
  let offset = ref 0 in
  for index = 0 to Array.length lines - 1 do
    Array.set_unchecked line_starts ~at:index ~value:!offset;
    let line = Array.get_unchecked lines ~at:index in
    offset := !offset + String.length line + 1
  done;
  { lines; line_starts }

let line_for_pos = fun layout pos ->
  let rec search low high best =
    if low > high then
      best
    else
      let mid = (low + high) / 2 in
      let mid_line = Array.get_unchecked layout.line_starts ~at:mid in
      if mid_line <= pos then
        search (mid + 1) high mid
      else
        search low (mid - 1) best
  in
  let last_index = Array.length layout.line_starts - 1 in
  let line_idx =
    if last_index < 0 then
      0
    else
      search 0 last_index 0
  in
  let start_offset =
    Array.get layout.line_starts ~at:line_idx
    |> Option.unwrap_or ~default:0
  in
  (line_idx, Int.max 0 (pos - start_offset))

let extract_code_snippet_from_layout = fun layout (span: Span.t) ->
  if Array.length layout.lines = 0 then
    None
  else
    let start_pos = span.start in
    let (line_idx, start_col) = line_for_pos layout start_pos in
    if line_idx >= 0 && line_idx < Array.length layout.lines then
      let code_line = Array.get_unchecked layout.lines ~at:line_idx in
      let pointer_line = String.make ~len:start_col ~char:' ' ^ "^" in
      Some (code_line, pointer_line, line_idx + 1)
    else
      None

(* Extract just the message from a diagnostic, without span info *)

let diagnostic_message = fun diag -> Diagnostic.main_message diag

let format_hint = fun text -> text

let format_diagnostic = fun ~layout diag ->
  let fix = Diagnostic.fix_message diag in
  let error_id =
    diag
    |> Diagnostic.error_id
    |> Error.id_to_string
  in
  let error_color = Color.make "#E06C75" in
  let error_style =
    Style.default
    |> Style.fg error_color
    |> Style.bold
  in
  let fix_style =
    Style.default
    |> Style.fg (Color.make "#98C379")
    |> Style.bold
  in
  let error_label = Style.styled error_style "error:" in
  let fix_label = Style.styled fix_style " fix:" in
  let main_msg = Diagnostic.main_message diag in
  let expected = Diagnostic.expected_message diag in
  let explain_msg =
    "  For more information about this error, try `riot fmt --explain " ^ error_id ^ "`"
  in
  match extract_code_snippet_from_layout layout diag.Diagnostic.span with
  | Some (code_line, pointer_line, line_num) -> (
      let styled_pointer = Style.styled error_style pointer_line in
      let styled_msg = Style.styled error_style ("expected " ^ expected) in
      match fix with
      | Some fix ->
          error_label
          ^ " "
          ^ main_msg
          ^ "\n  |\n"
          ^ Int.to_string line_num
          ^ " | "
          ^ code_line
          ^ "\n  | "
          ^ styled_pointer
          ^ " "
          ^ styled_msg
          ^ "\n  |\n\n"
          ^ fix_label
          ^ " "
          ^ fix
          ^ "\n\n"
          ^ explain_msg
          ^ "\n"
      | None ->
          error_label
          ^ " "
          ^ main_msg
          ^ "\n  |\n"
          ^ Int.to_string line_num
          ^ " | "
          ^ code_line
          ^ "\n  | "
          ^ styled_pointer
          ^ " "
          ^ styled_msg
          ^ "\n  |\n\n"
          ^ explain_msg
          ^ "\n"
    )
  | None ->
      panic
        "This is a bug! We couldn't show the error message because we couldn't \
         find the snippet in the code :("

let format = fun ~file ~source diagnostics ->
  let layout = make_source_layout source in
  file ^ "\n\n" ^ (
    diagnostics
    |> List.map ~fn:(format_diagnostic ~layout)
    |> String.concat ""
  ) ^ "\n"

(* Print diagnostics in a nice formatted way *)

let print = fun ~file ~source diagnostics -> print (format ~file ~source diagnostics)
