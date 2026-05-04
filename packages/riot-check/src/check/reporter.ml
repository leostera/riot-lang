open Std
open Std.Collections
open Tty

type rendered_file = {
  stdout: string list;
  stderr: string list;
}

let severity_style = fun severity ->
  match severity with
  | "warning" ->
      Style.default
      |> Style.fg (Color.make "#E5C07B")
      |> Style.bold
  | "note" ->
      Style.default
      |> Style.fg (Color.make "#56B6C2")
      |> Style.bold
  | _ ->
      Style.default
      |> Style.fg (Color.make "#E06C75")
      |> Style.bold

let fix_style =
  Style.default
  |> Style.fg (Color.make "#98C379")
  |> Style.bold

let position_of_offset = fun text offset -> Std.Unicode.Utf16.position_of_offset text ~offset

let make_source_layout = fun source ->
  let lines =
    String.split_on_char '\n' source
    |> Array.from_list
  in
  let line_starts = Array.make (Array.length lines) 0 in
  let offset = ref 0 in
  for index = 0 to Array.length lines - 1 do
    line_starts.(index) <- !offset;
    offset := !offset + String.length lines.(index) + 1
  done;
  (lines, line_starts)

let source_layout_line_for_pos = fun source_text (_, line_starts) pos ->
  let rec loop low high best =
    if low > high then
      best
    else
      let mid = (low + high) / 2 in
      if line_starts.(mid) <= pos then
        loop (mid + 1) high mid
      else
        loop low (mid - 1) best
  in
  if Array.length line_starts = 0 then
    (0, 0)
  else
    let last = Array.length line_starts - 1 in
    let line_idx =
      loop 0 last 0
      |> fun line_idx -> Int.min last (Int.max 0 line_idx)
    in
    (line_idx, Int.max 0 (position_of_offset source_text pos).character)

let extract_snippet = fun source_layout source_text (span: Syn.Span.t) ->
  if Array.length (fst source_layout) = 0 then
    None
  else
    let start_position = position_of_offset source_text span.start in
    let end_position = position_of_offset source_text span.end_ in
    let line_idx =
      source_layout_line_for_pos source_text source_layout span.start
      |> fst
    in
    if line_idx < 0 || line_idx >= Array.length (fst source_layout) then
      None
    else
      let line_text = (fst source_layout).(line_idx) in
      let start_col = start_position.character in
      let pointer_span =
        if end_position.line = start_position.line then
          Int.max 1 (end_position.character - start_col)
        else
          1
      in
      Some (line_idx + 1, start_col, line_text, pointer_span)

let format_diagnostic = fun ~path_text ~source_layout ~source_text diagnostic ->
  let span: Syn.Span.t = Diagnostic.span diagnostic in
  let start_position = position_of_offset source_text span.start in
  let line = start_position.line + 1 in
  let column = start_position.character + 1 in
  let severity = Diagnostic.severity diagnostic in
  let style = severity_style severity in
  let severity_label = Style.styled style (severity ^ ":") in
  let header =
    path_text
    ^ ":"
    ^ Int.to_string line
    ^ ":"
    ^ Int.to_string column
    ^ ":"
    ^ "\n\n"
    ^ severity_label
    ^ " ["
    ^ Diagnostic.phase diagnostic
    ^ "] "
    ^ Diagnostic.code diagnostic
    ^ ": "
    ^ Diagnostic.message diagnostic
  in
  let explain_msg =
    match diagnostic with
    | Diagnostic.Parse diagnostic ->
        let id = Syn.Diagnostic.id diagnostic in
        "  For more information about this error, try `riot fmt --explain " ^ id ^ "`"
    | Diagnostic.Lowering _
    | Diagnostic.Typing _ ->
        "  For more information about this error, try `riot check --explain "
        ^ Diagnostic.code diagnostic
        ^ "`"
  in
  match extract_snippet source_layout source_text span with
  | None -> header
  | Some (line_num, start_col, code_line, pointer_span) ->
      let line_label = Int.to_string line_num in
      let indent_prefix = String.make (String.length line_label + 1) ' ' in
      let pointer = String.make (Int.max 0 start_col) ' ' ^ String.make pointer_span '^' in
      let styled_pointer = Style.styled style pointer in
      let styled_expected =
        match Diagnostic.expected diagnostic with
        | None -> ""
        | Some msg -> " " ^ Style.styled style msg
      in
      let fix_line =
        match Diagnostic.fix diagnostic with
        | None -> ""
        | Some msg -> indent_prefix ^ Style.styled fix_style "fix:" ^ " " ^ msg ^ "\n\n"
      in
      header
      ^ "\n"
      ^ indent_prefix
      ^ "|\n"
      ^ line_label
      ^ " | "
      ^ code_line
      ^ "\n"
      ^ indent_prefix
      ^ "| "
      ^ styled_pointer
      ^ styled_expected
      ^ "\n"
      ^ indent_prefix
      ^ "|\n\n"
      ^ fix_line
      ^ indent_prefix
      ^ explain_msg
      ^ "\n"

let load_source_text = fun path ->
  match Fs.read path with
  | Ok source_text -> Some source_text
  | Error _ -> None

let format_diagnostic_without_snippet = fun ~path_text diagnostic ->
  let severity = Diagnostic.severity diagnostic in
  let style = severity_style severity in
  let severity_label = Style.styled style (severity ^ ":") in
  let explain_msg =
    match diagnostic with
    | Diagnostic.Parse diagnostic ->
        let id = Syn.Diagnostic.id diagnostic in
        "  For more information about this error, try `riot fmt --explain " ^ id ^ "`"
    | Diagnostic.Lowering _
    | Diagnostic.Typing _ ->
        "  For more information about this error, try `riot check --explain "
        ^ Diagnostic.code diagnostic
        ^ "`"
  in
  path_text
  ^ "\n\n"
  ^ severity_label
  ^ " ["
  ^ Diagnostic.phase diagnostic
  ^ "] "
  ^ Diagnostic.code diagnostic
  ^ ": "
  ^ Diagnostic.message diagnostic
  ^ "\n"
  ^ explain_msg
  ^ "\n"

let render_checked_file = fun ~workspace_root checked_file ->
  match checked_file with
  | State.Unreadable { path; reason } ->
      {
        stdout = [];
        stderr = [ Scope.relative_or_absolute ~workspace_root path ^ ": " ^ reason ^ "\n" ];
      }
  | State.Typed { path; report; diagnostics } ->
      if List.is_empty diagnostics then
        { stdout = []; stderr = [] }
      else
        let path_text = Scope.relative_or_absolute ~workspace_root path in
        match load_source_text report.filename with
        | Some source_text ->
            let source_layout = make_source_layout source_text in
            {
              stdout =
                diagnostics
                |> List.map
                  (fun diagnostic ->
                    format_diagnostic
                      ~path_text
                      ~source_layout
                      ~source_text
                      diagnostic);
              stderr = [];
            }
        | None ->
            {
              stdout =
                diagnostics
                |> List.map
                  (fun diagnostic -> format_diagnostic_without_snippet ~path_text diagnostic);
              stderr = [];
            }

let success_summary = fun ~quiet (summary: State.checked_summary) ->
  if quiet then
    None
  else if summary.diagnostics = 0 && summary.read_failures = 0 then
    None
  else if summary.has_error then
    None
  else if summary.checked_files = 1 then
    Some "Checked 1 file: ok\n"
  else
    Some ("Checked " ^ Int.to_string summary.checked_files ^ " files: ok\n")
