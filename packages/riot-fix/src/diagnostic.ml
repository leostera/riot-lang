open Std
open Std.Collections
module Array = Std.Collections.Array

type severity = Fixme.Diagnostic.severity =
  | Error
  | Warning
  | Info
  | Hint

type kind = Fixme.Diagnostic.kind =
  | Known of { rule_id: string; message: string }
  | Generic of { rule_id: string; message: string }

type t = Fixme.Diagnostic.t

let make = Fixme.Diagnostic.make

let kind = Fixme.Diagnostic.kind

let severity = Fixme.Diagnostic.severity

let span = Fixme.Diagnostic.span

let suggestion = Fixme.Diagnostic.suggestion

let fix = Fixme.Diagnostic.fix

let severity_to_string severity =
  match severity with
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
  | Hint -> "hint"

let severity_to_colored_string severity =
  match severity with
  | Error -> "\027[1;31merror\027[0m"
  | Warning -> "\027[1;33mwarning\027[0m"
  | Info -> "\027[1;36minfo\027[0m"
  | Hint -> "\027[1;90mhint\027[0m"

type source_layout = {
  lines: string array;
  line_starts: int array;
}

let message = Fixme.Diagnostic.message

let rule_id = Fixme.Diagnostic.rule_id

let header_label = fun severity rule_id -> "[" ^ severity_to_string severity ^ "] " ^ rule_id

let colored_header_label = fun severity rule_id ->
  "[" ^ severity_to_colored_string severity ^ "] " ^ rule_id

let explain_hint = fun severity rule_id ->
  "  For more information about this "
  ^ severity_to_string severity
  ^ ", try `riot fix --explain "
  ^ rule_id
  ^ "`"

let to_string = fun diag ->
  let severity_str = severity_to_string (severity diag) in
  let span_str = Syn.Ceibo.Span.to_string (span diag) in
  let base_msg =
    "["
    ^ severity_str
    ^ "] "
    ^ message diag
    ^ " at "
    ^ span_str
    ^ " ("
    ^ rule_id diag
    ^ ")"
  in
  match suggestion diag, fix diag with
  | Some sugg, _ -> base_msg ^ "\n  Suggestion: " ^ sugg
  | None, Some fix -> base_msg ^ "\n  Fix: " ^ Fix.title fix
  | None, None -> base_msg

let to_colored_string = fun diag ->
  let span_str = Syn.Ceibo.Span.to_string (span diag) in
  let lines = [
    colored_header_label (severity diag) (rule_id diag);
    "";
    "  at " ^ span_str;
    "";
    message diag;
  ] in
  let lines =
    match suggestion diag, fix diag with
    | Some sugg, _ -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines
  in
  String.concat "\n" lines

let make_source_layout = fun source ->
  let lines = String.split_on_char '\n' source |> Array.of_list in
  let line_starts = Array.make (Array.length lines) 0 in
  let offset = ref 0 in
  for index = 0 to Array.length lines - 1 do
    yield ();
    line_starts.(index) <- !offset;
    offset := !offset + String.length lines.(index) + 1
  done;
  { lines; line_starts }

let line_for_pos = fun layout pos ->
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
  let line_idx =
    if last_index < 0 then
      0
    else
      search 0 last_index 0
  in
  let start_offset =
    if Array.length layout.line_starts = 0 then
      0
    else
      layout.line_starts.(line_idx)
  in
  (line_idx, Int.max 0 (pos - start_offset))

let extract_code_snippet_from_layout = fun layout (span: Syn.Ceibo.Span.t) ->
  if Array.length layout.lines = 0 then
    None
  else
    let start_pos = span.start in
    let end_pos =
      if span.end_ <= span.start then
        span.start + 1
      else
        span.end_
    in
    let start_idx, start_col = line_for_pos layout start_pos in
    let end_idx, end_col = line_for_pos layout end_pos in
    if start_idx < 0 || start_idx >= Array.length layout.lines then
      None
    else
      let code_line = layout.lines.(start_idx) in
      let marker_width =
        if start_idx = end_idx then
          let width = end_col - start_col in
          if width <= 0 then
            1
          else
            width
        else
          let remaining = String.length code_line - start_col in
          if remaining <= 0 then
            1
          else
            remaining
      in
      let pointer_line = String.make start_col ' ' ^ "\027[1;33m" ^ String.make marker_width '^' ^ "\027[0m" in
      Some (code_line, pointer_line, start_idx + 1)

let extract_code_snippet = fun source span ->
  extract_code_snippet_from_layout (make_source_layout source) span

let to_formatted_output = fun ~file ~source diag ->
  let header = Path.to_string file ^ ":" in
  let basic_info = [ header_label (severity diag) (rule_id diag); ""; message diag ] in
  let lines_with_snippet =
    match extract_code_snippet source (span diag) with
    | Some (code_line, pointer_line, line_num) -> basic_info
    @ [
      "";
      "  \027[1;90m" ^ Int.to_string line_num ^ " |\027[0m " ^ code_line;
      "  \027[1;90m" ^ String.make (String.length (string_of_int line_num)) ' ' ^ " |\027[0m " ^ pointer_line;
    ]
    | None -> basic_info @ [ "  at " ^ Syn.Ceibo.Span.to_string (span diag) ]
  in
  let lines_with_suggestion =
    match suggestion diag, fix diag with
    | Some sugg, _ -> lines_with_snippet @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix -> lines_with_snippet @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines_with_snippet
  in
  let lines_with_explain = lines_with_suggestion
  @ [ ""; explain_hint (severity diag) (rule_id diag) ] in
  header ^ "\n" ^ String.concat "\n" lines_with_explain ^ "\n\n"

let to_json = fun diag ->
  let open Data.Json in
    Object [
      ("severity", String (severity_to_string (severity diag)));
      ("message", String (message diag));
      (
        "span",
        let span = span diag in
        Object [ ("start", Int span.start); ("end", Int span.end_) ]
      );
      ("rule_id", String (rule_id diag));
      (
        "suggestion",
        match suggestion diag with
        | Some s -> String s
        | None -> Null
      );
      (
        "fix",
        match fix diag with
        | Some fix -> Fix.to_json fix
        | None -> Null
      );
    ]

(* Grouped diagnostics *)

type grouped = {
  severity: severity;
  message: string;
  spans: Syn.Ceibo.Span.t list;
  rule_id: string;
  suggestion: string option;
  fix: Fix.fix option;
}

let group_diagnostics: t list -> grouped list = fun diags ->
  let module DiagMap = Collections.HashMap in
  let map = DiagMap.create () in
  List.iter
    (fun (diag: t) ->
      let fix_title = fix diag |> Option.map Fix.title in
      let key = (severity diag, message diag, rule_id diag, suggestion diag, fix_title) in
      match DiagMap.get map key with
      | Some existing_spans -> ignore
        (DiagMap.insert map key ((fix diag, span diag) :: existing_spans))
      | None -> ignore (DiagMap.insert map key [ (fix diag, span diag) ]))
    diags;
  DiagMap.into_iter map |> Iter.Iterator.map
    ~fn:(fun (((severity, message, rule_id, suggestion, _fix_title), spans)) ->
      let spans = List.rev spans in
      let fix =
        match spans with
        | [] -> None
        | (fix, _) :: _ -> fix
      in
      let spans = List.map snd spans in
      ({
          severity;
          message;
          spans;
          rule_id;
          suggestion;
          fix;
        }: grouped)) |> Iter.Iterator.to_list

let grouped_to_formatted_output = fun ~file ~source grouped ->
  let layout = make_source_layout source in
  let header = Path.to_string file ^ ":" in
  let basic_info = [ colored_header_label grouped.severity grouped.rule_id; ""; grouped.message; ] in
  let spans =
    List.sort
      (fun (left: Syn.Ceibo.Span.t) (right: Syn.Ceibo.Span.t) ->
        Int.compare left.start right.start)
      grouped.spans
  in
  let lines_with_snippets =
    List.fold_left
      (fun acc span ->
        match extract_code_snippet_from_layout layout span with
        | Some (code_line, pointer_line, line_num) -> acc
        @ [
          "";
          "  \027[1;90m" ^ Int.to_string line_num ^ " |\027[0m " ^ code_line;
          "  \027[1;90m"
          ^ String.make (String.length (string_of_int line_num)) ' '
          ^ " |\027[0m "
          ^ pointer_line;
        ]
        | None -> acc @ [ "  at " ^ Syn.Ceibo.Span.to_string span ])
      basic_info
      spans
  in
  let lines_with_suggestion =
    match grouped.suggestion, grouped.fix with
    | Some sugg, _ -> lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix -> lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines_with_snippets
  in
  let lines_with_explain = lines_with_suggestion
  @ [ ""; explain_hint grouped.severity grouped.rule_id ] in
  header ^ "\n" ^ String.concat "\n" lines_with_explain ^ "\n"

let grouped_to_formatted_output_with_layout = fun ~file ~layout grouped ->
  let header = Path.to_string file ^ ":" in
  let basic_info = [ colored_header_label grouped.severity grouped.rule_id; ""; grouped.message; ] in
  let spans =
    List.sort
      (fun (left: Syn.Ceibo.Span.t) (right: Syn.Ceibo.Span.t) ->
        Int.compare left.start right.start)
      grouped.spans
  in
  let lines_with_snippets =
    List.fold_left
      (fun acc span ->
        match extract_code_snippet_from_layout layout span with
        | Some (code_line, pointer_line, line_num) -> acc
        @ [
          "";
          "  \027[1;90m" ^ Int.to_string line_num ^ " |\027[0m " ^ code_line;
          "  \027[1;90m"
          ^ String.make (String.length (string_of_int line_num)) ' '
          ^ " |\027[0m "
          ^ pointer_line;
        ]
        | None -> acc @ [ "  at " ^ Syn.Ceibo.Span.to_string span ])
      basic_info
      spans
  in
  let lines_with_suggestion =
    match grouped.suggestion, grouped.fix with
    | Some sugg, _ -> lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix -> lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines_with_snippets
  in
  let lines_with_explain = lines_with_suggestion
  @ [ ""; explain_hint grouped.severity grouped.rule_id ] in
  header ^ "\n" ^ String.concat "\n" lines_with_explain ^ "\n"

let grouped_list_to_formatted_output = fun ~file ~source grouped ->
  let layout = make_source_layout source in
  grouped |> List.map (grouped_to_formatted_output_with_layout ~file ~layout) |> String.concat ""
