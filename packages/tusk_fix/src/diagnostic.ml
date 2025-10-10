open Std

type severity = Error | Warning | Info | Hint

type t = {
  severity : severity;
  message : string;
  span : Syn.Ceibo.Span.t;
  rule_id : string;
  suggestion : string option;
}

let make ~severity ~message ~span ~rule_id ?suggestion () =
  { severity; message; span; rule_id; suggestion }

let severity_to_string = function
  | Error -> "error"
  | Warning -> "warning"
  | Info -> "info"
  | Hint -> "hint"

let severity_to_colored_string = function
  | Error -> "\027[1;31merror\027[0m"
  | Warning -> "\027[1;33mwarning\027[0m"
  | Info -> "\027[1;36minfo\027[0m"
  | Hint -> "\027[1;90mhint\027[0m"

let to_string diag =
  let severity_str = severity_to_string diag.severity in
  let span_str = Syn.Ceibo.Span.to_string diag.span in
  let base_msg =
    format "[%s] %s at %s (%s)" severity_str diag.message span_str diag.rule_id
  in
  match diag.suggestion with
  | None -> base_msg
  | Some sugg -> format "%s\n  Suggestion: %s" base_msg sugg

let to_colored_string diag =
  let severity_str = severity_to_colored_string diag.severity in
  let span_str = Syn.Ceibo.Span.to_string diag.span in
  let lines =
    [
      format "[%s] %s" severity_str diag.rule_id;
      "";
      diag.message;
      format "  at %s" span_str;
    ]
  in
  let lines =
    match diag.suggestion with
    | None -> lines
    | Some sugg -> lines @ [ ""; format "  \027[1;90m→\027[0m %s" sugg ]
  in
  String.concat "\n" lines

let extract_code_snippet source (span : Syn.Ceibo.Span.t) =
  let lines = String.split_on_char '\n' source in
  let line_lengths = List.map String.length lines in
  let rec find_line line_lengths_list pos line_num acc_len =
    match line_lengths_list with
    | [] -> (line_num, 0)
    | len :: rest ->
        if pos <= acc_len + len then (line_num, pos - acc_len)
        else find_line rest pos (line_num + 1) (acc_len + len + 1)
    (* +1 for newline *)
  in
  let start_pos = span.start in
  let start_line, start_col = find_line line_lengths start_pos 1 0 in
  let line_idx = start_line - 1 in
  if line_idx >= 0 && line_idx < List.length lines then
    let code_line = List.nth lines line_idx in
    let pointer_line = String.make start_col ' ' ^ "\027[1;33m^\027[0m" in
    Some (code_line, pointer_line, start_line)
  else None

let to_formatted_output ~file ~source diag =
  let header = format "%s:" (Path.to_string file) in
  let severity_str = severity_to_colored_string diag.severity in
  let basic_info =
    [ format "[%s] %s" severity_str diag.rule_id; ""; diag.message ]
  in
  let lines_with_snippet =
    match extract_code_snippet source diag.span with
    | Some (code_line, pointer_line, line_num) ->
        basic_info
        @ [
            "";
            format "  \027[1;90m%d |\027[0m %s" line_num code_line;
            format "  \027[1;90m%s |\027[0m %s"
              (String.make (String.length (string_of_int line_num)) ' ')
              pointer_line;
          ]
    | None ->
        basic_info @ [ format "  at %s" (Syn.Ceibo.Span.to_string diag.span) ]
  in
  let lines_with_suggestion =
    match diag.suggestion with
    | None -> lines_with_snippet
    | Some sugg ->
        lines_with_snippet @ [ ""; format "  \027[1;90m→\027[0m %s" sugg ]
  in
  format "%s\n%s\n\n" header (String.concat "\n" lines_with_suggestion)

let to_json diag =
  let open Data.Json in
  let fields =
    [
      ("severity", String (severity_to_string diag.severity));
      ("message", String diag.message);
      ( "span",
        Object [ ("start", Int diag.span.start); ("end", Int diag.span.end_) ]
      );
      ("rule_id", String diag.rule_id);
    ]
  in
  let fields =
    match diag.suggestion with
    | None -> fields
    | Some sugg -> fields @ [ ("suggestion", String sugg) ]
  in
  Object fields

let severity diag = diag.severity
let message diag = diag.message
let span diag = diag.span
let rule_id diag = diag.rule_id
let suggestion diag = diag.suggestion

type grouped = {
  severity : severity;
  message : string;
  spans : Syn.Ceibo.Span.t list;
  rule_id : string;
  suggestion : string option;
}

let group_diagnostics diagnostics =
  let table = Collections.HashMap.create () in
  List.iter
    (fun (diag : t) ->
      let key = format "%s:%s" diag.rule_id diag.message in
      match Collections.HashMap.get table key with
      | Some existing ->
          let updated =
            { existing with spans = existing.spans @ [ diag.span ] }
          in
          ignore (Collections.HashMap.insert table key updated)
      | None ->
          let grouped =
            {
              severity = diag.severity;
              message = diag.message;
              spans = [ diag.span ];
              rule_id = diag.rule_id;
              suggestion = diag.suggestion;
            }
          in
          ignore (Collections.HashMap.insert table key grouped))
    diagnostics;
  Collections.HashMap.to_list table |> List.map snd

let grouped_to_formatted_output ~file ~source grouped =
  let header = format "%s:" (Path.to_string file) in
  let severity_str = severity_to_colored_string grouped.severity in
  let basic_info =
    [ format "[%s] %s" severity_str grouped.rule_id; ""; grouped.message; "" ]
  in
  (* Sort spans by their start position so diagnostics appear in source order *)
  let sorted_spans =
    List.sort
      (fun (span_a : Syn.Ceibo.Span.t) (span_b : Syn.Ceibo.Span.t) ->
        Int.compare span_a.start span_b.start)
      grouped.spans
  in
  let snippet_lines =
    List.filter_map
      (fun span ->
        match extract_code_snippet source span with
        | Some (code_line, pointer_line, line_num) ->
            Some
              [
                format "  \027[1;90m%d |\027[0m %s" line_num code_line;
                format "  \027[1;90m%s |\027[0m %s"
                  (String.make (String.length (string_of_int line_num)) ' ')
                  pointer_line;
                "";
              ]
        | None -> None)
      sorted_spans
    |> List.flatten
  in
  let lines_with_snippets = basic_info @ snippet_lines in
  let lines_with_suggestion =
    match grouped.suggestion with
    | None -> lines_with_snippets
    | Some sugg ->
        lines_with_snippets @ [ ""; format "  \027[1;90m→\027[0m %s" sugg ]
  in
  format "%s\n%s\n\n" header (String.concat "\n" lines_with_suggestion)
