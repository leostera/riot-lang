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
    "[" ^ severity_str ^ "] " ^ diag.message ^ " at " ^ span_str ^ " (" ^ diag.rule_id ^ ")"
  in
  match diag.suggestion with
  | None -> base_msg
  | Some sugg -> base_msg ^ "\n  Suggestion: " ^ sugg

let to_colored_string diag =
  let severity_str = severity_to_colored_string diag.severity in
  let span_str = Syn.Ceibo.Span.to_string diag.span in
  let lines =
    [
      "[" ^ severity_str ^ "] " ^ diag.rule_id;
      "";
      "  at " ^ span_str;
      "";
      diag.message;
    ]
  in
  let lines =
    match diag.suggestion with
    | None -> lines
    | Some sugg -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
  in
  let lines =
    match diag.suggestion with
    | None -> lines
    | Some sugg -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
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
  let header = Path.to_string file ^ ":" in
  let severity_str = severity_to_string diag.severity in
  let basic_info =
    [ "[" ^ severity_str ^ "] " ^ diag.rule_id; ""; diag.message ]
  in
  let lines_with_snippet =
    match extract_code_snippet source diag.span with
    | Some (code_line, pointer_line, line_num) ->
        basic_info
        @ [
            "";
            "  \027[1;90m" ^ Int.to_string line_num ^ " |\027[0m " ^ code_line;
            "  \027[1;90m" ^ String.make (String.length (string_of_int line_num)) ' ' ^ " |\027[0m " ^ pointer_line;
          ]
    | None ->
        basic_info @ [ "  at " ^ Syn.Ceibo.Span.to_string diag.span ]
  in
  let lines_with_suggestion =
    match diag.suggestion with
    | None -> lines_with_snippet
    | Some sugg ->
        lines_with_snippet @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
  in
  header ^ "\n" ^ String.concat "\n" lines_with_suggestion ^ "\n\n"

let to_json diag =
  let open Data.Json in
  Object
    [
      ("severity", String (severity_to_string diag.severity));
      ("message", String diag.message);
      ("span", Object [ ("start", Int diag.span.start); ("end", Int diag.span.end_) ]);
      ("rule_id", String diag.rule_id);
      ("suggestion", match diag.suggestion with Some s -> String s | None -> Null);
    ]

(* Accessor functions *)
let severity diag = diag.severity
let message diag = diag.message
let span diag = diag.span
let rule_id diag = diag.rule_id
let suggestion diag = diag.suggestion

(* Grouped diagnostics *)
type grouped = {
  severity : severity;
  message : string;
  spans : Syn.Ceibo.Span.t list;
  rule_id : string;
  suggestion : string option;
}

let group_diagnostics (diags : t list) : grouped list =
  let module DiagMap = Collections.HashMap in
  let map = DiagMap.create () in
  List.iter
    (fun (diag : t) ->
      let key = (diag.severity, diag.message, diag.rule_id, diag.suggestion) in
      match DiagMap.get map key with
      | Some existing_spans ->
          ignore (DiagMap.insert map key (diag.span :: existing_spans))
      | None -> ignore (DiagMap.insert map key [ diag.span ]))
    diags;
  DiagMap.into_iter map
  |> Iter.Iterator.map ~fn:(fun ((severity, message, rule_id, suggestion), spans) ->
         ({ severity; message; spans = List.rev spans; rule_id; suggestion } : grouped))
  |> Iter.Iterator.to_list

let grouped_to_formatted_output ~file ~source grouped =
  let header = Path.to_string file ^ ":" in
  let severity_str = severity_to_colored_string grouped.severity in
  let basic_info =
    [ "[" ^ severity_str ^ "] " ^ grouped.rule_id; ""; grouped.message ]
  in
  let lines_with_snippets =
    List.fold_left
      (fun acc span ->
        match extract_code_snippet source span with
        | Some (code_line, pointer_line, line_num) ->
            acc
            @ [
                "";
                "  \027[1;90m" ^ Int.to_string line_num ^ " |\027[0m " ^ code_line;
                "  \027[1;90m"
                ^ String.make (String.length (string_of_int line_num)) ' '
                ^ " |\027[0m " ^ pointer_line;
              ]
        | None -> acc @ [ "  at " ^ Syn.Ceibo.Span.to_string span ])
      basic_info grouped.spans
  in
  let lines_with_suggestion =
    match grouped.suggestion with
    | None -> lines_with_snippets
    | Some sugg ->
        lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
  in
  header ^ "\n" ^ String.concat "\n" lines_with_suggestion ^ "\n"
