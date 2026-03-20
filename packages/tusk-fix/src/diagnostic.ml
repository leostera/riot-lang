open Std

type severity = Error | Warning | Info | Hint

type t = {
  severity : severity;
  message : string;
  span : Syn.Ceibo.Span.t;
  rule_id : string;
  suggestion : string option;
  fix : Fix.fix option;
}

let make ~severity ~message ~span ~rule_id ?suggestion ?fix () =
  { severity; message; span; rule_id; suggestion; fix }

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
  match diag.suggestion, diag.fix with
  | Some sugg, _ -> base_msg ^ "\n  Suggestion: " ^ sugg
  | None, Some fix -> base_msg ^ "\n  Fix: " ^ Fix.title fix
  | None, None -> base_msg

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
    match diag.suggestion, diag.fix with
    | Some sugg, _ -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix -> lines @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines
  in
  String.concat "\n" lines

let extract_code_snippet source (span : Syn.Ceibo.Span.t) =
  let lines = String.split_on_char '\n' source in
  let line_lengths = List.map String.length lines in
  let rec line_at index = function
    | [] -> None
    | line :: _ when index = 0 -> Some line
    | _ :: rest when index > 0 -> line_at (index - 1) rest
    | _ -> None
  in
  let rec find_line line_lengths_list pos line_num acc_len =
    match line_lengths_list with
    | [] -> (line_num, 0)
    | len :: rest ->
        if pos <= acc_len + len then (line_num, pos - acc_len)
        else find_line rest pos (line_num + 1) (acc_len + len + 1)
    (* +1 for newline *)
  in
  let start_pos = span.start in
  let end_pos =
    if span.end_ <= span.start then span.start + 1 else span.end_
  in
  let start_line, start_col = find_line line_lengths start_pos 1 0 in
  let end_line, end_col = find_line line_lengths end_pos 1 0 in
  let line_idx = start_line - 1 in
  match line_at line_idx lines with
  | Some code_line ->
      let marker_width =
        if start_line = end_line then
          let width = end_col - start_col in
          if width <= 0 then 1 else width
        else
          let remaining = String.length code_line - start_col in
          if remaining <= 0 then 1 else remaining
      in
      let pointer_line =
        String.make start_col ' '
        ^ "\027[1;33m"
        ^ String.make marker_width '^'
        ^ "\027[0m"
      in
      Some (code_line, pointer_line, start_line)
  | None -> None

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
    match diag.suggestion, diag.fix with
    | Some sugg, _ -> lines_with_snippet @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix ->
        lines_with_snippet @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines_with_snippet
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
      ("fix", match diag.fix with Some fix -> Fix.to_json fix | None -> Null);
    ]

(* Accessor functions *)
let severity diag = diag.severity
let message diag = diag.message
let span diag = diag.span
let rule_id diag = diag.rule_id
let suggestion diag = diag.suggestion
let fix diag = diag.fix

(* Grouped diagnostics *)
type grouped = {
  severity : severity;
  message : string;
  spans : Syn.Ceibo.Span.t list;
  rule_id : string;
  suggestion : string option;
  fix : Fix.fix option;
}

let group_diagnostics (diags : t list) : grouped list =
  let module DiagMap = Collections.HashMap in
  let map = DiagMap.create () in
  List.iter
    (fun (diag : t) ->
      let fix_title = diag.fix |> Option.map Fix.title in
      let key = (diag.severity, diag.message, diag.rule_id, diag.suggestion, fix_title) in
      match DiagMap.get map key with
      | Some existing_spans ->
          ignore (DiagMap.insert map key ((diag.fix, diag.span) :: existing_spans))
      | None -> ignore (DiagMap.insert map key [ (diag.fix, diag.span) ]))
    diags;
  DiagMap.into_iter map
  |> Iter.Iterator.map ~fn:(fun ((severity, message, rule_id, suggestion, _fix_title), spans) ->
         let spans = List.rev spans in
         let fix =
           match spans with
           | [] -> None
           | (fix, _) :: _ -> fix
         in
         let spans = List.map snd spans in
         ({ severity; message; spans; rule_id; suggestion; fix } : grouped))
  |> Iter.Iterator.to_list

let grouped_to_formatted_output ~file ~source grouped =
  let header = Path.to_string file ^ ":" in
  let severity_str = severity_to_colored_string grouped.severity in
  let basic_info =
    [ "[" ^ severity_str ^ "] " ^ grouped.rule_id; ""; grouped.message ]
  in
  let spans =
    List.sort
      (fun (left : Syn.Ceibo.Span.t) (right : Syn.Ceibo.Span.t) ->
        Int.compare left.start right.start)
      grouped.spans
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
      basic_info spans
  in
  let lines_with_suggestion =
    match grouped.suggestion, grouped.fix with
    | Some sugg, _ -> lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ sugg ]
    | None, Some fix ->
        lines_with_snippets @ [ ""; "  \027[1;90m→\027[0m " ^ Fix.title fix ]
    | None, None -> lines_with_snippets
  in
  header ^ "\n" ^ String.concat "\n" lines_with_suggestion ^ "\n"
