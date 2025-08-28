(** Parse OCaml compiler error messages to extract useful information *)

type error_info = {
  file : string;
  line_start : int;
  col_start : int;
  line_end : int option;
  col_end : int option;
  error_type : string;
  message : string;
  hint : string option;
}

(** Parse file location from error message 
    Format: File "path/to/file.ml", line X, characters Y-Z:
    or: File "path/to/file.ml", lines X-Y, characters A-B:
*)
let parse_location line =
  try
    if String.starts_with ~prefix:"File \"" line then
      (* Extract filename *)
      let quote_end = String.index_from line 6 '"' in
      let file = String.sub line 6 (quote_end - 6) in
      
      (* Parse line numbers and character positions *)
      let rest = String.sub line (quote_end + 2) (String.length line - quote_end - 2) in
      
      (* Check if it's "line" or "lines" *)
      if String.starts_with ~prefix:", line " rest then
        (* Single line: "line X, characters Y-Z:" *)
        Scanf.sscanf rest ", line %d, characters %d-%d:" 
          (fun line col_start col_end ->
            Some (file, line, col_start, Some line, Some col_end))
      else if String.starts_with ~prefix:", lines " rest then
        (* Multiple lines: "lines X-Y, characters A-B:" *)
        Scanf.sscanf rest ", lines %d-%d, characters %d-%d:"
          (fun line_start line_end col_start col_end ->
            Some (file, line_start, col_start, Some line_end, Some col_end))
      else
        (* Unknown format, try to at least get the file *)
        Some (file, 1, 0, None, None)
    else
      None
  with _ -> None

(** Parse error type from message (e.g., "Error: Syntax error") *)
let parse_error_type line =
  if String.starts_with ~prefix:"Error: " line then
    let msg = String.sub line 7 (String.length line - 7) in
    if String.starts_with ~prefix:"Syntax error" msg then
      ("Syntax error", msg)
    else if String.contains msg ':' then
      let colon_pos = String.index msg ':' in
      let error_type = String.sub msg 0 colon_pos in
      let message = String.trim (String.sub msg (colon_pos + 1) (String.length msg - colon_pos - 1)) in
      (error_type, message)
    else
      ("Error", msg)
  else if String.starts_with ~prefix:"Warning " line then
    ("Warning", line)
  else
    ("", line)

(** Extract context lines around the error *)
let extract_context lines location_idx =
  let context_lines = ref [] in
  let idx = ref (location_idx + 1) in
  
  (* Collect lines until we hit another "File" or "Error:" or run out *)
  while !idx < Array.length lines && 
        not (String.starts_with ~prefix:"File \"" lines.(!idx)) &&
        not (String.starts_with ~prefix:"Error:" lines.(!idx)) do
    context_lines := lines.(!idx) :: !context_lines;
    incr idx
  done;
  
  List.rev !context_lines

(** Parse hint from context (look for lines starting with "Hint:") *)
let extract_hint context_lines =
  List.find_opt (fun line -> 
    String.starts_with ~prefix:"Hint: " line) context_lines
  |> Option.map (fun hint -> 
    String.sub hint 6 (String.length hint - 6))

(** Parse a complete OCaml compiler error message *)
let parse_error error_output =
  let lines = String.split_on_char '\n' error_output |> Array.of_list in
  let errors = ref [] in
  
  let i = ref 0 in
  while !i < Array.length lines do
    let line = lines.(!i) in
    
    (* Look for file location *)
    match parse_location line with
    | Some (file, line_start, col_start, line_end, col_end) ->
        (* Found a location, now look for the error message *)
        let error_idx = ref (!i + 1) in
        while !error_idx < Array.length lines && 
              not (String.starts_with ~prefix:"Error:" lines.(!error_idx)) &&
              not (String.starts_with ~prefix:"Warning " lines.(!error_idx)) do
          incr error_idx
        done;
        
        if !error_idx < Array.length lines then
          let error_type, message = parse_error_type lines.(!error_idx) in
          let context_lines = extract_context lines !error_idx in
          let hint = extract_hint context_lines in
          
          (* For syntax errors at 0-0, try to find actual location from context *)
          let (actual_line, actual_col) = 
            if line_start = 1 && col_start = 0 && col_end = Some 0 then
              (* Look for actual error location in context *)
              match context_lines with
              | _ :: actual_loc :: _ when String.contains actual_loc '^' ->
                  (* Found caret pointing to error *)
                  let caret_pos = String.index actual_loc '^' in
                  (line_start, caret_pos)
              | _ -> (line_start, col_start)
            else
              (line_start, col_start)
          in
          
          errors := {
            file;
            line_start = actual_line;
            col_start = actual_col;
            line_end;
            col_end;
            error_type;
            message;
            hint;
          } :: !errors;
          
          i := !error_idx + 1
        else
          incr i
    | None -> incr i
  done;
  
  List.rev !errors

(** Get the first (primary) error from compiler output *)
let get_primary_error error_output =
  match parse_error error_output with
  | [] -> None
  | err :: _ -> Some err

(** Format error for display *)
let format_error err =
  let location = 
    match err.line_end, err.col_end with
    | Some line_end, Some col_end when line_end <> err.line_start ->
        Printf.sprintf "%s, lines %d-%d, characters %d-%d" 
          err.file err.line_start line_end err.col_start col_end
    | _, Some col_end ->
        Printf.sprintf "%s:%d:%d-%d" 
          err.file err.line_start err.col_start col_end
    | _ ->
        Printf.sprintf "%s:%d:%d" 
          err.file err.line_start err.col_start
  in
  let hint_str = match err.hint with
    | Some h -> Printf.sprintf "\n  Hint: %s" h
    | None -> ""
  in
  Printf.sprintf "%s: %s: %s%s" location err.error_type err.message hint_str