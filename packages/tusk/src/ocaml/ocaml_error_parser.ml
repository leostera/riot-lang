(** Parse OCaml compiler error messages to extract useful information *)

type error_type =
  | SyntaxError
  | TypeError of string
  | UnboundValue of string
  | UnboundModule of string
  | FileNotFound of string
  | OtherError of string

type error_info = {
  file : string;
  line : int;
  span : int * int; (* start, end character positions *)
  hint : string; (* The source line with caret pointing to error *)
  error : error_type;
  raw : string; (* Raw compiler output *)
}

(** Parse file location from error message Format: File "path/to/file.ml", line
    X, characters Y-Z: or: File "path/to/file.ml", lines X-Y, characters A-B: *)
let parse_location line =
  try
    if String.starts_with ~prefix:"File \"" line then
      (* Extract filename *)
      let quote_end = String.index_from line 6 '"' in
      let file = String.sub line 6 (quote_end - 6) in

      (* Parse line numbers and character positions *)
      let rest =
        String.sub line (quote_end + 2) (String.length line - quote_end - 2)
      in

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
    else None
  with _ -> None

(** Parse error type from message (e.g., "Error: Syntax error") *)
let parse_error_type line =
  if String.starts_with ~prefix:"Error: " line then
    let msg = String.sub line 7 (String.length line - 7) in
    if String.starts_with ~prefix:"Syntax error" msg then SyntaxError
    else if String.starts_with ~prefix:"Unbound value " msg then
      let value = String.sub msg 14 (String.length msg - 14) in
      UnboundValue value
    else if String.starts_with ~prefix:"Unbound module " msg then
      let module_name = String.sub msg 15 (String.length msg - 15) in
      UnboundModule module_name
    else if String.starts_with ~prefix:"Cannot find file " msg then
      let file = String.sub msg 17 (String.length msg - 17) in
      FileNotFound file
    else if String.contains msg ':' then
      let colon_pos = String.index msg ':' in
      let error_desc = String.sub msg 0 colon_pos in
      TypeError error_desc
    else OtherError msg
  else if String.starts_with ~prefix:"Warning " line then OtherError line
  else OtherError line

(** Extract hint from context lines - the source line and caret line *)
let extract_hint lines start_idx =
  (* After "File..." line, we expect:
     Line N | source code
              ^^^^^^ (caret line)
     Error: message
  *)
  let hint_lines = ref [] in
  let idx = ref (start_idx + 1) in

  (* Collect the source line and caret line *)
  while
    !idx < Array.length lines
    && (not (String.starts_with ~prefix:"Error:" lines.(!idx)))
    && not (String.starts_with ~prefix:"File \"" lines.(!idx))
  do
    hint_lines := lines.(!idx) :: !hint_lines;
    incr idx
  done;

  (* Join the hint lines (source + caret) *)
  String.concat "\n" (List.rev !hint_lines)

(** Parse a complete OCaml compiler error message *)
let parse_error error_output =
  let lines = String.split_on_char '\n' error_output |> Array.of_list in
  let errors = ref [] in

  let i = ref 0 in
  while !i < Array.length lines do
    let line = lines.(!i) in

    (* Look for file location *)
    match parse_location line with
    | Some (file, line_num, col_start, _line_end, col_end) ->
        (* Extract hint (source line + caret) *)
        let hint = extract_hint lines !i in

        (* Find the error message line *)
        let error_idx = ref (!i + 1) in
        while
          !error_idx < Array.length lines
          && (not (String.starts_with ~prefix:"Error:" lines.(!error_idx)))
          && not (String.starts_with ~prefix:"Warning " lines.(!error_idx))
        do
          incr error_idx
        done;

        if !error_idx < Array.length lines then (
          let error_type = parse_error_type lines.(!error_idx) in

          (* Build the complete raw error for this location *)
          let raw_lines = ref [ line ] in
          (* Start with File line *)
          for j = !i + 1 to min !error_idx (Array.length lines - 1) do
            raw_lines := lines.(j) :: !raw_lines
          done;
          let raw = String.concat "\n" (List.rev !raw_lines) in

          let span = (col_start, Option.value col_end ~default:col_start) in

          errors :=
            { file; line = line_num; span; hint; error = error_type; raw }
            :: !errors;

          i := !error_idx + 1)
        else incr i
    | None -> incr i
  done;

  List.rev !errors

(** Get the first (primary) error from compiler output *)
let get_primary_error error_output =
  (* Filter out internal build system errors *)
  let filtered_lines =
    error_output |> String.split_on_char '\n'
    |> List.filter (fun line ->
        (* Filter out "Cannot find file *.cmo" errors which are internal *)
        not
          (String.starts_with ~prefix:"Error: Cannot find file" line
          && (String.ends_with ~suffix:".cmo\"" line
             || String.ends_with ~suffix:".cmi\"" line
             || String.ends_with ~suffix:".cma\"" line)))
    |> String.concat "\n"
  in
  match parse_error filtered_lines with [] -> None | err :: _ -> Some err

(** Format error for display *)
let format_error err =
  let col_start, col_end = err.span in
  let location = Printf.sprintf "%s:%d:%d" err.file err.line col_start in
  let error_str =
    match err.error with
    | SyntaxError -> "Syntax error"
    | TypeError s -> Printf.sprintf "Type error: %s" s
    | UnboundValue v -> Printf.sprintf "Unbound value: %s" v
    | UnboundModule m -> Printf.sprintf "Unbound module: %s" m
    | FileNotFound f -> Printf.sprintf "File not found: %s" f
    | OtherError e -> e
  in
  Printf.sprintf "error: %s\n       %s\n%s" location error_str err.hint
