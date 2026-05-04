open Std
open Yaml_value

module Array = Collections.Array

exception Parse_failure of string

let fail = fun message -> raise (Parse_failure message)

let fail_line = fun line_number message ->
  fail
    ("line " ^ Int.to_string line_number ^ ": " ^ message)

type line = { number: int; indent: int; text: string }

let is_ws = fun __tmp1 ->
  match __tmp1 with
  | ' '
  | '\t'
  | '\r'
  | '\n' -> true
  | _ -> false

let trim_right = fun value ->
  let rec loop index =
    if index <= 0 then
      ""
    else
      match String.get value ~at:(index - 1) with
      | Some char when is_ws char -> loop (index - 1)
      | _ -> String.sub value ~offset:0 ~len:index
  in
  loop (String.length value)

let strip_trailing_cr = fun value ->
  if String.ends_with ~suffix:"\r" value then
    String.sub value ~offset:0 ~len:(String.length value - 1)
  else
    value

let strip_comment = fun line ->
  let length = String.length line in
  let buffer = IO.Buffer.create ~size:length in
  let in_double = ref false in
  let in_single = ref false in
  let escaped = ref false in
  let rec loop index =
    if Int.equal index length then
      IO.Buffer.contents buffer
    else
      let current = String.unsafe_get line index in
      if !in_double then (
        IO.Buffer.add_char buffer current;
        if !escaped then
          escaped := false
        else if Char.equal current '\\' then
          escaped := true
        else if Char.equal current '"' then
          in_double := false;
        loop (index + 1)
      ) else if !in_single then (
        IO.Buffer.add_char buffer current;
        if Char.equal current '\'' then
          in_single := false;
        loop (index + 1)
      ) else if Char.equal current '"' then (
        in_double := true;
        IO.Buffer.add_char buffer current;
        loop (index + 1)
      ) else if Char.equal current '\'' then (
        in_single := true;
        IO.Buffer.add_char buffer current;
        loop (index + 1)
      ) else if Char.equal current '#' then
        IO.Buffer.contents buffer
      else (
        IO.Buffer.add_char buffer current;
        loop (index + 1)
      )
  in
  loop 0

let count_indent = fun line_number text ->
  let length = String.length text in
  let rec loop index =
    if Int.equal index length then
      index
    else
      match String.unsafe_get text index with
      | ' ' -> loop (index + 1)
      | '\t' -> fail_line line_number "tabs are not supported for indentation"
      | _ -> index
  in
  loop 0

let preprocess = fun input ->
  let raw_lines = String.split_on_char '\n' input in
  let lines = ref [] in
  raw_lines
  |> List.enumerate
  |> List.for_each
    ~fn:(fun (index, raw_line) ->
      let number = index + 1 in
      let line =
        raw_line
        |> strip_trailing_cr
        |> strip_comment
        |> trim_right
      in
      if not (String.equal (String.trim line) "") then
        let indent = count_indent number line in
        let text = String.sub line ~offset:indent ~len:(String.length line - indent) in
        lines := { number; indent; text } :: !lines);
  let lines = List.rev !lines in
  let lines =
    match lines with
    | { text = "---"; _ } :: rest -> rest
    | _ -> lines
  in
  let lines =
    match List.rev lines with
    | { text = "..."; _ } :: rest -> List.rev rest
    | _ -> lines
  in
  List.for_each
    lines
    ~fn:(fun line ->
      if String.equal line.text "---" || String.equal line.text "..." then
        fail_line line.number "multiple YAML documents are not supported");
  Array.from_list lines

let skip_spaces = fun text index ->
  let length = String.length text in
  let rec loop index =
    if index >= length then
      length
    else
      match String.unsafe_get text index with
      | ' ' -> loop (index + 1)
      | _ -> index
  in
  loop index

let parse_escape = fun text line_number index ->
  if index >= String.length text then
    fail_line line_number "unterminated escape sequence"
  else
    match String.unsafe_get text index with
    | '"' -> ('"', index + 1)
    | '\\' -> ('\\', index + 1)
    | '/' -> ('/', index + 1)
    | 'b' -> ('\b', index + 1)
    | 'f' -> ('\012', index + 1)
    | 'n' -> ('\n', index + 1)
    | 'r' -> ('\r', index + 1)
    | 't' -> ('\t', index + 1)
    | 'u' ->
        if index + 4 >= String.length text then
          fail_line line_number "unterminated unicode escape"
        else
          let hex_value = fun __tmp1 ->
            match __tmp1 with
            | '0' .. '9' as c -> Char.code c - Char.code '0'
            | 'a' .. 'f' as c -> 10 + Char.code c - Char.code 'a'
            | 'A' .. 'F' as c -> 10 + Char.code c - Char.code 'A'
            | _ -> fail_line line_number "expected hex digit in unicode escape"
          in
          let code =
            (hex_value (String.unsafe_get text (index + 1)) lsl 12)
            lor (hex_value (String.unsafe_get text (index + 2)) lsl 8)
            lor (hex_value (String.unsafe_get text (index + 3)) lsl 4)
            lor hex_value (String.unsafe_get text (index + 4))
          in
          let rune =
            match Unicode.Rune.from_int code with
            | Some rune -> rune
            | None -> fail_line line_number "invalid unicode scalar value"
          in
          (Unicode.Rune.to_char rune, index + 5)
    | _ -> fail_line line_number "unsupported escape sequence"

let parse_double_quoted = fun text line_number start ->
  let length = String.length text in
  if start >= length || not (Char.equal (String.unsafe_get text start) '"') then
    fail_line line_number "expected double-quoted string"
  else
    let buffer = IO.Buffer.create ~size:32 in
    let rec loop index =
      if index >= length then
        fail_line line_number "unterminated double-quoted string"
      else
        match String.unsafe_get text index with
        | '"' -> (IO.Buffer.contents buffer, index + 1)
        | '\\' ->
            let (escaped, next) = parse_escape text line_number (index + 1) in
            IO.Buffer.add_char buffer escaped;
            loop next
        | current ->
            IO.Buffer.add_char buffer current;
            loop (index + 1)
    in
    loop (start + 1)

let parse_single_quoted = fun text line_number start ->
  let length = String.length text in
  if start >= length || not (Char.equal (String.unsafe_get text start) '\'') then
    fail_line line_number "expected single-quoted string"
  else
    let buffer = IO.Buffer.create ~size:32 in
    let rec loop index =
      if index >= length then
        fail_line line_number "unterminated single-quoted string"
      else if Char.equal (String.unsafe_get text index) '\'' then
        if index + 1 < length && Char.equal (String.unsafe_get text (index + 1)) '\'' then (
          IO.Buffer.add_char buffer '\'';
          loop (index + 2)
        ) else
          (IO.Buffer.contents buffer, index + 1)
      else (
        IO.Buffer.add_char buffer (String.unsafe_get text index);
        loop (index + 1)
      )
    in
    loop (start + 1)

let find_mapping_separator = fun text line_number ->
  let length = String.length text in
  let rec scan index in_double in_single =
    if index >= length then
      None
    else
      let current = String.unsafe_get text index in
      if in_double then
        if Char.equal current '\\' then
          scan (index + 2) true false
        else if Char.equal current '"' then
          scan (index + 1) false false
        else
          scan (index + 1) true false
      else if in_single then
        if Char.equal current '\'' then
          if index + 1 < length && Char.equal (String.unsafe_get text (index + 1)) '\'' then
            scan (index + 2) false true
          else
            scan (index + 1) false false
        else
          scan (index + 1) false true
      else if Char.equal current '"' then
        scan (index + 1) true false
      else if Char.equal current '\'' then
        scan (index + 1) false true
      else if Char.equal current ':' then
        let next = index + 1 in
        if next >= length then
          Some index
        else if
          Char.equal (String.unsafe_get text next) ' '
          || Char.equal (String.unsafe_get text next) '\t'
        then
          Some index
        else
          scan (index + 1) false false
      else
        scan (index + 1) false false
  in
  ignore line_number;
  scan 0 false false

let parse_key = fun text line_number ->
  let trimmed = String.trim text in
  if String.equal trimmed "" then
    fail_line line_number "empty mapping key"
  else if Char.equal (String.unsafe_get trimmed 0) '"' then
    let (key, next) = parse_double_quoted trimmed line_number 0 in
    if Int.equal (skip_spaces trimmed next) (String.length trimmed) then
      key
    else
      fail_line line_number "unexpected trailing characters after quoted key"
  else if Char.equal (String.unsafe_get trimmed 0) '\'' then
    let (key, next) = parse_single_quoted trimmed line_number 0 in
    if Int.equal (skip_spaces trimmed next) (String.length trimmed) then
      key
    else
      fail_line line_number "unexpected trailing characters after quoted key"
  else
    trimmed

let split_mapping_head = fun line ->
  match find_mapping_separator line.text line.number with
  | None -> None
  | Some index ->
      let key = parse_key (String.sub line.text ~offset:0 ~len:index) line.number in
      let rest =
        String.sub line.text ~offset:(index + 1) ~len:(String.length line.text - index - 1)
        |> String.trim
      in
      Some (key, rest)

let parse_float = fun line_number value ->
  let normalized =
    if String.equal value ".inf" then
      "inf"
    else if String.equal value "-.inf" then
      "-inf"
    else if String.equal value ".nan" then
      "nan"
    else
      value
  in
  try Float.from_string normalized with
  | _ -> fail_line line_number ("invalid float literal '" ^ value ^ "'")

let parse_scalar_text = fun text line_number ->
  let value = String.trim text in
  if String.equal value "null" || String.equal value "~" || String.equal value "" then
    Null
  else if String.equal value "true" then
    Bool true
  else if String.equal value "false" then
    Bool false
  else if String.equal value "[]" then
    Seq []
  else if String.equal value "{}" then
    Map []
  else if Char.equal (String.unsafe_get value 0) '"' then
    let (string_value, next) = parse_double_quoted value line_number 0 in
    if Int.equal (skip_spaces value next) (String.length value) then
      String string_value
    else
      fail_line line_number "unexpected trailing characters after string literal"
  else if Char.equal (String.unsafe_get value 0) '\'' then
    let (string_value, next) = parse_single_quoted value line_number 0 in
    if Int.equal (skip_spaces value next) (String.length value) then
      String string_value
    else
      fail_line line_number "unexpected trailing characters after string literal"
  else
    try Int (Int64.from_string value) with
    | _ ->
        if
          String.contains value "."
          || String.contains value "e"
          || String.contains value "E"
          || String.equal value ".inf"
          || String.equal value "-.inf"
          || String.equal value ".nan"
        then
          Float (parse_float line_number value)
        else
          String value

let rec parse_nested_or_null = fun lines index parent_indent ->
  if index >= Array.length lines then
    (Null, index)
  else
    let next_line = Array.get_unchecked lines ~at:index in
    if next_line.indent > parent_indent then
      parse_node lines index next_line.indent
    else
      (Null, index)

and parse_tagged = fun lines line index head parent_indent ->
  let length = String.length head in
  let rec scan tag_end =
    if tag_end >= length then
      tag_end
    else
      match String.unsafe_get head tag_end with
      | ' ' -> tag_end
      | _ -> scan (tag_end + 1)
  in
  let tag_end = scan 1 in
  let tag = String.sub head ~offset:1 ~len:(tag_end - 1) in
  if String.equal tag "" then
    fail_line line.number "expected tag name after '!'";
  let rest =
    if Int.equal tag_end length then
      ""
    else
      String.sub head ~offset:tag_end ~len:(length - tag_end)
      |> String.trim
  in
  if String.equal rest "" then
    let (payload, next) = parse_nested_or_null lines index parent_indent in
    (Tagged (tag, payload), next)
  else
    (Tagged (tag, parse_scalar_text rest line.number), index)

and parse_value_head = fun lines line index head parent_indent ->
  let head = String.trim head in
  if String.equal head "" then
    parse_nested_or_null lines index parent_indent
  else if Char.equal (String.unsafe_get head 0) '!' then
    parse_tagged lines line index head parent_indent
  else
    (parse_scalar_text head line.number, index)

and parse_sequence = fun lines start_index indent ->
  let items = ref [] in
  let index = ref start_index in
  let continue = ref true in
  while !continue && !index < Array.length lines do
    let line = Array.get_unchecked lines ~at:!index in
    if not (Int.equal line.indent indent) then
      continue := false
    else if String.equal line.text "-" then
      let (value, next) = parse_nested_or_null lines (!index + 1) indent in
      items := value :: !items;
      index := next
    else if String.starts_with ~prefix:"- " line.text then
      let head = String.sub line.text ~offset:2 ~len:(String.length line.text - 2) in
      let (value, next) = parse_value_head lines line (!index + 1) head indent in
      items := value :: !items;
      index := next
    else
      continue := false
  done;
  (Seq (List.rev !items), !index)

and parse_mapping = fun lines start_index indent ->
  let items = ref [] in
  let index = ref start_index in
  let continue = ref true in
  while !continue && !index < Array.length lines do
    let line = Array.get_unchecked lines ~at:!index in
    if not (Int.equal line.indent indent) then
      continue := false
    else
      match split_mapping_head line with
      | None -> continue := false
      | Some (key, head) ->
          let (value, next) = parse_value_head lines line (!index + 1) head indent in
          items := (key, value) :: !items;
          index := next
  done;
  (Map (List.rev !items), !index)

and parse_node = fun lines index indent ->
  if index >= Array.length lines then
    fail "unexpected end of YAML input"
  else
    let line = Array.get_unchecked lines ~at:index in
    if not (Int.equal line.indent indent) then
      fail_line line.number "unexpected indentation"
    else if String.equal line.text "-" || String.starts_with ~prefix:"- " line.text then
      parse_sequence lines index indent
    else
      match split_mapping_head line with
      | Some _ -> parse_mapping lines index indent
      | None ->
          let (value, next) = parse_value_head lines line (index + 1) line.text indent in
          (value, next)

let parse_document = fun input ->
  try
    let lines = preprocess input in
    if Int.equal (Array.length lines) 0 then
      Ok Null
    else
      let first = Array.get_unchecked lines ~at:0 in
      let (value, next) = parse_node lines 0 first.indent in
      if Int.equal next (Array.length lines) then
        Ok value
      else
        let line = Array.get_unchecked lines ~at:next in
        Error (`Msg ("unexpected trailing YAML content at line " ^ Int.to_string line.number))
  with
  | Parse_failure message -> Error (`Msg message)
