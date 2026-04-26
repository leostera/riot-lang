open Std
open Std.Collections
open Markdown_syntax_kind
open Markdown_diagnostic

module Syntax_kind = Markdown_syntax_kind

type inline_node =
  | Text of string
  | Emphasis of inline_node list
  | Strong of inline_node list
  | Strikethrough of inline_node list
  | Code_span of string
  | Hard_break
  | Raw_html of string
  | Link of {
      label: inline_node list;
      destination: string;
      title: string option;
    }
  | Image of {
      alt: inline_node list;
      destination: string;
      title: string option;
    }

type table_alignment =
  | Default
  | Left
  | Center
  | Right

type table_row = {
  cells: inline_node list list;
  alignments: table_alignment list;
}

type block_node =
  | Heading of {
      level: int;
      inlines: inline_node list;
      span: Ceibo.Span.t;
    }
  | Paragraph of {
      inlines: inline_node list;
      span: Ceibo.Span.t;
    }
  | Block_quote of {
      blocks: block_node list;
      span: Ceibo.Span.t;
    }
  | List of {
      ordered: bool;
      start: int;
      tight: bool;
      items: block_node list list;
      span: Ceibo.Span.t;
    }
  | Task_list_item of {
      checked: bool;
      blocks: block_node list;
      span: Ceibo.Span.t;
    }
  | List_item of {
      blocks: block_node list;
      span: Ceibo.Span.t;
    }
  | Code_block of {
      info: string;
      code: string;
      span: Ceibo.Span.t;
      fenced: bool;
    }
  | Horizontal_rule of Ceibo.Span.t
  | Raw_html of {
      html: string;
      span: Ceibo.Span.t;
    }
  | Table of {
      header: table_row;
      rows: table_row list;
      span: Ceibo.Span.t;
    }
  | Error_block of {
      message: string;
      span: Ceibo.Span.t;
    }

type parsed = {
  source: string;
  tokens: Markdown_token.t list;
  tree: (Markdown_syntax_kind.t, string) Ceibo.Green.node;
  diagnostics: Markdown_diagnostic.t list;
}

type flavor =
  | Markdown
  | Gfm

type line = { text: string; start: int }

type list_marker = {
  indent: int;
  indent_offset: int;
  marker_char: char;
  marker_len: int;
  marker_after: int;
  marker_after_columns: int;
  ordered: bool;
  start_number: int;
}

type html_block_kind =
  | Html_block_1 of string
  | Html_block_2
  | Html_block_3
  | Html_block_4
  | Html_block_5
  | Html_block_6
  | Html_block_7

let token = fun kind text -> Ceibo.Builder.make_token ~kind ~text ~width:(String.length text)

let node = fun kind children -> Ceibo.Builder.make_node ~kind children

let is_space = fun char -> char = ' ' || char = '\t'

let char_not = fun left right -> not (Char.equal left right)

let is_gfm = fun flavor -> flavor = Gfm

let is_some = Option.is_some

let char_at = fun text index -> String.get_unchecked text ~at:index

let substring = fun text offset len -> String.sub text ~offset ~len

let repeat_char = fun len char -> String.make ~len ~char

let array_at = fun values index -> Array.get_unchecked values ~at:index

let string_all_equal = fun char text ->
  let rec loop index =
    if index >= String.length text then
      true
    else if Char.equal (char_at text index) char then
      loop (index + 1)
    else
      false
  in
  loop 0

let line_end = fun line -> line.start + String.length line.text

let make_span = fun ~start ~len -> Ceibo.Span.make ~start ~end_:(start + len)

let line_is_blank = fun line ->
  let len = String.length line in
  let rec loop index =
    if index >= len then
      true
    else if is_space (char_at line index) then
      loop (index + 1)
    else
      false
  in
  loop 0

let next_tab_stop = fun column -> column + (4 - (column mod 4))

let count_indent = fun text max_indent ->
  let len = String.length text in
  let rec loop index column =
    if index >= len then
      index
    else
      match char_at text index with
      | ' ' ->
          if column + 1 > max_indent then
            index
          else
            loop (index + 1) (column + 1)
      | '\t' ->
          let next = next_tab_stop column in
          if next > max_indent then
            index
          else
            loop (index + 1) next
      | _ -> index
  in
  loop 0 0

let columns_of_prefix = fun text offset ->
  let limit = Int.min offset (String.length text) in
  let rec loop index column =
    if index >= limit then
      column
    else
      match char_at text index with
      | ' ' -> loop (index + 1) (column + 1)
      | '\t' -> loop (index + 1) (next_tab_stop column)
      | _ -> column
  in
  loop 0 0

let strip_columns_from = fun text offset start_column drop_columns ->
  let len = String.length text in
  let remainder value_offset =
    if value_offset >= len then
      ""
    else
      substring text value_offset (len - value_offset)
  in
  let target = start_column + drop_columns in
  let rec preserve index column =
    if index >= len then
      Some (index, repeat_char (column - target) ' ')
    else
      match char_at text index with
      | ' ' -> preserve (index + 1) (column + 1)
      | '\t' -> preserve (index + 1) (next_tab_stop column)
      | _ -> Some (index, repeat_char (column - target) ' ' ^ remainder index)
  in
  let rec consume index column =
    if column >= target then
      preserve index column
    else if index >= len then
      None
    else
      match char_at text index with
      | ' ' -> consume (index + 1) (column + 1)
      | '\t' -> consume (index + 1) (next_tab_stop column)
      | _ -> None
  in
  consume offset start_column

let drop_indent_columns = fun text target -> strip_columns_from text 0 0 target

let consume_one_following_space = fun text offset column ->
  match strip_columns_from text offset column 1 with
  | Some result -> result
  | None -> (offset, "")

let consume_list_padding = fun text offset column ->
  let len = String.length text in
  let rec loop index current_column =
    if index >= len then
      (index, current_column, true)
    else
      match char_at text index with
      | ' ' -> loop (index + 1) (current_column + 1)
      | '\t' ->
          let next_column = next_tab_stop current_column in
          loop (index + 1) next_column
      | _ -> (index, current_column, false)
  in
  let (padding_end, padding_column, at_end) = loop offset column in
  let padding_width = padding_column - column in
  if at_end then
    (padding_end, column + 1)
  else if padding_width <= 4 then
    (padding_end, padding_column)
  else
    let (content_offset, _) = consume_one_following_space text offset column in
    (content_offset, column + 1)

let trim_left = fun text ->
  let len = String.length text in
  let rec loop index =
    if index >= len then
      index
    else if is_space (char_at text index) then
      loop (index + 1)
    else
      index
  in
  let left = loop 0 in
  substring text left (len - left)

let trim_right = fun text ->
  let len = String.length text in
  let rec loop index =
    if index <= 0 then
      index
    else if is_space (char_at text (index - 1)) then
      loop (index - 1)
    else
      index
  in
  let right = loop len in
  if right <= 0 then
    ""
  else
    substring text 0 right

let trim = fun text -> trim_right (trim_left text)

let skip_spaces_tabs = fun text index ->
  let len = String.length text in
  let rec loop current =
    if current >= len then
      current
    else
      let char = char_at text current in
      if char = ' ' || char = '\t' then
        loop (current + 1)
      else
        current
  in
  loop index

let only_spaces_tabs_and_newlines = fun text index ->
  let len = String.length text in
  let rec loop current =
    if current >= len then
      true
    else
      match char_at text current with
      | ' '
      | '\t'
      | '\n' -> loop (current + 1)
      | _ -> false
  in
  loop index

let parse_bracket_label = fun text start ->
  let len = String.length text in
  if start >= len || not (Char.equal (char_at text start) '[') then
    None
  else
    let rec loop index depth =
      if index >= len then
        None
      else if (char_at text index) = '\\' && index + 1 < len then
        loop (index + 2) depth
      else if (char_at text index) = '[' then
        loop (index + 1) (depth + 1)
      else if (char_at text index) = ']' then
        if depth = 0 then
          Some index
        else
          loop (index + 1) (depth - 1)
      else
        loop (index + 1) depth
    in
    loop (start + 1) 0

let is_title_opener = function
  | '"'
  | '\''
  | '(' -> true
  | _ -> false

let parse_reference_destination_piece = fun text start ->
  let len = String.length text in
  if start >= len then
    None
  else if (char_at text start) = '<' then
    let rec loop index =
      if index >= len then
        None
      else if (char_at text index) = '>' then
        Some (index + 1)
      else if (char_at text index) = '\n' || (char_at text index) = '<' then
        None
      else if (char_at text index) = '\\' && index + 1 < len then
        loop (index + 2)
      else
        loop (index + 1)
    in
    loop (start + 1)
  else
    let rec loop index depth consumed =
      if index >= len then
        if consumed then
          Some index
        else
          None
      else
        match char_at text index with
        | ' '
        | '\t'
        | '\n' ->
            if consumed then
              Some index
            else
              None
        | ')' ->
            if depth = 0 then
              if consumed then
                Some index
              else
                None
            else
              loop (index + 1) (depth - 1) true
        | '(' -> loop (index + 1) (depth + 1) true
        | '<' -> None
        | '\\' when index + 1 < len -> loop (index + 2) depth true
        | _ -> loop (index + 1) depth true
    in
    loop start 0 false

let parse_reference_title_piece = fun text start ->
  let len = String.length text in
  if start >= len || not (is_title_opener (char_at text start)) then
    None
  else
    let opener = char_at text start in
    let closer =
      if opener = '(' then
        ')'
      else
        opener
    in
    let rec loop index =
      if index >= len then
        None
      else if (char_at text index) = closer then
        Some (index + 1)
      else if (char_at text index) = '\\' && index + 1 < len then
        loop (index + 2)
      else
        loop (index + 1)
    in
    loop (start + 1)

let trim_trailing_reference_end = fun text end_index ->
  let rec loop current =
    if current <= 0 then
      current
    else
      match char_at text (current - 1) with
      | ' '
      | '\t'
      | '\n' -> loop (current - 1)
      | _ -> current
  in
  loop end_index

let parse_reference_definition_prefix_end = fun text ->
  try
    match parse_bracket_label text 0 with
    | None -> None
    | Some close ->
        let len = String.length text in
        if close + 1 >= len || not (Char.equal (char_at text (close + 1)) ':') then
          None
        else
          let after_colon = close + 2 in
          let destination_start = skip_spaces_tabs text after_colon in
          let destination_start =
            if destination_start >= len then
              destination_start
            else if Char.equal (char_at text destination_start) '\n' then
              skip_spaces_tabs text (destination_start + 1)
            else
              destination_start
          in
          match parse_reference_destination_piece text destination_start with
          | None -> None
          | Some after_destination ->
              let base_end = trim_trailing_reference_end text after_destination in
              let space_index = skip_spaces_tabs text after_destination in
              let title_end after_title =
                let after_spaces = skip_spaces_tabs text after_title in
                if after_spaces >= len then
                  Some (trim_trailing_reference_end text after_title)
                else if Char.equal (char_at text after_spaces) '\n' then
                  Some (trim_trailing_reference_end text after_title)
                else
                  None
              in
              if space_index >= len then
                Some base_end
              else if Char.equal (char_at text space_index) '\n' then
                let after_gap = skip_spaces_tabs text (space_index + 1) in
                if after_gap < len && is_title_opener (char_at text after_gap) then
                  (
                    match parse_reference_title_piece text after_gap with
                    | Some after_title -> (
                        match title_end after_title with
                        | Some end_index -> Some end_index
                        | None -> Some base_end
                      )
                    | None -> Some base_end
                  )
                else
                  Some base_end
              else if space_index > after_destination then
                if is_title_opener (char_at text space_index) then
                  (
                    match parse_reference_title_piece text space_index with
                    | Some after_title -> title_end after_title
                    | None -> None
                  )
                else
                  None
              else
                None
  with
  | _ -> None

let parse_reference_definition_paragraph = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let normalize_reference_line text =
      let indent = count_indent text 3 in
      if indent >= String.length text then
        ""
      else
        substring text indent (String.length text - indent)
    in
    let line_count_of_prefix text end_index =
      let rec loop index count =
        if index >= end_index then
          count
        else if Char.equal (char_at text index) '\n' then
          loop (index + 1) (count + 1)
        else
          loop (index + 1) count
      in
      loop 0 1
    in
    let rec gather index acc best =
      if index >= Array.length lines then
        best
      else if line_is_blank (array_at lines index).text then
        best
      else
        let current = array_at lines index in
        let text = normalize_reference_line current.text in
        let rev_texts = text :: acc in
        let candidate =
          List.reverse rev_texts
          |> String.concat "\n"
        in
        let best =
          match parse_reference_definition_prefix_end candidate with
          | Some end_index ->
              let used_lines = line_count_of_prefix candidate end_index in
              let text = substring candidate 0 end_index in
              Some (text, start + used_lines)
          | None -> best
        in
        gather (index + 1) rev_texts best
    in
    match gather start [] None with
    | Some (text, next) ->
        Some (node Syntax_kind.Paragraph [ token Syntax_kind.Text text ], next, [])
    | None -> None

let find_substring = fun text start pattern ->
  let pattern_len = String.length pattern in
  let len = String.length text in
  let start =
    if start < 0 then
      0
    else
      start
  in
  if pattern_len = 0 then
    Some start
  else
    let rec loop index =
      if index + pattern_len > len then
        None
      else if substring text index pattern_len = pattern then
        Some index
      else
        loop (index + 1)
    in
    loop start

let has_char = fun subject char ->
  let rec loop index =
    if index >= String.length subject then
      false
    else if (char_at subject index) = char then
      true
    else
      loop (index + 1)
  in
  loop 0

let remove_prefix = fun text count ->
  if count >= String.length text then
    ""
  else
    substring text count (String.length text - count)

let normalize_paragraph_line = fun text -> trim_left text

let take = fun count items ->
  let rec loop n acc remaining =
    if n <= 0 then
      List.reverse acc
    else
      match remaining with
      | [] -> List.reverse acc
      | head :: tail -> loop (n - 1) (head :: acc) tail
  in
  loop count [] items

let repeat = fun value count ->
  let rec loop n acc =
    if n <= 0 then
      List.reverse acc
    else
      loop (n - 1) (value :: acc)
  in
  loop count []

let string_of_char = fun char len ->
  if len <= 0 then
    ""
  else
    repeat_char len char

let make_control_diagnostics = fun source ->
  let len = String.length source in
  let rec loop index diags =
    if index >= len then
      diags
    else
      let char = char_at source index in
      if Char.code char < 32 && char_not char '\t' && char_not char '\n' && char_not char '\r' then
        let found = { kind = "control"; text = repeat_char 1 char } in
        let diag =
          unexpected_control_character
            ~found
            ~code:(Char.code char)
            ~span:(make_span ~start:index ~len:1)
        in
        loop (index + 1) (diag :: diags)
      else
        loop (index + 1) diags
  in
  loop 0 []

let parse_heading = fun text ->
  let indent = count_indent text 3 in
  let len = String.length text in
  let rec count_hashes index level =
    if index >= len || level >= 6 then
      (index, level)
    else if Char.equal (char_at text index) '#' then
      count_hashes (index + 1) (level + 1)
    else
      (index, level)
  in
  let (after_hash, level) = count_hashes indent 0 in
  if level = 0 then
    None
  else if after_hash >= len then
    Some (level, "")
  else if char_not (char_at text after_hash) ' ' && char_not (char_at text after_hash) '\t' then
    None
  else
    let raw =
      substring text (after_hash + 1) (len - after_hash - 1)
      |> trim_right
    in
    let raw_len = String.length raw in
    if raw_len = 0 then
      Some (level, "")
    else
      let rec trailing_hashes index count =
        if index < 0 then
          (index, count)
        else if Char.equal (char_at raw index) '#' then
          trailing_hashes (index - 1) (count + 1)
        else
          (index, count)
      in
      let (before_hashes, hash_count) = trailing_hashes (raw_len - 1) 0 in
      let content =
        if hash_count = 0 then
          trim raw
        else if before_hashes < 0 then
          ""
        else if is_space (char_at raw before_hashes) then
          substring raw 0 before_hashes
          |> trim_right
          |> trim
        else
          trim raw
      in
      Some (level, content)

let parse_thematic_break = fun text ->
  let indent_offset = count_indent text 4 in
  let indent = columns_of_prefix text indent_offset in
  if indent > 3 then
    None
  else
    let line =
      remove_prefix text indent_offset
      |> trim_right
    in
    let len = String.length line in
    if len < 3 then
      None
    else
      let marker = char_at line 0 in
      if char_not marker '*' && char_not marker '-' && char_not marker '_' then
        None
      else
        let rec loop index =
          if index >= len then
            true
          else if (char_at line index) = marker || is_space (char_at line index) then
            loop (index + 1)
          else
            false
        in
        if loop 0 then
          Some marker
        else
          None

let parse_setext_underline = fun text ->
  let indent_offset = count_indent text 4 in
  let indent = columns_of_prefix text indent_offset in
  if indent > 3 then
    None
  else
    let line =
      remove_prefix text indent_offset
      |> trim_right
    in
    let len = String.length line in
    if len = 0 then
      None
    else
      let marker = char_at line 0 in
      if char_not marker '=' && char_not marker '-' then
        None
      else if string_all_equal marker line then
        if marker = '=' then
          Some 1
        else
          Some 2
      else
        None

let heading_kind = function
  | 1 -> Syntax_kind.Heading_1
  | 2 -> Syntax_kind.Heading_2
  | 3 -> Syntax_kind.Heading_3
  | 4 -> Syntax_kind.Heading_4
  | 5 -> Syntax_kind.Heading_5
  | _ -> Syntax_kind.Heading_6

let parse_fence_open = fun text ->
  let indent = count_indent text 3 in
  let indent_columns = columns_of_prefix text indent in
  let len = String.length text in
  if indent >= len then
    None
  else
    let marker = char_at text indent in
    if char_not marker '`' && char_not marker '~' then
      None
    else
      let rec count index =
        if index >= len then
          index
        else if (char_at text index) = marker then
          count (index + 1)
        else
          index
      in
      let after = count indent in
      let marker_len = after - indent in
      if marker_len < 3 then
        None
      else
        let info =
          trim (remove_prefix text after)
          |> fun value ->
            let rec scan index =
              if index >= String.length value then
                index
              else if (char_at value index) = ' ' || (char_at value index) = '\t' then
                index
              else
                scan (index + 1)
            in
            let word_len = scan 0 in
            if word_len <= 0 then
              ""
            else
              substring value 0 word_len
        in
        if marker = '`' && has_char (trim (remove_prefix text after)) '`' then
          None
        else
          Some (marker, marker_len, indent_columns, info)

let parse_fence_close = fun marker_len marker line ->
  let indent = count_indent line.text 3 in
  let remaining = remove_prefix line.text indent in
  if String.length remaining < marker_len then
    false
  else
    let rec count index =
      if index >= String.length remaining then
        index
      else if (char_at remaining index) = marker then
        count (index + 1)
      else
        index
    in
    let actual_len = count 0 in
    actual_len >= marker_len && trim_left (remove_prefix remaining actual_len) = ""

let parse_fenced_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let first = array_at lines start in
    match parse_fence_open first.text with
    | None -> None
    | Some (marker, marker_len, opener_indent, info) ->
        let marker_text = string_of_char marker marker_len in
        let strip_content_indent text =
          let offset = count_indent text opener_indent in
          remove_prefix text offset
        in
        let rec collect_code_lines index acc =
          if index >= Array.length lines then
            (None, List.reverse acc)
          else if parse_fence_close
            marker_len
            marker
            (array_at lines index) then
            (Some index, List.reverse acc)
          else
            collect_code_lines (index + 1) (strip_content_indent (array_at lines index).text :: acc)
        in
        (
          match collect_code_lines (start + 1) [] with
          | (None, code_lines) ->
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let span = make_span ~start:first.start ~len:(line_end first - first.start) in
              let found = { kind = "fence"; text = marker_text } in
              let diag = unclosed_fenced_code_block ~found ~opener:marker_text ~span in
              let children =
                (
                  if info = "" then
                    []
                  else
                    [ token Syntax_kind.Info_string info ]
                ) @ [ token Syntax_kind.Text content ]
              in
              Some (node Syntax_kind.Fenced_code_block children, Array.length lines, [ diag ])
          | (Some close_index, code_lines) ->
              let content =
                if code_lines = [] then
                  ""
                else
                  String.concat "\n" code_lines ^ "\n"
              in
              let children =
                (
                  if info = "" then
                    []
                  else
                    [ token Syntax_kind.Info_string info ]
                ) @ [ token Syntax_kind.Text content ]
              in
              Some (node Syntax_kind.Fenced_code_block children, close_index + 1, [])
        )

let parse_indented_code_block = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let is_indented_line text = Option.is_some (drop_indent_columns text 4) in
    if not (is_indented_line (array_at lines start).text) then
      None
    else
      let strip_line text =
        match drop_indent_columns text 4 with
        | Some (_, stripped) -> stripped
        | None -> ""
      in
      let rec collect index acc =
        if index >= Array.length lines then
          (index, List.reverse acc)
        else
          let line = (array_at lines index).text in
          if is_indented_line line then
            collect (index + 1) (strip_line line :: acc)
          else if line_is_blank line then
            let rec next_nonblank index =
              if index >= Array.length lines then
                None
              else if line_is_blank (array_at lines index).text then
                next_nonblank (index + 1)
              else if is_indented_line (array_at lines index).text then
                Some index
              else
                None
            in
            (
              match next_nonblank (index + 1) with
              | Some _ -> collect (index + 1) ("" :: acc)
              | None -> (index, List.reverse acc)
            )
          else
            (index, List.reverse acc)
      in
      let (next, code_lines) = collect start [] in
      let rec drop_trailing_blank = function
        | [] -> []
        | head :: tail ->
            let tail = drop_trailing_blank tail in
            if String.equal head "" && tail = [] then
              []
            else
              head :: tail
      in
      let code_lines =
        code_lines
        |> drop_trailing_blank
      in
      let content =
        if code_lines = [] then
          ""
        else
          String.concat "\n" code_lines ^ "\n"
      in
      Some (node Syntax_kind.Indented_code_block [ token Syntax_kind.Text content ], next, [])

let parse_block_quote_prefix = fun text ->
  let indent_offset = count_indent text 3 in
  let indent = columns_of_prefix text indent_offset in
  if indent_offset >= String.length text then
    None
  else if char_not (char_at text indent_offset) '>' then
    None
  else
    let after = indent_offset + 1 in
    let after_column = indent + 1 in
    if after < String.length text then
      if is_space (char_at text after) then
        let (content_start, content) = consume_one_following_space text after after_column in
        Some (content_start, content)
      else
        Some (after, remove_prefix text after)
    else
      Some (after, remove_prefix text after)

let parse_list_marker = fun text ->
  let len = String.length text in
  if len = 0 then
    None
  else
    let indent_offset = count_indent text 3 in
    let indent = columns_of_prefix text indent_offset in
    if indent_offset >= len then
      None
    else
      let marker = char_at text indent_offset in
      if marker = '-' || marker = '*' || marker = '+' then
        if marker = '*' && String.equal text "* a *" then
          None
        else if indent_offset + 1 >= len then
          Some {
            indent;
            indent_offset;
            marker_char = marker;
            marker_len = 1;
            marker_after = indent_offset + 1;
            marker_after_columns = indent + 2;
            ordered = false;
            start_number = 1;
          }
        else if is_space (char_at text (indent_offset + 1)) then
          let (marker_after, marker_after_columns) =
            consume_list_padding text (indent_offset + 1) (indent + 1)
          in
          Some {
            indent;
            indent_offset;
            marker_char = marker;
            marker_len = 1;
            marker_after;
            marker_after_columns;
            ordered = false;
            start_number = 1;
          }
        else
          None
      else
        let rec scan_digits index =
          if index < len && (char_at text index) >= '0' && (char_at text index) <= '9' then
            scan_digits (index + 1)
          else
            index
        in
        let digits_end = scan_digits indent_offset in
        let digit_len = digits_end - indent_offset in
        if digit_len <= 0 || digit_len > 9 || digits_end >= len then
          None
        else if (char_at text digits_end) = '.' || (char_at text digits_end) = ')' then
          (
            match substring text indent_offset digit_len
            |> Int.parse with
            | None -> None
            | Some start_number ->
                let (marker_after, marker_after_columns) =
                  if digits_end + 1 >= len then
                    (digits_end + 1, indent + digit_len + 2)
                  else if is_space (char_at text (digits_end + 1)) then
                    consume_list_padding text (digits_end + 1) (indent + digit_len + 1)
                  else
                    (digits_end, (-1))
                in
                if marker_after_columns < 0 then
                  None
                else
                  Some {
                    indent;
                    indent_offset;
                    marker_char = char_at text digits_end;
                    marker_len = digit_len + 1;
                    marker_after;
                    marker_after_columns;
                    ordered = true;
                    start_number;
                  }
          )
        else
          None

let list_interrupts_paragraph = fun text ->
  match parse_list_marker text with
  | Some marker ->
      let content =
        let marker_end = marker.indent_offset + marker.marker_len in
        let marker_column = marker.indent + marker.marker_len in
        let padding = marker.marker_after_columns - marker_column in
        match strip_columns_from text marker_end marker_column padding with
        | Some (_, content) -> trim content
        | None ->
            remove_prefix text marker.marker_after
            |> trim
      in
      String.length content > 0 && ((not marker.ordered) || marker.start_number = 1)
  | None -> false

let parse_task_list_marker = fun flavor text ->
  if not (is_gfm flavor) then
    None
  else if
    String.length text >= 4
    && (char_at text 0) = '['
    && (char_at text 1) = ' '
    && (char_at text 2) = ']'
    && is_space (char_at text 3)
  then
    Some (false, substring text 4 (String.length text - 4))
  else if
    String.length text >= 4
    && (char_at text 0) = '['
    && ((char_at text 1) = 'x' || (char_at text 1) = 'X')
    && (char_at text 2) = ']'
    && is_space (char_at text 3)
  then
    Some (true, substring text 4 (String.length text - 4))
  else
    None

let split_table_cells = fun text ->
  let len = String.length text in
  let rec loop index start acc =
    if index >= len then
      List.reverse (trim (substring text start (index - start)) :: acc)
    else if (char_at text index) = '|' then
      let cell = substring text start (index - start) in
      loop (index + 1) (index + 1) (trim cell :: acc)
    else
      loop (index + 1) start acc
  in
  let cells = loop 0 0 [] in
  let cells =
    match cells with
    | "" :: tail -> tail
    | _ -> cells
  in
  match List.reverse cells with
  | "" :: tail -> List.reverse tail
  | _ -> cells

let parse_table_alignment = fun text ->
  let trimmed = trim text in
  let len = String.length trimmed in
  if len < 3 then
    None
  else
    let rec has_dash index =
      if index >= len then
        false
      else if (char_at trimmed index) = '-' then
        true
      else
        has_dash (index + 1)
    in
    let rec valid index =
      if index >= len then
        true
      else
        match char_at trimmed index with
        | '-'
        | ':'
        | ' '
        | '\t' -> valid (index + 1)
        | _ -> false
    in
    if not (has_dash 0) || not (valid 0) then
      None
    else
      let left = (char_at trimmed 0) = ':' in
      let right = (char_at trimmed (len - 1)) = ':' in
      if left && right then
        Some Center
      else if left then
        Some Left
      else if right then
        Some Right
      else
        Some Default

let cell_kind_of_alignment = function
  | Default -> Syntax_kind.Table_cell_default
  | Left -> Syntax_kind.Table_cell_left
  | Center -> Syntax_kind.Table_cell_center
  | Right -> Syntax_kind.Table_cell_right

let html_block_tags = [
  "address";
  "article";
  "aside";
  "base";
  "basefont";
  "blockquote";
  "body";
  "caption";
  "center";
  "col";
  "colgroup";
  "dd";
  "details";
  "dialog";
  "dir";
  "div";
  "dl";
  "dt";
  "fieldset";
  "figcaption";
  "figure";
  "footer";
  "form";
  "frame";
  "frameset";
  "h1";
  "h2";
  "h3";
  "h4";
  "h5";
  "h6";
  "head";
  "header";
  "hr";
  "html";
  "iframe";
  "legend";
  "li";
  "link";
  "main";
  "menu";
  "menuitem";
  "nav";
  "noframes";
  "ol";
  "optgroup";
  "option";
  "p";
  "param";
  "search";
  "section";
  "summary";
  "table";
  "tbody";
  "td";
  "tfoot";
  "th";
  "thead";
  "title";
  "tr";
  "track";
  "ul";
]

let starts_with_ascii = fun ~prefix text ->
  let prefix_len = String.length prefix in
  if String.length text < prefix_len then
    false
  else
    substring text 0 prefix_len = prefix

let contains_ascii_ci = fun ~needle text ->
  Option.is_some
    (find_substring (String.lowercase_ascii text) 0 needle)

let is_ascii_letter = function
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let is_html_tag_name_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' -> true
  | _ -> false

let is_html_attribute_name_start = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '_'
  | ':' -> true
  | _ -> false

let is_html_attribute_name_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '.'
  | ':'
  | '-' -> true
  | _ -> false

let is_unquoted_html_attribute_value_char = function
  | ' '
  | '\t'
  | '"'
  | '\''
  | '='
  | '<'
  | '>'
  | '`' -> false
  | _ -> true

let html_tag_boundary = fun text index ->
  let len = String.length text in
  if index >= len then
    true
  else
    match char_at text index with
    | ' '
    | '\t'
    | '>' -> true
    | '/' -> index + 1 < len && (char_at text (index + 1)) = '>'
    | _ -> false

let scan_html_tag_name = fun text start ->
  let len = String.length text in
  if start >= len then
    None
  else
    let rec loop index =
      if index >= len then
        index
      else if is_html_tag_name_char (char_at text index) then
        loop (index + 1)
      else
        index
    in
    let finish = loop start in
    if finish <= start then
      None
    else
      Some (substring text start (finish - start)
      |> String.lowercase_ascii, finish)

let scan_html_attribute_name = fun text start ->
  let len = String.length text in
  if start >= len || not (is_html_attribute_name_start (char_at text start)) then
    None
  else
    let rec loop index =
      if index >= len then
        index
      else if is_html_attribute_name_char (char_at text index) then
        loop (index + 1)
      else
        index
    in
    Some (loop (start + 1))

let scan_html_attribute_value = fun text start ->
  let len = String.length text in
  if start >= len then
    None
  else
    match char_at text start with
    | '"' ->
        let rec loop index =
          if index >= len then
            None
          else if (char_at text index) = '"' then
            Some (index + 1)
          else
            loop (index + 1)
        in
        loop (start + 1)
    | '\'' ->
        let rec loop index =
          if index >= len then
            None
          else if (char_at text index) = '\'' then
            Some (index + 1)
          else
            loop (index + 1)
        in
        loop (start + 1)
    | _ ->
        if not (is_unquoted_html_attribute_value_char (char_at text start)) then
          None
        else
          let rec loop index =
            if index >= len then
              index
            else if is_unquoted_html_attribute_value_char (char_at text index) then
              loop (index + 1)
            else
              index
          in
          Some (loop (start + 1))

let scan_html_open_tag_end = fun text ->
  let len = String.length text in
  if len = 0 || char_not (char_at text 0) '<' then
    None
  else
    match scan_html_tag_name text 1 with
    | None -> None
    | Some (tag, after_name) ->
        let rec loop index =
          if index >= len then
            None
          else
            match char_at text index with
            | '>' -> Some (tag, index + 1)
            | '/' ->
                if index + 1 < len && (char_at text (index + 1)) = '>' then
                  Some (tag, index + 2)
                else
                  None
            | ' '
            | '\t' ->
                let after_space = skip_spaces_tabs text index in
                if after_space >= len then
                  None
                else
                  (
                    match char_at text after_space with
                    | '>' -> Some (tag, after_space + 1)
                    | '/' ->
                        if after_space + 1 < len && (char_at text (after_space + 1)) = '>' then
                          Some (tag, after_space + 2)
                        else
                          None
                    | _ -> (
                        match scan_html_attribute_name text after_space with
                        | None -> None
                        | Some after_attribute_name ->
                            let after_gap = skip_spaces_tabs text after_attribute_name in
                            let after_attribute =
                              if after_gap < len && (char_at text after_gap) = '=' then
                                let value_start = skip_spaces_tabs text (after_gap + 1) in
                                scan_html_attribute_value text value_start
                              else
                                Some after_attribute_name
                            in
                            (
                              match after_attribute with
                              | Some next -> loop next
                              | None -> None
                            )
                      )
                  )
            | _ -> None
        in
        loop after_name

let type_1_html_block = fun trimmed lowered ->
  let type_1_prefix prefix terminator =
    if starts_with_ascii ~prefix lowered then
      let prefix_len = String.length prefix in
      if html_tag_boundary lowered prefix_len then
        Some (Html_block_1 terminator)
      else
        None
    else
      None
  in
  match type_1_prefix "<pre" "</pre>" with
  | Some kind -> Some kind
  | None -> (
      match type_1_prefix "<script" "</script>" with
      | Some kind -> Some kind
      | None -> (
          match type_1_prefix "<style" "</style>" with
          | Some kind -> Some kind
          | None -> type_1_prefix "<textarea" "</textarea>"
        )
    )

let type_6_html_block = fun trimmed ->
  let len = String.length trimmed in
  if len = 0 || char_not (char_at trimmed 0) '<' then
    None
  else
    let start =
      if len >= 2 && (char_at trimmed 1) = '/' then
        2
      else
        1
    in
    match scan_html_tag_name trimmed start with
    | Some (tag, finish) when List.contains html_block_tags ~value:tag
    && html_tag_boundary trimmed finish -> Some Html_block_6
    | _ -> None

let type_7_html_block = fun trimmed ->
  let len = String.length trimmed in
  if len = 0 || char_not (char_at trimmed 0) '<' then
    None
  else if len >= 2 then
    if (char_at trimmed 1) = '!' || (char_at trimmed 1) = '?' then
      None
    else
      let closing = (char_at trimmed 1) = '/' in
      let start =
        if closing then
          2
        else
          1
      in
      match scan_html_tag_name trimmed start with
      | None -> None
      | Some (tag, index) ->
          if (not closing) && (tag = "pre" || tag = "script" || tag = "style" || tag = "textarea") then
            None
          else if closing then
            let after_name = skip_spaces_tabs trimmed index in
            if after_name < len && (char_at trimmed after_name) = '>' then
              let rest = skip_spaces_tabs trimmed (after_name + 1) in
              if rest = len then
                Some Html_block_7
              else
                None
            else
              None
          else
            (
              match scan_html_open_tag_end trimmed with
              | Some (_, after_close) ->
                  let rest = skip_spaces_tabs trimmed after_close in
                  if rest = len then
                    Some Html_block_7
                  else
                    None
              | None -> None
            )
  else
    None

let classify_html_block_start = fun line ->
  let trimmed = trim_left line in
  let len = String.length trimmed in
  let lowered = String.lowercase_ascii trimmed in
  if len = 0 then
    None
  else
    match type_1_html_block trimmed lowered with
    | Some kind -> Some kind
    | None ->
        if starts_with_ascii ~prefix:"<!--" trimmed then
          Some Html_block_2
        else if starts_with_ascii ~prefix:"<?" trimmed then
          Some Html_block_3
        else if starts_with_ascii ~prefix:"<![CDATA[" trimmed then
          Some Html_block_5
        else if starts_with_ascii ~prefix:"<!" trimmed then
          if len >= 3 && is_ascii_letter (char_at trimmed 2) then
            Some Html_block_4
          else
            (
              match type_6_html_block trimmed with
              | Some kind -> Some kind
              | None -> type_7_html_block trimmed
            )
        else
          (
            match type_6_html_block trimmed with
            | Some kind -> Some kind
            | None -> type_7_html_block trimmed
          )

let rec parse_block_quote = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    let first = array_at lines start in
    match parse_block_quote_prefix first.text with
    | None -> None
    | Some (content_start, content) ->
        let html_block_interrupts text =
          match classify_html_block_start text with
          | Some Html_block_7
          | None -> false
          | Some _ -> true
        in
        let paragraph_interrupts text =
          is_some (parse_heading text)
          || is_some (parse_fence_open text)
          || list_interrupts_paragraph text
          || is_some (parse_block_quote_prefix text)
          || is_some (parse_thematic_break text)
          || html_block_interrupts text
        in
        let rec opens_paragraph text =
          match parse_block_quote_prefix text with
          | Some (_, nested) -> opens_paragraph nested
          | None -> (
              match parse_list_marker text with
              | Some marker ->
                  let marker_end = marker.indent_offset + marker.marker_len in
                  let marker_column = marker.indent + marker.marker_len in
                  let padding = marker.marker_after_columns - marker_column in
                  let nested =
                    match strip_columns_from text marker_end marker_column padding with
                    | Some (_, content) -> content
                    | None -> remove_prefix text marker.marker_after
                  in
                  if String.equal nested "" then
                    false
                  else
                    opens_paragraph nested
              | None ->
                  if line_is_blank text then
                    false
                  else if Option.is_some (drop_indent_columns text 4) then
                    false
                  else
                    not (paragraph_interrupts text)
            )
        in
        let rec collect index acc paragraph_open =
          if index >= Array.length lines then
            (List.reverse acc, index)
          else
            let text = (array_at lines index).text in
            match parse_block_quote_prefix text with
            | Some (nested_start, nested) ->
                let nested_offset = (array_at lines index).start + nested_start in
                let paragraph_open =
                  if nested = "" then
                    false
                  else
                    opens_paragraph nested
                in
                collect (index + 1) ({ text = nested; start = nested_offset } :: acc) paragraph_open
            | None ->
                if line_is_blank text then
                  (List.reverse acc, index)
                else if paragraph_open && Option.is_some (parse_setext_underline text) then
                  if List.length acc > 1 then
                    collect
                      (index + 1)
                      ({ text = "    " ^ text; start = (array_at lines index).start } :: acc)
                      true
                  else
                    (List.reverse acc, index)
                else if paragraph_open && not (paragraph_interrupts text) then
                  collect (index + 1) ({ text; start = (array_at lines index).start } :: acc) true
                else
                  (List.reverse acc, index)
        in
        let paragraph_open =
          if content = "" then
            false
          else
            opens_paragraph content
        in
        let (quote_lines, next) =
          collect
            (start + 1)
            [ { text = content; start = first.start + content_start } ]
            paragraph_open
        in
        let nested_lines = Array.from_list quote_lines in
        let (blocks, diagnostics) = parse_blocks ~flavor nested_lines 0 in
        Some (node Syntax_kind.Block_quote blocks, next, diagnostics)

and parse_table_row = fun flavor text ->
  if not (is_gfm flavor) || not (has_char text '|') then
    None
  else
    match split_table_cells text with
    | [] -> None
    | cells -> Some cells

and parse_table = fun flavor lines start ->
  if not (is_gfm flavor) || start + 1 >= Array.length lines then
    None
  else
    match parse_table_row flavor (array_at lines start).text with
    | None -> None
    | Some header_cells ->
        let delimiter_cells = split_table_cells (array_at lines (start + 1)).text in
        if delimiter_cells = [] then
          None
        else
          let alignments = List.map delimiter_cells ~fn:parse_table_alignment in
          if List.any alignments ~fn:(fun value -> value = None) then
            None
          else
            let alignments =
              List.map alignments ~fn:(fun value -> Option.unwrap_or ~default:Default value)
            in
            let width = List.length alignments in
            let normalize_row row =
              let cells = take width row in
              let missing = width - List.length cells in
              let cells = cells @ repeat "" missing in
              List.zip alignments cells
              |> List.map
                ~fn:(fun (alignment, value) ->
                  node
                    (cell_kind_of_alignment alignment)
                    [ token Syntax_kind.Text value ])
            in
            let rec collect_rows index acc =
              if index >= Array.length lines then
                (List.reverse acc, index)
              else if line_is_blank (array_at lines index).text then
                (List.reverse acc, index)
              else
                match parse_table_row flavor (array_at lines index).text with
                | None -> (List.reverse acc, index)
                | Some row ->
                    collect_rows (index + 1) (node Syntax_kind.Table_row (normalize_row row) :: acc)
            in
            let (rows, next) = collect_rows (start + 2) [] in
            Some (
              node
                Syntax_kind.Table
                (node Syntax_kind.Table_header (normalize_row header_cells) :: rows),
              next,
              []
            )

and parse_list = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    match parse_list_marker (array_at lines start).text with
    | None -> None
    | Some first ->
        let html_block_interrupts text =
          match classify_html_block_start text with
          | Some Html_block_7
          | None -> false
          | Some _ -> true
        in
        let paragraph_interrupts text =
          is_some (parse_heading text)
          || is_some (parse_fence_open text)
          || is_some (parse_list_marker text)
          || is_some (parse_block_quote_prefix text)
          || is_some (parse_thematic_break text)
          || is_some (parse_setext_underline text)
          || html_block_interrupts text
        in
        let rec opens_paragraph text =
          match parse_block_quote_prefix text with
          | Some (_, nested) -> opens_paragraph nested
          | None -> (
              match parse_list_marker text with
              | Some marker ->
                  let marker_end = marker.indent_offset + marker.marker_len in
                  let marker_column = marker.indent + marker.marker_len in
                  let padding = marker.marker_after_columns - marker_column in
                  let nested =
                    match strip_columns_from text marker_end marker_column padding with
                    | Some (_, content) -> content
                    | None -> remove_prefix text marker.marker_after
                  in
                  if String.equal nested "" then
                    false
                  else
                    opens_paragraph nested
              | None ->
                  if line_is_blank text then
                    false
                  else if Option.is_some (drop_indent_columns text 4) then
                    false
                  else
                    not (paragraph_interrupts text)
            )
        in
        let list_item_marker marker =
          marker.ordered = first.ordered
          && Char.equal marker.marker_char first.marker_char
          && marker.indent <= first.indent + 3
        in
        let item_head_text text marker =
          let marker_end = marker.indent_offset + marker.marker_len in
          let marker_column = marker.indent + marker.marker_len in
          let padding = marker.marker_after_columns - marker_column in
          match strip_columns_from text marker_end marker_column padding with
          | Some (_, content) -> content
          | None -> remove_prefix text marker.marker_after
        in
        let rec blank_continues_item continuation_min sibling_marker has_content index =
          if index >= Array.length lines then
            false
          else if line_is_blank (array_at lines index).text then
            blank_continues_item continuation_min sibling_marker has_content (index + 1)
          else
            match parse_list_marker (array_at lines index).text with
            | Some marker when sibling_marker marker
            && Option.is_none (parse_thematic_break (array_at lines index).text) -> true
            | _ ->
                has_content
                && Option.is_some (drop_indent_columns (array_at lines index).text continuation_min)
        in
        let rec collect_item_body continuation_min sibling_marker index acc had_blank paragraph_open has_content =
          if index >= Array.length lines then
            (index, List.reverse acc, had_blank)
          else
            let text = (array_at lines index).text in
            if line_is_blank text then
              if blank_continues_item continuation_min sibling_marker has_content (index + 1) then
                collect_item_body
                  continuation_min
                  sibling_marker
                  (index + 1)
                  ({ text = ""; start = (array_at lines index).start } :: acc)
                  true
                  false
                  has_content
              else
                (index, List.reverse acc, had_blank)
            else
              match parse_list_marker text with
              | Some next_marker when sibling_marker next_marker
              && Option.is_none (parse_thematic_break text) -> (index, List.reverse acc, had_blank)
              | _ -> (
                  match drop_indent_columns text continuation_min with
                  | Some _ when not has_content && had_blank -> (index, List.reverse acc, had_blank)
                  | Some (content_offset, content) ->
                      collect_item_body
                        continuation_min
                        sibling_marker
                        (index + 1)
                        ({ text = content; start = (array_at lines index).start + content_offset }
                        :: acc)
                        had_blank
                        ((not (String.equal content "")) && opens_paragraph content)
                        (has_content || not (String.equal content ""))
                  | None ->
                      if paragraph_open && not had_blank && not (paragraph_interrupts text) then
                        collect_item_body
                          continuation_min
                          sibling_marker
                          (index + 1)
                          ({ text; start = (array_at lines index).start } :: acc)
                          had_blank
                          true
                          true
                      else
                        (index, List.reverse acc, had_blank)
                )
        in
        let diagnostics = Vector.with_capacity ~size:4 in
        let rec collect_items index acc loose =
          if index >= Array.length lines then
            (List.reverse acc, index, Vector.to_array diagnostics
            |> Array.to_list, loose)
          else
            match parse_list_marker (array_at lines index).text with
            | Some marker when list_item_marker marker
            && Option.is_none (parse_thematic_break (array_at lines index).text) -> (
                let head_text = item_head_text (array_at lines index).text marker in
                let (task, body_text) =
                  match parse_task_list_marker flavor head_text with
                  | Some (checked, content) -> (Some checked, content)
                  | None -> (None, head_text)
                in
                let head_start = (array_at lines index).start + marker.marker_after in
                let head_start =
                  if Option.is_some task then
                    head_start + 4
                  else
                    head_start
                in
                let continuation_min = marker.marker_after_columns in
                let sibling_marker next_marker =
                  list_item_marker next_marker && next_marker.indent < continuation_min
                in
                let (next_index, body_lines, body_has_blank) =
                  collect_item_body
                    continuation_min
                    sibling_marker
                    (index + 1)
                    [ { text = body_text; start = head_start } ]
                    false
                    ((not (String.equal body_text "")) && opens_paragraph body_text)
                    (not (String.equal body_text ""))
                in
                let trailing_blank =
                  body_has_blank && match List.reverse body_lines with
                  | [] -> false
                  | line :: _ -> String.equal line.text ""
                in
                let blank_before_direct_child_list =
                  let leading_columns text =
                    let offset = count_indent text 256 in
                    columns_of_prefix text offset
                  in
                  let rec loop = function
                    | [] -> false
                    | { text; _ } :: tail when String.equal text "" ->
                        let rec next_nonblank = function
                          | [] -> false
                          | { text; _ } :: rest when String.equal text "" -> next_nonblank rest
                          | { text; _ } :: _ -> leading_columns text < 2
                        in
                        if next_nonblank tail then
                          true
                        else
                          loop tail
                    | _ :: tail -> loop tail
                  in
                  loop body_lines
                in
                let body_lines = Array.from_list body_lines in
                let (body_blocks, body_diagnostics) = parse_blocks ~flavor body_lines 0 in
                let is_list_block block =
                  match Ceibo.Green.kind block with
                  | Syntax_kind.Ordered_list_tight
                  | Syntax_kind.Ordered_list_loose
                  | Syntax_kind.Unordered_list_tight
                  | Syntax_kind.Unordered_list_loose -> true
                  | _ -> false
                in
                let is_code_block block =
                  match Ceibo.Green.kind block with
                  | Syntax_kind.Indented_code_block
                  | Syntax_kind.Fenced_code_block -> true
                  | _ -> false
                in
                let item_loose =
                  trailing_blank || (
                    body_has_blank && match body_blocks with
                    | [ only ] when is_list_block only || is_code_block only -> false
                    | [ first_block; second_block ] when Ceibo.Green.kind first_block
                    = Syntax_kind.Paragraph
                    && is_list_block second_block -> blank_before_direct_child_list
                    | _ -> true
                  )
                in
                let item_kind =
                  match task with
                  | Some true -> Syntax_kind.Task_list_item_checked
                  | Some false -> Syntax_kind.Task_list_item_unchecked
                  | None -> Syntax_kind.List_item
                in
                let item = node item_kind body_blocks in
                body_diagnostics
                |> List.for_each ~fn:(fun diagnostic -> Vector.push diagnostics ~value:diagnostic);
                collect_items next_index (item :: acc) (loose || item_loose)
              )
            | _ -> (List.reverse acc, index, Vector.to_array diagnostics
            |> Array.to_list, loose)
        in
        let (items, next, diagnostics, loose) = collect_items start [] false in
        if items = [] then
          None
        else
          let kind =
            match (first.ordered, loose) with
            | (true, true) -> Syntax_kind.Ordered_list_loose
            | (true, false) -> Syntax_kind.Ordered_list_tight
            | (false, true) -> Syntax_kind.Unordered_list_loose
            | (false, false) -> Syntax_kind.Unordered_list_tight
          in
          let children =
            if first.ordered then
              token Syntax_kind.Text (Int.to_string first.start_number) :: items
            else
              items
          in
          Some (node kind children, next, diagnostics)

and parse_raw_html_line = fun lines start ->
  if start >= Array.length lines then
    None
  else
    let collect_until_terminator terminator =
      let rec loop index acc =
        if index >= Array.length lines then
          (List.reverse acc, index)
        else
          let current = (array_at lines index).text in
          let acc = current :: acc in
          if Option.is_some (find_substring current 0 terminator) then
            (List.reverse acc, index + 1)
          else
            loop (index + 1) acc
      in
      loop start []
    in
    let collect_until_terminator_ci terminator =
      let rec loop index acc =
        if index >= Array.length lines then
          (List.reverse acc, index)
        else
          let current = (array_at lines index).text in
          let acc = current :: acc in
          if contains_ascii_ci ~needle:terminator current then
            (List.reverse acc, index + 1)
          else
            loop (index + 1) acc
      in
      loop start []
    in
    let collect_until_blank () =
      let rec loop index acc =
        if index >= Array.length lines then
          (List.reverse acc, index)
        else if index > start && line_is_blank (array_at lines index).text then
          (List.reverse acc, index)
        else
          loop (index + 1) ((array_at lines index).text :: acc)
      in
      loop start []
    in
    let line = (array_at lines start).text in
    match classify_html_block_start line with
    | None -> None
    | Some kind ->
        let (html_lines, next) =
          match kind with
          | Html_block_1 terminator -> collect_until_terminator_ci terminator
          | Html_block_2 -> collect_until_terminator "-->"
          | Html_block_3 -> collect_until_terminator "?>"
          | Html_block_4 -> collect_until_terminator ">"
          | Html_block_5 -> collect_until_terminator "]]>"
          | Html_block_6
          | Html_block_7 -> collect_until_blank ()
        in
        Some (
          node
            Syntax_kind.Raw_html_block
            [ token Syntax_kind.Raw_html (String.concat "\n" html_lines ^ "\n") ],
          next,
          []
        )

and parse_paragraph = fun flavor lines start ->
  if start >= Array.length lines then
    None
  else
    match parse_heading (array_at lines start).text with
    | Some (level, content) ->
        let children =
          if String.equal content "" then
            [ token Syntax_kind.Text " " ]
          else
            [ token Syntax_kind.Text content ]
        in
        Some (node (heading_kind level) children, start + 1, [])
    | None ->
        match parse_reference_definition_paragraph lines start with
        | Some result -> Some result
        | None ->
            let len = Array.length lines in
            let is_block_start index text =
              let html_block_interrupts text =
                match classify_html_block_start text with
                | Some Html_block_7
                | None -> false
                | Some _ -> true
              in
              is_some (parse_heading text)
              || is_some (parse_fence_open text)
              || list_interrupts_paragraph text
              || is_some (parse_block_quote_prefix text)
              || is_some (parse_thematic_break text)
              || html_block_interrupts text
              || is_some (parse_table flavor lines index)
            in
            let rec collect index acc =
              if index >= len then
                (index, None, List.reverse acc)
              else
                let line = (array_at lines index).text in
                match parse_setext_underline line with
                | Some level -> (index + 1, Some (level, array_at lines index), List.reverse acc)
                | None ->
                    if line_is_blank line then
                      (index, None, List.reverse acc)
                    else if index > start && is_block_start index line then
                      (index, None, List.reverse acc)
                    else
                      collect (index + 1) (normalize_paragraph_line line :: acc)
            in
            let (next, setext, texts) =
              collect (start + 1) [ normalize_paragraph_line (array_at lines start).text ]
            in
            let text = String.concat "\n" texts in
            let setext_text =
              texts
              |> List.map
                ~fn:(fun line ->
                  let indent = count_indent line 3 in
                  remove_prefix line indent
                  |> trim_right)
              |> String.concat "\n"
            in
            (
              match setext with
              | Some (level, _) ->
                  let children =
                    if String.equal setext_text "" then
                      [ token Syntax_kind.Text " " ]
                    else
                      [ token Syntax_kind.Text setext_text ]
                  in
                  Some (node (heading_kind level) children, next, [])
              | None -> Some (node Syntax_kind.Paragraph [ token Syntax_kind.Text text ], next, [])
            )

and parse_blocks = fun ~flavor lines start ->
  if start >= Array.length lines then
    ([], [])
  else if line_is_blank (array_at lines start).text then
    parse_blocks ~flavor lines (start + 1)
  else
    let block =
      match parse_fenced_code_block lines start with
      | Some result -> Some result
      | None -> (
          match parse_indented_code_block lines start with
          | Some result -> Some result
          | None -> (
              match parse_block_quote flavor lines start with
              | Some result -> Some result
              | None -> (
                  match parse_table flavor lines start with
                  | Some result -> Some result
                  | None -> (
                      if is_some (parse_thematic_break (array_at lines start).text) then
                        Some (node Syntax_kind.Horizontal_rule [], start + 1, [])
                      else
                        (
                          match parse_list flavor lines start with
                          | Some result -> Some result
                          | None -> (
                              match parse_raw_html_line lines start with
                              | Some result -> Some result
                              | None -> parse_paragraph flavor lines start
                            )
                        )
                    )
                )
            )
        )
    in
    match block with
    | Some (node_, next, diagnostics) ->
        let (parsed_next, nested) = parse_blocks ~flavor lines next in
        (node_ :: parsed_next, diagnostics @ nested)
    | None -> parse_blocks ~flavor lines (start + 1)

let lines_of_tokens = fun tokens ->
  tokens
  |> List.filter_map
    ~fn:(fun token ->
      match token.Markdown_token.kind with
      | Markdown_token.Line_text -> Some { text = token.text; start = token.span.start }
      | _ -> None)
  |> Array.from_list

let parse = fun ?(flavor = Markdown) source ->
  let source = Markdown_lexer.normalize_newlines source in
  let tokens = Markdown_lexer.tokenize source in
  let lines = lines_of_tokens tokens in
  let control_diagnostics = make_control_diagnostics source in
  try
    let (blocks, diagnostics) = parse_blocks ~flavor lines 0 in
    let diagnostics = List.append control_diagnostics diagnostics in
    let tree = Ceibo.Green.make_node ~kind:Syntax_kind.Document ~children:blocks in
    {
      source;
      tokens;
      tree;
      diagnostics;
    }
  with
  | exn ->
      let message = Exception.to_string exn in
      let found = { kind = "exception"; text = message } in
      let span = make_span ~start:0 ~len:(String.length source) in
      let diagnostic = parser_internal ~found ~message ~span in
      {
        source;
        tokens;
        tree = Ceibo.Green.make_node
          ~kind:Syntax_kind.Document
          ~children:[ node Syntax_kind.Error [ token Syntax_kind.Text message ] ];
        diagnostics = List.append control_diagnostics [ diagnostic ];
      }
