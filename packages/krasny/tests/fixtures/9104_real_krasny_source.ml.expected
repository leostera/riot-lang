open Std
open Std.Collections

let trim_trailing_newlines text =
  let rec loop index =
    if index <= 0 then
      ""
    else
      match text.[index - 1] with
      | '\n' | '\r' ->
          loop (index - 1)
      | _ ->
          String.sub text 0 index
  in
  loop (String.length text)

let trim_trailing_layout_whitespace text =
  let rec loop index =
    if index <= 0 then
      ""
    else
      match text.[index - 1] with
      | ' ' | '\t' | '\n' | '\r' ->
          loop (index - 1)
      | _ ->
          String.sub text 0 index
  in
  loop (String.length text)

let split_trailing_layout_whitespace text =
  let trimmed = trim_trailing_layout_whitespace text in
  let trimmed_length = String.length trimmed in
  let trailing_length = String.length text - trimmed_length in
  let trailing =
    if trailing_length <= 0 then
      ""
    else
      String.sub text trimmed_length trailing_length
  in
  (trimmed, trailing)

let line_start_before text index =
  let rec loop cursor =
    if cursor <= 0 then
      0
    else if text.[cursor - 1] = '\n' then
      cursor
    else
      loop (cursor - 1)
  in
  loop index

let find_trailing_comment_start text end_ =
  if end_ < 2 || text.[end_ - 2] != '*' || text.[end_ - 1] != ')' then
    None
  else
    let rec loop index depth =
      if index <= 0 then
        None
      else if text.[index - 1] = '(' && text.[index] = '*' then
        if depth = 1 then
          Some (index - 1)
        else
          loop (index - 2) (depth - 1)
      else if text.[index - 1] = '*' && text.[index] = ')' then
        loop (index - 2) (depth + 1)
      else
        loop (index - 1) depth
    in
    loop (end_ - 2) 1

let split_trailing_comment_block text =
  let body, trailing_layout = split_trailing_layout_whitespace text in
  let rec peel suffix_start =
    let prefix = String.sub body 0 suffix_start |> trim_trailing_layout_whitespace in
    let prefix_end = String.length prefix in
    match find_trailing_comment_start prefix prefix_end with
    | None ->
        suffix_start
    | Some comment_start ->
        let line_start = line_start_before prefix comment_start in
        let prefix_on_line =
          String.sub prefix line_start (comment_start - line_start)
        in
        if String.trim prefix_on_line = "" then
          peel line_start
        else
          suffix_start
  in
  let suffix_start = peel (String.length body) in
  if suffix_start = 0 || suffix_start = String.length body then
    (body, trailing_layout)
  else
    ( String.sub body 0 suffix_start,
      String.sub body suffix_start (String.length body - suffix_start)
      ^ trailing_layout )

let source_of_syntax_node (node : Syn.Cst.syntax_node) =
  let buffer = IO.Buffer.create 1024 in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token ->
        IO.Buffer.add_string buffer (Syn.Ceibo.Red.SyntaxToken.text token)
    | Syn.Ceibo.Red.Node _ ->
        ());
  IO.Buffer.contents buffer |> trim_trailing_newlines

let source_of_token token = Syn.Cst.Token.text token
let source_of_ident ident = Syn.Cst.Ident.segments ident |> List.map source_of_token |> String.concat "."
let source_of_result (result : Syn.Parser.parse_result) = result.source
let source_of_pattern pattern = source_of_syntax_node (Syn.Cst.Pattern.syntax_node pattern) |> String.trim
let source_of_parameter parameter = source_of_syntax_node (Syn.Cst.Parameter.syntax_node parameter) |> String.trim

let identifier_character = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '\'' ->
      true
  | _ ->
      false

let source_mentions_identifier source identifier =
  let source_length = String.length source in
  let identifier_length = String.length identifier in
  let rec loop index =
    if index + identifier_length > source_length then
      false
    else if String.sub source index identifier_length = identifier then
      let before_ok =
        index = 0 || not (identifier_character source.[index - 1])
      in
      let after_index = index + identifier_length in
      let after_ok =
        after_index = source_length
        || not (identifier_character source.[after_index])
      in
      if before_ok && after_ok then
        true
      else loop (index + 1)
    else loop (index + 1)
  in
  loop 0

let contains_substring source needle =
  let source_length = String.length source in
  let needle_length = String.length needle in
  if needle_length = 0 then
    true
  else
    let rec loop index =
      if index + needle_length > source_length then
        false
      else if String.sub source index needle_length = needle then
        true
      else loop (index + 1)
    in
    loop 0

let fresh_match_parameter_name syntax_node =
  let source = source_of_syntax_node syntax_node in
  let rec pick = function
    | [] ->
        "value"
    | name :: rest ->
        if source_mentions_identifier source name then
          pick rest
        else name
  in
  pick
    [
      "x";
      "value";
      "arg";
      "input";
      "subject";
      "subject0";
      "subject1";
    ]

let syntax_node_has_comment_like_trivia (node : Syn.Cst.syntax_node) =
  let found = ref false in
  Syn.Ceibo.Red.SyntaxNode.preorder node (function
    | Syn.Ceibo.Red.Token token -> (
        match Syn.Ceibo.Red.SyntaxToken.kind token with
        | Syn.SyntaxKind.COMMENT | Syn.SyntaxKind.DOCSTRING ->
            found := true
        | _ ->
            ())
    | Syn.Ceibo.Red.Node _ ->
        ());
  !found

let source_of_span source (span : Syn.Ceibo.Span.t) =
  let source_length = String.length source in
  let start =
    if span.start < 0 then
      0
    else if span.start > source_length then
      source_length
    else
      span.start
  in
  let end_ =
    if span.end_ < start then
      start
    else if span.end_ > source_length then
      source_length
    else
      span.end_
  in
  if end_ <= start then
    ""
  else
    String.sub source start (end_ - start)

let source_between source ~start ~end_ =
  source_of_span source (Syn.Ceibo.Span.make ~start ~end_)

let is_whitespace_only text = String.trim text = ""
let contains_comment_like_text text = contains_substring text "(*"
let source_of_node_from_source source node = source_of_span source (Syn.Ceibo.Red.SyntaxNode.span node)
