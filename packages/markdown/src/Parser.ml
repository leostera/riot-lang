open Std

(** Parser state *)
type parser = {
  cursor : Token_cursor.t;
}

(** Create a new parser from tokens *)
let create ~source tokens =
  { cursor = Token_cursor.create ~source tokens }

(** Get current position in token stream *)
let position parser = Token_cursor.position parser.cursor

(** Check if at end of tokens *)
let is_eof parser = Token_cursor.is_eof parser.cursor

(** Peek at current token without advancing *)
let peek parser = Token_cursor.peek parser.cursor

(** Peek at current token kind *)
let peek_kind parser = (peek parser).Token.kind

(** Peek n tokens ahead *)
let peek_n parser n = Token_cursor.peek_n parser.cursor n

(** Check if current token matches a specific kind *)
let at parser kind = peek_kind parser = kind

(** Advance to next token *)
let advance parser = Token_cursor.advance parser.cursor

(** Get text of a token from source *)
let token_text parser token = Token_cursor.view parser.cursor token.Token.span

(** Consume a single token *)
let consume parser =
  let token = peek parser in
  advance parser;
  token

(** Make a green tree node from children *)
let make_node kind children =
  let children_array = Array.of_list children in
  Ceibo.Green.make_node ~kind ~children:children_array

(** Make a token green element *)
let make_token parser token =
  let token_kind = token.Token.kind in
  let text = Token_cursor.view parser.cursor token.Token.span in
  let width = String.length text in
  let green_token = Ceibo.Green.make_token ~kind:token_kind ~text ~width in
  Ceibo.Green.Token green_token

(** Convert list of tokens to green elements *)
let tokens_to_green parser tokens = List.map (make_token parser) tokens

(** Parse a simple text node from TEXT_TOKEN *)
let rec parse_text parser =
  match peek_kind parser with
  | Syntax_kind.TEXT_TOKEN ->
      let tok = consume parser in
      make_node Syntax_kind.TEXT [ make_token parser tok ]
  | _ ->
      (* Return empty text for now *)
      make_node Syntax_kind.TEXT []

(** Parse code span (inline code) *)
and parse_code_span parser =
  (* Count opening backticks *)
  let rec count_opening n =
    match peek_n parser n with
    | { kind = Syntax_kind.BACKTICK; _ } -> count_opening (n + 1)
    | _ -> n
  in
  let open_count = count_opening 0 in
  
  (* Consume opening backticks *)
  for _ = 1 to open_count do
    advance parser
  done;
  
  (* Collect content until we find matching closing backticks *)
  let rec find_closing acc backtick_run =
    match peek_kind parser with
    | Syntax_kind.EOF -> 
        (* No closing backticks found - not a code span *)
        None
    | Syntax_kind.BACKTICK ->
        let tok = consume parser in
        find_closing acc (backtick_run + 1)
    | _ ->
        if backtick_run = open_count then
          (* Found matching closing backticks *)
          Some (List.rev acc)
        else if backtick_run > 0 then
          (* Wrong number of backticks - include them in content *)
          let tok = consume parser in
          let backticks = List.make ~len:backtick_run ~fn:(fun _ -> 
            Token.make Syntax_kind.BACKTICK (Ceibo.Span.make ~start:0 ~end_:1)
          ) in
          find_closing (make_token parser tok :: (List.map (make_token parser) backticks @ acc)) 0
        else
          let tok = consume parser in
          find_closing (make_token parser tok :: acc) 0
  in
  
  match find_closing [] 0 with
  | Some content ->
      (* Strip single leading/trailing space if both present *)
      let content = 
        match content with
        | [Ceibo.Green.Token tok] ->
            let text = tok.text in
            let len = String.length text in
            if len >= 2 && text.[0] = ' ' && text.[len - 1] = ' ' then
              let stripped = String.sub text 1 (len - 2) in
              [Ceibo.Green.Token { tok with text = stripped }]
            else
              content
        | _ -> content
      in
      Some (make_node Syntax_kind.INLINE_CODE content)
  | None ->
      (* Failed to parse code span *)
      None

(** Parse emphasis or strong emphasis *)
and parse_emphasis parser delim_kind =
  (* Count opening delimiters *)
  let rec count_delims n =
    match peek_n parser n with
    | { kind; _ } when kind = delim_kind -> count_delims (n + 1)
    | _ -> n
  in
  let open_count = count_delims 0 in
  
  (* Handle single (emphasis) or double (strong) delimiters *)
  if open_count >= 1 && open_count <= 2 then begin
    (* Check that opening delimiter is not followed by whitespace *)
    match peek_n parser open_count with
    | { kind = Syntax_kind.SPACE | Syntax_kind.TAB | Syntax_kind.NEWLINE | Syntax_kind.EOF; _ } ->
        None (* can't open emphasis with whitespace after *)
    | _ ->
        (* Consume opening delimiters *)
        for _ = 1 to open_count do
          advance parser
        done;
        
        let node_kind = if open_count = 2 then Syntax_kind.STRONG else Syntax_kind.EMPHASIS in
        
        (* Collect content until closing delimiters *)
        let rec find_closing acc last_was_space =
          match peek_kind parser with
          | Syntax_kind.EOF | Syntax_kind.NEWLINE -> None (* no closing found *)
          | k when k = delim_kind ->
              (* Check if we have enough closing delimiters *)
              let close_count = count_delims 0 in
              if close_count >= open_count && not last_was_space then begin
                (* Found valid closing *)
                for _ = 1 to open_count do
                  advance parser
                done;
                Some (List.rev acc)
              end else begin
                (* Not enough delimiters or preceded by space, include and continue *)
                let tok = consume parser in
                find_closing (make_token parser tok :: acc) false
              end
          | Syntax_kind.SPACE | Syntax_kind.TAB ->
              let tok = consume parser in
              find_closing (make_token parser tok :: acc) true
          | Syntax_kind.BACKTICK -> (
              match parse_code_span parser with
              | Some code_span -> find_closing (Ceibo.Green.Node code_span :: acc) false
              | None ->
                  let tok = consume parser in
                  find_closing (make_token parser tok :: acc) false)
          | Syntax_kind.TEXT_TOKEN ->
              let text = parse_text parser in
              find_closing (Ceibo.Green.Node text :: acc) false
          | _ ->
              let tok = consume parser in
              find_closing (make_token parser tok :: acc) false
        in
        
        match find_closing [] false with
        | Some content -> Some (make_node node_kind content)
        | None -> None (* failed to parse emphasis *)
  end else
    None

(** Parse autolink <url> or <email> *)
and parse_autolink parser =
  (* Peek ahead to check if this looks like an autolink *)
  let rec check_autolink offset =
    match peek_n parser offset with
    | { kind = Syntax_kind.GREATER_THAN; _ } when offset > 1 ->
        (* Found closing >, looks valid *)
        true
    | { kind = Syntax_kind.NEWLINE | Syntax_kind.EOF | Syntax_kind.SPACE; _ } ->
        (* Invalid autolink *)
        false
    | { kind = Syntax_kind.LESS_THAN; _ } when offset > 1 ->
        (* Nested <, invalid *)
        false
    | _ when offset > 100 ->
        (* Too long, give up *)
        false
    | _ ->
        check_autolink (offset + 1)
  in
  
  if not (check_autolink 1) then None
  else begin
    advance parser; (* consume < *)
    
    (* Collect until > *)
    let rec collect_url acc =
      match peek_kind parser with
      | Syntax_kind.GREATER_THAN ->
          advance parser; (* consume > *)
          Some (List.rev acc)
      | _ ->
          let tok = consume parser in
          collect_url (make_token parser tok :: acc)
    in
    
    match collect_url [] with
    | Some url_tokens ->
        (* Create a LINK node with the URL as both href and text *)
        Some (make_node Syntax_kind.LINK url_tokens)
    | None ->
        None
  end

(** Parse inline content (simplified for now) *)
and parse_inline parser =
  let rec collect_inline acc =
    match peek_kind parser with
    | Syntax_kind.EOF | Syntax_kind.NEWLINE -> List.rev acc
    | Syntax_kind.BACKTICK -> (
        (* Try to parse code span *)
        match parse_code_span parser with
        | Some code_span ->
            collect_inline (Ceibo.Green.Node code_span :: acc)
        | None ->
            (* Failed - consume backtick as regular token *)
            let tok = consume parser in
            collect_inline (make_token parser tok :: acc))
    | Syntax_kind.STAR | Syntax_kind.UNDERSCORE -> (
        (* Try to parse emphasis *)
        match parse_emphasis parser (peek_kind parser) with
        | Some emphasis ->
            collect_inline (Ceibo.Green.Node emphasis :: acc)
        | None ->
            (* Failed - consume delimiter as regular token *)
            let tok = consume parser in
            collect_inline (make_token parser tok :: acc))
    | Syntax_kind.LESS_THAN -> (
        (* Try to parse autolink *)
        match parse_autolink parser with
        | Some link ->
            collect_inline (Ceibo.Green.Node link :: acc)
        | None ->
            (* Failed - consume < as regular token *)
            let tok = consume parser in
            collect_inline (make_token parser tok :: acc))
    | Syntax_kind.TEXT_TOKEN ->
        let text = parse_text parser in
        collect_inline (Ceibo.Green.Node text :: acc)
    | _ ->
        (* For now, consume other tokens as text *)
        let tok = consume parser in
        collect_inline (make_token parser tok :: acc)
  in
  collect_inline []

(** Check if next line is a Setext heading underline (=== or ---) *)
and check_setext_underline parser =
  let rec check_underline char_kind offset count =
    match peek_n parser offset with
    | { kind; _ } when kind = char_kind -> check_underline char_kind (offset + 1) (count + 1)
    | { kind = Syntax_kind.SPACE | Syntax_kind.TAB; _ } -> check_underline char_kind (offset + 1) count
    | { kind = Syntax_kind.NEWLINE | Syntax_kind.EOF; _ } when count > 0 -> Some char_kind
    | _ -> None
  in
  match peek_kind parser with
  | Syntax_kind.EQUAL -> check_underline Syntax_kind.EQUAL 0 0
  | Syntax_kind.DASH -> check_underline Syntax_kind.DASH 0 0
  | _ -> None

(** Parse a paragraph *)
and parse_paragraph parser =
  let start_tok = peek parser in
  
  (* Collect first line of content *)
  let first_line = parse_inline parser in
  
  (* Check if we're at end or if there's a second line *)
  match peek_kind parser with
  | Syntax_kind.EOF ->
      make_node Syntax_kind.PARAGRAPH first_line
  | Syntax_kind.NEWLINE ->
      advance parser; (* consume newline *)
      (* Check if next line is a Setext underline *)
      (match check_setext_underline parser with
      | Some Syntax_kind.EQUAL ->
          (* H1 - consume underline *)
          while peek_kind parser <> Syntax_kind.NEWLINE && not (is_eof parser) do
            advance parser
          done;
          if peek_kind parser = Syntax_kind.NEWLINE then advance parser;
          make_node Syntax_kind.HEADING1 first_line
      | Some Syntax_kind.DASH ->
          (* H2 - consume underline *)
          while peek_kind parser <> Syntax_kind.NEWLINE && not (is_eof parser) do
            advance parser
          done;
          if peek_kind parser = Syntax_kind.NEWLINE then advance parser;
          make_node Syntax_kind.HEADING2 first_line
      | None ->
          (* Not a Setext heading, continue as paragraph *)
          (* Check if next is blank line *)
          (match peek_kind parser with
          | Syntax_kind.NEWLINE | Syntax_kind.EOF ->
              (* End of paragraph *)
              make_node Syntax_kind.PARAGRAPH first_line
          | _ ->
              (* Multi-line paragraph - collect rest *)
              let rec collect_rest acc =
                match peek_kind parser with
                | Syntax_kind.EOF -> acc
                | Syntax_kind.NEWLINE ->
                    let tok = consume parser in
                    (match peek_kind parser with
                    | Syntax_kind.NEWLINE | Syntax_kind.EOF ->
                        (* End of paragraph *)
                        acc
                    | _ ->
                        (* Continue paragraph *)
                        let inline = parse_inline parser in
                        collect_rest (acc @ [make_token parser tok] @ inline))
                | _ ->
                    let inline = parse_inline parser in
                    collect_rest (acc @ inline)
              in
              let rest = collect_rest [] in
              make_node Syntax_kind.PARAGRAPH (first_line @ rest)))
  | _ ->
      (* Shouldn't happen, but handle it *)
      make_node Syntax_kind.PARAGRAPH first_line

(** Check if line starts with 4+ spaces (indented code block) *)
and check_indented_code parser =
  let rec count_spaces n =
    match peek_n parser n with
    | { kind = Syntax_kind.SPACE; _ } -> count_spaces (n + 1)
    | { kind = Syntax_kind.TAB; _ } -> count_spaces (n + 4) (* tab = 4 spaces *)
    | { kind = Syntax_kind.NEWLINE | Syntax_kind.EOF; _ } -> false (* blank line *)
    | _ -> n >= 4
  in
  count_spaces 0

(** Parse indented code block *)
and parse_indented_code parser =
  (* Helper to check if next line is blank (0-3 spaces then newline) or indented (4+ spaces) *)
  let check_continue_code () =
    let rec check_line offset spaces =
      match peek_n parser offset with
      | { kind = Syntax_kind.SPACE; _ } -> check_line (offset + 1) (spaces + 1)
      | { kind = Syntax_kind.TAB; _ } -> true (* tab means 4+ spaces *)
      | { kind = Syntax_kind.NEWLINE; _ } -> true (* blank line, continue *)
      | { kind = Syntax_kind.EOF; _ } -> false
      | _ -> spaces >= 4 (* has content, continue only if 4+ spaces *)
    in
    check_line 0 0
  in
  
  let rec collect_lines acc =
    match peek_kind parser with
    | Syntax_kind.EOF -> acc
    | Syntax_kind.NEWLINE ->
        (* Check if next line continues the code block *)
        let tok = consume parser in
        if check_continue_code () then
          (* Continue - include the newline *)
          collect_lines (acc @ [make_token parser tok])
        else
          (* Stop - don't include the newline *)
          acc
    | Syntax_kind.SPACE | Syntax_kind.TAB ->
        (* Strip up to 4 spaces of indentation *)
        let rec strip_indent n =
          if n >= 4 then ()
          else
            match peek_kind parser with
            | Syntax_kind.SPACE -> advance parser; strip_indent (n + 1)
            | Syntax_kind.TAB -> advance parser (* tab counts as 4 spaces *)
            | _ -> ()
        in
        strip_indent 0;
        (* Collect rest of line *)
        let rec collect_to_newline line_acc =
          match peek_kind parser with
          | Syntax_kind.NEWLINE | Syntax_kind.EOF -> line_acc
          | _ ->
              let tok = consume parser in
              collect_to_newline (line_acc @ [make_token parser tok])
        in
        let line_tokens = collect_to_newline [] in
        collect_lines (acc @ line_tokens)
    | _ ->
        (* Shouldn't happen *)
        acc
  in
  
  let content = collect_lines [] in
  make_node Syntax_kind.CODE_BLOCK content

(** Check if we're at a fenced code block (``` or ~~~) with 0-3 leading spaces *)
and check_fenced_code parser =
  (* Skip 0-3 leading spaces *)
  let rec skip_leading_spaces offset count =
    if count >= 3 then offset
    else
      match peek_n parser offset with
      | { kind = Syntax_kind.SPACE; _ } -> skip_leading_spaces (offset + 1) (count + 1)
      | { kind = Syntax_kind.TAB; _ } -> offset (* tab would make it indented code *)
      | _ -> offset
  in
  let start_offset = skip_leading_spaces 0 0 in
  
  let rec count_fence_chars char_kind offset =
    match peek_n parser offset with
    | { kind; _ } when kind = char_kind -> count_fence_chars char_kind (offset + 1)
    | _ -> offset
  in
  match peek_n parser start_offset with
  | { kind = Syntax_kind.BACKTICK; _ } ->
      let count = count_fence_chars Syntax_kind.BACKTICK start_offset in
      if count - start_offset >= 3 then Some (Syntax_kind.BACKTICK, count - start_offset) else None
  | { kind = Syntax_kind.TILDE; _ } ->
      let count = count_fence_chars Syntax_kind.TILDE start_offset in
      if count - start_offset >= 3 then Some (Syntax_kind.TILDE, count - start_offset) else None
  | _ -> None

(** Parse fenced code block *)
and parse_fenced_code parser fence_char fence_count =
  (* Consume leading spaces (0-3) *)
  let rec consume_leading_spaces count =
    if count >= 3 then ()
    else
      match peek_kind parser with
      | Syntax_kind.SPACE -> advance parser; consume_leading_spaces (count + 1)
      | _ -> ()
  in
  consume_leading_spaces 0;
  
  (* Consume opening fence *)
  for _ = 1 to fence_count do
    advance parser
  done;
  
  (* Skip info string (everything until newline) *)
  let rec skip_info_string () =
    match peek_kind parser with
    | Syntax_kind.NEWLINE -> advance parser
    | Syntax_kind.EOF -> ()
    | _ -> advance parser; skip_info_string ()
  in
  skip_info_string ();
  
  (* Check if current position is at closing fence *)
  let is_closing_fence offset =
    let rec check_fence off count =
      match peek_n parser off with
      | { kind; _ } when kind = fence_char -> check_fence (off + 1) (count + 1)
      | { kind = Syntax_kind.SPACE | Syntax_kind.TAB; _ } -> check_fence (off + 1) count
      | { kind = Syntax_kind.NEWLINE | Syntax_kind.EOF; _ } -> count >= fence_count
      | _ -> false
    in
    check_fence offset 0
  in
  
  (* Collect content until closing fence *)
  let rec collect_content acc =
    match peek_kind parser with
    | Syntax_kind.EOF -> acc
    | Syntax_kind.NEWLINE ->
        (* Peek ahead to see if next line is closing fence *)
        if is_closing_fence 1 then begin
          (* Skip the newline and consume the closing fence *)
          advance parser;
          let rec consume_closing () =
            match peek_kind parser with
            | k when k = fence_char || k = Syntax_kind.SPACE || k = Syntax_kind.TAB ->
                advance parser; consume_closing ()
            | Syntax_kind.NEWLINE -> advance parser
            | Syntax_kind.EOF -> ()
            | _ -> ()
          in
          consume_closing ();
          acc
        end else begin
          (* Not closing fence, include the newline *)
          let tok = consume parser in
          collect_content (acc @ [make_token parser tok])
        end
    | _ when is_closing_fence 0 ->
        (* At closing fence without newline before it *)
        let rec consume_closing () =
          match peek_kind parser with
          | k when k = fence_char || k = Syntax_kind.SPACE || k = Syntax_kind.TAB ->
              advance parser; consume_closing ()
          | Syntax_kind.NEWLINE -> advance parser
          | Syntax_kind.EOF -> ()
          | _ -> ()
        in
        consume_closing ();
        acc
    | _ ->
        let tok = consume parser in
        collect_content (acc @ [make_token parser tok])
  in
  
  let content = collect_content [] in
  make_node Syntax_kind.FENCED_CODE_BLOCK content

(** Check if we're at a thematic break (including with 0-3 leading spaces) *)
and check_thematic_break parser =
  (* First skip 0-3 leading spaces *)
  let rec skip_leading_spaces offset count =
    if count >= 3 then offset
    else
      match peek_n parser offset with
      | { kind = Syntax_kind.SPACE; _ } -> skip_leading_spaces (offset + 1) (count + 1)
      | { kind = Syntax_kind.TAB; _ } -> offset (* tab would make it indented code *)
      | _ -> offset
  in
  let start_offset = skip_leading_spaces 0 0 in
  
  let rec count_chars char_kind offset count spaces_ok =
    let tok = peek_n parser offset in
    match tok.kind with
    | k when k = char_kind -> count_chars char_kind (offset + 1) (count + 1) spaces_ok
    | Syntax_kind.SPACE | Syntax_kind.TAB -> count_chars char_kind (offset + 1) count (spaces_ok + 1)
    | Syntax_kind.NEWLINE | Syntax_kind.EOF -> 
        if count >= 3 then Some offset else None
    | _ -> None
  in
  match peek_n parser start_offset with
  | { kind = Syntax_kind.STAR; _ } -> count_chars Syntax_kind.STAR start_offset 0 0
  | { kind = Syntax_kind.DASH; _ } -> count_chars Syntax_kind.DASH start_offset 0 0
  | { kind = Syntax_kind.UNDERSCORE; _ } -> count_chars Syntax_kind.UNDERSCORE start_offset 0 0
  | _ -> None

(** Check if we're at an ATX heading and return the level (with optional 0-3 leading spaces) *)
and check_atx_heading parser =
  (* Skip 0-3 leading spaces *)
  let rec skip_leading_spaces offset count =
    if count >= 3 then offset
    else
      match peek_n parser offset with
      | { kind = Syntax_kind.SPACE; _ } -> skip_leading_spaces (offset + 1) (count + 1)
      | { kind = Syntax_kind.TAB; _ } -> offset (* tab would make it indented code *)
      | _ -> offset
  in
  let start_offset = skip_leading_spaces 0 0 in
  
  let rec count_hashes offset n =
    if n > 6 then None
    else
      match peek_n parser offset with
      | { kind = Syntax_kind.HASH; _ } -> count_hashes (offset + 1) (n + 1)
      | { kind = Syntax_kind.SPACE; _ } | { kind = Syntax_kind.TAB; _ } 
      | { kind = Syntax_kind.NEWLINE; _ } | { kind = Syntax_kind.EOF; _ } ->
          if n > 0 && n <= 6 then Some n else None
      | _ -> None
  in
  match peek_n parser start_offset with
  | { kind = Syntax_kind.HASH; _ } -> count_hashes start_offset 0
  | _ -> None

(** Strip leading and trailing spaces and optional closing # from heading content *)
and strip_heading_whitespace elements =
  (* Strip leading space/tab tokens *)
  let rec strip_leading = function
    | Ceibo.Green.Token { kind = Syntax_kind.SPACE | Syntax_kind.TAB; _ } :: rest ->
        strip_leading rest
    | elements -> elements
  in
  
  (* Strip trailing space/tab tokens *)
  let rec strip_trailing = function
    | [] -> []
    | elements ->
        match List.rev elements with
        | Ceibo.Green.Token { kind = Syntax_kind.SPACE | Syntax_kind.TAB; _ } :: rest ->
            strip_trailing (List.rev rest)
        | _ -> elements
  in
  
  (* Strip optional closing # sequence (must be preceded by space) *)
  let rec strip_closing_hashes = function
    | [] -> []
    | elements ->
        (* First collect trailing hashes *)
        let rec collect_trailing_hashes acc = function
          | [] -> (acc, [])
          | Ceibo.Green.Token { kind = Syntax_kind.HASH; _ } as h :: rest ->
              collect_trailing_hashes (h :: acc) rest
          | rest -> (acc, rest)
        in
        let (hashes, rest) = collect_trailing_hashes [] (List.rev elements) in
        (* Only strip if preceded by space *)
        match rest with
        | Ceibo.Green.Token { kind = Syntax_kind.SPACE | Syntax_kind.TAB; _ } :: _ when List.length hashes > 0 ->
            (* Remove the hashes and trailing whitespace *)
            strip_trailing (List.rev rest)
        | _ ->
            (* No space before hashes, keep them *)
            elements
  in
  
  elements |> strip_leading |> strip_trailing |> strip_closing_hashes

(** Parse ATX heading *)
and parse_atx_heading parser level =
  (* Consume leading spaces (0-3) *)
  let rec consume_leading_spaces count =
    if count >= 3 then ()
    else
      match peek_kind parser with
      | Syntax_kind.SPACE -> advance parser; consume_leading_spaces (count + 1)
      | _ -> ()
  in
  consume_leading_spaces 0;
  
  (* Consume the hashes *)
  for _ = 1 to level do
    advance parser
  done;
  
  (* Skip optional space/tab after hashes *)
  (match peek_kind parser with
  | Syntax_kind.SPACE | Syntax_kind.TAB -> advance parser
  | _ -> ());
  
  (* Parse rest of line as inline content *)
  let inline = parse_inline parser in
  
  (* Strip leading/trailing spaces and closing # *)
  let inline = strip_heading_whitespace inline in
  
  (* Skip trailing newline if present *)
  (match peek_kind parser with
  | Syntax_kind.NEWLINE -> advance parser
  | _ -> ());
  
  let kind = match level with
    | 1 -> Syntax_kind.HEADING1
    | 2 -> Syntax_kind.HEADING2
    | 3 -> Syntax_kind.HEADING3
    | 4 -> Syntax_kind.HEADING4
    | 5 -> Syntax_kind.HEADING5
    | 6 -> Syntax_kind.HEADING6
    | _ -> Syntax_kind.HEADING1 (* shouldn't happen *)
  in
  make_node kind inline

(** Parse document (top level) *)
and parse_document parser =
  let rec parse_blocks acc =
    if is_eof parser then List.rev acc
    else
      match peek_kind parser with
      | Syntax_kind.EOF -> List.rev acc
      | Syntax_kind.NEWLINE ->
          (* Skip blank lines between blocks *)
          advance parser;
          parse_blocks acc
      | Syntax_kind.BACKTICK | Syntax_kind.TILDE -> (
          (* Check if this is a fenced code block *)
          match check_fenced_code parser with
          | Some (fence_char, fence_count) ->
              let code = parse_fenced_code parser fence_char fence_count in
              parse_blocks (Ceibo.Green.Node code :: acc)
          | None ->
              (* Not a fenced code block, treat as paragraph *)
              let para = parse_paragraph parser in
              parse_blocks (Ceibo.Green.Node para :: acc))
      | Syntax_kind.HASH -> (
          (* Check if this is an ATX heading *)
          match check_atx_heading parser with
          | Some level ->
              let heading = parse_atx_heading parser level in
              parse_blocks (Ceibo.Green.Node heading :: acc)
          | None ->
              (* Not a valid heading, treat as paragraph *)
              let para = parse_paragraph parser in
              parse_blocks (Ceibo.Green.Node para :: acc))
      | Syntax_kind.STAR | Syntax_kind.DASH | Syntax_kind.UNDERSCORE -> (
          (* Check if this is a thematic break *)
          match check_thematic_break parser with
          | Some token_count ->
              (* Consume all tokens in the thematic break *)
              for _ = 1 to token_count do
                advance parser
              done;
              (* Skip trailing newline *)
              (match peek_kind parser with
              | Syntax_kind.NEWLINE -> advance parser
              | _ -> ());
              let hr = make_node Syntax_kind.THEMATIC_BREAK [] in
              parse_blocks (Ceibo.Green.Node hr :: acc)
          | None ->
              (* Not a thematic break, treat as paragraph *)
              let para = parse_paragraph parser in
              parse_blocks (Ceibo.Green.Node para :: acc))
      | Syntax_kind.SPACE | Syntax_kind.TAB -> (
          (* Could be fenced code, indented code, thematic break, or ATX heading with leading spaces *)
          match check_fenced_code parser with
          | Some (fence_char, fence_count) ->
              (* Fenced code block with leading spaces *)
              let code = parse_fenced_code parser fence_char fence_count in
              parse_blocks (Ceibo.Green.Node code :: acc)
          | None ->
              match check_atx_heading parser with
              | Some level ->
                  (* ATX heading with leading spaces *)
                  let heading = parse_atx_heading parser level in
                  parse_blocks (Ceibo.Green.Node heading :: acc)
              | None ->
                  match check_thematic_break parser with
                  | Some token_count ->
                      (* Thematic break with leading spaces *)
                      for _ = 1 to token_count do
                        advance parser
                      done;
                      (match peek_kind parser with
                      | Syntax_kind.NEWLINE -> advance parser
                      | _ -> ());
                      let hr = make_node Syntax_kind.THEMATIC_BREAK [] in
                      parse_blocks (Ceibo.Green.Node hr :: acc)
                  | None when check_indented_code parser ->
                      (* Indented code block *)
                      let code = parse_indented_code parser in
                      parse_blocks (Ceibo.Green.Node code :: acc)
                  | None ->
                      (* Just a paragraph with leading space *)
                      let para = parse_paragraph parser in
                      parse_blocks (Ceibo.Green.Node para :: acc))
      | _ ->
          (* Everything else is a paragraph *)
          let para = parse_paragraph parser in
          parse_blocks (Ceibo.Green.Node para :: acc)
  in
  
  let children = parse_blocks [] in
  make_node Syntax_kind.DOCUMENT children

(** Main parse function *)
let parse ~source tokens =
  let parser = create ~source tokens in
  let tree = parse_document parser in
  tree
