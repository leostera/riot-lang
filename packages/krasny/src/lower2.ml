open Std
open Std.Collections
module Ast = Syn.Ast2
module Doc = Doc
module Kind = Syn.SyntaxKind2

type error = {
  message: string;
}

exception Unsupported of error

let error_to_string = fun err -> err.message

let unsupported = fun message -> raise (Unsupported { message })

let blank_line = Doc.concat [ Doc.line; Doc.line ]

let token_doc = fun token -> Doc.text (Ast.Token.text token)

let token_full_doc = fun token -> Doc.text (Ast.Token.full_text token)

let optional_token_doc = fun token ->
  match token with
  | Some token -> token_doc token
  | None -> Doc.empty

let direct_child_tokens = fun node ->
  let tokens = ref [] in
  Ast.Node.for_each_child_token node ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let tokens_doc = fun tokens -> Doc.concat (List.map tokens ~fn:token_doc)

let tokens_text = fun tokens ->
  String.concat "" (List.map tokens ~fn:Ast.Token.text)

let first_descendant_token = Ast.Node.first_descendant_token

let split_last = fun items ->
  let rec loop prefix = function
    | [] -> None
    | [ last ] -> Some (List.reverse prefix, last)
    | item :: rest -> loop (item :: prefix) rest
  in
  loop [] items

let index_shell_docs = fun expr ~fallback_open ~fallback_close ->
  match split_last (direct_child_tokens expr) with
  | Some (opening_tokens, closing) when not (List.is_empty opening_tokens) -> (
    tokens_doc opening_tokens,
    token_doc closing
  )
  | _ -> (Doc.text fallback_open, Doc.text fallback_close)

let infix_operator_tokens = fun expr fallback ->
  match direct_child_tokens expr with
  | [] -> [ fallback ]
  | tokens -> tokens

let infix_operator_doc = fun expr fallback -> tokens_doc (infix_operator_tokens expr fallback)

let infix_operator_text = fun expr fallback -> tokens_text (infix_operator_tokens expr fallback)

let prefix_operator_tokens = fun expr fallback ->
  match direct_child_tokens expr with
  | [] -> [ fallback ]
  | tokens -> tokens

let prefix_operator_doc = fun expr fallback -> tokens_doc (prefix_operator_tokens expr fallback)

let prefix_operator_text = fun expr fallback -> tokens_text (prefix_operator_tokens expr fallback)

let prefix_operator_doc_with_explicit_spacing = fun expr fallback ->
  match prefix_operator_tokens expr fallback with
  | first :: (_ :: _ as rest) when Kind.(Ast.Token.kind first = MINUS) -> Doc.concat
    [ token_doc first; Doc.space; tokens_doc rest ]
  | tokens -> tokens_doc tokens

let parenthesized_token_shell_doc = fun tokens ->
  match tokens with
  | opening :: rest when Kind.(Ast.Token.kind opening = LPAREN) -> (
      match split_last rest with
      | Some (inner, closing) when Kind.(Ast.Token.kind closing = RPAREN) -> (
          match inner with
          | [] -> Doc.concat [ token_doc opening; token_doc closing ]
          | inner -> Doc.concat
            [
              token_doc opening;
              Doc.space;
              Doc.concat (List.map inner ~fn:token_doc);
              Doc.space;
              token_doc closing;
            ]
        )
      | _ -> Doc.concat (List.map (opening :: rest) ~fn:token_doc)
    )
  | tokens -> Doc.concat (List.map tokens ~fn:token_doc)

let token_kind_is = fun token kind -> Kind.(Ast.Token.kind token = kind)

let rec node_tokens = fun node ->
  let tokens = ref [] in
  let rec visit node =
    Ast.Node.for_each_child node
      ~fn:(
        function
        | Syn.SyntaxTree.Token id -> tokens := ({ tree = node.tree; id }: Ast.Token.t) :: !tokens
        | Syn.SyntaxTree.Node id -> visit (({ tree = node.tree; id }: Ast.Node.t))
        | Syn.SyntaxTree.Missing _ -> ()
      )
  in
  visit node;
  List.reverse !tokens

let node_token_text_width = fun node ->
  let rec loop width = function
    | [] -> width
    | token :: rest -> loop Int.(width + String.length (Ast.Token.text token)) rest
  in
  loop 0 (node_tokens node)

let node_has_token_kind = fun node kind ->
  List.any (node_tokens node) ~fn:(fun token -> token_kind_is token kind)

let type_token_needs_space = fun previous current ->
  match previous, current with
  | (_, kind) when Kind.(kind = RPAREN || kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE) -> false
  | (_, kind) when Kind.(kind = COMMA || kind = SEMI || kind = COLON) -> false
  | (kind, _) when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE) -> false
  | (kind, _) when Kind.(kind = QUOTE || kind = BACKTICK || kind = QUESTION || kind = TILDE) -> false
  | (kind, _) when Kind.(kind = PERCENT || kind = AT || kind = ATAT) -> false
  | (_, kind) when Kind.(kind = DOT) -> false
  | (kind, current) when Kind.(kind = DOT) -> Kind.(current = QUOTE)
  | (kind, _) when Kind.(kind = COMMA) -> true
  | (_, kind) when Kind.(kind = ARROW || kind = STAR || kind = OF_KW || kind = AS_KW) -> true
  | (kind, _) when Kind.(kind = ARROW || kind = STAR || kind = OF_KW || kind = AS_KW) -> true
  | (kind, _) when Kind.(kind = COLON) -> false
  | _ -> true

let type_tokens_inline_doc = fun tokens ->
  let rec loop previous acc = function
    | [] -> acc
    | token :: rest ->
        let current = Ast.Token.kind token in
        let piece = token_doc token in
        let acc =
          match previous with
          | Some previous when type_token_needs_space previous current -> Doc.concat
            [ acc; Doc.space; piece ]
          | _ -> Doc.concat [ acc; piece ]
        in
        loop (Some current) acc rest
  in
  loop None Doc.empty tokens

let starts_attribute_suffix_tokens = fun token rest ->
  token_kind_is token Kind.LBRACKET && match rest with
  | sigil :: _ -> token_kind_is sigil Kind.AT || token_kind_is sigil Kind.ATAT
  | [] -> false

let starts_floating_attribute_item_tokens = fun token rest ->
  token_kind_is token Kind.LBRACKET && match rest with
  | first :: second :: _ -> (token_kind_is first Kind.ATAT && token_kind_is second Kind.AT)
  || (token_kind_is first Kind.AT && token_kind_is second Kind.ATAT)
  | _ -> false

let starts_extension_item_tokens = fun token rest ->
  token_kind_is token Kind.LBRACKET && match rest with
  | first :: second :: _ -> token_kind_is first Kind.PERCENT && token_kind_is second Kind.PERCENT
  | _ -> false

let starts_shell_body_item_tokens = fun token rest ->
  starts_floating_attribute_item_tokens token rest || starts_extension_item_tokens token rest

let local_module_token_depth_after = fun depth token ->
  let decrease depth =
    if Int.(depth <= 0) then
      0
    else
      Int.(depth - 1)
  in
  match Ast.Token.kind token with
  | kind when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACE) -> Int.(depth + 1)
  | kind when Kind.(kind = STRUCT_KW || kind = SIG_KW || kind = BEGIN_KW) -> Int.(depth + 1)
  | kind when Kind.(kind = RPAREN || kind = RBRACKET || kind = RBRACE) -> decrease depth
  | kind when Kind.(kind = END_KW) -> decrease depth
  | _ -> depth

let local_module_token_needs_space = fun ~depth previous current ->
  match previous, current with
  | (_, kind) when Kind.(kind = RPAREN || kind = RBRACKET || kind = RBRACE) -> false
  | (_, kind) when Kind.(kind = COMMA) -> false
  | (_, kind) when Kind.(kind = DOT) -> false
  | (_, kind) when Kind.(kind = COLON) && Int.equal depth 0 -> false
  | (kind, _) when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACE) -> false
  | (kind, _) when Kind.(kind = DOT || kind = QUESTION || kind = TILDE) -> false
  | (kind, current) when Kind.(kind = COLON && current = UNDERSCORE) -> false
  | _ -> true

let local_module_tokens_doc = fun tokens ->
  let rec loop depth previous acc = function
    | [] -> acc
    | token :: rest ->
        let current = Ast.Token.kind token in
        let piece = token_doc token in
        let acc =
          match previous with
          | Some previous when local_module_token_needs_space ~depth previous current -> Doc.concat
            [ acc; Doc.space; piece ]
          | _ -> Doc.concat [ acc; piece ]
        in
        loop (local_module_token_depth_after depth token) (Some current) acc rest
  in
  loop 0 None Doc.empty tokens

let local_module_split_top_level_token = fun tokens ~matches ->
  let rec loop before depth = function
    | token :: rest when Int.equal depth 0 && matches (Ast.Token.kind token) -> Some (
      List.reverse before,
      token,
      rest
    )
    | token :: rest -> loop (token :: before) (local_module_token_depth_after depth token) rest
    | [] -> None
  in
  loop [] 0 tokens

let local_module_expr_tokens_doc = fun tokens ->
  match local_module_split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = COMMA)) with
  | Some (left, comma, right) -> Doc.concat
    [
      Doc.lparen;
      local_module_tokens_doc left;
      token_doc comma;
      Doc.space;
      local_module_tokens_doc right;
      Doc.rparen
    ]
  | None -> local_module_tokens_doc tokens

let local_module_body_tokens = fun expr ->
  let tokens = ref [] in
  let after_equals = ref false in
  let done_ = ref false in
  Ast.Node.for_each_child_token expr
    ~fn:(fun token ->
      if !after_equals && not !done_ then
        if token_kind_is token Kind.IN_KW then
          done_ := true
        else
          tokens := token :: !tokens
      else if token_kind_is token Kind.EQ then
        after_equals := true);
  List.reverse !tokens

let local_module_starts_structure_body_item = fun kind ->
  Kind.(kind = LET_KW
  || kind = TYPE_KW
  || kind = MODULE_KW
  || kind = OPEN_KW
  || kind = INCLUDE_KW
  || kind = EXTERNAL_KW
  || kind = EXCEPTION_KW
  || kind = CLASS_KW)

let local_module_continues_compound_structure_item_head = fun current token ->
  token_kind_is token Kind.TYPE_KW && match current with
  | previous :: [] -> token_kind_is previous Kind.MODULE_KW || token_kind_is previous Kind.CLASS_KW
  | _ -> false

let local_module_split_structure_body_items = fun tokens ->
  let rec loop current items depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> items
            | _ -> List.reverse current :: items
          )
    | token :: rest when Int.equal depth 0
    && local_module_starts_structure_body_item (Ast.Token.kind token)
    && not (local_module_continues_compound_structure_item_head current token)
    && not (List.is_empty current) -> loop
      [ token ]
      (List.reverse current :: items)
      (local_module_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) items (local_module_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let local_module_structure_body_let_item_doc = fun tokens ->
  match local_module_split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (head_tokens, equals_token, body_tokens) -> Doc.concat
    [
      local_module_tokens_doc head_tokens;
      Doc.space;
      token_doc equals_token;
      Doc.space;
      local_module_expr_tokens_doc body_tokens;
    ]
  | None -> local_module_tokens_doc tokens

let local_module_structure_body_item_doc = fun tokens ->
  match tokens with
  | token :: _ when token_kind_is token Kind.LET_KW -> local_module_structure_body_let_item_doc tokens
  | _ -> local_module_tokens_doc tokens

let local_module_struct_body_doc = fun tokens ->
  match tokens with
  | struct_token :: rest when token_kind_is struct_token Kind.STRUCT_KW ->
      let rec take_body body depth = function
        | [] -> None
        | token :: after when Int.equal depth 0 && token_kind_is token Kind.END_KW -> Some (
          List.reverse body,
          token,
          after
        )
        | token :: rest -> take_body (token :: body) (local_module_token_depth_after depth token) rest
      in
      (
        match take_body [] 0 rest with
        | Some (body_tokens, end_token, []) ->
            let body_items = local_module_split_structure_body_items body_tokens in
            Some (
              Doc.concat
                [ token_doc struct_token; (
                    match body_items with
                    | [] -> Doc.space
                    | items -> Doc.concat
                      [
                        Doc.line;
                        Doc.indent
                          2
                          (Doc.join
                            blank_line
                            (List.map items ~fn:local_module_structure_body_item_doc));
                        Doc.line;
                      ]
                  ); token_doc end_token; ]
            )
        | _ -> None
      )
  | _ -> None

let token_text_is = fun token expected ->
  String.equal (Ast.Token.text token) expected

let token_is_operator_word = fun token ->
  match Ast.Token.text token with
  | "asr"
  | "land"
  | "lor"
  | "lsl"
  | "lsr"
  | "lxor"
  | "mod" -> true
  | _ -> false

let token_is_symbolic_operator_part = fun token ->
  match Ast.Token.kind token with
  | Kind.PLUS
  | Kind.MINUS
  | Kind.STAR
  | Kind.SLASH
  | Kind.PERCENT
  | Kind.CARET
  | Kind.EQ
  | Kind.LT
  | Kind.GT
  | Kind.LTE
  | Kind.GTE
  | Kind.NE
  | Kind.BANG
  | Kind.AMPAMP
  | Kind.BARBAR
  | Kind.PIPE
  | Kind.AMPERSAND
  | Kind.AT
  | Kind.HASH
  | Kind.TILDE
  | Kind.QUESTION
  | Kind.DOLLAR
  | Kind.COLONCOLON
  | Kind.COLONEQ
  | Kind.ARROW
  | Kind.LEFT_ARROW
  | Kind.STARSTAR
  | Kind.EQEQ
  | Kind.BANGEQ
  | Kind.ATAT
  | Kind.PIPEGT
  | Kind.PERCENTGT
  | Kind.LTPERCENT
  | Kind.PLUSDOT
  | Kind.MINUSDOT
  | Kind.STARDOT
  | Kind.SLASHDOT -> true
  | _ -> false

let is_ascii_alpha = function
  | 'a' .. 'z'
  | 'A' .. 'Z' -> true
  | _ -> false

let find_char_from = fun text start target ->
  let rec loop index =
    if Int.(index >= String.length text) then
      None
    else if Char.equal (String.get_unchecked text ~at:index) target then
      Some index
    else
      loop Int.(index + 1)
  in
  loop start

let split_trailing_alpha_suffix = fun text ->
  let rec loop index =
    if Int.(index < 0) then
      0
    else if is_ascii_alpha (String.get_unchecked text ~at:index) then
      loop Int.(index - 1)
    else
      Int.(index + 1)
  in
  let suffix_start = loop Int.(String.length text - 1) in
  (
    String.sub text ~offset:0 ~len:suffix_start,
    if Int.(suffix_start < String.length text) then
      Some (String.sub text ~offset:suffix_start ~len:Int.(String.length text - suffix_start))
    else
      None
  )

let strip_underscores = fun text ->
  if not (String.contains text "_") then
    text
  else
    let length = String.length text in
    let buffer = IO.Buffer.create ~size:length in
    let rec loop index =
      if Int.(index >= length) then
        IO.Buffer.contents buffer
      else
        (
          let char = String.get_unchecked text ~at:index in
          if not (Char.equal char '_') then
            IO.Buffer.add_char buffer char;
          loop Int.(index + 1)
        )
    in
    loop 0

let group_digits_from_left = fun ~group_size digits ->
  let digits = strip_underscores digits in
  let length = String.length digits in
  if Int.(length <= group_size) then
    digits
  else
    let buffer = IO.Buffer.create ~size:Int.(length + (length / group_size)) in
    let rec loop index =
      if Int.(index >= length) then
        IO.Buffer.contents buffer
      else (
        if Int.(index > 0) then
          IO.Buffer.add_char buffer '_';
        let chunk_size = Int.min group_size Int.(length - index) in
        IO.Buffer.add_string buffer (String.sub digits ~offset:index ~len:chunk_size);
        loop Int.(index + chunk_size)
      )
    in
    loop 0

let group_digits_from_right = fun ~group_size digits ->
  let digits = strip_underscores digits in
  let length = String.length digits in
  if Int.(length <= group_size) then
    digits
  else
    let first_group_size =
      match Int.(length mod group_size) with
      | 0 -> group_size
      | remainder -> remainder
    in
    let buffer = IO.Buffer.create ~size:Int.(length + (length / group_size)) in
    IO.Buffer.add_string buffer (String.sub digits ~offset:0 ~len:first_group_size);
    let rec loop index =
      if Int.(index >= length) then
        IO.Buffer.contents buffer
      else (
        IO.Buffer.add_char buffer '_';
        IO.Buffer.add_string buffer (String.sub digits ~offset:index ~len:group_size);
        loop Int.(index + group_size)
      )
    in
    loop first_group_size

type integer_base =
  | Decimal
  | Hexadecimal
  | Octal
  | Binary

type exponent_sign =
  | Positive
  | Negative

type float_exponent = {
  marker: string;
  sign: exponent_sign option;
  digits: string;
}

let integer_parts = fun text ->
  let base, prefix =
    if String.starts_with ~prefix:"0x" text || String.starts_with ~prefix:"0X" text then
      (Hexadecimal, Some (String.sub text ~offset:0 ~len:2))
    else if String.starts_with ~prefix:"0o" text || String.starts_with ~prefix:"0O" text then
      (Octal, Some (String.sub text ~offset:0 ~len:2))
    else if String.starts_with ~prefix:"0b" text || String.starts_with ~prefix:"0B" text then
      (Binary, Some (String.sub text ~offset:0 ~len:2))
    else
      (Decimal, None)
  in
  let digit_start =
    match prefix with
    | Some prefix -> String.length prefix
    | None -> 0
  in
  let is_digit =
    match base with
    | Decimal -> (
        function
        | '0' .. '9'
        | '_' -> true
        | _ -> false
      )
    | Hexadecimal -> (
        function
        | '0' .. '9'
        | 'a' .. 'f'
        | 'A' .. 'F'
        | '_' -> true
        | _ -> false
      )
    | Octal -> (
        function
        | '0' .. '7'
        | '_' -> true
        | _ -> false
      )
    | Binary -> (
        function
        | '0'
        | '1'
        | '_' -> true
        | _ -> false
      )
  in
  let rec find_suffix_start index =
    if Int.(index >= String.length text) then
      index
    else if is_digit (String.get_unchecked text ~at:index) then
      find_suffix_start Int.(index + 1)
    else
      index
  in
  let suffix_start = find_suffix_start digit_start in
  let digits = String.sub text ~offset:digit_start ~len:Int.(suffix_start - digit_start) in
  let suffix =
    if Int.(suffix_start < String.length text) then
      Some (String.sub text ~offset:suffix_start ~len:Int.(String.length text - suffix_start))
    else
      None
  in
  (base, prefix, digits, suffix)

let float_parts = fun text ->
  let body, suffix = split_trailing_alpha_suffix text in
  let exponent_marker_index =
    match find_char_from body 0 'e' with
    | Some _ as found -> found
    | None -> find_char_from body 0 'E'
  in
  let body_without_exponent, exponent =
    match exponent_marker_index with
    | Some index ->
        let exponent_body = String.sub
          body
          ~offset:Int.(index + 1)
          ~len:Int.(String.length body - index - 1) in
        let sign, digits =
          if Int.(String.length exponent_body = 0) then
            (None, "")
          else
            match String.get_unchecked exponent_body ~at:0 with
            | '+' -> (
              Some Positive,
              String.sub exponent_body ~offset:1 ~len:Int.(String.length exponent_body - 1)
            )
            | '-' -> (
              Some Negative,
              String.sub exponent_body ~offset:1 ~len:Int.(String.length exponent_body - 1)
            )
            | _ -> (None, exponent_body)
        in
        (
          String.sub body ~offset:0 ~len:index,
          Some { marker = String.sub body ~offset:index ~len:1; sign; digits }
        )
    | None -> (body, None)
  in
  let dot_index = find_char_from body_without_exponent 0 '.' in
  let integral_digits, fractional_digits =
    match dot_index with
    | Some index -> (
      String.sub body_without_exponent ~offset:0 ~len:index,
      String.sub
        body_without_exponent
        ~offset:Int.(index + 1)
        ~len:Int.(String.length body_without_exponent - index - 1)
    )
    | None -> (body_without_exponent, "")
  in
  (integral_digits, fractional_digits, exponent, suffix)

let render_integer_literal = fun text ->
  let base, prefix, digits, suffix = integer_parts text in
  let prefix =
    match base with
    | Decimal -> Option.unwrap_or prefix ~default:""
    | Hexadecimal -> "0x"
    | Octal -> "0o"
    | Binary -> "0b"
  in
  let digits =
    match base with
    | Decimal
    | Octal -> group_digits_from_right ~group_size:3 digits
    | Binary -> group_digits_from_right ~group_size:4 digits
    | Hexadecimal -> digits |> String.lowercase_ascii |> group_digits_from_right ~group_size:4
  in
  let suffix = Option.unwrap_or suffix ~default:"" in
  prefix ^ digits ^ suffix

let render_float_literal = fun text ->
  let integral_digits, fractional_digits, exponent, suffix = float_parts text in
  let exponent =
    match exponent with
    | None -> ""
    | Some exponent ->
        let sign =
          match exponent.sign with
          | None -> ""
          | Some Positive -> "+"
          | Some Negative -> "-"
        in
        exponent.marker ^ sign ^ exponent.digits
  in
  let suffix = Option.unwrap_or suffix ~default:"" in
  let normalized_integral_digits = strip_underscores integral_digits in
  let integral_digits =
    if Int.(String.length normalized_integral_digits >= 8) then
      group_digits_from_right ~group_size:3 normalized_integral_digits
    else
      normalized_integral_digits
  in
  let fractional_digits = group_digits_from_left ~group_size:3 fractional_digits in
  integral_digits ^ "." ^ fractional_digits ^ exponent ^ suffix

let literal_token_doc = fun token ->
  match Ast.Token.kind token with
  | kind when Kind.(kind = INT) -> Doc.text (render_integer_literal (Ast.Token.text token))
  | kind when Kind.(kind = FLOAT) -> Doc.text (render_float_literal (Ast.Token.text token))
  | _ -> token_doc token

let strip_leading_whitespace = fun text ->
  let length = String.length text in
  let rec loop index =
    if Int.(index >= length) then
      ""
    else
      match String.get_unchecked text ~at:index with
      | ' '
      | '\t'
      | '\n'
      | '\r' -> loop Int.(index + 1)
      | _ ->
          if Int.equal index 0 then
            text
          else
            String.sub text ~offset:index ~len:Int.(length - index)
  in
  loop 0

let strip_trailing_whitespace = fun text ->
  let rec loop index =
    if Int.(index < 0) then
      ""
    else
      match String.get_unchecked text ~at:index with
      | ' '
      | '\t'
      | '\n'
      | '\r' -> loop Int.(index - 1)
      | _ -> String.sub text ~offset:0 ~len:Int.(index + 1)
  in
  loop Int.(String.length text - 1)

let strip_trailing_horizontal_whitespace = fun text ->
  let rec loop index =
    if Int.(index < 0) then
      ""
    else
      match String.get_unchecked text ~at:index with
      | ' '
      | '\t' -> loop Int.(index - 1)
      | _ -> String.sub text ~offset:0 ~len:Int.(index + 1)
  in
  loop Int.(String.length text - 1)

type leading_docstring = {
  text: string;
  is_section: bool;
}

type leading_comment = {
  text: string;
  kind: Kind.t;
  is_section: bool;
}

let is_section_docstring_text = fun comment_text ->
  let len = String.length comment_text in
  if Int.(len < 5) then
    false
  else
    let body = String.sub comment_text ~offset:3 ~len:Int.(len - 5) |> String.trim in
    if Int.(String.length body = 0) then
      false
    else
      let first = String.get_unchecked body ~at:0 in
      Char.equal first '{' || Char.equal first '#'

let is_markdown_section_docstring_text = fun comment_text ->
  let len = String.length comment_text in
  if Int.(len < 5) then
    false
  else
    let body = String.sub comment_text ~offset:3 ~len:Int.(len - 5) |> String.trim in
    Int.(String.length body > 0) && Char.equal (String.get_unchecked body ~at:0) '#'

let leading_docstring_separator = fun (left: leading_docstring) (_right: leading_docstring) ->
  if not left.is_section then
    "\n"
  else
    "\n\n"

let standalone_docstring_separator = fun (left: leading_docstring) (right: leading_docstring) ->
  if (not left.is_section) && right.is_section then
    "\n"
  else
    "\n\n"

let rec leading_docstring_text_with_separator:
  separator:(leading_docstring -> leading_docstring -> string) -> leading_docstring list -> string = fun ~separator ->
  function
  | [] ->
      ""
  | [ docstring ] ->
      let suffix =
        if docstring.is_section then
          "\n\n"
        else
          "\n"
      in
      docstring.text ^ suffix
  | left :: (right :: _ as rest) ->
      left.text ^ separator left right ^ leading_docstring_text_with_separator ~separator rest

let leading_docstring_text = leading_docstring_text_with_separator ~separator:leading_docstring_separator

let standalone_docstring_text = leading_docstring_text_with_separator ~separator:standalone_docstring_separator

let leading_comments = fun token ->
  let comments = ref [] in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = COMMENT || kind = DOCSTRING) then
        comments := {
          text = strip_trailing_whitespace text;
          kind;
          is_section = Kind.(kind = DOCSTRING) && is_section_docstring_text text
        }
        :: !comments);
  List.reverse !comments

let normalized_leading_docstrings_with = fun ~docstrings_text token ->
  let docstrings = ref [] in
  let has_comment = ref false in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = DOCSTRING) then
        docstrings := {
          text = strip_trailing_whitespace text;
          is_section = is_section_docstring_text text
        }
        :: !docstrings
      else if Kind.(kind = COMMENT) then
        has_comment := true);
  if !has_comment then
    None
  else
    match List.reverse !docstrings with
    | [] -> None
    | docstrings -> Some (docstrings_text docstrings)

let normalized_leading_docstrings = normalized_leading_docstrings_with ~docstrings_text:leading_docstring_text

let normalized_standalone_docstrings = normalized_leading_docstrings_with ~docstrings_text:standalone_docstring_text

let paragraph_comment_separator = fun left right ->
  match Kind.(left.kind = DOCSTRING), Kind.(right.kind = DOCSTRING) with
  | true, true -> leading_docstring_separator
    { text = left.text; is_section = left.is_section }
    { text = right.text; is_section = right.is_section }
  | true, false when not left.is_section -> "\n"
  | _ -> "\n\n"

let rec paragraph_comment_text = function
  | [] ->
      ""
  | [ comment ] ->
      let suffix =
        if Kind.(comment.kind = DOCSTRING) && not comment.is_section then
          "\n"
        else
          "\n\n"
      in
      comment.text ^ suffix
  | left :: (right :: _ as rest) ->
      left.text ^ paragraph_comment_separator left right ^ paragraph_comment_text rest

let leading_comment_text = fun token ->
  if Ast.Token.has_leading_docstring token then
    match normalized_leading_docstrings token with
    | Some text -> text
    | None ->
        let text = Ast.Token.leading_text token |> strip_leading_whitespace in
        let text = strip_trailing_whitespace text in
        if Int.(String.length text = 0) then
          ""
        else
          text ^ "\n"
  else
    Ast.Token.leading_text token |> strip_leading_whitespace

let standalone_comment_text = fun token ->
  if Ast.Token.has_leading_docstring token then
    match normalized_standalone_docstrings token with
    | Some text -> text
    | None ->
        let text = Ast.Token.leading_text token |> strip_leading_whitespace in
        let text = strip_trailing_whitespace text in
        if Int.(String.length text = 0) then
          ""
        else
          text ^ "\n"
  else
    Ast.Token.leading_text token |> strip_leading_whitespace

let text_lines_doc = fun text -> text |> String.split ~by:"\n" |> List.map ~fn:Doc.text |> Doc.lines

let compact_trailing_blank_line = fun text ->
  if String.ends_with ~suffix:"\n\n" text then
    String.sub text ~offset:0 ~len:(String.length text - 1)
  else
    text

let leading_comment_token_doc = fun ?(compact_trailing_blank = false) token ->
  if Ast.Token.has_leading_comment token then
    let text = leading_comment_text token in
    let text =
      if compact_trailing_blank then
        compact_trailing_blank_line text
      else
        text
    in
    text_lines_doc text
  else
    Doc.empty

let trimmed_leading_comment_token_doc = fun token ->
  let doc = ref Doc.empty in
  let first = ref true in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = COMMENT || kind = DOCSTRING) then
        (
          let comment = text |> strip_trailing_whitespace |> Doc.text in
          doc := if !first then
            comment
          else
            Doc.concat [ !doc; Doc.line; comment ];
          first := false
        ));
  !doc

let inline_leading_comment_token_doc = fun token ->
  leading_comment_text token |> strip_trailing_whitespace |> Doc.text

let token_has_inline_leading_comment = fun token ->
  let text = Ast.Token.leading_text token in
  let length = String.length text in
  let rec loop index =
    if Int.(index >= length) then
      false
    else
      match String.get_unchecked text ~at:index with
      | ' '
      | '\t'
      | '\r' -> loop Int.(index + 1)
      | '\n' -> false
      | _ -> Ast.Token.has_leading_comment token
  in
  loop 0

let token_has_inline_leading_comment_after_horizontal = fun token ->
  if not (token_has_inline_leading_comment token) then
    false
  else
    let text = Ast.Token.leading_text token in
    Int.(String.length text > 0) && match String.get_unchecked text ~at:0 with
    | ' '
    | '\t'
    | '\r' -> true
    | _ -> false

let token_has_leading_plain_comment = fun token ->
  let has_plain_comment = ref false in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text:_ ->
      if Kind.(kind = COMMENT) then
        has_plain_comment := true);
  !has_plain_comment

let comments_have_plain_comment = fun comments ->
  List.any comments ~fn:(fun comment -> Kind.(comment.kind = COMMENT))

let leading_comments_for_paragraph = fun ?(drop_inline = false) token ->
  let comments = leading_comments token in
  if drop_inline && token_has_inline_leading_comment_after_horizontal token then
    match comments with
    | _ :: rest -> rest
    | [] -> []
  else
    comments

let token_has_leading_nonsection_comment = fun token ->
  let has_comment = ref false in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = COMMENT) then
        has_comment := true
      else if Kind.(kind = DOCSTRING) && not (is_section_docstring_text text) then
        has_comment := true);
  !has_comment

let token_has_leading_markdown_section_docstring = fun token ->
  let has_section = ref false in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = DOCSTRING) && is_markdown_section_docstring_text text then
        has_section := true);
  !has_section

let leading_comment_token_paragraph_doc = fun ?(drop_inline = false) token ->
  if Ast.Token.has_leading_comment token then
    let dropped_inline = drop_inline && token_has_inline_leading_comment_after_horizontal token in
    let comments = leading_comments_for_paragraph ~drop_inline token in
    if List.is_empty comments then
      Doc.empty
    else
      let text =
        if comments_have_plain_comment comments || dropped_inline then
          paragraph_comment_text comments
        else
          leading_comment_text token
      in
      let text = text |> strip_trailing_horizontal_whitespace in
      let text =
        if not (comments_have_plain_comment comments) then
          text
        else if String.ends_with ~suffix:"\n\n" text then
          text
        else if String.ends_with ~suffix:"\n" text then
          text ^ "\n"
        else
          text ^ "\n\n"
      in
      text_lines_doc text
  else
    Doc.empty

let leading_comment_node_paragraph_doc = fun ?(drop_inline = false) node ->
  match Ast.Node.first_descendant_token node with
  | Some token -> leading_comment_token_paragraph_doc ~drop_inline token
  | None -> Doc.empty

let inline_leading_comment_node_doc = fun node ->
  match Ast.Node.first_descendant_token node with
  | Some token when token_has_inline_leading_comment_after_horizontal token
  && token_has_leading_plain_comment token -> inline_leading_comment_token_doc token
  | _ -> Doc.empty

let eof_comment_token = fun source_file ->
  match Ast.Node.first_child_token source_file ~kind:Kind.EOF with
  | Some token when Ast.Token.has_leading_comment token -> Some token
  | _ -> None

let eof_comment_doc = fun source_file ->
  match eof_comment_token source_file with
  | Some token -> Doc.text (standalone_comment_text token)
  | None -> Doc.empty

let eof_comment_separator = fun source_file ->
  match eof_comment_token source_file with
  | Some token when token_has_leading_plain_comment token
  && not (Ast.Token.has_leading_docstring token) -> Doc.line
  | _ -> blank_line

let shell_token_needs_space = fun previous current ->
  match previous, current with
  | (_, kind) when Kind.(kind = RBRACKET || kind = BAR_RBRACKET) -> false
  | (_, kind) when Kind.(kind = DOT || kind = COLON) -> false
  | (kind, _) when Kind.(kind = LBRACKET || kind = LBRACKET_BAR) -> false
  | (kind, _) when Kind.(kind = DOT) -> false
  | (kind, _) when Kind.(kind = AT || kind = ATAT || kind = PERCENT || kind = PERCENTGT || kind = LTPERCENT) -> false
  | _ -> true

let bracketed_shell_doc = fun ~empty_message ~for_each_shell_token ->
  let shell = ref Doc.empty in
  let first = ref true in
  let previous = ref None in
  for_each_shell_token
    ~fn:(fun token ->
      let current = Ast.Token.kind token in
      let piece = token_doc token in
      let part =
        match !previous with
        | None ->
            first := false;
            piece
        | Some previous when shell_token_needs_space previous current ->
            Doc.concat [ Doc.space; piece ]
        | Some _ ->
            piece
      in
      shell := Doc.concat [ !shell; part ];
      previous := Some current);
  if !first then
    unsupported empty_message
  else
    !shell

let attribute_shell_doc = fun ~for_each_shell_token ->
  bracketed_shell_doc ~empty_message:"attribute without shell tokens" ~for_each_shell_token

let extension_shell_doc = fun ~for_each_shell_token ->
  bracketed_shell_doc ~empty_message:"extension without shell tokens" ~for_each_shell_token

let for_each_token_in_list = fun tokens ~fn ->
  let rec loop = function
    | [] -> ()
    | token :: rest ->
        fn token;
        loop rest
  in
  loop tokens

let attribute_shell_tokens_doc = fun tokens ->
  attribute_shell_doc ~for_each_shell_token:(fun ~fn -> for_each_token_in_list tokens ~fn)

let extension_shell_tokens_doc = fun tokens ->
  extension_shell_doc ~for_each_shell_token:(fun ~fn -> for_each_token_in_list tokens ~fn)

let path_doc = fun path ->
  let segments = ref [] in
  Ast.Path.for_each_ident path ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> parenthesized_token_shell_doc (direct_child_tokens path)
  | segments -> Doc.join (Doc.text ".") segments

let open_path_doc = fun decl ->
  let segments = ref [] in
  Ast.OpenDeclaration.for_each_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  Doc.join (Doc.text ".") (List.reverse !segments)

let include_path_doc = fun decl ->
  let segments = ref [] in
  Ast.IncludeDeclaration.for_each_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "include declaration without target"
  | segments -> Doc.join (Doc.text ".") segments

let child_expr_docs = fun expr ->
  let docs = ref [] in
  Ast.Expr.for_each_child_expr expr ~fn:(fun child -> docs := child :: !docs);
  List.reverse !docs

let expr_list_has_trailing_separator = fun expr items ->
  match List.reverse items with
  | [] -> false
  | last_item :: _ ->
      let _, last_item_end = Ast.Node.raw_range last_item in
      let rec loop = function
        | [] -> false
        | token :: rest ->
            let token_start, _ = Ast.Token.raw_range token in
            if Int.(token_start < last_item_end) then
              loop rest
            else if token_kind_is token Kind.RBRACKET then
              false
            else if token_kind_is token Kind.SEMI then
              true
            else
              loop rest
      in
      loop (direct_child_tokens expr)

let child_pattern_docs = fun pattern ->
  let docs = ref [] in
  Ast.Pattern.for_each_child_pattern pattern ~fn:(fun child -> docs := child :: !docs);
  List.reverse !docs

let or_pattern_items = fun pattern ->
  let rec collect pattern acc =
    match Ast.Pattern.view pattern with
    | Or { left=Some left; right=Some right } -> collect left (collect right acc)
    | _ -> pattern :: acc
  in
  collect pattern []

let direct_pattern_docs = fun node ->
  let docs = ref [] in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.Pattern.cast child with
      | Some pattern -> docs := pattern :: !docs
      | None -> ());
  List.reverse !docs

let first_ident_token = fun node ->
  let found = ref None in
  Ast.Node.for_each_child_token node
    ~fn:(fun token ->
      match !found with
      | Some _ -> ()
      | None ->
          if Kind.(Ast.Token.kind token = IDENT) then
            found := Some token);
  !found

let let_binding_nodes = fun node ->
  let bindings = ref [] in
  Ast.Node.for_each_child_node node
    ~fn:(fun child ->
      match Ast.LetBinding.cast child with
      | Some binding -> bindings := binding :: !bindings
      | None -> ());
  List.reverse !bindings

type let_binding_parts = {
  pattern: Ast.pattern option;
  parameters: Ast.pattern list;
  annotation: Ast.type_expr option;
  body: Ast.expr option;
}

type binding_annotation =
  | TypeAnnotation of Ast.TypeExpr.t
  | TokenAnnotation of Ast.Token.t list

type binding_head_parameter =
  | PatternParameter of Ast.Pattern.t
  | LabelOnlyParameter of Ast.Pattern.t

let let_binding_parts = fun binding ->
  let parameters = ref [] in
  let annotation = ref None in
  let body = ref None in
  Ast.LetBinding.for_each_parameter binding ~fn:(fun pattern -> parameters := pattern :: !parameters);
  Ast.Node.for_each_child_node binding
    ~fn:(fun child ->
      match Ast.TypeExpr.cast child with
      | Some type_expr -> (
          match !annotation with
          | None -> annotation := Some type_expr
          | Some _ -> ()
        )
      | None -> (
          match Ast.Expr.cast child with
          | Some expr -> (
              match !body with
              | None -> body := Some expr
              | Some _ -> ()
            )
          | None -> ()
        ));
  {
    pattern = Ast.LetBinding.pattern binding;
    parameters = List.reverse !parameters;
    annotation = !annotation;
    body = !body
  }

let rec type_expr_doc = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Path { path } ->
      path_doc path
  | Var { name=Some name } ->
      Doc.concat [ Doc.text "'"; token_doc name ]
  | Var { name=None } ->
      unsupported "type variable without name"
  | Wildcard ->
      Doc.text "_"
  | Arrow { left=Some left; right=Some right } ->
      Doc.concat [ type_expr_doc left; Doc.space; Doc.arrow; Doc.space; type_expr_doc right ]
  | Arrow _ ->
      unsupported "incomplete arrow type expression"
  | Poly { body=Some body } ->
      let names = ref [] in
      Ast.TypeExpr.for_each_poly_type_name
        type_expr
        ~fn:(fun name -> names := token_doc name :: !names);
      (
        match List.reverse !names with
        | [] -> unsupported "locally abstract type without names"
        | names -> Doc.concat
          [
            Doc.text "type";
            Doc.space;
            Doc.join Doc.space names;
            Doc.text ".";
            Doc.space;
            type_expr_doc body;
          ]
      )
  | Poly { body=None } ->
      unsupported "locally abstract type without body"
  | Labeled { optional_token; label=Some label; annotation=Some annotation } ->
      Doc.concat
        [
          optional_token_doc optional_token;
          token_doc label;
          Doc.text ":";
          type_expr_doc annotation;
        ]
  | Labeled _ ->
      unsupported "incomplete labeled type expression"
  | Tuple { separator=Star; _ } ->
      let items = ref [] in
      Ast.TypeExpr.for_each_child_type
        type_expr
        ~fn:(fun child -> items := type_expr_doc child :: !items);
      (
        match List.reverse !items with
        | [] -> unsupported "incomplete tuple type expression"
        | items -> Doc.join (Doc.concat [ Doc.space; Doc.text "*"; Doc.space ]) items
      )
  | Tuple { separator=Comma; _ } ->
      let items = ref [] in
      Ast.TypeExpr.for_each_child_type
        type_expr
        ~fn:(fun child -> items := type_expr_doc child :: !items);
      (
        match List.reverse !items with
        | [] -> unsupported "incomplete tuple type expression"
        | items -> Doc.concat
          [ Doc.lparen; Doc.join (Doc.concat [ Doc.comma; Doc.space ]) items; Doc.rparen; ]
      )
  | Tuple { separator=UnknownSeparator; _ } ->
      unsupported "tuple type expression without separator"
  | Apply { argument=Some argument; constructor=Some constructor } ->
      Doc.concat [ type_expr_doc argument; Doc.space; type_expr_doc constructor ]
  | Apply _ ->
      unsupported "incomplete type application"
  | Parenthesized { inner=Some inner } ->
      Doc.concat [ Doc.lparen; type_expr_doc inner; Doc.rparen ]
  | Parenthesized { inner=None } ->
      Doc.concat [ Doc.lparen; Doc.rparen ]
  | Opaque node
  | Unknown node ->
      type_tokens_inline_doc (node_tokens node)
  | Error _ ->
      unsupported "unsupported type expression"

let type_expr_poly_names_doc = fun type_expr ->
  let names = ref [] in
  Ast.TypeExpr.for_each_poly_type_name type_expr ~fn:(fun name -> names := token_doc name :: !names);
  match List.reverse !names with
  | [] -> None
  | names -> Some (Doc.concat
    [ Doc.text "type"; Doc.space; Doc.join Doc.space names; Doc.text "."; Doc.space ])

let type_expr_arrow_chain_docs = fun type_expr ->
  let rec collect type_expr docs =
    match Ast.TypeExpr.view type_expr with
    | Arrow { left=Some left; right=Some right } -> collect right (type_expr_doc left :: docs)
    | _ -> List.reverse (type_expr_doc type_expr :: docs)
  in
  collect type_expr []

let type_expr_top_level_arrow_count = fun type_expr ->
  let rec count type_expr =
    match Ast.TypeExpr.view type_expr with
    | Arrow { right=Some right; _ } -> Int.(1 + count right)
    | _ -> 0
  in
  count type_expr

let type_expr_doc_with_arrow_threshold = fun ~threshold type_expr ->
  if Int.(type_expr_top_level_arrow_count type_expr >= threshold) then
    Doc.join (Doc.concat [ Doc.space; Doc.arrow; Doc.line ]) (type_expr_arrow_chain_docs type_expr)
  else
    type_expr_doc type_expr

let type_expr_binding_annotation_doc = fun type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Poly { body=Some body } -> (
      match type_expr_poly_names_doc type_expr with
      | None -> type_expr_doc type_expr
      | Some prefix -> (
          match type_expr_arrow_chain_docs body with
          | _ :: _ :: _ as parts -> Doc.concat
            [ prefix; Doc.join (Doc.concat [ Doc.space; Doc.arrow; Doc.line ]) parts; ]
          | _ -> type_expr_doc type_expr
        )
    )
  | _ -> type_expr_doc_with_arrow_threshold ~threshold:2 type_expr

let rec pattern_doc = fun pattern ->
  match Ast.Pattern.view pattern with
  | Wildcard ->
      Doc.text "_"
  | Path { path } ->
      path_doc path
  | Literal { token=Some token } -> (
      match Ast.Pattern.literal_sign_token pattern with
      | Some sign -> Doc.concat [ token_doc sign; literal_token_doc token ]
      | None -> literal_token_doc token
    )
  | Literal { token=None } ->
      unsupported "literal pattern without token"
  | Parenthesized { inner=Some inner } when pattern_is_operator_word_path inner ->
      Doc.concat [ Doc.lparen; Doc.space; pattern_doc inner; Doc.space; Doc.rparen ]
  | Parenthesized { inner=Some inner } ->
      let inner_doc = pattern_doc inner in
      if Doc.is_multiline inner_doc then
        Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
      else
        Doc.concat [ Doc.lparen; inner_doc; Doc.rparen ]
  | Parenthesized { inner=None } ->
      parenthesized_empty_pattern_doc pattern
  | Tuple ->
      child_pattern_docs pattern
      |> List.map ~fn:pattern_doc
      |> Doc.join (Doc.concat [ Doc.comma; Doc.space ])
  | List -> (
      match child_pattern_docs pattern |> List.map ~fn:pattern_doc with
      | [] -> Doc.concat [ Doc.lbracket; Doc.rbracket ]
      | items -> Doc.concat
        [
          Doc.lbracket;
          Doc.space;
          Doc.join (Doc.concat [ Doc.semi; Doc.space ]) items;
          Doc.space;
          Doc.rbracket;
        ]
    )
  | Array ->
      child_pattern_docs pattern
      |> List.map ~fn:pattern_doc
      |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])
      |> fun items -> Doc.concat [ Doc.text "[|"; items; Doc.text "|]" ]
  | Record ->
      record_pattern_doc pattern
  | Cons { head=Some head; tail=Some tail } ->
      Doc.concat [ pattern_doc head; Doc.space; Doc.text "::"; Doc.space; pattern_doc tail ]
  | Cons _ ->
      unsupported "incomplete cons pattern"
  | Constraint { pattern=Some pattern; annotation=Some annotation } ->
      let annotation_doc = type_expr_doc_with_arrow_threshold ~threshold:2 annotation in
      if Doc.is_multiline annotation_doc then
        Doc.concat [ pattern_doc pattern; Doc.text ":"; Doc.line; Doc.indent 2 annotation_doc ]
      else
        Doc.concat [ pattern_doc pattern; Doc.text ":"; Doc.space; annotation_doc ]
  | Constraint _ ->
      unsupported "incomplete typed pattern"
  | Alias { pattern=Some pattern; alias=Some alias } ->
      Doc.concat [ pattern_doc pattern; Doc.space; Doc.text "as"; Doc.space; pattern_doc alias ]
  | Alias _ ->
      unsupported "incomplete alias pattern"
  | Apply { callee=Some callee; argument=Some argument } ->
      Doc.concat [ pattern_doc callee; Doc.space; pattern_apply_argument_doc argument ]
  | Apply _ ->
      unsupported "incomplete apply pattern"
  | Or { left=Some left; right=Some right } ->
      Doc.concat [ pattern_doc left; Doc.space; Doc.bar; Doc.space; pattern_doc right ]
  | Or _ ->
      unsupported "incomplete or pattern"
  | PolyVariant ->
      if node_has_token_kind pattern Kind.HASH then
        (
          match child_pattern_docs pattern with
          | [ inherited ] -> Doc.concat [ Doc.text "#"; pattern_doc inherited ]
          | _ -> unsupported "polymorphic variant inherit pattern without path"
        )
      else
        let head =
          match first_ident_token pattern with
          | Some tag -> Doc.concat [ Doc.text "`"; token_doc tag ]
          | None -> unsupported "polymorphic variant pattern without tag"
        in
        (
          match child_pattern_docs pattern with
          | [] -> head
          | [ payload ] -> Doc.concat [ head; Doc.space; pattern_doc payload ]
          | _ -> unsupported "polymorphic variant pattern with multiple payloads"
        )
  | LabeledParam parameter ->
      parameter_doc parameter
  | OptionalParam parameter ->
      parameter_doc parameter
  | OptionalParamDefault parameter ->
      parameter_doc parameter
  | Interval { left=Some left; right=Some right } ->
      Doc.concat [ pattern_doc left; Doc.space; Doc.text ".."; Doc.space; pattern_doc right ]
  | Interval _ ->
      unsupported "incomplete interval pattern"
  | Lazy { pattern=Some pattern } ->
      Doc.concat [ Doc.text "lazy"; Doc.space; pattern_doc pattern ]
  | Lazy _ ->
      unsupported "lazy pattern without payload"
  | Exception { pattern=Some pattern } ->
      Doc.concat [ Doc.text "exception"; Doc.space; pattern_doc pattern ]
  | Exception _ ->
      unsupported "exception pattern without payload"
  | LocalOpen ->
      local_open_pattern_doc pattern
  | Attribute { inner=Some _ } ->
      attribute_pattern_doc pattern
  | Attribute _ ->
      unsupported "attribute pattern without inner pattern"
  | Extension ->
      extension_pattern_doc pattern
  | LocallyAbstractType ->
      locally_abstract_type_pattern_doc pattern
  | FirstClassModule ->
      first_class_module_pattern_doc pattern
  | Error _
  | Unknown _ ->
      unsupported "unsupported pattern"

and pattern_apply_argument_doc = fun pattern ->
  match Ast.Pattern.view pattern with
  | Record -> compact_record_pattern_doc pattern
  | _ -> pattern_doc pattern

and parenthesized_empty_pattern_doc = fun pattern ->
  parenthesized_token_shell_doc (direct_child_tokens pattern)

and attribute_pattern_doc = fun pattern ->
  match Ast.AttributePattern.cast pattern with
  | Some attribute -> (
      match Ast.AttributePattern.inner attribute with
      | Some inner -> Doc.concat
        [
          pattern_doc inner;
          Doc.space;
          attribute_shell_doc
            ~for_each_shell_token:(fun ~fn -> Ast.AttributePattern.for_each_shell_token attribute ~fn);
        ]
      | None -> unsupported "attribute pattern without inner pattern"
    )
  | None -> unsupported "unsupported attribute pattern"

and extension_pattern_doc = fun pattern ->
  match Ast.ExtensionPattern.cast pattern with
  | Some extension -> extension_shell_doc
    ~for_each_shell_token:(fun ~fn -> Ast.ExtensionPattern.for_each_shell_token extension ~fn)
  | None -> unsupported "unsupported extension pattern"

and locally_abstract_type_pattern_doc = fun pattern ->
  match Ast.LocallyAbstractTypePattern.cast pattern with
  | None -> unsupported "unsupported locally abstract type pattern"
  | Some pattern -> (
      let names = ref [] in
      Ast.LocallyAbstractTypePattern.for_each_type_name
        pattern
        ~fn:(fun token -> names := token_doc token :: !names);
      match Ast.LocallyAbstractTypePattern.opening_token pattern, Ast.LocallyAbstractTypePattern.type_token
        pattern, List.reverse !names, Ast.LocallyAbstractTypePattern.closing_token pattern with
      | Some opening_token, Some type_token, [], Some closing_token -> Doc.concat
        [ token_doc opening_token; token_doc type_token; token_doc closing_token ]
      | Some opening_token, Some type_token, names, Some closing_token -> Doc.concat
        [
          token_doc opening_token;
          token_doc type_token;
          Doc.space;
          Doc.join Doc.space names;
          token_doc closing_token;
        ]
      | _ -> unsupported "incomplete locally abstract type pattern"
    )

and first_class_module_pattern_ascription_doc = fun pattern ->
  let segments = ref [] in
  Ast.FirstClassModulePattern.for_each_ascription_path_ident
    pattern
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "first-class module pattern without module type path"
  | segments -> Doc.join (Doc.text ".") segments

and first_class_module_pattern_doc = fun pattern ->
  let module_pattern =
    match Ast.FirstClassModulePattern.cast pattern with
    | Some module_pattern -> module_pattern
    | None -> unsupported "unsupported first-class module pattern"
  in
  match Ast.FirstClassModulePattern.opening_token module_pattern, Ast.FirstClassModulePattern.module_token
    module_pattern, Ast.FirstClassModulePattern.binder module_pattern, Ast.FirstClassModulePattern.ascription
    module_pattern, Ast.FirstClassModulePattern.closing_token module_pattern with
  | Some opening_token, Some module_token, Some binder, Ast.FirstClassModulePattern.NoAscription, Some closing_token ->
      Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          token_doc binder;
          token_doc closing_token;
        ]
  | Some opening_token, Some module_token, Some binder, Ast.FirstClassModulePattern.PathAscription, Some closing_token -> (
      match Ast.FirstClassModulePattern.colon_token module_pattern with
      | Some colon_token -> Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          token_doc binder;
          Doc.space;
          token_doc colon_token;
          Doc.space;
          first_class_module_pattern_ascription_doc module_pattern;
          token_doc closing_token;
        ]
      | None -> unsupported "first-class module pattern ascription without colon token"
    )
  | _ ->
      unsupported "unsupported first-class module pattern"

and local_open_pattern_path_doc = fun pattern ->
  let segments = ref [] in
  Ast.LocalOpenPattern.for_each_module_path_ident
    pattern
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "local open pattern without module path"
  | segments -> Doc.join (Doc.text ".") segments

and local_open_pattern_doc = fun pattern ->
  match Ast.LocalOpenPattern.cast pattern with
  | None -> unsupported "unsupported local open pattern"
  | Some local_open -> (
      match Ast.LocalOpenPattern.dot_token local_open, Ast.LocalOpenPattern.opening_token local_open, Ast.LocalOpenPattern.pattern
        local_open, Ast.LocalOpenPattern.closing_token local_open with
      | Some dot_token, Some opening_token, Some inner, Some closing_token ->
          Doc.concat
            [
              local_open_pattern_path_doc local_open;
              token_doc dot_token;
              token_doc opening_token;
              (
                match Ast.Pattern.view inner with
                | Record -> record_pattern_body_doc inner
                | List -> list_pattern_body_doc inner
                | Array -> array_pattern_body_doc inner
                | _ -> pattern_doc inner
              );
              token_doc closing_token;
            ]
      | _ -> unsupported "incomplete local open pattern"
    )

and record_pattern_field_doc = fun (field: Ast.RecordPattern.field) ->
  match field.path with
  | Some path -> (
      match field.pattern with
      | Some pattern -> Doc.concat
        [ path_doc path; Doc.space; Doc.equal; Doc.space; pattern_doc pattern ]
      | None -> path_doc path
    )
  | None -> unsupported "unsupported record pattern field"

and record_pattern_body_doc = fun pattern ->
  let fields = ref [] in
  Ast.RecordPattern.for_each_field
    pattern
    ~fn:(fun field -> fields := record_pattern_field_doc field :: !fields);
  let fields =
    (
      match Ast.RecordPattern.open_wildcard pattern with
      | Some wildcard -> token_doc wildcard :: !fields
      | None -> !fields
    )
    |> List.reverse
  in
  match fields with
  | [] -> Doc.empty
  | fields when Int.(List.length fields > 3) -> Doc.concat
    [ Doc.line; Doc.indent 2 (Doc.join (Doc.concat [ Doc.semi; Doc.line ]) fields); Doc.line; ]
  | fields -> Doc.concat
    [ Doc.space; Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields; Doc.space; ]

and record_pattern_doc = fun pattern ->
  Doc.concat [ Doc.lbrace; record_pattern_body_doc pattern; Doc.rbrace ]

and list_pattern_body_doc = fun pattern ->
  match child_pattern_docs pattern |> List.map ~fn:pattern_doc with
  | [] -> Doc.empty
  | items -> Doc.concat
    [ Doc.space; Doc.join (Doc.concat [ Doc.semi; Doc.space ]) items; Doc.space; ]

and array_pattern_body_doc = fun pattern ->
  child_pattern_docs pattern
  |> List.map ~fn:pattern_doc
  |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])

and compact_record_pattern_field_doc = fun (field: Ast.RecordPattern.field) ->
  match field.path with
  | Some path -> (
      match field.pattern with
      | Some pattern -> Doc.concat [ path_doc path; Doc.equal; pattern_doc pattern ]
      | None -> path_doc path
    )
  | None -> unsupported "unsupported record pattern field"

and compact_record_pattern_doc = fun pattern ->
  let fields = ref [] in
  Ast.RecordPattern.for_each_field
    pattern
    ~fn:(fun field -> fields := compact_record_pattern_field_doc field :: !fields);
  let fields =
    (
      match Ast.RecordPattern.open_wildcard pattern with
      | Some wildcard -> token_doc wildcard :: !fields
      | None -> !fields
    )
    |> List.reverse
  in
  match fields with
  | [] -> Doc.concat [ Doc.lbrace; Doc.rbrace ]
  | fields when Int.(List.length fields > 3) -> Doc.concat
    [
      Doc.lbrace;
      Doc.line;
      Doc.indent 2 (Doc.join (Doc.concat [ Doc.semi; Doc.line ]) fields);
      Doc.line;
      Doc.rbrace;
    ]
  | fields -> Doc.concat
    [
      Doc.lbrace;
      Doc.space;
      Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields;
      Doc.space;
      Doc.rbrace;
    ]

and path_single_ident_token = fun path ->
  let found = ref None in
  let count = ref 0 in
  Ast.Path.for_each_ident path
    ~fn:(fun token ->
      count := !count + 1;
      if Int.equal !count 1 then
        found := Some token
      else
        ());
  if Int.equal !count 1 then
    !found
  else
    None

and path_single_operator_word_token = fun path ->
  match path_single_ident_token path with
  | Some token when token_is_operator_word token -> Some token
  | _ -> None

and pattern_is_operator_word_path = fun pattern ->
  match Ast.Pattern.view pattern with
  | Path { path } -> Option.is_some (path_single_operator_word_token path)
  | _ -> false

and expr_is_operator_word_path = fun expr ->
  match Ast.Expr.view expr with
  | Path { path } -> Option.is_some (path_single_operator_word_token path)
  | _ -> false

and expr_is_operator_value_path = fun expr ->
  match Ast.Expr.view expr with
  | Path { path } ->
      Option.is_some (path_single_operator_word_token path) || (
        match direct_child_tokens path with
        | [] -> false
        | tokens -> List.all tokens ~fn:token_is_symbolic_operator_part
      )
  | _ -> false

and pattern_binding_ident_token = fun pattern ->
  match Ast.Pattern.view pattern with
  | Path { path } -> path_single_ident_token path
  | Parenthesized { inner=Some inner }
  | Constraint { pattern=Some inner; _ }
  | Attribute { inner=Some inner } -> pattern_binding_ident_token inner
  | _ -> None

and parameter_pattern_matches_label = fun label pattern ->
  match pattern_binding_ident_token pattern with
  | Some binding -> token_text_equal label binding
  | None -> false

and split_typed_parameter_pattern = fun pattern ->
  match Ast.Pattern.view pattern with
  | Constraint { pattern=Some pattern; annotation=Some annotation } -> Some (pattern, annotation)
  | _ -> None

and parameter_pattern_doc = fun pattern ->
  match Ast.Pattern.view pattern with
  | Record -> compact_record_pattern_doc pattern
  | _ -> pattern_doc pattern

and named_parameter_doc = fun ~sigil label pattern ->
  match split_typed_parameter_pattern pattern with
  | Some (inner, annotation) when parameter_pattern_matches_label label inner -> Doc.concat
    [
      Doc.text sigil;
      Doc.lparen;
      pattern_doc inner;
      Doc.text ":";
      type_expr_doc annotation;
      Doc.rparen;
    ]
  | _ when parameter_pattern_matches_label label pattern -> Doc.concat
    [ Doc.text sigil; token_doc label ]
  | _ -> Doc.concat [ Doc.text sigil; token_doc label; Doc.text ":"; parameter_pattern_doc pattern ]

and parameter_doc = fun parameter ->
  match Ast.Parameter.view parameter with
  | Labeled { label=Some label; pattern=None } ->
      Doc.concat [ Doc.text "~"; token_doc label ]
  | Labeled { label=Some label; pattern=Some pattern } ->
      named_parameter_doc ~sigil:"~" label pattern
  | Labeled _ ->
      unsupported "labeled parameter without label"
  | Optional { label=Some label; pattern=None } ->
      Doc.concat [ Doc.text "?"; token_doc label ]
  | Optional { label=Some label; pattern=Some pattern } ->
      named_parameter_doc ~sigil:"?" label pattern
  | Optional _ ->
      unsupported "optional parameter without label"
  | OptionalDefault { label=Some label; pattern=Some pattern; default=Some default } ->
      let parts =
        if parameter_pattern_matches_label label pattern then
          [
            Doc.text "?";
            Doc.lparen;
            pattern_doc pattern;
            Doc.space;
            Doc.equal;
            Doc.space;
            expr_doc default;
            Doc.rparen;
          ]
        else
          [
            Doc.text "?";
            token_doc label;
            Doc.text ":(";
            pattern_doc pattern;
            Doc.space;
            Doc.equal;
            Doc.space;
            expr_doc default;
            Doc.rparen;
          ]
      in
      Doc.concat parts
  | OptionalDefault _ ->
      unsupported "incomplete optional parameter default"
  | Unknown _ ->
      unsupported "unsupported parameter"

and pattern_breaks_long_match_case = fun pattern ->
  match Ast.Pattern.view pattern with
  | Or _ -> false
  | _ -> node_has_token_kind pattern Kind.DOT && Int.(node_token_text_width pattern > 18)

and expr_case_body_stays_with_arrow_for_long_pattern = fun expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | Parenthesized { inner=None } -> true
  | _ -> expr_case_body_stays_with_arrow expr

and match_case_body_breaks = fun match_case ->
  let view = Ast.MatchCase.view match_case in
  match view.pattern, view.body with
  | pattern, Some body ->
      expr_case_body_breaks body || expr_has_unconsumed_boundary_leading_comment body || (
        (
          match pattern with
          | Some pattern -> pattern_breaks_long_match_case pattern
          | None -> false
        ) && not (expr_case_body_stays_with_arrow_for_long_pattern body)
      )
  | _, None -> false

and match_case_body_forces_group_break = fun match_case ->
  let view = Ast.MatchCase.view match_case in
  match view.body with
  | Some body -> (
      match Ast.Expr.view body with
      | If _
      | Sequence _ ->
          true
      | Parenthesized { inner=Some inner } when not (expr_is_begin_block body) -> (
          match Ast.Expr.view inner with
          | Match _ -> true
          | _ -> false
        )
      | Apply _ ->
          let _callee, arguments = application_parts body in
          (
            match arguments with
            | first :: _ when expr_multiline_apply_argument_stays_with_callee first -> false
            | _ -> Int.(List.length arguments >= 4)
            || Int.(node_token_text_width body > 70)
            || Doc.is_multiline (application_doc body)
          )
      | _ ->
          false
    )
  | None -> false

and match_case_doc = fun ?(force_body_break = false) ?(break_long_pattern = false) match_case ->
  let view = Ast.MatchCase.view match_case in
  match view.pattern, view.body with
  | Some pattern, Some body ->
      let guard =
        match view.guard with
        | Some guard -> Doc.concat [ Doc.space; Doc.text "when"; Doc.space; expr_doc guard ]
        | None -> Doc.empty
      in
      let body_breaks =
        expr_case_body_breaks body
        || (break_long_pattern && match_case_body_breaks match_case)
        || (force_body_break && not (expr_case_body_stays_with_arrow body))
        || expr_has_unconsumed_boundary_leading_comment body in
      let body_doc =
        if body_breaks then
          expr_multiline_body_doc body
        else
          expr_doc_with_boundary_leading_comment body
      in
      let final_case_doc pattern =
        if body_breaks then
          Doc.concat
            [
              Doc.bar;
              Doc.space;
              pattern_doc pattern;
              guard;
              Doc.space;
              Doc.arrow;
              Doc.line;
              Doc.indent 4 body_doc
            ]
        else
          Doc.concat
            [
              Doc.bar;
              Doc.space;
              pattern_doc pattern;
              guard;
              Doc.space;
              Doc.arrow;
              Doc.space;
              body_doc;
            ]
      in
      (
        match or_pattern_items pattern with
        | []
        | [ _ ] -> final_case_doc pattern
        | alternatives -> (
            match List.reverse alternatives with
            | [] -> final_case_doc pattern
            | last :: rest ->
                let prefix_cases = rest
                |> List.reverse
                |> List.map
                  ~fn:(fun pattern -> Doc.concat [ Doc.bar; Doc.space; pattern_doc pattern ]) in
                Doc.join Doc.line (prefix_cases @ [ final_case_doc last ])
          )
      )
  | _ -> unsupported "incomplete match case"

and expr_apply_callee_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Path _
  | FieldAccess _
  | MethodCall _
  | Apply _
  | Parenthesized _ -> expr_doc_with_view expr view
  | _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]

and expr_parens_can_elide = fun expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | PolyVariant { payload=None } -> true
  | Parenthesized { inner=Some inner } -> expr_parens_can_elide inner
  | _ -> false

and expr_parens_can_elide_in_apply_argument = fun expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | PolyVariant { payload=None } -> true
  | Parenthesized { inner=Some inner } -> expr_parens_can_elide_in_apply_argument inner
  | _ -> false

and expr_apply_argument_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Parenthesized { inner=Some inner } when expr_parens_can_elide_in_apply_argument inner -> expr_doc
    inner
  | Parenthesized { inner=Some inner } -> parenthesized_expr_doc_as_apply_argument expr inner
  | Parenthesized { inner=None } -> expr_doc_with_view expr view
  | Tuple when expr_tuple_has_explicit_delimiter expr -> expr_doc_with_view expr view
  | Path _
  | Literal _
  | List
  | Array
  | PolyVariant _
  | LabeledArg _
  | OptionalArg _
  | Record
  | RecordUpdate -> expr_doc_with_view expr view
  | _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]

and expr_apply_argument_doc_with_leading_comment = fun expr ->
  let argument_doc = expr_apply_argument_doc expr in
  match first_descendant_token expr with
  | Some token when token_has_inline_leading_comment token -> Doc.concat
    [ inline_leading_comment_token_doc token; Doc.space; argument_doc ]
  | Some token when Ast.Token.has_leading_comment token -> Doc.concat
    [ trimmed_leading_comment_token_doc token; Doc.line; argument_doc ]
  | _ -> argument_doc

and application_parts = fun expr ->
  let rec append left right =
    match left with
    | [] -> right
    | item :: rest -> item :: append rest right
  in
  let rec collect expr args =
    match Ast.Expr.view expr with
    | Apply _ -> (
        match expr_child_exprs expr with
        | callee :: arguments -> collect callee (append arguments args)
        | [] -> (expr, args)
      )
    | _ -> (expr, args)
  in
  collect expr []

and expr_is_labeled_argument = fun expr ->
  match Ast.Expr.view expr with
  | LabeledArg _
  | OptionalArg _ -> true
  | _ -> false

and expr_prefers_application_head = fun expr ->
  match Ast.Expr.view expr with
  | Path _
  | Literal _
  | PolyVariant { payload=None }
  | Parenthesized { inner=None } -> true
  | _ -> false

and application_has_many_labeled_arguments = fun arguments ->
  Int.(List.length arguments >= 4) && List.all arguments ~fn:expr_is_labeled_argument

and application_should_break_arguments = fun expr arguments ->
  let _ = expr in
  let _ = arguments in
  false

and split_before_first_multiline_doc = fun docs ->
  let rec loop prefix docs =
    match docs with
    | [] -> (List.reverse prefix, [])
    | doc :: rest when Doc.is_multiline doc -> (List.reverse prefix, doc :: rest)
    | doc :: rest -> loop (doc :: prefix) rest
  in
  loop [] docs

and expr_is_capitalized_path = fun expr ->
  match Ast.Expr.view expr with
  | Path { path } -> (
      match Ast.Path.last_ident path with
      | Some token ->
          let text = Ast.Token.text token in
          Int.(String.length text > 0) && (
            match String.get_unchecked text ~at:0 with
            | 'A' .. 'Z' -> true
            | _ -> false
          )
      | None -> false
    )
  | _ -> false

and expr_is_record_expr = fun expr ->
  match Ast.Expr.view expr with
  | Record
  | RecordUpdate -> true
  | _ -> false

and constructor_record_application_breaks = fun argument -> Int.(node_token_text_width argument > 45)

and expr_is_constructor_record_application = fun expr ->
  match Ast.Expr.view expr with
  | Apply _ ->
      let callee, arguments = application_parts expr in
      expr_is_capitalized_path callee && (
        match arguments with
        | [ argument ] -> expr_is_record_expr argument && constructor_record_application_breaks argument
        | _ -> false
      )
  | _ -> false

and constructor_record_body_doc = fun argument ->
  match record_expr_field_docs argument with
  | [] -> Doc.empty
  | fields -> Doc.concat
    [ Doc.line; Doc.indent 2 (Doc.join (Doc.concat [ Doc.semi; Doc.line ]) fields); Doc.line; ]

and constructor_record_application_doc = fun callee argument ->
  Doc.concat
    [
      expr_apply_callee_doc callee;
      Doc.space;
      Doc.lbrace;
      constructor_record_body_doc argument;
      Doc.rbrace;
    ]

and expr_multiline_apply_argument_stays_with_callee = fun expr ->
  match Ast.Expr.view expr with
  | PolyVariant { payload=Some _ } ->
      true
  | LocalOpen _ -> (
      match Ast.LocalOpenExpr.cast expr with
      | Some local_open -> (
          match Ast.LocalOpenExpr.view local_open with
          | Delimited { module_path=None; body=Some body; _ } -> (
              match Ast.Expr.view body with
              | Record
              | RecordUpdate -> true
              | _ -> false
            )
          | _ -> false
        )
      | None -> false
    )
  | Parenthesized { inner=Some inner } -> (
      match Ast.Expr.view inner with
      | PolyVariant { payload=Some _ } -> true
      | Match _ -> true
      | Infix { operator=Some operator; _ } when (String.equal (infix_operator_text inner operator) "^"
      || String.equal (infix_operator_text inner operator) "::")
      && Int.(node_token_text_width inner > 70) -> true
      | Apply _ when expr_is_constructor_record_application inner -> true
      | LocalOpen _ -> expr_multiline_apply_argument_stays_with_callee inner
      | _ -> false
    )
  | _ ->
      false

and expr_multiline_apply_argument_breaks_after_callee = fun expr ->
  match Ast.Expr.view expr with
  | Parenthesized { inner=Some inner } -> (
      match Ast.Expr.view inner with
      | Infix { operator=Some operator; _ } when String.equal (infix_operator_text inner operator) "^"
      && Int.(node_token_text_width inner > 70) -> true
      | _ -> false
    )
  | _ -> false

and application_breaks_after_equal = fun expr inline_body_doc ->
  if not (Doc.is_multiline inline_body_doc) then
    false
  else
    match Ast.Expr.view expr with
    | Apply _ ->
        let _, arguments = application_parts expr in
        if application_has_many_labeled_arguments arguments then
          false
        else
          (
            match arguments with
            | first :: _ when expr_multiline_apply_argument_stays_with_callee first ->
                false
            | [ first ] -> (
                match Ast.Expr.view first with
                | List
                | Array -> false
                | _ -> Doc.is_multiline (expr_apply_argument_doc_with_leading_comment first)
              )
            | first :: _ ->
                Doc.is_multiline (expr_apply_argument_doc_with_leading_comment first)
            | [] ->
                false
          )
    | _ -> false

and application_doc = fun ?(prefer_first_argument_head = false) expr ->
  let callee, arguments = application_parts expr in
  match arguments with
  | [] ->
      expr_doc callee
  | [ argument ] when expr_is_capitalized_path callee
  && expr_is_record_expr argument
  && constructor_record_application_breaks argument ->
      constructor_record_application_doc callee argument
  | arguments ->
      let callee_doc = expr_apply_callee_doc callee in
      let argument_docs = List.map arguments ~fn:expr_apply_argument_doc_with_leading_comment in
      if
        application_has_many_labeled_arguments arguments || application_should_break_arguments expr arguments
      then
        Doc.concat [ callee_doc; Doc.line; Doc.indent 2 (Doc.join Doc.line argument_docs) ]
      else
        match arguments, argument_docs with
        | first_arg :: _, first_doc :: rest_docs when Doc.is_multiline first_doc
        && expr_multiline_apply_argument_breaks_after_callee first_arg ->
            Doc.concat
              [ callee_doc; Doc.line; Doc.indent 2 (Doc.join Doc.line (first_doc :: rest_docs)) ]
        | first_arg :: _, first_doc :: rest_docs when Doc.is_multiline first_doc
        && expr_multiline_apply_argument_stays_with_callee first_arg ->
            Doc.concat
              [ callee_doc; Doc.space; first_doc; (
                  match rest_docs with
                  | [] -> Doc.empty
                  | rest_docs -> Doc.concat [ Doc.line; Doc.indent 2 (Doc.join Doc.line rest_docs) ]
                ); ]
        | _ when List.any argument_docs ~fn:Doc.is_multiline ->
            let inline_docs, multiline_docs = split_before_first_multiline_doc argument_docs in
            let head_doc =
              match inline_docs with
              | [] -> callee_doc
              | docs -> Doc.concat [ callee_doc; Doc.space; Doc.join Doc.space docs ]
            in
            Doc.concat [ head_doc; Doc.line; Doc.indent 2 (Doc.join Doc.line multiline_docs) ]
        | first_arg :: rest_args, first_doc :: rest_docs when prefer_first_argument_head
        && (not (List.is_empty rest_args))
        && expr_prefers_application_head first_arg ->
            Doc.group
              (Doc.concat
                [
                  callee_doc;
                  Doc.space;
                  first_doc;
                  Doc.indent 2 (Doc.concat [ Doc.break (); Doc.join (Doc.break ()) rest_docs ]);
                ])
        | _ ->
            Doc.group
              (Doc.concat
                [
                  callee_doc;
                  Doc.indent 2 (Doc.concat [ Doc.break (); Doc.join (Doc.break ()) argument_docs ]);
                ])

and token_text_equal = fun left right ->
  String.equal (Ast.Token.text left) (Ast.Token.text right)

and collect_same_infix_chain = fun operator_text expr acc ->
  match Ast.Expr.view expr with
  | Infix { left=Some left; operator=Some next_operator; right=Some right } when String.equal
    operator_text
    (infix_operator_text expr next_operator) ->
      let acc = collect_same_infix_chain operator_text left acc in
      collect_same_infix_chain operator_text right acc
  | _ -> expr :: acc

and same_infix_chain = fun operator expr ->
  collect_same_infix_chain (infix_operator_text expr operator) expr [] |> List.reverse

and infix_chain_doc = fun ~minimum_parts expr ->
  match Ast.Expr.view expr with
  | Infix { operator=Some operator; _ } -> (
      let parts = same_infix_chain operator expr in
      let operator_doc = infix_operator_doc expr operator in
      if Int.(List.length parts < minimum_parts) then
        None
      else
        match parts with
        | [] -> None
        | first :: rest -> Some (Doc.concat
          (expr_infix_operand_doc first
          :: (rest
          |> List.map
            ~fn:(fun part -> [ Doc.line; operator_doc; Doc.space; expr_infix_operand_doc part ])
          |> List.concat)))
    )
  | _ -> None

and large_infix_chain_doc = fun expr -> infix_chain_doc ~minimum_parts:9 expr

and expr_infix_inline_doc = fun expr ->
  match Ast.Expr.view expr with
  | Infix { left=Some left; operator=Some operator; right=Some right } -> Doc.concat
    [
      expr_infix_inline_operand_doc left;
      Doc.space;
      infix_operator_doc expr operator;
      Doc.space;
      expr_infix_inline_operand_doc right;
    ]
  | _ -> expr_doc expr

and expr_infix_inline_operand_doc = fun expr ->
  match Ast.Expr.view expr with
  | Infix _ -> expr_infix_inline_doc expr
  | _ -> expr_infix_operand_doc expr

and expr_list_item_doc = fun ~singleton expr ->
  match singleton, Ast.Expr.view expr with
  | true, Infix { operator=Some operator; _ } when String.equal (infix_operator_text expr operator) "|>" -> expr_infix_inline_doc
    expr
  | _ -> expr_doc expr

and function_binding_body_doc = fun expr ->
  match Ast.Expr.view expr with
  | Function { first_case=Some _ } -> Some (Doc.concat
    [ Doc.text "function"; Doc.line; Doc.indent 2 (match_cases_doc expr) ])
  | _ -> None

and expr_is_begin_block = fun expr ->
  match Ast.Node.first_child_token expr ~kind:Kind.BEGIN_KW with
  | Some _ -> true
  | None -> false

and expr_tuple_has_explicit_delimiter = fun expr ->
  match Ast.Node.first_child_token expr ~kind:Kind.LPAREN, Ast.Node.first_child_token
    expr
    ~kind:Kind.BEGIN_KW with
  | (Some _, _)
  | (_, Some _) -> true
  | None, None -> false

and let_module_expr_is_multiline = fun expr ->
  match Ast.LetModuleExpr.cast expr with
  | Some module_expr -> Doc.is_multiline (let_module_body_doc module_expr)
  | None -> false

and binding_operator_body_breaks = fun expr ->
  match Ast.BindingOperatorExpr.cast expr with
  | Some view -> (
      let clause_breaks = ref false in
      Ast.BindingOperatorExpr.for_each_clause view
        ~fn:(fun clause ->
          match Ast.LetBinding.body clause.binding with
          | Some body when Doc.is_multiline (let_binding_doc clause.binding)
          || expr_binding_body_breaks_after_equal body
          || expr_has_unconsumed_boundary_leading_comment body -> clause_breaks := true
          | _ -> ());
      match Ast.BindingOperatorExpr.body view with
      | Some body -> !clause_breaks
      || expr_binding_body_breaks_after_equal body
      || expr_has_unconsumed_boundary_leading_comment body
      | None -> !clause_breaks
    )
  | None -> false

and expr_binding_body_breaks_after_equal = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | Assign _ -> true
  | BindingOperator _ -> binding_operator_body_breaks expr
  | LocalOpen _ -> Doc.is_multiline (local_open_expr_doc expr)
  | LetModule _ -> let_module_expr_is_multiline expr
  | Parenthesized _ -> expr_is_begin_block expr
  | _ -> false

and expr_parenthesized_inner_breaks_after_equal = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | Function _ -> true
  | _ -> false

and expr_arrow_body_breaks = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | LetException _ ->
      true
  | Apply _ ->
      let _callee, arguments = application_parts expr in
      (Int.(List.length arguments >= 4) && Int.(node_token_text_width expr > 60))
      || Int.(node_token_text_width expr > 80)
      || Doc.is_multiline (application_doc expr)
  | LocalOpen _ ->
      Doc.is_multiline (local_open_expr_doc expr)
  | Parenthesized _ ->
      expr_is_begin_block expr
  | _ ->
      false

and expr_try_body_breaks = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | LetException _ -> true
  | Parenthesized _ -> expr_is_begin_block expr
  | _ -> false

and expr_case_body_breaks = fun expr ->
  match Ast.Expr.view expr with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | LetException _ ->
      true
  | Literal { token=Some token } ->
      String.contains (Ast.Token.text token) "\n"
  | Apply _ ->
      let _callee, arguments = application_parts expr in
      (
        match arguments with
        | first :: _ when expr_multiline_apply_argument_stays_with_callee first -> false
        | _ -> Int.(List.length arguments >= 4)
        || Int.(node_token_text_width expr > 70)
        || Doc.is_multiline (application_doc expr)
      )
  | Parenthesized _ when expr_is_begin_block expr ->
      true
  | _ ->
      false

and expr_case_body_stays_with_arrow = fun expr ->
  match Ast.Expr.view expr with
  | Literal _ ->
      true
  | Parenthesized { inner=None } ->
      true
  | Parenthesized { inner=Some inner } when not (expr_is_begin_block expr) -> (
      match Ast.Expr.view inner with
      | Match _ -> true
      | _ -> false
    )
  | Apply _ ->
      let _callee, arguments = application_parts expr in
      (
        match arguments with
        | first :: _ -> expr_multiline_apply_argument_stays_with_callee first
        | [] -> false
      )
  | _ ->
      false

and expr_loop_body_breaks = fun expr ->
  match Ast.Expr.view expr with
  | Assign _
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | LetException _
  | BindingOperator _ ->
      true
  | Apply _ ->
      let _callee, arguments = application_parts expr in
      Int.(List.length arguments >= 2)
      || Int.(node_token_text_width expr > 70)
      || Doc.is_multiline (application_doc expr)
  | Parenthesized _ ->
      expr_is_begin_block expr
  | _ ->
      false

and expr_infix_operand_doc = fun expr ->
  let view = Ast.Expr.view expr in
  match view with
  | Tuple when expr_tuple_has_explicit_delimiter expr -> expr_doc_with_view expr view
  | Tuple
  | Sequence _
  | Let _
  | LocalOpen _
  | LetModule _
  | LetException _
  | BindingOperator _
  | If _
  | Match _
  | Fun _
  | Function _
  | Try _
  | While _
  | For _ -> Doc.concat [ Doc.lparen; expr_doc_with_view expr view; Doc.rparen ]
  | _ -> expr_doc_with_view expr view

and expr_prefix_operand_doc = fun expr ->
  match Ast.Expr.view expr with
  | Parenthesized { inner=Some inner } -> parenthesized_expr_doc expr inner
  | _ -> expr_doc expr

and expr_doc = fun expr -> expr_doc_with_view expr (Ast.Expr.view expr)

and expr_consumes_boundary_leading_comment = fun expr ->
  match Ast.Expr.view expr with
  | If _ -> true
  | _ -> false

and expr_has_unconsumed_boundary_leading_comment = fun expr ->
  match Ast.Node.first_descendant_token expr with
  | Some token when Ast.Token.has_leading_comment token
  && not (expr_consumes_boundary_leading_comment expr) -> true
  | _ -> false

and expr_doc_with_boundary_leading_comment = fun expr ->
  match Ast.Node.first_descendant_token expr with
  | Some token when expr_has_unconsumed_boundary_leading_comment expr -> Doc.concat
    [ trimmed_leading_comment_token_doc token; Doc.line; expr_doc expr ]
  | _ -> expr_doc expr

and expr_doc_with_boundary_leading_comment_doc = fun expr body_doc ->
  match Ast.Node.first_descendant_token expr with
  | Some token when expr_has_unconsumed_boundary_leading_comment expr -> Doc.concat
    [ trimmed_leading_comment_token_doc token; Doc.line; body_doc ]
  | _ -> body_doc

and expr_infix_right_operand_doc = fun expr ->
  let operand_doc = expr_infix_operand_doc expr in
  match first_descendant_token expr with
  | Some token when token_has_inline_leading_comment token -> Doc.concat
    [ inline_leading_comment_token_doc token; Doc.space; operand_doc ]
  | Some token when Ast.Token.has_leading_comment token -> Doc.concat
    [ trimmed_leading_comment_token_doc token; Doc.line; operand_doc ]
  | _ -> operand_doc

and parenthesized_expr_multiline_body_doc = fun inner ->
  Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 (expr_doc inner); Doc.line; Doc.rparen ]

and expr_multiline_body_doc = fun expr ->
  match Ast.Expr.view expr with
  | LetException _ ->
      let_exception_expr_doc_with_body_break expr
  | Parenthesized { inner=Some inner } when (not (expr_is_begin_block expr))
  && expr_parenthesized_inner_breaks_after_equal inner ->
      parenthesized_expr_multiline_body_doc inner
  | Infix { operator=Some operator; _ } when String.equal (infix_operator_text expr operator) "|>" -> (
      match infix_chain_doc ~minimum_parts:2 expr with
      | Some body_doc -> expr_doc_with_boundary_leading_comment_doc expr body_doc
      | None -> expr_doc_with_boundary_leading_comment expr
    )
  | _ ->
      expr_doc_with_boundary_leading_comment expr

and expr_has_leading_comment = fun expr ->
  match Ast.Node.first_descendant_token expr with
  | Some token -> Ast.Token.has_leading_comment token
  | None -> false

and sequence_item_doc = fun ?(skip_boundary_leading_comment = false) expr ->
  match Ast.Expr.view expr with
  | Parenthesized { inner=Some inner } when (not (expr_is_begin_block expr))
  && expr_parenthesized_inner_breaks_after_equal inner -> parenthesized_expr_multiline_body_doc inner
  | Apply _ when skip_boundary_leading_comment -> application_doc ~prefer_first_argument_head:true expr
  | Apply _ -> expr_doc_with_boundary_leading_comment_doc
    expr
    (application_doc ~prefer_first_argument_head:true expr)
  | _ when skip_boundary_leading_comment -> expr_doc expr
  | _ -> expr_doc_with_boundary_leading_comment expr

and sequence_separator_consumes_leading_comment = fun expr ->
  match Ast.Node.first_child_token expr ~kind:Kind.SEMI with
  | Some separator -> token_has_inline_leading_comment_after_horizontal separator
  | None -> false

and sequence_separator_suffix_doc = fun expr ->
  match Ast.Node.first_child_token expr ~kind:Kind.SEMI with
  | Some separator when token_has_inline_leading_comment_after_horizontal separator -> (
      match leading_comments separator with
      | comment :: _ -> Doc.concat [ Doc.semi; Doc.line; Doc.text comment.text; Doc.line ]
      | [] -> Doc.concat [ Doc.semi; Doc.line ]
    )
  | _ -> Doc.concat [ Doc.semi; Doc.line ]

and sequence_doc = fun expr ->
  match Ast.Expr.view expr with
  | Sequence { left=Some left; right=Some right } -> Doc.concat
    [
      sequence_doc left;
      sequence_separator_suffix_doc expr;
      sequence_item_doc
        ~skip_boundary_leading_comment:(sequence_separator_consumes_leading_comment expr)
        right;
    ]
  | _ -> sequence_item_doc expr

and match_cases_doc = fun expr ->
  let cases = ref [] in
  Ast.Expr.for_each_match_case expr ~fn:(fun match_case -> cases := match_case :: !cases);
  let cases = List.reverse !cases in
  let force_body_break =
    match Ast.Expr.view expr with
    | Function _ -> List.any cases ~fn:match_case_body_forces_group_break
    | _ -> false
  in
  let break_long_pattern =
    match Ast.Expr.view expr with
    | Match _
    | Try _ -> true
    | _ -> false
  in
  cases |> List.map ~fn:(match_case_doc ~force_body_break ~break_long_pattern) |> Doc.join Doc.line

and parenthesized_expr_doc = fun expr inner ->
  if expr_is_begin_block expr then
    Doc.concat
      [ Doc.text "begin"; Doc.line; Doc.indent 2 (expr_doc inner); Doc.line; Doc.text "end" ]
  else
    let inner_doc = expr_doc_with_boundary_leading_comment inner in
    if expr_has_leading_comment inner then
      Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
    else if Doc.is_multiline inner_doc && (
        match Ast.Expr.view inner with
        | Typed _ -> true
        | _ -> false
      ) then
      Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
    else
      match Ast.Expr.view inner with
      | If _
      | Let _
      | Match _
      | Try _
      | Sequence _ ->
          Doc.concat
            [ Doc.lparen; Doc.line; Doc.indent 4 inner_doc; Doc.line; Doc.indent 2 Doc.rparen ]
      | Function _ ->
          Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
      | Prefix { operator=Some operator; operand=Some operand } -> (
          match Ast.Expr.view operand with
          | Literal { token=Some token } when String.equal (prefix_operator_text inner operator) "-" -> Doc.concat
            [ Doc.lparen; prefix_operator_doc inner operator; literal_token_doc token; Doc.rparen ]
          | _ -> Doc.concat [ Doc.lparen; inner_doc; Doc.rparen ]
        )
      | _ ->
          Doc.concat [ Doc.lparen; inner_doc; Doc.rparen ]

and parenthesized_expr_doc_as_apply_argument = fun expr inner ->
  let inner_doc = expr_doc_with_boundary_leading_comment inner in
  match Ast.Expr.view inner with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _ -> Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
  | _ -> parenthesized_expr_doc expr inner

and if_keyword_doc = fun token ->
  if Ast.Token.has_leading_comment token then
    Doc.concat [ trimmed_leading_comment_token_doc token; Doc.line; token_doc token ]
  else
    token_doc token

and if_then_branch_doc = fun then_branch else_token ->
  let branch_doc = expr_doc_with_boundary_leading_comment then_branch in
  match else_token with
  | Some token when Ast.Token.has_leading_comment token -> Doc.concat
    [ branch_doc; Doc.line; trimmed_leading_comment_token_doc token ]
  | _ -> branch_doc

and parenthesized_expr_doc_after_keyword = fun expr inner ->
  let inner_doc = expr_doc_with_boundary_leading_comment inner in
  match Ast.Expr.view inner with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _ -> Doc.concat [ Doc.lparen; Doc.line; Doc.indent 2 inner_doc; Doc.line; Doc.rparen ]
  | _ -> parenthesized_expr_doc expr inner

and tuple_expr_doc = fun expr ->
  let items = child_expr_docs expr |> List.map ~fn:expr_doc in
  let inline_items = Doc.join (Doc.concat [ Doc.comma; Doc.space ]) items in
  let multiline_items = Doc.join (Doc.concat [ Doc.comma; Doc.line ]) items in
  match Ast.Node.first_child_token expr ~kind:Kind.LPAREN, Ast.Node.first_child_token
    expr
    ~kind:Kind.BEGIN_KW with
  | _, Some _ -> Doc.concat
    [ Doc.text "begin"; Doc.line; Doc.indent 2 inline_items; Doc.line; Doc.text "end" ]
  | Some _, _ when List.any items ~fn:Doc.is_multiline || Int.(node_token_text_width expr > 70) -> Doc.concat
    [ Doc.lparen; Doc.line; Doc.indent 2 multiline_items; Doc.line; Doc.rparen ]
  | Some _, _ -> Doc.concat [ Doc.lparen; inline_items; Doc.rparen ]
  | None, None -> inline_items

and if_else_branch_doc = fun else_token else_branch ->
  match Ast.Expr.view else_branch with
  | If _ when not (Ast.Token.has_leading_comment else_token)
  && not (expr_has_leading_comment else_branch) -> Doc.concat
    [ token_doc else_token; Doc.space; expr_doc else_branch ]
  | Parenthesized { inner=Some inner } when not (Ast.Token.has_leading_comment else_token)
  && not (expr_has_leading_comment else_branch)
  && not (expr_is_begin_block else_branch) -> Doc.concat
    [ token_doc else_token; Doc.space; parenthesized_expr_doc_after_keyword else_branch inner ]
  | _ -> Doc.concat [ token_doc else_token; Doc.line; Doc.indent 2 (expr_doc else_branch) ]

and if_expr_doc = fun expr condition then_branch else_branch ->
  match Ast.Node.first_child_token expr ~kind:Kind.IF_KW, Ast.Node.first_child_token
    expr
    ~kind:Kind.THEN_KW with
  | Some if_token, Some then_token ->
      let else_token = Ast.Node.first_child_token expr ~kind:Kind.ELSE_KW in
      let condition_doc = expr_doc condition in
      let condition_breaks =
        Doc.is_multiline condition_doc || Int.(node_token_text_width condition > 70) in
      let head =
        Doc.concat
          (
            if condition_breaks then
              [
                if_keyword_doc if_token;
                Doc.line;
                Doc.indent 2 condition_doc;
                Doc.line;
                token_doc then_token;
                Doc.line;
                Doc.indent 2 (if_then_branch_doc then_branch else_token);
              ]
            else
              [
                if_keyword_doc if_token;
                Doc.space;
                condition_doc;
                Doc.space;
                token_doc then_token;
                Doc.line;
                Doc.indent 2 (if_then_branch_doc then_branch else_token);
              ]
          )
      in
      (
        match else_token, else_branch with
        | Some else_token, Some else_branch -> Doc.concat
          [ head; Doc.line; if_else_branch_doc else_token else_branch ]
        | None, None -> head
        | _ -> unsupported "incomplete if expression"
      )
  | _ -> unsupported "if expression without keyword tokens"

and assignment_operator_doc = fun expr ->
  match direct_child_tokens expr
  |> List.find
    ~fn:(fun token -> token_kind_is token Kind.LEFT_ARROW || token_kind_is token Kind.COLONEQ) with
  | Some token -> token_doc token
  | None -> Doc.text "<-"

and expr_doc_with_view = fun expr (view: Ast.Expr.view) ->
  match view with
  | Path { path } ->
      path_doc path
  | Literal { token=Some token } ->
      literal_token_doc token
  | Literal { token=None } ->
      unsupported "literal expression without token"
  | Parenthesized { inner=Some inner } when expr_parens_can_elide inner
  && not (expr_has_leading_comment inner) ->
      expr_doc inner
  | Parenthesized { inner=Some inner } ->
      parenthesized_expr_doc expr inner
  | Parenthesized { inner=None } ->
      if expr_is_begin_block expr then
        Doc.concat [ Doc.text "begin"; Doc.space; Doc.text "end" ]
      else
        parenthesized_token_shell_doc (direct_child_tokens expr)
  | Infix { left=Some left; operator=Some operator; right=Some right } ->
      let operator_text = infix_operator_text expr operator in
      let chain_doc =
        if
          String.equal operator_text "^"
          && Int.(List.length (same_infix_chain operator expr) >= 4)
          && Int.(node_token_text_width expr > 70)
        then
          infix_chain_doc ~minimum_parts:4 expr
        else if String.equal operator_text "|>" then
          let parts = same_infix_chain operator expr in
          let first_part_is_wide =
            match parts with
            | first :: _ -> Int.(node_token_text_width first > 45)
            | [] -> false
          in
          if
            Int.(node_token_text_width expr > 65)
            && (Int.(List.length parts >= 3) || first_part_is_wide)
          then
            infix_chain_doc ~minimum_parts:2 expr
          else
            None
        else
          None
      in
      (
        match chain_doc with
        | Some doc -> doc
        | None -> Doc.concat
          [
            expr_infix_operand_doc left;
            Doc.space;
            infix_operator_doc expr operator;
            Doc.space;
            expr_infix_right_operand_doc right;
          ]
      )
  | Infix _ ->
      unsupported "incomplete infix expression"
  | Prefix { operator=Some operator; operand=Some operand } -> (
      match Ast.Expr.view operand with
      | Literal { token=Some token } when String.equal (prefix_operator_text expr operator) "-" -> Doc.concat
        [ Doc.lparen; prefix_operator_doc expr operator; literal_token_doc token; Doc.rparen ]
      | Prefix _ when String.equal (prefix_operator_text expr operator) "-" -> Doc.concat
        [ prefix_operator_doc expr operator; Doc.space; expr_prefix_operand_doc operand ]
      | _ -> Doc.concat
        [ prefix_operator_doc_with_explicit_spacing expr operator; expr_prefix_operand_doc operand ]
    )
  | Prefix _ ->
      unsupported "incomplete prefix expression"
  | Apply { callee=Some _; argument=Some _ } ->
      application_doc expr
  | Apply _ ->
      unsupported "incomplete apply expression"
  | Typed { expr=Some expr; annotation=Some annotation } ->
      let annotation_doc = type_expr_doc_with_arrow_threshold ~threshold:2 annotation in
      if Doc.is_multiline annotation_doc then
        Doc.concat [ expr_doc expr; Doc.text ":"; Doc.line; Doc.indent 2 annotation_doc ]
      else
        Doc.concat [ expr_doc expr; Doc.text ":"; Doc.space; annotation_doc ]
  | Typed _ ->
      unsupported "incomplete typed expression"
  | If { condition=Some condition; then_branch=Some then_branch; else_branch=Some else_branch } ->
      if_expr_doc expr condition then_branch (Some else_branch)
  | If { condition=Some condition; then_branch=Some then_branch; else_branch=None } ->
      if_expr_doc expr condition then_branch None
  | If _ ->
      unsupported "incomplete if expression"
  | Tuple ->
      tuple_expr_doc expr
  | List ->
      Doc.concat [ Doc.lbracket; list_expr_body_doc expr; Doc.rbracket ]
  | Array ->
      Doc.concat [ Doc.text "[|"; array_expr_body_doc expr; Doc.text "|]" ]
  | Record
  | RecordUpdate ->
      record_expr_doc expr
  | Sequence { left=Some _; right=Some _ } ->
      sequence_doc expr
  | Sequence _ ->
      unsupported "incomplete sequence expression"
  | Let { first_binding=Some _; body=Some body } ->
      let bindings, last_binding_is_multiline, last_binding_keeps_in = let_bindings_block_doc
        ~keyword:"let"
        ~rec_token:(Ast.Node.first_child_token expr ~kind:Kind.REC_KW)
        expr in
      let body_doc =
        match Ast.Expr.view body with
        | Sequence _ -> expr_doc body
        | _ -> expr_doc_with_boundary_leading_comment body
      in
      Doc.concat
        [ bindings; (
            if last_binding_is_multiline && not last_binding_keeps_in then
              Doc.line
            else
              Doc.space
          ); Doc.text "in"; Doc.line; body_doc; ]
  | Let _ ->
      unsupported "incomplete let expression"
  | Fun { body=Some body } -> (
      match direct_pattern_docs expr with
      | [] -> unsupported "function expression without parameters"
      | parameters ->
          let head = Doc.concat
            [
              Doc.text "fun";
              Doc.space;
              Doc.join Doc.space (List.map parameters ~fn:pattern_doc);
              Doc.space;
              Doc.arrow;
            ] in
          let body_breaks =
            expr_arrow_body_breaks body || expr_has_unconsumed_boundary_leading_comment body in
          let body_doc =
            if body_breaks then
              expr_multiline_body_doc body
            else
              expr_doc_with_boundary_leading_comment body
          in
          if body_breaks then
            Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
          else
            Doc.concat [ head; Doc.space; body_doc ]
    )
  | Fun _ ->
      unsupported "incomplete function expression"
  | Match { scrutinee=Some scrutinee; first_case=Some _ } ->
      Doc.concat
        [
          Doc.text "match";
          Doc.space;
          expr_doc scrutinee;
          Doc.space;
          Doc.text "with";
          Doc.line;
          match_cases_doc expr;
        ]
  | Match _ ->
      unsupported "incomplete match expression"
  | Function { first_case=Some _ } ->
      Doc.concat [ Doc.text "function"; Doc.line; match_cases_doc expr ]
  | Function _ ->
      unsupported "incomplete function expression"
  | Try { body=Some body; first_case=Some _ } ->
      let body_separator =
        if expr_try_body_breaks body then
          Doc.concat [ Doc.line; Doc.indent 2 (expr_doc body); Doc.line ]
        else
          Doc.concat [ Doc.space; expr_doc body; Doc.space ]
      in
      Doc.concat [ Doc.text "try"; body_separator; Doc.text "with"; Doc.line; match_cases_doc expr ]
  | Try _ ->
      unsupported "incomplete try expression"
  | While { condition=Some condition; body=Some body } ->
      let body_doc = expr_doc body in
      if expr_loop_body_breaks body || Doc.is_multiline body_doc then
        Doc.concat
          [
            Doc.text "while";
            Doc.space;
            expr_doc condition;
            Doc.space;
            Doc.text "do";
            Doc.line;
            Doc.indent 2 body_doc;
            Doc.line;
            Doc.text "done";
          ]
      else
        Doc.concat
          [
            Doc.text "while";
            Doc.space;
            expr_doc condition;
            Doc.space;
            Doc.text "do";
            Doc.space;
            body_doc;
            Doc.space;
            Doc.text "done";
          ]
  | While _ ->
      unsupported "incomplete while expression"
  | For { pattern=Some pattern; start_=Some start_; stop=Some stop; body=Some body } ->
      let direction =
        match Ast.Node.first_child_token expr ~kind:Kind.DOWNTO_KW with
        | Some token -> token_doc token
        | None -> Doc.text "to"
      in
      let head = Doc.concat
        [
          Doc.text "for";
          Doc.space;
          pattern_doc pattern;
          Doc.space;
          Doc.equal;
          Doc.space;
          expr_doc start_;
          Doc.space;
          direction;
          Doc.space;
          expr_doc stop;
          Doc.space;
          Doc.text "do";
        ]
      in
      let body_doc = expr_doc body in
      if expr_loop_body_breaks body || Doc.is_multiline body_doc then
        Doc.concat [ head; Doc.line; Doc.indent 2 body_doc; Doc.line; Doc.text "done" ]
      else
        Doc.concat [ head; Doc.space; body_doc; Doc.space; Doc.text "done" ]
  | For _ ->
      unsupported "incomplete for expression"
  | Assert { argument=Some argument } ->
      Doc.concat [ Doc.text "assert"; Doc.space; expr_doc argument ]
  | Assert _ ->
      unsupported "assert expression without argument"
  | Lazy { argument=Some argument } ->
      Doc.concat [ Doc.text "lazy"; Doc.space; expr_doc argument ]
  | Lazy _ ->
      unsupported "lazy expression without argument"
  | Assign { target=Some target; value=Some value } ->
      Doc.concat
        [ expr_doc target; Doc.space; assignment_operator_doc expr; Doc.space; expr_doc value ]
  | Assign _ ->
      unsupported "incomplete assignment expression"
  | FieldAccess { target=Some target; field=Some field } ->
      Doc.concat [ expr_doc target; Doc.text "."; token_doc field ]
  | FieldAccess _ ->
      unsupported "incomplete field access expression"
  | MethodCall { target=Some target; method_=Some method_ } ->
      Doc.concat [ expr_doc target; Doc.text "#"; token_doc method_ ]
  | MethodCall _ ->
      unsupported "incomplete method call expression"
  | PolyVariant { payload } ->
      let head =
        match first_ident_token expr with
        | Some tag -> Doc.concat [ Doc.text "`"; token_doc tag ]
        | None -> unsupported "polymorphic variant expression without tag"
      in
      (
        match payload with
        | Some payload -> Doc.concat [ head; Doc.space; expr_doc payload ]
        | None -> head
      )
  | ArrayIndex { target=Some target; index=Some index } ->
      let opening_doc, closing_doc = index_shell_docs expr ~fallback_open:".(" ~fallback_close:")" in
      if expr_is_operator_word_path index then
        Doc.concat
          [ expr_doc target; opening_doc; Doc.space; expr_doc index; Doc.space; closing_doc ]
      else
        Doc.concat [ expr_doc target; opening_doc; expr_doc index; closing_doc ]
  | ArrayIndex _ ->
      unsupported "incomplete array index expression"
  | StringIndex { target=Some target; index=Some index } ->
      let opening_doc, closing_doc = index_shell_docs expr ~fallback_open:".[" ~fallback_close:"]" in
      Doc.concat [ expr_doc target; opening_doc; expr_doc index; closing_doc ]
  | StringIndex _ ->
      unsupported "incomplete string index expression"
  | LabeledArg { label=Some label; value=None } ->
      Doc.concat [ Doc.text "~"; token_doc label ]
  | LabeledArg { label=Some label; value=Some value } ->
      Doc.concat [ Doc.text "~"; token_doc label; Doc.text ":"; expr_apply_argument_doc value ]
  | LabeledArg _ ->
      unsupported "labeled argument without label"
  | OptionalArg { label=Some label; value=None } ->
      Doc.concat [ Doc.text "?"; token_doc label ]
  | OptionalArg { label=Some label; value=Some value } ->
      Doc.concat [ Doc.text "?"; token_doc label; Doc.text ":"; expr_apply_argument_doc value ]
  | OptionalArg _ ->
      unsupported "optional argument without label"
  | LocalOpen _ ->
      local_open_expr_doc expr
  | LetModule _ ->
      let_module_expr_doc expr
  | LetException _ ->
      let_exception_expr_doc expr
  | FirstClassModule ->
      first_class_module_expr_doc expr
  | Unreachable -> (
      match Ast.UnreachableExpr.cast expr with
      | Some unreachable -> (
          match Ast.UnreachableExpr.dot_token unreachable with
          | Some dot -> token_doc dot
          | None -> unsupported "unreachable expression without dot token"
        )
      | None -> unsupported "unsupported unreachable expression"
    )
  | Attribute { inner=Some _ } ->
      attribute_expr_doc expr
  | Attribute _ ->
      unsupported "attribute expression without inner expression"
  | Extension ->
      extension_expr_doc expr
  | Object
  | New
  | Error _
  | Unknown _ ->
      unsupported "unsupported expression"
  | BindingOperator _ ->
      binding_operator_expr_doc expr

and attribute_expr_doc = fun expr ->
  match Ast.AttributeExpr.cast expr with
  | Some attribute -> (
      match Ast.AttributeExpr.inner attribute with
      | Some inner -> Doc.concat
        [
          expr_doc inner;
          Doc.space;
          attribute_shell_doc
            ~for_each_shell_token:(fun ~fn -> Ast.AttributeExpr.for_each_shell_token attribute ~fn);
        ]
      | None -> unsupported "attribute expression without inner expression"
    )
  | None -> unsupported "unsupported attribute expression"

and extension_expr_doc = fun expr ->
  match Ast.ExtensionExpr.cast expr with
  | Some extension -> extension_shell_doc
    ~for_each_shell_token:(fun ~fn -> Ast.ExtensionExpr.for_each_shell_token extension ~fn)
  | None -> unsupported "unsupported extension expression"

and list_expr_body_doc = fun expr ->
  let items = child_expr_docs expr in
  let singleton =
    match items with
    | [ _ ] -> true
    | _ -> false
  in
  let item_docs = List.map items ~fn:(expr_list_item_doc ~singleton) in
  let simple_inline_item item =
    match Ast.Expr.view item with
    | Path _
    | Literal _
    | PolyVariant { payload=None }
    | Parenthesized { inner=None } ->
        true
    | Parenthesized { inner=Some inner } -> (
        match Ast.Expr.view inner with
        | Path _
        | Literal _
        | PolyVariant { payload=None }
        | Parenthesized { inner=None } -> true
        | _ -> false
      )
    | _ ->
        false
  in
  let multiline_items_doc ~trailing_semi item_docs = Doc.concat
    [ Doc.line; Doc.indent 2 (Doc.join (Doc.concat [ Doc.semi; Doc.line ]) item_docs); (
        if trailing_semi then
          Doc.semi
        else
          Doc.empty
      ); Doc.line; ]
  in
  if
    (Int.(List.length item_docs > 3)
    && (not (List.all items ~fn:simple_inline_item) || Int.(node_token_text_width expr > 70)))
    || (Int.(List.length item_docs > 2)
    && Int.(node_token_text_width expr > 70)
    && not (List.all items ~fn:simple_inline_item))
  then
    multiline_items_doc ~trailing_semi:true item_docs
  else if List.any item_docs ~fn:Doc.is_multiline then
    multiline_items_doc ~trailing_semi:(expr_list_has_trailing_separator expr items) item_docs
  else if expr_list_has_trailing_separator expr items then
    Doc.concat
      [ Doc.space; Doc.join (Doc.concat [ Doc.semi; Doc.space ]) item_docs; Doc.semi; Doc.space; ]
  else
    (
      match item_docs with
      | [] -> Doc.empty
      | item_docs -> Doc.concat
        [ Doc.space; Doc.join (Doc.concat [ Doc.semi; Doc.space ]) item_docs; Doc.space; ]
    )

and array_expr_body_doc = fun expr ->
  child_expr_docs expr |> List.map ~fn:expr_doc |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])

and record_expr_field_value_breaks = fun value value_doc ->
  Doc.is_multiline value_doc || match Ast.Expr.view value with
  | If _
  | Let _
  | Match _
  | Try _
  | Sequence _
  | LetException _ -> true
  | _ -> false

and record_expr_field_doc = fun (field: Ast.RecordExpr.field) ->
  match field.path with
  | Some path -> (
      match field.value with
      | Some value ->
          let value_doc = expr_doc value in
          if record_expr_field_value_breaks value value_doc then
            Doc.concat [ path_doc path; Doc.space; Doc.equal; Doc.line; Doc.indent 2 value_doc ]
          else
            Doc.concat [ path_doc path; Doc.space; Doc.equal; Doc.space; value_doc ]
      | None -> path_doc path
    )
  | None -> unsupported "unsupported record expression field"

and record_expr_field_docs = fun expr ->
  let fields = ref [] in
  Ast.RecordExpr.for_each_field
    expr
    ~fn:(fun field -> fields := record_expr_field_doc field :: !fields);
  List.reverse !fields

and record_expr_fields_doc = fun expr ->
  record_expr_field_docs expr |> Doc.join (Doc.concat [ Doc.semi; Doc.space ])

and record_expr_body_doc = fun ?(force_multiline = false) expr ->
  let multiline_fields_doc ~trailing_semi fields = Doc.concat
    [ Doc.line; Doc.indent 2 (Doc.join (Doc.concat [ Doc.semi; Doc.line ]) fields); (
        if trailing_semi then
          Doc.semi
        else
          Doc.empty
      ); Doc.line; ]
  in
  let fields = record_expr_field_docs expr in
  match Ast.RecordExpr.base expr with
  | Some base ->
      let fields_doc = Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields in
      Doc.concat
        [ Doc.space; expr_doc base; Doc.space; Doc.text "with"; (
            match fields_doc with
            | Doc.Empty -> Doc.empty
            | fields_doc -> Doc.concat [ Doc.space; fields_doc ]
          ); Doc.space; ]
  | None -> (
      match fields with
      | [] -> Doc.empty
      | fields when Int.(List.length fields > 3) || List.any fields ~fn:Doc.is_multiline -> multiline_fields_doc
        ~trailing_semi:true
        fields
      | fields when force_multiline -> multiline_fields_doc ~trailing_semi:false fields
      | fields -> Doc.concat
        [ Doc.space; Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields; Doc.space; ]
    )

and record_expr_doc = fun expr ->
  match Ast.RecordExpr.base expr with
  | Some _ -> Doc.concat [ Doc.lbrace; record_expr_body_doc expr; Doc.rbrace ]
  | None -> Doc.concat [ Doc.lbrace; record_expr_body_doc expr; Doc.rbrace ]

and expr_child_exprs = fun expr ->
  let children = ref [] in
  Ast.Expr.for_each_child_expr expr ~fn:(fun child -> children := child :: !children);
  List.reverse !children

and local_open_poly_variant_record_doc = fun expr dot_token opening_token body closing_token ->
  match expr_child_exprs expr with
  | prefix :: _ when not (Ast.Node.raw_range prefix = Ast.Node.raw_range body) -> Some (Doc.concat
    [
      expr_doc prefix;
      token_doc dot_token;
      token_doc opening_token;
      record_expr_body_doc ~force_multiline:true body;
      token_doc closing_token;
    ])
  | _ -> None

and local_open_expr_doc = fun expr ->
  let local_open =
    match Ast.LocalOpenExpr.cast expr with
    | Some local_open -> local_open
    | None -> unsupported "unsupported local open expression"
  in
  match Ast.LocalOpenExpr.view local_open with
  | LetOpen {
    let_token=Some let_token;
    open_token=Some open_token;
    bang_token;
    module_path=Some module_path;
    in_token=Some in_token;
    body=Some body;

  } ->
      let head = Doc.concat
        [
          token_doc let_token;
          Doc.space;
          token_doc open_token;
          optional_token_doc bang_token;
          Doc.space;
          path_doc module_path;
          Doc.space;
          token_doc in_token;
        ] in
      let body_doc = expr_doc body in
      let body_breaks =
        match Ast.Expr.view body with
        | Apply _ when Doc.is_multiline body_doc -> true
        | LocalOpen _ -> true
        | _ -> false
      in
      if body_breaks then
        Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
      else
        Doc.concat [ head; Doc.space; body_doc ]
  | LetOpen _ ->
      unsupported "incomplete let open expression"
  | Delimited {
    module_path=Some module_path;
    dot_token=Some dot_token;
    opening_token=Some opening_token;
    body=Some body;
    closing_token=Some closing_token;

  } ->
      Doc.concat
        (
          if expr_is_operator_value_path body then
            [
              path_doc module_path;
              token_doc dot_token;
              token_doc opening_token;
              Doc.space;
              expr_doc body;
              Doc.space;
              token_doc closing_token;
            ]
          else
            [ path_doc module_path; token_doc dot_token; token_doc opening_token; (
                match Ast.Expr.view body with
                | List -> list_expr_body_doc body
                | Array -> array_expr_body_doc body
                | Record
                | RecordUpdate -> record_expr_body_doc ~force_multiline:true body
                | _ -> expr_doc body
              ); token_doc closing_token; ]
        )
  | Delimited {
    module_path=None;
    dot_token=Some dot_token;
    opening_token=Some opening_token;
    body=Some body;
    closing_token=Some closing_token;

  } when (
    match Ast.Expr.view body with
    | Record
    | RecordUpdate -> true
    | _ -> false
  ) -> (
      match local_open_poly_variant_record_doc expr dot_token opening_token body closing_token with
      | Some doc -> doc
      | None -> unsupported "incomplete delimited local open expression"
    )
  | Delimited _ ->
      unsupported "incomplete delimited local open expression"

and let_module_path_body_doc = fun expr ->
  let doc = ref None in
  Ast.LetModuleExpr.for_each_module_body_path_ident expr
    ~fn:(fun token ->
      let segment = token_doc token in
      doc := Some (
        match !doc with
        | None -> segment
        | Some doc -> Doc.concat [ doc; Doc.text "."; segment ]
      ));
  match !doc with
  | Some doc -> doc
  | None -> unsupported "let module expression path body without identifiers"

and let_module_body_doc = fun expr ->
  match Ast.LetModuleExpr.module_body expr with
  | Ast.LetModuleExpr.Path ->
      let_module_path_body_doc expr
  | Ast.LetModuleExpr.EmptyStruct ->
      Doc.concat [ Doc.text "struct"; Doc.space; Doc.text "end" ]
  | Ast.LetModuleExpr.Unsupported -> (
      match local_module_struct_body_doc (local_module_body_tokens expr) with
      | Some doc -> doc
      | None -> unsupported "unsupported let module body"
    )

and let_module_expr_doc = fun expr ->
  let module_expr =
    match Ast.LetModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported "unsupported let module expression"
  in
  match Ast.LetModuleExpr.let_token module_expr, Ast.LetModuleExpr.module_token module_expr, Ast.LetModuleExpr.name
    module_expr, Ast.LetModuleExpr.equals_token module_expr, Ast.LetModuleExpr.in_token module_expr, Ast.LetModuleExpr.body
    module_expr with
  | Some let_token, Some module_token, Some name, Some equals_token, Some in_token, Some body ->
      let module_body_doc = let_module_body_doc module_expr in
      let body_doc = expr_doc body in
      Doc.concat
        (
          [
            token_doc let_token;
            Doc.space;
            token_doc module_token;
            Doc.space;
            token_doc name;
            Doc.space;
            token_doc equals_token;
            Doc.space;
            module_body_doc;
            Doc.space;
            token_doc in_token;
          ] @ if Doc.is_multiline module_body_doc then
            [ Doc.line; body_doc ]
          else
            [ Doc.space; body_doc ]
        )
  | _ -> unsupported "incomplete let module expression"

and let_exception_payload_needs_space = fun previous current ->
  match previous, current with
  | (_, Kind.DOT)
  | (Kind.DOT, _)
  | (Kind.QUOTE, _)
  | (Kind.LPAREN, _)
  | (Kind.LBRACKET, _)
  | (_, Kind.RPAREN)
  | (_, Kind.RBRACKET) -> false
  | _ -> true

and let_exception_payload_doc = fun expr ->
  let doc = ref None in
  let previous = ref None in
  Ast.LetExceptionExpr.for_each_payload_token expr
    ~fn:(fun token ->
      let segment = token_doc token in
      let kind = Ast.Token.kind token in
      doc := Some (
        match !doc, !previous with
        | None, _ -> segment
        | Some doc, Some previous when let_exception_payload_needs_space previous kind -> Doc.concat
          [ doc; Doc.space; segment ]
        | Some doc, _ -> Doc.concat [ doc; segment ]
      );
      previous := Some kind);
  !doc

and let_exception_expr_doc = fun expr ->
  let exception_expr =
    match Ast.LetExceptionExpr.cast expr with
    | Some exception_expr -> exception_expr
    | None -> unsupported "unsupported let exception expression"
  in
  match Ast.LetExceptionExpr.let_token exception_expr, Ast.LetExceptionExpr.exception_token exception_expr, Ast.LetExceptionExpr.name
    exception_expr, Ast.LetExceptionExpr.in_token exception_expr, Ast.LetExceptionExpr.body exception_expr with
  | Some let_token, Some exception_token, Some name, Some in_token, Some body ->
      let payload =
        match Ast.LetExceptionExpr.of_token exception_expr with
        | None -> Doc.empty
        | Some of_token -> (
            match let_exception_payload_doc exception_expr with
            | None -> Doc.concat [ Doc.space; token_doc of_token ]
            | Some payload -> Doc.concat [ Doc.space; token_doc of_token; Doc.space; payload ]
          )
      in
      let head = Doc.concat
        [
          token_doc let_token;
          Doc.space;
          token_doc exception_token;
          Doc.space;
          token_doc name;
          payload;
          Doc.space;
          token_doc in_token;
        ] in
      if expr_arrow_body_breaks body || expr_has_unconsumed_boundary_leading_comment body then
        Doc.concat [ head; Doc.line; expr_doc_with_boundary_leading_comment body ]
      else
        Doc.concat [ head; Doc.space; expr_doc body ]
  | _ -> unsupported "incomplete let exception expression"

and let_exception_expr_doc_with_body_break = fun expr ->
  let exception_expr =
    match Ast.LetExceptionExpr.cast expr with
    | Some exception_expr -> exception_expr
    | None -> unsupported "unsupported let exception expression"
  in
  match Ast.LetExceptionExpr.let_token exception_expr, Ast.LetExceptionExpr.exception_token exception_expr, Ast.LetExceptionExpr.name
    exception_expr, Ast.LetExceptionExpr.in_token exception_expr, Ast.LetExceptionExpr.body exception_expr with
  | Some let_token, Some exception_token, Some name, Some in_token, Some body ->
      let payload =
        match Ast.LetExceptionExpr.of_token exception_expr with
        | None -> Doc.empty
        | Some of_token -> (
            match let_exception_payload_doc exception_expr with
            | None -> Doc.concat [ Doc.space; token_doc of_token ]
            | Some payload -> Doc.concat [ Doc.space; token_doc of_token; Doc.space; payload ]
          )
      in
      Doc.concat
        [
          token_doc let_token;
          Doc.space;
          token_doc exception_token;
          Doc.space;
          token_doc name;
          payload;
          Doc.space;
          token_doc in_token;
          Doc.line;
          expr_doc_with_boundary_leading_comment body;
        ]
  | _ -> unsupported "incomplete let exception expression"

and first_class_module_path_doc = fun expr ->
  let segments = ref [] in
  Ast.FirstClassModuleExpr.for_each_module_path_ident
    expr
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "first-class module expression without module path"
  | segments -> Doc.join (Doc.text ".") segments

and first_class_module_ascription_doc = fun expr ->
  let segments = ref [] in
  Ast.FirstClassModuleExpr.for_each_ascription_path_ident
    expr
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "first-class module expression without module type path"
  | segments -> Doc.join (Doc.text ".") segments

and first_class_module_expr_doc = fun expr ->
  let module_expr =
    match Ast.FirstClassModuleExpr.cast expr with
    | Some module_expr -> module_expr
    | None -> unsupported "unsupported first-class module expression"
  in
  match Ast.FirstClassModuleExpr.opening_token module_expr, Ast.FirstClassModuleExpr.module_token module_expr, Ast.FirstClassModuleExpr.module_path
    module_expr, Ast.FirstClassModuleExpr.ascription module_expr, Ast.FirstClassModuleExpr.closing_token
    module_expr with
  | Some opening_token, Some module_token, Ast.FirstClassModuleExpr.ModulePath, Ast.FirstClassModuleExpr.NoAscription, Some closing_token ->
      Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          first_class_module_path_doc module_expr;
          token_doc closing_token;
        ]
  | Some opening_token, Some module_token, Ast.FirstClassModuleExpr.ModulePath, Ast.FirstClassModuleExpr.PathAscription, Some closing_token -> (
      match Ast.FirstClassModuleExpr.colon_token module_expr with
      | Some colon_token -> Doc.concat
        [
          token_doc opening_token;
          token_doc module_token;
          Doc.space;
          first_class_module_path_doc module_expr;
          Doc.space;
          token_doc colon_token;
          Doc.space;
          first_class_module_ascription_doc module_expr;
          token_doc closing_token;
        ]
      | None -> unsupported "first-class module ascription without colon token"
    )
  | _ ->
      unsupported "unsupported first-class module expression"

and binding_operator_clause_doc = fun (clause: Ast.BindingOperatorExpr.clause) ->
  match clause.keyword, clause.operator with
  | Some keyword, Some operator -> Doc.concat
    [ token_doc keyword; token_doc operator; Doc.space; let_binding_doc clause.binding ]
  | _ -> unsupported "incomplete binding operator clause"

and binding_operator_clause_breaks = fun (clause: Ast.BindingOperatorExpr.clause) ->
  match Ast.LetBinding.body clause.binding with
  | Some body -> Doc.is_multiline (let_binding_doc clause.binding)
  || expr_binding_body_breaks_after_equal body
  || expr_has_unconsumed_boundary_leading_comment body
  | None -> false

and binding_operator_expr_doc = fun expr ->
  let view =
    match Ast.BindingOperatorExpr.cast expr with
    | Some view -> view
    | None -> unsupported "unsupported binding operator expression"
  in
  let clauses = ref [] in
  Ast.BindingOperatorExpr.for_each_clause
    view
    ~fn:(fun clause -> clauses := (clause, binding_operator_clause_doc clause) :: !clauses);
  match List.reverse !clauses, Ast.BindingOperatorExpr.in_token view, Ast.BindingOperatorExpr.body view with
  | [], _, _ ->
      unsupported "binding operator expression without binding"
  | _, None, _ ->
      unsupported "binding operator expression without in"
  | _, _, None ->
      unsupported "binding operator expression without body"
  | clauses, Some in_token, Some body ->
      let multiline_clause =
        List.any
          clauses
          ~fn:(fun (clause, doc) -> binding_operator_clause_breaks clause || Doc.is_multiline doc)
      in
      let clause_docs =
        List.map clauses ~fn:(fun (_, doc) -> doc)
      in
      let head =
        if multiline_clause then
          Doc.concat [ Doc.join Doc.line clause_docs; Doc.line; token_doc in_token ]
        else
          Doc.concat [ Doc.join Doc.space clause_docs; Doc.space; token_doc in_token ]
      in
      if
        multiline_clause
        || expr_binding_body_breaks_after_equal body
        || expr_has_unconsumed_boundary_leading_comment body
      then
        Doc.concat [ head; Doc.line; expr_doc_with_boundary_leading_comment body ]
      else
        Doc.concat [ head; Doc.space; expr_doc body ]

and split_typed_binding_pattern = fun pattern ->
  match Ast.Pattern.view pattern with
  | Constraint { pattern=Some pattern; annotation=Some annotation } -> (
    pattern,
    Some (TypeAnnotation annotation)
  )
  | _ -> (pattern, None)

and collect_binding_head_parameters = fun pattern ->
  let rec collect pattern parameters =
    match Ast.Pattern.view pattern with
    | Apply { callee=Some callee; argument=Some argument } -> collect callee (argument :: parameters)
    | _ -> (pattern, parameters)
  in
  collect pattern []

and parameter_colon_has_leading_space = fun parameter ->
  match Ast.Node.first_child_token parameter ~kind:Kind.COLON with
  | Some colon -> not (String.is_empty (Ast.Token.leading_text colon))
  | None -> false

and split_labeled_parameter_binding_annotation = fun parameter ->
  match Ast.Parameter.view parameter with
  | Labeled { pattern=Some annotation; _ }
  | Optional { pattern=Some annotation; _ } when parameter_colon_has_leading_space parameter -> Some (TokenAnnotation (node_tokens
    annotation))
  | _ -> None

and binding_head_parameter_doc = function
  | PatternParameter pattern -> pattern_doc pattern
  | LabelOnlyParameter pattern -> (
      match Ast.Pattern.view pattern with
      | LabeledParam parameter -> (
          match Ast.Parameter.view parameter with
          | Labeled { label=Some label; _ } -> Doc.concat [ Doc.text "~"; token_doc label ]
          | _ -> pattern_doc pattern
        )
      | OptionalParam parameter
      | OptionalParamDefault parameter -> (
          match Ast.Parameter.view parameter with
          | Optional { label=Some label; _ }
          | OptionalDefault { label=Some label; _ } -> Doc.concat [ Doc.text "?"; token_doc label ]
          | _ -> pattern_doc pattern
        )
      | _ ->
          pattern_doc pattern
    )

and binding_head_parameter_is_labeled = function
  | PatternParameter pattern
  | LabelOnlyParameter pattern -> (
      match Ast.Pattern.view pattern with
      | LabeledParam _
      | OptionalParam _
      | OptionalParamDefault _ -> true
      | _ -> false
    )

and pattern_parameters = fun parameters ->
  List.map parameters ~fn:(fun parameter -> PatternParameter parameter)

and split_parameterized_binding_annotation = fun parameters ->
  match List.reverse parameters with
  | [] -> ([], None)
  | last :: prefix_rev -> (
      match Ast.Pattern.view last with
      | LabeledParam parameter
      | OptionalParam parameter
      | OptionalParamDefault parameter -> (
          match split_labeled_parameter_binding_annotation parameter with
          | Some annotation -> (
            pattern_parameters (List.reverse prefix_rev) @ [ LabelOnlyParameter last ],
            Some annotation
          )
          | None -> (pattern_parameters parameters, None)
        )
      | _ ->
          let parameter, annotation = split_typed_binding_pattern last in
          match annotation with
          | Some annotation ->
              let first_parameter, rest_parameters = collect_binding_head_parameters parameter in
              let prefix = List.reverse prefix_rev in
              (
                match rest_parameters with
                | [] -> (
                    match Ast.Pattern.view first_parameter with
                    | LabeledParam _
                    | OptionalParam _
                    | OptionalParamDefault _ -> (
                      pattern_parameters prefix @ [ PatternParameter first_parameter ],
                      Some annotation
                    )
                    | _ -> (pattern_parameters parameters, None)
                  )
                | _ -> (
                  pattern_parameters (prefix @ (first_parameter :: rest_parameters)),
                  Some annotation
                )
              )
          | None -> (pattern_parameters parameters, None)
    )

and binding_annotation_doc = fun parameters annotation ->
  let annotation_doc =
    match annotation with
    | TypeAnnotation annotation -> type_expr_binding_annotation_doc annotation
    | TokenAnnotation tokens -> type_tokens_inline_doc tokens
  in
  let loose_colon =
    match List.reverse parameters with
    | last :: _ -> binding_head_parameter_is_labeled last
    | [] -> false
  in
  if Doc.is_multiline annotation_doc then
    if loose_colon then
      Doc.concat [ Doc.space; Doc.text ":"; Doc.line; Doc.indent 2 annotation_doc ]
    else
      Doc.concat [ Doc.text ":"; Doc.line; Doc.indent 2 annotation_doc ]
  else if loose_colon then
    Doc.concat [ Doc.space; Doc.text ":"; Doc.space; annotation_doc ]
  else
    Doc.concat [ Doc.text ":"; Doc.space; annotation_doc ]

and expr_contains_labeled_argument = fun expr ->
  let rec loop expr =
    match Ast.Expr.view expr with
    | LabeledArg _
    | OptionalArg _ -> true
    | Apply { callee=Some callee; argument=Some argument }
    | Infix { left=Some callee; right=Some argument; _ } -> loop callee || loop argument
    | Parenthesized { inner=Some inner }
    | Typed { expr=Some inner; _ } -> loop inner
    | _ -> false
  in
  loop expr

and local_binding_infix_chain_doc = fun expr ->
  match Ast.Expr.view expr with
  | Infix { operator=Some operator; _ } -> (
      let operator_text = infix_operator_text expr operator in
      if
        String.equal operator_text "^"
        || String.equal operator_text "||"
        || String.equal operator_text "&&"
      then
        let parts = same_infix_chain operator expr in
        if Int.(List.length parts >= 4) && List.any parts ~fn:expr_contains_labeled_argument then
          match infix_chain_doc ~minimum_parts:2 expr with
          | Some doc ->
              if String.equal operator_text "^" then
                Some (`After_equal doc)
              else
                Some (`BodyBreak doc)
          | None -> None
        else
          None
      else
        None
    )
  | _ -> None

and let_binding_doc = fun ?(force_body_break_after_equal = false) binding ->
  let parts = let_binding_parts binding in
  match parts.pattern, parts.body with
  | Some pattern, Some body ->
      let pattern, annotation_from_pattern = split_typed_binding_pattern pattern in
      let pattern, parameters_from_pattern = collect_binding_head_parameters pattern in
      let parameters =
        match parts.parameters with
        | [] -> parameters_from_pattern
        | parameters -> parameters
      in
      let parameters, annotation_from_parameters = split_parameterized_binding_annotation parameters in
      let annotation =
        match parts.annotation with
        | Some annotation -> Some (TypeAnnotation annotation)
        | None -> (
            match annotation_from_parameters with
            | Some annotation -> Some annotation
            | None -> annotation_from_pattern
          )
      in
      let head = Doc.concat
        [ pattern_doc pattern; (
            match parameters with
            | [] -> Doc.empty
            | parameters -> Doc.concat
              [ Doc.space; Doc.join Doc.space (List.map parameters ~fn:binding_head_parameter_doc) ]
          ); (
            match annotation with
            | Some annotation -> binding_annotation_doc parameters annotation
            | None -> Doc.empty
          ); Doc.space; Doc.equal; ]
      in
      (
        match function_binding_body_doc body with
        | Some body_doc -> Doc.concat [ head; Doc.space; body_doc ]
        | None -> (
            match local_binding_infix_chain_doc body with
            | Some (`After_equal body_doc) ->
                Doc.concat [ head; Doc.space; body_doc ]
            | Some (`BodyBreak body_doc) ->
                Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
            | None -> (
                match large_infix_chain_doc body with
                | Some body_doc -> Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
                | None ->
                    let body =
                      match Ast.Expr.view body with
                      | Parenthesized { inner=Some inner } when not
                        (expr_has_leading_comment inner || expr_is_begin_block body) -> (
                          match Ast.Expr.view inner with
                          | Parenthesized { inner=Some inner } when not
                            (expr_has_leading_comment inner || expr_is_begin_block body) -> inner
                          | _ -> body
                        )
                      | _ -> body
                    in
                    let inline_body_doc = expr_doc body in
                    let body_breaks =
                      force_body_break_after_equal
                      || expr_binding_body_breaks_after_equal body
                      || expr_has_unconsumed_boundary_leading_comment body
                      || match Ast.Expr.view body with
                      | Apply _ -> application_breaks_after_equal body inline_body_doc
                      | Parenthesized { inner=Some inner } -> expr_parenthesized_inner_breaks_after_equal
                        inner
                      | _ -> false
                    in
                    let body_doc =
                      if body_breaks then
                        expr_multiline_body_doc body
                      else
                        expr_doc_with_boundary_leading_comment body
                    in
                    if body_breaks then
                      Doc.concat [ head; Doc.line; Doc.indent 2 body_doc ]
                    else
                      Doc.concat [ head; Doc.space; body_doc ]
              )
          )
      )
  | _ -> unsupported "incomplete let binding"

and let_bindings_doc = fun ~keyword ~rec_token node ->
  match let_binding_nodes node with
  | [] -> unsupported (keyword ^ " declaration without binding")
  | first :: rest ->
      let and_tokens = direct_child_tokens node
      |> List.filter ~fn:(fun token -> token_kind_is token Kind.AND_KW) in
      let rec zip_and_tokens tokens bindings =
        match tokens, bindings with
        | token :: tokens, binding :: bindings -> (Some token, binding) :: zip_and_tokens tokens bindings
        | [], binding :: bindings -> (None, binding) :: zip_and_tokens [] bindings
        | _, [] -> []
      in
      let rest =
        zip_and_tokens and_tokens rest
        |> List.map
          ~fn:(fun (and_token, binding) ->
            let keyword_doc =
              match and_token with
              | Some token -> token_doc token
              | None -> Doc.text "and"
            in
            let leading =
              match and_token with
              | Some token when Ast.Token.has_leading_comment token -> Doc.concat
                [ trimmed_leading_comment_token_doc token; Doc.line ]
              | _ -> Doc.empty
            in
            Doc.concat [ Doc.space; leading; keyword_doc; Doc.space; let_binding_doc binding ])
      in
      Doc.concat
        (
          [ Doc.text keyword; (
              match rec_token with
              | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
              | None -> Doc.space
            ); let_binding_doc first; ] @ rest
        )

and let_binding_keeps_in_on_same_line = fun binding ->
  match Ast.LetBinding.body binding with
  | Some body -> (
      match Ast.Expr.view body with
      | Infix { operator=Some operator; _ } ->
          String.equal (infix_operator_text body operator) "|>"
      | Apply _ -> (
          let _callee, arguments = application_parts body in
          match arguments with
          | [ argument ] -> (
              match Ast.Expr.view argument with
              | List
              | Array -> true
              | _ -> false
            )
          | _ -> false
        )
      | _ ->
          false
    )
  | None -> false

and let_bindings_block_doc = fun ~keyword ~rec_token node ->
  match let_binding_nodes node with
  | [] -> unsupported (keyword ^ " declaration without binding")
  | first :: rest ->
      let first_doc = let_binding_doc first in
      let rec collect rest docs last_is_multiline last_keeps_in =
        match rest with
        | [] -> (List.reverse docs, last_is_multiline, last_keeps_in)
        | binding :: rest ->
            let binding_doc = let_binding_doc binding in
            collect
              rest
              (Doc.concat [ Doc.line; Doc.text "and"; Doc.space; binding_doc ] :: docs)
              (Doc.is_multiline binding_doc)
              (let_binding_keeps_in_on_same_line binding)
      in
      let rest_docs, last_is_multiline, last_keeps_in = collect
        rest
        []
        (Doc.is_multiline first_doc)
        (let_binding_keeps_in_on_same_line first) in
      (
        Doc.concat
          (
            [ Doc.text keyword; (
                match rec_token with
                | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
                | None -> Doc.space
              ); first_doc; ] @ rest_docs
          ),
        last_is_multiline,
        last_keeps_in
      )

let let_decl_doc = fun decl ->
  let_bindings_doc ~keyword:"let" ~rec_token:(Ast.LetDeclaration.rec_token decl) decl

let let_decl_block_doc = fun ?(force_body_break_after_equal = false) decl ->
  match let_binding_nodes decl with
  | [] -> unsupported "let declaration without binding"
  | first :: rest ->
      let and_tokens = direct_child_tokens decl
      |> List.filter ~fn:(fun token -> token_kind_is token Kind.AND_KW) in
      let rec zip_and_tokens tokens bindings =
        match tokens, bindings with
        | token :: tokens, binding :: bindings -> (Some token, binding) :: zip_and_tokens tokens bindings
        | [], binding :: bindings -> (None, binding) :: zip_and_tokens [] bindings
        | _, [] -> []
      in
      let rest =
        zip_and_tokens and_tokens rest
        |> List.map
          ~fn:(fun (and_token, binding) ->
            let keyword_doc =
              match and_token with
              | Some token -> token_doc token
              | None -> Doc.text "and"
            in
            let leading =
              match and_token with
              | Some token when Ast.Token.has_leading_comment token -> Doc.concat
                [ trimmed_leading_comment_token_doc token; Doc.line ]
              | _ -> Doc.empty
            in
            Doc.concat
              [
                blank_line;
                leading;
                keyword_doc;
                Doc.space;
                let_binding_doc ~force_body_break_after_equal binding
              ])
      in
      Doc.concat
        (
          [ Doc.text "let"; (
              match Ast.LetDeclaration.rec_token decl with
              | Some rec_token -> Doc.concat [ Doc.space; token_doc rec_token; Doc.space ]
              | None -> Doc.space
            ); let_binding_doc ~force_body_break_after_equal first; ] @ rest
        )

let type_parameter_doc = function
  | Ast.TypeDeclaration.Named { name; quote; variance; injective } -> Doc.concat
    [
      optional_token_doc variance;
      optional_token_doc injective;
      optional_token_doc quote;
      token_doc name;
    ]
  | Ast.TypeDeclaration.Wildcard { wildcard; variance; injective } -> Doc.concat
    [ optional_token_doc variance; optional_token_doc injective; token_doc wildcard ]

let type_parameters_doc = fun decl ->
  let parameters = ref [] in
  Ast.TypeDeclaration.for_each_parameter
    decl
    ~fn:(fun param -> parameters := type_parameter_doc param :: !parameters);
  match List.reverse !parameters with
  | [] -> Doc.empty
  | [ parameter ] -> Doc.concat [ parameter; Doc.space ]
  | parameters ->
      if Int.(List.length parameters > 4) then
        Doc.concat
          [
            Doc.lparen;
            Doc.line;
            Doc.indent 2 (Doc.join (Doc.concat [ Doc.comma; Doc.line ]) parameters);
            Doc.line;
            Doc.rparen;
            Doc.space;
          ]
      else
        Doc.concat
          [
            Doc.lparen;
            Doc.join (Doc.concat [ Doc.comma; Doc.space ]) parameters;
            Doc.rparen;
            Doc.space;
          ]

let type_decl_tokens = fun decl ->
  let tokens = ref [] in
  Ast.TypeDeclaration.for_each_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let type_token_depth_after = fun depth token ->
  match Ast.Token.kind token with
  | kind when Kind.(kind = LPAREN || kind = LBRACE || kind = LBRACKET || kind = LBRACKET_BAR) -> Int.(depth
  + 1)
  | kind when Kind.(kind = RPAREN || kind = RBRACE || kind = RBRACKET || kind = BAR_RBRACKET)
  && Int.(depth > 0) -> Int.(depth - 1)
  | _ -> depth

let token_docs = fun tokens -> List.map tokens ~fn:token_doc

let type_parameter_tokens_doc = fun tokens -> Doc.concat (token_docs tokens)

let split_type_parameter_groups = fun tokens ->
  let rec loop current groups = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> groups
            | _ -> List.reverse current :: groups
          )
    | token :: rest when token_kind_is token Kind.COMMA -> loop [] (List.reverse current :: groups) rest
    | token :: rest -> loop (token :: current) groups rest
  in
  loop [] [] tokens

let type_parameters_from_parenthesized_tokens = fun tokens ->
  match tokens with
  | opening :: rest when token_kind_is opening Kind.LPAREN ->
      let rec gather current depth = function
        | [] -> (List.reverse current, [])
        | token :: rest when token_kind_is token Kind.RPAREN && Int.equal depth 0 -> (
          List.reverse current,
          rest
        )
        | token :: rest -> gather (token :: current) (type_token_depth_after depth token) rest
      in
      let inside, rest = gather [] 0 rest in
      let params = split_type_parameter_groups inside |> List.map ~fn:type_parameter_tokens_doc in
      (params, rest)
  | _ -> ([], tokens)

let type_parameter_starts = fun token ->
  let kind = Ast.Token.kind token in
  Kind.(kind = PLUS || kind = MINUS || kind = BANG || kind = QUOTE || kind = UNDERSCORE)

let take_type_parameter_tokens = fun tokens ->
  let rec take_modifiers current = function
    | token :: rest when token_kind_is token Kind.PLUS
    || token_kind_is token Kind.MINUS
    || token_kind_is token Kind.BANG -> take_modifiers (token :: current) rest
    | tokens -> (current, tokens)
  in
  let modifiers, rest = take_modifiers [] tokens in
  match rest with
  | quote :: name :: rest when token_kind_is quote Kind.QUOTE && token_kind_is name Kind.IDENT -> (
    Some (List.reverse (name :: quote :: modifiers)),
    rest
  )
  | wildcard :: rest when token_kind_is wildcard Kind.UNDERSCORE -> (
    Some (List.reverse (wildcard :: modifiers)),
    rest
  )
  | _ -> (None, tokens)

type rendered_type_body = {
  doc: Doc.t;
  leading_line: bool;
  break_after_equal: bool;
}

type parsed_type_member = {
  type_keyword: Ast.Token.t;
  nonrec_token: Ast.Token.t option;
  parameters: Doc.t list;
  name: Ast.Token.t option;
  body_tokens: Ast.Token.t list;
}

let parse_type_member_header = fun tokens ->
  let type_keyword, rest =
    match tokens with
    | keyword :: rest when token_kind_is keyword Kind.TYPE_KW || token_kind_is keyword Kind.AND_KW -> (
      keyword,
      rest
    )
    | _ -> unsupported "type declaration member without type keyword"
  in
  let nonrec_token, rest =
    match rest with
    | token :: rest when token_kind_is token Kind.NONREC_KW -> (Some token, rest)
    | _ -> (None, rest)
  in
  let rec parse_parameters parameters tokens =
    match tokens with
    | token :: _ when token_kind_is token Kind.LPAREN ->
        let grouped, rest = type_parameters_from_parenthesized_tokens tokens in
        parse_parameters (List.reverse grouped @ parameters) rest
    | token :: _ when type_parameter_starts token -> (
        match take_type_parameter_tokens tokens with
        | Some parameter, rest -> parse_parameters
          (type_parameter_tokens_doc parameter :: parameters)
          rest
        | None, rest -> parse_parameters parameters rest
      )
    | _ ->
        (List.reverse parameters, tokens)
  in
  let parameters, rest = parse_parameters [] rest in
  let name, rest =
    match rest with
    | token :: rest when token_kind_is token Kind.IDENT -> (Some token, rest)
    | _ -> (None, rest)
  in
  let body_tokens =
    match rest with
    | token :: rest when token_kind_is token Kind.EQ -> rest
    | _ -> []
  in
  {
    type_keyword;
    nonrec_token;
    parameters;
    name;
    body_tokens;
  }

let type_member_parameters_doc = fun parameters ->
  match parameters with
  | [] -> Doc.empty
  | [ parameter ] -> Doc.concat [ parameter; Doc.space ]
  | parameters ->
      if Int.(List.length parameters > 4) then
        Doc.concat
          [
            Doc.lparen;
            Doc.line;
            Doc.indent 2 (Doc.join (Doc.concat [ Doc.comma; Doc.line ]) parameters);
            Doc.line;
            Doc.rparen;
            Doc.space;
          ]
      else
        Doc.concat
          [
            Doc.lparen;
            Doc.join (Doc.concat [ Doc.comma; Doc.space ]) parameters;
            Doc.rparen;
            Doc.space;
          ]

let split_top_level_token = fun tokens ~matches ->
  let rec loop before depth = function
    | [] -> None
    | token :: rest when Int.equal depth 0 && matches (Ast.Token.kind token) -> Some (
      List.reverse before,
      token,
      rest
    )
    | token :: rest -> loop (token :: before) (type_token_depth_after depth token) rest
  in
  loop [] 0 tokens

let split_top_level_all = fun tokens ~matches ->
  let rec loop current groups depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> groups
            | _ -> List.reverse current :: groups
          )
    | token :: rest when Int.equal depth 0 && matches (Ast.Token.kind token) -> loop
      []
      (List.reverse current :: groups)
      (type_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) groups (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let declaration_name_doc = fun tokens ->
  match tokens with
  | opening :: rest when token_kind_is opening Kind.LPAREN ->
      let rec gather_inner current depth = function
        | [] -> None
        | token :: rest when token_kind_is token Kind.RPAREN && Int.equal depth 0 -> Some (
          List.reverse current,
          token,
          rest
        )
        | token :: rest -> gather_inner (token :: current) (type_token_depth_after depth token) rest
      in
      (
        match gather_inner [] 0 rest with
        | Some (inner, closing, []) -> Doc.concat
          [
            token_doc opening;
            Doc.space;
            type_tokens_inline_doc inner;
            Doc.space;
            token_doc closing
          ]
        | _ -> type_tokens_inline_doc tokens
      )
  | _ -> type_tokens_inline_doc tokens

let declaration_head_token_needs_space = fun previous current ->
  match previous, current with
  | (_, kind) when Kind.(kind = PERCENT) -> false
  | (kind, _) when Kind.(kind = PERCENT) -> false
  | (_, kind) when Kind.(kind = RPAREN || kind = RBRACKET || kind = RBRACE) -> false
  | (kind, _) when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACE) -> false
  | (kind, _) when Kind.(kind = AT || kind = ATAT) -> false
  | (_, kind) when Kind.(kind = DOT) -> false
  | (kind, _) when Kind.(kind = DOT) -> false
  | _ -> true

let declaration_head_tokens_doc = fun tokens ->
  let rec loop previous acc = function
    | [] -> acc
    | token :: rest ->
        let current = Ast.Token.kind token in
        let piece = token_doc token in
        let acc =
          match previous with
          | Some previous when declaration_head_token_needs_space previous current -> Doc.concat
            [ acc; Doc.space; piece ]
          | _ -> Doc.concat [ acc; piece ]
        in
        loop (Some current) acc rest
  in
  loop None Doc.empty tokens

let split_top_level_arrows = fun tokens ->
  let rec loop current groups depth = function
    | [] -> List.reverse ((List.reverse current, false) :: groups)
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.ARROW -> loop
      []
      ((List.reverse current, true) :: groups)
      depth
      rest
    | token :: rest -> loop (token :: current) groups (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let top_level_arrow_count = fun tokens ->
  let rec loop depth count = function
    | [] -> count
    | token :: rest ->
        let count =
          if Int.equal depth 0 && token_kind_is token Kind.ARROW then
            Int.(count + 1)
          else
            count
        in
        loop (type_token_depth_after depth token) count rest
  in
  loop 0 0 tokens

let tokens_have_long_text = fun ~minimum tokens ->
  List.any tokens ~fn:(fun token -> Int.(String.length (Ast.Token.text token) >= minimum))

let tokens_text_width = fun tokens ->
  let rec loop previous width = function
    | [] -> width
    | token :: rest ->
        let current = Ast.Token.kind token in
        let width = width + String.length (Ast.Token.text token) + match previous with
        | Some previous when type_token_needs_space previous current -> 1
        | _ -> 0
        in
        loop (Some current) width rest
  in
  loop None 0 tokens

let multiline_top_level_arrow_type_threshold = 4

let strip_enclosing_token = fun ~opening ~closing tokens ->
  match tokens with
  | first :: rest when token_kind_is first opening ->
      let rec strip acc = function
        | [] -> None
        | [ last ] when token_kind_is last closing -> Some (List.reverse acc)
        | token :: rest -> strip (token :: acc) rest
      in
      strip [] rest
  | _ -> None

let rec type_tokens_doc_with_threshold = fun ~threshold tokens ->
  match object_type_tokens_doc ~threshold tokens with
  | Some doc ->
      doc
  | None when let arrow_count = top_level_arrow_count tokens in
  Int.(arrow_count > threshold)
  || (Int.(arrow_count >= 2) && Int.(tokens_text_width tokens > 80))
  || (Int.(arrow_count >= 4) && Int.(tokens_text_width tokens > 70)) ->
      let parts = split_top_level_arrows tokens in
      let rec render = function
        | [] -> Doc.empty
        | [ (tokens, false) ] -> type_tokens_inline_doc tokens
        | (tokens, true) :: rest -> Doc.concat
          [ type_tokens_inline_doc tokens; Doc.space; Doc.arrow; Doc.line; render rest ]
        | (tokens, false) :: rest -> Doc.concat
          [ type_tokens_inline_doc tokens; Doc.line; render rest ]
      in
      render parts
  | None ->
      type_tokens_inline_doc tokens

and object_type_field_doc = fun ~threshold tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = COLON)) with
  | Some (name_tokens, _colon, type_tokens) ->
      let type_doc = type_tokens_doc_with_threshold ~threshold type_tokens in
      if Doc.is_multiline type_doc then
        Doc.concat
          [ declaration_head_tokens_doc name_tokens; Doc.colon; Doc.line; Doc.indent 2 type_doc; ]
      else
        Doc.concat [ declaration_head_tokens_doc name_tokens; Doc.colon; Doc.space; type_doc ]
  | None -> type_tokens_inline_doc tokens

and object_type_tokens_doc = fun ~threshold tokens ->
  match strip_enclosing_token ~opening:Kind.LT ~closing:Kind.GT tokens with
  | None -> None
  | Some fields_tokens ->
      let fields =
        split_top_level_all fields_tokens ~matches:(fun kind -> Kind.(kind = SEMI))
      in
      let field_threshold =
        if Int.(threshold > 1) then
          1
        else
          threshold
      in
      let field_docs = List.map fields ~fn:(object_type_field_doc ~threshold:field_threshold) in
      Some (Doc.concat
        [ Doc.text "<"; Doc.line; Doc.indent 2 (Doc.lines field_docs); Doc.line; Doc.text ">"; ])

let type_tokens_doc = type_tokens_doc_with_threshold ~threshold:multiline_top_level_arrow_type_threshold

let value_decl_type_tokens_doc = type_tokens_doc_with_threshold ~threshold:3

let field_type_breaks_after_colon = fun tokens ->
  let rec contains_and = function
    | [] -> false
    | token :: rest -> token_kind_is token Kind.AND_KW || contains_and rest
  in
  match tokens with
  | opening :: module_token :: _ when token_kind_is opening Kind.LPAREN
  && token_kind_is module_token Kind.MODULE_KW -> contains_and tokens
  | _ -> false

let trailing_field_comment_doc = function
  | Some token when token_has_inline_leading_comment token -> Some (Doc.concat
    [ Doc.spaces 2; inline_leading_comment_token_doc token ])
  | _ -> None

let record_field_doc = fun ~inline ?trailing_comment tokens ->
  let field_doc =
    match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = COLON)) with
    | Some (name_tokens, _colon, type_tokens) ->
        let type_doc = type_tokens_doc_with_threshold ~threshold:1 type_tokens in
        Doc.concat
          [ type_tokens_inline_doc name_tokens; Doc.colon; (
              if
                (not inline)
                && (field_type_breaks_after_colon type_tokens || Doc.is_multiline type_doc)
              then
                Doc.concat [ Doc.line; Doc.indent 2 type_doc ]
              else
                Doc.concat [ Doc.space; type_doc ]
            ); (
              if inline then
                Doc.empty
              else
                Doc.semi
            ); (
              match trailing_comment with
              | Some trailing_comment -> trailing_comment
              | None -> Doc.empty
            ); ]
    | None -> type_tokens_inline_doc tokens
  in
  match tokens with
  | first :: _ when (not inline)
  && Ast.Token.has_leading_comment first
  && not (token_has_inline_leading_comment first) -> Doc.concat
    [ trimmed_leading_comment_token_doc first; Doc.line; field_doc ]
  | _ -> field_doc

let record_body_parts = fun tokens ->
  let tokens, closing =
    match tokens with
    | opening :: rest when token_kind_is opening Kind.LBRACE ->
        let rec strip_closing acc = function
          | [] -> (List.reverse acc, None)
          | [ closing ] when token_kind_is closing Kind.RBRACE -> (List.reverse acc, Some closing)
          | token :: rest -> strip_closing (token :: acc) rest
        in
        strip_closing [] rest
    | _ -> (tokens, None)
  in
  (split_top_level_all tokens ~matches:(fun kind -> Kind.(kind = SEMI)), closing)

let record_body_field_groups = fun tokens ->
  let fields, _closing = record_body_parts tokens in
  fields

let record_body_closing_comment_doc = function
  | Some closing when Ast.Token.has_leading_comment closing
  && Option.is_none (trailing_field_comment_doc (Some closing)) -> Some (trimmed_leading_comment_token_doc
    closing)
  | _ -> None

let record_body_has_standalone_closing_comment = fun tokens ->
  let _fields, closing = record_body_parts tokens in
  Option.is_some (record_body_closing_comment_doc closing)

let first_token = function
  | token :: _ -> Some token
  | [] -> None

let record_field_docs = fun ~inline fields closing ->
  let rec loop = function
    | [] -> []
    | [ tokens ] -> [
      record_field_doc ~inline ?trailing_comment:(trailing_field_comment_doc closing) tokens
    ]
    | tokens :: (next :: _ as rest) -> record_field_doc
      ~inline
      ?trailing_comment:(trailing_field_comment_doc (first_token next))
      tokens
    :: loop rest
  in
  loop fields

let record_body_doc = fun ~inline tokens ->
  let field_groups, closing = record_body_parts tokens in
  let fields = record_field_docs ~inline field_groups closing in
  match inline with
  | true -> Doc.concat
    [
      Doc.lbrace;
      Doc.space;
      Doc.join (Doc.concat [ Doc.semi; Doc.space ]) fields;
      Doc.space;
      Doc.rbrace
    ]
  | false ->
      Doc.concat
        [ Doc.lbrace; Doc.line; Doc.indent 2
            (
              Doc.lines
                (
                  match record_body_closing_comment_doc closing with
                  | Some closing_comment -> fields @ [ closing_comment ]
                  | None -> fields
                )
            ); Doc.line; Doc.rbrace; ]

let inline_constructor_payload = fun tokens ->
  Int.(List.length (record_body_field_groups tokens) <= 3)
  && not (record_body_has_standalone_closing_comment tokens)

let split_variant_constructors = fun tokens ->
  let rec loop current constructors depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> constructors
            | _ -> List.reverse current :: constructors
          )
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.PIPE ->
        let constructors =
          match current with
          | [] -> constructors
          | _ -> List.reverse current :: constructors
        in
        loop [ token ] constructors depth rest
    | token :: rest ->
        loop (token :: current) constructors (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let token_is_capitalized_ident = fun token ->
  if not (token_kind_is token Kind.IDENT) then
    false
  else
    let text = Ast.Token.text token in
    if Int.(String.length text = 0) then
      false
    else
      match String.get_unchecked text ~at:0 with
      | 'A' .. 'Z' -> true
      | _ -> false

let tokens_look_like_bare_variant_body = fun tokens ->
  match tokens with
  | first :: _ when token_is_capitalized_ident first -> not
    (Option.is_some (split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = DOT))))
  | _ -> false

let poly_variant_row_doc = fun ~bar tokens ->
  let row =
    match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = OF_KW)) with
    | Some (tag_tokens, of_token, payload_tokens) -> Doc.concat
      [
        type_tokens_inline_doc tag_tokens;
        Doc.space;
        token_doc of_token;
        Doc.space;
        type_tokens_doc payload_tokens;
      ]
    | None -> type_tokens_doc tokens
  in
  if bar then
    Doc.concat [ Doc.bar; Doc.space; row ]
  else
    row

let poly_variant_body_doc = fun tokens ->
  let opener, rest =
    match tokens with
    | opening :: marker :: rest when token_kind_is opening Kind.LBRACKET
    && (token_kind_is marker Kind.GT || token_kind_is marker Kind.LT) -> (
      Doc.concat [ token_doc opening; token_doc marker ],
      rest
    )
    | opening :: rest when token_kind_is opening Kind.LBRACKET -> (token_doc opening, rest)
    | _ -> (Doc.lbracket, tokens)
  in
  let rec strip_closing acc = function
    | [] -> List.reverse acc
    | [ closing ] when token_kind_is closing Kind.RBRACKET -> List.reverse acc
    | token :: rest -> strip_closing (token :: acc) rest
  in
  let rows = split_variant_constructors (strip_closing [] rest) in
  let rec row_docs first = function
    | [] -> []
    | tokens :: rest ->
        let tokens =
          match tokens with
          | token :: rest when token_kind_is token Kind.PIPE -> rest
          | _ -> tokens
        in
        poly_variant_row_doc ~bar:(not first) tokens :: row_docs false rest
  in
  Doc.concat
    [ opener; Doc.line; Doc.indent 2 (Doc.lines (row_docs true rows)); Doc.line; Doc.rbracket; ]

let constructor_payload_doc = fun tokens ->
  match tokens with
  | opening :: _ when token_kind_is opening Kind.LBRACE ->
      if inline_constructor_payload tokens then
        record_body_doc ~inline:true tokens
      else
        Doc.indent 2 (record_body_doc ~inline:false tokens)
  | opening :: _ when token_kind_is opening Kind.LBRACKET -> poly_variant_body_doc tokens
  | _ -> type_tokens_doc tokens

let leading_parenthesized_type_group = fun tokens ->
  match tokens with
  | opening :: rest when token_kind_is opening Kind.LPAREN ->
      let rec loop depth inner = function
        | [] -> None
        | token :: rest when Int.equal depth 0 && token_kind_is token Kind.RPAREN -> Some (
          List.reverse inner,
          rest
        )
        | token :: rest -> loop (type_token_depth_after depth token) (token :: inner) rest
      in
      loop 0 [] rest
  | _ -> None

let leading_parenthesized_product_type_doc = fun tokens ->
  match leading_parenthesized_type_group tokens with
  | Some (inner_tokens, after_tokens) when Int.(tokens_text_width tokens > 40) -> (
      match split_top_level_token inner_tokens ~matches:(fun kind -> Kind.(kind = STAR)) with
      | Some (left_tokens, star_token, right_tokens) ->
          Some (
            Doc.concat
              [
                Doc.lparen;
                type_tokens_inline_doc left_tokens;
                Doc.space;
                token_doc star_token;
                Doc.line;
                type_tokens_inline_doc right_tokens;
                Doc.rparen;
                (
                  match after_tokens with
                  | [] -> Doc.empty
                  | tokens -> Doc.concat [ Doc.space; type_tokens_inline_doc tokens ]
                );
              ]
          )
      | None -> None
    )
  | _ -> None

let constructor_result_type_doc = fun tokens ->
  match tokens with
  | opening :: _ when token_kind_is opening Kind.LBRACE -> (
      let payload_doc tokens = record_body_doc ~inline:false tokens |> Doc.indent 2 in
      match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = ARROW)) with
      | Some (payload_tokens, arrow_token, result_tokens) -> Doc.concat
        [
          payload_doc payload_tokens;
          Doc.space;
          token_doc arrow_token;
          Doc.space;
          type_tokens_doc result_tokens;
        ]
      | None -> payload_doc tokens
    )
  | _ -> (
      match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = ARROW)) with
      | Some (payload_tokens, arrow_token, result_tokens) ->
          Doc.concat
            [ type_tokens_inline_doc payload_tokens; Doc.space; token_doc arrow_token; Doc.space; (
                match leading_parenthesized_product_type_doc result_tokens with
                | Some result_doc -> result_doc
                | None -> type_tokens_doc result_tokens
              ); ]
      | None -> type_tokens_doc tokens
    )

let constructor_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = COLON)) with
  | Some (name_tokens, _colon, type_tokens) ->
      let result_doc = constructor_result_type_doc type_tokens in
      let result_is_record =
        match type_tokens with
        | token :: _ -> token_kind_is token Kind.LBRACE
        | [] -> false
      in
      if
        (not result_is_record)
        && (Doc.is_multiline result_doc
        || (Int.(top_level_arrow_count type_tokens > 0) && tokens_have_long_text ~minimum:30 type_tokens))
      then
        Doc.concat
          [ type_tokens_inline_doc name_tokens; Doc.colon; Doc.line; Doc.indent 2 result_doc; ]
      else
        Doc.concat [ type_tokens_inline_doc name_tokens; Doc.colon; Doc.space; result_doc ]
  | None -> (
      match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = OF_KW)) with
      | Some (name_tokens, of_token, payload_tokens) -> Doc.concat
        [
          type_tokens_inline_doc name_tokens;
          Doc.space;
          token_doc of_token;
          Doc.space;
          constructor_payload_doc payload_tokens;
        ]
      | None -> type_tokens_inline_doc tokens
    )

let variant_body_doc = fun tokens ->
  let private_token, tokens =
    match tokens with
    | token :: rest when token_kind_is token Kind.PRIVATE_KW -> (Some token, rest)
    | _ -> (None, tokens)
  in
  let constructors = split_variant_constructors tokens in
  let rec row_docs first = function
    | [] -> []
    | tokens :: rest ->
        let pipe_token, tokens =
          match tokens with
          | token :: rest when token_kind_is token Kind.PIPE -> (Some token, rest)
          | _ -> (None, tokens)
        in
        let prefix =
          match private_token, first, pipe_token with
          | Some private_token, true, Some pipe_token -> Doc.concat
            [ token_doc private_token; Doc.space; token_doc pipe_token; Doc.space ]
          | Some private_token, true, None -> Doc.concat [ token_doc private_token; Doc.space ]
          | _, _, Some pipe_token -> Doc.concat [ token_doc pipe_token; Doc.space ]
          | _ -> Doc.empty
        in
        let row = Doc.concat [ prefix; constructor_doc tokens ] in
        let row =
          match pipe_token with
          | Some pipe_token when Ast.Token.has_leading_comment pipe_token -> Doc.concat
            [ trimmed_leading_comment_token_doc pipe_token; Doc.line; row ]
          | _ -> row
        in
        row :: row_docs false rest
  in
  Doc.concat [ Doc.line; Doc.indent 2 (Doc.lines (row_docs true constructors)) ]

let rec rendered_type_body = fun tokens ->
  match tokens with
  | [] ->
      { doc = Doc.empty; leading_line = false; break_after_equal = false }
  | token :: _ when token_kind_is token Kind.LBRACE ->
      { doc = record_body_doc ~inline:false tokens; leading_line = false; break_after_equal = false }
  | private_token :: opening :: rest when token_kind_is private_token Kind.PRIVATE_KW
  && token_kind_is opening Kind.LBRACE ->
      {
        doc = Doc.concat
          [ token_doc private_token; Doc.space; record_body_doc ~inline:false (opening :: rest) ];
        leading_line = false;
        break_after_equal = false
      }
  | token :: _ when token_kind_is token Kind.PIPE || tokens_look_like_bare_variant_body tokens ->
      { doc = variant_body_doc tokens; leading_line = true; break_after_equal = false }
  | private_token :: rest when token_kind_is private_token Kind.PRIVATE_KW && (
    match rest with
    | pipe :: _ when token_kind_is pipe Kind.PIPE -> true
    | _ -> tokens_look_like_bare_variant_body rest
  ) ->
      { doc = variant_body_doc tokens; leading_line = true; break_after_equal = false }
  | opening :: row_start :: _ when token_kind_is opening Kind.LBRACKET
  && token_kind_is row_start Kind.BACKTICK ->
      {
        doc = Doc.concat [ Doc.line; poly_variant_body_doc tokens ];
        leading_line = true;
        break_after_equal = false
      }
  | token :: _ when token_kind_is token Kind.LBRACKET ->
      { doc = poly_variant_body_doc tokens; leading_line = false; break_after_equal = false }
  | token :: _ when token_kind_is token Kind.LT ->
      { doc = type_tokens_doc tokens; leading_line = false; break_after_equal = false }
  | _ ->
      let doc = type_tokens_doc tokens in
      {
        doc;
        leading_line = false;
        break_after_equal = Doc.is_multiline doc
        && Int.(top_level_arrow_count tokens > multiline_top_level_arrow_type_threshold)
      }

let split_constraints = fun tokens ->
  let rec collect_constraints current constraints depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> constraints
            | _ -> List.reverse current :: constraints
          )
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.CONSTRAINT_KW ->
        collect_constraints []
          (
            match current with
            | [] -> constraints
            | _ -> List.reverse current :: constraints
          )
          depth
          rest
    | token :: rest -> collect_constraints
      (token :: current)
      constraints
      (type_token_depth_after depth token)
      rest
  in
  let rec loop body depth = function
    | [] -> (List.reverse body, [])
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.CONSTRAINT_KW -> (
      List.reverse body,
      collect_constraints [] [] depth rest
    )
    | token :: rest -> loop (token :: body) (type_token_depth_after depth token) rest
  in
  loop [] 0 tokens

let constraint_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (left, _eq, right) -> Doc.concat
    [ Doc.text "constraint"; Doc.space; type_tokens_doc left; Doc.equal; type_tokens_doc right ]
  | None -> Doc.concat [ Doc.text "constraint"; Doc.space; type_tokens_doc tokens ]

let append_type_constraints = fun doc constraints ->
  match constraints with
  | [] -> doc
  | constraints -> Doc.concat
    (doc
    :: (constraints
    |> List.map ~fn:(fun tokens -> Doc.concat [ Doc.line; Doc.indent 2 (constraint_doc tokens) ])))

let render_type_body_after_equal = fun head body constraints ->
  let body = rendered_type_body body in
  let doc =
    if body.leading_line then
      Doc.concat [ head; Doc.space; Doc.equal; body.doc ]
    else if body.break_after_equal then
      Doc.concat [ head; Doc.space; Doc.equal; Doc.line; Doc.indent 2 body.doc ]
    else
      Doc.concat [ head; Doc.space; Doc.equal; Doc.space; body.doc ]
  in
  append_type_constraints doc constraints

let type_member_head_doc = fun member_ ->
  match member_.name with
  | None -> unsupported "type declaration without name"
  | Some name ->
      let keyword_doc = token_doc member_.type_keyword in
      Doc.concat
        [ keyword_doc; Doc.space; (
            match member_.nonrec_token with
            | Some nonrec_token -> Doc.concat [ token_doc nonrec_token; Doc.space ]
            | None -> Doc.empty
          ); type_member_parameters_doc member_.parameters; token_doc name; ]

let type_member_doc = fun member_ ->
  let head = type_member_head_doc member_ in
  let body_tokens, constraints = split_constraints member_.body_tokens in
  match body_tokens with
  | [] -> append_type_constraints head constraints
  | _ -> (
      match split_top_level_token body_tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
      | Some (alias_tokens, _eq, representation_tokens) ->
          let representation = rendered_type_body representation_tokens in
          let doc =
            if representation.leading_line then
              Doc.concat
                [
                  head;
                  Doc.space;
                  Doc.equal;
                  Doc.space;
                  type_tokens_doc alias_tokens;
                  Doc.space;
                  Doc.equal;
                  representation.doc;
                ]
            else
              Doc.concat
                [
                  head;
                  Doc.space;
                  Doc.equal;
                  Doc.space;
                  type_tokens_doc alias_tokens;
                  Doc.space;
                  Doc.equal;
                  Doc.space;
                  representation.doc;
                ]
          in
          append_type_constraints doc constraints
      | None -> render_type_body_after_equal head body_tokens constraints
    )

let type_decl_members = fun tokens ->
  let rec loop current members depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> members
            | _ -> parse_type_member_header (List.reverse current) :: members
          )
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.AND_KW ->
        let members =
          match current with
          | [] -> members
          | _ -> parse_type_member_header (List.reverse current) :: members
        in
        loop [ token ] members depth rest
    | token :: rest ->
        loop (token :: current) members (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let split_type_extension_operator = fun tokens ->
  let rec loop before depth = function
    | plus_token :: eq_token :: rest when Int.equal depth 0
    && token_kind_is plus_token Kind.PLUS
    && token_kind_is eq_token Kind.EQ -> Some (List.reverse before, plus_token, eq_token, rest)
    | token :: rest -> loop (token :: before) (type_token_depth_after depth token) rest
    | [] -> None
  in
  loop [] 0 tokens

let type_extension_doc = fun before plus_token eq_token body_tokens ->
  let member_ = parse_type_member_header before in
  let head = type_member_head_doc member_ in
  let operator = Doc.concat [ token_doc plus_token; token_doc eq_token ] in
  match body_tokens with
  | [] -> Doc.concat [ head; Doc.space; operator ]
  | token :: _ when token_kind_is token Kind.PIPE -> Doc.concat
    [ head; Doc.space; operator; variant_body_doc body_tokens ]
  | _ -> Doc.concat [ head; Doc.space; operator; Doc.space; type_tokens_doc body_tokens ]

let type_decl_doc = fun decl ->
  let tokens = type_decl_tokens decl in
  match split_type_extension_operator tokens with
  | Some (before, plus_token, eq_token, body_tokens) -> type_extension_doc
    before
    plus_token
    eq_token
    body_tokens
  | None -> (
      match type_decl_members tokens with
      | [] -> unsupported "empty type declaration"
      | first :: rest ->
          let first_doc = type_member_doc first in
          let rest_docs = rest
          |> List.map
            ~fn:(fun member_ ->
              Doc.concat
                [
                  blank_line;
                  leading_comment_token_doc member_.type_keyword;
                  type_member_doc member_
                ]) in
          Doc.concat (first_doc :: rest_docs)
    )

let module_token_depth_after = fun depth token ->
  let decrease depth =
    if Int.(depth <= 0) then
      0
    else
      Int.(depth - 1)
  in
  match Ast.Token.kind token with
  | kind when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACE) -> Int.(depth + 1)
  | kind when Kind.(kind = STRUCT_KW || kind = SIG_KW || kind = BEGIN_KW) -> Int.(depth + 1)
  | kind when Kind.(kind = RPAREN || kind = RBRACKET || kind = RBRACE) -> decrease depth
  | kind when Kind.(kind = END_KW) -> decrease depth
  | _ -> depth

let module_token_needs_space = fun ~depth previous current ->
  match previous, current with
  | (_, kind) when Kind.(kind = RPAREN || kind = RBRACKET || kind = RBRACE) -> false
  | (_, kind) when Kind.(kind = COMMA) -> false
  | (_, kind) when Kind.(kind = DOT) -> false
  | (_, kind) when Kind.(kind = COLON) && Int.equal depth 0 -> false
  | (_, kind) when Kind.(kind = AT || kind = ATAT || kind = PERCENT) -> false
  | (kind, _) when Kind.(kind = LPAREN || kind = LBRACKET || kind = LBRACE) -> false
  | (kind, _) when Kind.(kind = DOT) -> false
  | (kind, _) when Kind.(kind = AT || kind = ATAT || kind = PERCENT) -> false
  | _ -> true

let module_tokens_doc = fun tokens ->
  let rec loop depth previous acc = function
    | [] -> acc
    | token :: rest ->
        let current = Ast.Token.kind token in
        let piece = literal_token_doc token in
        let acc =
          match previous with
          | Some previous when module_token_needs_space ~depth previous current -> Doc.concat
            [ acc; Doc.space; piece ]
          | _ -> Doc.concat [ acc; piece ]
        in
        loop (module_token_depth_after depth token) (Some current) acc rest
  in
  loop 0 None Doc.empty tokens

let module_expr_tokens_doc = fun tokens ->
  match split_top_level_all tokens ~matches:(fun kind -> Kind.(kind = COMMA)) with
  | []
  | [ _ ] -> module_tokens_doc tokens
  | parts -> Doc.concat
    [
      Doc.lparen;
      Doc.join (Doc.concat [ Doc.comma; Doc.space ]) (List.map parts ~fn:module_tokens_doc);
      Doc.rparen;
    ]

let module_after_tokens_doc = fun tokens ->
  match tokens with
  | [] -> Doc.empty
  | token :: rest when starts_attribute_suffix_tokens token rest -> Doc.concat
    [ Doc.space; attribute_shell_tokens_doc tokens ]
  | token :: rest when starts_extension_item_tokens token rest -> Doc.concat
    [ Doc.space; extension_shell_tokens_doc tokens ]
  | _ -> Doc.concat [ Doc.space; module_tokens_doc tokens ]

let starts_module_signature_body_item = fun token rest ->
  let kind = Ast.Token.kind token in
  Kind.(kind = VAL_KW
  || kind = TYPE_KW
  || kind = MODULE_KW
  || kind = OPEN_KW
  || kind = INCLUDE_KW
  || kind = EXTERNAL_KW
  || kind = EXCEPTION_KW
  || kind = CLASS_KW)
  || starts_shell_body_item_tokens token rest

let continues_compound_module_signature_item_head = fun current token ->
  token_kind_is token Kind.TYPE_KW && match current with
  | previous :: [] -> token_kind_is previous Kind.MODULE_KW || token_kind_is previous Kind.CLASS_KW
  | _ -> false

let split_module_signature_body_items = fun tokens ->
  let rec loop current items depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> items
            | _ -> List.reverse current :: items
          )
    | token :: rest when Int.(depth = 0)
    && starts_module_signature_body_item token rest
    && not (continues_compound_module_signature_item_head current token)
    && not (List.is_empty current) -> loop
      [ token ]
      (List.reverse current :: items)
      (type_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) items (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let module_signature_body_type_item_doc = fun tokens ->
  match type_decl_members tokens with
  | [] -> unsupported "empty module signature type item"
  | first :: rest ->
      let first_doc = type_member_doc first in
      let rest_docs = rest
      |> List.map
        ~fn:(fun member_ ->
          Doc.concat
            [ blank_line; leading_comment_token_doc member_.type_keyword; type_member_doc member_; ]) in
      Doc.concat (first_doc :: rest_docs)

let module_signature_body_val_item_doc = fun tokens ->
  match tokens with
  | val_token :: rest when token_kind_is val_token Kind.VAL_KW -> (
      match split_top_level_token rest ~matches:(fun kind -> Kind.(kind = COLON)) with
      | Some (name_tokens, _colon, type_tokens) ->
          Doc.concat
            [ token_doc val_token; Doc.space; declaration_name_doc name_tokens; Doc.colon; (
                let type_doc = value_decl_type_tokens_doc type_tokens in
                if Doc.is_multiline type_doc then
                  Doc.concat [ Doc.line; Doc.indent 2 type_doc ]
                else
                  Doc.concat [ Doc.space; type_doc ]
              ); ]
      | None -> type_tokens_inline_doc tokens
    )
  | _ -> type_tokens_inline_doc tokens

let module_signature_body_equals_item_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (head_tokens, equals_token, body_tokens) -> Doc.concat
    [
      declaration_head_tokens_doc head_tokens;
      Doc.space;
      token_doc equals_token;
      Doc.space;
      type_tokens_doc body_tokens;
    ]
  | None -> declaration_head_tokens_doc tokens

let module_signature_body_item_body_doc = fun tokens ->
  match tokens with
  | token :: _ when token_kind_is token Kind.TYPE_KW -> module_signature_body_type_item_doc tokens
  | token :: _ when token_kind_is token Kind.VAL_KW -> module_signature_body_val_item_doc tokens
  | token :: rest when starts_floating_attribute_item_tokens token rest -> attribute_shell_tokens_doc
    tokens
  | token :: rest when starts_extension_item_tokens token rest -> extension_shell_tokens_doc tokens
  | first :: second :: _ when token_kind_is first Kind.MODULE_KW && token_kind_is second Kind.TYPE_KW -> module_signature_body_equals_item_doc
    tokens
  | first :: second :: _ when token_kind_is first Kind.CLASS_KW && token_kind_is second Kind.TYPE_KW -> module_signature_body_equals_item_doc
    tokens
  | _ -> type_tokens_inline_doc tokens

let module_signature_body_item_doc = fun tokens ->
  match tokens with
  | first :: _ when token_has_inline_leading_comment_after_horizontal first -> module_signature_body_item_body_doc
    tokens
  | first :: _ ->
      Doc.concat
        [ (
            if
              token_kind_is first Kind.VAL_KW
              && token_has_leading_markdown_section_docstring first
              && not (token_has_leading_nonsection_comment first)
            then
              leading_comment_token_doc ~compact_trailing_blank:true first
            else
              leading_comment_token_paragraph_doc first
          ); module_signature_body_item_body_doc tokens; ]
  | [] -> module_signature_body_item_body_doc tokens

let module_signature_body_item_is_type = function
  | token :: _ -> token_kind_is token Kind.TYPE_KW
  | [] -> false

let module_signature_body_item_is_open = function
  | token :: _ -> token_kind_is token Kind.OPEN_KW
  | [] -> false

let module_signature_body_item_is_value = function
  | token :: _ -> token_kind_is token Kind.VAL_KW
  | [] -> false

let module_signature_body_item_has_leading_nonsection_comment = function
  | token :: _ -> token_has_leading_nonsection_comment token
  | [] -> false

let module_signature_body_item_has_mixed_leading_section_docstrings = fun tokens ->
  match tokens with
  | [] -> false
  | token :: _ ->
      let has_section = ref false in
      let has_nonsection = ref false in
      Ast.Token.for_each_leading_trivia token
        ~fn:(fun ~kind ~text ->
          if Kind.(kind = DOCSTRING) then
            if is_section_docstring_text text then
              has_section := true
            else
              has_nonsection := true);
      !has_section && !has_nonsection

let module_signature_body_items_compact_between = fun left right ->
  (module_signature_body_item_is_type left
  && module_signature_body_item_is_type right
  && not (module_signature_body_item_has_mixed_leading_section_docstrings right))
  || (module_signature_body_item_is_open left && module_signature_body_item_is_open right)
  || (module_signature_body_item_is_type left
  && module_signature_body_item_is_value right
  && not (module_signature_body_item_has_leading_nonsection_comment right))

let module_signature_body_items_doc = fun items ->
  let rec loop previous doc = function
    | [] -> doc
    | next :: rest ->
        let separator =
          match next with
          | first :: _ when module_signature_body_item_is_value previous
          && token_has_inline_leading_comment_after_horizontal first -> Doc.concat
            [ Doc.space; inline_leading_comment_token_doc first; blank_line ]
          | _ when module_signature_body_items_compact_between previous next -> Doc.line
          | _ -> blank_line
        in
        loop next (Doc.concat [ doc; separator; module_signature_body_item_doc next ]) rest
  in
  match items with
  | [] -> Doc.empty
  | first :: rest -> loop first (module_signature_body_item_doc first) rest

let signature_body_doc_with_closing_leading_comment = fun body_doc end_token ->
  if token_has_inline_leading_comment_after_horizontal end_token then
    Doc.concat [ body_doc; Doc.space; inline_leading_comment_token_doc end_token ]
  else
    match trimmed_leading_comment_token_doc end_token with
    | Doc.Empty -> body_doc
    | closing_comment -> (
        match body_doc with
        | Doc.Empty -> closing_comment
        | body_doc -> Doc.concat [ body_doc; blank_line; closing_comment ]
      )

let module_signature_tokens_doc = fun sig_token body_tokens end_token ->
  let items = split_module_signature_body_items body_tokens in
  let body_doc = module_signature_body_items_doc items
  |> fun body_doc -> signature_body_doc_with_closing_leading_comment body_doc end_token in
  match items with
  | [] when Doc.is_multiline body_doc -> Doc.concat
    [ token_doc sig_token; Doc.line; Doc.indent 2 body_doc; Doc.line; token_doc end_token; ]
  | [] -> Doc.concat [ token_doc sig_token; Doc.space; token_doc end_token ]
  | _ -> Doc.concat
    [ token_doc sig_token; Doc.line; Doc.indent 2 body_doc; Doc.line; token_doc end_token; ]

let split_signature_shell = fun tokens ->
  let rec take_body before sig_token body depth = function
    | [] -> None
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.END_KW -> Some (
      List.reverse before,
      sig_token,
      List.reverse body,
      token,
      rest
    )
    | token :: rest -> take_body
      before
      sig_token
      (token :: body)
      (module_token_depth_after depth token)
      rest
  in
  let rec find before depth = function
    | [] -> None
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.SIG_KW -> take_body
      before
      token
      []
      0
      rest
    | token :: rest -> find (token :: before) (module_token_depth_after depth token) rest
  in
  find [] 0 tokens

let module_head_tokens_doc = fun tokens ->
  match split_signature_shell tokens with
  | Some (before, sig_token, body_tokens, end_token, after) -> Doc.concat
    [
      module_tokens_doc before;
      Doc.space;
      module_signature_tokens_doc sig_token body_tokens end_token;
      (module_after_tokens_doc after);
    ]
  | None -> module_tokens_doc tokens

let starts_structure_body_item = fun token rest ->
  let kind = Ast.Token.kind token in
  Kind.(kind = LET_KW
  || kind = TYPE_KW
  || kind = MODULE_KW
  || kind = OPEN_KW
  || kind = INCLUDE_KW
  || kind = EXTERNAL_KW
  || kind = EXCEPTION_KW
  || kind = CLASS_KW)
  || starts_shell_body_item_tokens token rest

let continues_compound_structure_item_head = fun current token ->
  token_kind_is token Kind.TYPE_KW && match current with
  | previous :: [] -> token_kind_is previous Kind.MODULE_KW || token_kind_is previous Kind.CLASS_KW
  | _ -> false

let split_structure_body_items = fun tokens ->
  let rec loop current items depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> items
            | _ -> List.reverse current :: items
          )
    | token :: rest when Int.equal depth 0
    && starts_structure_body_item token rest
    && not (continues_compound_structure_item_head current token)
    && not (List.is_empty current) -> loop
      [ token ]
      (List.reverse current :: items)
      (module_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) items (module_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let structure_body_doc_with_closing_leading_comment = fun body_doc end_token ->
  match trimmed_leading_comment_token_doc end_token with
  | Doc.Empty -> body_doc
  | closing_comment -> (
      match body_doc with
      | Doc.Empty -> closing_comment
      | body_doc -> Doc.concat [ body_doc; blank_line; closing_comment ]
    )

let rec module_member_doc = fun ?(force_empty_struct_multiline = false) tokens ->
  match split_module_struct_body tokens with
  | Some (head_tokens, struct_token, body_tokens, end_token, after) ->
      Doc.concat
        [ module_head_tokens_doc head_tokens; Doc.space; token_doc struct_token; (
            match split_structure_body_items body_tokens with
            | [] ->
                let body_doc = structure_body_doc_with_closing_leading_comment Doc.empty end_token in
                (
                  match body_doc with
                  | Doc.Empty when force_empty_struct_multiline -> blank_line
                  | Doc.Empty -> Doc.space
                  | body_doc -> Doc.concat [ Doc.line; Doc.indent 2 body_doc; Doc.line ]
                )
            | items ->
                let body_doc = structure_body_items_doc items
                |> fun body_doc -> structure_body_doc_with_closing_leading_comment body_doc end_token in
                Doc.concat [ Doc.line; Doc.indent 2 body_doc; Doc.line; ]
          ); token_doc end_token; module_after_tokens_doc after; ]
  | None -> (
      match split_signature_shell tokens with
      | Some _ -> module_head_tokens_doc tokens
      | None -> module_expr_tokens_doc tokens
    )

and split_module_struct_body = fun tokens ->
  let rec take_body before struct_token body depth = function
    | [] -> None
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.END_KW -> Some (
      List.reverse before,
      struct_token,
      List.reverse body,
      token,
      rest
    )
    | token :: rest -> take_body
      before
      struct_token
      (token :: body)
      (module_token_depth_after depth token)
      rest
  in
  let rec find before depth = function
    | [] -> None
    | token :: rest when Int.equal depth 0 && token_kind_is token Kind.STRUCT_KW -> take_body
      before
      token
      []
      0
      rest
    | token :: rest -> find (token :: before) (module_token_depth_after depth token) rest
  in
  find [] 0 tokens

and structure_body_item_is_type = function
  | token :: _ -> token_kind_is token Kind.TYPE_KW
  | [] -> false

and structure_body_item_is_floating_attribute = function
  | token :: rest -> starts_floating_attribute_item_tokens token rest
  | [] -> false

and structure_body_items_separator = fun left right ->
  if structure_body_item_is_type left && structure_body_item_is_floating_attribute right then
    Doc.line
  else
    blank_line

and structure_body_items_doc = fun items ->
  let rec loop previous doc = function
    | [] -> doc
    | next :: rest ->
        let separator = structure_body_items_separator previous next in
        loop next (Doc.concat [ doc; separator; structure_body_item_doc next ]) rest
  in
  match items with
  | [] -> Doc.empty
  | first :: rest -> loop first (structure_body_item_doc first) rest

and structure_body_item_doc = fun tokens ->
  let body =
    match tokens with
    | token :: _ when token_kind_is token Kind.TYPE_KW -> module_signature_body_type_item_doc tokens
    | token :: _ when token_kind_is token Kind.LET_KW -> structure_body_let_item_doc tokens
    | token :: _ when token_kind_is token Kind.MODULE_KW -> module_member_doc tokens
    | token :: rest when starts_floating_attribute_item_tokens token rest -> attribute_shell_tokens_doc
      tokens
    | token :: rest when starts_extension_item_tokens token rest -> extension_shell_tokens_doc tokens
    | _ -> module_tokens_doc tokens
  in
  match tokens with
  | first :: _ -> Doc.concat [ leading_comment_token_paragraph_doc first; body ]
  | [] -> body

and structure_body_expr_tokens_doc = fun tokens ->
  match tokens with
  | if_token :: rest when token_kind_is if_token Kind.IF_KW -> (
      match split_top_level_token rest ~matches:(fun kind -> Kind.(kind = THEN_KW)) with
      | Some (condition_tokens, then_token, then_else_tokens) -> (
          match split_top_level_token then_else_tokens ~matches:(fun kind -> Kind.(kind = ELSE_KW)) with
          | Some (then_tokens, else_token, else_tokens) ->
              Doc.concat
                [
                  token_doc if_token;
                  Doc.space;
                  module_expr_tokens_doc condition_tokens;
                  Doc.space;
                  token_doc then_token;
                  Doc.line;
                  Doc.indent 2 (module_expr_tokens_doc then_tokens);
                  Doc.line;
                  token_doc else_token;
                  Doc.line;
                  Doc.indent 2 (module_expr_tokens_doc else_tokens);
                ]
          | None -> module_expr_tokens_doc tokens
        )
      | None -> module_expr_tokens_doc tokens
    )
  | _ -> module_expr_tokens_doc tokens

and structure_body_let_item_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (head_tokens, equals_token, body_tokens) ->
      if match body_tokens with
        | token :: _ -> token_kind_is token Kind.IF_KW
        | [] -> false then
        Doc.concat
          [
            module_tokens_doc head_tokens;
            Doc.space;
            token_doc equals_token;
            Doc.line;
            Doc.indent 2 (structure_body_expr_tokens_doc body_tokens);
          ]
      else
        Doc.concat
          [
            module_tokens_doc head_tokens;
            Doc.space;
            token_doc equals_token;
            Doc.space;
            structure_body_expr_tokens_doc body_tokens;
          ]
  | None -> module_tokens_doc tokens

let split_module_members = fun tokens ->
  let rec loop current members depth seen_struct_body = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> members
            | _ -> List.reverse current :: members
          )
    | token :: rest when Int.equal depth 0 && seen_struct_body && token_kind_is token Kind.AND_KW ->
        loop
          [ token ]
          (List.reverse current :: members)
          (module_token_depth_after depth token)
          false
          rest
    | token :: rest ->
        let seen_struct_body =
          seen_struct_body || (Int.equal depth 0 && token_kind_is token Kind.STRUCT_KW) in
        loop (token :: current) members (module_token_depth_after depth token) seen_struct_body rest
  in
  loop [] [] 0 false tokens

let module_declaration_tokens_doc = fun ?(force_empty_struct_multiline = false) tokens ->
  match split_module_members tokens with
  | [] -> Doc.empty
  | [ member_ ] -> module_member_doc ~force_empty_struct_multiline member_
  | members -> Doc.join blank_line (List.map members ~fn:module_member_doc)

let module_decl_tokens = fun decl ->
  let tokens = ref [] in
  Ast.Node.for_each_child_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let module_type_decl_tokens = fun decl ->
  let tokens = ref [] in
  Ast.Node.for_each_child_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let module_decl_path_body_doc = fun decl ->
  let segments = ref [] in
  Ast.ModuleDeclaration.for_each_body_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "module declaration path body without identifiers"
  | segments -> Doc.join (Doc.text ".") segments

let module_type_decl_path_body_doc = fun decl ->
  let segments = ref [] in
  Ast.ModuleTypeDeclaration.for_each_body_path_ident
    decl
    ~fn:(fun token -> segments := token_doc token :: !segments);
  match List.reverse !segments with
  | [] -> unsupported "module type declaration path body without identifiers"
  | segments -> Doc.join (Doc.text ".") segments

let module_type_decl_sig_body_tokens = fun decl ->
  let tokens = ref [] in
  Ast.ModuleTypeDeclaration.for_each_sig_body_token
    decl
    ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let module_decl_sig_body_tokens = fun decl ->
  let tokens = ref [] in
  Ast.ModuleDeclaration.for_each_sig_body_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let module_type_decl_head_tokens = fun decl ->
  let tokens = ref [] in
  Ast.ModuleTypeDeclaration.for_each_head_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let module_type_decl_head_doc = fun decl ->
  match module_type_decl_head_tokens decl with
  | [] -> unsupported "module type declaration without head tokens"
  | tokens -> declaration_head_tokens_doc tokens

let starts_signature_body_item = fun token rest ->
  let kind = Ast.Token.kind token in
  Kind.(kind = VAL_KW
  || kind = TYPE_KW
  || kind = MODULE_KW
  || kind = OPEN_KW
  || kind = INCLUDE_KW
  || kind = EXTERNAL_KW
  || kind = EXCEPTION_KW
  || kind = CLASS_KW)
  || starts_shell_body_item_tokens token rest

let continues_compound_signature_item_head = fun current token ->
  token_kind_is token Kind.TYPE_KW && match current with
  | previous :: [] -> token_kind_is previous Kind.MODULE_KW || token_kind_is previous Kind.CLASS_KW
  | _ -> false

let split_signature_body_items = fun tokens ->
  let rec loop current items depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> items
            | _ -> List.reverse current :: items
          )
    | token :: rest when Int.(depth = 0)
    && starts_signature_body_item token rest
    && not (continues_compound_signature_item_head current token)
    && not (List.is_empty current) -> loop
      [ token ]
      (List.reverse current :: items)
      (type_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) items (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let signature_body_type_item_doc = fun tokens ->
  match type_decl_members tokens with
  | [] -> unsupported "empty signature type item"
  | first :: rest ->
      let first_doc = type_member_doc first in
      let rest_docs = rest
      |> List.map
        ~fn:(fun member_ ->
          Doc.concat
            [ blank_line; leading_comment_token_doc member_.type_keyword; type_member_doc member_; ]) in
      Doc.concat (first_doc :: rest_docs)

let signature_body_val_item_doc = fun tokens ->
  match tokens with
  | val_token :: rest when token_kind_is val_token Kind.VAL_KW -> (
      match split_top_level_token rest ~matches:(fun kind -> Kind.(kind = COLON)) with
      | Some (name_tokens, _colon, type_tokens) ->
          let type_doc = value_decl_type_tokens_doc type_tokens in
          Doc.concat
            [ token_doc val_token; Doc.space; declaration_name_doc name_tokens; Doc.colon; (
                if Doc.is_multiline type_doc then
                  Doc.concat [ Doc.line; Doc.indent 2 type_doc ]
                else
                  Doc.concat [ Doc.space; type_doc ]
              ); ]
      | None -> type_tokens_inline_doc tokens
    )
  | _ -> type_tokens_inline_doc tokens

let signature_body_equals_item_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (head_tokens, equals_token, body_tokens) -> Doc.concat
    [
      declaration_head_tokens_doc head_tokens;
      Doc.space;
      token_doc equals_token;
      Doc.space;
      type_tokens_doc body_tokens;
    ]
  | None -> declaration_head_tokens_doc tokens

let signature_body_item_body_doc = fun tokens ->
  match tokens with
  | token :: _ when token_kind_is token Kind.TYPE_KW -> signature_body_type_item_doc tokens
  | token :: _ when token_kind_is token Kind.VAL_KW -> signature_body_val_item_doc tokens
  | token :: rest when starts_floating_attribute_item_tokens token rest -> attribute_shell_tokens_doc
    tokens
  | token :: rest when starts_extension_item_tokens token rest -> extension_shell_tokens_doc tokens
  | first :: second :: _ when token_kind_is first Kind.MODULE_KW && token_kind_is second Kind.TYPE_KW -> signature_body_equals_item_doc
    tokens
  | first :: second :: _ when token_kind_is first Kind.CLASS_KW && token_kind_is second Kind.TYPE_KW -> signature_body_equals_item_doc
    tokens
  | _ -> type_tokens_inline_doc tokens

let signature_body_item_doc = fun tokens ->
  match tokens with
  | first :: _ ->
      let compact_trailing_blank = token_kind_is first Kind.VAL_KW in
      Doc.concat
        [
          leading_comment_token_doc ~compact_trailing_blank first;
          signature_body_item_body_doc tokens;
        ]
  | [] -> signature_body_item_body_doc tokens

let signature_body_items_blank_between = fun left right ->
  module_signature_body_item_is_type left
  && module_signature_body_item_is_value right
  && module_signature_body_item_has_leading_nonsection_comment right

let signature_body_items_doc = fun items ->
  let rec loop previous doc = function
    | [] -> doc
    | next :: rest ->
        let separator =
          if signature_body_items_blank_between previous next then
            blank_line
          else
            Doc.line
        in
        loop next (Doc.concat [ doc; separator; signature_body_item_doc next ]) rest
  in
  match items with
  | [] -> Doc.empty
  | first :: rest -> loop first (signature_body_item_doc first) rest

let module_type_decl_sig_after_tokens = fun decl ->
  match split_signature_shell (module_type_decl_tokens decl) with
  | Some (_, _, _, _, after) -> after
  | None -> []

let module_type_decl_sig_body_doc = fun decl ->
  match Ast.ModuleTypeDeclaration.sig_token decl, Ast.ModuleTypeDeclaration.end_token decl with
  | Some sig_token, Some end_token ->
      let items = split_signature_body_items (module_type_decl_sig_body_tokens decl) in
      let body_doc = signature_body_items_doc items
      |> fun body_doc -> signature_body_doc_with_closing_leading_comment body_doc end_token in
      let after = module_type_decl_sig_after_tokens decl in
      (
        match items with
        | [] when Doc.is_multiline body_doc -> Doc.concat
          [
            token_doc sig_token;
            Doc.line;
            Doc.indent 2 body_doc;
            Doc.line;
            token_doc end_token;
            module_after_tokens_doc after;
          ]
        | [] -> Doc.concat
          [ token_doc sig_token; Doc.space; token_doc end_token; module_after_tokens_doc after ]
        | _ -> Doc.concat
          [
            token_doc sig_token;
            Doc.line;
            Doc.indent 2 body_doc;
            Doc.line;
            token_doc end_token;
            module_after_tokens_doc after;
          ]
      )
  | _ -> unsupported "module type signature body without sig/end tokens"

let module_type_decl_unsupported_body_doc = fun decl ->
  match split_top_level_token (module_type_decl_tokens decl) ~matches:(fun kind -> Kind.(kind = EQ)) with
  | Some (_, _, body_tokens) -> module_tokens_doc body_tokens
  | None -> unsupported "unsupported module type declaration body"

let module_decl_sig_body_doc = fun decl ->
  match Ast.ModuleDeclaration.sig_token decl, Ast.ModuleDeclaration.end_token decl with
  | Some sig_token, Some end_token ->
      let items = split_signature_body_items (module_decl_sig_body_tokens decl) in
      let body_doc = signature_body_items_doc items
      |> fun body_doc -> signature_body_doc_with_closing_leading_comment body_doc end_token in
      (
        match items with
        | [] when Doc.is_multiline body_doc -> Doc.concat
          [ token_doc sig_token; Doc.line; Doc.indent 2 body_doc; Doc.line; token_doc end_token; ]
        | [] -> Doc.concat [ token_doc sig_token; Doc.space; token_doc end_token ]
        | _ -> Doc.concat
          [ token_doc sig_token; Doc.line; Doc.indent 2 body_doc; Doc.line; token_doc end_token; ]
      )
  | _ -> unsupported "module signature body without sig/end tokens"

let module_decl_body_doc = fun decl ->
  match Ast.ModuleDeclaration.body decl with
  | Path -> module_decl_path_body_doc decl
  | EmptyStruct -> Doc.concat [ Doc.text "struct"; Doc.space; Doc.text "end" ]
  | EmptySig -> Doc.concat [ Doc.text "sig"; Doc.space; Doc.text "end" ]
  | Sig -> module_decl_sig_body_doc decl
  | Unsupported -> unsupported "unsupported module declaration body"

let module_type_decl_body_doc = fun decl ->
  match Ast.ModuleTypeDeclaration.body decl with
  | Abstract -> Doc.empty
  | Path -> module_type_decl_path_body_doc decl
  | EmptySig -> module_type_decl_sig_body_doc decl
  | Sig -> module_type_decl_sig_body_doc decl
  | Unsupported -> module_type_decl_unsupported_body_doc decl

let module_decl_doc = fun ?(force_empty_struct_multiline = false) decl ->
  match module_decl_tokens decl with
  | [] -> unsupported "module declaration without tokens"
  | tokens -> module_declaration_tokens_doc ~force_empty_struct_multiline tokens

let module_type_decl_doc = fun decl ->
  match Ast.ModuleTypeDeclaration.name decl with
  | None -> unsupported "module type declaration without name"
  | Some name ->
      let _ = name in
      let head = module_type_decl_head_doc decl in
      (
        match Ast.ModuleTypeDeclaration.equals_token decl, Ast.ModuleTypeDeclaration.body decl with
        | None, Abstract -> head
        | Some equals_token, (Path | EmptySig | Sig | Unsupported) -> Doc.concat
          [ head; Doc.space; token_doc equals_token; Doc.space; module_type_decl_body_doc decl ]
        | Some _, Abstract -> unsupported "unsupported module type declaration body"
        | None, _ -> unsupported "module type declaration body without equals token"
      )

let value_decl_parts = fun decl ->
  match node_tokens decl with
  | val_token :: rest when token_kind_is val_token Kind.VAL_KW -> split_top_level_token
    rest
    ~matches:(fun kind -> Kind.(kind = COLON))
  | _ -> None

let value_decl_doc = fun decl ->
  match value_decl_parts decl with
  | Some ([], _, _) ->
      unsupported "value declaration without name"
  | Some (name_tokens, colon_token, ((_ :: _) as annotation_tokens)) ->
      let annotation_doc = value_decl_type_tokens_doc annotation_tokens in
      let separator =
        if Doc.is_multiline annotation_doc then
          Doc.line
        else
          Doc.space
      in
      let annotation_doc =
        if Doc.is_multiline annotation_doc then
          Doc.indent 2 annotation_doc
        else
          annotation_doc
      in
      Doc.concat
        [
          Doc.text "val";
          Doc.space;
          declaration_name_doc name_tokens;
          token_doc colon_token;
          separator;
          annotation_doc;
        ]
  | None
  | Some (_, _, []) ->
      unsupported "incomplete value declaration"

let external_decl_name_tokens = fun decl ->
  let tokens = ref [] in
  Ast.ExternalDeclaration.for_each_name_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let external_decl_annotation_tokens = fun decl ->
  match split_top_level_token (node_tokens decl) ~matches:(fun kind -> Kind.(kind = COLON)) with
  | Some (_head, _colon, after_colon) -> (
      match split_top_level_token after_colon ~matches:(fun kind -> Kind.(kind = EQ)) with
      | Some (annotation_tokens, _equals_token, _rest) -> annotation_tokens
      | None -> []
    )
  | None -> []

let external_decl_doc = fun decl ->
  match external_decl_name_tokens decl, Ast.ExternalDeclaration.colon_token decl, Ast.ExternalDeclaration.type_annotation
    decl with
  | [], _, _ ->
      unsupported "external declaration without name"
  | name_tokens, Some colon_token, Some annotation ->
      let primitives = ref [] in
      Ast.ExternalDeclaration.for_each_primitive_string
        decl
        ~fn:(fun token -> primitives := token_doc token :: !primitives);
      let attribute_tokens = ref [] in
      Ast.ExternalDeclaration.for_each_attribute_token
        decl
        ~fn:(fun token -> attribute_tokens := token :: !attribute_tokens);
      (
        match List.reverse !primitives with
        | [] -> unsupported "external declaration without primitive strings"
        | primitives ->
            let attributes =
              match List.reverse !attribute_tokens with
              | [] -> Doc.empty
              | tokens -> Doc.concat [ Doc.space; type_tokens_doc tokens ]
            in
            let annotation_doc =
              match external_decl_annotation_tokens decl with
              | [] -> type_expr_doc annotation
              | tokens -> type_tokens_doc_with_threshold
                ~threshold:multiline_top_level_arrow_type_threshold
                tokens
            in
            let primitive_doc = Doc.concat
              [ Doc.equal; Doc.space; Doc.join Doc.space primitives; attributes ] in
            if Doc.is_multiline annotation_doc then
              Doc.concat
                [
                  Doc.text "external";
                  Doc.space;
                  declaration_name_doc name_tokens;
                  token_doc colon_token;
                  Doc.line;
                  Doc.indent 2 annotation_doc;
                  Doc.line;
                  Doc.indent 2 primitive_doc;
                ]
            else
              Doc.concat
                [
                  Doc.text "external";
                  Doc.space;
                  declaration_name_doc name_tokens;
                  token_doc colon_token;
                  Doc.space;
                  annotation_doc;
                  Doc.space;
                  primitive_doc;
                ]
      )
  | _ ->
      unsupported "incomplete external declaration"

let open_decl_doc = fun decl ->
  let bang_token = Ast.Node.first_child_token decl ~kind:Kind.BANG in
  Doc.concat [ Doc.text "open"; optional_token_doc bang_token; Doc.space; open_path_doc decl ]

let include_decl_doc = fun decl ->
  Doc.concat [ Doc.text "include"; Doc.space; include_path_doc decl ]

let exception_decl_tail_tokens = fun decl ->
  let tokens = ref [] in
  Ast.ExceptionDeclaration.for_each_tail_token decl ~fn:(fun token -> tokens := token :: !tokens);
  List.reverse !tokens

let exception_decl_tail_has_comment = fun tokens ->
  let rec loop = function
    | [] -> false
    | token :: rest -> Ast.Token.has_leading_comment token || loop rest
  in
  loop tokens

let exception_decl_tail_token_doc = fun token ->
  if Ast.Token.has_leading_comment token then
    let comment = leading_comment_text token |> strip_trailing_whitespace |> text_lines_doc in
    Doc.concat [ comment; Doc.line; token_doc token ]
  else
    token_doc token

let exception_decl_tail_doc = fun tokens ->
  match tokens with
  | [] -> Doc.empty
  | tokens when exception_decl_tail_has_comment tokens -> Doc.concat
    [
      Doc.line;
      Doc.indent 2 (Doc.join Doc.line (List.map tokens ~fn:exception_decl_tail_token_doc))
    ]
  | tokens -> Doc.concat [ Doc.space; module_tokens_doc tokens ]

let exception_decl_doc = fun decl ->
  match Ast.ExceptionDeclaration.name decl with
  | Some name ->
      let tail = exception_decl_tail_doc (exception_decl_tail_tokens decl) in
      Doc.concat [ Doc.text "exception"; Doc.space; token_doc name; tail ]
  | None -> unsupported "exception declaration without name"

let strip_trailing_end_token = fun tokens ->
  let rec loop acc = function
    | [] -> List.reverse acc
    | [ token ] when token_kind_is token Kind.END_KW -> List.reverse acc
    | token :: rest -> loop (token :: acc) rest
  in
  loop [] tokens

let starts_class_body_field = fun token ->
  let kind = Ast.Token.kind token in
  Kind.(kind = METHOD_KW || kind = VAL_KW)

let split_class_body_fields = fun tokens ->
  let rec loop current fields depth = function
    | [] ->
        List.reverse
          (
            match current with
            | [] -> fields
            | _ -> List.reverse current :: fields
          )
    | token :: rest when Int.(depth = 0)
    && starts_class_body_field token
    && not (List.is_empty current) -> loop
      [ token ]
      (List.reverse current :: fields)
      (type_token_depth_after depth token)
      rest
    | token :: rest -> loop (token :: current) fields (type_token_depth_after depth token) rest
  in
  loop [] [] 0 tokens

let class_body_field_doc = fun tokens ->
  match split_top_level_token tokens ~matches:(fun kind -> Kind.(kind = COLON)) with
  | Some (head_tokens, _colon, type_tokens) ->
      let type_doc = type_tokens_doc_with_threshold ~threshold:1 type_tokens in
      if Doc.is_multiline type_doc then
        Doc.concat
          [ declaration_head_tokens_doc head_tokens; Doc.colon; Doc.line; Doc.indent 2 type_doc; ]
      else
        Doc.concat [ declaration_head_tokens_doc head_tokens; Doc.colon; Doc.space; type_doc ]
  | None -> declaration_head_tokens_doc tokens

let class_decl_doc = fun decl ->
  let _ = decl in
  unsupported "class declarations are not supported by the parser2 formatter"

let extension_item_doc = fun item ->
  extension_shell_doc
    ~for_each_shell_token:(fun ~fn -> Ast.ExtensionItem.for_each_shell_token item ~fn)

let attribute_item_doc = fun item ->
  attribute_shell_doc
    ~for_each_shell_token:(fun ~fn -> Ast.AttributeItem.for_each_shell_token item ~fn)

let structure_item_doc = fun item ->
  let body =
    match Ast.StructureItem.view item with
    | Let decl ->
        let_decl_block_doc decl
    | Type decl ->
        type_decl_doc decl
    | Module decl ->
        module_decl_doc decl
    | Open decl ->
        open_decl_doc decl
    | Include decl ->
        include_decl_doc decl
    | External decl ->
        external_decl_doc decl
    | Exception decl ->
        exception_decl_doc decl
    | ModuleType decl ->
        module_type_decl_doc decl
    | Expr expr_item -> (
        match Ast.ExprItem.expr expr_item with
        | Some expr -> expr_doc expr
        | None -> unsupported "expression item without expression"
      )
    | Extension item ->
        extension_item_doc item
    | Attribute item ->
        attribute_item_doc item
    | Class decl ->
        class_decl_doc decl
    | Error _
    | Unknown _ ->
        unsupported "unsupported structure item"
  in
  Doc.concat [ leading_comment_node_paragraph_doc ~drop_inline:true item; body ]

let structure_item_phrase_separator_doc = fun item ->
  let body =
    match Ast.StructureItem.view item with
    | Let decl -> let_decl_block_doc ~force_body_break_after_equal:true decl
    | Module decl -> module_decl_doc ~force_empty_struct_multiline:true decl
    | _ -> structure_item_doc item
  in
  match Ast.StructureItem.view item with
  | Let _
  | Module _ -> Doc.concat [ leading_comment_node_paragraph_doc item; body ]
  | _ -> body

let signature_item_doc = fun item ->
  let body =
    match Ast.SignatureItem.view item with
    | Value decl -> value_decl_doc decl
    | Type decl -> type_decl_doc decl
    | Module decl -> module_decl_doc decl
    | Open decl -> open_decl_doc decl
    | Include decl -> include_decl_doc decl
    | External decl -> external_decl_doc decl
    | Exception decl -> exception_decl_doc decl
    | ModuleType decl -> module_type_decl_doc decl
    | Extension item -> extension_item_doc item
    | Attribute item -> attribute_item_doc item
    | Class decl -> class_decl_doc decl
    | Error _
    | Unknown _ -> unsupported "unsupported signature item"
  in
  Doc.concat [ leading_comment_node_paragraph_doc ~drop_inline:true item; body ]

let signature_item_is_type = fun item ->
  match Ast.SignatureItem.view item with
  | Type _ -> true
  | _ -> false

let signature_item_is_open = fun item ->
  match Ast.SignatureItem.view item with
  | Open _ -> true
  | _ -> false

let signature_item_is_value = fun item ->
  match Ast.SignatureItem.view item with
  | Value _ -> true
  | _ -> false

let signature_item_has_leading_nonsection_comment = fun item ->
  match Ast.Node.first_descendant_token item with
  | Some token -> token_has_leading_nonsection_comment token
  | None -> false

let token_has_mixed_leading_section_docstrings = fun token ->
  let has_section = ref false in
  let has_nonsection = ref false in
  Ast.Token.for_each_leading_trivia token
    ~fn:(fun ~kind ~text ->
      if Kind.(kind = DOCSTRING) then
        if is_section_docstring_text text then
          has_section := true
        else
          has_nonsection := true);
  !has_section && !has_nonsection

let signature_item_has_mixed_leading_section_docstrings = fun item ->
  match Ast.Node.first_descendant_token item with
  | Some token -> token_has_mixed_leading_section_docstrings token
  | None -> false

let signature_items_compact_between = fun left right ->
  (signature_item_is_type left
  && signature_item_is_type right
  && not (signature_item_has_mixed_leading_section_docstrings right))
  || (signature_item_is_open left && signature_item_is_open right)
  || (signature_item_is_type left
  && signature_item_is_value right
  && not (signature_item_has_leading_nonsection_comment right))

let signature_items_doc = fun items ->
  let rec loop previous doc = function
    | [] -> doc
    | (next_item, next_doc) :: rest ->
        let separator =
          if signature_items_compact_between previous next_item then
            Doc.line
          else
            blank_line
        in
        let next_doc =
          match inline_leading_comment_node_doc next_item with
          | Doc.Empty -> next_doc
          | comment -> Doc.concat [ comment; blank_line; next_doc ]
        in
        loop next_item (Doc.concat [ doc; separator; next_doc ]) rest
  in
  match items with
  | [] -> Doc.empty
  | (first_item, first_doc) :: rest -> loop first_item first_doc rest

let structure_item_is_open = fun item ->
  match Ast.StructureItem.view item with
  | Open _ -> true
  | _ -> false

let structure_item_is_type = fun item ->
  match Ast.StructureItem.view item with
  | Type _ -> true
  | _ -> false

let structure_item_is_attribute = fun item ->
  match Ast.StructureItem.view item with
  | Attribute _ -> true
  | _ -> false

let structure_item_is_module_path_alias = fun item ->
  match Ast.StructureItem.view item with
  | Module decl -> (
      match Ast.ModuleDeclaration.body decl with
      | Path -> true
      | _ -> false
    )
  | _ -> false

let structure_items_separator = fun left right ->
  if structure_item_is_type left && structure_item_is_attribute right then
    Doc.space
  else if structure_item_is_open left && structure_item_is_open right then
    Doc.line
  else if structure_item_is_module_path_alias left && structure_item_is_module_path_alias right then
    Doc.line
  else
    blank_line

let structure_items_doc = fun items ->
  let rec loop previous doc = function
    | [] -> doc
    | (next_item, next_doc) :: rest ->
        let separator = structure_items_separator previous next_item in
        let next_doc =
          match inline_leading_comment_node_doc next_item with
          | Doc.Empty -> next_doc
          | comment -> Doc.concat [ comment; blank_line; next_doc ]
        in
        loop next_item (Doc.concat [ doc; separator; next_doc ]) rest
  in
  match items with
  | [] -> Doc.empty
  | (first_item, first_doc) :: rest -> loop first_item first_doc rest

let implementation_doc = fun implementation ->
  let items = ref [] in
  let semis = ref 0 in
  let append_phrase_separator () =
    match !items with
    | (item, _doc) :: rest -> items := (
      item,
      Doc.concat [ structure_item_phrase_separator_doc item; Doc.text ";;" ]
    )
    :: rest
    | [] -> ()
  in
  Ast.Node.for_each_child implementation
    ~fn:(
      function
      | Syn.SyntaxTree.Node id -> (
          semis := 0;
          let node: Ast.Node.t = { Ast.tree = implementation.Ast.tree; id } in
          match Ast.StructureItem.cast node with
          | Some item -> items := (item, structure_item_doc item) :: !items
          | None -> ()
        )
      | Syn.SyntaxTree.Token id ->
          let token: Ast.Token.t = { Ast.tree = implementation.Ast.tree; id } in
          if token_kind_is token Kind.SEMI then
            (
              semis := !semis + 1;
              if Int.equal !semis 2 then
                (
                  append_phrase_separator ();
                  semis := 0
                )
            )
          else
            semis := 0
      | Syn.SyntaxTree.Missing _ ->
          semis := 0
    );
  structure_items_doc (List.reverse !items)

let interface_doc = fun interface ->
  let items = ref [] in
  Ast.Interface.for_each_item
    interface
    ~fn:(fun item -> items := (item, signature_item_doc item) :: !items);
  signature_items_doc (List.reverse !items)

let append_eof_comment = fun source_file doc ->
  match eof_comment_doc source_file with
  | Doc.Empty -> doc
  | comment -> (
      match doc with
      | Doc.Empty -> comment
      | _ -> Doc.concat [ doc; eof_comment_separator source_file; comment ]
    )

let source_file = fun source_file ->
  try
    let body =
      match Ast.SourceFile.view source_file with
      | Empty -> Doc.empty
      | Implementation implementation -> implementation_doc implementation
      | Interface interface -> interface_doc interface
    in
    Ok (append_eof_comment source_file body)
  with
  | Unsupported err -> Error err
