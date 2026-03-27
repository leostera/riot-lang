open Std
open Std.Sync
open Std.Collections

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

exception Bail of error

let bail ~message ~syntax_node ~context =
  raise
    (Bail
       {
         message;
         syntax_kind = Ceibo.Red.SyntaxNode.kind syntax_node;
         span = Ceibo.Red.SyntaxNode.span syntax_node;
         context;
       })

let unsupported_parameter node =
  bail ~message:"unsupported parameter shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "parameter" ]

let unsupported_pattern node =
  bail ~message:"unsupported pattern shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "pattern" ]

let unsupported_expression node =
  bail ~message:"unsupported expression shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "expression" ]

let unsupported_module_expression node =
  bail ~message:"unsupported module expression shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "module_expression" ]

let unsupported_class_expression node =
  bail ~message:"unsupported class expression shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "class_expression" ]

let unsupported_class_type node =
  bail ~message:"unsupported class type shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "class_type" ]

let unsupported_item node =
  bail ~message:"unsupported structure item during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "item" ]

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let token syntax_tok = Cst.Token.{ syntax_token = syntax_tok }

let synthetic_token ~kind ~text ~start_offset ~end_offset =
  let green_token =
    Ceibo.Green.make_token ~kind ~text ~width:(String.length text)
  in
  let syntax_token =
    Ceibo.Red.new_token green_token (Ceibo.Span.make ~start:start_offset ~end_:end_offset)
  in
  token syntax_token

let substring text start length =
  let text_length = String.length text in
  if length <= 0 || start >= text_length then
    ""
  else
    let safe_start = Int.max 0 start in
    String.sub text safe_start (Int.min length (text_length - safe_start))

let is_ascii_alpha = function
  | 'a' .. 'z' | 'A' .. 'Z' ->
      true
  | _ ->
      false

let find_char_from text start target =
  let rec loop index =
    if index >= String.length text then
      None
    else if Char.equal (String.get text index) target then
      Some index
    else
      loop (index + 1)
  in
  loop start

let split_trailing_alpha_suffix text =
  let rec loop index =
    if index < 0 then
      0
    else if is_ascii_alpha (String.get text index) then
      loop (index - 1)
    else
      index + 1
  in
  let suffix_start = loop (String.length text - 1) in
  ( substring text 0 suffix_start,
    if suffix_start < String.length text then
      Some (substring text suffix_start (String.length text - suffix_start))
    else
      None )

let string_delimiter_and_contents text =
  let len = String.length text in
  if len > 0 && Char.equal (String.get text 0) '"' then
    let terminated = len > 1 && Char.equal (String.get text (len - 1)) '"' in
    let contents_end = if terminated then len - 1 else len in
    ( Cst.DoubleQuote,
      substring text 1 (contents_end - 1),
      terminated )
  else if len > 0 && Char.equal (String.get text 0) '{' then
    match find_char_from text 1 '|' with
    | Some pipe_index ->
        let marker = substring text 1 (pipe_index - 1) in
        let closing = "|" ^ marker ^ "}" in
        let closing_len = String.length closing in
        let terminated =
          len >= pipe_index + 1 + closing_len
          && String.equal (substring text (len - closing_len) closing_len) closing
        in
        let contents_start = pipe_index + 1 in
        let contents_end = if terminated then len - closing_len else len in
        ( Cst.Quoted { marker },
          substring text contents_start (contents_end - contents_start),
          terminated )
    | None ->
        (Cst.Quoted { marker = "" }, substring text 1 (len - 1), false)
  else
    (Cst.DoubleQuote, text, false)

let integer_parts text =
  let base, prefix =
    let starts_with prefix =
      let prefix_len = String.length prefix in
      if String.length text < prefix_len then
        false
      else
        String.equal (substring text 0 prefix_len) prefix
    in
    if starts_with "0x" || starts_with "0X" then
      (Cst.Hexadecimal, Some (substring text 0 2))
    else if starts_with "0o" || starts_with "0O" then
      (Cst.Octal, Some (substring text 0 2))
    else if starts_with "0b" || starts_with "0B" then
      (Cst.Binary, Some (substring text 0 2))
    else
      (Cst.Decimal, None)
  in
  let digit_start =
    match prefix with
    | Some prefix -> String.length prefix
    | None -> 0
  in
  let is_digit =
    match base with
    | Cst.Decimal ->
        (function '0' .. '9' | '_' -> true | _ -> false)
    | Cst.Hexadecimal ->
        (function
        | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' | '_' -> true
        | _ -> false)
    | Cst.Octal ->
        (function '0' .. '7' | '_' -> true | _ -> false)
    | Cst.Binary ->
        (function '0' | '1' | '_' -> true | _ -> false)
  in
  let rec find_suffix_start index =
    if index >= String.length text then
      index
    else if is_digit (String.get text index) then
      find_suffix_start (index + 1)
    else
      index
  in
  let suffix_start = find_suffix_start digit_start in
  let digits = substring text digit_start (suffix_start - digit_start) in
  let suffix =
    if suffix_start < String.length text then
      Some (substring text suffix_start (String.length text - suffix_start))
    else
      None
  in
  (base, prefix, digits, suffix)

let float_parts text =
  let body, suffix = split_trailing_alpha_suffix text in
  let exponent_marker_index =
    match find_char_from body 0 'e' with
    | Some _ as found -> found
    | None -> find_char_from body 0 'E'
  in
  let body_without_exponent, exponent =
    match exponent_marker_index with
    | Some index ->
        let exponent_body =
          substring body (index + 1) (String.length body - index - 1)
        in
        let sign, digits =
          if String.length exponent_body = 0 then
            (None, "")
          else
            match String.get exponent_body 0 with
            | '+' ->
                (Some Cst.Positive, substring exponent_body 1 (String.length exponent_body - 1))
            | '-' ->
                (Some Cst.Negative, substring exponent_body 1 (String.length exponent_body - 1))
            | _ ->
                (None, exponent_body)
        in
        ( substring body 0 index,
          Some
            {
              Cst.marker = substring body index 1;
              sign;
              digits;
            } )
    | None ->
        (body, None)
  in
  let dot_index = find_char_from body_without_exponent 0 '.' in
  let integral_digits, fractional_digits =
    match dot_index with
    | Some index ->
        ( substring body_without_exponent 0 index,
          substring body_without_exponent (index + 1)
            (String.length body_without_exponent - index - 1) )
    | None ->
        (body_without_exponent, "")
  in
  (integral_digits, fractional_digits, exponent, suffix)

let char_contents text =
  let len = String.length text in
  if len > 0 && Char.equal (String.get text 0) '\'' then
    let contents_end =
      if len > 1 && Char.equal (String.get text (len - 1)) '\'' then
        len - 1
      else
        len
    in
    substring text 1 (contents_end - 1)
  else
    text

let is_constant_syntax_kind = function
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL ->
      true
  | _ ->
      false

let direct_non_trivia_nodes node =
  Ceibo.Red.SyntaxNode.direct_nodes node
  |> List.filter (fun child -> not (is_trivia (Ceibo.Red.SyntaxNode.kind child)))

let direct_non_trivia_tokens node =
  Ceibo.Red.SyntaxNode.direct_tokens node
  |> List.filter (fun tok -> not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)))

let subtree_non_trivia_tokens node =
  Ceibo.Red.SyntaxNode.tokens node
  |> List.filter (fun tok -> not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)))

let expression_grouping_from_node node =
  match direct_non_trivia_tokens node with
  | opening :: _ -> (
      match Ceibo.Red.SyntaxToken.text opening with
      | "begin" ->
          Cst.BeginEnd
      | _ ->
          Cst.Parens)
  | [] ->
      Cst.Parens

let first_and_last_direct_token node =
  let rec last = function
    | [] ->
        None
    | [ token ] ->
        Some token
    | _ :: rest ->
        last rest
  in
  match direct_non_trivia_tokens node with
  | [] ->
      None
  | [ token ] ->
      Some (token, token)
  | first :: rest -> (
      match last rest with
      | Some last_token ->
          Some (first, last_token)
      | None ->
          Some (first, first))

let rec drop_attribute_shell_tokens tokens =
  let token_text = Ceibo.Red.SyntaxToken.text in
  let rec loop depth = function
    | [] ->
        []
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if String.equal text "[" then
          loop (depth + 1) rest
        else if String.equal text "]" then
          if depth = 1 then
            rest
          else
            loop (depth - 1) rest
        else
          loop depth rest
  in
  loop 1 tokens

let rec drop_leading_declaration_decorator_tokens tokens =
  let token_text = Ceibo.Red.SyntaxToken.text in
  match tokens with
  | percent_token :: _extension_name :: rest
    when String.equal (token_text percent_token) "%" ->
      drop_leading_declaration_decorator_tokens rest
  | open_bracket :: rest when String.equal (token_text open_bracket) "[" ->
      drop_leading_declaration_decorator_tokens (drop_attribute_shell_tokens rest)
  | _ ->
      tokens

let declaration_tokens_after_keywords ~keyword_count tokens =
  let rec drop count remaining =
    if count <= 0 then
      remaining
    else
      match remaining with
      | _ :: rest ->
          drop (count - 1) rest
      | [] ->
          []
  in
  drop keyword_count tokens |> drop_leading_declaration_decorator_tokens

let find_declaration_name_token ~skip_keywords tokens =
  let token_text = Ceibo.Red.SyntaxToken.text in
  let rec loop = function
    | [] ->
        None
    | syntax_token :: rest ->
        let text = token_text syntax_token in
        if List.exists (String.equal text) skip_keywords then
          loop rest
        else if String.equal text "%" then
          (match rest with
          | _extension_name :: after_extension ->
              loop after_extension
          | [] ->
              None)
        else if String.equal text "[" then
          loop (drop_attribute_shell_tokens rest)
        else if
          String.equal text ":"
          || String.equal text "="
          || String.equal text ":="
        then
          None
        else
          Some syntax_token
  in
  loop tokens

let direct_tokens node =
  Ceibo.Red.SyntaxNode.direct_tokens node

let is_literal_token_kind = function
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL ->
      true
  | _ ->
      false

let literal_token_from_node ~context node =
  let direct_tokens = direct_non_trivia_tokens node in
  let find_literal_token syntax_tokens =
    syntax_tokens
    |> List.find_opt (fun syntax_token ->
           is_literal_token_kind (Ceibo.Red.SyntaxToken.kind syntax_token))
  in
  match find_literal_token direct_tokens with
  | Some literal_syntax_token ->
      token literal_syntax_token
  | None -> (
      match find_literal_token (subtree_non_trivia_tokens node) with
      | Some literal_syntax_token ->
          token literal_syntax_token
      | None ->
          bail ~message:"expected literal token during Ceibo -> CST lifting"
            ~syntax_node:node ~context)

let constant_from_syntax_token ~syntax_node syntax_token =
  let literal_token = token syntax_token in
  match Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syntax_kind.STRING_LITERAL ->
      let delimiter, contents, terminated =
        string_delimiter_and_contents (Cst.Token.text literal_token)
      in
      Cst.Constant.String
        {
          syntax_node;
          literal_token;
          delimiter;
          contents;
          terminated;
          attributes = [];
        }
  | Syntax_kind.INT_LITERAL ->
      let base, prefix, digits, suffix = integer_parts (Cst.Token.text literal_token) in
      Cst.Constant.Int
        {
          syntax_node;
          literal_token;
          base;
          prefix;
          digits;
          suffix;
          attributes = [];
        }
  | Syntax_kind.FLOAT_LITERAL ->
      let integral_digits, fractional_digits, exponent, suffix =
        float_parts (Cst.Token.text literal_token)
      in
      Cst.Constant.Float
        {
          syntax_node;
          literal_token;
          integral_digits;
          fractional_digits;
          exponent;
          suffix;
          attributes = [];
        }
  | Syntax_kind.CHAR_LITERAL ->
      Cst.Constant.Char
        {
          syntax_node;
          literal_token;
          contents = char_contents (Cst.Token.text literal_token);
          attributes = [];
        }
  | Syntax_kind.BOOL_LITERAL ->
      Cst.Constant.Bool
        {
          syntax_node;
          literal_token;
          value = String.equal (Cst.Token.text literal_token) "true";
          attributes = [];
        }
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Constant.Unit { syntax_node; attributes = [] }
  | _ ->
      bail ~message:"expected literal token during Ceibo -> CST lifting"
        ~syntax_node ~context:[ "constant" ]

let constant_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Constant.Unit { syntax_node = node; attributes = [] }
  | _ ->
      let literal_token = literal_token_from_node ~context:[ "constant" ] node in
      constant_from_syntax_token ~syntax_node:node
        (Cst.Token.syntax_token literal_token)

let module_path_from_tokens ~syntax_node syntax_tokens =
  let rec skip_to_name = function
    | [] -> None
    | syntax_token :: rest ->
        if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "." then
          skip_to_name rest
        else Some (syntax_token, rest)
  in
  let rec build prefix = function
    | [] -> prefix
    | dot_token :: rest
      when String.equal (Ceibo.Red.SyntaxToken.text dot_token) "." -> (
        match skip_to_name rest with
        | Some (name_syntax_token, tail) ->
            build
              (Cst.Ident.Qualified
                {
                  syntax_node;
                  prefix;
                  dot_token = token dot_token;
                  name_token = token name_syntax_token;
                })
              tail
        | None -> prefix)
    | _unexpected :: rest ->
        build prefix rest
  in
  match skip_to_name syntax_tokens with
  | Some (name_syntax_token, rest) ->
      build
        (Cst.Ident.Ident
           { syntax_node; name_token = token name_syntax_token })
        rest
  | None ->
      bail ~message:"expected at least one path segment during Ceibo -> CST lifting"
        ~syntax_node ~context:[ "module_path" ]

let module_path_from_node node =
  let parts = direct_non_trivia_tokens node in
  module_path_from_tokens ~syntax_node:node parts

let ident_path_from_node node =
  match direct_non_trivia_tokens node with
  | first :: rest ->
      let name_token =
        match rest with
        | [] ->
            token first
        | _ ->
            let tokens = first :: rest in
            let text =
              tokens |> List.map Ceibo.Red.SyntaxToken.text |> String.concat ""
            in
            let start_offset = (Ceibo.Red.SyntaxToken.span first).start in
            let last = List.fold_left (fun _ token -> token) first rest in
            let end_offset = (Ceibo.Red.SyntaxToken.span last).end_ in
            synthetic_token ~kind:Syntax_kind.IDENT_EXPR ~text ~start_offset ~end_offset
      in
      Cst.Ident.Ident
        { syntax_node = node; name_token }
  | [] ->
      bail ~message:"expected identifier path segment during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_path"; "ident" ]

let rec module_path_like_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH | Syntax_kind.MODULE_TYPE_PATH ->
      module_path_from_node node
  | Syntax_kind.IDENT_EXPR ->
      ident_path_from_node node
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match
        direct_non_trivia_nodes node,
        List.rev (direct_non_trivia_tokens node)
      with
      | receiver_node :: _, name_syntax_token :: dot_syntax_token :: _ ->
          let prefix = module_path_like_from_node receiver_node in
          Cst.Ident.Qualified
            {
              syntax_node = node;
              prefix;
              dot_token = token dot_syntax_token;
              name_token = token name_syntax_token;
            }
      | _ ->
          module_path_from_tokens ~syntax_node:node (direct_non_trivia_tokens node))
  | _ ->
      module_path_from_tokens ~syntax_node:node (direct_non_trivia_tokens node)

let type_constructor_path_from_node node =
  let is_identifier_like_text text =
    let is_alpha_or_underscore ch =
      (ch >= 'a' && ch <= 'z')
      || (ch >= 'A' && ch <= 'Z')
      || ch = '_'
    in
    let len = String.length text in
    if len = 0 then
      false
    else
      let ch = String.get text 0 in
      is_alpha_or_underscore ch
      || if ch = '#' && len > 1 then
           let next = String.get text 1 in
           is_alpha_or_underscore next
         else if ch = '\\' then
           true
         else
           false
  in
  match
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           let kind = Ceibo.Red.SyntaxNode.kind child in
           kind = Syntax_kind.MODULE_PATH
           || kind = Syntax_kind.MODULE_TYPE_PATH
           || kind = Syntax_kind.IDENT_EXPR)
  with
  | Some path_node ->
      module_path_like_from_node path_node
  | None ->
      let path_tokens =
        direct_non_trivia_tokens node
        |> List.filter (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               is_identifier_like_text text || String.equal text ".")
      in
      if List.length path_tokens = 0 then
        bail ~message:"expected type constructor path during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.constr" ]
      else
        module_path_from_tokens ~syntax_node:node path_tokens

let token_starts_with_uppercase token =
  let text = Cst.Token.text token in
  let len = String.length text in
  len > 0 &&
  let first = String.get text 0 in
  first >= 'A' && first <= 'Z'

let is_constructor_path (path : Cst.Ident.t) =
  match Cst.Ident.last_segment path with
  | Some segment -> token_starts_with_uppercase segment
  | None -> false

let rec module_like_path_from_expression_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      let path = ident_path_from_node node in
      if is_constructor_path path then
        Some path
      else
        None
  | Syntax_kind.MODULE_PATH ->
      let path = module_path_from_node node in
      if is_constructor_path path then
        Some path
      else
        None
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match
        direct_non_trivia_nodes node,
        List.rev (direct_non_trivia_tokens node)
      with
      | receiver_node :: _, name_syntax_token :: dot_syntax_token :: _
        when token_starts_with_uppercase (token name_syntax_token) -> (
          match module_like_path_from_expression_node receiver_node with
          | Some prefix ->
              Some
                (Cst.Ident.Qualified
                   {
                     syntax_node = node;
                     prefix;
                     dot_token = token dot_syntax_token;
                     name_token = token name_syntax_token;
                   })
          | None ->
              None)
      | _ ->
          None)
  | _ ->
      None

let constructor_path_from_expression_node node =
  match module_like_path_from_expression_node node with
  | Some path when is_constructor_path path -> Some path
  | Some _ | None -> None

let annotation_shell_and_payload ~annotation_kind ~sigils node =
  let direct_tokens = direct_non_trivia_tokens node in
  let has_sigil tokens =
    tokens
    |> List.exists (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           List.exists (String.equal text) sigils)
  in
  if has_sigil direct_tokens then
    (node, None)
  else
    let direct_children = direct_non_trivia_nodes node in
    let shell_node_opt =
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child = annotation_kind
             && has_sigil (direct_non_trivia_tokens child))
    in
    let payload_node_opt =
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child != annotation_kind)
    in
    match shell_node_opt with
    | Some shell_node -> (shell_node, payload_node_opt)
    | None ->
        bail ~message:"expected attribute or extension shell during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "annotation" ]

let annotation_name_from_tokens ~syntax_node ~sigils syntax_tokens =
  let name_tokens =
    syntax_tokens
    |> List.filter (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not
             (String.equal text "["
             || String.equal text "]"
             || List.exists (String.equal text) sigils))
  in
  module_path_from_tokens ~syntax_node name_tokens

type raw_annotation_payload_kind =
  | Unmarked
  | TypePayload
  | PatternPayload

type raw_annotation_payload = {
  kind : raw_annotation_payload_kind;
  text : string;
  start_offset : int;
}

let parse_implementation_fragment source =
  let tokens = Lexer.tokenize source in
  Parser.parse_implementation ~source tokens

let parse_interface_fragment source =
  let tokens = Lexer.tokenize source in
  Parser.parse_interface ~source tokens

let make_padded_fragment ~start_offset text =
  String.make start_offset ' ' ^ text

let make_wrapped_fragment ~prefix ~suffix ~start_offset text =
  let padding = Int.max 0 (start_offset - String.length prefix) in
  String.make padding ' ' ^ prefix ^ text ^ suffix

let payload_text_from_tokens all_tokens ~start_offset ~end_offset =
  all_tokens
  |> List.filter (fun syntax_token ->
         let span = Ceibo.Red.SyntaxToken.span syntax_token in
         span.start >= start_offset && span.end_ <= end_offset)
  |> List.map Ceibo.Red.SyntaxToken.text
  |> String.concat ""

let annotation_payload_from_shell_impl :
    (Cst.syntax_node -> Cst.payload option) Cell.t =
  Cell.create (fun _ -> None)

let annotation_payload_from_shell shell_node =
  (Cell.get annotation_payload_from_shell_impl) shell_node

let attribute_from_node node : Cst.attribute =
  let shell_node, payload_syntax_node =
    annotation_shell_and_payload ~annotation_kind:Syntax_kind.ATTRIBUTE_EXPR
      ~sigils:[ "@"; "@@"; "@@@" ] node
  in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_token =
    shell_tokens
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           String.equal text "@" || String.equal text "@@" || String.equal text "@@@")
  in
  match sigil_syntax_token with
  | Some sigil_syntax_token ->
      {
        Cst.syntax_node = node;
        sigil_token = token sigil_syntax_token;
        name =
          annotation_name_from_tokens ~syntax_node:shell_node
            ~sigils:[ "@"; "@@"; "@@@" ] shell_tokens;
        payload_syntax_node;
        payload = annotation_payload_from_shell shell_node;
      }
  | None ->
      bail ~message:"expected attribute sigil during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "attribute" ]

let extension_from_node node : Cst.extension =
  let shell_node, payload_syntax_node =
    annotation_shell_and_payload ~annotation_kind:Syntax_kind.EXTENSION_EXPR
      ~sigils:[ "%"; "%%"; "%%%" ] node
  in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_token =
    shell_tokens
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           String.equal text "%" || String.equal text "%%" || String.equal text "%%%")
  in
  match sigil_syntax_token with
  | Some sigil_syntax_token ->
      {
        Cst.syntax_node = node;
        sigil_token = token sigil_syntax_token;
        name =
          annotation_name_from_tokens ~syntax_node:shell_node
            ~sigils:[ "%"; "%%"; "%%%" ] shell_tokens;
        payload_syntax_node;
        payload = annotation_payload_from_shell shell_node;
        attributes = [];
      }
  | None ->
      bail ~message:"expected extension sigil during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "extension" ]

let attributes_from_node node =
  direct_non_trivia_nodes node
  |> List.filter (fun child ->
         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR)
  |> List.map attribute_from_node

let attribute_is_item_like (attribute : Cst.attribute) =
  match Cst.Token.text attribute.sigil_token with
  | "@@" | "@@@" -> true
  | _ -> false

let is_initializer_node node =
  match direct_non_trivia_tokens node with
  | initializer_kw :: _ ->
      String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer"
  | [] ->
      false

let class_field_with_attributes field attributes =
  List.fold_left
    (fun field attribute ->
      Cst.ClassField.Attribute
        {
          syntax_node = Cst.ClassField.syntax_node field;
          field;
          attribute;
        })
    field attributes

let non_paren_tokens node =
  direct_non_trivia_tokens node
  |> List.filter (fun syntax_token ->
         let text = Ceibo.Red.SyntaxToken.text syntax_token in
         not (String.equal text "(" || String.equal text ")"))

let is_type_syntax_kind = function
  | Syntax_kind.TYPE_VAR
  | Syntax_kind.TYPE_CONSTR
  | Syntax_kind.TYPE_RECORD
  | Syntax_kind.TYPE_TUPLE
  | Syntax_kind.TYPE_ALIAS
  | Syntax_kind.TYPE_ARROW
  | Syntax_kind.TYPE_PAREN
  | Syntax_kind.TYPE_POLY_VARIANT
  | Syntax_kind.POLY_TYPE
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE
  | Syntax_kind.OBJECT_TYPE
  | Syntax_kind.LOCAL_OPEN_TYPE
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | _ -> false

let rec can_lift_core_type_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_core_type_node
  | kind ->
      is_type_syntax_kind kind

let rec peel_outer_type_attributes node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_core_type_node (first_child :: rest),
            List.find_opt (fun child ->
                Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR)
              rest
          with
          | Some payload_node, Some attribute_node ->
              let payload_node, attributes = peel_outer_type_attributes payload_node in
              (payload_node, attributes @ [ attribute_from_node attribute_node ])
          | _ ->
              (node, []))
      | [] ->
          (node, []))
  | _ ->
      (node, [])

let rec can_lift_module_type_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH
  | Syntax_kind.MODULE_TYPE_OF
  | Syntax_kind.MODULE_TYPE_EXPR
  | Syntax_kind.FUNCTOR_TYPE
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | Syntax_kind.PAREN_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          String.equal (Ceibo.Red.SyntaxToken.text first) "sig"
      | [] -> false)
  | _ ->
      false

let rec can_lift_class_type_field_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_INHERIT
  | Syntax_kind.OBJECT_VAL
  | Syntax_kind.OBJECT_METHOD
  | Syntax_kind.TYPE_CONSTRAINT
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_class_type_field_node
  | _ ->
      false

let rec can_lift_class_type_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.TYPE_CONSTR
  | Syntax_kind.TYPE_ARROW
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.ARRAY_INDEX_EXPR ->
      true
  | Syntax_kind.PAREN_EXPR | Syntax_kind.ATTRIBUTE_EXPR | Syntax_kind.APPLY_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_class_type_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | _ :: _ -> true
      | [] -> false)
  | _ ->
      false

let rec can_lift_class_expression_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LET_REC_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | Syntax_kind.PAREN_EXPR | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_class_expression_node
  | _ ->
      false

let rec can_lift_module_expression_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.STRUCT_EXPR
  | Syntax_kind.MODULE_APPLICATION
  | Syntax_kind.MODULE_UNIT_APPLICATION
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR ->
      true
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          String.equal (Ceibo.Red.SyntaxToken.text first) "functor"
      | [] -> false)
  | Syntax_kind.PAREN_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          not (String.equal (Ceibo.Red.SyntaxToken.text first) "sig")
      | [] -> false)
  | _ ->
      false

let is_pattern_syntax_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.EFFECT_PATTERN
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN ->
      true
  | _ -> false

let is_expression_syntax_kind = function
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.METHOD_CALL_EXPR
  | Syntax_kind.NEW_EXPR
  | Syntax_kind.FIELD_ACCESS_EXPR
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.STRING_INDEX_EXPR
  | Syntax_kind.ASSIGN_EXPR
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.ASSERT_EXPR
  | Syntax_kind.LAZY_EXPR
  | Syntax_kind.WHILE_EXPR
  | Syntax_kind.FOR_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.POLY_VARIANT_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR
  | Syntax_kind.LET_MODULE_EXPR
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LET_REC_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.COERCE_EXPR
  | Syntax_kind.PREFIX_EXPR
  | Syntax_kind.INFIX_EXPR
  | Syntax_kind.SEQUENCE_EXPR
  | Syntax_kind.TUPLE_EXPR
  | Syntax_kind.LIST_EXPR
  | Syntax_kind.ARRAY_EXPR
  | Syntax_kind.RECORD_EXPR
  | Syntax_kind.RECORD_UPDATE_EXPR
  | Syntax_kind.UNREACHABLE_EXPR
  | Syntax_kind.OBJECT_UPDATE_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.FUNCTION_EXPR
  | Syntax_kind.MATCH_EXPR
  | Syntax_kind.TRY_EXPR
  | Syntax_kind.IF_EXPR
  | Syntax_kind.PAREN_EXPR ->
      true
  | _ -> false

let rec can_lift_expression_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_expression_node
  | kind ->
      is_expression_syntax_kind kind

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      true
  | _ -> false

let pattern_with_attributes pattern attributes =
  match attributes with
  | [] ->
      pattern
  | _ ->
      let append existing = existing @ attributes in
      match pattern with
      | Cst.Pattern.Identifier pattern ->
          Cst.Pattern.Identifier
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Wildcard pattern ->
          Cst.Pattern.Wildcard { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Extension pattern ->
          Cst.Pattern.Extension
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Literal pattern ->
          Cst.Pattern.Literal { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Lazy pattern ->
          Cst.Pattern.Lazy { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Exception pattern ->
          Cst.Pattern.Exception
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Range pattern ->
          Cst.Pattern.Range { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Operator pattern ->
          Cst.Pattern.Operator { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.FirstClassModule pattern ->
          Cst.Pattern.FirstClassModule
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.PolyVariant pattern ->
          Cst.Pattern.PolyVariant
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.PolyVariantInherit pattern ->
          Cst.Pattern.PolyVariantInherit
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Constructor pattern ->
          Cst.Pattern.Constructor
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Tuple pattern ->
          Cst.Pattern.Tuple { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.List pattern ->
          Cst.Pattern.List { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Array pattern ->
          Cst.Pattern.Array { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Record pattern ->
          Cst.Pattern.Record { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Cons pattern ->
          Cst.Pattern.Cons { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Or pattern ->
          Cst.Pattern.Or { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Alias pattern ->
          Cst.Pattern.Alias { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Typed pattern ->
          Cst.Pattern.Typed { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Effect pattern ->
          Cst.Pattern.Effect { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.LocalOpen pattern ->
          Cst.Pattern.LocalOpen
            { pattern with attributes = append pattern.attributes }
      | Cst.Pattern.Parenthesized pattern ->
          Cst.Pattern.Parenthesized
            { pattern with attributes = append pattern.attributes }

let name_token_from_ident_pattern node =
  match direct_non_trivia_tokens node with
  | first :: _ -> Some (token first)
  | [] -> None

let is_identifier_like_text text =
  let is_alpha_or_underscore ch =
    (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || ch = '_'
  in
  let len = String.length text in
  if len = 0 then
    false
  else
    let ch = String.get text 0 in
    is_alpha_or_underscore ch
    || if ch = '#' && len > 1 then
      let next = String.get text 1 in
      is_alpha_or_underscore next
    else if ch = '\\' then
      if len > 2 && String.get text 1 = '#' then
        let next = String.get text 2 in
        is_alpha_or_underscore next
      else
        false
    else
      false

let rec simple_pattern_name_token node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN ->
      name_token_from_ident_pattern node
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: _ -> simple_pattern_name_token first_child
      | [] -> None)
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.LAZY_PATTERN ->
      (match direct_non_trivia_nodes node |> List.find_opt (fun _ -> true) with
      | Some child -> simple_pattern_name_token child
      | None -> None)
  | Syntax_kind.LOCAL_OPEN_PATTERN -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt (fun child ->
               is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
      with
      | Some child -> simple_pattern_name_token child
      | None -> None)
  | Syntax_kind.AS_PATTERN ->
      (match
         direct_non_trivia_nodes node
         |> List.rev
         |> List.find_opt (fun child ->
                Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN)
      with
      | Some child -> name_token_from_ident_pattern child
      | None -> None)
  | _ -> None

let is_attribute_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.ATTRIBUTE_EXPR

let is_extension_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.EXTENSION_EXPR

let is_let_binding_node node =
  let kind = Ceibo.Red.SyntaxNode.kind node in
  kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING

let split_at_first_and_binding nodes =
  let rec loop acc = function
    | child :: rest when is_let_binding_node child ->
        (List.rev acc, child :: rest)
    | child :: rest ->
        loop (child :: acc) rest
    | [] -> (List.rev acc, [])
  in
  loop [] nodes

let let_expression_parts ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
         (direct_non_trivia_tokens node)
  in
  let binding_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | exception_decl :: rest
    when Ceibo.Red.SyntaxNode.kind exception_decl = Syntax_kind.EXCEPTION_DECL ->
      (match List.rev rest with
      | body_node :: _ ->
          Some (`Exception (exception_decl, body_node))
      | [] -> None)
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | body_node :: rev_prefix ->
          let prefix = List.rev rev_prefix in
          let binding_prefix, and_binding_nodes =
            split_at_first_and_binding prefix
          in
          (match List.rev binding_prefix with
          | bound_value_node :: rev_param_nodes ->
              Some
                (`Value
                  ( is_recursive_binding,
                    binding_pattern_node,
                    List.rev rev_param_nodes,
                    bound_value_node,
                    and_binding_nodes,
                    body_node ))
          | [] -> None)
      | [] -> None)
  | [] -> None

let is_binding_operator_expression_node node =
  let non_trivia_children =
    Ceibo.Red.SyntaxNode.children node
    |> Array.to_list
    |> List.filter (function
         | Ceibo.Red.Token syntax_token
           when is_trivia (Ceibo.Red.SyntaxToken.kind syntax_token) ->
             false
         | _ -> true)
  in
  match Ceibo.Red.SyntaxNode.kind node, non_trivia_children with
  | Syntax_kind.LET_EXPR,
    Ceibo.Red.Token let_kw
    :: Ceibo.Red.Token operator_syntax_token
    :: Ceibo.Red.Node pattern_node
    :: Ceibo.Red.Token equals_syntax_token
    :: _
    when String.equal (Ceibo.Red.SyntaxToken.text let_kw) "let"
         && is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind pattern_node)
         && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) "="
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "rec")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "open")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "module")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "exception") ->
      true
  | _ ->
      false

let binding_operator_pairs_from_tokens node =
  let rec loop acc = function
    | keyword_syntax_token :: operator_syntax_token :: equals_syntax_token :: rest
      when (String.equal (Ceibo.Red.SyntaxToken.text keyword_syntax_token) "let"
            || String.equal (Ceibo.Red.SyntaxToken.text keyword_syntax_token) "and")
           && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) "=" ->
        loop
          ((token keyword_syntax_token, token operator_syntax_token) :: acc)
          rest
    | in_syntax_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text in_syntax_token) "in" ->
        List.rev acc
    | [] ->
        List.rev acc
    | _ ->
        bail
          ~message:
            "expected binding-operator token sequence during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "expression"; "let_operator"; "tokens" ]
  in
  loop [] (direct_non_trivia_tokens node)

let first_ident_token_in_subtree node =
  let rec go_node node =
    match
      direct_non_trivia_tokens node
      |> List.find_opt (fun tok ->
             is_identifier_like_text (Ceibo.Red.SyntaxToken.text tok))
    with
    | Some tok -> Some (token tok)
    | None -> direct_non_trivia_nodes node |> List.find_map go_node
  in
  go_node node

let quoted_type_binder_from_node node =
  match first_ident_token_in_subtree node with
  | Some name_token ->
      Cst.TypeBinder.Quoted { syntax_node = node; name_token }
  | None ->
      bail ~message:"expected quantified type binder name during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_binder.quoted" ]

let bare_type_binders_from_tokens syntax_tokens =
  let rec collect started acc = function
    | [] ->
        List.rev acc
    | syntax_token :: rest ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        if started then
          if String.equal text "." then
            List.rev acc
          else if is_identifier_like_text text then
            collect true
              (Cst.TypeBinder.Bare { name_token = token syntax_token } :: acc)
              rest
          else
            collect true acc rest
        else if String.equal text "type" then
          collect true acc rest
        else
          collect false acc rest
  in
  collect false [] syntax_tokens

let bare_type_binders_from_node ~context node =
  let binders = bare_type_binders_from_tokens (direct_non_trivia_tokens node) in
  if List.length binders = 0 then
    bail
      ~message:
        "expected locally abstract type binders during Ceibo -> CST lifting"
      ~syntax_node:node ~context
  else
    binders

let locally_abstract_type_parameter_from_node node :
    Cst.locally_abstract_type_parameter =
  let binders =
    bare_type_binders_from_node ~context:[ "parameter.locally_abstract" ] node
  in
  { Cst.syntax_node = node; binders }

let constructor_pattern_existentials_from_node node :
    Cst.constructor_pattern_existentials =
  let binders =
    bare_type_binders_from_node
      ~context:[ "pattern.constructor.existentials" ]
      node
  in
  { Cst.syntax_node = node; binders }

let constructor_pattern_existentials_from_children node =
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         if
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM
         then
           Some (constructor_pattern_existentials_from_node child)
         else
           None)

let token_with_text node expected =
  subtree_non_trivia_tokens node
  |> List.find_opt (fun syntax_token ->
         String.equal (Ceibo.Red.SyntaxToken.text syntax_token) expected)
  |> Option.map token

let direct_token_with_text node expected =
  direct_non_trivia_tokens node
  |> List.find_opt (fun syntax_token ->
         String.equal (Ceibo.Red.SyntaxToken.text syntax_token) expected)
  |> Option.map token

let direct_required_token_with_text ~context node expected =
  match direct_token_with_text node expected with
  | Some token ->
      token
  | None ->
      bail
        ~message:
          ("expected '" ^ expected ^ "' token during Ceibo -> CST lifting")
        ~syntax_node:node ~context

let rec take_tokens_until_equals acc = function
  | [] -> List.rev acc
  | syntax_token :: rest ->
      if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=" then
        List.rev acc
      else
        take_tokens_until_equals (syntax_token :: acc) rest

let operator_tokens_from_node node =
  direct_non_trivia_tokens node
  |> List.filter (fun syntax_token ->
         let text = Ceibo.Red.SyntaxToken.text syntax_token in
         not
           (String.equal text "("
           || String.equal text ")"
           || String.equal text " "))
  |> List.map token

let arrow_label_from_node node =
  let text syntax_token = Ceibo.Red.SyntaxToken.text syntax_token in
  match direct_non_trivia_tokens node with
  | label_syntax_token :: colon_syntax_token :: _
    when String.equal (text colon_syntax_token) ":" ->
      Some
        (Cst.ArrowLabel.Named
           { sigil_token = None; label_token = token label_syntax_token })
  | sigil_syntax_token :: label_syntax_token :: colon_syntax_token :: _
    when String.equal (text sigil_syntax_token) "~"
         && String.equal (text colon_syntax_token) ":" ->
      Some
        (Cst.ArrowLabel.Named
           {
             sigil_token = Some (token sigil_syntax_token);
             label_token = token label_syntax_token;
           })
  | sigil_syntax_token :: label_syntax_token :: colon_syntax_token :: _
    when String.equal (text sigil_syntax_token) "?"
         && String.equal (text colon_syntax_token) ":" ->
      Some
        (Cst.ArrowLabel.OptionalNamed
           {
             sigil_token = token sigil_syntax_token;
             label_token = token label_syntax_token;
           })
  | _ ->
      None

let rec module_type_constraint_from_node node =
  match
    direct_non_trivia_nodes node |> List.filter can_lift_core_type_node
  with
  | constrained_type_node :: replacement_type_node :: _ ->
      let is_destructive =
        direct_non_trivia_tokens node
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":=")
      in
      Cst.ModuleTypeConstraint.
        {
          syntax_node = node;
          constrained_type = core_type_from_node constrained_type_node;
          replacement_type = core_type_from_node replacement_type_node;
          is_destructive;
        }
  | _ ->
      bail
        ~message:
          "expected constrained and replacement types in module type constraint during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_type.constraint" ]

and functor_parameter_from_node node =
  let name_token =
    match direct_non_trivia_tokens node with
    | _lparen :: name_token :: _ ->
        token name_token
    | _ ->
        bail ~message:"expected functor parameter name during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.functor.parameter" ]
  in
  let module_type =
    match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node with
    | Some module_type_node ->
        module_type_from_node module_type_node
    | None ->
        bail
          ~message:
            "expected functor parameter module type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.functor.parameter" ]
  in
  Cst.FunctorParameter.{ syntax_node = node; name_token; module_type }

and module_type_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH ->
      Cst.ModuleType.Path (module_path_from_node node)
  | Syntax_kind.MODULE_TYPE_OF -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: _ ->
          Cst.ModuleType.TypeOf
            {
              syntax_node = node;
              module_path = module_path_like_from_node module_path_node;
            }
      | [] ->
          bail
            ~message:
              "expected module path in module type of expression during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.type_of" ])
  | Syntax_kind.MODULE_TYPE_EXPR -> (
      match direct_non_trivia_nodes node with
      | base_node :: constraint_nodes ->
          Cst.ModuleType.With
            {
              syntax_node = node;
              base = module_type_from_node base_node;
              constraints =
                constraint_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
                |> List.map module_type_constraint_from_node;
            }
      | [] ->
          bail
            ~message:
              "expected base module type in constrained module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.with" ])
  | Syntax_kind.FUNCTOR_TYPE -> (
      match List.rev (direct_non_trivia_nodes node) with
      | result_node :: rev_parameter_nodes ->
          Cst.ModuleType.Functor
            {
              syntax_node = node;
              parameters =
                List.rev rev_parameter_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
                |> List.map functor_parameter_from_node;
              result = module_type_from_node result_node;
            }
      | [] ->
          bail
            ~message:
              "expected functor parameters and result module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.functor" ])
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node with
      | Some inner_node ->
          Cst.ModuleType.Parenthesized
            { syntax_node = node; inner = module_type_from_node inner_node }
      | None ->
          bail
            ~message:
              "expected inner module type in parenthesized module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.parenthesized" ])
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_module_type_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ModuleType.Attribute
                {
                  syntax_node = node;
                  module_type = module_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              bail
                ~message:
                  "expected attributed module type payload during Ceibo -> CST lifting"
                ~syntax_node:node ~context:[ "module_type.attribute" ])
      | [] ->
          bail
            ~message:
              "expected attributed module type contents during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.attribute" ])
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ModuleType.Extension (extension_from_node node)
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | sig_kw :: _ when String.equal (Ceibo.Red.SyntaxToken.text sig_kw) "sig" ->
          Cst.ModuleType.Signature
            { syntax_node = node; signature_syntax_node = node }
      | _ ->
          bail
            ~message:"unsupported module type identifier shape during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type" ])
  | _ ->
      bail ~message:"unsupported module type shape during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_type" ]

and module_type_from_first_class_module_type_node node =
  match direct_non_trivia_nodes node with
  | base_node :: constraint_nodes ->
      let base = module_type_from_node base_node in
      let constraints =
        constraint_nodes
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
        |> List.map module_type_constraint_from_node
      in
      if List.length constraints = 0 then
        base
      else
        Cst.ModuleType.With { syntax_node = node; base; constraints }
  | [] ->
      bail
        ~message:
          "expected module type inside first-class module type during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_type.first_class_module" ]

and core_type_payload_and_field_attribute node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR ->
      let attribute = attribute_from_node node in
      if attribute_is_item_like attribute then
        match attribute.payload_syntax_node with
        | Some payload_node when can_lift_core_type_node payload_node ->
            (payload_node, Some attribute)
        | _ ->
            (node, None)
      else
        (node, None)
  | _ ->
      (node, None)

and class_type_field_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.OBJECT_INHERIT ->
      let make_field class_type =
        Cst.ClassTypeField.Inherit { syntax_node = node; class_type }
      in
      begin
        match direct_non_trivia_nodes node with
        | child :: _ when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.APPLY_EXPR -> (
            match direct_non_trivia_nodes child with
            | payload_node :: rest -> (
                match List.find_opt is_attribute_node rest with
                | Some attribute_node when can_lift_class_type_node payload_node ->
                    Cst.ClassTypeField.Attribute
                      {
                        syntax_node = node;
                        field = make_field (class_type_from_node payload_node);
                        attribute = attribute_from_node attribute_node;
                      }
                | _ ->
                    unsupported_class_type node)
            | [] ->
                unsupported_class_type node)
        | child :: _ ->
            make_field (class_type_from_node child)
        | [] ->
            unsupported_class_type node
      end
  | Syntax_kind.OBJECT_VAL -> (
      match direct_non_trivia_nodes node with
      | name_node :: remainder
        when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
          match
            first_ident_token_in_subtree name_node,
            List.find_opt can_lift_core_type_node remainder
          with
          | Some name_token, Some type_node ->
              let payload_type_node, field_attribute =
                core_type_payload_and_field_attribute type_node
              in
              let field =
                Cst.ClassTypeField.Value
                  {
                    syntax_node = node;
                    name_token;
                    type_ = core_type_from_node payload_type_node;
                    is_mutable =
                      List.exists
                        (fun tok ->
                          String.equal (Ceibo.Red.SyntaxToken.text tok) "mutable")
                        (direct_non_trivia_tokens node);
                  }
              in
              (match field_attribute with
              | Some attribute ->
                  Cst.ClassTypeField.Attribute
                    { syntax_node = node; field; attribute }
              | None ->
                  field)
          | _ ->
              unsupported_class_type node)
      | _ ->
          unsupported_class_type node)
  | Syntax_kind.OBJECT_METHOD -> (
      match direct_non_trivia_nodes node with
      | name_node :: remainder
        when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
          match
            first_ident_token_in_subtree name_node,
            List.find_opt can_lift_core_type_node remainder
          with
          | Some name_token, Some type_node ->
              let payload_type_node, field_attribute =
                core_type_payload_and_field_attribute type_node
              in
              let field =
                Cst.ClassTypeField.Method
                  {
                    syntax_node = node;
                    name_token;
                    type_ = core_type_from_node payload_type_node;
                    is_private =
                      List.exists
                        (fun tok ->
                          String.equal (Ceibo.Red.SyntaxToken.text tok) "private")
                        (direct_non_trivia_tokens node);
                  }
              in
              (match field_attribute with
              | Some attribute ->
                  Cst.ClassTypeField.Attribute
                    { syntax_node = node; field; attribute }
              | None ->
                  field)
          | _ ->
              unsupported_class_type node)
      | _ ->
          unsupported_class_type node)
  | Syntax_kind.TYPE_CONSTRAINT -> (
      match
        direct_non_trivia_nodes node |> List.filter can_lift_core_type_node
      with
      | left_node :: right_node :: _ ->
          let payload_right_node, field_attribute =
            core_type_payload_and_field_attribute right_node
          in
          let field =
            Cst.ClassTypeField.Constraint
              {
                syntax_node = node;
                left = core_type_from_node left_node;
                right = core_type_from_node payload_right_node;
              }
          in
          (match field_attribute with
          | Some attribute ->
              Cst.ClassTypeField.Attribute
                { syntax_node = node; field; attribute }
          | None ->
              field)
      | _ ->
          unsupported_class_type node)
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_class_type_field_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ClassTypeField.Attribute
                {
                  syntax_node = node;
                  field = class_type_field_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_class_type node)
      | [] ->
          unsupported_class_type node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassTypeField.Extension (extension_from_node node)
  | _ ->
      unsupported_class_type node

and class_type_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ClassType.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ClassType.Path (module_path_from_node node)
  | Syntax_kind.TYPE_CONSTR ->
      if
        direct_non_trivia_nodes node
        |> List.exists (fun child ->
               let kind = Ceibo.Red.SyntaxNode.kind child in
               can_lift_core_type_node child
               && not (kind = Syntax_kind.IDENT_EXPR)
               && not (kind = Syntax_kind.MODULE_PATH)
               && not (kind = Syntax_kind.MODULE_TYPE_PATH))
      then
        unsupported_class_type node
      else
        Cst.ClassType.Path (type_constructor_path_from_node node)
  | Syntax_kind.TYPE_ARROW -> (
      match direct_non_trivia_nodes node with
      | parameter_node :: result_node :: _
        when can_lift_core_type_node parameter_node
             && can_lift_class_type_node result_node ->
          Cst.ClassType.Arrow
            {
              syntax_node = node;
              label = arrow_label_from_node node;
              parameter_type = core_type_from_node parameter_node;
              result_type = class_type_from_node result_node;
            }
      | _ ->
          unsupported_class_type node)
  | Syntax_kind.OBJECT_EXPR ->
      Cst.ClassType.Signature
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.map class_type_field_from_node;
        }
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: class_type_node :: _ ->
          Cst.ClassType.LocalOpen
            {
              syntax_node = node;
              module_path = module_path_like_from_node module_path_node;
              class_type = class_type_from_node class_type_node;
            }
      | _ ->
          unsupported_class_type node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | payload_node :: rest -> (
          match List.find_opt is_attribute_node rest with
          | Some attribute_node when can_lift_class_type_node payload_node ->
              Cst.ClassType.Attribute
                {
                  syntax_node = node;
                  class_type = class_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_class_type node)
      | [] ->
          unsupported_class_type node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_class_type_node with
      | Some inner_node ->
          Cst.ClassType.Parenthesized
            { syntax_node = node; inner = class_type_from_node inner_node }
      | None ->
          unsupported_class_type node)
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_class_type_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ClassType.Attribute
                {
                  syntax_node = node;
                  class_type = class_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_class_type node)
      | [] ->
          unsupported_class_type node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassType.Extension (extension_from_node node)
  | _ ->
      unsupported_class_type node

and core_type_from_node node =
  let child_type_nodes node =
    direct_non_trivia_nodes node
    |> List.filter can_lift_core_type_node
  in
  let type_binders_from_poly_type_node node =
    let quoted_binders =
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
      |> List.map quoted_type_binder_from_node
    in
    if List.length quoted_binders > 0 then
      quoted_binders
    else
      bare_type_binders_from_tokens (direct_non_trivia_tokens node)
  in
  let rec class_type_path_from_node node =
    let rec after_hash = function
      | [] -> None
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#" then
            Some (syntax_token, rest)
          else
            after_hash rest
    in
    match after_hash (direct_non_trivia_tokens node) with
    | Some (hash_syntax_token, class_path_tokens) ->
        if List.length class_path_tokens > 0 then
          ( token hash_syntax_token,
            module_path_from_tokens ~syntax_node:node class_path_tokens )
        else
          bail ~message:"expected class type path after # during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.class" ]
    | None ->
        bail ~message:"expected class type marker during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.class" ]
  and local_open_core_type_from_node node =
    match direct_non_trivia_nodes node with
    | module_path_node :: type_node :: _ ->
        {
          Cst.syntax_node = node;
          module_path = module_path_like_from_node module_path_node;
          type_ = core_type_from_node type_node;
        }
    | _ ->
        bail ~message:"expected local-open module path and body during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.local_open" ]
  and object_type_field_from_node node =
    match
      first_ident_token_in_subtree node,
      direct_non_trivia_nodes node
      |> List.find_opt can_lift_core_type_node
    with
    | Some field_name, Some field_type_node ->
        {
          Cst.syntax_node = node;
          field_name;
          field_type = core_type_from_node field_type_node;
        }
    | _ ->
        bail ~message:"expected object type field name and type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.object_field" ]
  and record_type_field_from_node node =
    let field_name =
      match direct_non_trivia_tokens node with
      | mutable_kw :: name_token :: _
        when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" ->
          Some (token name_token)
      | name_token :: _ -> Some (token name_token)
      | [] -> None
    in
    let mutable_field =
      match direct_non_trivia_tokens node with
      | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "mutable"
      | [] -> false
    in
    let field_type_node =
      direct_non_trivia_nodes node
      |> List.find_opt can_lift_core_type_node
    in
    match
      field_name,
      field_type_node
    with
    | Some field_name, Some field_type_node ->
        let field_type_node, attributes = peel_outer_type_attributes field_type_node in
        {
          Cst.syntax_node = node;
          field_name;
          field_type = core_type_from_node field_type_node;
          is_mutable = mutable_field;
          attributes;
        }
    | _ ->
        bail ~message:"expected record type field name and type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.record_field" ]
  and poly_variant_tag_from_node node =
    let direct_children = direct_non_trivia_nodes node in
    let lifted_attributes =
      direct_children
      |> List.filter is_attribute_node
      |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
      |> List.map attribute_from_node
    in
    match direct_non_trivia_tokens node with
    | _backtick :: tag_name :: _ ->
        {
          Cst.syntax_node = node;
          attributes = lifted_attributes;
          tag_name = token tag_name;
          payload_type =
            (direct_children
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
    | tag_name :: _ ->
        {
          Cst.syntax_node = node;
          attributes = lifted_attributes;
          tag_name = token tag_name;
          payload_type =
            (direct_children
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
    | [] ->
        bail ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.poly_variant_tag" ]
  and poly_variant_bound_from_node node =
    match direct_non_trivia_tokens node with
    | _open_bracket :: marker_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "<" ->
        Cst.PolyVariantBound.UpperBound { marker_token = token marker_token }
    | _open_bracket :: marker_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ">" ->
        Cst.PolyVariantBound.LowerBound { marker_token = token marker_token }
    | _ ->
        Cst.PolyVariantBound.Exact
  and row_field_from_node node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.POLY_VARIANT_TAG ->
        Cst.RowField.Tag (poly_variant_tag_from_node node)
    | _ when can_lift_core_type_node node ->
        let inherited_type =
          match Ceibo.Red.SyntaxNode.kind node with
          | Syntax_kind.TYPE_CONSTR ->
              Cst.CoreType.Constr
                {
                  syntax_node = node;
                  constructor_path = type_constructor_path_from_node node;
                  arguments = [];
                }
          | _ ->
              core_type_from_node node
        in
        Cst.RowField.Inherit
          { syntax_node = node; type_ = inherited_type }
    | _ ->
        bail ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.poly_variant.row_field" ]
  and poly_variant_from_node node =
    {
      Cst.syntax_node = node;
      kind = poly_variant_bound_from_node node;
      fields =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               let kind = Ceibo.Red.SyntaxNode.kind child in
               kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child)
        |> List.map row_field_from_node;
    }
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_VAR -> (
      match List.rev (direct_non_trivia_tokens node) with
      | syntax_token :: _ ->
          let lifted = token syntax_token in
          if String.equal (Cst.Token.text lifted) "_" then
            Cst.CoreType.Wildcard { syntax_node = node; wildcard_token = lifted }
          else
            Cst.CoreType.Var { syntax_node = node; name_token = lifted }
      | [] ->
          bail ~message:"expected type variable token during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.var" ])
  | Syntax_kind.TYPE_CONSTR ->
      let child_types = child_type_nodes node in
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let opens_with_lparen =
        match non_trivia_tokens with
        | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "("
        | [] -> false
      in
      let closes_with_rparen =
        match List.rev non_trivia_tokens with
        | last :: _ -> String.equal (Ceibo.Red.SyntaxToken.text last) ")"
        | [] -> false
      in
      if opens_with_lparen && closes_with_rparen && List.length child_types = 1 then
        match child_types with
        | [ inner_type ] ->
            Cst.CoreType.Parenthesized
              { syntax_node = node; inner = core_type_from_node inner_type }
        | _ ->
            bail ~message:"expected a single inner type inside parenthesized type"
              ~syntax_node:node ~context:[ "core_type.parenthesized" ]
      else if
        non_trivia_tokens
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#")
      then
        let hash_token, class_path = class_type_path_from_node node in
        Cst.CoreType.Class
          {
            syntax_node = node;
            hash_token;
            class_path;
            arguments = child_types |> List.map core_type_from_node;
          }
      else
        Cst.CoreType.Constr
          {
            syntax_node = node;
            constructor_path = type_constructor_path_from_node node;
            arguments = child_types |> List.map core_type_from_node;
          }
  | Syntax_kind.TYPE_ALIAS -> (
      match child_type_nodes node with
      | type_node :: alias_node :: _ -> (
          match List.rev (direct_non_trivia_tokens alias_node) with
          | alias_token :: _ ->
              Cst.CoreType.Alias
                {
                  syntax_node = node;
                  type_ = core_type_from_node type_node;
                  name_token = token alias_token;
                }
          | [] ->
              bail ~message:"expected alias name token during Ceibo -> CST lifting"
                ~syntax_node:alias_node ~context:[ "core_type.alias" ])
      | _ ->
          bail ~message:"expected aliased type and alias variable during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.alias" ])
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_core_type_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.CoreType.Attribute
                {
                  syntax_node = node;
                  type_ = core_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              bail ~message:"expected attribute payload during Ceibo -> CST lifting"
                ~syntax_node:node ~context:[ "core_type.attribute" ])
      | [] ->
          bail ~message:"expected attributed type payload during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.attribute" ])
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.CoreType.Extension (extension_from_node node)
  | Syntax_kind.POLY_TYPE -> (
      let binders = type_binders_from_poly_type_node node in
      let body_node =
        direct_non_trivia_nodes node
        |> List.rev
        |> List.find_opt (fun child ->
               can_lift_core_type_node child
               && Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_VAR)
      in
      match binders, body_node with
      | _ :: _, Some body_node ->
          Cst.CoreType.Poly
            {
              syntax_node = node;
              binders;
              body = core_type_from_node body_node;
            }
      | [], _ ->
          bail
            ~message:
              "expected quantified type binders during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.poly" ]
      | _, None ->
          bail
            ~message:"expected quantified type body during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.poly" ])
  | Syntax_kind.TYPE_ARROW -> (
      match child_type_nodes node with
      | parameter_node :: result_node :: _ ->
          Cst.CoreType.Arrow
            {
              syntax_node = node;
              label = arrow_label_from_node node;
              parameter_type = core_type_from_node parameter_node;
              result_type = core_type_from_node result_node;
            }
      | _ ->
          bail ~message:"expected arrow parameter and result types during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.arrow" ])
  | Syntax_kind.TYPE_TUPLE ->
      Cst.CoreType.Tuple
        {
          syntax_node = node;
          elements = child_type_nodes node |> List.map core_type_from_node;
        }
  | Syntax_kind.TYPE_PAREN -> (
      match child_type_nodes node with
      | inner_node :: _ ->
          Cst.CoreType.Parenthesized
            { syntax_node = node; inner = core_type_from_node inner_node }
      | [] ->
          bail ~message:"expected inner type inside parenthesized type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.parenthesized" ])
  | Syntax_kind.LOCAL_OPEN_TYPE ->
      Cst.CoreType.LocalOpen (local_open_core_type_from_node node)
  | Syntax_kind.TYPE_POLY_VARIANT ->
      Cst.CoreType.PolyVariant (poly_variant_from_node node)
  | Syntax_kind.TYPE_RECORD ->
      Cst.CoreType.Record
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
            |> List.map record_type_field_from_node;
        }
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE -> (
      Cst.CoreType.FirstClassModule
        {
          syntax_node = node;
          module_type = module_type_from_first_class_module_type_node node;
        })
  | Syntax_kind.OBJECT_TYPE ->
      Cst.CoreType.Object
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.OBJECT_TYPE_FIELD)
            |> List.map object_type_field_from_node;
        }
  | _ ->
      bail ~message:"unsupported core type shape during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "core_type" ]

let rec pattern_from_node node =
  let pattern_children node =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
    |> List.map pattern_from_node
  in
  let rec peel_outer_pattern_attributes node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.ATTRIBUTE_EXPR -> (
        match direct_non_trivia_nodes node with
        | first_child :: rest -> (
            match
              List.find_opt
                (fun child ->
                  is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                (first_child :: rest),
              List.find_opt is_attribute_node rest
            with
            | Some payload_node, Some attribute_node ->
                let payload_node, attributes =
                  peel_outer_pattern_attributes payload_node
                in
                (payload_node, attributes @ [ attribute_from_node attribute_node ])
            | _ ->
                (node, []))
        | [] ->
            (node, []))
    | _ ->
        (node, [])
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  let rec tuple_pattern_label_token_from_node node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.IDENT_PATTERN -> (
        match direct_non_trivia_tokens node with
        | label_syntax_token :: _ ->
            token label_syntax_token
        | [] ->
            unsupported_pattern node)
    | Syntax_kind.TYPED_PATTERN -> (
        match direct_non_trivia_nodes node with
        | label_pattern_node :: _ ->
            tuple_pattern_label_token_from_node label_pattern_node
        | [] ->
            unsupported_pattern node)
    | _ ->
        unsupported_pattern node
  in
  let tuple_pattern_from_node node =
    let labeled_punning_element label_pattern_node =
      {
        Cst.label_token = Some (tuple_pattern_label_token_from_node label_pattern_node);
        pattern = pattern_from_node label_pattern_node;
      }
    in
    let labeled_payload_element label_pattern_node payload_pattern_node =
      {
        Cst.label_token = Some (tuple_pattern_label_token_from_node label_pattern_node);
        pattern = pattern_from_node payload_pattern_node;
      }
    in
    let unlabeled_element pattern_node =
      {
        Cst.label_token = None;
        pattern = pattern_from_node pattern_node;
      }
    in
    let rec loop children saw_tilde pending_label awaiting_payload acc =
      match children with
      | [] ->
          let acc =
            match pending_label, awaiting_payload with
            | Some label_pattern_node, false ->
                labeled_punning_element label_pattern_node :: acc
            | _ ->
                acc
          in
          {
            Cst.syntax_node = node;
            elements = List.rev acc;
            open_tail = None;
            attributes = [];
          }
      | Ceibo.Red.Token syntax_token :: rest
        when is_trivia (Ceibo.Red.SyntaxToken.kind syntax_token) ->
          loop rest saw_tilde pending_label awaiting_payload acc
      | Ceibo.Red.Token syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "~" then
            loop rest true pending_label awaiting_payload acc
          else if String.equal text ":" then
            loop rest saw_tilde pending_label true acc
          else if String.equal text "," then
            let acc =
              match pending_label, awaiting_payload with
              | Some label_pattern_node, false ->
                  labeled_punning_element label_pattern_node :: acc
              | _ ->
                  acc
            in
            loop rest false None false acc
          else if String.equal text ".." then
            let acc =
              match pending_label, awaiting_payload with
              | Some label_pattern_node, false ->
                  labeled_punning_element label_pattern_node :: acc
              | _ ->
                  acc
            in
            {
              Cst.syntax_node = node;
              elements = List.rev acc;
              open_tail = Some { dotdot_token = token syntax_token };
              attributes = [];
            }
          else if String.equal text "(" || String.equal text ")" then
            loop rest saw_tilde pending_label awaiting_payload acc
          else
            loop rest saw_tilde pending_label awaiting_payload acc
      | Ceibo.Red.Node child :: rest ->
          if is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
            if awaiting_payload then
              match pending_label with
              | Some label_pattern_node ->
                  loop rest false None false
                    (labeled_payload_element label_pattern_node child :: acc)
              | None ->
                  loop rest false None false (unlabeled_element child :: acc)
            else if saw_tilde then
              loop rest false (Some child) false acc
            else
              let acc =
                match pending_label with
                | Some label_pattern_node ->
                    labeled_punning_element label_pattern_node :: acc
                | None ->
                    acc
              in
              loop rest false None false (unlabeled_element child :: acc)
          else
            loop rest saw_tilde pending_label awaiting_payload acc
    in
    Ceibo.Red.SyntaxNode.children node |> Array.to_list |> fun children ->
    loop children false None false []
  in
  let node, attributes = peel_outer_pattern_attributes node in
  let pattern =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.IDENT_PATTERN -> (
        match direct_non_trivia_tokens node with
        | first :: _ ->
            Cst.Pattern.Identifier
              { syntax_node = node; name_token = token first; attributes = [] }
        | [] -> unsupported_pattern node)
    | Syntax_kind.WILDCARD_PATTERN ->
        Cst.Pattern.Wildcard { syntax_node = node; attributes = [] }
    | Syntax_kind.ATTRIBUTE_EXPR ->
        unsupported_pattern node
    | Syntax_kind.EXTENSION_EXPR ->
        Cst.Pattern.Extension
          { syntax_node = node; extension = extension_from_node node; attributes = [] }
    | Syntax_kind.LAZY_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ ->
            Cst.Pattern.Lazy
              {
                syntax_node = node;
                pattern = pattern_from_node inner_node;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.EXCEPTION_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ ->
            Cst.Pattern.Exception
              {
                syntax_node = node;
                keyword_token =
                  direct_required_token_with_text ~context:[ "exception_pattern" ]
                    node "exception";
                pattern = pattern_from_node inner_node;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.RANGE_PATTERN -> (
        match direct_non_trivia_tokens node with
        | lower_syntax_token :: _range_syntax_token :: upper_syntax_token :: _ ->
            Cst.Pattern.Range
              {
                syntax_node = node;
                lower = constant_from_syntax_token ~syntax_node:node lower_syntax_token;
                upper = constant_from_syntax_token ~syntax_node:node upper_syntax_token;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.OPERATOR_PATTERN ->
        Cst.Pattern.Operator
          {
            syntax_node = node;
            operator_tokens = operator_tokens_from_node node;
            attributes = [];
          }
    | Syntax_kind.FIRST_CLASS_MODULE_PATTERN -> (
        match direct_non_trivia_tokens node with
        | _lparen :: _module_kw :: binding_syntax_token :: _ ->
            Cst.Pattern.FirstClassModule
              {
                syntax_node = node;
                binding =
                  if String.equal (Ceibo.Red.SyntaxToken.text binding_syntax_token) "_" then
                    Cst.Anonymous { wildcard_token = token binding_syntax_token }
                  else
                    Cst.Named { name_token = token binding_syntax_token };
                module_type =
                  (direct_non_trivia_nodes node
                   |> List.find_opt can_lift_module_type_node
                   |> Option.map module_type_from_node);
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.STRING_LITERAL | Syntax_kind.INT_LITERAL
    | Syntax_kind.FLOAT_LITERAL | Syntax_kind.CHAR_LITERAL
    | Syntax_kind.BOOL_LITERAL | Syntax_kind.UNIT_LITERAL ->
        Cst.Pattern.Literal
          { syntax_node = node; literal = constant_from_node node; attributes = [] }
    | Syntax_kind.POLY_VARIANT_PATTERN -> (
        match poly_variant_tag_token node with
        | Some tag_token ->
            Cst.Pattern.PolyVariant
              {
                syntax_node = node;
                tag_token;
                payload =
                  (direct_non_trivia_nodes node
                  |> List.find_opt (fun child ->
                         is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                  |> Option.map pattern_from_node);
                attributes = [];
              }
        | None -> unsupported_pattern node)
    | Syntax_kind.POLY_VARIANT_TYPE_PATTERN ->
        Cst.Pattern.PolyVariantInherit
          {
            syntax_node = node;
            type_path = module_path_like_from_node node;
            attributes = [];
          }
    | Syntax_kind.CONSTRUCTOR_PATTERN ->
        Cst.Pattern.Constructor
          {
            syntax_node = node;
            constructor_path = module_path_from_node node;
            existentials = constructor_pattern_existentials_from_children node;
            arguments = pattern_children node;
            attributes = [];
          }
    | Syntax_kind.TUPLE_PATTERN ->
        Cst.Pattern.Tuple (tuple_pattern_from_node node)
    | Syntax_kind.LIST_PATTERN ->
        Cst.Pattern.List
          { syntax_node = node; elements = pattern_children node; attributes = [] }
    | Syntax_kind.ARRAY_PATTERN ->
        Cst.Pattern.Array
          { syntax_node = node; elements = pattern_children node; attributes = [] }
    | Syntax_kind.RECORD_PATTERN ->
        let closedness =
          match
            direct_non_trivia_tokens node
            |> List.find_opt (fun syntax_token ->
                   String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "_")
          with
          | Some wildcard_token ->
              Cst.Open { wildcard_token = token wildcard_token }
          | None ->
              Cst.Closed
        in
        Cst.Pattern.Record
          {
            syntax_node = node;
            fields =
              direct_non_trivia_nodes node
              |> List.filter (fun child ->
                     Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD_PATTERN)
              |> List.filter_map record_pattern_field_from_node;
            closedness;
            attributes = [];
          }
    | Syntax_kind.CONS_PATTERN -> (
        match direct_non_trivia_nodes node with
        | head_node :: tail_node :: _ ->
            Cst.Pattern.Cons
              {
                syntax_node = node;
                head = pattern_from_node head_node;
                tail = pattern_from_node tail_node;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.OR_PATTERN ->
        Cst.Pattern.Or
          { syntax_node = node; alternatives = pattern_children node; attributes = [] }
    | Syntax_kind.AS_PATTERN -> (
        match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
        | pattern_node :: _, name_syntax_token :: _ ->
            Cst.Pattern.Alias
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                name_token = token name_syntax_token;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.TYPED_PATTERN -> (
        match direct_non_trivia_nodes node with
        | pattern_node :: type_node :: _ ->
            Cst.Pattern.Typed
              {
                syntax_node = node;
                pattern = pattern_from_node pattern_node;
                type_ = core_type_from_node type_node;
                attributes = [];
              }
        | _ -> unsupported_pattern node)
    | Syntax_kind.EFFECT_PATTERN -> (
        match effect_pattern_from_node node with
        | Some pattern -> Cst.Pattern.Effect pattern
        | None -> unsupported_pattern node)
    | Syntax_kind.LOCAL_OPEN_PATTERN -> (
        match local_open_pattern_from_node node with
        | Some pattern -> Cst.Pattern.LocalOpen pattern
        | None -> unsupported_pattern node)
    | Syntax_kind.PAREN_PATTERN -> (
        match direct_non_trivia_nodes node with
        | inner_node :: _ ->
            Cst.Pattern.Parenthesized
              {
                syntax_node = node;
                inner = pattern_from_node inner_node;
                attributes = [];
              }
        | [] -> unsupported_pattern node)
    | _ -> unsupported_pattern node
  in
  pattern_with_attributes pattern attributes

and record_pattern_field_from_node node =
  let lifted_field_path =
    let tokens =
      direct_non_trivia_tokens node |> take_tokens_until_equals []
    in
    module_path_from_tokens ~syntax_node:node tokens
  in
  match Cst.Ident.segments lifted_field_path with
  | [] -> None
  | _ ->
      Some
        {
          syntax_node = node;
          field_path = lifted_field_path;
          pattern =
            (direct_non_trivia_nodes node
            |> List.find_opt (fun child ->
                   is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
            |> Option.map pattern_from_node);
        }

and local_open_pattern_from_node node =
  match direct_non_trivia_nodes node with
  | module_path_node :: pattern_node :: _ ->
      Some
        {
          syntax_node = node;
          module_path = module_path_like_from_node module_path_node;
          pattern = pattern_from_node pattern_node;
          attributes = [];
        }
  | _ -> None

and effect_pattern_from_node node =
  match direct_non_trivia_nodes node with
  | effect_node :: continuation_node :: _ ->
      Some
        {
          syntax_node = node;
          effect_pattern = pattern_from_node effect_node;
          continuation = pattern_from_node continuation_node;
          attributes = [];
        }
  | _ -> None

let parameter_from_node node =
  let binding_pattern_from_direct_nodes ~label_name_token direct_nodes =
    match direct_nodes with
    | binding_pattern_node :: _
      when is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind binding_pattern_node) ->
        Some (pattern_from_node binding_pattern_node)
    | type_node :: _
      when is_type_syntax_kind (Ceibo.Red.SyntaxNode.kind type_node) ->
        let identifier_pattern =
          Cst.Pattern.Identifier
            {
              syntax_node = node;
              name_token = label_name_token;
              attributes = [];
            }
        in
        Some
          (Cst.Pattern.Typed
             {
               syntax_node = node;
               pattern = identifier_pattern;
               type_ = core_type_from_node type_node;
               attributes = [];
             })
    | _ ->
        None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_PARAM -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "~", first_ident_token_in_subtree node with
      | Some sigil_token, Some label_name_token ->
          let binding_pattern =
            binding_pattern_from_direct_nodes ~label_name_token direct_nodes
          in
          Cst.Parameter.Labeled
            {
              syntax_node = node;
              sigil_token = sigil_token;
              label_token = label_name_token;
              binding_name_token =
                (match binding_pattern with
                | Some pattern ->
                    simple_pattern_name_token (Cst.Pattern.syntax_node pattern)
                | None ->
                    None);
              binding_pattern;
            }
      | _ -> unsupported_parameter node)
  | Syntax_kind.OPTIONAL_PARAM -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "?", first_ident_token_in_subtree node with
      | Some sigil_token, Some label_name_token ->
          let binding_pattern =
            binding_pattern_from_direct_nodes ~label_name_token direct_nodes
          in
          Cst.Parameter.Optional
            {
              syntax_node = node;
              sigil_token = sigil_token;
              label_token = label_name_token;
              binding_name_token =
                (match binding_pattern with
                | Some pattern ->
                    simple_pattern_name_token (Cst.Pattern.syntax_node pattern)
                | None ->
                    None);
              has_default = false;
              binding_pattern;
            }
      | _ -> unsupported_parameter node)
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> (
      let direct_nodes = direct_non_trivia_nodes node in
      match token_with_text node "?", direct_nodes with
      | Some sigil_token, binding_pattern_node :: _default_value_node :: _ ->
          let binding_pattern = pattern_from_node binding_pattern_node in
          Cst.Parameter.Optional
            {
              syntax_node = node;
              sigil_token = sigil_token;
              label_token =
                (match first_ident_token_in_subtree binding_pattern_node with
                | Some token ->
                    token
                | None -> (
                    match first_ident_token_in_subtree node with
                    | Some token ->
                        token
                    | None ->
                        bail
                          ~message:
                            "expected optional parameter label during Ceibo -> CST lifting"
                          ~syntax_node:node ~context:[ "parameter.optional_default" ]));
              binding_name_token = simple_pattern_name_token binding_pattern_node;
              has_default = true;
              binding_pattern = Some binding_pattern;
            }
      | _ -> unsupported_parameter node)
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      Cst.Parameter.LocallyAbstract
        (locally_abstract_type_parameter_from_node node)
  | _ ->
      Cst.Parameter.Positional
        {
          syntax_node = node;
          pattern = pattern_from_node node;
          name_token = simple_pattern_name_token node;
        }

let parameter_with_attributes parameter attributes =
  match attributes, parameter with
  | [], _ ->
      parameter
  | _, Cst.Parameter.Positional parameter ->
      Cst.Parameter.Positional
        { parameter with pattern = pattern_with_attributes parameter.pattern attributes }
  | _, Cst.Parameter.Labeled parameter -> (
      match parameter.binding_pattern with
      | Some pattern ->
          Cst.Parameter.Labeled
            {
              parameter with
              binding_pattern = Some (pattern_with_attributes pattern attributes);
            }
      | None ->
          Cst.Parameter.Labeled parameter)
  | _, Cst.Parameter.Optional parameter -> (
      match parameter.binding_pattern with
      | Some pattern ->
          Cst.Parameter.Optional
            {
              parameter with
              binding_pattern = Some (pattern_with_attributes pattern attributes);
            }
      | None ->
          Cst.Parameter.Optional parameter)
  | _, Cst.Parameter.LocallyAbstract _ ->
      parameter

let standalone_attribute_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.ATTRIBUTE_EXPR
  && not
       (direct_non_trivia_nodes node
       |> List.exists (fun child ->
              let kind = Ceibo.Red.SyntaxNode.kind child in
              is_parameter_like_kind kind || is_pattern_syntax_kind kind))

let parameter_candidate_node node =
  let kind = Ceibo.Red.SyntaxNode.kind node in
  is_parameter_like_kind kind
  || (kind = Syntax_kind.ATTRIBUTE_EXPR && not (standalone_attribute_node node))

let parameters_from_nodes nodes =
  let rec loop pending_attributes acc = function
    | [] ->
        List.rev acc
    | node :: rest when standalone_attribute_node node ->
        loop (pending_attributes @ [ attribute_from_node node ]) acc rest
    | node :: rest when parameter_candidate_node node ->
        let parameter =
          parameter_with_attributes (parameter_from_node node) pending_attributes
        in
        loop [] (parameter :: acc) rest
    | _ :: rest ->
        loop pending_attributes acc rest
  in
  loop [] [] nodes

let rec apply_argument_from_node node =
  let first_nontrivia_expression_child node =
    direct_non_trivia_nodes node
    |> List.find_opt can_lift_expression_node
    |> Option.map expression_from_node
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_ARG -> (
      match direct_non_trivia_tokens node with
      | sigil_syntax_token :: label_syntax_token :: _ ->
          Cst.Labeled
            {
              syntax_node = node;
              sigil_token = token sigil_syntax_token;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.OPTIONAL_ARG -> (
      match direct_non_trivia_tokens node with
      | sigil_syntax_token :: label_syntax_token :: _ ->
          Cst.Optional
            {
              syntax_node = node;
              sigil_token = token sigil_syntax_token;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> unsupported_expression node)
  | _ -> Cst.Positional (expression_from_node node)

(* Let-binding annotations do not have dedicated expression nodes, so the CST
   reconstructs typed/polymorphic wrappers around the bound value. *)
and expression_with_type_annotation ~syntax_node ~expression type_node =
  let type_ = core_type_from_node type_node in
  match type_ with
  | Cst.CoreType.Poly { binders; _ }
    when List.exists Cst.TypeBinder.is_quoted binders ->
      Cst.Expression.Polymorphic { syntax_node; expression; type_; attributes = [] }
  | _ ->
      Cst.Expression.Typed { syntax_node; expression; type_; attributes = [] }

and binding_type_annotation_node prefix_nodes =
  prefix_nodes |> List.find_opt can_lift_core_type_node

and binding_parameter_nodes prefix_nodes =
  prefix_nodes
  |> List.filter (fun child ->
         is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))

and binding_parameters_from_prefix prefix_nodes =
  binding_parameter_nodes prefix_nodes |> parameters_from_nodes

and binding_value_from_prefix ~binding_syntax_node ~prefix_nodes ~value_node =
  let value = expression_from_node value_node in
  match binding_type_annotation_node prefix_nodes with
  | Some type_node ->
      expression_with_type_annotation ~syntax_node:binding_syntax_node
        ~expression:value type_node
  | None ->
      value

and constrain_module_expression ~syntax_node ~module_expression module_type =
  Cst.ModuleExpression.Constraint
    { syntax_node; module_expression; module_type }

and module_expression_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ModuleExpression.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ModuleExpression.Path (module_path_from_node node)
  | Syntax_kind.STRUCT_EXPR ->
      Cst.ModuleExpression.Structure
        {
          syntax_node = node;
          item_syntax_nodes = direct_non_trivia_nodes node;
        }
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node, List.rev (direct_non_trivia_nodes node) with
      | functor_kw :: _, body_node :: rev_parameter_nodes
        when String.equal (Ceibo.Red.SyntaxToken.text functor_kw) "functor" ->
          Cst.ModuleExpression.Functor
            {
              syntax_node = node;
              parameters =
                List.rev rev_parameter_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
                |> List.map functor_parameter_from_node;
              body = module_expression_from_node body_node;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.MODULE_APPLICATION -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Cst.ModuleExpression.Apply
            {
              syntax_node = node;
              callee = module_expression_from_node callee_node;
              argument = module_expression_from_node argument_node;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.MODULE_UNIT_APPLICATION -> (
      match direct_non_trivia_nodes node with
      | callee_node :: _ ->
          Cst.ModuleExpression.ApplyUnit
            {
              syntax_node = node;
              callee = module_expression_from_node callee_node;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match non_paren_tokens node, direct_non_trivia_nodes node with
      | val_kw :: _, expression_node :: _
        when String.equal (Ceibo.Red.SyntaxToken.text val_kw) "val" ->
          let module_type =
            direct_non_trivia_nodes node
            |> List.find_opt can_lift_module_type_node
            |> Option.map module_type_from_node
          in
          Cst.ModuleExpression.ModuleUnpack
            {
              syntax_node = node;
              expression = expression_from_node expression_node;
              module_type;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_expression_node with
      | Some inner_node ->
          Cst.ModuleExpression.Parenthesized
            { syntax_node = node; inner = module_expression_from_node inner_node }
      | None ->
          unsupported_module_expression node)
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_module_expression_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ModuleExpression.Attribute
                {
                  syntax_node = node;
                  module_expression = module_expression_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_module_expression node)
      | [] ->
          unsupported_module_expression node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ModuleExpression.Extension (extension_from_node node)
  | _ ->
      unsupported_module_expression node

and expression_from_node node =
  let known_expression_children node =
    direct_non_trivia_nodes node
    |> List.filter can_lift_expression_node
    |> List.map expression_from_node
  in
  let rec peel_outer_expression_attributes node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.ATTRIBUTE_EXPR -> (
        match direct_non_trivia_nodes node with
        | first_child :: rest -> (
            match
              List.find_opt can_lift_expression_node (first_child :: rest),
              List.find_opt is_attribute_node rest
            with
            | Some payload_node, Some attribute_node ->
                let payload_node, attributes =
                  peel_outer_expression_attributes payload_node
                in
                (payload_node, attributes @ [ attribute_from_node attribute_node ])
            | _ ->
                (node, []))
        | [] ->
            (node, []))
    | _ ->
        (node, [])
  in
  let constant_with_attributes constant attributes =
    match attributes with
    | [] ->
        constant
    | _ ->
        let append existing = existing @ attributes in
        match constant with
        | Cst.Constant.String constant ->
            Cst.Constant.String { constant with attributes = append constant.attributes }
        | Cst.Constant.Int constant ->
            Cst.Constant.Int { constant with attributes = append constant.attributes }
        | Cst.Constant.Float constant ->
            Cst.Constant.Float { constant with attributes = append constant.attributes }
        | Cst.Constant.Char constant ->
            Cst.Constant.Char { constant with attributes = append constant.attributes }
        | Cst.Constant.Bool constant ->
            Cst.Constant.Bool { constant with attributes = append constant.attributes }
        | Cst.Constant.Unit constant ->
            Cst.Constant.Unit { constant with attributes = append constant.attributes }
  in
  let expression_with_attributes expression attributes =
    match attributes with
    | [] ->
        expression
    | _ ->
        let append existing = existing @ attributes in
        match expression with
        | Cst.Expression.Path expr ->
            Cst.Expression.Path { expr with attributes = append expr.attributes }
        | Cst.Expression.Constructor expr ->
            Cst.Expression.Constructor { expr with attributes = append expr.attributes }
        | Cst.Expression.Operator expr ->
            Cst.Expression.Operator { expr with attributes = append expr.attributes }
        | Cst.Expression.Literal literal ->
            Cst.Expression.Literal (constant_with_attributes literal attributes)
        | Cst.Expression.Unreachable expr ->
            Cst.Expression.Unreachable { expr with attributes = append expr.attributes }
        | Cst.Expression.Extension ext ->
            Cst.Expression.Extension { ext with attributes = append ext.attributes }
        | Cst.Expression.Object expr ->
            Cst.Expression.Object { expr with attributes = append expr.attributes }
        | Cst.Expression.PolyVariant expr ->
            Cst.Expression.PolyVariant { expr with attributes = append expr.attributes }
        | Cst.Expression.ModulePack expr ->
            Cst.Expression.ModulePack
              { expr with attributes = append expr.attributes }
        | Cst.Expression.LetModule expr ->
            Cst.Expression.LetModule { expr with attributes = append expr.attributes }
        | Cst.Expression.LetException expr ->
            Cst.Expression.LetException { expr with attributes = append expr.attributes }
        | Cst.Expression.Assert expr ->
            Cst.Expression.Assert { expr with attributes = append expr.attributes }
        | Cst.Expression.Lazy expr ->
            Cst.Expression.Lazy { expr with attributes = append expr.attributes }
        | Cst.Expression.While expr ->
            Cst.Expression.While { expr with attributes = append expr.attributes }
        | Cst.Expression.For expr ->
            Cst.Expression.For { expr with attributes = append expr.attributes }
        | Cst.Expression.Apply expr ->
            Cst.Expression.Apply { expr with attributes = append expr.attributes }
        | Cst.Expression.MethodCall expr ->
            Cst.Expression.MethodCall { expr with attributes = append expr.attributes }
        | Cst.Expression.New expr ->
            Cst.Expression.New { expr with attributes = append expr.attributes }
        | Cst.Expression.Prefix expr ->
            Cst.Expression.Prefix { expr with attributes = append expr.attributes }
        | Cst.Expression.FieldAccess expr ->
            Cst.Expression.FieldAccess { expr with attributes = append expr.attributes }
        | Cst.Expression.Index expr ->
            Cst.Expression.Index { expr with attributes = append expr.attributes }
        | Cst.Expression.ObjectOverride expr ->
            Cst.Expression.ObjectOverride
              { expr with attributes = append expr.attributes }
        | Cst.Expression.InstanceVariableAssign expr ->
            Cst.Expression.InstanceVariableAssign
              { expr with attributes = append expr.attributes }
        | Cst.Expression.FieldAssign expr ->
            Cst.Expression.FieldAssign { expr with attributes = append expr.attributes }
        | Cst.Expression.Assign expr ->
            Cst.Expression.Assign { expr with attributes = append expr.attributes }
        | Cst.Expression.Infix expr ->
            Cst.Expression.Infix { expr with attributes = append expr.attributes }
        | Cst.Expression.Typed expr ->
            Cst.Expression.Typed { expr with attributes = append expr.attributes }
        | Cst.Expression.Polymorphic expr ->
            Cst.Expression.Polymorphic { expr with attributes = append expr.attributes }
        | Cst.Expression.Coerce expr ->
            Cst.Expression.Coerce { expr with attributes = append expr.attributes }
        | Cst.Expression.Sequence expr ->
            Cst.Expression.Sequence { expr with attributes = append expr.attributes }
        | Cst.Expression.Tuple expr ->
            Cst.Expression.Tuple { expr with attributes = append expr.attributes }
        | Cst.Expression.List expr ->
            Cst.Expression.List { expr with attributes = append expr.attributes }
        | Cst.Expression.Array expr ->
            Cst.Expression.Array { expr with attributes = append expr.attributes }
        | Cst.Expression.Record (Cst.RecordExpression.Literal expr) ->
            Cst.Expression.Record
              (Cst.RecordExpression.Literal
                 { expr with attributes = append expr.attributes })
        | Cst.Expression.Record (Cst.RecordExpression.Update expr) ->
            Cst.Expression.Record
              (Cst.RecordExpression.Update
                 { expr with attributes = append expr.attributes })
        | Cst.Expression.LocalOpen expr ->
            Cst.Expression.LocalOpen { expr with attributes = append expr.attributes }
        | Cst.Expression.Fun expr ->
            Cst.Expression.Fun { expr with attributes = append expr.attributes }
        | Cst.Expression.Function expr ->
            Cst.Expression.Function { expr with attributes = append expr.attributes }
        | Cst.Expression.LetOperator expr ->
            Cst.Expression.LetOperator { expr with attributes = append expr.attributes }
        | Cst.Expression.Let expr ->
            Cst.Expression.Let { expr with attributes = append expr.attributes }
        | Cst.Expression.Match expr ->
            Cst.Expression.Match { expr with attributes = append expr.attributes }
        | Cst.Expression.Try expr ->
            Cst.Expression.Try { expr with attributes = append expr.attributes }
        | Cst.Expression.If expr ->
            Cst.Expression.If { expr with attributes = append expr.attributes }
        | Cst.Expression.Parenthesized expr ->
            Cst.Expression.Parenthesized
              { expr with attributes = append expr.attributes }
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  let node, attributes = peel_outer_expression_attributes node in
  let expression =
    match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR -> (
      let path = ident_path_from_node node in
      if is_constructor_path path then
        Cst.Expression.Constructor
          {
            syntax_node = node;
            constructor_path = path;
            payload = None;
            attributes = [];
          }
      else
        Cst.Expression.Path { syntax_node = node; path; attributes = [] })
  | Syntax_kind.MODULE_PATH -> (
      let path = module_path_from_node node in
      if is_constructor_path path then
        Cst.Expression.Constructor
          {
            syntax_node = node;
            constructor_path = path;
            payload = None;
            attributes = [];
          }
      else
        Cst.Expression.Path { syntax_node = node; path; attributes = [] })
  | Syntax_kind.OPERATOR_PATTERN ->
      Cst.Expression.Operator
        {
          syntax_node = node;
          operator_tokens = operator_tokens_from_node node;
          attributes = [];
        }
  | Syntax_kind.UNREACHABLE_EXPR -> (
      match direct_non_trivia_tokens node with
      | dot_syntax_token :: _ ->
          Cst.Expression.Unreachable
            {
              syntax_node = node;
              dot_token = token dot_syntax_token;
              attributes = [];
            }
      | [] -> unsupported_expression node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.Expression.Extension (extension_from_node node)
  | Syntax_kind.OBJECT_EXPR -> (
      match object_expression_from_node node with
      | Some expr -> Cst.Expression.Object expr
      | None -> unsupported_expression node)
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Expression.Literal (constant_from_node node)
  | Syntax_kind.METHOD_CALL_EXPR -> (
      match method_call_expression_from_node node with
      | Some expr -> Cst.Expression.MethodCall expr
      | None -> unsupported_expression node)
  | Syntax_kind.NEW_EXPR -> (
      match new_expression_from_node node with
      | Some expr -> Cst.Expression.New expr
      | None -> unsupported_expression node)
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match constructor_path_from_expression_node node with
      | Some constructor_path ->
          Cst.Expression.Constructor
            {
              syntax_node = node;
              constructor_path;
              payload = None;
              attributes = [];
            }
      | None -> (
          match field_access_expression_from_node node with
          | Some expr -> expr
          | None -> unsupported_expression node))
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> unsupported_expression node)
  | Syntax_kind.STRING_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> unsupported_expression node)
  | Syntax_kind.ASSIGN_EXPR -> (
      match assign_expression_from_node node with
      | Some expr -> expr
      | None -> unsupported_expression node)
  | Syntax_kind.STRING_LITERAL | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL ->
      Cst.Expression.Literal (constant_from_node node)
  | Syntax_kind.ASSERT_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt can_lift_expression_node
        |> Option.map expression_from_node
      with
      | Some asserted ->
          Cst.Expression.Assert { syntax_node = node; asserted; attributes = [] }
      | None -> unsupported_expression node)
  | Syntax_kind.LAZY_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt can_lift_expression_node
        |> Option.map expression_from_node
      with
      | Some body -> Cst.Expression.Lazy { syntax_node = node; body; attributes = [] }
      | None -> unsupported_expression node)
  | Syntax_kind.WHILE_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.filter (fun child -> not (is_attribute_node child))
      with
      | condition_node :: body_node :: _ ->
          Cst.Expression.While
            {
              syntax_node = node;
              condition = expression_from_node condition_node;
              body = expression_from_node body_node;
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.FOR_EXPR -> (
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let direction =
        non_trivia_tokens
        |> List.find_opt (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               String.equal text "to" || String.equal text "downto")
        |> Option.map (fun direction_syntax_token ->
               let direction_token = token direction_syntax_token in
               match Ceibo.Red.SyntaxToken.text direction_syntax_token with
               | "to" ->
                   Cst.To { direction_token }
               | "downto" ->
                   Cst.Downto { direction_token }
               | _ ->
                   bail
                     ~message:
                       "expected for-loop direction token during Ceibo -> CST lifting"
                     ~syntax_node:node ~context:[ "expression.for"; "direction" ])
      in
      match
        ( direct_non_trivia_nodes node
          |> List.filter (fun child -> not (is_attribute_node child)) ),
        non_trivia_tokens,
        direction
      with
      | start_node :: end_node :: body_node :: _, _for_kw :: iterator_syntax_token :: _, Some direction ->
          Cst.Expression.For
            {
              syntax_node = node;
              iterator_token = token iterator_syntax_token;
              start_expr = expression_from_node start_node;
              direction;
              end_expr = expression_from_node end_node;
              body = expression_from_node body_node;
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: [ attribute_node ]
        when Ceibo.Red.SyntaxNode.kind attribute_node = Syntax_kind.ATTRIBUTE_EXPR ->
          expression_with_attributes (expression_from_node callee_node)
            [ attribute_from_node attribute_node ]
      | callee_node :: argument_node :: _ -> (
          match
            constructor_path_from_expression_node callee_node,
            apply_argument_from_node argument_node
          with
          | Some constructor_path, Cst.Positional payload ->
              Cst.Expression.Constructor
                {
                  syntax_node = node;
                  constructor_path;
                  payload = Some payload;
                  attributes = [];
                }
          | _ ->
              Cst.Expression.Apply
                {
                  syntax_node = node;
                  callee = expression_from_node callee_node;
                  argument = apply_argument_from_node argument_node;
                  attributes = [];
                })
      | _ -> unsupported_expression node)
  | Syntax_kind.POLY_VARIANT_EXPR -> (
      match poly_variant_tag_token node with
      | Some tag_token ->
          Cst.Expression.PolyVariant
            {
              syntax_node = node;
              tag_token;
              payload =
                (direct_non_trivia_nodes node
                |> List.find_opt can_lift_expression_node
                |> Option.map expression_from_node);
              attributes = [];
            }
      | None -> unsupported_expression node)
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match non_paren_tokens node, direct_non_trivia_nodes node with
      | module_kw :: _, module_expression_node :: _
        when String.equal (Ceibo.Red.SyntaxToken.text module_kw) "module" ->
          let module_type =
            direct_non_trivia_nodes node
            |> List.find_opt can_lift_module_type_node
            |> Option.map module_type_from_node
          in
          Cst.Expression.ModulePack
            {
              syntax_node = node;
              module_expression = module_expression_from_node module_expression_node;
              module_type;
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.LET_MODULE_EXPR -> (
      match let_module_expression_from_node node with
      | Some expr -> Cst.Expression.LetModule expr
      | None -> unsupported_expression node)
  | Syntax_kind.LET_EXPR -> (
      if is_binding_operator_expression_node node then
        match let_operator_expression_from_node node with
        | Some expr -> Cst.Expression.LetOperator expr
        | None -> unsupported_expression node
      else
        match let_expression_parts ~is_recursive_binding:false node with
        | Some (`Exception (exception_decl_node, body_node)) -> (
            match direct_non_trivia_tokens exception_decl_node with
            | _exception_kw :: name_syntax_token :: _ ->
                Cst.Expression.LetException
                  {
                    syntax_node = node;
                    exception_declaration =
                      {
                        syntax_node = exception_decl_node;
                        name_token = token name_syntax_token;
                      };
                    body = expression_from_node body_node;
                    attributes = [];
                  }
            | _ -> unsupported_expression node)
        | _ -> (
            match let_expression_from_node ~is_recursive_binding:false node with
            | Some expr -> Cst.Expression.Let expr
            | None -> unsupported_expression node))
  | Syntax_kind.LET_REC_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.Expression.Let expr
      | None -> unsupported_expression node)
  | Syntax_kind.TYPED_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: type_node :: _ ->
          expression_with_type_annotation ~syntax_node:node
            ~expression:(expression_from_node expr_node) type_node
      | _ -> unsupported_expression node)
  | Syntax_kind.COERCE_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: to_type_node :: [] ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type = None;
              to_type = core_type_from_node to_type_node;
              attributes = [];
            }
      | expr_node :: from_type_node :: to_type_node :: _ ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type = Some (core_type_from_node from_type_node);
              to_type = core_type_from_node to_type_node;
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.PREFIX_EXPR -> (
      match prefix_expression_from_node node with
      | Some expr -> Cst.Expression.Prefix expr
      | None -> unsupported_expression node)
  | Syntax_kind.INFIX_EXPR -> (
      match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
      | left_node :: right_node :: _, operator_syntax_token :: _ ->
          Cst.Expression.Infix
            {
              syntax_node = node;
              left = expression_from_node left_node;
              operator_token = token operator_syntax_token;
              right = expression_from_node right_node;
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.SEQUENCE_EXPR -> (
      match sequence_expression_from_node node with
      | Some expr -> Cst.Expression.Sequence expr
      | None -> unsupported_expression node)
  | Syntax_kind.TUPLE_EXPR ->
      Cst.Expression.Tuple
        { syntax_node = node; elements = known_expression_children node; attributes = [] }
  | Syntax_kind.LIST_EXPR ->
      Cst.Expression.List
        { syntax_node = node; elements = known_expression_children node; attributes = [] }
  | Syntax_kind.ARRAY_EXPR ->
      Cst.Expression.Array
        { syntax_node = node; elements = known_expression_children node; attributes = [] }
  | Syntax_kind.RECORD_EXPR -> (
      match record_literal_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Literal expr)
      | None -> unsupported_expression node)
  | Syntax_kind.RECORD_UPDATE_EXPR -> (
      match record_update_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Update expr)
      | None -> unsupported_expression node)
  | Syntax_kind.OBJECT_UPDATE_EXPR -> (
      match object_override_expression_from_node node with
      | Some expr -> Cst.Expression.ObjectOverride expr
      | None -> unsupported_expression node)
  | Syntax_kind.LOCAL_OPEN_EXPR -> (
      match local_open_expression_from_node node with
      | Some expr -> Cst.Expression.LocalOpen expr
      | None -> unsupported_expression node)
  | Syntax_kind.FUN_EXPR -> (
      match fun_expression_from_node node with
      | Some expr -> Cst.Expression.Fun expr
      | None -> unsupported_expression node)
  | Syntax_kind.FUNCTION_EXPR -> (
      match function_expression_from_node node with
      | Some expr -> Cst.Expression.Function expr
      | None -> unsupported_expression node)
  | Syntax_kind.MATCH_EXPR -> (
      match match_expression_from_node node with
      | Some expr -> Cst.Expression.Match expr
      | None -> unsupported_expression node)
  | Syntax_kind.TRY_EXPR -> (
      match try_expression_from_node node with
      | Some expr -> Cst.Expression.Try expr
      | None -> unsupported_expression node)
  | Syntax_kind.IF_EXPR -> (
      let expression_children =
        direct_non_trivia_nodes node
        |> List.filter can_lift_expression_node
      in
      match expression_children with
      | condition_node :: then_node :: else_nodes ->
          Cst.Expression.If
            {
              syntax_node = node;
              keyword_token =
                direct_required_token_with_text ~context:[ "if_expression" ]
                  node "if";
              then_token =
                direct_required_token_with_text ~context:[ "if_expression" ]
                  node "then";
              else_token = direct_token_with_text node "else";
              condition = expression_from_node condition_node;
              then_branch = expression_from_node then_node;
              else_branch =
                (match else_nodes with
                | else_node :: _ -> Some (expression_from_node else_node)
                | [] -> None);
              attributes = [];
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.PAREN_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.filter (fun child -> not (is_attribute_node child))
      with
      | inner_node :: _ ->
          let opening_token, closing_token =
            match first_and_last_direct_token node with
            | Some (opening_token, closing_token) ->
                (token opening_token, token closing_token)
            | None ->
                bail
                  ~message:
                    "expected grouping delimiter tokens during Ceibo -> CST lifting"
                  ~syntax_node:node ~context:[ "parenthesized_expression" ]
          in
          Cst.Expression.Parenthesized
            {
              syntax_node = node;
              opening_token;
              closing_token;
              grouping = expression_grouping_from_node node;
              inner = expression_from_node inner_node;
              attributes = [];
            }
      | [] -> unsupported_expression node)
  | _ -> unsupported_expression node
  in
  expression_with_attributes expression attributes

and object_method_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          Some
            {
              Cst.syntax_node = node;
              attributes = attributes_from_node node;
              name_token;
              body =
                (remainder
                |> List.rev
                |> List.find_opt can_lift_expression_node
                |> Option.map expression_from_node);
              type_ =
                List.find_opt
                  can_lift_core_type_node
                  remainder
                |> Option.map core_type_from_node;
              is_private =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "private")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
      | None -> None)
  | _ -> None

and object_value_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          Some
            {
              Cst.syntax_node = node;
              attributes = attributes_from_node node;
              name_token;
              value =
                (remainder
                |> List.rev
                |> List.find_opt can_lift_expression_node
                |> Option.map expression_from_node);
              type_ =
                List.find_opt
                  can_lift_core_type_node
                  remainder
                |> Option.map core_type_from_node;
              is_mutable =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "mutable")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
      | None -> None)
  | _ -> None

and object_inherit_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
    |> List.find_map (fun child ->
           if can_lift_expression_node child then
             Some (expression_from_node child)
           else
             None)
  with
  | Some expression ->
      Some
        {
          Cst.syntax_node = node;
          attributes = attributes_from_node node;
          expression;
        }
  | None -> None

and object_initializer_from_node node =
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | initializer_kw :: _, children
    when String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer" ->
      let body =
        children
        |> List.filter (fun child -> not (is_attribute_node child))
        |> List.find_map (fun child ->
               if can_lift_expression_node child then
                 Some (expression_from_node child)
               else
                 None)
      in
      Some ({ syntax_node = node; body } : Cst.object_initializer)
  | _ -> None

and object_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let self_pattern, member_children =
    match non_trivia_children with
    | self_node :: rest
      when Ceibo.Red.SyntaxNode.kind self_node = Syntax_kind.OBJECT_SELF -> (
        match direct_non_trivia_nodes self_node with
        | pattern_node :: _ -> (Some (pattern_from_node pattern_node), rest)
        | [] -> (None, rest))
    | _ -> (None, non_trivia_children)
  in
  let rec lift_members acc = function
    | [] -> Some (List.rev acc)
    | child :: rest -> (
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.OBJECT_METHOD -> (
            match object_method_from_node child with
            | Some member ->
                lift_members ((Cst.ObjectMember.Method member) :: acc) rest
            | None -> None)
        | Syntax_kind.OBJECT_VAL -> (
            match object_value_from_node child with
            | Some member ->
                lift_members ((Cst.ObjectMember.Value member) :: acc) rest
            | None -> None)
        | Syntax_kind.OBJECT_INHERIT -> (
            match object_inherit_from_node child with
            | Some member ->
                lift_members ((Cst.ObjectMember.Inherit member) :: acc) rest
            | None -> None)
        | Syntax_kind.EXTENSION_EXPR ->
            lift_members ((Cst.ObjectMember.Extension (extension_from_node child)) :: acc)
              rest
        | Syntax_kind.IDENT_EXPR -> (
            match object_initializer_from_node child with
            | Some member ->
                lift_members ((Cst.ObjectMember.Initializer member) :: acc) rest
            | None -> None)
        | Syntax_kind.ATTRIBUTE_EXPR ->
            lift_members acc rest
        | _ -> None)
  in
  match lift_members [] member_children with
  | Some members ->
      Some
        ({ syntax_node = node; self_pattern; members; attributes = [] }
          : Cst.object_expression)
  | None -> None

and method_call_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, method_name_tok :: _ ->
      Some
        {
          Cst.syntax_node = node;
          receiver = expression_from_node receiver_node;
          method_name = token method_name_tok;
          attributes = [];
        }
  | _ -> None

and new_expression_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  with
  | class_path_node :: _ ->
      Some
        {
          Cst.syntax_node = node;
          class_path = module_path_like_from_node class_path_node;
          attributes = [];
        }
  | [] -> None

and object_override_field_from_node node =
  let lifted_field_path = record_field_path_from_node node in
  match Cst.Ident.segments lifted_field_path with
  | [ field_name ] ->
      Some
        (({
            Cst.syntax_node = node;
            field_name;
            value = record_field_value_from_node node;
          }
          : Cst.object_override_field))
  | _ -> None

and object_override_expression_from_node node =
  let children = direct_non_trivia_nodes node in
  if
    List.for_all
      (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
      children
  then
    Some
      {
        Cst.syntax_node = node;
        fields = List.filter_map object_override_field_from_node children;
        attributes = [];
      }
  else
    None

and field_access_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, field_token :: _ ->
      let receiver =
        match module_like_path_from_expression_node receiver_node with
        | Some path ->
            Cst.Expression.Path { syntax_node = receiver_node; path; attributes = [] }
        | None ->
            expression_from_node receiver_node
      in
      Some
        (Cst.Expression.FieldAccess
           {
             syntax_node = node;
             receiver;
             field_name = token field_token;
             attributes = [];
           })
  | _ -> None

and index_expression_from_node node =
  match direct_non_trivia_nodes node with
  | collection_node :: index_node :: _ ->
      Some
        {
          syntax_node = node;
          collection = expression_from_node collection_node;
          index = expression_from_node index_node;
          attributes = [];
        }
  | _ -> None

and assign_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | target_node :: value_node :: _, operator_syntax_token :: _ ->
      let operator_token = token operator_syntax_token in
      Some
        (match
           Ceibo.Red.SyntaxNode.kind target_node,
           direct_non_trivia_tokens target_node
         with
        | Syntax_kind.IDENT_EXPR, name_syntax_token :: _
          when String.equal (Cst.Token.text operator_token) "<-" ->
            Cst.Expression.InstanceVariableAssign
              {
                syntax_node = node;
                name_token = token name_syntax_token;
                operator_token;
                value = expression_from_node value_node;
                attributes = [];
              }
        | Syntax_kind.FIELD_ACCESS_EXPR, _
          when String.equal (Cst.Token.text operator_token) "<-" -> (
            match field_access_expression_from_node target_node with
            | Some (Cst.Expression.FieldAccess target) ->
                Cst.Expression.FieldAssign
                  {
                    syntax_node = node;
                    target;
                    operator_token;
                    value = expression_from_node value_node;
                    attributes = [];
                  }
            | _ ->
                Cst.Expression.Assign
                  {
                    syntax_node = node;
                    target = expression_from_node target_node;
                    operator_token;
                    value = expression_from_node value_node;
                    attributes = [];
                  })
        | _ ->
            Cst.Expression.Assign
              {
                syntax_node = node;
                target = expression_from_node target_node;
                operator_token;
                value = expression_from_node value_node;
                attributes = [];
              })
  | _ -> None

and prefix_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | operand_node :: _, operator_syntax_token :: _ ->
      Some
        {
          syntax_node = node;
          operator_token = token operator_syntax_token;
          operand = expression_from_node operand_node;
          attributes = [];
        }
  | _ -> None

and sequence_expression_from_node node =
  match direct_non_trivia_nodes node with
  | first :: rest ->
      Some
        {
          syntax_node = node;
          separator_token =
            direct_required_token_with_text ~context:[ "sequence_expression" ]
              node ";";
          expressions = List.map expression_from_node (first :: rest);
          attributes = [];
        }
  | _ -> None

and record_field_path_from_node node =
  let tokens =
    direct_non_trivia_tokens node |> take_tokens_until_equals []
  in
  module_path_from_tokens ~syntax_node:node tokens

and record_field_value_from_node node =
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         if can_lift_expression_node child then
           Some (expression_from_node child)
         else
           None)

and record_expression_field_from_node node =
  let lifted_field_path = record_field_path_from_node node in
  match Cst.Ident.last_segment lifted_field_path with
  | None -> None
  | Some field_name ->
      let value, source =
        match record_field_value_from_node node with
        | Some value -> (value, Cst.Explicit)
        | None ->
            ( Cst.Expression.Path
                { syntax_node = node; path = lifted_field_path; attributes = [] },
              Cst.Punned )
      in
      Some
        (({
            Cst.syntax_node = node;
            field_path = lifted_field_path;
            field_name;
            value;
            source;
          }
          : Cst.record_expression_field))

and record_literal_expression_from_node node =
  let fields =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
    |> List.filter_map record_expression_field_from_node
  in
  Some ({ syntax_node = node; fields; attributes = [] } : Cst.record_literal_expression)

and record_update_expression_from_node node =
  match direct_non_trivia_nodes node with
  | base_node :: rest -> (
      let lifted_base =
        match Ceibo.Red.SyntaxNode.kind base_node with
        | Syntax_kind.RECORD_FIELD -> (
            match record_expression_field_from_node base_node with
            | Some { field_path; source = Cst.Punned; _ } ->
                Some
                  (Cst.Expression.Path
                     { syntax_node = base_node; path = field_path; attributes = [] })
            | _ -> None)
        | _ ->
            if can_lift_expression_node base_node then
              Some (expression_from_node base_node)
            else
              None
      in
      match lifted_base with
      | Some base ->
          Some
            ( {
                syntax_node = node;
                base;
                fields =
                  rest
                  |> List.filter (fun child ->
                         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
                  |> List.filter_map record_expression_field_from_node;
                attributes = [];
              }
              : Cst.record_update_expression )
      | None -> None)
  | [] -> None

and local_open_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  let via_let_open =
    non_trivia_tokens
    |> List.exists (fun tok ->
           String.equal (Ceibo.Red.SyntaxToken.text tok) "let")
  in
  let module_path =
    let rec collect_after_open = function
      | [] -> []
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "open" then
            collect_module_tokens [] rest
          else collect_after_open rest
    and collect_module_tokens acc = function
      | [] -> List.rev acc
      | syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "in" then
            List.rev acc
          else collect_module_tokens (syntax_token :: acc) rest
    in
    match collect_after_open non_trivia_tokens with
    | [] -> None
    | lifted_tokens ->
        Some (module_path_from_tokens ~syntax_node:node lifted_tokens)
  in
  let prefix_module_path =
    match non_trivia_children with
    | module_path_node :: _body_node :: _ ->
        Some (module_path_like_from_node module_path_node)
    | _ -> None
  in
  let module_path =
    if via_let_open then module_path else prefix_module_path
  in
  let body_expr =
    if via_let_open then
      List.rev non_trivia_children
      |> List.find_map (fun child ->
             if can_lift_expression_node child then
               Some (expression_from_node child)
             else
               None)
    else
      match non_trivia_children with
      | _module_path_node :: body_node :: _ ->
          if can_lift_expression_node body_node then
            Some (expression_from_node body_node)
          else
            None
      | _ -> None
  in
  match module_path, body_expr with
  | Some lifted_module_path, Some lifted_body ->
      Some
        {
          syntax_node = node;
          module_path = lifted_module_path;
          body = lifted_body;
          via_let_open;
          attributes = [];
        }
  | _ -> None

and let_module_expression_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let payload_children =
    direct_children |> List.filter (fun child -> not (is_attribute_node child))
  in
  match direct_non_trivia_tokens node, payload_children with
  | _let_kw :: _module_kw :: module_name_token :: _, module_expression_node :: body_node :: _ ->
      Some
        {
          syntax_node = node;
          module_name_token = token module_name_token;
          module_expression = module_expression_from_node module_expression_node;
          body = expression_from_node body_node;
          attributes = [];
        }
  | _ -> None

and fun_expression_from_node node =
  match List.rev (direct_non_trivia_nodes node) with
  | body_node :: rev_prefix_nodes ->
      let prefix_nodes = List.rev rev_prefix_nodes in
      Some
        {
          syntax_node = node;
          keyword_token =
            direct_required_token_with_text ~context:[ "fun_expression" ] node
              "fun";
          arrow_token =
            direct_required_token_with_text ~context:[ "fun_expression" ] node
              "->";
          parameters = parameters_from_nodes prefix_nodes;
          body = fun_body_from_node body_node;
          attributes = [];
        }
  | [] -> None

and function_case_body_from_node node =
  let cases =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
    |> List.filter_map match_case_from_node
  in
  let cases =
    match direct_token_with_text node "|", cases with
    | Some leading_bar_token, ({ Cst.bar_token = None; _ } as first_case) :: rest ->
        { first_case with bar_token = Some leading_bar_token } :: rest
    | _ ->
        cases
  in
  ({ Cst.syntax_node = node; cases } : Cst.function_case_body)

and fun_body_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.FUNCTION_EXPR ->
      Cst.Cases (function_case_body_from_node node)
  | _ ->
      Cst.Expression (expression_from_node node)

and function_expression_from_node node =
  Some
    {
      Cst.syntax_node = node;
      keyword_token =
        direct_required_token_with_text ~context:[ "function_expression" ] node
          "function";
      cases = (function_case_body_from_node node).cases;
      attributes = [];
    }

and let_operator_expression_from_node node =
  let operator_pairs = binding_operator_pairs_from_tokens node in
  let lifted_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match List.rev lifted_children with
  | body_node :: rev_binding_nodes ->
      let binding_nodes = List.rev rev_binding_nodes in
      let expected_binding_nodes = 2 * List.length operator_pairs in
      if
        List.length operator_pairs = 0
        || not (Int.equal (List.length binding_nodes) expected_binding_nodes)
      then
        None
      else
        let rec lift_bindings acc token_pairs nodes =
          match token_pairs, nodes with
          | [], [] ->
              List.rev acc
          | (keyword_token, operator_token) :: rest_pairs, binding_pattern_node :: bound_value_node :: rest_nodes ->
              let binding : Cst.binding_operator_binding =
                {
                  keyword_token = keyword_token;
                  operator_token = operator_token;
                  binding_pattern = pattern_from_node binding_pattern_node;
                  bound_value = expression_from_node bound_value_node;
                }
              in
              lift_bindings (binding :: acc) rest_pairs rest_nodes
          | _ ->
              bail
                ~message:
                  "expected alternating binding-operator pattern/value nodes during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "expression"; "let_operator"; "bindings" ]
        in
        (match lift_bindings [] operator_pairs binding_nodes with
        | binding :: and_bindings ->
            Some
              {
                Cst.syntax_node = node;
                binding;
                and_bindings;
                body = expression_from_node body_node;
                attributes = [];
              }
        | [] -> None)
  | [] ->
      None

and let_expression_from_node ~is_recursive_binding node =
  if is_binding_operator_expression_node node then
    None
  else
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          prefix_nodes,
          bound_value_node,
          and_binding_nodes,
          body_node )) ->
      let and_keyword_tokens =
        direct_non_trivia_tokens node
        |> List.filter (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
        |> List.map token
      in
      let lift_and_binding and_keyword_token node =
        let direct_children = direct_non_trivia_nodes node in
        let binding_attributes =
          direct_children
          |> List.filter is_attribute_node
          |> List.map attribute_from_node
        in
        let binding_children =
          direct_children |> List.filter (fun child -> not (is_attribute_node child))
        in
        match binding_children with
        | nested_binding_pattern_node :: rest -> (
            match List.rev rest with
            | value_node :: rev_param_nodes ->
                let prefix_nodes = List.rev rev_param_nodes in
                Some
                  Cst.LetBinding.
                    {
                      syntax_node = node;
                      keyword_token = and_keyword_token;
                      rec_token = direct_token_with_text node "rec";
                      equals_token =
                        (match direct_token_with_text node "=" with
                        | Some equals_token ->
                            equals_token
                        | None ->
                            bail
                              ~message:
                                "expected and-binding equals during Ceibo -> CST lifting"
                              ~syntax_node:node ~context:[ "let_expression.and" ]);
                      attributes = binding_attributes;
                      binding_pattern = pattern_from_node nested_binding_pattern_node;
                      binding_name =
                        simple_pattern_name_token nested_binding_pattern_node;
                      parameters = binding_parameters_from_prefix prefix_nodes;
                      value =
                        binding_value_from_prefix ~binding_syntax_node:node
                          ~prefix_nodes:prefix_nodes ~value_node:value_node;
                      and_bindings = [];
                      is_recursive = is_recursive_binding;
                    }
            | [] -> None)
        | [] -> None
      in
      Some
        {
          syntax_node = node;
          keyword_token =
            (match direct_token_with_text node "let" with
            | Some keyword_token ->
                keyword_token
            | None ->
                bail ~message:"expected let keyword during Ceibo -> CST lifting"
                  ~syntax_node:node ~context:[ "let_expression" ]);
          rec_token = direct_token_with_text node "rec";
          equals_token =
            (match direct_token_with_text node "=" with
            | Some equals_token ->
                equals_token
            | None ->
                bail ~message:"expected let equals during Ceibo -> CST lifting"
                  ~syntax_node:node ~context:[ "let_expression" ]);
          in_token =
            (match direct_token_with_text node "in" with
            | Some in_token ->
                in_token
            | None ->
                bail ~message:"expected let-in keyword during Ceibo -> CST lifting"
                  ~syntax_node:node ~context:[ "let_expression" ]);
          binding_pattern = pattern_from_node binding_pattern_node;
          parameters = binding_parameters_from_prefix prefix_nodes;
          bound_value =
            binding_value_from_prefix ~binding_syntax_node:node
              ~prefix_nodes:prefix_nodes ~value_node:bound_value_node;
          and_bindings =
            and_binding_nodes
            |> List.mapi (fun index and_binding_node ->
                   match List.nth_opt and_keyword_tokens index with
                   | Some and_keyword_token ->
                       lift_and_binding and_keyword_token and_binding_node
                   | None ->
                       bail
                         ~message:
                           "expected matching and keyword for let-expression binding during Ceibo -> CST lifting"
                         ~syntax_node:and_binding_node
                         ~context:[ "let_expression"; "and_bindings" ])
            |> List.filter_map (fun binding -> binding);
          body = expression_from_node body_node;
          is_recursive = is_recursive_binding;
          attributes = [];
        }
  | _ -> None

and apply_payload_and_item_attribute ~can_lift_payload node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | payload_node :: [ attribute_node ]
        when is_attribute_node attribute_node ->
          let attribute = attribute_from_node attribute_node in
          if attribute_is_item_like attribute && can_lift_payload payload_node then
            (payload_node, Some attribute)
          else
            (node, None)
      | _ ->
          (node, None))
  | _ ->
      (node, None)

and expression_payload_and_field_attribute node =
  apply_payload_and_item_attribute
    ~can_lift_payload:can_lift_expression_node
    node

and class_expression_payload_and_field_attribute node =
  apply_payload_and_item_attribute ~can_lift_payload:can_lift_class_expression_node
    node

and class_method_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let body, field_attributes =
            match
              remainder
              |> List.rev
              |> List.find_opt can_lift_expression_node
            with
            | Some body_node ->
                let payload_node, field_attribute =
                  expression_payload_and_field_attribute body_node
                in
                ( Some (expression_from_node payload_node),
                  Option.to_list field_attribute )
            | None ->
                (None, [])
          in
          let type_, type_attributes =
            match List.find_opt can_lift_core_type_node remainder with
            | Some type_node ->
                let payload_type_node, field_attribute =
                  core_type_payload_and_field_attribute type_node
                in
                ( Some (core_type_from_node payload_type_node),
                  Option.to_list field_attribute )
            | None ->
                (None, [])
          in
          let field : Cst.class_method =
            {
              Cst.syntax_node = node;
              name_token;
              body;
              type_;
              is_private =
                List.exists
                  (fun tok ->
                    String.equal (Ceibo.Red.SyntaxToken.text tok) "private")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok ->
                    String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
          in
          Some (field, type_attributes @ field_attributes)
      | None -> None)
  | _ ->
      None

and class_value_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          let value, field_attributes =
            match
              remainder
              |> List.rev
              |> List.find_opt can_lift_expression_node
            with
            | Some value_node ->
                let payload_node, field_attribute =
                  expression_payload_and_field_attribute value_node
                in
                ( Some (expression_from_node payload_node),
                  Option.to_list field_attribute )
            | None ->
                (None, [])
          in
          let type_, type_attributes =
            match List.find_opt can_lift_core_type_node remainder with
            | Some type_node ->
                let payload_type_node, field_attribute =
                  core_type_payload_and_field_attribute type_node
                in
                ( Some (core_type_from_node payload_type_node),
                  Option.to_list field_attribute )
            | None ->
                (None, [])
          in
          let field : Cst.class_value =
            {
              Cst.syntax_node = node;
              name_token;
              value;
              type_;
              is_mutable =
                List.exists
                  (fun tok ->
                    String.equal (Ceibo.Red.SyntaxToken.text tok) "mutable")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok ->
                    String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
          in
          Some (field, type_attributes @ field_attributes)
      | None -> None)
  | _ ->
      None

and class_inherit_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
    |> List.find_map (fun child ->
           if can_lift_class_expression_node child then
             let payload_node, field_attribute =
               class_expression_payload_and_field_attribute child
             in
             Some
               ( class_expression_from_node payload_node,
                 Option.to_list field_attribute )
           else
             None)
  with
  | Some (class_expression, field_attributes) ->
      Some
        ( ({ syntax_node = node; class_expression } : Cst.class_inherit),
          field_attributes )
  | None ->
      None

and class_constraint_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter can_lift_core_type_node
  with
  | left_node :: right_node :: _ ->
      let payload_right_node, _field_attribute =
        core_type_payload_and_field_attribute right_node
      in
      Some
        ( ({ syntax_node = node;
             left = core_type_from_node left_node;
             right = core_type_from_node payload_right_node;
           }
            : Cst.class_constraint),
          [] )
  | _ ->
      None

and class_initializer_from_node node =
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | initializer_kw :: _, children
    when String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer" ->
      let body, field_attributes =
        match
          children
          |> List.filter (fun child -> not (is_attribute_node child))
          |> List.find_map (fun child ->
                 if can_lift_expression_node child then
                   let payload_node, field_attribute =
                     expression_payload_and_field_attribute child
                   in
                   Some
                     ( Some (expression_from_node payload_node),
                       Option.to_list field_attribute )
                 else
                   None)
        with
        | Some body_and_attributes ->
            body_and_attributes
        | None ->
            (None, [])
      in
      Some (({ syntax_node = node; body } : Cst.class_initializer), field_attributes)
  | _ ->
      None

and class_field_from_node node =
  let direct_attributes = attributes_from_node node in
  let field, lifted_attributes =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.OBJECT_METHOD -> (
        match class_method_from_node node with
        | Some (field, field_attributes) ->
            (Cst.ClassField.Method field, field_attributes)
        | None -> unsupported_class_expression node)
    | Syntax_kind.OBJECT_VAL -> (
        match class_value_from_node node with
        | Some (field, field_attributes) ->
            (Cst.ClassField.Value field, field_attributes)
        | None -> unsupported_class_expression node)
    | Syntax_kind.OBJECT_INHERIT -> (
        match class_inherit_from_node node with
        | Some (field, field_attributes) ->
            (Cst.ClassField.Inherit field, field_attributes)
        | None -> unsupported_class_expression node)
    | Syntax_kind.TYPE_CONSTRAINT -> (
        match class_constraint_from_node node with
        | Some (field, field_attributes) ->
            (Cst.ClassField.Constraint field, field_attributes)
        | None -> unsupported_class_expression node)
    | Syntax_kind.EXTENSION_EXPR ->
        (Cst.ClassField.Extension (extension_from_node node), [])
    | Syntax_kind.IDENT_EXPR when is_initializer_node node -> (
        match class_initializer_from_node node with
        | Some (field, field_attributes) ->
            (Cst.ClassField.Initializer field, field_attributes)
        | None -> unsupported_class_expression node)
    | _ ->
        unsupported_class_expression node
  in
  class_field_with_attributes field (direct_attributes @ lifted_attributes)

and class_structure_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let self_pattern, field_children =
    match non_trivia_children with
    | self_node :: rest
      when Ceibo.Red.SyntaxNode.kind self_node = Syntax_kind.OBJECT_SELF -> (
        match direct_non_trivia_nodes self_node with
        | pattern_node :: _ -> (Some (pattern_from_node pattern_node), rest)
        | [] -> (None, rest))
    | _ ->
        (None, non_trivia_children)
  in
  let rec lift_fields acc = function
    | [] ->
        Some (List.rev acc)
    | child :: rest -> (
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.OBJECT_METHOD
        | Syntax_kind.OBJECT_VAL
        | Syntax_kind.OBJECT_INHERIT
        | Syntax_kind.TYPE_CONSTRAINT
        | Syntax_kind.EXTENSION_EXPR ->
            lift_fields (class_field_from_node child :: acc) rest
        | Syntax_kind.IDENT_EXPR when is_initializer_node child ->
            lift_fields (class_field_from_node child :: acc) rest
        | _ ->
            None)
  in
  match lift_fields [] field_children with
  | Some fields ->
      Some ({ syntax_node = node; self_pattern; fields } : Cst.class_structure)
  | None ->
      None

and class_let_expression_from_node ~is_recursive_binding node =
  if is_binding_operator_expression_node node then
    None
  else
    match let_expression_parts ~is_recursive_binding node with
    | Some
        (`Value
          ( is_recursive_binding,
            binding_pattern_node,
            prefix_nodes,
            bound_value_node,
            and_binding_nodes,
            body_node ))
      when can_lift_class_expression_node body_node ->
        let and_keyword_tokens =
          direct_non_trivia_tokens node
          |> List.filter (fun syntax_token ->
                 String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "and")
          |> List.map token
        in
        let lift_and_binding and_keyword_token node =
          let direct_children = direct_non_trivia_nodes node in
          let binding_attributes =
            direct_children
            |> List.filter is_attribute_node
            |> List.map attribute_from_node
          in
          let binding_children =
            direct_children |> List.filter (fun child -> not (is_attribute_node child))
          in
          match binding_children with
          | nested_binding_pattern_node :: rest -> (
              match List.rev rest with
              | value_node :: rev_param_nodes ->
                  let prefix_nodes = List.rev rev_param_nodes in
                  Some
                    Cst.LetBinding.
                      {
                        syntax_node = node;
                        keyword_token = and_keyword_token;
                        rec_token = direct_token_with_text node "rec";
                        equals_token =
                          (match direct_token_with_text node "=" with
                          | Some equals_token ->
                              equals_token
                          | None ->
                              bail
                                ~message:
                                  "expected class let and-binding equals during Ceibo -> CST lifting"
                                ~syntax_node:node ~context:[ "class_let_expression.and" ]);
                        attributes = binding_attributes;
                        binding_pattern = pattern_from_node nested_binding_pattern_node;
                        binding_name =
                          simple_pattern_name_token nested_binding_pattern_node;
                        parameters = binding_parameters_from_prefix prefix_nodes;
                        value =
                          binding_value_from_prefix ~binding_syntax_node:node
                            ~prefix_nodes:prefix_nodes ~value_node:value_node;
                        and_bindings = [];
                        is_recursive = is_recursive_binding;
                      }
              | [] -> None)
          | [] -> None
        in
        Some
          ({
             syntax_node = node;
             keyword_token =
               (match direct_token_with_text node "let" with
               | Some keyword_token ->
                   keyword_token
               | None ->
                   bail
                     ~message:"expected class let keyword during Ceibo -> CST lifting"
                     ~syntax_node:node ~context:[ "class_let_expression" ]);
             rec_token = direct_token_with_text node "rec";
             equals_token =
               (match direct_token_with_text node "=" with
               | Some equals_token ->
                   equals_token
               | None ->
                   bail
                     ~message:"expected class let equals during Ceibo -> CST lifting"
                     ~syntax_node:node ~context:[ "class_let_expression" ]);
             in_token =
               (match direct_token_with_text node "in" with
               | Some in_token ->
                   in_token
               | None ->
                   bail
                     ~message:"expected class let-in keyword during Ceibo -> CST lifting"
                     ~syntax_node:node ~context:[ "class_let_expression" ]);
             binding_pattern = pattern_from_node binding_pattern_node;
             parameters = binding_parameters_from_prefix prefix_nodes;
             bound_value =
               binding_value_from_prefix ~binding_syntax_node:node
                 ~prefix_nodes:prefix_nodes ~value_node:bound_value_node;
             and_bindings =
               and_binding_nodes
               |> List.mapi (fun index and_binding_node ->
                      match List.nth_opt and_keyword_tokens index with
                      | Some and_keyword_token ->
                          lift_and_binding and_keyword_token and_binding_node
                      | None ->
                          bail
                            ~message:
                              "expected matching and keyword for class let-expression binding during Ceibo -> CST lifting"
                            ~syntax_node:and_binding_node
                            ~context:[ "class_let_expression"; "and_bindings" ])
               |> List.filter_map (fun binding -> binding);
             body = class_expression_from_node body_node;
             is_recursive = is_recursive_binding;
           }
            : Cst.class_let_expression)
    | _ ->
        None

and local_open_class_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  let via_let_open =
    non_trivia_tokens
    |> List.exists (fun tok ->
           String.equal (Ceibo.Red.SyntaxToken.text tok) "let")
  in
  let module_path =
    let rec collect_after_open = function
      | [] -> []
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "open" then
            collect_module_tokens [] rest
          else collect_after_open rest
    and collect_module_tokens acc = function
      | [] -> List.rev acc
      | syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "in" then
            List.rev acc
          else collect_module_tokens (syntax_token :: acc) rest
    in
    match collect_after_open non_trivia_tokens with
    | [] -> None
    | lifted_tokens ->
        Some (module_path_from_tokens ~syntax_node:node lifted_tokens)
  in
  let prefix_module_path =
    match non_trivia_children with
    | module_path_node :: _body_node :: _ ->
        Some (module_path_like_from_node module_path_node)
    | _ ->
        None
  in
  let module_path =
    if via_let_open then module_path else prefix_module_path
  in
  let body_expr =
    if via_let_open then
      List.rev non_trivia_children
      |> List.find_map (fun child ->
             if can_lift_class_expression_node child then
               Some (class_expression_from_node child)
             else
               None)
    else
      match non_trivia_children with
      | _module_path_node :: body_node :: _ when can_lift_class_expression_node body_node ->
          Some (class_expression_from_node body_node)
      | _ ->
          None
  in
  match module_path, body_expr with
  | Some module_path, Some class_expression ->
      Some
        ({
           syntax_node = node;
           module_path;
           class_expression;
           via_let_open;
         }
          : Cst.local_open_class_expression)
  | _ ->
      None

and class_expression_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ClassExpression.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ClassExpression.Path (module_path_from_node node)
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: class_expression_node :: _ ->
          Cst.ClassExpression.LocalOpen
            {
              syntax_node = node;
              module_path = module_path_like_from_node module_path_node;
              class_expression = class_expression_from_node class_expression_node;
              via_let_open = false;
            }
      | _ ->
          unsupported_class_expression node)
  | Syntax_kind.OBJECT_EXPR -> (
      match class_structure_from_node node with
      | Some structure -> Cst.ClassExpression.Structure structure
      | None -> unsupported_class_expression node)
  | Syntax_kind.FUN_EXPR -> (
      match List.rev (direct_non_trivia_nodes node) with
      | body_node :: rev_param_nodes when can_lift_class_expression_node body_node ->
          Cst.ClassExpression.Fun
            {
              syntax_node = node;
              parameters = rev_param_nodes |> List.rev |> parameters_from_nodes;
              body = class_expression_from_node body_node;
            }
      | _ ->
          unsupported_class_expression node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: [ attribute_node ]
        when can_lift_class_expression_node callee_node
             && Ceibo.Red.SyntaxNode.kind attribute_node = Syntax_kind.ATTRIBUTE_EXPR ->
          Cst.ClassExpression.Attribute
            {
              syntax_node = node;
              class_expression = class_expression_from_node callee_node;
              attribute = attribute_from_node attribute_node;
            }
      | callee_node :: argument_node :: _ when can_lift_class_expression_node callee_node ->
          Cst.ClassExpression.Apply
            {
              syntax_node = node;
              callee = class_expression_from_node callee_node;
              argument = apply_argument_from_node argument_node;
            }
      | _ ->
          unsupported_class_expression node)
  | Syntax_kind.LET_EXPR -> (
      match class_let_expression_from_node ~is_recursive_binding:false node with
      | Some expr -> Cst.ClassExpression.Let expr
      | None -> unsupported_class_expression node)
  | Syntax_kind.LET_REC_EXPR -> (
      match class_let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.ClassExpression.Let expr
      | None -> unsupported_class_expression node)
  | Syntax_kind.TYPED_EXPR -> (
      match direct_non_trivia_nodes node with
      | expression_node :: type_node :: _
        when can_lift_class_expression_node expression_node
             && can_lift_class_type_node type_node ->
          Cst.ClassExpression.Constraint
            {
              syntax_node = node;
              class_expression = class_expression_from_node expression_node;
              class_type = class_type_from_node type_node;
            }
      | _ ->
          unsupported_class_expression node)
  | Syntax_kind.LOCAL_OPEN_EXPR -> (
      match local_open_class_expression_from_node node with
      | Some expr -> Cst.ClassExpression.LocalOpen expr
      | None -> unsupported_class_expression node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_class_expression_node with
      | Some inner_node ->
          Cst.ClassExpression.Parenthesized
            { syntax_node = node; inner = class_expression_from_node inner_node }
      | None ->
          unsupported_class_expression node)
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_class_expression_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ClassExpression.Attribute
                {
                  syntax_node = node;
                  class_expression = class_expression_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_class_expression node)
      | [] ->
          unsupported_class_expression node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ClassExpression.Extension (extension_from_node node)
  | _ ->
      unsupported_class_expression node

and let_binding_from_binding_operator_binding ~binding_syntax_node
    ( { keyword_token = binding_keyword_token; binding_pattern = clause_pattern; bound_value = clause_value; _ } :
        Cst.binding_operator_binding ) =
  Cst.LetBinding.
    {
      syntax_node = binding_syntax_node;
      keyword_token = binding_keyword_token;
      rec_token = None;
      equals_token =
        (match
           subtree_non_trivia_tokens binding_syntax_node
           |> List.find_opt (fun syntax_token ->
                  String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
         with
        | Some syntax_token ->
            token syntax_token
        | None ->
            bail
              ~message:"expected binding-operator equals during Ceibo -> CST lifting"
              ~syntax_node:binding_syntax_node
              ~context:[ "let_operator.binding" ]);
      attributes = [];
      binding_pattern = clause_pattern;
      binding_name =
        simple_pattern_name_token (Cst.Pattern.syntax_node clause_pattern);
      parameters = [];
      value = clause_value;
      and_bindings = [];
      is_recursive = false;
    }

and match_case_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let has_guard =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "when")
  in
  match non_trivia_children with
  | pattern_node :: rest -> (
      let expression_children =
        rest
        |> List.filter can_lift_expression_node
        |> List.map expression_from_node
      in
      match expression_children, has_guard with
      | [], _ -> None
      | body_exprs, false -> (
          match List.rev body_exprs with
          | body_expr :: _ ->
              Some
                {
                  syntax_node = node;
                  bar_token = direct_token_with_text node "|";
                  when_token = None;
                  arrow_token =
                    direct_required_token_with_text ~context:[ "match_case" ]
                      node "->";
                  pattern = pattern_from_node pattern_node;
                  guard = None;
                  body = body_expr;
                }
          | [] -> None)
      | guard_expr :: body_expr :: _, true ->
          Some
            {
              syntax_node = node;
              bar_token = direct_token_with_text node "|";
              when_token = direct_token_with_text node "when";
              arrow_token =
                direct_required_token_with_text ~context:[ "match_case" ] node
                  "->";
              pattern = pattern_from_node pattern_node;
              guard = Some guard_expr;
              body = body_expr;
            }
      | body_expr :: _, true ->
          Some
            {
              syntax_node = node;
              bar_token = direct_token_with_text node "|";
              when_token = direct_token_with_text node "when";
              arrow_token =
                direct_required_token_with_text ~context:[ "match_case" ] node
                  "->";
              pattern = pattern_from_node pattern_node;
              guard = None;
              body = body_expr;
            })
  | [] -> None

and match_expression_from_node node =
  let non_attribute_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match non_attribute_children with
  | scrutinee_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        {
          syntax_node = node;
          keyword_token =
            direct_required_token_with_text ~context:[ "match_expression" ] node
              "match";
          with_token =
            direct_required_token_with_text ~context:[ "match_expression" ] node
              "with";
          scrutinee = expression_from_node scrutinee_node;
          cases = match_cases;
          attributes = [];
        }
  | [] -> None

and try_expression_from_node node =
  let non_attribute_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match non_attribute_children with
  | body_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        {
          syntax_node = node;
          keyword_token =
            direct_required_token_with_text ~context:[ "try_expression" ] node
              "try";
          with_token =
            direct_required_token_with_text ~context:[ "try_expression" ] node
              "with";
          body = expression_from_node body_node;
          cases = match_cases;
          attributes = [];
        }
  | [] -> None

and type_variable_from_node node =
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ ->
      Some Cst.TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

and type_parameter_from_node node =
  let lifted_type_variable =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  let parameter_variance =
    direct_non_trivia_tokens node
    |> List.find_map (fun syntax_token ->
           match Ceibo.Red.SyntaxToken.text syntax_token with
           | "+" ->
               Some
                 (Cst.TypeParameterVariance.Covariant
                    { marker_token = token syntax_token })
           | "-" ->
               Some
                 (Cst.TypeParameterVariance.Contravariant
                    { marker_token = token syntax_token })
           | _ -> None)
  in
  let parameter_is_injective =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "!")
  in
  Cst.TypeParameter.
    {
      syntax_node = node;
      variance = parameter_variance;
      is_injective = parameter_is_injective;
      type_variable = lifted_type_variable;
    }

and type_parameters_from_node node =
  direct_non_trivia_nodes node
  |> List.filter (fun child ->
         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
  |> List.map type_parameter_from_node

let record_field_name_token node =
  match direct_non_trivia_tokens node with
  | mutable_kw :: field_name :: _
    when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" ->
      Some (token field_name)
  | field_name :: _ -> Some (token field_name)
  | [] -> None

let type_constraint_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter can_lift_core_type_node
  with
  | left_node :: right_node :: _ ->
      Some
        Cst.TypeConstraint.
          {
            syntax_node = node;
            left = core_type_from_node left_node;
            right = core_type_from_node right_node;
          }
  | _ ->
      None

let private_flag_from_type_declaration_node node =
  direct_non_trivia_tokens node
  |> List.find_opt (fun syntax_token ->
         String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "private")
  |> function
  | Some private_token ->
      Cst.PrivateFlag.Private { private_token = token private_token }
  | None ->
      Cst.PrivateFlag.Public

let record_field_from_node node =
  record_field_name_token node
  |> Option.map (fun field_name ->
         let mutable_field =
           match direct_non_trivia_tokens node with
           | first :: _ ->
               String.equal (Ceibo.Red.SyntaxToken.text first) "mutable"
           | [] -> false
         in
         let field_type_node =
           direct_non_trivia_nodes node
           |> List.find_opt can_lift_core_type_node
         in
         let lifted_field_type, lifted_attributes =
           match field_type_node with
           | Some field_type_node ->
               let field_type_node, attributes =
                 peel_outer_type_attributes field_type_node
               in
               (core_type_from_node field_type_node, attributes)
           | None ->
               bail
                 ~message:
                   "expected record field type during Ceibo -> CST lifting"
                 ~syntax_node:node ~context:[ "type_definition.record_field" ]
         in
         Cst.RecordField.
           {
             syntax_node = node;
             field_name;
             field_type = lifted_field_type;
             is_mutable = mutable_field;
             attributes = lifted_attributes;
           })

let variant_constructor_from_node node =
  let constructor_payload_signature payload_type =
    let body =
      match payload_type with
      | Cst.CoreType.Poly { body; _ } -> body
      | _ -> payload_type
    in
    let rec collect_parameters acc = function
      | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
          collect_parameters (parameter_type :: acc) result_type
      | result_type ->
          (List.rev acc, result_type)
    in
    collect_parameters [] body
  in
  let direct_children = direct_non_trivia_nodes node in
  let lifted_attributes =
    direct_children
    |> List.filter is_attribute_node
    |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
    |> List.map attribute_from_node
  in
  let constructor_children =
    direct_children
    |> List.filter (fun child ->
           not
             (is_attribute_node child && not (can_lift_core_type_node child)))
  in
  match constructor_children with
  | first_child :: _ -> (
      match direct_non_trivia_tokens first_child with
      | constructor_name :: _ ->
          let is_gadt_constructor =
            direct_non_trivia_tokens node
            |> List.exists (fun syntax_token ->
                   String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":")
          in
          let lifted_payload_type =
            constructor_children
            |> List.find_opt (fun child ->
                   let kind = Ceibo.Red.SyntaxNode.kind child in
                   can_lift_core_type_node child
                   && kind != Syntax_kind.IDENT_EXPR)
            |> Option.map core_type_from_node
          in
          let lifted_arguments =
            if
              direct_non_trivia_tokens node
              |> List.exists (fun syntax_token ->
                     String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "of")
            then
              let type_nodes =
                constructor_children
                |> List.filter can_lift_core_type_node
              in
              match type_nodes with
              | [] ->
                  None
              | [ record_node ]
                when Ceibo.Red.SyntaxNode.kind record_node = Syntax_kind.TYPE_RECORD ->
                  Some
                    (Cst.ConstructorArguments.Record
                       (direct_non_trivia_nodes record_node
                       |> List.filter (fun child ->
                              Ceibo.Red.SyntaxNode.kind child
                              = Syntax_kind.TYPE_RECORD_FIELD)
                       |> List.filter_map record_field_from_node))
              | [ tuple_node ]
                when Ceibo.Red.SyntaxNode.kind tuple_node = Syntax_kind.TYPE_TUPLE ->
                  Some
                    (Cst.ConstructorArguments.Tuple
                       (direct_non_trivia_nodes tuple_node
                       |> List.filter can_lift_core_type_node
                       |> List.map core_type_from_node))
              | _ ->
                  Some
                    (Cst.ConstructorArguments.Tuple
                       (List.map core_type_from_node type_nodes))
            else if is_gadt_constructor then
              (match lifted_payload_type with
              | Some payload_type ->
                  let parameter_types, _result_type =
                    constructor_payload_signature payload_type
                  in
                  if List.length parameter_types = 0 then
                    None
                  else
                    Some (Cst.ConstructorArguments.Tuple parameter_types)
              | None ->
                  None)
            else
              None
          in
          let lifted_result_type =
            if is_gadt_constructor then
              lifted_payload_type
              |> Option.map (fun payload_type ->
                     let _parameter_types, result_type =
                       constructor_payload_signature payload_type
                     in
                     result_type)
            else
              None
          in
          Some
            Cst.VariantConstructor.
              {
                syntax_node = node;
                attributes = lifted_attributes;
                constructor_name = token constructor_name;
                arguments = lifted_arguments;
                payload_type = lifted_payload_type;
                result_type = lifted_result_type;
              }
      | [] -> None)
  | [] -> None

let poly_variant_tag_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let lifted_attributes =
    direct_children
    |> List.filter is_attribute_node
    |> List.filter (fun attribute_node -> not (can_lift_core_type_node attribute_node))
    |> List.map attribute_from_node
  in
  match direct_non_trivia_tokens node with
  | _backtick :: tag_name :: _ ->
      Cst.PolyVariantTag.
        {
          syntax_node = node;
          attributes = lifted_attributes;
          tag_name = token tag_name;
          payload_type =
            (direct_children
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
  | tag_name :: _ ->
      Cst.PolyVariantTag.
        {
          syntax_node = node;
          attributes = lifted_attributes;
          tag_name = token tag_name;
          payload_type =
            (direct_children
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
  | [] ->
      bail ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_definition.poly_variant_tag" ]

let poly_variant_bound_from_node node =
  match direct_non_trivia_tokens node with
  | _open_bracket :: marker_token :: _
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "<" ->
      Cst.PolyVariantBound.UpperBound { marker_token = token marker_token }
  | _open_bracket :: marker_token :: _
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ">" ->
      Cst.PolyVariantBound.LowerBound { marker_token = token marker_token }
  | _ ->
      Cst.PolyVariantBound.Exact

let row_field_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.POLY_VARIANT_TAG ->
      Cst.RowField.Tag (poly_variant_tag_from_node node)
  | _ when can_lift_core_type_node node ->
      let inherited_type =
        match Ceibo.Red.SyntaxNode.kind node with
        | Syntax_kind.TYPE_CONSTR ->
            Cst.CoreType.Constr
              {
                syntax_node = node;
                constructor_path = type_constructor_path_from_node node;
                arguments = [];
              }
        | _ ->
            core_type_from_node node
      in
      Cst.RowField.Inherit
        { syntax_node = node; type_ = inherited_type }
  | _ ->
      bail ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_definition.poly_variant.row_field" ]

let poly_variant_from_node node =
  {
    Cst.syntax_node = node;
    kind = poly_variant_bound_from_node node;
    fields =
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child)
      |> List.map row_field_from_node;
  }

let type_declaration_name_path node =
  let is_name_node child =
    let kind = Ceibo.Red.SyntaxNode.kind child in
    kind = Syntax_kind.IDENT_EXPR || kind = Syntax_kind.MODULE_PATH
  in
  direct_non_trivia_nodes node
  |> List.find_opt is_name_node
  |> Option.map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.MODULE_PATH -> module_path_from_node child
         | Syntax_kind.IDENT_EXPR -> ident_path_from_node child
         | _ ->
             bail ~message:"expected type declaration name path during Ceibo -> CST lifting"
               ~syntax_node:child ~context:[ "type_declaration"; "name" ])

let is_type_extension_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_DECL
  &&
  (direct_non_trivia_tokens node
  |> List.exists (fun syntax_token ->
         String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "+"))

let type_definition_from_node node =
  if Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_EXTENSIBLE then
    Cst.TypeDefinition.Extensible { syntax_node = node }
  else
  let direct_children = direct_non_trivia_nodes node in
  let variant_constructors =
    direct_children
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  if List.length variant_constructors > 0 then
    Cst.TypeDefinition.Variant { syntax_node = node; constructors = variant_constructors }
  else
    match
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD)
    with
    | Some record_node ->
        let fields =
          direct_non_trivia_nodes record_node
          |> List.filter (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
          |> List.filter_map record_field_from_node
        in
        Cst.TypeDefinition.Record { syntax_node = record_node; fields }
    | None -> (
        match
          direct_children
          |> List.find_opt (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_POLY_VARIANT)
        with
        | Some poly_variant_node ->
            Cst.TypeDefinition.PolyVariant (poly_variant_from_node poly_variant_node)
        | None ->
            let remaining_nodes =
              direct_children
              |> List.filter (fun child ->
                     let kind = Ceibo.Red.SyntaxNode.kind child in
                     kind != Syntax_kind.TYPE_PARAM
                     && kind != Syntax_kind.IDENT_EXPR
                     && kind != Syntax_kind.MODULE_PATH
                     && not (kind = Syntax_kind.ATTRIBUTE_EXPR && not (can_lift_core_type_node child)))
            in
            match remaining_nodes with
            | [] -> Cst.TypeDefinition.Abstract
            | first :: _ ->
                let kind = Ceibo.Red.SyntaxNode.kind first in
                if kind = Syntax_kind.TYPE_EXTENSIBLE then
                  Cst.TypeDefinition.Extensible { syntax_node = first }
                else if kind = Syntax_kind.OBJECT_TYPE then
                  Cst.TypeDefinition.Object
                    {
                      syntax_node = first;
                      fields =
                        direct_non_trivia_nodes first
                        |> List.filter (fun child ->
                               Ceibo.Red.SyntaxNode.kind child
                               = Syntax_kind.OBJECT_TYPE_FIELD)
                        |> List.map (fun field_node ->
                               match
                                 first_ident_token_in_subtree field_node,
                                 direct_non_trivia_nodes field_node
                                 |> List.find_opt can_lift_core_type_node
                               with
                               | Some field_name, Some field_type_node ->
                                   {
                                     Cst.syntax_node = field_node;
                                     field_name;
                                     field_type = core_type_from_node field_type_node;
                                   }
                               | _ ->
                                   bail
                                     ~message:
                                       "expected object type field name and type during Ceibo -> CST lifting"
                                     ~syntax_node:field_node
                                     ~context:[ "type_definition.object_field" ]);
                    }
                else if kind = Syntax_kind.FIRST_CLASS_MODULE_TYPE then
                  Cst.TypeDefinition.FirstClassModule
                    {
                      syntax_node = first;
                      module_type =
                        module_type_from_first_class_module_type_node first;
                    }
                else if can_lift_core_type_node first then
                  Cst.TypeDefinition.Alias
                    { syntax_node = first; manifest = core_type_from_node first }
                else
                  bail
                    ~message:
                      "unsupported type definition shape during Ceibo -> CST lifting"
                    ~syntax_node:first ~context:[ "type_definition" ])

let type_declaration_from_node node =
  let lifted_type_params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  let lifted_constraints =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
    |> List.filter_map type_constraint_from_node
  in
  let has_destructive_substitution =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":=")
  in
  match type_declaration_name_path node with
  | Some lifted_type_name -> (
      match Cst.Ident.last_segment lifted_type_name with
      | Some _ ->
          Some
            Cst.TypeDeclaration.
              {
                syntax_node = node;
                type_name = lifted_type_name;
                type_params = lifted_type_params;
                type_definition = type_definition_from_node node;
                private_flag = private_flag_from_type_declaration_node node;
                constraints = lifted_constraints;
                and_declarations = [];
                is_destructive_substitution = has_destructive_substitution;
              }
      | None -> None)
  | None -> None

let grouped_type_declaration_from_nodes ~group_syntax_node nodes =
  let decls =
    nodes
    |> List.filter (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL)
    |> List.filter_map type_declaration_from_node
  in
  match decls with
  | [] ->
      None
  | first :: rest ->
      Some { first with syntax_node = group_syntax_node; and_declarations = rest }

let type_extension_from_node node =
  let extension_type_params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  let extension_constructors =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  match type_declaration_name_path node with
  | Some extension_type_name when List.length extension_constructors > 0 -> (
      match Cst.Ident.last_segment extension_type_name with
      | Some _ ->
          Some
            Cst.TypeExtension.
              {
                syntax_node = node;
                type_name = extension_type_name;
                type_params = extension_type_params;
                constructors = extension_constructors;
              }
      | None -> None)
  | _ -> None

let let_binding_from_node_with_keyword ~keyword_token ~is_recursive_binding node =
  let binding_keyword_token = keyword_token in
  let binding_rec_token = direct_token_with_text node "rec" in
  let binding_equals_token =
    match direct_token_with_text node "=" with
    | Some equals_token ->
        equals_token
    | None ->
        bail ~message:"expected let-binding equals during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "let_binding" ]
  in
  let is_recursive_binding = is_recursive_binding || Option.is_some binding_rec_token in
  let direct_children = direct_non_trivia_nodes node in
  let binding_attributes =
    direct_children
    |> List.filter is_attribute_node
    |> List.map attribute_from_node
  in
  let binding_children =
    direct_children |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_param_nodes ->
          let prefix_nodes = List.rev rev_param_nodes in
          Some
            Cst.LetBinding.
                {
                  syntax_node = node;
                  keyword_token = binding_keyword_token;
                  rec_token = binding_rec_token;
                  equals_token = binding_equals_token;
                  attributes = binding_attributes;
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters = binding_parameters_from_prefix prefix_nodes;
                value =
                  binding_value_from_prefix ~binding_syntax_node:node
                    ~prefix_nodes:prefix_nodes ~value_node:value_node;
                and_bindings = [];
                is_recursive = is_recursive_binding;
              }
      | [] -> None)
  | [] -> None

let let_binding_from_node ~is_recursive_binding node =
  let binding_keyword_token =
    match direct_token_with_text node "let" with
    | Some keyword_token ->
        keyword_token
    | None -> (
        match direct_token_with_text node "and" with
        | Some keyword_token ->
            keyword_token
        | None ->
            bail ~message:"expected let-binding keyword during Ceibo -> CST lifting"
              ~syntax_node:node ~context:[ "let_binding" ])
  in
  let_binding_from_node_with_keyword ~keyword_token:binding_keyword_token
    ~is_recursive_binding node

let let_expression_binding_from_node ~is_recursive_binding node =
  if (not is_recursive_binding) && is_binding_operator_expression_node node then
    match let_operator_expression_from_node node with
    | Some expr ->
        Some
          (let_binding_from_binding_operator_binding ~binding_syntax_node:node
             expr.binding)
    | None ->
        None
  else
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          prefix_nodes,
          bound_value_node,
          _and_binding_nodes,
          _body_node )) ->
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                keyword_token =
                  (match direct_token_with_text node "let" with
                  | Some keyword_token ->
                      keyword_token
                  | None ->
                      bail
                        ~message:
                          "expected let-expression binding keyword during Ceibo -> CST lifting"
                        ~syntax_node:node ~context:[ "let_expression.binding" ]);
                rec_token = direct_token_with_text node "rec";
                equals_token =
                  (match direct_token_with_text node "=" with
                  | Some equals_token ->
                      equals_token
                  | None ->
                      bail
                        ~message:
                          "expected let-expression binding equals during Ceibo -> CST lifting"
                        ~syntax_node:node ~context:[ "let_expression.binding" ]);
                attributes = [];
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters = binding_parameters_from_prefix prefix_nodes;
                value =
                  binding_value_from_prefix ~binding_syntax_node:node
                    ~prefix_nodes:prefix_nodes ~value_node:bound_value_node;
                and_bindings = [];
                is_recursive = is_recursive_binding;
              }
  | _ -> None

let module_declaration_from_node node =
  let direct_tokens = direct_non_trivia_tokens node in
  let is_recursive_declaration =
    direct_tokens
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
  in
  match
    find_declaration_name_token ~skip_keywords:[ "module"; "rec"; "and" ]
      direct_tokens
  with
  | Some module_name -> (
      let direct_children = direct_non_trivia_nodes node in
      let destructive_substitution =
        direct_non_trivia_tokens node
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":=")
      in
      let has_equals =
        direct_non_trivia_tokens node
        |> List.exists (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               String.equal text "=" || String.equal text ":=")
      in
      let lifted_module_expression =
        if has_equals then
          direct_children
          |> List.rev
          |> List.find_opt can_lift_module_expression_node
          |> Option.map module_expression_from_node
        else
          None
      in
      let module_type_search_children =
        if has_equals then
          match
            direct_children
            |> List.rev
            |> List.find_opt can_lift_module_expression_node
          with
          | Some module_expression_node ->
              direct_children
              |> List.filter (fun child ->
                     child != module_expression_node)
          | None ->
              direct_children
        else
          direct_children
      in
      let lifted_module_type =
        module_type_search_children
        |> List.find_opt can_lift_module_type_node
        |> Option.map module_type_from_node
      in
      let lifted_module_expression =
        match lifted_module_expression, lifted_module_type with
        | Some module_expression, Some module_type when has_equals ->
            Some
              (constrain_module_expression ~syntax_node:node
                 ~module_expression module_type)
        | _ ->
            lifted_module_expression
      in
      Some
        Cst.ModuleDeclaration.
          {
            syntax_node = node;
            module_name = token module_name;
            functor_parameters =
              direct_children
              |> List.filter (fun child ->
                     Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
              |> List.map functor_parameter_from_node;
            module_type = lifted_module_type;
            module_expression = lifted_module_expression;
            is_destructive_substitution = destructive_substitution;
            is_recursive = is_recursive_declaration;
          })
  | None -> None

let recursive_module_declaration_from_nodes ~group_syntax_node module_decl_nodes =
  let recursive_declarations =
    module_decl_nodes
    |> List.map (fun module_decl_node ->
           match module_declaration_from_node module_decl_node with
           | Some decl ->
               ({ decl with is_recursive = true } : Cst.ModuleDeclaration.t)
           | None ->
               bail
                 ~message:
                   "expected recursive module binding during Ceibo -> CST lifting"
                 ~syntax_node:module_decl_node
                 ~context:[ "item"; "recursive_module_declaration" ])
  in
  match recursive_declarations with
  | [] -> None
  | _ ->
      Some
        Cst.RecursiveModuleDeclaration.
          {
            syntax_node = group_syntax_node;
            declarations = recursive_declarations;
          }

let module_type_declaration_from_node node =
  match
    find_declaration_name_token ~skip_keywords:[ "module"; "type" ]
      (direct_non_trivia_tokens node)
  with
  | Some module_type_name ->
      let destructive_substitution =
        direct_non_trivia_tokens node
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":=")
      in
      Some
        Cst.ModuleTypeDeclaration.
          {
            syntax_node = node;
            module_type_name = token module_type_name;
            module_type =
              (direct_non_trivia_nodes node
              |> List.rev
              |> List.find_opt can_lift_module_type_node
              |> Option.map module_type_from_node);
            is_destructive_substitution = destructive_substitution;
          }
  | None -> None

let class_declaration_from_node node =
  let has_equals_body =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
  in
  let has_colon_body =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":")
  in
  let class_type_and_body_from_child child =
    match Ceibo.Red.SyntaxNode.kind child with
    | Syntax_kind.INFIX_EXPR -> (
        match direct_non_trivia_nodes child with
        | class_type_node :: class_body_node :: _
          when can_lift_class_type_node class_type_node
               && can_lift_class_expression_node class_body_node ->
            ( Some (class_type_from_node class_type_node),
              Some (class_expression_from_node class_body_node) )
        | _ ->
            (None, None))
    | _ ->
        ( (if has_colon_body && can_lift_class_type_node child then
             Some (class_type_from_node child)
           else
             None),
          if has_equals_body && can_lift_class_expression_node child then
            Some (class_expression_from_node child)
          else
            None )
  in
  let children =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM)
  in
  let rec split_at_name acc = function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR ->
        Some (child, List.rev acc, rest)
    | child :: rest ->
        split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, _prefix, remainder) -> (
      match first_ident_token_in_subtree name_node, List.rev remainder with
      | Some class_name, class_body_node :: rev_prefix ->
          let suffix_class_type, suffix_class_body =
            class_type_and_body_from_child class_body_node
          in
          let prefix_class_type =
            if Option.is_some suffix_class_type then
              None
            else
              match List.rev rev_prefix with
              | class_type_node :: _ when can_lift_class_type_node class_type_node ->
                  Some (class_type_from_node class_type_node)
              | _ -> None
          in
          Some
            {
              Cst.syntax_node = node;
              type_params = type_parameters_from_node node;
              class_name;
              class_type =
                (match suffix_class_type with
                | Some _ -> suffix_class_type
                | None -> prefix_class_type);
              class_body = suffix_class_body;
            }
      | _ -> None)
  | None -> None

let class_type_declaration_from_node node =
  let children =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM)
  in
  let rec split_at_name acc = function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR ->
        Some (child, List.rev acc, rest)
    | child :: rest ->
        split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, _prefix, body_node :: _) -> (
      match first_ident_token_in_subtree name_node with
      | Some class_type_name ->
          if can_lift_class_type_node body_node then
            Some
              {
                Cst.syntax_node = node;
                type_params = type_parameters_from_node node;
                class_type_name;
                class_type_body = class_type_from_node body_node;
              }
          else
            None
      | None -> None)
  | _ -> None

let open_statement_from_node node =
  let tokens = direct_non_trivia_tokens node in
  let bang_token_opt =
    tokens
    |> List.find_opt (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "!")
    |> Option.map token
  in
  let lifted_target =
    direct_non_trivia_nodes node
    |> List.find_opt can_lift_module_expression_node
    |> Option.map (fun target_node ->
           Cst.OpenStatement.ModuleExpression
             (module_expression_from_node target_node))
  in
  match lifted_target with
  | Some lifted_target ->
      Some
        Cst.OpenStatement.
          {
            syntax_node = node;
            target = lifted_target;
            bang_token = bang_token_opt;
          }
  | None ->
      let module_tokens =
        tokens
        |> List.filter (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               not
                 (String.equal text "open"
                 || String.equal text "!"))
      in
      (match module_tokens with
      | [] -> None
      | _ ->
          Some
            Cst.OpenStatement.
              {
                syntax_node = node;
                target =
                  Cst.OpenStatement.Path
                    (module_path_from_tokens ~syntax_node:node module_tokens);
                bang_token = bang_token_opt;
              })

let declaration_name_token_from_node node =
  let operator_token_from_node node =
    direct_non_trivia_tokens node
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not (String.equal text "(" || String.equal text ")"))
    |> Option.map token
  in
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.IDENT_EXPR ->
             direct_non_trivia_tokens child |> List.find_opt (fun _ -> true) |> Option.map token
         | Syntax_kind.OPERATOR_PATTERN -> operator_token_from_node child
         | _ -> None)

let value_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  match declaration_name_token_from_node node with
  | Some lifted_name_token -> (
      match
        List.rev direct_children
        |> List.find_opt can_lift_core_type_node
      with
      | Some lifted_type_node ->
          Some
            ({ syntax_node = node; name_token = lifted_name_token; type_ = core_type_from_node lifted_type_node }
              : Cst.value_declaration)
      | None -> None)
  | None -> None

let external_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let lifted_primitive_name_tokens =
    direct_non_trivia_tokens node
    |> List.filter (fun syntax_token ->
           Ceibo.Red.SyntaxToken.kind syntax_token = Syntax_kind.STRING_LITERAL)
    |> List.map token
  in
  let external_name_token =
    find_declaration_name_token ~skip_keywords:[ "external" ]
      (direct_non_trivia_tokens node)
    |> Option.map token
  in
  match external_name_token with
  | Some lifted_name_token -> (
      match
        direct_children
        |> List.find_opt can_lift_core_type_node
      with
      | Some lifted_type_node ->
          Some
            ({
               syntax_node = node;
               name_token = lifted_name_token;
               type_ = core_type_from_node lifted_type_node;
               primitive_name_tokens = lifted_primitive_name_tokens;
               attributes = attributes_from_node node;
             }
              : Cst.external_declaration)
      | None -> None)
  | None -> None

let include_statement_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           can_lift_module_expression_node child || can_lift_module_type_node child)
  with
  | Some included_node ->
      Some
        ({
           syntax_node = node;
           target =
             if can_lift_module_expression_node included_node then
               Cst.ModuleExpression (module_expression_from_node included_node)
             else if can_lift_module_type_node included_node then
               Cst.ModuleType (module_type_from_node included_node)
             else
               bail ~message:"expected include target during Ceibo -> CST lifting"
                 ~syntax_node:included_node ~context:[ "include_statement" ];
         }
          : Cst.include_statement)
  | None -> None

let exception_declaration_from_node node =
  match
    find_declaration_name_token ~skip_keywords:[ "exception" ]
      (direct_non_trivia_tokens node)
  with
  | Some name_syntax_token ->
      Some
        ({ syntax_node = node; name_token = token name_syntax_token }
          : Cst.exception_declaration)
  | None -> None

let rec structure_items_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      if is_type_extension_node node then
        match type_extension_from_node node with
        | Some decl -> [ Cst.StructureItem.TypeExtension decl ]
        | None -> unsupported_item node
      else
        match type_declaration_from_node node with
        | Some decl -> [ Cst.StructureItem.TypeDeclaration decl ]
        | None -> unsupported_item node)
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      let child_nodes = direct_non_trivia_nodes node in
      if
        child_nodes != []
        && List.for_all
             (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MODULE_DECL)
             child_nodes
      then
        match
          recursive_module_declaration_from_nodes ~group_syntax_node:node child_nodes
        with
        | Some decl -> [ Cst.StructureItem.RecursiveModuleDeclaration decl ]
        | None -> unsupported_item node
      else if
        child_nodes != []
        && List.for_all
             (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL)
             child_nodes
      then
        match grouped_type_declaration_from_nodes ~group_syntax_node:node child_nodes with
        | Some decl -> [ Cst.StructureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
      else
        child_nodes |> List.concat_map structure_items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Cst.StructureItem.LetBinding binding ]
      | None -> unsupported_item node)
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Cst.StructureItem.LetBinding binding ]
      | None -> unsupported_item node)
  | Syntax_kind.LET_MUTUAL_DECL ->
      let binding_nodes =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               let kind = Ceibo.Red.SyntaxNode.kind child in
               kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      in
      (match binding_nodes with
      | first_node :: rest_nodes ->
          let first_binding_tokens = direct_non_trivia_tokens first_node in
          let is_recursive_group =
            first_binding_tokens
            |> List.exists (fun token ->
                   String.equal (Ceibo.Red.SyntaxToken.text token) "rec")
          in
          let let_keyword_token =
            match List.find_opt (fun token -> String.equal (Ceibo.Red.SyntaxToken.text token) "let") first_binding_tokens with
            | Some token ->
                token
            | None ->
                bail
                  ~message:"expected let keyword for let-mutual declaration during Ceibo -> CST lifting"
                  ~syntax_node:first_node ~context:[ "item"; "let_mutual_declaration" ]
          in
          let group_tokens = direct_non_trivia_tokens node in
          let and_keyword_tokens =
            group_tokens
            |> List.filter (fun token -> String.equal (Ceibo.Red.SyntaxToken.text token) "and")
          in
          (match
             let_binding_from_node_with_keyword
               ~keyword_token:(token let_keyword_token)
               ~is_recursive_binding:is_recursive_group first_node
           with
          | Some first_binding ->
              let and_bindings =
                rest_nodes
                |> List.mapi (fun index binding_node ->
                       match List.nth_opt and_keyword_tokens index with
                       | Some and_keyword_token ->
                           let_binding_from_node_with_keyword
                             ~keyword_token:(token and_keyword_token)
                             ~is_recursive_binding:is_recursive_group binding_node
                       | None ->
                           bail
                             ~message:
                               "expected matching and keyword for let-mutual declaration during Ceibo -> CST lifting"
                             ~syntax_node:binding_node
                             ~context:[ "item"; "let_mutual_declaration"; "and_bindings" ])
                |> List.filter_map (fun value -> value)
              in
              let grouped_binding =
                {
                  first_binding with
                  syntax_node = node;
                  and_bindings;
                }
              in
              [ Cst.StructureItem.LetBinding grouped_binding ]
          | None ->
              unsupported_item node)
      | [] ->
          unsupported_item node)
  | Syntax_kind.CLASS_DECL -> (
      match class_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ClassDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.CLASS_TYPE_DECL -> (
      match class_type_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ClassTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl when Cst.ModuleDeclaration.is_recursive decl -> (
          match
            recursive_module_declaration_from_nodes ~group_syntax_node:node [ node ]
          with
          | Some recursive_decl ->
              [ Cst.StructureItem.RecursiveModuleDeclaration recursive_decl ]
          | None ->
              unsupported_item node)
      | Some decl -> [ Cst.StructureItem.ModuleDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ModuleTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.StructureItem.OpenStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ValueDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ExternalDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.StructureItem.IncludeStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.StructureItem.ExceptionDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.ATTRIBUTE_EXPR ->
      [ Cst.StructureItem.Attribute (attribute_from_node node) ]
  | Syntax_kind.APPLY_EXPR when List.for_all is_attribute_node (direct_non_trivia_nodes node) ->
      direct_non_trivia_nodes node
      |> List.map (fun child ->
             Cst.StructureItem.Attribute (attribute_from_node child))
  | Syntax_kind.EXTENSION_EXPR ->
      [ Cst.StructureItem.Extension (extension_from_node node) ]
  | Syntax_kind.SEQUENCE_EXPR -> (
      [ Cst.StructureItem.Expression (expression_from_node node) ])
  | _ -> (
      [ Cst.StructureItem.Expression (expression_from_node node) ])

let rec signature_items_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      if is_type_extension_node node then
        match type_extension_from_node node with
        | Some decl -> [ Cst.SignatureItem.TypeExtension decl ]
        | None -> unsupported_item node
      else
        match type_declaration_from_node node with
        | Some decl -> [ Cst.SignatureItem.TypeDeclaration decl ]
        | None -> unsupported_item node)
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      let child_nodes = direct_non_trivia_nodes node in
      if
        child_nodes != []
        && List.for_all
             (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MODULE_DECL)
             child_nodes
      then
        match
          recursive_module_declaration_from_nodes ~group_syntax_node:node child_nodes
        with
        | Some decl -> [ Cst.SignatureItem.RecursiveModuleDeclaration decl ]
        | None -> unsupported_item node
      else if
        child_nodes != []
        && List.for_all
             (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_DECL)
             child_nodes
      then
        match grouped_type_declaration_from_nodes ~group_syntax_node:node child_nodes with
        | Some decl -> [ Cst.SignatureItem.TypeDeclaration decl ]
        | None -> unsupported_item node
      else
        child_nodes |> List.concat_map signature_items_from_node
  | Syntax_kind.CLASS_DECL -> (
      match class_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ClassDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.CLASS_TYPE_DECL -> (
      match class_type_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ClassTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl when Cst.ModuleDeclaration.is_recursive decl -> (
          match
            recursive_module_declaration_from_nodes ~group_syntax_node:node [ node ]
          with
          | Some recursive_decl ->
              [ Cst.SignatureItem.RecursiveModuleDeclaration recursive_decl ]
          | None ->
              unsupported_item node)
      | Some decl -> [ Cst.SignatureItem.ModuleDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ModuleTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.SignatureItem.OpenStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ValueDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ExternalDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.SignatureItem.IncludeStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.SignatureItem.ExceptionDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.ATTRIBUTE_EXPR ->
      [ Cst.SignatureItem.Attribute (attribute_from_node node) ]
  | Syntax_kind.APPLY_EXPR when List.for_all is_attribute_node (direct_non_trivia_nodes node) ->
      direct_non_trivia_nodes node
      |> List.map (fun child ->
             Cst.SignatureItem.Attribute (attribute_from_node child))
  | Syntax_kind.EXTENSION_EXPR ->
      [ Cst.SignatureItem.Extension (extension_from_node node) ]
  | _ ->
      unsupported_item node

let raw_annotation_payload_from_shell shell_node =
  let all_tokens = direct_tokens shell_node in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let shell_close_offset =
    match List.rev all_tokens with
    | close_token :: _ ->
        (Ceibo.Red.SyntaxToken.span close_token).start
    | [] ->
        Ceibo.Red.SyntaxNode.span shell_node |> fun span -> span.end_
  in
  let payload_non_trivia_tokens =
    let rec skip_name = function
      | dot_token :: _name_token :: rest
        when String.equal (Ceibo.Red.SyntaxToken.text dot_token) "." ->
          skip_name rest
      | rest ->
          rest
    in
    match shell_tokens with
    | _open_token :: _sigil_token :: _name_token :: rest -> (
        let rest = skip_name rest in
        match List.rev rest with
        | close_token :: payload_rev
          when String.equal (Ceibo.Red.SyntaxToken.text close_token) "]"
               || String.equal (Ceibo.Red.SyntaxToken.text close_token) "}" ->
            List.rev payload_rev
        | _ ->
            rest)
    | _ ->
        []
  in
  match payload_non_trivia_tokens with
  | [] ->
      None
  | marker_token :: rest
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ":" -> (
      match rest with
      | first_content_token :: _ ->
          Some
            {
              kind = TypePayload;
              text =
                payload_text_from_tokens all_tokens
                  ~start_offset:(Ceibo.Red.SyntaxToken.span first_content_token).start
                  ~end_offset:shell_close_offset;
              start_offset =
                (Ceibo.Red.SyntaxToken.span first_content_token).start;
            }
      | [] ->
          None)
  | marker_token :: rest
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "?" -> (
      match rest with
      | first_content_token :: _ ->
          Some
            {
              kind = PatternPayload;
              text =
                payload_text_from_tokens all_tokens
                  ~start_offset:(Ceibo.Red.SyntaxToken.span first_content_token).start
                  ~end_offset:shell_close_offset;
              start_offset =
                (Ceibo.Red.SyntaxToken.span first_content_token).start;
            }
      | [] ->
          None)
  | first_payload_token :: _ ->
      Some
        {
          kind = Unmarked;
          text =
            payload_text_from_tokens all_tokens
              ~start_offset:(Ceibo.Red.SyntaxToken.span first_payload_token).start
              ~end_offset:shell_close_offset;
          start_offset = (Ceibo.Red.SyntaxToken.span first_payload_token).start;
        }

let structure_payload_item_syntax_nodes_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ decl.syntax_node ]
      | None -> unsupported_item node)
  | _ ->
      structure_items_from_node node |> List.map Cst.StructureItem.syntax_node

let signature_payload_item_syntax_nodes_from_node node =
  signature_items_from_node node |> List.map Cst.SignatureItem.syntax_node

let item_syntax_nodes_from_parse_result ~kind result =
  if List.length result.Parser.diagnostics > 0 then
    None
  else
    let root = Ceibo.Red.new_root result.tree in
    try
      Some
        (match kind with
        | `Implementation ->
            direct_non_trivia_nodes root
            |> List.concat_map structure_payload_item_syntax_nodes_from_node
        | `Interface ->
            direct_non_trivia_nodes root
            |> List.concat_map signature_payload_item_syntax_nodes_from_node)
    with
    | Bail _ ->
        None

let annotation_payload_from_shell_lift shell_node =
  let structure_payload raw_payload =
    let source =
      make_padded_fragment ~start_offset:raw_payload.start_offset raw_payload.text
    in
    parse_implementation_fragment source
    |> item_syntax_nodes_from_parse_result ~kind:`Implementation
    |> Option.map (fun item_syntax_nodes ->
           Cst.Payload.Structure { item_syntax_nodes })
  in
  let signature_payload raw_payload =
    let source =
      make_padded_fragment ~start_offset:raw_payload.start_offset raw_payload.text
    in
    parse_interface_fragment source
    |> item_syntax_nodes_from_parse_result ~kind:`Interface
    |> Option.map (fun item_syntax_nodes ->
           Cst.Payload.Signature { item_syntax_nodes })
  in
  let type_payload raw_payload =
    let source =
      make_wrapped_fragment ~prefix:"type t = " ~suffix:"\n"
        ~start_offset:raw_payload.start_offset raw_payload.text
    in
    match parse_implementation_fragment source with
    | { diagnostics = []; tree; _ } ->
        let root = Ceibo.Red.new_root tree in
        (match direct_non_trivia_nodes root with
        | type_decl_node :: _ -> (
            match type_declaration_from_node type_decl_node with
            | Some
                {
                  type_definition =
                    Cst.TypeDefinition.Alias { manifest; _ };
                  _;
                } ->
                Some (Cst.Payload.Type manifest)
            | _ ->
                None)
        | [] ->
            None)
    | _ ->
        None
  in
  let pattern_payload raw_payload =
    let source =
      make_wrapped_fragment ~prefix:"let p = function | " ~suffix:" -> ()\n"
        ~start_offset:raw_payload.start_offset raw_payload.text
    in
    match parse_implementation_fragment source with
    | { diagnostics = []; tree; _ } ->
        let root = Ceibo.Red.new_root tree in
        (match direct_non_trivia_nodes root with
        | let_binding_node :: _ -> (
            match let_binding_from_node ~is_recursive_binding:false let_binding_node with
            | Some
                {
                  value = Cst.Expression.Function { cases = case :: _; _ };
                  _;
                } ->
                Some
                  (Cst.Payload.Pattern
                     {
                       pattern_syntax_node = Cst.Pattern.syntax_node case.pattern;
                       guard_syntax_node =
                         Option.map Cst.Expression.syntax_node case.guard;
                     })
            | _ ->
                None)
        | [] ->
            None)
    | _ ->
        None
  in
  match raw_annotation_payload_from_shell shell_node with
  | Some ({ kind = TypePayload; _ } as raw_payload) ->
      type_payload raw_payload
  | Some ({ kind = PatternPayload; _ } as raw_payload) ->
      pattern_payload raw_payload
  | Some ({ kind = Unmarked; _ } as raw_payload) -> (
      match structure_payload raw_payload with
      | Some payload ->
          Some payload
      | None ->
          signature_payload raw_payload)
  | None ->
      None

let () =
  Cell.set annotation_payload_from_shell_impl annotation_payload_from_shell_lift

let build_source_file_body tree items_from_node =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  (root, file_items)

let rec validate_pattern ~context = function
  | Cst.Pattern.Identifier _ | Cst.Pattern.Wildcard _ | Cst.Pattern.Literal _
  | Cst.Pattern.Extension _ ->
      ()
  | Cst.Pattern.Lazy { pattern; _ } ->
      validate_pattern ~context:("pattern.lazy" :: context) pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      validate_pattern ~context:("pattern.exception" :: context) pattern
  | Cst.Pattern.Range _ | Cst.Pattern.Operator _
  | Cst.Pattern.PolyVariantInherit _ ->
      ()
  | Cst.Pattern.FirstClassModule { module_type; _ } ->
      Option.iter
        (validate_module_type ~context:("pattern.first_class_module.type" :: context))
        module_type
  | Cst.Pattern.PolyVariant { payload; _ } ->
      Option.iter
        (validate_pattern ~context:("pattern.poly_variant.payload" :: context))
        payload
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.iteri
        (fun index argument ->
          validate_pattern
            ~context:
              (("pattern.constructor.argument[" ^ Int.to_string index ^ "]")
             :: context)
            argument)
        arguments
  | Cst.Pattern.Tuple { elements; _ } ->
      List.iteri
        (fun index ({ Cst.pattern; _ } : Cst.tuple_pattern_element) ->
          validate_pattern
            ~context:(("pattern.tuple.element[" ^ Int.to_string index ^ "]") :: context)
            pattern)
        elements
  | Cst.Pattern.List { elements; _ }
  | Cst.Pattern.Array { elements; _ }
  | Cst.Pattern.Or { alternatives = elements; _ } ->
      List.iteri
        (fun index pattern ->
          validate_pattern
            ~context:(("pattern.element[" ^ Int.to_string index ^ "]") :: context)
            pattern)
        elements
  | Cst.Pattern.Record { fields; _ } ->
      List.iteri
        (fun index (field : Cst.record_pattern_field) ->
          Option.iter
            (validate_pattern
               ~context:
                 (("pattern.record.field[" ^ Int.to_string index ^ "].pattern")
                 :: context))
            field.pattern)
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      validate_pattern ~context:("pattern.cons.head" :: context) head;
      validate_pattern ~context:("pattern.cons.tail" :: context) tail
  | Cst.Pattern.Alias { pattern; _ } ->
      validate_pattern ~context:("pattern.alias.pattern" :: context) pattern
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      validate_pattern ~context:("pattern.typed.pattern" :: context) pattern;
      validate_core_type ~context:("pattern.typed.type" :: context) type_
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      validate_pattern ~context:("pattern.effect.effect" :: context)
        effect_pattern;
      validate_pattern ~context:("pattern.effect.continuation" :: context)
        continuation
  | Cst.Pattern.LocalOpen { pattern; _ } ->
      validate_pattern ~context:("pattern.local_open.pattern" :: context) pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      validate_pattern ~context:("pattern.parenthesized" :: context) inner

and validate_parameter ~context = function
  | Cst.Parameter.Positional _ | Cst.Parameter.Labeled _
  | Cst.Parameter.Optional _ | Cst.Parameter.LocallyAbstract _ ->
      ()

and validate_module_type ~context = function
  | Cst.ModuleType.Path _ | Cst.ModuleType.TypeOf _ | Cst.ModuleType.Signature _
  | Cst.ModuleType.Extension _ ->
      ()
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      validate_module_type ~context:("module_type.parenthesized" :: context) inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      validate_module_type ~context:("module_type.attribute" :: context) module_type
  | Cst.ModuleType.With { base; constraints; _ } ->
      validate_module_type ~context:("module_type.with.base" :: context) base;
      List.iteri
        (fun index
             ({ constrained_type; replacement_type; _ } :
               Cst.module_type_constraint) ->
          validate_core_type
            ~context:
              (("module_type.with.constraint[" ^ Int.to_string index ^ "].target")
              :: context)
            constrained_type;
          validate_core_type
            ~context:
              (("module_type.with.constraint[" ^ Int.to_string index
              ^ "].replacement")
              :: context)
            replacement_type)
        constraints
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      List.iteri
        (fun index ({ module_type; _ } : Cst.functor_parameter) ->
          validate_module_type
            ~context:
              (("module_type.functor.parameter[" ^ Int.to_string index ^ "]")
              :: context)
            module_type)
        parameters;
      validate_module_type ~context:("module_type.functor.result" :: context) result

and validate_core_type ~context = function
  | Cst.CoreType.Wildcard _
  | Cst.CoreType.Var _
  | Cst.CoreType.Extension _ ->
      ()
  | Cst.CoreType.Poly { body; _ } ->
      validate_core_type ~context:("core_type.poly.body" :: context) body
  | Cst.CoreType.FirstClassModule { module_type; _ } ->
      validate_module_type ~context:("core_type.first_class_module" :: context)
        module_type
  | Cst.CoreType.Constr { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.constr.arg[" ^ Int.to_string index ^ "]") :: context)
            type_)
        arguments
  | Cst.CoreType.Class { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.class.arg[" ^ Int.to_string index ^ "]") :: context)
            type_)
        arguments
  | Cst.CoreType.Alias { type_; _ } ->
      validate_core_type ~context:("core_type.alias.type" :: context) type_
  | Cst.CoreType.Attribute { type_; _ } ->
      validate_core_type ~context:("core_type.attribute.type" :: context) type_
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      validate_core_type
        ~context:("core_type.arrow.parameter" :: context) parameter_type;
      validate_core_type ~context:("core_type.arrow.result" :: context) result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.tuple.element[" ^ Int.to_string index ^ "]") :: context)
            type_)
        elements
  | Cst.CoreType.Parenthesized { inner; _ } ->
      validate_core_type ~context:("core_type.parenthesized" :: context) inner
  | Cst.CoreType.LocalOpen { type_; _ } ->
      validate_core_type ~context:("core_type.local_open.type" :: context) type_
  | Cst.CoreType.PolyVariant poly_variant ->
      validate_poly_variant ~context:("core_type.poly_variant" :: context)
        poly_variant
  | Cst.CoreType.Record { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.record_type_field) ->
          validate_core_type
            ~context:
              (("core_type.record.field[" ^ Int.to_string index ^ "].type") :: context)
            field_type)
        fields
  | Cst.CoreType.Object { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.object_type_field) ->
          validate_core_type
            ~context:
              (("core_type.object.field[" ^ Int.to_string index ^ "].type") :: context)
            field_type)
        fields

and validate_row_field ~context index = function
  | Cst.RowField.Tag tag ->
      Option.iter
        (validate_core_type
           ~context:
             (("row_field[" ^ Int.to_string index ^ "].tag.payload") :: context))
        (Cst.PolyVariantTag.payload_type tag)
  | Cst.RowField.Inherit { type_; _ } ->
      validate_core_type
        ~context:(("row_field[" ^ Int.to_string index ^ "].inherit") :: context)
        type_

and validate_poly_variant ~context poly_variant =
  Cst.PolyVariant.fields poly_variant
  |> List.iteri (validate_row_field ~context)

and validate_class_type_field ~context = function
  | Cst.ClassTypeField.Inherit { class_type; _ } ->
      validate_class_type
        ~context:("class_type_field.inherit" :: context)
        class_type
  | Cst.ClassTypeField.Value { type_; _ } ->
      validate_core_type ~context:("class_type_field.value" :: context) type_
  | Cst.ClassTypeField.Method { type_; _ } ->
      validate_core_type ~context:("class_type_field.method" :: context) type_
  | Cst.ClassTypeField.Constraint { left; right; _ } ->
      validate_core_type ~context:("class_type_field.constraint.left" :: context)
        left;
      validate_core_type ~context:("class_type_field.constraint.right" :: context)
        right
  | Cst.ClassTypeField.Attribute { field; _ } ->
      validate_class_type_field
        ~context:("class_type_field.attribute" :: context)
        field
  | Cst.ClassTypeField.Extension _ ->
      ()

and validate_class_type ~context = function
  | Cst.ClassType.Path _ | Cst.ClassType.Extension _ ->
      ()
  | Cst.ClassType.Signature { fields; _ } ->
      List.iteri
        (fun index field ->
          validate_class_type_field
            ~context:
              (("class_type.signature.field[" ^ Int.to_string index ^ "]")
              :: context)
            field)
        fields
  | Cst.ClassType.Arrow { parameter_type; result_type; _ } ->
      validate_core_type
        ~context:("class_type.arrow.parameter" :: context)
        parameter_type;
      validate_class_type
        ~context:("class_type.arrow.result" :: context)
        result_type
  | Cst.ClassType.Parenthesized { inner; _ } ->
      validate_class_type ~context:("class_type.parenthesized" :: context) inner
  | Cst.ClassType.LocalOpen { class_type; _ } ->
      validate_class_type ~context:("class_type.local_open" :: context) class_type
  | Cst.ClassType.Attribute { class_type; _ } ->
      validate_class_type ~context:("class_type.attribute" :: context) class_type

and validate_class_field ~context = function
  | Cst.ClassField.Method { body; type_; _ } ->
      Option.iter
        (validate_expression ~context:("class_field.method.body" :: context))
        body;
      Option.iter
        (validate_core_type ~context:("class_field.method.type" :: context))
        type_
  | Cst.ClassField.Value { value; type_; _ } ->
      Option.iter
        (validate_expression ~context:("class_field.value.value" :: context))
        value;
      Option.iter
        (validate_core_type ~context:("class_field.value.type" :: context))
        type_
  | Cst.ClassField.Inherit { class_expression; _ } ->
      validate_class_expression
        ~context:("class_field.inherit.class_expression" :: context)
        class_expression
  | Cst.ClassField.Constraint { left; right; _ } ->
      validate_core_type ~context:("class_field.constraint.left" :: context) left;
      validate_core_type ~context:("class_field.constraint.right" :: context)
        right
  | Cst.ClassField.Initializer { body; _ } ->
      Option.iter
        (validate_expression ~context:("class_field.initializer.body" :: context))
        body
  | Cst.ClassField.Attribute { field; _ } ->
      validate_class_field ~context:("class_field.attribute" :: context) field
  | Cst.ClassField.Extension _ ->
      ()

and validate_class_expression ~context = function
  | Cst.ClassExpression.Path _ | Cst.ClassExpression.Extension _ ->
      ()
  | Cst.ClassExpression.Structure { self_pattern; fields; _ } ->
      Option.iter
        (validate_pattern ~context:("class_expression.structure.self_pattern" :: context))
        self_pattern;
      List.iteri
        (fun index field ->
          validate_class_field
            ~context:
              (("class_expression.structure.field[" ^ Int.to_string index ^ "]")
              :: context)
            field)
        fields
  | Cst.ClassExpression.Fun { body; _ } ->
      validate_class_expression ~context:("class_expression.fun.body" :: context)
        body
  | Cst.ClassExpression.Apply { callee; argument; _ } ->
      validate_class_expression
        ~context:("class_expression.apply.callee" :: context)
        callee;
      validate_apply_argument
        ~context:("class_expression.apply.argument" :: context)
        argument
  | Cst.ClassExpression.Let { parameters; bound_value; and_bindings; body; _ } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:
              (("class_expression.let.parameters[" ^ Int.to_string index ^ "]")
              :: context)
            parameter)
        parameters;
      validate_expression ~context:("class_expression.let.bound_value" :: context)
        bound_value;
      List.iteri
        (fun index binding ->
          validate_expression
            ~context:
              (("class_expression.let.and_binding[" ^ Int.to_string index ^ "]")
              :: context)
            (Cst.LetBinding.value binding))
        and_bindings;
      validate_class_expression ~context:("class_expression.let.body" :: context)
        body
  | Cst.ClassExpression.Constraint
      { class_expression; class_type; _ } ->
      validate_class_expression
        ~context:("class_expression.constraint.expression" :: context)
        class_expression;
      validate_class_type
        ~context:("class_expression.constraint.class_type" :: context)
        class_type
  | Cst.ClassExpression.LocalOpen { class_expression; _ } ->
      validate_class_expression
        ~context:("class_expression.local_open" :: context)
        class_expression
  | Cst.ClassExpression.Parenthesized { inner; _ } ->
      validate_class_expression
        ~context:("class_expression.parenthesized" :: context)
        inner
  | Cst.ClassExpression.Attribute { class_expression; _ } ->
      validate_class_expression
        ~context:("class_expression.attribute" :: context)
        class_expression

and validate_apply_argument ~context = function
  | Cst.Positional expr ->
      validate_expression ~context:("apply_argument.positional" :: context) expr
  | Cst.Labeled { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.labeled.value" :: context))
        value
  | Cst.Optional { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.optional.value" :: context))
        value

and validate_module_expression ~context = function
  | Cst.ModuleExpression.Path _ | Cst.ModuleExpression.Structure _
  | Cst.ModuleExpression.Extension _ ->
      ()
  | Cst.ModuleExpression.Functor { parameters; body; _ } ->
      List.iteri
        (fun index ({ module_type; _ } : Cst.functor_parameter) ->
          validate_module_type
            ~context:
              (("module_expression.functor.parameter[" ^ Int.to_string index ^ "]")
              :: context)
            module_type)
        parameters;
      validate_module_expression ~context:("module_expression.functor.body" :: context)
        body
  | Cst.ModuleExpression.Apply { callee; argument; _ } ->
      validate_module_expression ~context:("module_expression.apply.callee" :: context)
        callee;
      validate_module_expression ~context:("module_expression.apply.argument" :: context)
        argument
  | Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      validate_module_expression
        ~context:("module_expression.apply_unit.callee" :: context)
        callee
  | Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      validate_module_expression
        ~context:("module_expression.constraint.expression" :: context)
        module_expression;
      validate_module_type ~context:("module_expression.constraint.type" :: context)
        module_type
  | Cst.ModuleExpression.ModuleUnpack { expression; module_type; _ } ->
      validate_expression ~context:("module_expression.unpack.expression" :: context)
        expression;
      Option.iter
        (validate_module_type ~context:("module_expression.unpack.type" :: context))
        module_type
  | Cst.ModuleExpression.Parenthesized { inner; _ } ->
      validate_module_expression
        ~context:("module_expression.parenthesized" :: context)
        inner
  | Cst.ModuleExpression.Attribute { module_expression; _ } ->
      validate_module_expression
        ~context:("module_expression.attribute" :: context)
        module_expression

and validate_object_member ~context = function
  | Cst.ObjectMember.Method { body; type_; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.method.body" :: context))
        body;
      Option.iter
        (validate_core_type ~context:("object_member.method.type" :: context))
        type_
  | Cst.ObjectMember.Value { value; type_; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.value.value" :: context))
        value;
      Option.iter
        (validate_core_type ~context:("object_member.value.type" :: context))
        type_
  | Cst.ObjectMember.Inherit { expression; _ } ->
      validate_expression ~context:("object_member.inherit.expression" :: context)
        expression
  | Cst.ObjectMember.Extension _ ->
      ()
  | Cst.ObjectMember.Initializer { body; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.initializer.body" :: context))
        body

and validate_fun_body ~context = function
  | Cst.Expression expression ->
      validate_expression ~context:("fun_body.expression" :: context)
        expression
  | Cst.Cases { cases; _ } ->
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("fun_body.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases

and validate_expression ~context = function
  | Cst.Expression.Path _ | Cst.Expression.Operator _ | Cst.Expression.Literal _
  | Cst.Expression.Unreachable _ | Cst.Expression.Extension _ ->
      ()
  | Cst.Expression.Constructor { payload; _ } ->
      Option.iter
        (validate_expression ~context:("expression.constructor.payload" :: context))
        payload
  | Cst.Expression.ModulePack { module_expression; module_type; _ } ->
      validate_module_expression
        ~context:("expression.first_class_module.expression" :: context)
        module_expression;
      Option.iter
        (validate_module_type
           ~context:("expression.first_class_module.type" :: context))
        module_type
  | Cst.Expression.Object { self_pattern; members; _ } ->
      Option.iter
        (validate_pattern ~context:("expression.object.self_pattern" :: context))
        self_pattern;
      List.iteri
        (fun index member ->
          validate_object_member
            ~context:(("expression.object.member[" ^ Int.to_string index ^ "]") :: context)
            member)
        members
  | Cst.Expression.LetModule { body; _ } ->
      validate_expression ~context:("expression.let_module.body" :: context) body
  | Cst.Expression.LetException { body; _ } ->
      validate_expression ~context:("expression.let_exception.body" :: context)
        body
  | Cst.Expression.PolyVariant { payload; _ } ->
      Option.iter
        (validate_expression ~context:("expression.poly_variant.payload" :: context))
        payload
  | Cst.Expression.Assert { asserted; _ } ->
      validate_expression ~context:("expression.assert.asserted" :: context)
        asserted
  | Cst.Expression.Lazy { body; _ } ->
      validate_expression ~context:("expression.lazy.body" :: context) body
  | Cst.Expression.While { condition; body; _ } ->
      validate_expression ~context:("expression.while.condition" :: context)
        condition;
      validate_expression ~context:("expression.while.body" :: context) body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      validate_expression ~context:("expression.for.start" :: context) start_expr;
      validate_expression ~context:("expression.for.end" :: context) end_expr;
      validate_expression ~context:("expression.for.body" :: context) body
  | Cst.Expression.Apply { callee; argument; _ } ->
      validate_expression ~context:("expression.apply.callee" :: context) callee;
      validate_apply_argument ~context:("expression.apply.argument" :: context)
        argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      validate_expression ~context:("expression.method_call.receiver" :: context)
        receiver
  | Cst.Expression.New _ ->
      ()
  | Cst.Expression.Prefix { operand; _ } ->
      validate_expression ~context:("expression.prefix.operand" :: context)
        operand
  | Cst.Expression.FieldAccess { receiver; _ } ->
      validate_expression ~context:("expression.field_access.receiver" :: context)
        receiver
  | Cst.Expression.Index { collection; index; _ } ->
      validate_expression ~context:("expression.index.collection" :: context)
        collection;
      validate_expression ~context:("expression.index.index" :: context) index
  | Cst.Expression.ObjectOverride { fields; _ } ->
      List.iteri
        (fun index (field : Cst.object_override_field) ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.object_override.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            field.value)
        fields
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      validate_expression
        ~context:("expression.instance_variable_assign.value" :: context)
        value
  | Cst.Expression.FieldAssign { target; value; _ } ->
      validate_expression ~context:("expression.field_assign.receiver" :: context)
        target.receiver;
      validate_expression ~context:("expression.field_assign.value" :: context)
        value
  | Cst.Expression.Assign { target; value; _ } ->
      validate_expression ~context:("expression.assign.target" :: context) target;
      validate_expression ~context:("expression.assign.value" :: context) value
  | Cst.Expression.Infix { left; right; _ } ->
      validate_expression ~context:("expression.infix.left" :: context) left;
      validate_expression ~context:("expression.infix.right" :: context) right
  | Cst.Expression.Typed { expression; type_; _ } ->
      validate_expression ~context:("expression.typed.expression" :: context)
        expression;
      validate_core_type ~context:("expression.typed.type" :: context) type_
  | Cst.Expression.Polymorphic { expression; type_; _ } ->
      validate_expression ~context:("expression.polymorphic.expression" :: context)
        expression;
      validate_core_type ~context:("expression.polymorphic.type" :: context)
        type_
  | Cst.Expression.Coerce { expression; from_type; to_type; _ } ->
      validate_expression ~context:("expression.coerce.expression" :: context)
        expression;
      Option.iter
        (validate_core_type ~context:("expression.coerce.from_type" :: context))
        from_type;
      validate_core_type ~context:("expression.coerce.to_type" :: context) to_type
  | Cst.Expression.Sequence { expressions; _ } ->
      List.iter
        (validate_expression ~context:("expression.sequence.expressions" :: context))
        expressions
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      List.iteri
        (fun index expr ->
          validate_expression
            ~context:(("expression.element[" ^ Int.to_string index ^ "]") :: context)
            expr)
        elements
  | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
      List.iteri
        (fun index (field : Cst.record_expression_field) ->
          validate_expression
            ~context:
              (("expression.record.field[" ^ Int.to_string index ^ "].value")
              :: context)
            field.value)
        fields
  | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
      validate_expression ~context:("expression.record.base" :: context) base;
      List.iteri
        (fun index (field : Cst.record_expression_field) ->
          validate_expression
            ~context:
              (("expression.record.field[" ^ Int.to_string index ^ "].value")
              :: context)
            field.value)
        fields
  | Cst.Expression.LocalOpen { body; _ } ->
      validate_expression ~context:("expression.local_open.body" :: context) body
  | Cst.Expression.Fun { parameters; body; _ } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:(("expression.fun.parameter[" ^ Int.to_string index ^ "]") :: context)
            parameter)
        parameters;
      validate_fun_body ~context:("expression.fun.body" :: context) body
  | Cst.Expression.Function { cases; _ } ->
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.function.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
      validate_pattern ~context:("expression.let_operator.binding.pattern" :: context)
        binding.binding_pattern;
      validate_expression
        ~context:("expression.let_operator.binding.value" :: context)
        binding.bound_value;
      List.iteri
        (fun index ({ binding_pattern; bound_value; _ } : Cst.binding_operator_binding) ->
          validate_pattern
            ~context:
              (("expression.let_operator.and_bindings[" ^ Int.to_string index
               ^ "].pattern")
              :: context)
            binding_pattern;
          validate_expression
            ~context:
              (("expression.let_operator.and_bindings[" ^ Int.to_string index
               ^ "].value")
              :: context)
            bound_value)
        and_bindings;
      validate_expression ~context:("expression.let_operator.body" :: context)
        body
  | Cst.Expression.Let
      { binding_pattern; parameters; bound_value; and_bindings; body; _ } ->
      validate_pattern ~context:("expression.let.pattern" :: context) binding_pattern;
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:
              (("expression.let.parameters[" ^ Int.to_string index ^ "]")
              :: context)
            parameter)
        parameters;
      validate_expression ~context:("expression.let.bound_value" :: context) bound_value;
      List.iteri
        (fun index binding ->
          validate_pattern
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].pattern")
              :: context)
            (Cst.LetBinding.binding_pattern binding);
          validate_expression
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].value")
              :: context)
            (Cst.LetBinding.value binding))
        and_bindings;
      validate_expression ~context:("expression.let.body" :: context) body
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      validate_expression ~context:("expression.match.scrutinee" :: context) scrutinee;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.match.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.Try { body; cases; _ } ->
      validate_expression ~context:("expression.try.body" :: context) body;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.try.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      validate_expression ~context:("expression.if.condition" :: context) condition;
      validate_expression ~context:("expression.if.then_branch" :: context) then_branch;
      Option.iter
        (validate_expression ~context:("expression.if.else_branch" :: context))
        else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      validate_expression ~context:("expression.parenthesized" :: context) inner

and validate_match_case ~context ({ pattern; guard; body; _ } : Cst.match_case) =
  validate_pattern ~context:("match_case.pattern" :: context) pattern;
  Option.iter (validate_expression ~context:("match_case.guard" :: context)) guard;
  validate_expression ~context:("match_case.body" :: context) body

let validate_constructor_arguments ~context = function
  | Cst.ConstructorArguments.Tuple elements ->
      List.iteri
        (fun index element ->
          validate_core_type
            ~context:
              (("constructor_arguments.tuple[" ^ Int.to_string index ^ "]")
              :: context)
            element)
        elements
  | Cst.ConstructorArguments.Record fields ->
      List.iteri
        (fun index field ->
          validate_core_type
            ~context:
              (("constructor_arguments.record[" ^ Int.to_string index ^ "].type")
              :: context)
            (Cst.RecordField.field_type field))
        fields

let validate_type_definition ~context = function
  | Cst.TypeDefinition.Abstract -> ()
  | Cst.TypeDefinition.Alias { manifest; _ } ->
      validate_core_type ~context:("type_definition.alias" :: context) manifest
  | Cst.TypeDefinition.Extensible _ -> ()
  | Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
      validate_module_type
        ~context:("type_definition.first_class_module" :: context)
        module_type
  | Cst.TypeDefinition.Object { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.object_type_field) ->
          validate_core_type
            ~context:
              (("type_definition.object.field[" ^ Int.to_string index ^ "].type")
              :: context)
            field_type)
        fields
  | Cst.TypeDefinition.Record { fields; _ } ->
      List.iteri
        (fun index field ->
          validate_core_type
            ~context:
              (("type_definition.record.field[" ^ Int.to_string index ^ "].type")
              :: context)
            (Cst.RecordField.field_type field))
        fields
  | Cst.TypeDefinition.Variant { constructors; _ } ->
      List.iteri
        (fun index constructor ->
          Option.iter
            (validate_constructor_arguments
               ~context:
                 (("type_definition.variant.constructor[" ^ Int.to_string index
                  ^ "].arguments")
                 :: context))
            (Cst.VariantConstructor.arguments constructor);
          Option.iter
            (validate_core_type
               ~context:
                 (("type_definition.variant.constructor[" ^ Int.to_string index
                  ^ "].payload")
                 :: context))
            (Cst.VariantConstructor.payload_type constructor);
          Option.iter
            (validate_core_type
               ~context:
                 (("type_definition.variant.constructor[" ^ Int.to_string index
                  ^ "].result")
                 :: context))
            (Cst.VariantConstructor.result_type constructor))
        constructors
  | Cst.TypeDefinition.PolyVariant poly_variant ->
      validate_poly_variant ~context:("type_definition.poly_variant" :: context)
        poly_variant

let validate_type_constraint ~context ({ left; right; _ } : Cst.type_constraint) =
  validate_core_type ~context:("type_constraint.left" :: context) left;
  validate_core_type ~context:("type_constraint.right" :: context) right

let rec validate_type_declaration ~context
    ({ type_definition; constraints; and_declarations; _ } : Cst.TypeDeclaration.t) =
  validate_type_definition ~context:("item.type_declaration" :: context)
    type_definition;
  List.iteri
    (fun index constraint_ ->
      validate_type_constraint
        ~context:
          (("item.type_declaration.constraint[" ^ Int.to_string index ^ "]")
          :: context)
        constraint_)
    constraints;
  List.iteri
    (fun index declaration ->
      validate_type_declaration
        ~context:
          (("item.type_declaration.and_declarations[" ^ Int.to_string index ^ "]")
          :: context)
        declaration)
    and_declarations

let validate_type_extension ~context ({ constructors; _ } : Cst.TypeExtension.t) =
  List.iteri
    (fun index constructor ->
      Option.iter
        (validate_constructor_arguments
           ~context:
             (("item.type_extension.constructor[" ^ Int.to_string index
              ^ "].arguments")
             :: context))
        (Cst.VariantConstructor.arguments constructor);
      Option.iter
        (validate_core_type
           ~context:
             (("item.type_extension.constructor[" ^ Int.to_string index
              ^ "].payload")
             :: context))
        (Cst.VariantConstructor.payload_type constructor);
      Option.iter
        (validate_core_type
           ~context:
             (("item.type_extension.constructor[" ^ Int.to_string index
              ^ "].result")
             :: context))
        (Cst.VariantConstructor.result_type constructor))
    constructors

let validate_class_declaration ~context ({ class_type; class_body; _ } : Cst.class_declaration)
    =
  Option.iter
    (validate_class_type ~context:("item.class_declaration.type" :: context))
    class_type;
  Option.iter
    (validate_class_expression ~context:("item.class_declaration.body" :: context))
    class_body

let validate_class_type_declaration ~context
    ({ class_type_body; _ } : Cst.class_type_declaration) =
  validate_class_type ~context:("item.class_type_declaration.body" :: context)
    class_type_body

let validate_module_type_declaration ~context
    ({ module_type; _ } : Cst.ModuleTypeDeclaration.t) =
  Option.iter
    (validate_module_type ~context:("item.module_type_declaration" :: context))
    module_type

let validate_open_statement ~context stmt =
  match Cst.OpenStatement.target stmt with
  | Cst.OpenStatement.Path _ ->
      ()
  | Cst.OpenStatement.ModuleExpression expr ->
      validate_module_expression ~context:("item.open_statement.target" :: context)
        expr

let validate_structure_item ~context = function
  | Cst.StructureItem.TypeDeclaration decl ->
      validate_type_declaration ~context decl
  | Cst.StructureItem.TypeExtension decl ->
      validate_type_extension ~context decl
  | Cst.StructureItem.LetBinding { binding_pattern; value; and_bindings; _ } ->
      validate_pattern ~context:("item.let_binding.pattern" :: context)
        binding_pattern;
      validate_expression ~context:("item.let_binding.value" :: context) value;
      List.iteri
        (fun index binding ->
          validate_pattern
            ~context:
              (("item.let_binding.and_bindings[" ^ Int.to_string index ^ "].pattern")
              :: context)
            (Cst.LetBinding.binding_pattern binding);
          validate_expression
            ~context:
              (("item.let_binding.and_bindings[" ^ Int.to_string index ^ "].value")
              :: context)
            (Cst.LetBinding.value binding))
        and_bindings
  | Cst.StructureItem.Expression expr ->
      validate_expression ~context:("item.expression" :: context) expr
  | Cst.StructureItem.ClassDeclaration decl ->
      validate_class_declaration ~context decl
  | Cst.StructureItem.ClassTypeDeclaration decl ->
      validate_class_type_declaration ~context decl
  | Cst.StructureItem.Attribute _ | Cst.StructureItem.Extension _ ->
      ()
  | Cst.StructureItem.ValueDeclaration { type_; _ } ->
      validate_core_type ~context:("item.value_declaration.type" :: context) type_
  | Cst.StructureItem.ExternalDeclaration { type_; _ } ->
      validate_core_type ~context:("item.external_declaration.type" :: context)
        type_
  | Cst.StructureItem.ModuleTypeDeclaration decl ->
      validate_module_type_declaration ~context decl
  | Cst.StructureItem.OpenStatement stmt ->
      validate_open_statement ~context stmt
  | Cst.StructureItem.ModuleDeclaration _
  | Cst.StructureItem.RecursiveModuleDeclaration _
  | Cst.StructureItem.IncludeStatement _
  | Cst.StructureItem.ExceptionDeclaration _ ->
      ()

let validate_signature_item ~context = function
  | Cst.SignatureItem.TypeDeclaration decl ->
      validate_type_declaration ~context decl
  | Cst.SignatureItem.TypeExtension decl ->
      validate_type_extension ~context decl
  | Cst.SignatureItem.Attribute _ | Cst.SignatureItem.Extension _ ->
      ()
  | Cst.SignatureItem.ClassDeclaration decl ->
      validate_class_declaration ~context decl
  | Cst.SignatureItem.ClassTypeDeclaration decl ->
      validate_class_type_declaration ~context decl
  | Cst.SignatureItem.ModuleTypeDeclaration decl ->
      validate_module_type_declaration ~context decl
  | Cst.SignatureItem.OpenStatement stmt ->
      validate_open_statement ~context stmt
  | Cst.SignatureItem.ValueDeclaration { type_; _ } ->
      validate_core_type ~context:("item.value_declaration.type" :: context) type_
  | Cst.SignatureItem.ExternalDeclaration { type_; _ } ->
      validate_core_type ~context:("item.external_declaration.type" :: context)
        type_
  | Cst.SignatureItem.ModuleDeclaration _
  | Cst.SignatureItem.RecursiveModuleDeclaration _
  | Cst.SignatureItem.IncludeStatement _
  | Cst.SignatureItem.ExceptionDeclaration _ ->
      ()

let validate_source_file source_file =
  (match source_file with
  | Cst.Implementation { items; _ } ->
      List.iteri
        (fun index item ->
          validate_structure_item
            ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ]
            item)
        items
  | Cst.Interface { items; _ } ->
      List.iteri
        (fun index item ->
          validate_signature_item
            ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ]
            item)
        items)

let lift ~kind tree =
  let cst =
    match kind with
    | `Implementation ->
        let syntax_node, items =
          build_source_file_body tree structure_items_from_node
        in
        Cst.Implementation { syntax_node; items }
    | `Interface ->
        let syntax_node, items =
          build_source_file_body tree signature_items_from_node
        in
        Cst.Interface { syntax_node; items }
  in
  validate_source_file cst;
  cst

let create_from_ceibo ~kind tree =
  match lift ~kind tree with
  | cst -> Ok cst
  | exception Bail error -> Error error

let structure_item_payload_nodes_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.STRUCT_EXPR ->
      direct_non_trivia_nodes node
  | _ ->
      [ node ]

let signature_item_payload_nodes_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.SIGNATURE ->
      direct_non_trivia_nodes node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | sig_kw :: _
        when String.equal (Ceibo.Red.SyntaxToken.text sig_kw) "sig" ->
          direct_non_trivia_nodes node
      | _ ->
          [ node ])
  | _ ->
      [ node ]

let structure_items_from_syntax_node node =
  match structure_item_payload_nodes_from_node node |> List.concat_map structure_items_from_node with
  | items -> Ok items
  | exception Bail error -> Error error

let structure_items_from_syntax_nodes nodes =
  match
    nodes
    |> List.concat_map structure_item_payload_nodes_from_node
    |> List.concat_map structure_items_from_node
  with
  | items -> Ok items
  | exception Bail error -> Error error

let signature_items_from_syntax_node node =
  match signature_item_payload_nodes_from_node node |> List.concat_map signature_items_from_node with
  | items -> Ok items
  | exception Bail error -> Error error

let signature_items_from_syntax_nodes nodes =
  match
    nodes
    |> List.concat_map signature_item_payload_nodes_from_node
    |> List.concat_map signature_items_from_node
  with
  | items -> Ok items
  | exception Bail error -> Error error
