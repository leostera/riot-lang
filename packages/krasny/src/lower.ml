open Std
open Std.Collections

module Doc = Doc

let blank_line = Doc.concat [ Doc.line; Doc.line ]
let equals = Doc.concat [ Doc.space; Doc.equal; Doc.space ]
let arrow = Doc.concat [ Doc.space; Doc.arrow; Doc.space ]
let colon = Doc.concat [ Doc.space; Doc.colon; Doc.space ]

let token_text = Syn.Cst.Token.text
let doc_of_token token = Doc.text (token_text token)

let doc_of_verbatim_syntax_node node =
  Syn.Ceibo.Red.SyntaxNode.tokens node
  |> List.map (fun token -> Doc.text (Syn.Ceibo.Red.SyntaxToken.text token))
  |> Doc.concat

let doc_of_node = doc_of_verbatim_syntax_node

let doc_of_top_level_trivia_token syntax_token =
  match Syn.Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      Some (Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token))
  | _ ->
      None

let doc_of_ident ident =
  Syn.Cst.Ident.segments ident
  |> List.map doc_of_token
  |> Doc.join (Doc.text ".")

let doc_of_nontrivia_direct_tokens syntax_node =
  Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node
  |> List.filter (fun syntax_token ->
         match Syn.Ceibo.Red.SyntaxToken.kind syntax_token with
         | Syn.SyntaxKind.WHITESPACE
         | Syn.SyntaxKind.COMMENT
         | Syn.SyntaxKind.DOCSTRING ->
             false
         | _ ->
             true)
  |> List.map (fun syntax_token ->
         Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token))
  |> Doc.concat

let doc_of_core_type type_ =
  doc_of_verbatim_syntax_node (Syn.Cst.CoreType.syntax_node type_)

let doc_of_module_expression expression =
  doc_of_verbatim_syntax_node (Syn.Cst.ModuleExpression.syntax_node expression)

let doc_of_module_type module_type =
  doc_of_verbatim_syntax_node (Syn.Cst.ModuleType.syntax_node module_type)

let kw_let = Doc.text "let"
let kw_rec = Doc.text "rec"
let kw_and = Doc.text "and"
let kw_in = Doc.text "in"
let kw_if = Doc.text "if"
let kw_then = Doc.text "then"
let kw_else = Doc.text "else"
let kw_match = Doc.text "match"
let kw_try = Doc.text "try"
let kw_with = Doc.text "with"
let kw_when = Doc.text "when"
let kw_function = Doc.text "function"
let kw_fun = Doc.text "fun"
let kw_open = Doc.text "open"

let group_digits_from_left ~group_size digits =
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let buffer = IO.Buffer.create (length + length / group_size) in
    let rec loop index =
      if index >= length then
        IO.Buffer.contents buffer
      else (
        if index > 0 then
          IO.Buffer.add_char buffer '_';
        let chunk_size = Int.min group_size (length - index) in
        IO.Buffer.add_string buffer (String.sub digits index chunk_size);
        loop (index + chunk_size))
    in
    loop 0

let group_digits_from_right ~group_size digits =
  let digits = String.split_on_char '_' digits |> String.concat "" in
  let length = String.length digits in
  if length <= group_size then
    digits
  else
    let first_group_size =
      match length mod group_size with
      | 0 -> group_size
      | remainder -> remainder
    in
    let buffer = IO.Buffer.create (length + length / group_size) in
    IO.Buffer.add_string buffer (String.sub digits 0 first_group_size);
    let rec loop index =
      if index >= length then
        IO.Buffer.contents buffer
      else (
        IO.Buffer.add_char buffer '_';
        IO.Buffer.add_string buffer (String.sub digits index group_size);
        loop (index + group_size))
    in
    loop first_group_size

let render_integer_constant (literal : Syn.Cst.integer_constant) =
  let prefix =
    match literal.base with
    | Syn.Cst.Decimal -> Option.unwrap_or literal.prefix ~default:""
    | Syn.Cst.Hexadecimal -> "0x"
    | Syn.Cst.Octal -> "0o"
    | Syn.Cst.Binary -> "0b"
  in
  let digits =
    match literal.base with
    | Syn.Cst.Decimal | Syn.Cst.Octal ->
        group_digits_from_right ~group_size:3 literal.digits
    | Syn.Cst.Binary ->
        group_digits_from_right ~group_size:4 literal.digits
    | Syn.Cst.Hexadecimal ->
        literal.digits |> String.lowercase_ascii |> group_digits_from_right ~group_size:4
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  prefix ^ digits ^ suffix

let render_float_constant (literal : Syn.Cst.float_constant) =
  let exponent =
    match literal.exponent with
    | None -> ""
    | Some exponent ->
        let sign =
          match exponent.sign with
          | None -> ""
          | Some Syn.Cst.Positive -> "+"
          | Some Syn.Cst.Negative -> "-"
        in
        exponent.marker ^ sign ^ exponent.digits
  in
  let suffix = Option.unwrap_or literal.suffix ~default:"" in
  let integral_digits = group_digits_from_right ~group_size:3 literal.integral_digits in
  let fractional_digits = group_digits_from_left ~group_size:3 literal.fractional_digits in
  integral_digits ^ "." ^ fractional_digits ^ exponent ^ suffix

let render_literal = function
  | Syn.Cst.Literal.Int literal ->
      Doc.text (render_integer_constant literal)
  | Syn.Cst.Literal.Float literal ->
      Doc.text (render_float_constant literal)
  | Syn.Cst.Literal.String literal ->
      doc_of_token literal.literal_token
  | Syn.Cst.Literal.Char literal ->
      doc_of_token literal.literal_token
  | Syn.Cst.Literal.Bool literal ->
      Doc.text (if literal.value then "true" else "false")
  | Syn.Cst.Literal.Unit _ ->
      Doc.text "()"

let rec join_map separator f = function
  | [] -> Doc.empty
  | [ value ] -> f value
  | value :: rest ->
      Doc.concat (f value :: List.map (fun item -> Doc.concat [ separator; f item ]) rest)

let rec render_pattern = function
  | Syn.Cst.Pattern.Identifier { name_token; _ } ->
      doc_of_token name_token
  | Syn.Cst.Pattern.Wildcard _ ->
      Doc.text "_"
  | Syn.Cst.Pattern.Literal { literal; _ } ->
      render_literal literal
  | Syn.Cst.Pattern.Constructor { constructor_path; arguments; _ } ->
      let head = doc_of_ident constructor_path in
      (match arguments with
      | [] ->
          head
      | arguments ->
          Doc.concat
            [
              head;
              Doc.space;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_pattern arguments;
            ])
  | Syn.Cst.Pattern.Tuple { elements; _ } ->
      Doc.concat
        [
          Doc.lparen;
          join_map (Doc.concat [ Doc.comma; Doc.space ]) (fun (element : Syn.Cst.tuple_pattern_element) ->
              match element.label_token with
              | None ->
                  render_pattern element.pattern
              | Some label_token ->
                  Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
            elements;
          Doc.rparen;
        ]
  | Syn.Cst.Pattern.List { elements; _ } ->
      Doc.concat
        [
          Doc.lbracket;
          join_map (Doc.concat [ Doc.semi; Doc.space ]) render_pattern elements;
          Doc.rbracket;
        ]
  | Syn.Cst.Pattern.Array { elements; _ } ->
      Doc.concat
        [
          Doc.text "[|";
          join_map (Doc.concat [ Doc.semi; Doc.space ]) render_pattern elements;
          Doc.text "|]";
        ]
  | Syn.Cst.Pattern.Record { fields; closedness; _ } ->
      let fields =
        fields
        |> List.map (fun (field : Syn.Cst.record_pattern_field) ->
               match field.pattern with
               | None ->
                   doc_of_ident field.field_path
               | Some pattern ->
                   Doc.concat [ doc_of_ident field.field_path; equals; render_pattern pattern ])
      in
      let fields =
        match closedness with
        | Syn.Cst.Closed ->
            fields
        | Syn.Cst.Open _ ->
            fields @ [ Doc.text "_" ]
      in
      if List.length fields > 4 then
        Doc.concat
          [
            Doc.lbrace;
            Doc.line;
            Doc.indent 2
              (join_map
                 (Doc.concat [ Doc.semi; Doc.line ])
                 (fun doc -> doc)
                 fields);
            Doc.line;
            Doc.rbrace;
          ]
      else
        Doc.group
          (Doc.concat
             [
               Doc.lbrace;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      join_map
                        (Doc.concat [ Doc.semi; Doc.break ~flat:" " () ])
                        (fun doc -> doc)
                        fields;
                    ]);
               Doc.break ~flat:" " ();
               Doc.rbrace;
             ])
  | Syn.Cst.Pattern.Cons { head; tail; _ } ->
      Doc.concat [ render_pattern head; Doc.space; Doc.text "::"; Doc.space; render_pattern tail ]
  | Syn.Cst.Pattern.Or { alternatives; _ } ->
      join_map (Doc.concat [ Doc.space; Doc.bar; Doc.space ]) render_pattern alternatives
  | Syn.Cst.Pattern.Exception { keyword_token; pattern; _ } ->
      Doc.concat [ doc_of_token keyword_token; Doc.space; render_pattern pattern ]
  | Syn.Cst.Pattern.Range { lower; upper; _ } ->
      Doc.concat
        [
          render_literal lower;
          Doc.space;
          Doc.text "..";
          Doc.space;
          render_literal upper;
        ]
  | Syn.Cst.Pattern.Parenthesized { inner; _ } ->
      (match inner with
      | Syn.Cst.Pattern.Tuple _
      | Syn.Cst.Pattern.List _
      | Syn.Cst.Pattern.Array _
      | Syn.Cst.Pattern.Record _ ->
          render_pattern inner
      | _ ->
          Doc.concat [ Doc.lparen; render_pattern inner; Doc.rparen ])
  | Syn.Cst.Pattern.PolyVariant { syntax_node; payload; _ } ->
      let head = doc_of_nontrivia_direct_tokens syntax_node in
      (match payload with
      | None ->
          head
      | Some payload ->
          Doc.concat [ head; Doc.space; render_pattern payload ])
  | other ->
      doc_of_verbatim_syntax_node (Syn.Cst.Pattern.syntax_node other)

let render_parameter = function
  | Syn.Cst.Parameter.Positional { pattern; _ } ->
      render_pattern pattern
  | Syn.Cst.Parameter.Labeled { sigil_token; label_token; binding_pattern; _ } ->
      (match binding_pattern with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some pattern ->
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.colon;
              render_pattern pattern;
            ])
  | Syn.Cst.Parameter.Optional { has_default = true; syntax_node; _ } ->
      doc_of_verbatim_syntax_node syntax_node
  | Syn.Cst.Parameter.Optional { sigil_token; label_token; binding_pattern; _ } ->
      (match binding_pattern with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some pattern ->
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.colon;
              render_pattern pattern;
            ])
  | Syn.Cst.Parameter.LocallyAbstract { syntax_node; _ } ->
      doc_of_verbatim_syntax_node syntax_node

let is_simple_expression = function
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.PolyVariant _ ->
      true
  | Syn.Cst.Expression.Constructor { payload = None; _ } ->
      true
  | _ ->
      false

let expression_needs_parens_in_apply = function
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Infix _ ->
      true
  | _ ->
      false

let expression_needs_multiline_binding = function
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _ ->
      true
  | _ ->
      false

let rec expression_prefers_multiline_layout = function
  | Syn.Cst.Expression.If { then_branch; else_branch; _ } ->
      branch_prefers_multiline_layout then_branch
      || Option.unwrap_or
           (Option.map branch_prefers_multiline_layout else_branch)
           ~default:false
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _ ->
      true
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Expression body; _ } ->
      expression_prefers_multiline_layout body
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Cases _; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_prefers_multiline_layout inner
  | _ ->
      false

and branch_prefers_multiline_layout = function
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | _ ->
      false

let rec function_body_prefers_multiline = function
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Expression body; _ } ->
      function_body_prefers_multiline body
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Cases _; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      function_body_prefers_multiline inner
  | _ ->
      false

let rec expression_keeps_inline_binding_value = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_keeps_inline_binding_value inner
  | _ ->
      false

let rec collapse_redundant_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.Parens; inner; _ } ->
      collapse_redundant_parenthesized_expression inner
  | Syn.Cst.Expression.Prefix { operator_token; operand = Syn.Cst.Expression.Literal literal; _ }
    when
      let operator = token_text operator_token in
      operator = "-" || operator = "~-" ->
      Some (`NegativeLiteral literal)
  | expression when is_simple_expression expression ->
      Some (`Expression expression)
  | _ ->
      None

let infix_chain operator expression =
  let rec collect acc = function
    | Syn.Cst.Expression.Infix { left; operator_token; right; _ }
      when token_text operator_token = operator ->
        collect (collect acc left) right
    | expression ->
        acc @ [ expression ]
  in
  collect [] expression

let rec render_expression expression =
  match expression with
  | Syn.Cst.Expression.Path { path; _ } ->
      doc_of_ident path
  | Syn.Cst.Expression.Literal literal ->
      render_literal literal
  | Syn.Cst.Expression.Constructor { constructor_path; payload; _ } ->
      let head = doc_of_ident constructor_path in
      (match payload with
      | None ->
          head
      | Some payload ->
          let payload =
            if expression_needs_parens_in_apply payload then
              Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
            else
              render_expression payload
          in
          Doc.concat [ head; Doc.space; payload ])
  | Syn.Cst.Expression.Operator { operator_tokens; _ } ->
      operator_tokens |> List.map token_text |> String.concat "" |> Doc.text
  | Syn.Cst.Expression.Tuple { elements; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lparen;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression elements;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rparen;
           ])
  | Syn.Cst.Expression.List { elements; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lbracket;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map (Doc.concat [ Doc.semi; Doc.break () ]) render_expression
                      elements;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rbracket;
           ])
  | Syn.Cst.Expression.Array { elements; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.text "[|";
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map (Doc.concat [ Doc.semi; Doc.break () ]) render_expression
                      elements;
                  ]);
             Doc.break ~flat:"" ();
             Doc.text "|]";
           ])
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      render_parenthesized_expression expression
  | Syn.Cst.Expression.Prefix { operator_token; operand; _ } ->
      let operator = token_text operator_token in
      (match operand with
      | Syn.Cst.Expression.Literal literal when operator = "-" || operator = "~-" ->
          Doc.concat [ Doc.lparen; Doc.text "-"; render_literal literal; Doc.rparen ]
      | _ ->
          Doc.concat [ Doc.text operator; render_expression operand ])
  | Syn.Cst.Expression.Infix { operator_token; _ } as infix ->
      let operator = token_text operator_token in
      let parts = infix_chain operator infix in
      Doc.group
        (join_map
           (Doc.concat [ Doc.break (); Doc.text operator; Doc.space ])
           render_expression parts)
  | Syn.Cst.Expression.Apply apply ->
      render_apply_expression apply
  | Syn.Cst.Expression.If if_ ->
      render_if_expression if_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~keyword_token:match_.keyword_token
        ~scrutinee:match_.scrutinee ~with_token:match_.with_token
        ~cases:match_.cases
  | Syn.Cst.Expression.Try try_ ->
      render_match_expression ~keyword_token:try_.keyword_token
        ~scrutinee:try_.body ~with_token:try_.with_token ~cases:try_.cases
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression function_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression fun_
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression let_
  | Syn.Cst.Expression.Sequence sequence ->
      render_sequence_expression sequence
  | Syn.Cst.Expression.Record record ->
      render_record_expression record
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } ->
      let receiver =
        match receiver with
        | Syn.Cst.Expression.If _
        | Syn.Cst.Expression.Match _
        | Syn.Cst.Expression.Try _
        | Syn.Cst.Expression.Let _
        | Syn.Cst.Expression.Sequence _
        | Syn.Cst.Expression.Fun _
        | Syn.Cst.Expression.Function _ ->
            Doc.concat [ Doc.lparen; render_expression receiver; Doc.rparen ]
        | _ ->
            render_expression receiver
      in
      Doc.concat [ receiver; Doc.text "."; doc_of_token field_name ]
  | Syn.Cst.Expression.Typed { expression; type_; _ } ->
      Doc.concat [ render_expression expression; colon; doc_of_core_type type_ ]
  | Syn.Cst.Expression.PolyVariant { syntax_node; payload; _ } ->
      let head = doc_of_nontrivia_direct_tokens syntax_node in
      (match payload with
      | None ->
          head
      | Some payload ->
          let payload =
            if expression_needs_parens_in_apply payload then
              Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
            else
              render_expression payload
          in
          Doc.concat [ head; Doc.space; payload ])
  | other ->
      doc_of_node (Syn.Cst.Expression.syntax_node other)

and render_record_field (field : Syn.Cst.record_expression_field) =
  match field.source with
  | Syn.Cst.Punned ->
      doc_of_ident field.field_path
  | Syn.Cst.Explicit ->
      Doc.concat [ doc_of_ident field.field_path; equals; render_expression field.value ]

and render_record_expression = function
  | Syn.Cst.RecordExpression.Literal { fields; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    join_map
                      (Doc.concat [ Doc.semi; Doc.break () ])
                      render_record_field
                      fields;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rbrace;
           ])
  | Syn.Cst.RecordExpression.Update { base; fields; _ } ->
      Doc.group
        (Doc.concat
           [
             Doc.lbrace;
             Doc.indent 2
               (Doc.concat
                  [
                    Doc.break ~flat:"" ();
                    render_expression base;
                    Doc.break ();
                    kw_with;
                    Doc.space;
                    join_map
                      (Doc.concat [ Doc.semi; Doc.break () ])
                      render_record_field
                      fields;
                  ]);
             Doc.break ~flat:"" ();
             Doc.rbrace;
           ])

and render_apply_argument = function
  | Syn.Cst.Positional expression ->
      if expression_needs_parens_in_apply expression then
        Doc.concat [ Doc.lparen; render_expression expression; Doc.rparen ]
      else
        render_expression expression
  | Syn.Cst.Labeled { sigil_token; label_token; value; _ } ->
      (match value with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some value ->
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.text ":";
              render_expression value;
            ])
  | Syn.Cst.Optional { sigil_token; label_token; value; _ } ->
      (match value with
      | None ->
          Doc.concat [ doc_of_token sigil_token; doc_of_token label_token ]
      | Some value ->
          Doc.concat
            [
              doc_of_token sigil_token;
              doc_of_token label_token;
              Doc.text ":";
              render_expression value;
            ])

and render_apply_expression ({ callee; argument; _ } : Syn.Cst.apply_expression) =
  let rec collect_arguments acc = function
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        collect_arguments (argument :: acc) callee
    | expression ->
        (expression, acc)
  in
  let head, arguments = collect_arguments [ argument ] callee in
  let rendered_head =
    match head with
    | Syn.Cst.Expression.Parenthesized _ as expression ->
        render_parenthesized_expression expression
    | _ ->
        render_expression head
  in
  let rendered_arguments = arguments |> List.map render_apply_argument in
  if List.exists Doc.is_multiline rendered_arguments then
    Doc.concat
      [
        rendered_head;
        Doc.line;
        Doc.indent 2 (Doc.join Doc.line rendered_arguments);
      ]
  else
    Doc.group
      (Doc.concat
         (rendered_head
         :: List.map
              (fun argument ->
                Doc.concat [ Doc.break (); argument ])
              rendered_arguments))

and render_if_expression
    ({ syntax_node; keyword_token; then_token; else_token; condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  let condition_doc = render_expression condition in
  let needs_multiline_then =
    branch_prefers_multiline_layout then_branch
  in
  let then_doc =
    if needs_multiline_then then
      render_block_expression then_branch
    else
      render_expression then_branch
  in
  let head =
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.space;
        condition_doc;
        Doc.space;
        doc_of_token then_token;
      ]
  in
  if needs_multiline_then then
    render_if_expression_block
      {
        syntax_node;
        keyword_token;
        then_token;
        else_token;
        condition;
        then_branch;
        else_branch;
        attributes = [];
      }
  else
  match else_branch, else_token with
  | None, _ ->
      Doc.concat [ head; Doc.space; then_doc ]
  | Some else_branch, Some else_token ->
      let needs_multiline_else =
        branch_prefers_multiline_layout else_branch
      in
      let else_doc =
        if needs_multiline_else then
          render_block_expression else_branch
        else
          render_expression else_branch
      in
      if needs_multiline_else then
        Doc.concat
          [
            head;
            Doc.line;
            Doc.indent 2 then_doc;
            Doc.line;
            doc_of_token else_token;
            Doc.line;
            Doc.indent 2 else_doc;
          ]
      else
        Doc.concat [ head; Doc.space; then_doc; Doc.space; doc_of_token else_token; Doc.space; else_doc ]
  | Some else_branch, None ->
      let else_doc = render_expression else_branch in
      Doc.concat [ head; Doc.space; then_doc; Doc.space; kw_else; Doc.space; else_doc ]

and render_case (case : Syn.Cst.match_case) =
  let body = render_expression case.body in
  let prefix =
    match case.bar_token with
    | Some token ->
        Doc.concat [ doc_of_token token; Doc.space ]
    | None ->
        Doc.empty
  in
  let rendered_pattern =
    match case.pattern with
    | Syn.Cst.Pattern.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ]) (fun (element : Syn.Cst.tuple_pattern_element) ->
            match element.label_token with
            | None ->
                render_pattern element.pattern
            | Some label_token ->
                Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
          elements
    | pattern ->
        render_pattern pattern
  in
  let render_branch pattern =
    let guard =
      match case.guard, case.when_token with
      | Some guard, Some when_token ->
          Doc.concat [ Doc.space; doc_of_token when_token; Doc.space; render_expression guard ]
      | Some guard, None ->
          Doc.concat [ Doc.space; kw_when; Doc.space; render_expression guard ]
      | None, _ ->
          Doc.empty
    in
    match case.body with
    | Syn.Cst.Expression.Parenthesized _ when Doc.is_multiline body ->
        Doc.concat
          [
            prefix;
            pattern;
            guard;
            Doc.space;
            doc_of_token case.arrow_token;
            Doc.space;
            Doc.indent 2 body;
          ]
    | _ when Doc.is_multiline body || expression_prefers_multiline_layout case.body ->
      Doc.concat
        [
          prefix;
          pattern;
          guard;
          Doc.space;
          doc_of_token case.arrow_token;
          Doc.line;
          Doc.indent 2 body;
        ]
    | _ ->
        Doc.concat [ prefix; pattern; guard; Doc.space; doc_of_token case.arrow_token; Doc.space; body ]
  in
  match case.pattern with
  | Syn.Cst.Pattern.Or { alternatives; _ } -> (
      match List.rev alternatives with
      | [] ->
          Doc.empty
      | last :: rest_reversed ->
          let leading =
            rest_reversed
            |> List.rev
            |> List.map (fun alternative ->
                   Doc.concat [ prefix; render_pattern alternative; Doc.line ])
          in
          Doc.concat (leading @ [ render_branch (render_pattern last) ]))
  | _ ->
      render_branch rendered_pattern

and render_match_expression ~keyword_token ~scrutinee ~with_token ~cases =
  let scrutinee_doc =
    match scrutinee with
    | Syn.Cst.Expression.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ]) render_expression elements
    | _ when expression_prefers_multiline_layout scrutinee ->
        render_block_expression scrutinee
    | _ ->
        render_expression scrutinee
  in
  let head =
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.space;
        scrutinee_doc;
      ]
  in
  if expression_prefers_multiline_layout scrutinee || Doc.is_multiline scrutinee_doc then
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        Doc.indent 2 scrutinee_doc;
        Doc.line;
        doc_of_token with_token;
        Doc.line;
        join_map Doc.line render_case cases;
      ]
  else
    Doc.concat [ head; Doc.space; doc_of_token with_token; Doc.line; join_map Doc.line render_case cases ]

and flatten_fun_expression ({ parameters; body; _ } : Syn.Cst.fun_expression) =
  let rec loop acc = function
    | Syn.Cst.Cases _ as body ->
        (List.rev acc, body)
    | Syn.Cst.Expression (Syn.Cst.Expression.Fun ({ parameters; body; _ } as inner)) ->
        loop (List.rev_append parameters acc) body
    | Syn.Cst.Expression expression ->
        (List.rev acc, Syn.Cst.Expression expression)
  in
  loop (List.rev parameters) body

and render_fun_expression
    ({ keyword_token; arrow_token; parameters = _; body = _; _ } as fun_ : Syn.Cst.fun_expression) =
  let parameters, body = flatten_fun_expression fun_ in
  let parameters = parameters |> List.map render_parameter in
  let has_multiline_parameter = List.exists Doc.is_multiline parameters in
  let body = render_fun_body body in
  if has_multiline_parameter then
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        Doc.indent 2
          (Doc.concat [ Doc.join Doc.space parameters; Doc.space; doc_of_token arrow_token ]);
        Doc.line;
        Doc.indent 2 body;
      ]
  else if Doc.is_multiline body || List.length parameters = 0 then
    Doc.concat
      [
        doc_of_token keyword_token;
        (if List.length parameters = 0 then Doc.empty else Doc.concat [ Doc.space; Doc.join Doc.space parameters ]);
        Doc.space;
        doc_of_token arrow_token;
        Doc.line;
        Doc.indent 2 body;
      ]
  else
    Doc.concat
      [
        doc_of_token keyword_token;
        (if List.length parameters = 0 then Doc.empty else Doc.concat [ Doc.space; Doc.join Doc.space parameters ]);
        Doc.space;
        doc_of_token arrow_token;
        Doc.space;
        body;
      ]

and render_function_expression ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  match cases with
  | [ { pattern; guard = None; body; _ } ] ->
      Doc.concat [ kw_fun; Doc.space; render_pattern pattern; arrow; render_expression body ]
  | _ ->
      Doc.concat
        [
          doc_of_token keyword_token;
          Doc.line;
          join_map Doc.line render_case cases;
        ]

and render_fun_body = function
  | Syn.Cst.Expression body ->
      if expression_prefers_multiline_layout body then
        render_block_expression body
      else
        render_expression body
  | Syn.Cst.Cases { cases; _ } ->
      Doc.concat
        [
          kw_function;
          Doc.line;
          join_map Doc.line render_case cases;
        ]

and render_block_expression = function
  | Syn.Cst.Expression.If if_ ->
      render_if_expression_block if_
  | Syn.Cst.Expression.Match match_ ->
      render_match_expression ~keyword_token:match_.keyword_token
        ~scrutinee:match_.scrutinee ~with_token:match_.with_token
        ~cases:match_.cases
  | Syn.Cst.Expression.Try try_ ->
      render_match_expression ~keyword_token:try_.keyword_token
        ~scrutinee:try_.body ~with_token:try_.with_token ~cases:try_.cases
  | Syn.Cst.Expression.Let let_ ->
      render_let_expression let_
  | Syn.Cst.Expression.Sequence sequence ->
      render_sequence_expression sequence
  | Syn.Cst.Expression.Function function_ ->
      render_function_expression function_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression fun_
  | Syn.Cst.Expression.Parenthesized _ as expression ->
      render_parenthesized_expression expression
  | expression ->
      render_expression expression

and render_if_expression_block
    ({ keyword_token; then_token; else_token; condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  let then_doc =
    if branch_prefers_multiline_layout then_branch then
      render_block_expression then_branch
    else
      render_expression then_branch
  in
  let head =
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.space;
        render_expression condition;
        Doc.space;
        doc_of_token then_token;
      ]
  in
  match else_branch, else_token with
  | None, _ ->
      Doc.concat [ head; Doc.line; Doc.indent 2 then_doc ]
  | Some else_branch, Some else_token ->
      let else_doc =
        if branch_prefers_multiline_layout else_branch then
          render_block_expression else_branch
        else
          render_expression else_branch
      in
      Doc.concat
        [
          head;
          Doc.line;
          Doc.indent 2 then_doc;
          Doc.line;
          doc_of_token else_token;
          Doc.line;
          Doc.indent 2 else_doc;
        ]
  | Some else_branch, None ->
      let else_doc = render_expression else_branch in
      Doc.concat [ head; Doc.line; Doc.indent 2 then_doc; Doc.line; kw_else; Doc.line; Doc.indent 2 else_doc ]

and render_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized
      { opening_token; closing_token; grouping; inner; _ } ->
      let rendered_inner = render_expression inner in
      (match grouping with
      | Syn.Cst.BeginEnd ->
          Doc.concat
            [
              doc_of_token opening_token;
              Doc.line;
              Doc.indent 2 rendered_inner;
              Doc.line;
              doc_of_token closing_token;
            ]
      | Syn.Cst.Parens -> (
          match inner with
          | Syn.Cst.Expression.Tuple _
          | Syn.Cst.Expression.List _
          | Syn.Cst.Expression.Array _
          | Syn.Cst.Expression.Record _ ->
              render_expression inner
          | Syn.Cst.Expression.Function { keyword_token; cases; _ } ->
              Doc.concat
                [
                  doc_of_token opening_token;
                  doc_of_token keyword_token;
                  Doc.line;
                  Doc.indent 5 (join_map Doc.line render_case cases);
                  doc_of_token closing_token;
                ]
          | _ -> (
              match collapse_redundant_parenthesized_expression inner with
              | Some (`NegativeLiteral literal) ->
                  Doc.concat
                    [
                      doc_of_token opening_token;
                      Doc.text "-";
                      render_literal literal;
                      doc_of_token closing_token;
                    ]
              | Some (`Expression expression) ->
                  render_expression expression
              | None ->
                  if expression_prefers_multiline_layout inner || Doc.is_multiline rendered_inner then
                    (match inner with
                    | Syn.Cst.Expression.Function _
                    | Syn.Cst.Expression.Fun _ ->
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            rendered_inner;
                            doc_of_token closing_token;
                          ]
                    | _ ->
                        Doc.concat
                          [
                            doc_of_token opening_token;
                            Doc.line;
                            Doc.indent 2 rendered_inner;
                            Doc.line;
                            doc_of_token closing_token;
                          ])
                  else
                    Doc.concat
                      [
                        doc_of_token opening_token;
                        rendered_inner;
                        doc_of_token closing_token;
                      ])))
  | expression ->
      render_expression expression

and render_sequence_expression ({ separator_token; left; right; _ } : Syn.Cst.sequence_expression) =
  let rec flatten acc = function
    | Syn.Cst.Expression.Sequence { left; right; _ } ->
        flatten (flatten acc left) right
    | expression ->
        acc @ [ expression ]
  in
  let expressions = flatten [] left @ [ right ] in
  expressions
  |> List.mapi (fun index expression ->
         let suffix =
           if index < List.length expressions - 1 then
             doc_of_token separator_token
           else
             Doc.empty
         in
         Doc.concat [ render_expression expression; suffix ])
  |> Doc.join Doc.line

and render_binding_header ~keyword_token ~rec_token pattern =
  let rec_part =
    match rec_token with
    | None ->
        []
    | Some token ->
        [ Doc.space; doc_of_token token ]
  in
  Doc.concat ([ doc_of_token keyword_token ] @ rec_part @ [ Doc.space; render_pattern pattern ])

and render_binding_value ~parameters ~value =
  match parameters with
  | [] ->
      render_expression value
  | parameters ->
      let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
      let has_multiline_parameters = Doc.is_multiline parameters in
      let body = render_expression value in
      if has_multiline_parameters then
        Doc.concat
          [
            kw_fun;
            Doc.line;
            Doc.indent 2 (Doc.concat [ parameters; Doc.space; Doc.arrow ]);
            Doc.line;
            Doc.indent 2 body;
          ]
      else if function_body_prefers_multiline value || Doc.is_multiline body then
        Doc.concat
          [
            kw_fun;
            Doc.space;
            parameters;
            Doc.space;
            Doc.arrow;
            Doc.line;
            Doc.indent 2 body;
          ]
      else
        Doc.group
          (Doc.concat
             [
               kw_fun;
               Doc.space;
               parameters;
               Doc.space;
               Doc.arrow;
               Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
             ])

and render_local_binding ~keyword_token ~rec_token ~equals_token ~pattern ~parameters ~value =
  let header = render_binding_header ~keyword_token ~rec_token pattern in
  let rendered_value = render_binding_value ~parameters ~value in
  let keep_value_after_equals =
    match value with
    | Syn.Cst.Expression.Fun _ ->
        true
    | _ ->
        List.length parameters > 0 || expression_keeps_inline_binding_value value
  in
  if keep_value_after_equals then
    Doc.concat
      [
        header;
        Doc.space;
        doc_of_token equals_token;
        Doc.space;
        rendered_value;
      ]
  else if expression_prefers_multiline_layout value || Doc.is_multiline rendered_value then
    Doc.concat
      [
        header;
        Doc.space;
        doc_of_token equals_token;
        Doc.line;
        Doc.indent 2 rendered_value;
      ]
  else
    Doc.group
      (Doc.concat
         [
           header;
           Doc.space;
           doc_of_token equals_token;
           Doc.indent 2 (Doc.concat [ Doc.break (); rendered_value ]);
         ])

and render_let_expression
    ({ keyword_token; rec_token; equals_token; binding_pattern; parameters; bound_value; and_bindings; body; in_token; _ } :
      Syn.Cst.let_expression) =
  let first_binding =
    render_local_binding ~keyword_token ~rec_token ~equals_token ~pattern:binding_pattern
      ~parameters
      ~value:bound_value
  in
  let and_bindings = and_bindings |> List.map render_let_binding_group_item in
  let bindings =
    Doc.concat
      (first_binding :: List.map (fun binding -> Doc.concat [ Doc.line; binding ]) and_bindings)
  in
  let body_doc = render_expression body in
  if Doc.is_multiline first_binding then
    Doc.concat
      [
        bindings;
        Doc.line;
        doc_of_token in_token;
        Doc.line;
        body_doc;
      ]
  else
    Doc.concat
      [
        bindings;
        Doc.space;
        doc_of_token in_token;
        Doc.line;
        body_doc;
      ]

and render_let_binding_group_item (binding : Syn.Cst.let_binding) =
  render_local_binding ~keyword_token:binding.keyword_token ~rec_token:binding.rec_token
    ~equals_token:binding.equals_token ~pattern:binding.binding_pattern
    ~parameters:binding.parameters ~value:binding.value

let render_let_binding (binding : Syn.Cst.let_binding) =
  render_let_binding_group_item binding

let render_open_target = function
  | Syn.Cst.OpenStatement.Path path ->
      doc_of_ident path
  | Syn.Cst.OpenStatement.ModuleExpression expression ->
      doc_of_module_expression expression

let render_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      render_let_binding binding
  | Syn.Cst.StructureItem.OpenStatement open_ ->
      Doc.concat
        [
          Doc.text "open";
          (if open_.bang_token = None then Doc.empty else Doc.text "!");
          Doc.space;
          render_open_target open_.target;
        ]
  | Syn.Cst.StructureItem.Expression expression ->
      render_expression expression
  | item ->
      doc_of_node (Syn.Cst.StructureItem.syntax_node item)

let render_signature_item item =
  doc_of_node (Syn.Cst.SignatureItem.syntax_node item)

let render_top_level_items ~source_node ~items ~render_item =
  let flush_pending pending acc =
    match pending with
    | [] ->
        acc
    | pending ->
        Doc.join Doc.line (List.rev pending) :: acc
  in
  let rec loop pending acc items = function
    | [] ->
        let acc = flush_pending pending acc in
        Doc.join blank_line (List.rev acc)
    | child :: rest -> (
        match child with
        | Syn.Ceibo.Red.Token syntax_token -> (
            match doc_of_top_level_trivia_token syntax_token with
            | Some doc ->
                loop (doc :: pending) acc items rest
            | None ->
                loop pending acc items rest)
        | Syn.Ceibo.Red.Node _ -> (
            match items with
            | item :: items ->
                let acc = flush_pending pending acc in
                loop [] (render_item item :: acc) items rest
            | [] ->
                loop pending acc items rest))
  in
  loop [] [] items (Syn.Ceibo.Red.SyntaxNode.children_list source_node)

let render_structure_items ~source_node items =
  render_top_level_items ~source_node ~items ~render_item:render_structure_item

let render_signature_items ~source_node items =
  render_top_level_items ~source_node ~items ~render_item:render_signature_item

let source_file ~source:_ = function
  | Syn.Cst.Implementation implementation ->
      Some
        (render_structure_items
           ~source_node:(Syn.Cst.SourceFile.syntax_node (Syn.Cst.Implementation implementation))
           implementation.items)
  | Syn.Cst.Interface interface ->
      Some
        (render_signature_items
           ~source_node:(Syn.Cst.SourceFile.syntax_node (Syn.Cst.Interface interface))
           interface.items)
