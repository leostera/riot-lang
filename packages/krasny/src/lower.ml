open Std
open Std.Collections

module Doc = Doc

let blank_line = Doc.concat [ Doc.line; Doc.line ]
let equals = Doc.concat [ Doc.space; Doc.equal; Doc.space ]
let arrow = Doc.concat [ Doc.space; Doc.arrow; Doc.space ]
let colon = Doc.concat [ Doc.space; Doc.colon; Doc.space ]
let star = Doc.text "*"

type pending_trivia_entry =
  | TriviaDoc of Doc.t
  | TriviaBreak of int

let token_text = Syn.Cst.Token.text
let doc_of_token token = Doc.text (token_text token)

let doc_of_verbatim_syntax_node node =
  Syn.Ceibo.Red.SyntaxNode.tokens node
  |> List.map (fun token -> Doc.text (Syn.Ceibo.Red.SyntaxToken.text token))
  |> Doc.concat

let doc_of_node = doc_of_verbatim_syntax_node

let text_of_syntax_node syntax_node =
  Syn.Ceibo.Red.SyntaxNode.tokens syntax_node
  |> List.map Syn.Ceibo.Red.SyntaxToken.text
  |> String.concat ""

let string_contains_substring text pattern =
  let text_length = String.length text in
  let pattern_length = String.length pattern in
  let rec loop index =
    if index + pattern_length > text_length then
      false
    else if String.sub text index pattern_length = pattern then
      true
    else
      loop (index + 1)
  in
  pattern_length > 0 && loop 0

let normalized_source_length source =
  let rec loop index in_whitespace acc =
    if index >= String.length source then
      acc
    else
      match source.[index] with
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
          if in_whitespace then
            loop (index + 1) true acc
          else
            loop (index + 1) true (acc + 1)
      | _ ->
          loop (index + 1) false (acc + 1)
  in
  loop 0 true 0

let trim_trailing_layout_whitespace text =
  let rec find_last_non_layout index =
    if index < 0 then
      -1
    else
      match text.[index] with
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
          find_last_non_layout (index - 1)
      | _ ->
          index
  in
  let last_index = find_last_non_layout (String.length text - 1) in
  if last_index < 0 then
    ""
  else
    String.sub text 0 (last_index + 1)

let syntax_node_has_internal_newline syntax_node =
  let text = text_of_syntax_node syntax_node |> trim_trailing_layout_whitespace in
  String.contains text "\n"

let doc_of_top_level_trivia_token syntax_token =
  match Syn.Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      Some (Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token))
  | _ ->
      None

let is_comment_like_token syntax_token =
  match Syn.Ceibo.Red.SyntaxToken.kind syntax_token with
  | Syn.SyntaxKind.COMMENT
  | Syn.SyntaxKind.DOCSTRING ->
      true
  | _ ->
      false

let syntax_node_has_comment_like_token syntax_node =
  Syn.Ceibo.Red.SyntaxNode.tokens syntax_node
  |> List.exists is_comment_like_token

let is_whitespace_token syntax_token =
  Syn.Ceibo.Red.SyntaxToken.kind syntax_token = Syn.SyntaxKind.WHITESPACE

let text_of_tokens_between_spans syntax_node ~start_offset ~end_offset =
  if end_offset <= start_offset then
    ""
  else
    Syn.Ceibo.Red.SyntaxNode.tokens syntax_node
    |> List.filter (fun syntax_token ->
           let span = Syn.Ceibo.Red.SyntaxToken.span syntax_token in
           span.start >= start_offset && span.end_ <= end_offset)
    |> List.map Syn.Ceibo.Red.SyntaxToken.text
    |> String.concat ""
    |> String.trim

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

let nontrivia_direct_tokens syntax_node =
  Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node
  |> List.filter (fun syntax_token ->
         match Syn.Ceibo.Red.SyntaxToken.kind syntax_token with
         | Syn.SyntaxKind.WHITESPACE
         | Syn.SyntaxKind.COMMENT
         | Syn.SyntaxKind.DOCSTRING ->
             false
         | _ ->
             true)

let whitespace_has_newline syntax_token =
  is_whitespace_token syntax_token
  && String.contains (Syn.Ceibo.Red.SyntaxToken.text syntax_token) "\n"

let whitespace_has_blank_line syntax_token =
  let text = Syn.Ceibo.Red.SyntaxToken.text syntax_token in
  let rec has_double_newline index =
    if index + 1 >= String.length text then
      false
    else if text.[index] = '\n' && text.[index + 1] = '\n' then
      true
    else
      has_double_newline (index + 1)
  in
  Syn.Ceibo.Red.SyntaxToken.kind syntax_token = Syn.SyntaxKind.WHITESPACE
  && has_double_newline 0

let separator_of_whitespace_token syntax_token =
  if whitespace_has_blank_line syntax_token then
    blank_line
  else if whitespace_has_newline syntax_token then
    Doc.line
  else
    Doc.space

let newline_count_of_whitespace_token syntax_token =
  let text = Syn.Ceibo.Red.SyntaxToken.text syntax_token in
  let rec loop index count =
    if index >= String.length text then
      count
    else if text.[index] = '\n' then
      loop (index + 1) (count + 1)
    else
      loop (index + 1) count
  in
  if is_whitespace_token syntax_token then
    loop 0 0
  else
    0

let trailing_comment_suffix_doc syntax_node =
  let trailing_tokens =
    let rec collect acc = function
      | [] ->
          acc
      | token :: rest when is_whitespace_token token || is_comment_like_token token ->
          collect (token :: acc) rest
      | _ ->
          acc
    in
    Syn.Ceibo.Red.SyntaxNode.tokens syntax_node |> List.rev |> collect []
  in
  let rec loop acc separator = function
    | [] ->
        acc
    | token :: rest when is_whitespace_token token ->
        loop acc (separator_of_whitespace_token token) rest
    | token :: rest when is_comment_like_token token ->
        let doc = Doc.text (Syn.Ceibo.Red.SyntaxToken.text token) in
        let acc =
          match acc with
          | None ->
              Some (Doc.concat [ separator; doc ])
          | Some current ->
              Some (Doc.concat [ current; separator; doc ])
        in
        loop acc Doc.empty rest
    | _ :: rest ->
        loop acc separator rest
  in
  loop None Doc.empty trailing_tokens

let push_pending_break pending break_count =
  if break_count <= 0 then
    pending
  else
  match pending with
  | TriviaBreak existing :: rest ->
      TriviaBreak (Int.max existing break_count) :: rest
  | _ ->
      TriviaBreak break_count :: pending

let pending_doc_count pending =
  pending
  |> List.fold_left
       (fun count -> function
         | TriviaDoc _ ->
             count + 1
         | TriviaBreak _ ->
             count)
       0

let render_pending_trivia ?(strip_trailing_breaks = true) pending =
  let break_doc break_count =
    List.init break_count (fun _ -> Doc.line)
    |> Doc.concat
  in
  let rec strip_trailing_blanks = function
    | [] ->
        []
    | [ TriviaBreak _ ] ->
        []
    | entry :: rest ->
        let rest = strip_trailing_blanks rest in
        (match entry, rest with
        | TriviaBreak _, [] ->
            []
        | _ ->
            entry :: rest)
  in
  let rec loop acc separator = function
    | [] ->
        acc
    | TriviaBreak break_count :: rest ->
        let separator = break_doc break_count in
        loop acc separator rest
    | TriviaDoc doc :: rest ->
        let acc =
          match acc with
          | None ->
              Some doc
          | Some current ->
              Some (Doc.concat [ current; separator; doc ])
        in
        loop acc Doc.line rest
  in
  let pending = List.rev pending in
  let pending =
    if strip_trailing_breaks then
      strip_trailing_blanks pending
    else
      pending
  in
  let trailing_break =
    if strip_trailing_breaks then
      None
    else
      match List.rev pending with
      | TriviaBreak break_count :: _ ->
          Some (break_doc break_count)
      | _ ->
          None
  in
  match loop None Doc.line pending, trailing_break with
  | Some doc, Some trailing_break ->
      Some (Doc.concat [ doc; trailing_break ])
  | doc, None ->
      doc
  | None, Some _ ->
      None

let render_interleaved_node_docs ~source_node ~should_consume_node ~node_docs =
  let flush_pending pending acc =
    match render_pending_trivia pending with
    | None ->
        acc
    | Some pending_doc ->
        pending_doc :: acc
  in
  let rec loop pending acc node_docs = function
    | [] ->
        flush_pending pending acc |> List.rev
    | child :: rest -> (
        match child with
        | Syn.Ceibo.Red.Token syntax_token -> (
            match doc_of_top_level_trivia_token syntax_token with
            | Some doc ->
                loop (TriviaDoc doc :: pending) acc node_docs rest
            | None ->
                let pending =
                  let newline_count = newline_count_of_whitespace_token syntax_token in
                  if newline_count > 0 then
                    push_pending_break pending newline_count
                  else
                    pending
                in
                loop pending acc node_docs rest)
        | Syn.Ceibo.Red.Node node ->
            if should_consume_node node then
              match node_docs with
              | node_doc :: node_docs ->
                  let acc = flush_pending pending acc in
                  loop [] (node_doc :: acc) node_docs rest
              | [] ->
                  loop pending acc node_docs rest
            else
              loop pending acc node_docs rest)
  in
  loop [] [] node_docs (Syn.Ceibo.Red.SyntaxNode.children_list source_node)

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
let kw_type = Doc.text "type"
let kw_external = Doc.text "external"
let kw_constraint = Doc.text "constraint"
let kw_of = Doc.text "of"
let kw_mutable = Doc.text "mutable"
let kw_private = Doc.text "private"

let join_map separator f = function
  | [] ->
      Doc.empty
  | first :: rest ->
      Doc.concat
        (f first :: List.map (fun item -> Doc.concat [ separator; f item ]) rest)

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
  let normalized_integral_digits =
    String.split_on_char '_' literal.integral_digits |> String.concat ""
  in
  let integral_digits =
    if String.length normalized_integral_digits >= 8 then
      group_digits_from_right ~group_size:3 normalized_integral_digits
    else
      normalized_integral_digits
  in
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

let render_type_binder = function
  | Syn.Cst.TypeBinder.Quoted binder ->
      Doc.text (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Quoted binder))
  | Syn.Cst.TypeBinder.Bare binder ->
      Doc.text (Syn.Cst.TypeBinder.text (Syn.Cst.TypeBinder.Bare binder))

let render_arrow_label = function
  | None ->
      Doc.empty
  | Some (Syn.Cst.ArrowLabel.Named { sigil_token; label_token }) ->
      Doc.concat
        [
          Option.unwrap_or (Option.map doc_of_token sigil_token) ~default:Doc.empty;
          doc_of_token label_token;
          Doc.colon;
        ]
  | Some (Syn.Cst.ArrowLabel.OptionalNamed { sigil_token; label_token }) ->
      Doc.concat [ doc_of_token sigil_token; doc_of_token label_token; Doc.colon ]

let rec core_type_needs_parens_in_application = function
  | Syn.Cst.CoreType.Arrow _
  | Syn.Cst.CoreType.Tuple _
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _
  | Syn.Cst.CoreType.Alias _ ->
      true
  | Syn.Cst.CoreType.Parenthesized _ ->
      false
  | _ ->
      false

let render_type_parameter parameter =
  let variance =
    match Syn.Cst.TypeParameter.variance parameter with
    | None ->
        Doc.empty
    | Some (Syn.Cst.TypeParameterVariance.Covariant { marker_token }) ->
        doc_of_token marker_token
    | Some (Syn.Cst.TypeParameterVariance.Contravariant { marker_token }) ->
        doc_of_token marker_token
  in
  let injective =
    if Syn.Cst.TypeParameter.is_injective parameter then
      Doc.text "!"
    else
      Doc.empty
  in
  let variable =
    match Syn.Cst.TypeParameter.type_variable parameter with
    | Some type_variable ->
        Doc.text (Syn.Cst.TypeVariable.text type_variable)
    | None ->
        Doc.text "_"
  in
  Doc.concat [ variance; injective; variable ]

let render_type_parameters parameters =
  match parameters with
  | [] ->
      Doc.empty
  | [ parameter ] ->
      render_type_parameter parameter
  | parameters when List.length parameters > 6 ->
      Doc.concat
        [
          Doc.lparen;
          Doc.line;
          Doc.indent 2
            (join_map (Doc.concat [ Doc.comma; Doc.line ]) render_type_parameter parameters);
          Doc.line;
          Doc.rparen;
        ]
  | parameters ->
      Doc.concat
        [
          Doc.lparen;
          join_map (Doc.concat [ Doc.comma; Doc.space ]) render_type_parameter parameters;
          Doc.rparen;
        ]

let rec core_type_arrow_arity = function
  | Syn.Cst.CoreType.Arrow { result_type; _ } ->
      1 + core_type_arrow_arity result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_arrow_arity inner
  | _ ->
      0

let rec core_type_has_labeled_arrow = function
  | Syn.Cst.CoreType.Arrow { label = Some _; _ } ->
      true
  | Syn.Cst.CoreType.Arrow { result_type; _ } ->
      core_type_has_labeled_arrow result_type
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_has_labeled_arrow inner
  | _ ->
      false

let rec core_type_prefers_multiline = function
  | Syn.Cst.CoreType.Arrow arrow ->
      core_type_arrow_arity (Syn.Cst.CoreType.Arrow arrow) >= 5
      || core_type_prefers_multiline arrow.parameter_type
      || core_type_prefers_multiline arrow.result_type
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      List.length elements > 3 || List.exists core_type_prefers_multiline elements
  | Syn.Cst.CoreType.PolyVariant _
  | Syn.Cst.CoreType.Record _
  | Syn.Cst.CoreType.Object _ ->
      true
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_prefers_multiline inner
  | Syn.Cst.CoreType.Alias { type_; _ } ->
      core_type_prefers_multiline type_
  | Syn.Cst.CoreType.Constr { arguments; _ } ->
      List.exists core_type_prefers_multiline arguments
  | _ ->
      false

let rec core_type_is_atomic = function
  | Syn.Cst.CoreType.Wildcard _
  | Syn.Cst.CoreType.Var _ ->
      true
  | Syn.Cst.CoreType.Constr { arguments = []; _ } ->
      true
  | Syn.Cst.CoreType.Constr { arguments = [ argument ]; _ } ->
      core_type_is_atomic argument
  | Syn.Cst.CoreType.Constr { arguments = [ Syn.Cst.CoreType.Tuple { elements; _ } ]; _ } ->
      List.for_all core_type_is_atomic elements
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      core_type_is_atomic inner
  | _ ->
      false

let record_field_prefers_multiline ~name_token ~field_type =
  String.length (token_text name_token) > 32 && not (core_type_is_atomic field_type)

let rec render_core_type = function
  | Syn.Cst.CoreType.Wildcard { wildcard_token; _ } ->
      doc_of_token wildcard_token
  | Syn.Cst.CoreType.Var { syntax_node; _ } ->
      doc_of_nontrivia_direct_tokens syntax_node
  | Syn.Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      let head = doc_of_ident constructor_path in
      (match arguments with
      | [] ->
          head
      | [ Syn.Cst.CoreType.Tuple { elements; _ } ] ->
          Doc.concat
            [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type elements;
              Doc.rparen;
              Doc.space;
              head;
            ]
      | [ argument ] ->
          let argument =
            if core_type_needs_parens_in_application argument then
              Doc.concat [ Doc.lparen; render_core_type argument; Doc.rparen ]
            else
              render_core_type argument
          in
          Doc.concat [ argument; Doc.space; head ]
      | arguments ->
          Doc.concat
            [
              Doc.lparen;
              join_map (Doc.concat [ Doc.comma; Doc.space ]) render_core_type arguments;
              Doc.rparen;
              Doc.space;
              head;
            ])
  | Syn.Cst.CoreType.Alias { type_; name_token; _ } ->
      Doc.concat [ render_core_type type_; Doc.space; Doc.text "as"; Doc.space; doc_of_token name_token ]
  | Syn.Cst.CoreType.Poly { binders; body; _ } ->
      Doc.concat
        [
          join_map (Doc.concat [ Doc.space ]) render_type_binder binders;
          Doc.text ".";
          Doc.space;
          render_core_type body;
        ]
  | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      let render_arrow_parameter label parameter_type =
        let parameter_type =
          match parameter_type with
          | Syn.Cst.CoreType.Arrow _ ->
              Doc.concat [ Doc.lparen; render_core_type parameter_type; Doc.rparen ]
          | _ ->
              render_core_type parameter_type
        in
        Doc.concat [ render_arrow_label label; parameter_type ]
      in
      let rec collect params label parameter_type result_type =
        let params = params @ [ render_arrow_parameter label parameter_type ] in
        match result_type with
        | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
            collect params label parameter_type result_type
        | result_type ->
            (params, render_core_type result_type)
      in
      let parameters, result = collect [] label parameter_type result_type in
      let parts = parameters @ [ result ] in
      Doc.group
        (join_map (Doc.concat [ Doc.space; Doc.arrow; Doc.break () ]) (fun doc -> doc) parts)
  | Syn.Cst.CoreType.Tuple { elements; _ } ->
      Doc.group
        (join_map
           (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ])
           render_core_type elements)
  | Syn.Cst.CoreType.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_core_type inner; Doc.rparen ]
  | Syn.Cst.CoreType.PolyVariant poly_variant ->
      render_poly_variant_type poly_variant
  | Syn.Cst.CoreType.Record { fields; _ } ->
      render_record_type fields
  | Syn.Cst.CoreType.FirstClassModule { module_type; _ } ->
      Doc.concat
        [ Doc.lparen; Doc.text "module"; Doc.space; doc_of_module_type module_type; Doc.rparen ]
  | other ->
      doc_of_verbatim_syntax_node (Syn.Cst.CoreType.syntax_node other)

and render_record_core_type_field (field : Syn.Cst.record_type_field) =
  let type_doc = render_core_type field.field_type in
  let separator =
    if
      core_type_prefers_multiline field.field_type
      || record_field_prefers_multiline
           ~name_token:field.field_name ~field_type:field.field_type
    then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    if field.is_mutable then
      Doc.concat [ kw_mutable; Doc.space; doc_of_token field.field_name ]
    else
      doc_of_token field.field_name
  in
  Doc.group
    (Doc.concat
       [
         prefix;
         Doc.space;
         Doc.colon;
         Doc.indent 2 (Doc.concat [ separator; type_doc ]);
       ])

and render_record_type fields =
  Doc.concat
    [
      Doc.lbrace;
      Doc.line;
      Doc.indent 2
        (join_map (Doc.concat [ Doc.semi; Doc.line ]) render_record_core_type_field fields);
      Doc.line;
      Doc.rbrace;
    ]

and render_record_definition_field (field : Syn.Cst.RecordField.t) =
  let field_type = Syn.Cst.RecordField.field_type field in
  let type_doc = render_core_type field_type in
  let separator =
    if
      core_type_prefers_multiline field_type
      || record_field_prefers_multiline
           ~name_token:(Syn.Cst.RecordField.field_name_token field)
           ~field_type
    then
      Doc.line
    else
      Doc.break ()
  in
  let prefix =
    if Syn.Cst.RecordField.is_mutable field then
      Doc.concat [ kw_mutable; Doc.space; doc_of_token (Syn.Cst.RecordField.field_name_token field) ]
    else
      doc_of_token (Syn.Cst.RecordField.field_name_token field)
  in
  Doc.group
    (Doc.concat
       [
         prefix;
         Doc.space;
         Doc.colon;
         Doc.indent 2 (Doc.concat [ separator; type_doc ]);
       ])

and render_record_definition fields =
  let body =
    join_map (Doc.concat [ Doc.semi; Doc.line ]) render_record_definition_field fields
  in
  Doc.concat
    [
      Doc.lbrace;
      Doc.line;
      Doc.indent 2
        (Doc.concat
           [
             body;
             (if fields = [] then Doc.empty else Doc.semi);
           ]);
      Doc.line;
      Doc.rbrace;
    ]

and render_inline_record_definition fields =
  if fields = [] then
    Doc.concat [ Doc.lbrace; Doc.rbrace ]
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
                    render_record_definition_field fields;
                ]);
           Doc.break ~flat:" " ();
           Doc.rbrace;
         ])

and render_record_definition_with_comments ~source_node fields =
  let field_docs =
    fields
    |> List.map (fun field ->
           Doc.concat [ render_record_definition_field field; Doc.semi ])
  in
  let body =
    render_interleaved_node_docs ~source_node
      ~should_consume_node:(fun node ->
        Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_RECORD_FIELD)
      ~node_docs:field_docs
    |> Doc.join Doc.line
  in
  Doc.concat [ Doc.lbrace; Doc.line; Doc.indent 2 body; Doc.line; Doc.rbrace ]

and render_inline_record_definition_with_comments ~source_node fields =
  let field_count = List.length fields in
  let field_docs =
    fields
    |> List.mapi (fun index field ->
           let doc = render_record_definition_field field in
           if index < field_count - 1 then
             Doc.concat [ doc; Doc.semi ]
           else
             doc)
  in
  let body =
    render_interleaved_node_docs ~source_node
      ~should_consume_node:(fun node ->
        Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_RECORD_FIELD)
      ~node_docs:field_docs
    |> Doc.join Doc.line
  in
  Doc.concat [ Doc.lbrace; Doc.line; Doc.indent 2 body; Doc.line; Doc.rbrace ]

and render_object_type_field (field : Syn.Cst.object_type_field) =
  Doc.group
    (Doc.concat
       [
         doc_of_token field.field_name;
         Doc.space;
         Doc.colon;
         Doc.indent 2 (Doc.concat [ Doc.break (); render_core_type field.field_type ]);
       ])

and render_object_type fields =
  Doc.concat
    [
      Doc.text "<";
      Doc.line;
      Doc.indent 2
        (join_map (Doc.concat [ Doc.semi; Doc.line ]) render_object_type_field fields);
      Doc.line;
      Doc.text ">";
    ]

and render_poly_variant_field = function
  | Syn.Cst.RowField.Tag tag ->
      let head = Doc.concat [ Doc.text "`"; doc_of_token tag.tag_name ] in
      (match tag.payload_type with
      | None ->
          head
      | Some payload_type ->
          Doc.concat [ head; Doc.space; kw_of; Doc.space; render_core_type payload_type ])
  | Syn.Cst.RowField.Inherit { type_; _ } ->
      render_core_type type_

and render_poly_variant_type ?(field_indent = 2) poly_variant =
  let open_doc =
    match Syn.Cst.PolyVariant.kind poly_variant with
    | Syn.Cst.PolyVariantBound.Exact ->
        Doc.lbracket
    | Syn.Cst.PolyVariantBound.UpperBound { marker_token } ->
        Doc.concat [ Doc.lbracket; doc_of_token marker_token ]
    | Syn.Cst.PolyVariantBound.LowerBound { marker_token } ->
        Doc.concat [ Doc.lbracket; doc_of_token marker_token ]
  in
  let fields =
    Syn.Cst.PolyVariant.fields poly_variant
    |> List.map (fun field ->
           Doc.concat [ Doc.bar; Doc.space; render_poly_variant_field field ])
  in
  Doc.concat
    [
      open_doc;
      Doc.line;
      Doc.indent field_indent (Doc.join Doc.line fields);
      Doc.line;
      Doc.rbracket;
    ]

let poly_variant_has_inherit_field poly_variant =
  Syn.Cst.PolyVariant.fields poly_variant
  |> List.exists (function
         | Syn.Cst.RowField.Inherit _ ->
             true
         | Syn.Cst.RowField.Tag _ ->
             false)

let render_type_constraint (constraint_ : Syn.Cst.type_constraint) =
  Doc.concat
    [
      kw_constraint;
      Doc.space;
      render_core_type constraint_.left;
      equals;
      render_core_type constraint_.right;
    ]

let render_variant_constructor_arguments ?(prefer_multiline_inline_record = false) = function
  | Syn.Cst.ConstructorArguments.Tuple types ->
      Doc.group
        (join_map (Doc.concat [ Doc.space; star; Doc.break ~flat:" " () ]) render_core_type
           types)
  | Syn.Cst.ConstructorArguments.Record fields ->
      let source_node =
        match fields with
        | [] ->
            None
        | field :: _ ->
            Syn.Ceibo.Red.SyntaxNode.parent (Syn.Cst.RecordField.syntax_node field)
      in
      (match source_node with
      | Some source_node
        when
          syntax_node_has_comment_like_token source_node
          || (Syn.Ceibo.Red.SyntaxNode.tokens source_node
             |> List.exists whitespace_has_newline) ->
          Doc.indent 2 (render_record_definition_with_comments ~source_node fields)
      | Some source_node
        when prefer_multiline_inline_record ->
          Doc.indent 2 (render_record_definition_with_comments ~source_node fields)
      | Some _ ->
          Doc.indent 2 (render_record_definition fields)
      | None ->
          Doc.indent 2 (render_record_definition fields))

let render_variant_constructor
    ?(include_trailing_comment = true)
    ?(prefer_multiline_inline_record = false)
    constructor =
  let head =
    Doc.concat
      [
        Doc.bar;
        Doc.space;
        doc_of_token (Syn.Cst.VariantConstructor.constructor_name_token constructor);
      ]
  in
  let body =
    match
    Syn.Cst.VariantConstructor.arguments constructor,
    Syn.Cst.VariantConstructor.result_type constructor
  with
  | Some arguments, Some result_type ->
      let payload =
        render_variant_constructor_arguments
          ~prefer_multiline_inline_record arguments
      in
      Doc.concat [ head; Doc.space; Doc.colon; Doc.space; payload; arrow; render_core_type result_type ]
  | Some arguments, None ->
      Doc.concat
        [
          head;
          Doc.space;
          kw_of;
          Doc.space;
          render_variant_constructor_arguments
            ~prefer_multiline_inline_record arguments;
        ]
  | None, Some result_type ->
      Doc.concat [ head; Doc.space; Doc.colon; Doc.space; render_core_type result_type ]
  | None, None ->
      head
  in
  if include_trailing_comment then
    match trailing_comment_suffix_doc (Syn.Cst.VariantConstructor.syntax_node constructor) with
    | None ->
        body
    | Some suffix ->
        Doc.concat [ body; suffix ]
  else
    body

let render_variant_definition ~source_node constructors =
  let constructors_all_inline_records =
    constructors != []
    && List.for_all
         (fun constructor ->
           match Syn.Cst.VariantConstructor.arguments constructor with
           | Some (Syn.Cst.ConstructorArguments.Record _) ->
               true
           | _ ->
               false)
         constructors
  in
  let constructor_count = List.length constructors in
  let constructor_docs =
    constructors
    |> List.mapi (fun index constructor ->
           render_variant_constructor
             ~prefer_multiline_inline_record:constructors_all_inline_records
             ~include_trailing_comment:(index < constructor_count - 1)
             constructor)
  in
  render_interleaved_node_docs ~source_node
    ~should_consume_node:(fun node ->
      Syn.Ceibo.Red.SyntaxNode.kind node = Syn.SyntaxKind.TYPE_VARIANT_CONSTR)
    ~node_docs:constructor_docs
  |> Doc.join Doc.line

let render_type_definition = function
  | Syn.Cst.TypeDefinition.Abstract ->
      None
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      Some (render_core_type manifest)
  | Syn.Cst.TypeDefinition.Record { syntax_node; fields } ->
      Some (render_record_definition_with_comments ~source_node:syntax_node fields)
  | Syn.Cst.TypeDefinition.Variant { syntax_node; constructors } ->
      Some (render_variant_definition ~source_node:syntax_node constructors)
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant ->
      (match Syn.Cst.PolyVariant.kind poly_variant with
      | Syn.Cst.PolyVariantBound.Exact when not (poly_variant_has_inherit_field poly_variant) ->
          let fields =
            Syn.Cst.PolyVariant.fields poly_variant
            |> List.map (fun field ->
                   Doc.concat [ Doc.bar; Doc.space; render_poly_variant_field field ])
          in
          Some
            (Doc.concat
               [
                 Doc.indent 2 (Doc.concat [ Doc.lbracket; Doc.line; Doc.join Doc.line fields ]);
                 Doc.line;
                 Doc.rbracket;
               ])
      | Syn.Cst.PolyVariantBound.Exact
      | Syn.Cst.PolyVariantBound.UpperBound _
      | Syn.Cst.PolyVariantBound.LowerBound _ ->
          Some (render_poly_variant_type poly_variant))
  | Syn.Cst.TypeDefinition.Extensible _ ->
      Some (Doc.text "..")
  | Syn.Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
      Some
        (Doc.concat
           [ Doc.lparen; Doc.text "module"; Doc.space; doc_of_module_type module_type; Doc.rparen ])
  | Syn.Cst.TypeDefinition.Object { fields; _ } ->
      Some (render_object_type fields)

type type_definition_layout =
  | Inline_definition
  | Inline_opening_definition
  | Broken_definition
  | Broken_definition_no_outer_indent

let type_definition_layout decl =
  match Syn.Cst.TypeDeclaration.type_definition decl with
  | Syn.Cst.TypeDefinition.Record _
  | Syn.Cst.TypeDefinition.Object _ ->
      Inline_opening_definition
  | Syn.Cst.TypeDefinition.PolyVariant poly_variant ->
      (match Syn.Cst.PolyVariant.kind poly_variant with
      | Syn.Cst.PolyVariantBound.Exact ->
          if poly_variant_has_inherit_field poly_variant then
            Inline_opening_definition
          else
            Broken_definition_no_outer_indent
      | Syn.Cst.PolyVariantBound.UpperBound _
      | Syn.Cst.PolyVariantBound.LowerBound _ ->
          Inline_opening_definition)
  | Syn.Cst.TypeDefinition.Variant _ ->
      Broken_definition
  | Syn.Cst.TypeDefinition.Alias { manifest; _ } ->
      if core_type_prefers_multiline manifest then
        Broken_definition
      else
        Inline_definition
  | Syn.Cst.TypeDefinition.FirstClassModule _
  | Syn.Cst.TypeDefinition.Extensible _ ->
      Inline_definition
  | Syn.Cst.TypeDefinition.Abstract ->
      Inline_definition

let render_type_declaration_with_keyword keyword decl =
  let type_name =
    Syn.Cst.TypeDeclaration.type_name decl
  in
  let type_definition =
    Syn.Cst.TypeDeclaration.type_definition decl
  in
  let params = render_type_parameters (Syn.Cst.TypeDeclaration.type_params decl) in
  let header =
    if params = Doc.empty then
      Doc.concat [ keyword; Doc.space; doc_of_ident type_name ]
    else
      Doc.concat
        [
          keyword;
          Doc.space;
          params;
          Doc.space;
          doc_of_ident type_name;
        ]
  in
  let header =
    header
  in
  let definition =
    match Syn.Cst.TypeDeclaration.private_flag decl with
    | Syn.Cst.PrivateFlag.Public ->
        render_type_definition type_definition
    | Syn.Cst.PrivateFlag.Private _ ->
        Option.map
          (fun definition -> Doc.concat [ kw_private; Doc.space; definition ])
          (render_type_definition type_definition)
  in
  let with_definition =
    match definition with
    | None ->
        header
    | Some definition ->
        (match type_definition_layout decl with
        | Inline_definition ->
            Doc.concat [ header; equals; definition ]
        | Inline_opening_definition ->
            Doc.concat [ header; Doc.space; Doc.equal; Doc.space; definition ]
        | Broken_definition ->
            Doc.concat [ header; Doc.space; Doc.equal; Doc.line; Doc.indent 2 definition ]
        | Broken_definition_no_outer_indent ->
            Doc.concat [ header; Doc.space; Doc.equal; Doc.line; definition ])
  in
  let with_constraints =
    Syn.Cst.TypeDeclaration.constraints decl
    |> List.fold_left
         (fun acc constraint_ ->
           Doc.concat [ acc; Doc.line; Doc.indent 2 (render_type_constraint constraint_) ])
         with_definition
  in
  with_constraints

let render_type_mutual_declaration decl =
  match Syn.Cst.TypeMutualDeclaration.declarations decl with
  | [] ->
      Doc.empty
  | first :: rest ->
      Doc.join blank_line
        (render_type_declaration_with_keyword kw_type first
        :: List.map (render_type_declaration_with_keyword kw_and) rest)

let render_external_declaration (decl : Syn.Cst.external_declaration) =
  let primitive_names =
    decl.primitive_name_tokens |> List.map doc_of_token |> Doc.join Doc.space
  in
  Doc.concat
    [
      kw_external;
      Doc.space;
      doc_of_token decl.name_token;
      Doc.space;
      Doc.colon;
      Doc.space;
      render_core_type decl.type_;
      Doc.space;
      Doc.equal;
      Doc.space;
      primitive_names;
    ]

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
  | Syn.Cst.Pattern.List { syntax_node; elements; _ } ->
      if elements = [] then
        Doc.concat [ Doc.lbracket; Doc.rbracket ]
      else
        let edge_space =
          if List.length elements = 1 then
            let text = text_of_syntax_node syntax_node in
            if
              string_contains_substring text "[ "
              || string_contains_substring text " ]"
            then
              " "
            else
              ""
          else
            ""
        in
        Doc.group
          (Doc.concat
             [
               Doc.lbracket;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:edge_space ();
                      join_map
                        (Doc.concat [ Doc.semi; Doc.break ~flat:edge_space () ])
                        render_pattern
                        elements;
                    ]);
               Doc.break ~flat:edge_space ();
               Doc.rbracket;
             ])
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

let rec expression_needs_parens_in_constructor = function
  | Syn.Cst.Expression.Parenthesized
      { inner = Syn.Cst.Expression.PolyVariant { payload = Some _; _ }; _ } ->
      true
  | Syn.Cst.Expression.Parenthesized _ ->
      false
  | Syn.Cst.Expression.PolyVariant { payload = Some _; _ } ->
      true
  | expression ->
      expression_needs_parens_in_apply expression

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
  | Syn.Cst.Expression.If if_ ->
      if_prefers_multiline_layout if_
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

and if_prefers_multiline_layout ({ condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  let else_prefers_multiline =
    match else_branch with
    | Some (Syn.Cst.Expression.If _) ->
        false
    | Some else_branch ->
        branch_prefers_multiline_layout else_branch
    | None ->
        false
  in
  expression_prefers_multiline_layout condition
  || branch_prefers_multiline_layout then_branch
  || else_prefers_multiline

and branch_prefers_multiline_layout = function
  | Syn.Cst.Expression.If if_ ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_prefers_multiline_layout inner
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

let case_body_prefers_multiline ({ body; _ } : Syn.Cst.match_case) =
  expression_prefers_multiline_layout body

let rec function_body_prefers_multiline = function
  | Syn.Cst.Expression.If if_ ->
      if_prefers_multiline_layout if_
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.BeginEnd; _ } ->
      true
  | Syn.Cst.Expression.Function { cases; _ } ->
      List.exists case_body_prefers_multiline cases
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Expression body; _ } ->
      function_body_prefers_multiline body
  | Syn.Cst.Expression.Fun { body = Syn.Cst.Cases _; _ } ->
      false
  | Syn.Cst.Expression.Apply apply ->
      qualified_multi_argument_apply_prefers_multiline apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      function_body_prefers_multiline inner
  | _ ->
      false

and qualified_multi_argument_apply_prefers_multiline
    ({ callee; argument; _ } : Syn.Cst.apply_expression) =
  let rec argument_count count = function
    | Syn.Cst.Expression.Apply { callee; _ } ->
        argument_count (count + 1) callee
    | _ ->
        count
  in
  let rec head_is_qualified_path = function
    | Syn.Cst.Expression.Apply { callee; _ } ->
        head_is_qualified_path callee
    | Syn.Cst.Expression.Path { path; _ } ->
        List.length (Syn.Cst.Ident.segments path) > 1
    | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
        (match receiver with
        | Syn.Cst.Expression.Path _ | Syn.Cst.Expression.FieldAccess _ ->
            true
        | _ ->
            false)
    | _ ->
        false
  in
  let rec has_non_positional_argument acc = function
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        let acc =
          acc
          ||
          match argument with
          | Syn.Cst.Positional _ ->
              false
          | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
              true
        in
        has_non_positional_argument acc callee
    | _ ->
        acc
  in
  let acc =
    match argument with
    | Syn.Cst.Positional _ ->
        false
    | Syn.Cst.Labeled _ | Syn.Cst.Optional _ ->
        true
  in
  argument_count 1 callee > 1
  && head_is_qualified_path callee
  && not (has_non_positional_argument acc callee)

let rec expression_keeps_inline_binding_value = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_keeps_inline_binding_value inner
  | _ ->
      false

let tuple_source_is_long syntax_node =
  let source = text_of_syntax_node syntax_node |> String.trim in
  normalized_source_length source > 100

let rec expression_is_pipeline = function
  | Syn.Cst.Expression.Infix { operator_token; left; right; _ } ->
      token_text operator_token = "|>"
      || expression_is_pipeline left
      || expression_is_pipeline right
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_pipeline inner
  | _ ->
      false

let rec expression_is_boolean_infix = function
  | Syn.Cst.Expression.Infix { operator_token; left; right; _ } ->
      let operator = token_text operator_token in
      operator = "&&"
      || operator = "||"
      || expression_is_boolean_infix left
      || expression_is_boolean_infix right
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_boolean_infix inner
  | _ ->
      false

let rec find_boolean_infix_expression = function
  | Syn.Cst.Expression.Infix ({ operator_token; _ } as infix) ->
      let operator = token_text operator_token in
      if operator = "&&" || operator = "||" then
        Some infix
      else
        None
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      find_boolean_infix_expression inner
  | Syn.Cst.Expression.Prefix { operand; _ } ->
      find_boolean_infix_expression operand
  | Syn.Cst.Expression.Apply { callee; argument; _ } -> (
      match argument with
      | Syn.Cst.Positional value ->
          (match find_boolean_infix_expression value with
          | Some infix ->
              Some infix
          | None ->
              find_boolean_infix_expression callee)
      | Syn.Cst.Labeled { value = Some value; _ }
      | Syn.Cst.Optional { value = Some value; _ } ->
          (match find_boolean_infix_expression value with
          | Some infix ->
              Some infix
          | None ->
              find_boolean_infix_expression callee)
      | Syn.Cst.Labeled { value = None; _ }
      | Syn.Cst.Optional { value = None; _ } ->
          find_boolean_infix_expression callee)
  | _ ->
      None

let rec expression_is_function_like = function
  | Syn.Cst.Expression.Function _ ->
      true
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_function_like inner
  | _ ->
      false

let rec unwrap_parenthesized_expression = function
  | Syn.Cst.Expression.Parenthesized { grouping = Syn.Cst.Parens; inner; _ } ->
      unwrap_parenthesized_expression inner
  | expression ->
      expression

let rec infix_chain_term_count = function
  | Syn.Cst.Expression.Infix { left; right; _ } ->
      infix_chain_term_count left + infix_chain_term_count right
  | _ ->
      1

let rec expression_is_simple_after_equals = function
  | Syn.Cst.Expression.Infix infix ->
      infix_chain_term_count (Syn.Cst.Expression.Infix infix) <= 8
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.Constructor _
  | Syn.Cst.Expression.PolyVariant _
  | Syn.Cst.Expression.Prefix _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Index _
  | Syn.Cst.Expression.Coerce _
  | Syn.Cst.Expression.Typed _
  | Syn.Cst.Expression.MethodCall _
  | Syn.Cst.Expression.New _
  | Syn.Cst.Expression.LocalOpen _ ->
      true
  | Syn.Cst.Expression.Apply apply ->
      apply_expression_is_simple_after_equals apply
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      expression_is_simple_after_equals inner
  | _ ->
      false

and apply_expression_is_simple_after_equals
    ({ syntax_node; _ } : Syn.Cst.apply_expression) =
  if syntax_node_has_comment_like_token syntax_node then
    false
  else
    let source = text_of_syntax_node syntax_node |> String.trim in
    not
      (string_contains_substring source "function"
      || string_contains_substring source "match "
      || string_contains_substring source "if "
      || string_contains_substring source "try "
      || string_contains_substring source "while "
      || string_contains_substring source "for ")

let expression_requires_break_after_equals = function
  | Syn.Cst.Expression.Function _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.While _
  | Syn.Cst.Expression.For _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.LetModule _
  | Syn.Cst.Expression.LocalOpen _ ->
      true
  | _ ->
      false

let expression_source_is_long expression =
  let source = text_of_syntax_node (Syn.Cst.Expression.syntax_node expression) in
  normalized_source_length source > 100

let expression_source_has_newline expression =
  let source = text_of_syntax_node (Syn.Cst.Expression.syntax_node expression) in
  string_contains_substring source "\n" || string_contains_substring source "\r\n"

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
            if expression_needs_parens_in_constructor payload then
              Doc.concat [ Doc.lparen; render_expression payload; Doc.rparen ]
            else
              render_expression payload
          in
          Doc.concat [ head; Doc.space; payload ])
  | Syn.Cst.Expression.Operator { operator_tokens; _ } ->
      operator_tokens |> List.map token_text |> String.concat "" |> Doc.text
  | Syn.Cst.Expression.Tuple { elements; _ } ->
      let rendered_elements = List.map render_expression elements in
      let prefers_multiline =
        tuple_source_is_long (Syn.Cst.Expression.syntax_node expression)
        || List.exists Doc.is_multiline rendered_elements
      in
      if prefers_multiline then
        let lines = join_map (Doc.concat [ Doc.comma; Doc.line ]) render_expression elements in
        Doc.concat
          [
            Doc.lparen;
            Doc.line;
            Doc.indent 2 lines;
            Doc.line;
            Doc.rparen;
          ]
      else
        Doc.group
          (Doc.concat
             [
               Doc.lparen;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:"" ();
                      join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression
                        elements;
                    ]);
               Doc.break ~flat:"" ();
               Doc.rparen;
             ])
  | Syn.Cst.Expression.List { elements; _ } ->
      if elements = [] then
        Doc.concat [ Doc.lbracket; Doc.rbracket ]
      else
        Doc.group
          (Doc.concat
             [
               Doc.lbracket;
               Doc.indent 2
                 (Doc.concat
                    [
                      Doc.break ~flat:" " ();
                      join_map (Doc.concat [ Doc.semi; Doc.break () ]) render_expression
                        elements;
                    ]);
               Doc.break ~flat:" " ();
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
  | Syn.Cst.Expression.Infix infix ->
      render_infix_expression infix
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
  | Syn.Cst.Expression.Index index ->
      render_index_expression index
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

and render_index_expression ({ syntax_node; collection; index; _ } : Syn.Cst.index_expression) =
  let collection_doc =
    match collection with
    | Syn.Cst.Expression.If _
    | Syn.Cst.Expression.Match _
    | Syn.Cst.Expression.Try _
    | Syn.Cst.Expression.Let _
    | Syn.Cst.Expression.Sequence _
    | Syn.Cst.Expression.Fun _
    | Syn.Cst.Expression.Function _ ->
        Doc.concat [ Doc.lparen; render_expression collection; Doc.rparen ]
    | _ ->
        render_expression collection
  in
  let punct =
    nontrivia_direct_tokens syntax_node
    |> List.map Syn.Ceibo.Red.SyntaxToken.text
  in
  match punct with
  | [ dot; left_delim; right_delim ] ->
      Doc.concat
        [
          collection_doc;
          Doc.text dot;
          Doc.text left_delim;
          render_expression index;
          Doc.text right_delim;
        ]
  | _ ->
      Doc.concat
        [
          collection_doc;
          Doc.text ".(";
          render_expression index;
          Doc.rparen;
        ]

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

and render_tuple_expression_bare elements =
  Doc.group
    (join_map (Doc.concat [ Doc.comma; Doc.break () ]) render_expression elements)

and render_apply_argument = function
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Function _ as expression) ->
      Doc.concat
        [
          Doc.lparen;
          Doc.line;
          Doc.indent 2 (render_expression expression);
          Doc.line;
          Doc.rparen;
        ]
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

and syntax_node_of_apply_argument = function
  | Syn.Cst.Positional expression ->
      Syn.Cst.Expression.syntax_node expression
  | Syn.Cst.Labeled argument ->
      argument.syntax_node
  | Syn.Cst.Optional argument ->
      argument.syntax_node

and apply_argument_prefers_break = function
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Parenthesized
        {
          inner =
            ( Syn.Cst.Expression.If _
            | Syn.Cst.Expression.Match _
            | Syn.Cst.Expression.Try _
            | Syn.Cst.Expression.Let _
            | Syn.Cst.Expression.Sequence _ );
          _;
        }) ->
      true
  | Syn.Cst.Positional
      (Syn.Cst.Expression.Parenthesized
        { inner = Syn.Cst.Expression.Infix { operator_token; _ }; _ }) ->
      token_text operator_token = "|>"
  | Syn.Cst.Positional expression ->
      expression_prefers_multiline_layout expression
  | Syn.Cst.Labeled { value = Some value; _ }
  | Syn.Cst.Optional { value = Some value; _ } ->
      expression_prefers_multiline_layout value
  | _ ->
      false

and render_infix_expression ({ syntax_node; left; operator_token; right; _ } :
      Syn.Cst.infix_expression) =
  if List.exists is_comment_like_token (Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node) then
    let left_offset = Syn.Ceibo.Red.SyntaxNode.offset (Syn.Cst.Expression.syntax_node left) in
    let right_offset = Syn.Ceibo.Red.SyntaxNode.offset (Syn.Cst.Expression.syntax_node right) in
    let rec loop acc separator = function
      | [] ->
          Option.unwrap_or acc ~default:Doc.empty
      | child :: rest -> (
          match child with
          | Syn.Ceibo.Red.Token syntax_token when is_whitespace_token syntax_token ->
              loop acc (separator_of_whitespace_token syntax_token) rest
          | Syn.Ceibo.Red.Token syntax_token when is_comment_like_token syntax_token ->
              let doc = Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token) in
              let acc =
                match acc with
                | None ->
                    Some doc
                | Some current ->
                    Some (Doc.concat [ current; separator; doc ])
              in
              loop acc Doc.empty rest
          | Syn.Ceibo.Red.Token syntax_token ->
              let doc = Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token) in
              let acc =
                match acc with
                | None ->
                    Some doc
                | Some current ->
                    Some (Doc.concat [ current; separator; doc ])
              in
              loop acc Doc.empty rest
          | Syn.Ceibo.Red.Node node ->
              let node_offset = Syn.Ceibo.Red.SyntaxNode.offset node in
              let doc =
                if node_offset = left_offset then
                  render_expression left
                else if node_offset = right_offset then
                  render_expression right
                else
                  doc_of_verbatim_syntax_node node
              in
              let acc =
                match acc with
                | None ->
                    Some doc
                | Some current ->
                    Some (Doc.concat [ current; separator; doc ])
              in
              loop acc Doc.empty rest)
    in
    loop None Doc.empty (Syn.Ceibo.Red.SyntaxNode.children_list syntax_node)
  else
    let operator = token_text operator_token in
    let parts =
      infix_chain operator
        (Syn.Cst.Expression.Infix { syntax_node; left; operator_token; right; attributes = [] })
    in
    Doc.group
      (join_map
         (Doc.concat [ Doc.break (); Doc.text operator; Doc.space ])
         render_expression parts)

and render_apply_expression ({ syntax_node; callee; argument; _ } : Syn.Cst.apply_expression) =
  if List.exists is_comment_like_token (Syn.Ceibo.Red.SyntaxNode.direct_tokens syntax_node) then
    let callee_offset = Syn.Ceibo.Red.SyntaxNode.offset (Syn.Cst.Expression.syntax_node callee) in
    let argument_offset = Syn.Ceibo.Red.SyntaxNode.offset (syntax_node_of_apply_argument argument) in
    let rec loop acc separator = function
      | [] ->
          Option.unwrap_or acc ~default:Doc.empty
      | child :: rest -> (
          match child with
          | Syn.Ceibo.Red.Token syntax_token when is_whitespace_token syntax_token ->
              loop acc (separator_of_whitespace_token syntax_token) rest
          | Syn.Ceibo.Red.Token syntax_token when is_comment_like_token syntax_token ->
              let doc = Doc.text (Syn.Ceibo.Red.SyntaxToken.text syntax_token) in
              let acc =
                match acc with
                | None ->
                    Some doc
                | Some current ->
                    Some (Doc.concat [ current; separator; doc ])
              in
              loop acc Doc.empty rest
          | Syn.Ceibo.Red.Token _ ->
              loop acc separator rest
          | Syn.Ceibo.Red.Node node ->
              let node_offset = Syn.Ceibo.Red.SyntaxNode.offset node in
              let doc =
                if node_offset = callee_offset then
                  render_expression callee
                else if node_offset = argument_offset then
                  render_apply_argument argument
                else
                  doc_of_verbatim_syntax_node node
              in
              let acc =
                match acc with
                | None ->
                    Some doc
                | Some current ->
                    Some (Doc.concat [ current; separator; doc ])
              in
              loop acc Doc.empty rest)
    in
    loop None Doc.empty (Syn.Ceibo.Red.SyntaxNode.children_list syntax_node)
  else
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
  let rendered_argument_pairs = List.combine arguments rendered_arguments in
  if
    List.exists
      (fun (argument, doc) -> apply_argument_prefers_break argument || Doc.is_multiline doc)
      rendered_argument_pairs
  then
    let rec split_inline_prefix acc = function
      | (argument, doc) :: rest
        when not (apply_argument_prefers_break argument) && not (Doc.is_multiline doc) ->
          split_inline_prefix (doc :: acc) rest
      | rest ->
          (List.rev acc, rest)
    in
    let inline_arguments, multiline_arguments = split_inline_prefix [] rendered_argument_pairs in
    let head_with_inline_arguments =
      Doc.concat
        (rendered_head
        :: List.map
             (fun argument -> Doc.concat [ Doc.space; argument ])
             inline_arguments)
    in
    (match multiline_arguments with
    | [] ->
        head_with_inline_arguments
    | multiline_arguments ->
        Doc.concat
          [
            head_with_inline_arguments;
            Doc.line;
            Doc.indent 2
              (multiline_arguments
              |> List.map snd
              |> Doc.join Doc.line);
          ])
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

and render_case ?(force_multiline_body = false) ?(force_leading_bar = false)
    (case : Syn.Cst.match_case) =
  let body = render_expression case.body in
  let prefix =
    match case.bar_token with
    | Some token ->
        Doc.concat [ doc_of_token token; Doc.space ]
    | None when force_leading_bar ->
        Doc.concat [ Doc.bar; Doc.space ]
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
    | _
      when
        force_multiline_body
        || Doc.is_multiline body
        || expression_prefers_multiline_layout case.body
        ||
        let case_source = text_of_syntax_node case.syntax_node in
        string_contains_substring case_source "->\n"
        || string_contains_substring case_source "->\r\n" ->
      Doc.concat
        [
          prefix;
          pattern;
          guard;
          Doc.space;
          doc_of_token case.arrow_token;
          Doc.line;
          Doc.indent 4 body;
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
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
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
        join_map Doc.line (render_case ~force_multiline_body:force_multiline_cases) cases;
      ]
  else
    Doc.concat
      [
        head;
        Doc.space;
        doc_of_token with_token;
        Doc.line;
        join_map Doc.line (render_case ~force_multiline_body:force_multiline_cases) cases;
      ]

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
    ({ keyword_token; arrow_token; parameters = _; body = _; _ } as fun_ :
      Syn.Cst.fun_expression) =
  let parameters, body = flatten_fun_expression fun_ in
  let parameters = parameters |> List.map render_parameter in
  let has_multiline_parameter = List.exists Doc.is_multiline parameters in
  let body = render_fun_body body in
  let body_prefers_multiline =
    match body with
    | _ when Doc.is_multiline body ->
        true
    | _ ->
        (match flatten_fun_expression fun_ |> snd with
        | Syn.Cst.Expression expression ->
            function_body_prefers_multiline expression
        | Syn.Cst.Cases _ ->
            true)
  in
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
  else if body_prefers_multiline || List.length parameters = 0 then
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

and render_function_expression ({ syntax_node; keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  let source_has_newline = syntax_node_has_internal_newline syntax_node in
  if source_has_newline then
    Doc.concat
      [
        doc_of_token keyword_token;
        Doc.line;
        join_map Doc.line
          (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
          cases;
      ]
  else
    Doc.group
      (Doc.concat
         [
           doc_of_token keyword_token;
           Doc.space;
           join_map
             (Doc.concat [ Doc.space ])
             (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
             cases;
         ])

and render_function_expression_inline
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      Doc.indent 2
        (join_map Doc.line
           (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
           cases);
    ]

and render_function_expression_unindented
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      join_map Doc.line
        (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
        cases;
    ]

and render_function_expression_indented
    ({ keyword_token; cases; _ } : Syn.Cst.function_expression) =
  let force_multiline_cases =
    List.length cases > 2 && List.exists case_body_prefers_multiline cases
  in
  Doc.concat
    [
      doc_of_token keyword_token;
      Doc.line;
      Doc.indent 2
        (join_map Doc.line
           (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
           cases);
    ]

and render_fun_body = function
  | Syn.Cst.Expression (Syn.Cst.Expression.Tuple { elements; _ }) ->
      render_tuple_expression_bare elements
  | Syn.Cst.Expression body ->
      if expression_prefers_multiline_layout body then
        render_block_expression body
      else
        render_expression body
  | Syn.Cst.Cases { cases; _ } ->
      let force_multiline_cases =
        List.length cases > 2 && List.exists case_body_prefers_multiline cases
      in
      Doc.concat
        [
          kw_function;
          Doc.line;
          join_map Doc.line
            (render_case ~force_multiline_body:force_multiline_cases ~force_leading_bar:true)
            cases;
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
      render_function_expression_unindented function_
  | Syn.Cst.Expression.Fun fun_ ->
      render_fun_expression fun_
  | Syn.Cst.Expression.Parenthesized _ as expression ->
      render_parenthesized_expression expression
  | expression ->
      render_expression expression

and render_if_expression_block
    ({ keyword_token; then_token; else_token; condition; then_branch; else_branch; _ } :
      Syn.Cst.if_expression) =
  let render_condition_with_boolean_breaks condition =
    let syntax_node = Syn.Cst.Expression.syntax_node condition in
    let tokens = Syn.Ceibo.Red.SyntaxNode.tokens syntax_node in
    let has_boolean_operator =
      List.exists
        (fun token ->
          let text = Syn.Ceibo.Red.SyntaxToken.text token in
          text = "&&" || text = "||")
        tokens
    in
    let has_comment_like =
      List.exists
        (fun token ->
          match Syn.Ceibo.Red.SyntaxToken.kind token with
          | Syn.SyntaxKind.COMMENT
          | Syn.SyntaxKind.DOCSTRING ->
              true
          | _ ->
              false)
        tokens
    in
    if (not has_boolean_operator) || has_comment_like then
      render_expression condition
    else
      let rec loop acc pending_space = function
        | [] ->
            List.rev acc
        | token :: rest ->
            let kind = Syn.Ceibo.Red.SyntaxToken.kind token in
            let text = Syn.Ceibo.Red.SyntaxToken.text token in
            (match kind with
            | Syn.SyntaxKind.WHITESPACE ->
                loop acc true rest
            | _ when text = "&&" || text = "||" ->
                let rec drop_leading_whitespace = function
                  | next :: tail
                    when Syn.Ceibo.Red.SyntaxToken.kind next = Syn.SyntaxKind.WHITESPACE ->
                      drop_leading_whitespace tail
                  | remaining ->
                      remaining
                in
                loop
                  (Doc.space :: Doc.text text :: Doc.break () :: acc)
                  false
                  (drop_leading_whitespace rest)
            | _ ->
                let acc =
                  if pending_space then
                    Doc.text text :: Doc.space :: acc
                  else
                    Doc.text text :: acc
                in
                loop acc false rest)
      in
      Doc.concat (loop [] false tokens)
  in
  let condition_doc =
    render_condition_with_boolean_breaks condition
  in
  let then_doc =
    if branch_prefers_multiline_layout then_branch then
      render_block_expression then_branch
    else
      render_expression then_branch
  in
  let head =
    Doc.group
      (Doc.concat
         [
           doc_of_token keyword_token;
           Doc.indent 2 (Doc.concat [ Doc.break (); condition_doc ]);
           Doc.break ();
           doc_of_token then_token;
         ])
  in
  match else_branch, else_token with
  | None, _ -> (
      match then_branch with
      | Syn.Cst.Expression.Sequence { expressions = first :: rest; separator_token; _ } when not (rest = []) ->
          let first_doc =
            Doc.concat [ head; Doc.line; Doc.indent 2 (render_expression first); doc_of_token separator_token ]
          in
          let tail_doc =
            rest
            |> List.mapi (fun index expression ->
                   let suffix =
                     if index < List.length rest - 1 then
                       doc_of_token separator_token
                     else
                       Doc.empty
                   in
                   Doc.concat [ render_expression expression; suffix ])
            |> Doc.join Doc.line
          in
          Doc.concat [ first_doc; Doc.line; tail_doc ]
      | _ ->
          Doc.concat [ head; Doc.line; Doc.indent 2 then_doc ]
    )
  | Some (Syn.Cst.Expression.If nested_if), Some else_token ->
      Doc.concat
        [
          head;
          Doc.line;
          Doc.indent 2 then_doc;
          Doc.line;
          doc_of_token else_token;
          Doc.space;
          render_if_expression_block nested_if;
        ]
      | Some else_branch, Some else_token ->
      let else_doc =
        if branch_prefers_multiline_layout else_branch then
          render_block_expression else_branch
        else
          render_expression else_branch
      in
      (match else_branch with
      | Syn.Cst.Expression.Parenthesized { inner = Syn.Cst.Expression.Sequence _; _ } ->
          Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.space;
              else_doc;
            ]
      | _ ->
          Doc.concat
            [
              head;
              Doc.line;
              Doc.indent 2 then_doc;
              Doc.line;
              doc_of_token else_token;
              Doc.line;
              Doc.indent 2 else_doc;
            ])
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
          | _ when expression_is_function_like inner ->
              Doc.concat
                [
                  doc_of_token opening_token;
                  Doc.line;
                  Doc.indent 2 rendered_inner;
                  Doc.line;
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

and render_sequence_expression ({ separator_token; expressions; _ } : Syn.Cst.sequence_expression) =
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
  let pattern =
    match pattern with
    | Syn.Cst.Pattern.Tuple { elements; _ } ->
        join_map (Doc.concat [ Doc.comma; Doc.space ])
          (fun (element : Syn.Cst.tuple_pattern_element) ->
            match element.label_token with
            | None ->
                render_pattern element.pattern
            | Some label_token ->
                Doc.concat [ doc_of_token label_token; render_pattern element.pattern ])
          elements
    | _ ->
        render_pattern pattern
  in
  Doc.concat ([ doc_of_token keyword_token ] @ rec_part @ [ Doc.space; pattern ])

and split_typed_binding_value = function
  | Syn.Cst.Expression.Typed { expression; type_; _ } ->
      (expression, Some type_)
  | expression ->
      (expression, None)

and render_binding_value ~force_multiline_body ~parameters ~value =
  match parameters with
  | [] ->
      (match value with
      | Syn.Cst.Expression.Fun ({ keyword_token; arrow_token; _ } as fun_)
        when force_multiline_body ->
          let parameters, body = flatten_fun_expression fun_ in
          let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
          let body = render_fun_body body in
          Doc.concat
            [
              doc_of_token keyword_token;
              (if parameters = Doc.empty then Doc.empty else Doc.concat [ Doc.space; parameters ]);
              Doc.space;
              doc_of_token arrow_token;
              Doc.line;
              Doc.indent 2 body;
            ]
      | _ ->
          if expression_requires_break_after_equals value then
            render_block_expression value
          else
            render_expression value)
  | parameters ->
      let parameters = parameters |> List.map render_parameter |> Doc.join Doc.space in
      let has_multiline_parameters = Doc.is_multiline parameters in
      let body =
        match value with
        | Syn.Cst.Expression.Tuple { elements; _ } ->
            render_tuple_expression_bare elements
        | _ ->
            render_expression value
      in
      if has_multiline_parameters then
        Doc.concat
          [
            kw_fun;
            Doc.line;
            Doc.indent 2 (Doc.concat [ parameters; Doc.space; Doc.arrow ]);
            Doc.line;
            Doc.indent 2 body;
          ]
      else if force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body then
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

and render_binding_value_with_parameter_doc ~force_multiline_body ~parameter_doc ~value =
  let body =
    match value with
    | Syn.Cst.Expression.Tuple { elements; _ } ->
        render_tuple_expression_bare elements
    | _ ->
        render_expression value
  in
  if force_multiline_body || function_body_prefers_multiline value || Doc.is_multiline body then
    Doc.concat
      [
        kw_fun;
        Doc.space;
        parameter_doc;
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
           parameter_doc;
           Doc.space;
           Doc.arrow;
           Doc.indent 2 (Doc.concat [ Doc.break (); body ]);
         ])

and render_local_binding
    ~local_context
    ~keyword_token ~rec_token ~equals_token ~pattern ~parameters ~value =
  let value, type_annotation = split_typed_binding_value value in
  let header = render_binding_header ~keyword_token ~rec_token pattern in
  let header =
    match type_annotation with
    | None ->
        header
    | Some type_ ->
        Doc.concat [ header; colon; render_core_type type_ ]
  in
  let force_multiline_body =
    local_context
    &&
    Option.is_some rec_token
    &&
    (List.length parameters > 0
    || expression_prefers_multiline_layout value
    ||
    match value with
    | Syn.Cst.Expression.Fun _ ->
        true
    | _ ->
        false)
  in
  let keep_value_after_equals =
    let has_fun_rhs = List.length parameters > 0 || match value with Syn.Cst.Expression.Fun _ -> true | _ -> false in
    match value with
    | _ when has_fun_rhs ->
        true
    | _ when expression_is_boolean_infix value ->
        false
    | _ when expression_requires_break_after_equals value ->
        false
    | _ ->
        expression_is_simple_after_equals value || expression_keeps_inline_binding_value value
  in
  let rendered_value =
    match value with
    | Syn.Cst.Expression.Function function_
      when parameters = []
           && keep_value_after_equals
           && syntax_node_has_internal_newline function_.syntax_node ->
        render_function_expression_indented function_
    | Syn.Cst.Expression.Function function_
      when parameters = [] && not keep_value_after_equals ->
        render_function_expression_unindented function_
    | _ ->
        render_binding_value ~force_multiline_body ~parameters ~value
  in
  let keep_value_after_equals =
    match value with
    | Syn.Cst.Expression.Fun _ ->
        keep_value_after_equals
    | _ when List.length parameters > 0 ->
        keep_value_after_equals
    | _ when expression_is_pipeline value && Doc.is_multiline rendered_value ->
        false
    | _ ->
        keep_value_after_equals
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
  else if
    syntax_node_has_internal_newline (Syn.Cst.Expression.syntax_node value)
    || not (expression_is_simple_after_equals value)
    || expression_prefers_multiline_layout value
    || Doc.is_multiline rendered_value
  then
    let rendered_value =
      match value with
      | Syn.Cst.Expression.Infix ({ operator_token; _ } as infix) ->
          let operator = token_text operator_token in
          let parts = infix_chain operator (Syn.Cst.Expression.Infix infix) in
          join_map
            (Doc.concat [ Doc.line; Doc.text operator; Doc.space ])
            render_expression
            parts
      | _ ->
          rendered_value
    in
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
    render_local_binding ~local_context:true ~keyword_token ~rec_token ~equals_token
      ~pattern:binding_pattern
      ~parameters
      ~value:bound_value
  in
  let and_bindings =
    and_bindings
    |> List.map (fun (binding : Syn.Cst.let_binding) ->
           render_local_binding ~local_context:true ~keyword_token:binding.keyword_token
             ~rec_token:binding.rec_token ~equals_token:binding.equals_token
             ~pattern:binding.binding_pattern ~parameters:binding.parameters
             ~value:binding.value)
  in
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
  render_local_binding ~local_context:false ~keyword_token:binding.keyword_token
    ~rec_token:binding.rec_token
    ~equals_token:binding.equals_token ~pattern:binding.binding_pattern
    ~parameters:binding.parameters ~value:binding.value

let render_let_binding (binding : Syn.Cst.let_binding) =
  render_let_binding_group_item binding

let nested_structure_items_from_syntax_nodes syntax_nodes =
  Syn.CstBuilder.structure_items_from_syntax_nodes syntax_nodes
  |> Result.expect
       ~msg:"module structure bodies should re-lift from successful syntax nodes"

let nested_signature_items_from_syntax_node syntax_node =
  Syn.CstBuilder.signature_items_from_syntax_node syntax_node
  |> Result.expect
       ~msg:"module signature bodies should re-lift from successful syntax nodes"

let rec render_module_type_constraint ~keyword (constraint_ : Syn.Cst.module_type_constraint) =
  let separator =
    if constraint_.is_destructive then
      Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
    else
      equals
  in
  Doc.concat
    [
      keyword;
      Doc.space;
      kw_type;
      Doc.space;
      render_core_type constraint_.constrained_type;
      separator;
      render_core_type constraint_.replacement_type;
    ]

and render_functor_parameter ({ name_token; module_type; _ } : Syn.Cst.functor_parameter) =
  Doc.concat
    [
      Doc.lparen;
      doc_of_token name_token;
      colon;
      render_module_type_doc module_type;
      Doc.rparen;
    ]

and render_module_type_doc = function
  | Syn.Cst.ModuleType.Path path ->
      doc_of_ident path
  | Syn.Cst.ModuleType.TypeOf { module_path; _ } ->
      Doc.concat [ Doc.text "module"; Doc.space; kw_type; Doc.space; Doc.text "of"; Doc.space; doc_of_ident module_path ]
  | Syn.Cst.ModuleType.Signature { syntax_node; signature_syntax_node } ->
      let items = nested_signature_items_from_syntax_node signature_syntax_node in
      let body = render_signature_items ~source_node:syntax_node items in
      Doc.concat
        [
          Doc.text "sig";
          Doc.line;
          Doc.indent 2 body;
          Doc.line;
          Doc.text "end";
        ]
  | Syn.Cst.ModuleType.Functor { parameters; result; _ } ->
      Doc.concat
        [
          Doc.text "functor";
          Doc.space;
          Doc.join Doc.space (List.map render_functor_parameter parameters);
          Doc.space;
          Doc.arrow;
          Doc.space;
          render_module_type_doc result;
        ]
  | Syn.Cst.ModuleType.With { base; constraints; _ } ->
      let first, rest =
        match constraints with
        | [] ->
            (Doc.empty, [])
        | first :: rest ->
            (render_module_type_constraint ~keyword:kw_with first, rest)
      in
      Doc.concat
        (render_module_type_doc base
        :: Doc.space
        :: first
        :: List.map (fun constraint_ ->
               Doc.concat
                 [
                   Doc.space;
                   render_module_type_constraint ~keyword:kw_and constraint_;
                 ])
             rest)
  | Syn.Cst.ModuleType.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_type_doc inner; Doc.rparen ]
  | Syn.Cst.ModuleType.Attribute { module_type; _ } ->
      render_module_type_doc module_type
  | module_type ->
      doc_of_node (Syn.Cst.ModuleType.syntax_node module_type)

and render_module_application_argument = function
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_expression_doc inner; Doc.rparen ]
  | argument ->
      Doc.concat [ Doc.lparen; render_module_expression_doc argument; Doc.rparen ]

and render_module_expression_doc = function
  | Syn.Cst.ModuleExpression.Path path ->
      doc_of_ident path
  | Syn.Cst.ModuleExpression.Structure { syntax_node; item_syntax_nodes } ->
      let items = nested_structure_items_from_syntax_nodes item_syntax_nodes in
      let body = render_structure_items ~source_node:syntax_node items in
      Doc.concat
        [
          Doc.text "struct";
          Doc.line;
          Doc.indent 2 body;
          Doc.line;
          Doc.text "end";
        ]
  | Syn.Cst.ModuleExpression.Functor { parameters; body; _ } ->
      Doc.concat
        [
          Doc.text "functor";
          Doc.space;
          Doc.join Doc.space (List.map render_functor_parameter parameters);
          Doc.space;
          Doc.arrow;
          Doc.space;
          render_module_expression_doc body;
        ]
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
      Doc.concat
        [
          render_module_expression_doc callee;
          Doc.space;
          render_module_application_argument argument;
        ]
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      Doc.concat [ render_module_expression_doc callee; Doc.space; Doc.lparen; Doc.rparen ]
  | Syn.Cst.ModuleExpression.Constraint { module_expression; module_type; _ } ->
      Doc.concat
        [
          render_module_expression_doc module_expression;
          colon;
          render_module_type_doc module_type;
        ]
  | Syn.Cst.ModuleExpression.ModuleUnpack { expression; module_type; _ } ->
      let constraint_doc =
        match module_type with
        | None ->
            Doc.empty
        | Some module_type ->
            Doc.concat [ colon; render_module_type_doc module_type ]
      in
      Doc.concat
        [
          Doc.lparen;
          Doc.text "val";
          Doc.space;
          render_expression expression;
          constraint_doc;
          Doc.rparen;
        ]
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      Doc.concat [ Doc.lparen; render_module_expression_doc inner; Doc.rparen ]
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } ->
      render_module_expression_doc module_expression
  | module_expression ->
      doc_of_node (Syn.Cst.ModuleExpression.syntax_node module_expression)

and render_module_declaration_with_keyword keyword_doc
    ({ module_name; functor_parameters; module_type; module_expression; is_destructive_substitution; _ } :
      Syn.Cst.ModuleDeclaration.t) =
  let header =
    Doc.concat
      [
        keyword_doc;
        Doc.space;
        doc_of_token module_name;
        (if functor_parameters = [] then
           Doc.empty
         else
           Doc.concat
             [
               Doc.space;
               Doc.join Doc.space (List.map render_functor_parameter functor_parameters);
             ]);
      ]
  in
  let header =
    match module_type with
    | None ->
        header
    | Some module_type ->
        Doc.concat [ header; colon; render_module_type_doc module_type ]
  in
  match module_expression with
  | None ->
      header
  | Some (Syn.Cst.ModuleExpression.Constraint { module_expression; _ })
    when Option.is_some module_type ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_expression_doc module_expression ]
  | Some module_expression ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_expression_doc module_expression ]

and render_recursive_module_declaration (decl : Syn.Cst.RecursiveModuleDeclaration.t) =
  match Syn.Cst.RecursiveModuleDeclaration.declarations decl with
  | [] ->
      Doc.empty
  | first :: rest ->
      Doc.join blank_line
        (render_module_declaration_with_keyword
           (Doc.concat [ Doc.text "module"; Doc.space; kw_rec ])
           first
        :: List.map (render_module_declaration_with_keyword kw_and) rest)

and render_module_type_declaration ({ module_type_name; module_type; is_destructive_substitution; _ } :
      Syn.Cst.ModuleTypeDeclaration.t) =
  let header =
    Doc.concat [ Doc.text "module"; Doc.space; kw_type; Doc.space; doc_of_token module_type_name ]
  in
  match module_type with
  | None ->
      header
  | Some module_type ->
      let separator =
        if is_destructive_substitution then
          Doc.concat [ Doc.space; Doc.text ":="; Doc.space ]
        else
          equals
      in
      Doc.concat [ header; separator; render_module_type_doc module_type ]

and render_open_target = function
  | Syn.Cst.OpenStatement.Path path ->
      doc_of_ident path
  | Syn.Cst.OpenStatement.ModuleExpression expression ->
      render_module_expression_doc expression

and render_include_statement ({ target; _ } : Syn.Cst.include_statement) =
  let target =
    match target with
    | Syn.Cst.ModuleExpression expression ->
        render_module_expression_doc expression
    | Syn.Cst.ModuleType module_type ->
        render_module_type_doc module_type
  in
  Doc.concat [ Doc.text "include"; Doc.space; target ]

and is_module_alias_structure_item = function
  | Syn.Cst.StructureItem.ModuleDeclaration
      { functor_parameters = []; module_type = None; module_expression = Some (Syn.Cst.ModuleExpression.Path _); _ } ->
      true
  | _ ->
      false

and is_open_structure_item = function
  | item when is_module_alias_structure_item item ->
      true
  | Syn.Cst.StructureItem.OpenStatement _ ->
      true
  | _ ->
      false

and is_open_signature_item = function
  | Syn.Cst.SignatureItem.OpenStatement _ ->
      true
  | _ ->
      false

and render_structure_entry item =
  let trailing_suffix = trailing_comment_suffix_doc (Syn.Cst.StructureItem.syntax_node item) in
  let doc =
    match trailing_suffix with
    | None ->
        render_structure_item item
    | Some suffix ->
        Doc.concat [ render_structure_item item; suffix ]
  in
  (doc, is_open_structure_item item, false, false, false, is_module_alias_structure_item item)

and render_signature_entry item =
  let trailing_suffix = trailing_comment_suffix_doc (Syn.Cst.SignatureItem.syntax_node item) in
  let doc =
    match trailing_suffix with
    | None ->
        render_signature_item item
    | Some suffix ->
        Doc.concat [ render_signature_item item; suffix ]
  in
  (doc, is_open_signature_item item, false, false, false, false)

and render_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      render_let_binding binding
  | Syn.Cst.StructureItem.TypeDeclaration decl ->
      render_type_declaration_with_keyword kw_type decl
  | Syn.Cst.StructureItem.TypeMutualDeclaration decl ->
      render_type_mutual_declaration decl
  | Syn.Cst.StructureItem.ExternalDeclaration decl ->
      render_external_declaration decl
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      render_module_declaration_with_keyword (Doc.text "module") decl
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
      render_recursive_module_declaration decl
  | Syn.Cst.StructureItem.ModuleTypeDeclaration decl ->
      render_module_type_declaration decl
  | Syn.Cst.StructureItem.IncludeStatement stmt ->
      render_include_statement stmt
  | Syn.Cst.StructureItem.OpenStatement open_ ->
      Doc.concat
        [
          kw_open;
          (if open_.bang_token = None then Doc.empty else Doc.text "!");
          Doc.space;
          render_open_target open_.target;
        ]
  | Syn.Cst.StructureItem.Expression expression ->
      render_expression expression
  | item ->
      doc_of_node (Syn.Cst.StructureItem.syntax_node item)

and render_signature_item item =
  match item with
  | Syn.Cst.SignatureItem.TypeDeclaration decl ->
      render_type_declaration_with_keyword kw_type decl
  | Syn.Cst.SignatureItem.TypeMutualDeclaration decl ->
      render_type_mutual_declaration decl
  | Syn.Cst.SignatureItem.ModuleDeclaration decl ->
      render_module_declaration_with_keyword (Doc.text "module") decl
  | Syn.Cst.SignatureItem.RecursiveModuleDeclaration decl ->
      render_recursive_module_declaration decl
  | Syn.Cst.SignatureItem.ModuleTypeDeclaration decl ->
      render_module_type_declaration decl
  | Syn.Cst.SignatureItem.IncludeStatement stmt ->
      render_include_statement stmt
  | Syn.Cst.SignatureItem.OpenStatement open_ ->
      Doc.concat
        [
          kw_open;
          (if open_.bang_token = None then Doc.empty else Doc.text "!");
          Doc.space;
          render_open_target open_.target;
        ]
  | Syn.Cst.SignatureItem.ValueDeclaration decl ->
      Doc.concat
        [
          Doc.text "val";
          Doc.space;
          doc_of_token decl.name_token;
          colon;
          render_core_type decl.type_;
        ]
  | item ->
      doc_of_node (Syn.Cst.SignatureItem.syntax_node item)

and render_structure_top_level_items ~source_node ~items =
  let flush_pending pending acc =
    let strip_trailing_breaks =
      not (acc = [] && pending_doc_count pending = 1)
    in
    match render_pending_trivia ~strip_trailing_breaks pending with
    | None ->
        acc
    | Some pending_doc ->
        (pending_doc, false, true, false, not strip_trailing_breaks, false) :: acc
  in
  let pending_has_only_breaks pending =
    List.for_all
      (function
        | TriviaBreak _ ->
            true
        | TriviaDoc _ ->
            false)
      pending
  in
  let rec join_entries = function
    | [] ->
        Doc.empty
    | (doc, _, _, _, _, _) :: [] ->
        doc
    | (doc, is_open, is_trivia, tight_after, has_trailing_break, _)
      :: ((_, next_is_open, _, _, _, _) :: _ as rest) ->
        let separator =
          if has_trailing_break then
            Doc.empty
          else if tight_after || is_trivia then
            Doc.line
          else if is_open && next_is_open then
            Doc.line
          else
            blank_line
        in
        Doc.concat [ doc; separator; join_entries rest ]
  in
  let rec loop pending acc items = function
    | [] ->
        let acc = flush_pending pending acc in
        join_entries (List.rev acc)
    | child :: rest -> (
        match child with
        | Syn.Ceibo.Red.Token syntax_token -> (
            match doc_of_top_level_trivia_token syntax_token with
            | Some doc ->
                loop (TriviaDoc doc :: pending) acc items rest
            | None ->
                let pending =
                  let newline_count = newline_count_of_whitespace_token syntax_token in
                  if newline_count > 0 then
                    push_pending_break pending newline_count
                  else
                    pending
                in
                loop pending acc items rest)
        | Syn.Ceibo.Red.Node _ -> (
            match items with
            | item :: items ->
                let pending =
                  match acc with
                  | (_, _, _, _, _, prev_is_module_alias) :: _
                    when
                      prev_is_module_alias
                      && is_module_alias_structure_item item
                      && pending_has_only_breaks pending ->
                      []
                  | _ ->
                      pending
                in
                let acc = flush_pending pending acc in
                loop [] (render_structure_entry item :: acc) items rest
            | [] ->
                loop pending acc items rest))
  in
  loop [] [] items (Syn.Ceibo.Red.SyntaxNode.children_list source_node)

and render_structure_items ~source_node items =
  render_structure_top_level_items ~source_node ~items

and render_signature_top_level_items ~source_node ~items =
  let flush_pending pending acc =
    match render_pending_trivia pending with
    | None ->
        acc
    | Some pending_doc ->
        (pending_doc, false, true, false, false, false) :: acc
  in
  let rec join_entries = function
    | [] ->
        Doc.empty
    | (doc, _, _, _, _, _) :: [] ->
        doc
    | (doc, is_open, is_trivia, tight_after, has_trailing_break, _)
      :: ((_, next_is_open, _, _, _, _) :: _ as rest) ->
        let separator =
          if has_trailing_break then
            Doc.empty
          else if tight_after || is_trivia then
            Doc.line
          else if is_open && next_is_open then
            Doc.line
          else
            blank_line
        in
        Doc.concat [ doc; separator; join_entries rest ]
  in
  let rec loop pending acc items = function
    | [] ->
        let acc = flush_pending pending acc in
        join_entries (List.rev acc)
    | child :: rest -> (
        match child with
        | Syn.Ceibo.Red.Token syntax_token -> (
            match doc_of_top_level_trivia_token syntax_token with
            | Some doc ->
                loop (TriviaDoc doc :: pending) acc items rest
            | None ->
                let pending =
                  let newline_count = newline_count_of_whitespace_token syntax_token in
                  if newline_count > 0 then
                    push_pending_break pending newline_count
                  else
                    pending
                in
                loop pending acc items rest)
        | Syn.Ceibo.Red.Node _ -> (
            match items with
            | item :: items ->
                let acc = flush_pending pending acc in
                loop [] (render_signature_entry item :: acc) items rest
            | [] ->
                loop pending acc items rest))
  in
  loop [] [] items (Syn.Ceibo.Red.SyntaxNode.children_list source_node)

and render_signature_items ~source_node items =
  render_signature_top_level_items ~source_node ~items

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
