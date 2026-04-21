open Std
open Std.Collections
module Slice = IO.IoVec.IoSlice

type file_kind =
[
  | `Implementation
  | `Interface
]

type parse_result = {
  source: Slice.t;
  kind: file_kind;
  tokens: Raw_token.stream;
  events: Event.Buffer.t;
  tree: Syntax_tree.t;
  diagnostics: Diagnostic.t Vector.t;
}

type parser = {
  source: Slice.t;
  token_stream: Raw_token.stream;
  events: Event.Buffer.t;
  mutable pos: int;
}

let create = fun source ->
  let token_stream = Lexer.tokenize_slice source |> Raw_token.of_lexer_tokens in
  { source; token_stream; events = Event.Buffer.create (); pos = 0 }

let significant_count = fun p -> Vector.length p.token_stream.Raw_token.significant

let raw_count = fun p -> Vector.length p.token_stream.Raw_token.raw

let raw_at = fun p raw_index -> Vector.get_unchecked p.token_stream.Raw_token.raw ~at:raw_index

let significant_raw_at = fun p index ->
  let count = significant_count p in
  if count = 0 then
    0
  else if index < 0 then
    Vector.get_unchecked p.token_stream.Raw_token.significant ~at:0
  else if index >= count then
    Vector.get_unchecked p.token_stream.Raw_token.significant ~at:(count - 1)
  else
    Vector.get_unchecked p.token_stream.Raw_token.significant ~at:index

let current_raw_index = fun p -> significant_raw_at p p.pos

let peek_raw_index = fun p offset -> significant_raw_at p (p.pos + offset)

let current = fun p -> raw_at p (current_raw_index p)

let peek = fun p offset -> raw_at p (peek_raw_index p offset)

let current_kind = fun p -> (current p).Raw_token.kind

let peek_kind = fun p offset -> (peek p offset).Raw_token.kind

let at = fun p kind -> current_kind p = kind

let is_eof = fun p -> at p Syntax_kind2.EOF

let current_offset = fun p -> (current p).Raw_token.span.Ceibo.Span.start

let token_text = fun p raw -> Raw_token.text_slice ~source:p.source raw

let found_token = fun p raw : Diagnostic.found_token ->
  { kind = Syntax_kind2.to_string raw.Raw_token.kind; text = token_text p raw }

let diagnostic_at_current = fun p kind ->
  let raw = current p in
  Diagnostic.make ~kind ~span:raw.Raw_token.span

let invalid_expression = fun p ->
  diagnostic_at_current p (Diagnostic.InvalidExpression { found = found_token p (current p) })

let invalid_pattern = fun p ->
  diagnostic_at_current p (Diagnostic.InvalidPattern { found = found_token p (current p) })

let invalid_type_expression = fun p ->
  diagnostic_at_current p (Diagnostic.InvalidTypeExpression { found = found_token p (current p) })

let start_node = fun p -> Event.Buffer.start_node p.events

let complete = fun p marker kind ->
  Event.Buffer.complete p.events marker kind

let precede = fun p completed ->
  Event.Buffer.precede p.events completed

let bump = fun p ->
  if p.pos < significant_count p then
    (
      Event.Buffer.token p.events ~raw_index:(current_raw_index p);
      p.pos <- p.pos + 1
    )

let bump_if = fun p kind ->
  if at p kind then
    (
      bump p;
      true
    )
  else
    false

let expect = fun p kind diagnostic ->
  if at p kind then
    bump p
  else (
    Event.Buffer.missing p.events ~kind ~offset:(current_offset p);
    Event.Buffer.error p.events diagnostic
  )

let recover_current_as_error = fun p diagnostic ->
  let marker = start_node p in
  Event.Buffer.error p.events diagnostic;
  if is_eof p then
    Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p)
  else
    bump p;
  ignore (complete p marker Syntax_kind2.ERROR)

let ensure_progress = fun p before diagnostic ->
  if p.pos = before && not (is_eof p) then
    recover_current_as_error p diagnostic

let starts_structure_item = function
  | Syntax_kind2.LET_KW
  | Syntax_kind2.TYPE_KW
  | Syntax_kind2.MODULE_KW
  | Syntax_kind2.OPEN_KW
  | Syntax_kind2.INCLUDE_KW
  | Syntax_kind2.EXTERNAL_KW
  | Syntax_kind2.EXCEPTION_KW
  | Syntax_kind2.CLASS_KW -> true
  | _ -> false

let starts_signature_item = function
  | Syntax_kind2.VAL_KW
  | Syntax_kind2.TYPE_KW
  | Syntax_kind2.MODULE_KW
  | Syntax_kind2.OPEN_KW
  | Syntax_kind2.INCLUDE_KW
  | Syntax_kind2.EXTERNAL_KW
  | Syntax_kind2.EXCEPTION_KW
  | Syntax_kind2.CLASS_KW -> true
  | _ -> false

let raw_contains_newline = fun p raw_index ->
  let raw = raw_at p raw_index in
  match raw.Raw_token.kind with
  | Syntax_kind2.WHITESPACE
  | Syntax_kind2.COMMENT
  | Syntax_kind2.DOCSTRING -> Raw_token.contains_char ~source:p.source raw '\n'
  | _ -> false

let leading_trivia_contains_newline = fun p ->
  let current_raw = current_raw_index p in
  let previous_raw =
    if p.pos <= 0 then
      (-1)
    else
      significant_raw_at p (p.pos - 1)
  in
  let rec loop index =
    if index >= current_raw || index >= raw_count p then
      false
    else if raw_contains_newline p index then
      true
    else
      loop (index + 1)
  in
  loop (previous_raw + 1)

let at_item_boundary = fun p ~signature ->
  if is_eof p then
    true
  else if not (leading_trivia_contains_newline p) then
    false
  else if signature then
    starts_signature_item (current_kind p)
    || (at p Syntax_kind2.LBRACKET
       && (peek_kind p 1 = Syntax_kind2.PERCENT
          || peek_kind p 1 = Syntax_kind2.AT
          || peek_kind p 1 = Syntax_kind2.ATAT))
  else
    starts_structure_item (current_kind p)
    || (at p Syntax_kind2.LBRACKET
       && (peek_kind p 1 = Syntax_kind2.PERCENT
          || peek_kind p 1 = Syntax_kind2.AT
          || peek_kind p 1 = Syntax_kind2.ATAT))

let expression_boundary = fun p ~stop_at_item ~stop_at_semi ~signature ->
  match current_kind p with
  | Syntax_kind2.EOF
  | Syntax_kind2.IN_KW
  | Syntax_kind2.THEN_KW
  | Syntax_kind2.ELSE_KW
  | Syntax_kind2.WITH_KW
  | Syntax_kind2.WHEN_KW
  | Syntax_kind2.ARROW
  | Syntax_kind2.PIPE
  | Syntax_kind2.COMMA
  | Syntax_kind2.RPAREN
  | Syntax_kind2.RBRACKET
  | Syntax_kind2.RBRACE
  | Syntax_kind2.BAR_RBRACKET
  | Syntax_kind2.END_KW
  | Syntax_kind2.DONE_KW -> true
  | Syntax_kind2.SEMI -> stop_at_semi || (stop_at_item && peek_kind p 1 = Syntax_kind2.SEMI)
  | _ -> stop_at_item && at_item_boundary p ~signature

let can_start_atom = function
  | Syntax_kind2.IDENT
  | Syntax_kind2.INT
  | Syntax_kind2.FLOAT
  | Syntax_kind2.STRING
  | Syntax_kind2.CHAR
  | Syntax_kind2.TRUE_KW
  | Syntax_kind2.FALSE_KW
  | Syntax_kind2.LPAREN
  | Syntax_kind2.BEGIN_KW
  | Syntax_kind2.LBRACKET
  | Syntax_kind2.LBRACKET_BAR
  | Syntax_kind2.LBRACE
  | Syntax_kind2.LET_KW
  | Syntax_kind2.IF_KW
  | Syntax_kind2.MATCH_KW
  | Syntax_kind2.FUN_KW
  | Syntax_kind2.FUNCTION_KW
  | Syntax_kind2.TRY_KW
  | Syntax_kind2.ASSERT_KW
  | Syntax_kind2.LAZY_KW
  | Syntax_kind2.WHILE_KW
  | Syntax_kind2.FOR_KW
  | Syntax_kind2.OBJECT_KW
  | Syntax_kind2.NEW_KW
  | Syntax_kind2.BACKTICK
  | Syntax_kind2.TILDE
  | Syntax_kind2.QUESTION
  | Syntax_kind2.PLUS
  | Syntax_kind2.MINUS
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.BANG -> true
  | _ -> false

let can_start_pattern_atom = function
  | Syntax_kind2.IDENT
  | Syntax_kind2.UNDERSCORE
  | Syntax_kind2.INT
  | Syntax_kind2.FLOAT
  | Syntax_kind2.STRING
  | Syntax_kind2.CHAR
  | Syntax_kind2.TRUE_KW
  | Syntax_kind2.FALSE_KW
  | Syntax_kind2.LPAREN
  | Syntax_kind2.LBRACKET
  | Syntax_kind2.LBRACKET_BAR
  | Syntax_kind2.LBRACE
  | Syntax_kind2.BACKTICK
  | Syntax_kind2.HASH
  | Syntax_kind2.TILDE
  | Syntax_kind2.QUESTION
  | Syntax_kind2.LAZY_KW
  | Syntax_kind2.EXCEPTION_KW -> true
  | _ -> false

let prefix_operator = function
  | Syntax_kind2.PLUS
  | Syntax_kind2.MINUS
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.BANG -> true
  | _ -> false

let operator_pattern_token = function
  | Syntax_kind2.PLUS
  | Syntax_kind2.MINUS
  | Syntax_kind2.STAR
  | Syntax_kind2.SLASH
  | Syntax_kind2.PERCENT
  | Syntax_kind2.CARET
  | Syntax_kind2.EQ
  | Syntax_kind2.LT
  | Syntax_kind2.GT
  | Syntax_kind2.LTE
  | Syntax_kind2.GTE
  | Syntax_kind2.NE
  | Syntax_kind2.BANG
  | Syntax_kind2.AMPAMP
  | Syntax_kind2.BARBAR
  | Syntax_kind2.PIPE
  | Syntax_kind2.AMPERSAND
  | Syntax_kind2.AT
  | Syntax_kind2.HASH
  | Syntax_kind2.TILDE
  | Syntax_kind2.QUESTION
  | Syntax_kind2.DOLLAR
  | Syntax_kind2.COLONCOLON
  | Syntax_kind2.COLONEQ
  | Syntax_kind2.ARROW
  | Syntax_kind2.LEFT_ARROW
  | Syntax_kind2.STARSTAR
  | Syntax_kind2.EQEQ
  | Syntax_kind2.BANGEQ
  | Syntax_kind2.ATAT
  | Syntax_kind2.PIPEGT
  | Syntax_kind2.PERCENTGT
  | Syntax_kind2.LTPERCENT
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.STARDOT
  | Syntax_kind2.SLASHDOT -> true
  | _ -> false

let symbolic_operator_part = function
  | kind when operator_pattern_token kind -> true
  | _ -> false

let parenthesized_operator_start = fun p ->
  at p Syntax_kind2.DOT
  || (operator_pattern_token (current_kind p)
     && (peek_kind p 1 = Syntax_kind2.RPAREN || symbolic_operator_part (peek_kind p 1)))

let index_opener = function
  | Syntax_kind2.LPAREN
  | Syntax_kind2.LBRACKET
  | Syntax_kind2.LBRACE -> true
  | _ -> false

let symbolic_sequence_followed_by_index_opener = fun p offset ->
  let rec loop offset consumed =
    let kind = peek_kind p offset in
    if symbolic_operator_part kind then
      loop (offset + 1) true
    else
      consumed && index_opener kind
  in
  loop offset false

let binding_operator_suffix = function
  | Syntax_kind2.STAR
  | Syntax_kind2.PLUS -> true
  | _ -> false

let binding_operator_keyword = function
  | Syntax_kind2.LET_KW
  | Syntax_kind2.AND_KW -> true
  | _ -> false

let infix_binding_power = function
  | Syntax_kind2.BARBAR -> Some 10
  | Syntax_kind2.AMPAMP -> Some 15
  | Syntax_kind2.EQ
  | Syntax_kind2.EQEQ
  | Syntax_kind2.BANGEQ
  | Syntax_kind2.NE
  | Syntax_kind2.LT
  | Syntax_kind2.GT
  | Syntax_kind2.LTE
  | Syntax_kind2.GTE
  | Syntax_kind2.LTPERCENT -> Some 20
  | Syntax_kind2.COLONCOLON
  | Syntax_kind2.AT
  | Syntax_kind2.ATAT
  | Syntax_kind2.PIPEGT -> Some 30
  | Syntax_kind2.DOLLAR -> Some 30
  | Syntax_kind2.AMPERSAND -> Some 35
  | Syntax_kind2.PLUS
  | Syntax_kind2.MINUS
  | Syntax_kind2.CARET -> Some 40
  | Syntax_kind2.STAR
  | Syntax_kind2.SLASH
  | Syntax_kind2.PERCENT
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.STARDOT
  | Syntax_kind2.SLASHDOT
  | Syntax_kind2.PERCENTGT
  | Syntax_kind2.OPERATOR_KW -> Some 50
  | Syntax_kind2.STARSTAR -> Some 60
  | _ -> None

let pattern_binding_power = function
  | Syntax_kind2.PIPE -> Some 10
  | Syntax_kind2.COMMA -> Some 15
  | Syntax_kind2.AS_KW -> Some 20
  | Syntax_kind2.DOTDOT -> Some 25
  | Syntax_kind2.COLONCOLON -> Some 30
  | Syntax_kind2.COLON -> Some 40
  | _ -> None

let rec consume_path_segments = fun p ->
  if at p Syntax_kind2.DOT && peek_kind p 1 = Syntax_kind2.IDENT then
    (
      bump p;
      bump p;
      consume_path_segments p
    )

let rec consume_balanced_until = fun p ~closer depth ->
  if not (is_eof p || (depth = 0 && at p closer)) then
    (
      let depth =
        match current_kind p with
        | Syntax_kind2.LPAREN
        | Syntax_kind2.LBRACE
        | Syntax_kind2.LBRACKET
        | Syntax_kind2.LBRACKET_BAR
        | Syntax_kind2.BEGIN_KW
        | Syntax_kind2.STRUCT_KW
        | Syntax_kind2.SIG_KW -> depth + 1
        | Syntax_kind2.RPAREN
        | Syntax_kind2.RBRACE
        | Syntax_kind2.RBRACKET
        | Syntax_kind2.BAR_RBRACKET
        | Syntax_kind2.END_KW when depth > 0 -> depth - 1
        | _ -> depth
      in
      bump p;
      consume_balanced_until p ~closer depth
    )

let is_attribute_suffix = fun p ->
  at p Syntax_kind2.LBRACKET
  && (peek_kind p 1 = Syntax_kind2.AT || peek_kind p 1 = Syntax_kind2.ATAT)

let is_extension_shell = fun p ->
  at p Syntax_kind2.LBRACKET && peek_kind p 1 = Syntax_kind2.PERCENT

let is_attribute_shell = fun p ->
  at p Syntax_kind2.LBRACKET
  && (peek_kind p 1 = Syntax_kind2.AT || peek_kind p 1 = Syntax_kind2.ATAT)

let rec consume_attribute_sigils = fun p ->
  if at p Syntax_kind2.AT || at p Syntax_kind2.ATAT then
    (
      bump p;
      consume_attribute_sigils p
    )

let rec consume_extension_sigils = fun p ->
  if at p Syntax_kind2.PERCENT then
    (
      bump p;
      consume_extension_sigils p
    )

let rec consume_until = fun p closer ->
  if not (is_eof p || at p closer) then
    (
      bump p;
      consume_until p closer
    )

let consume_extension_payload = fun p ->
  if at p Syntax_kind2.LBRACE && peek_kind p 1 = Syntax_kind2.DOT then
    consume_until p Syntax_kind2.RBRACKET
  else
    consume_balanced_until p ~closer:Syntax_kind2.RBRACKET 0

let rec consume_symbolic_operator = fun p ->
  if symbolic_operator_part (current_kind p) then
    (
      bump p;
      consume_symbolic_operator p
    )

let rec parse_expression = fun p ~signature ~stop_at_item ?(stop_at_semi = false) min_bp ->
  let rec loop lhs =
    if expression_boundary p ~stop_at_item ~stop_at_semi ~signature then
      lhs
    else if at p Syntax_kind2.SEMI && not stop_at_semi && min_bp <= 0 then
      (
        let marker = precede p lhs in
        bump p;
        let _rhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi 0 in
        loop (complete p marker Syntax_kind2.SEQUENCE_EXPR)
      )
    else if at p Syntax_kind2.DOT then
      (
        match peek_kind p 1 with
        | Syntax_kind2.LPAREN ->
            let marker = precede p lhs in
            bump p;
            bump p;
            if not (at p Syntax_kind2.RPAREN || is_eof p) then
              ignore (parse_expression p ~signature ~stop_at_item:false 0);
            expect p Syntax_kind2.RPAREN (invalid_expression p);
            loop (complete p marker Syntax_kind2.ARRAY_INDEX_EXPR)
        | Syntax_kind2.LBRACKET ->
            let marker = precede p lhs in
            bump p;
            bump p;
            if not (at p Syntax_kind2.RBRACKET || is_eof p) then
              ignore (parse_expression p ~signature ~stop_at_item:false 0);
            expect p Syntax_kind2.RBRACKET (invalid_expression p);
            loop (complete p marker Syntax_kind2.STRING_INDEX_EXPR)
        | Syntax_kind2.LBRACKET_BAR ->
            let marker = precede p lhs in
            bump p;
            bump p;
            let rec parse_elements () =
              if not (at p Syntax_kind2.BAR_RBRACKET || is_eof p) then
                (
                  let before = p.pos in
                  ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
                  ensure_progress p before (invalid_expression p);
                  ignore (bump_if p Syntax_kind2.SEMI);
                  parse_elements ()
                )
            in
            parse_elements ();
            expect p Syntax_kind2.BAR_RBRACKET (invalid_expression p);
            loop (complete p marker Syntax_kind2.LOCAL_OPEN_EXPR)
        | Syntax_kind2.LBRACE ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_record_expr p ~signature);
            loop (complete p marker Syntax_kind2.LOCAL_OPEN_EXPR)
        | Syntax_kind2.IDENT ->
            let marker = precede p lhs in
            bump p;
            expect p Syntax_kind2.IDENT (invalid_expression p);
            loop (complete p marker Syntax_kind2.FIELD_ACCESS_EXPR)
        | Syntax_kind2.BANG ->
            loop (parse_dot_bang_expr p lhs ~signature ~stop_at_item ~stop_at_semi)
        | kind when symbolic_operator_part kind && symbolic_sequence_followed_by_index_opener p 1 ->
            loop (parse_extended_index_expr p lhs ~signature ~stop_at_item)
        | _ -> lhs
      )
    else if at p Syntax_kind2.HASH then
      if peek_kind p 1 = Syntax_kind2.IDENT then
        (
          let marker = precede p lhs in
          bump p;
          expect p Syntax_kind2.IDENT (invalid_expression p);
          loop (complete p marker Syntax_kind2.METHOD_CALL_EXPR)
        )
      else if min_bp <= 50 then
        (
          let marker = precede p lhs in
          consume_symbolic_operator p;
          let _rhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi 51 in
          loop (complete p marker Syntax_kind2.INFIX_EXPR)
        )
      else
        lhs
    else if (at p Syntax_kind2.LEFT_ARROW || at p Syntax_kind2.COLONEQ) && min_bp <= 5 then
      (
        let marker = precede p lhs in
        bump p;
        let _rhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi 6 in
        loop (complete p marker Syntax_kind2.ASSIGN_EXPR)
      )
    else if at p Syntax_kind2.COLON && min_bp <= 5 then
      (
        let marker = precede p lhs in
        bump p;
        parse_type_expr p ~stop_at_arrow:false;
        loop (complete p marker Syntax_kind2.TYPED_EXPR)
      )
    else if is_attribute_suffix p then
      (
        let marker = precede p lhs in
        bump p;
        bump p;
        consume_balanced_until p ~closer:Syntax_kind2.RBRACKET 0;
        expect p Syntax_kind2.RBRACKET (invalid_expression p);
        loop (complete p marker Syntax_kind2.ATTRIBUTE_EXPR)
      )
    else if can_start_atom (current_kind p) then
      let marker = precede p lhs in
      let _argument = parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi in
      loop (complete p marker Syntax_kind2.APPLY_EXPR)
    else
      match infix_binding_power (current_kind p) with
      | Some bp when bp >= min_bp ->
          let marker = precede p lhs in
          bump p;
          let _rhs = parse_expression p ~signature ~stop_at_item ~stop_at_semi (bp + 1) in
          loop (complete p marker Syntax_kind2.INFIX_EXPR)
      | _ -> lhs
  in
  loop (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi)

and parse_prefix_or_atom = fun p ~signature ~stop_at_item ~stop_at_semi ->
  match current_kind p with
  | kind when prefix_operator kind ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi
      else
        (
          let marker = start_node p in
          bump p;
          let _operand = parse_expression p ~signature ~stop_at_item ~stop_at_semi 70 in
          complete p marker Syntax_kind2.PREFIX_EXPR
        )
  | Syntax_kind2.LET_KW ->
      parse_let_expr p ~signature ~stop_at_item
  | Syntax_kind2.DOT ->
      parse_unreachable_expr p
  | Syntax_kind2.BACKTICK ->
      parse_poly_variant_expr p ~signature ~stop_at_item ~stop_at_semi
  | Syntax_kind2.TILDE ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi
      else
        parse_label_arg_expr p ~signature ~stop_at_item ~stop_at_semi Syntax_kind2.LABELED_ARG
  | Syntax_kind2.QUESTION ->
      if symbolic_operator_part (peek_kind p 1) then
        parse_symbolic_prefix_expr p ~signature ~stop_at_item ~stop_at_semi
      else
        parse_label_arg_expr p ~signature ~stop_at_item ~stop_at_semi Syntax_kind2.OPTIONAL_ARG
  | Syntax_kind2.IF_KW ->
      parse_if_expr p ~signature ~stop_at_item
  | Syntax_kind2.MATCH_KW ->
      parse_match_expr p ~signature ~stop_at_item
  | Syntax_kind2.FUN_KW ->
      parse_fun_expr p ~signature ~stop_at_item
  | Syntax_kind2.FUNCTION_KW ->
      parse_function_expr p ~signature ~stop_at_item
  | Syntax_kind2.TRY_KW ->
      parse_try_expr p ~signature ~stop_at_item
  | Syntax_kind2.ASSERT_KW ->
      parse_unary_keyword_expr p ~signature ~stop_at_item Syntax_kind2.ASSERT_EXPR
  | Syntax_kind2.LAZY_KW ->
      parse_unary_keyword_expr p ~signature ~stop_at_item Syntax_kind2.LAZY_EXPR
  | Syntax_kind2.WHILE_KW ->
      parse_while_expr p ~signature ~stop_at_item
  | Syntax_kind2.FOR_KW ->
      parse_for_expr p ~signature ~stop_at_item
  | Syntax_kind2.OBJECT_KW ->
      parse_object_expr p
  | Syntax_kind2.NEW_KW ->
      parse_new_expr p
  | Syntax_kind2.IDENT ->
      parse_path_expr p
  | Syntax_kind2.INT
  | Syntax_kind2.FLOAT
  | Syntax_kind2.STRING
  | Syntax_kind2.CHAR
  | Syntax_kind2.TRUE_KW
  | Syntax_kind2.FALSE_KW ->
      parse_literal_expr p
  | Syntax_kind2.LPAREN
  | Syntax_kind2.BEGIN_KW ->
      parse_parenthesized_expr p ~signature ~stop_at_item
  | Syntax_kind2.LBRACKET ->
      if is_extension_shell p then
        parse_extension_expr p
      else
        parse_list_expr p ~signature
  | Syntax_kind2.LBRACKET_BAR ->
      parse_array_expr p ~signature
  | Syntax_kind2.LBRACE ->
      parse_record_expr p ~signature
  | _ ->
      let marker = start_node p in
      Event.Buffer.error p.events (invalid_expression p);
      if not (is_eof p) then
        bump p
      else
        Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
      complete p marker Syntax_kind2.ERROR

and parse_symbolic_prefix_expr = fun p ~signature ~stop_at_item ~stop_at_semi ->
  let marker = start_node p in
  consume_symbolic_operator p;
  let _operand = parse_expression p ~signature ~stop_at_item ~stop_at_semi 70 in
  complete p marker Syntax_kind2.PREFIX_EXPR

and parse_unreachable_expr = fun p ->
  let marker = start_node p in
  bump p;
  complete p marker Syntax_kind2.UNREACHABLE_EXPR

and parse_object_expr = fun p ->
  let marker = start_node p in
  bump p;
  consume_balanced_until p ~closer:Syntax_kind2.END_KW 0;
  expect p Syntax_kind2.END_KW (invalid_expression p);
  complete p marker Syntax_kind2.OBJECT_EXPR

and parse_new_expr = fun p ->
  let marker = start_node p in
  bump p;
  if at p Syntax_kind2.IDENT then
    ignore (parse_path_expr p)
  else
    Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
  complete p marker Syntax_kind2.NEW_EXPR

and parse_extended_index_expr = fun p lhs ~signature ~stop_at_item:_ ->
  let marker = precede p lhs in
  bump p;
  consume_symbolic_operator p;
  let closer =
    match current_kind p with
    | Syntax_kind2.LPAREN ->
        bump p;
        Syntax_kind2.RPAREN
    | Syntax_kind2.LBRACKET ->
        bump p;
        Syntax_kind2.RBRACKET
    | Syntax_kind2.LBRACE ->
        bump p;
        Syntax_kind2.RBRACE
    | _ ->
        Event.Buffer.error p.events (invalid_expression p);
        Syntax_kind2.RPAREN
  in
  if not (at p closer || is_eof p) then
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p closer (invalid_expression p);
  complete p marker Syntax_kind2.ARRAY_INDEX_EXPR

and parse_path_expr = fun p ->
  let marker = start_node p in
  expect p Syntax_kind2.IDENT (invalid_expression p);
  consume_path_segments p;
  complete p marker Syntax_kind2.PATH_EXPR

and parse_literal_expr = fun p ->
  let marker = start_node p in
  bump p;
  complete p marker Syntax_kind2.LITERAL_EXPR

and parse_poly_variant_expr = fun p ~signature ~stop_at_item ~stop_at_semi ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind2.IDENT (invalid_expression p);
  if
    (not (expression_boundary p ~stop_at_item ~stop_at_semi ~signature))
    && can_start_atom (current_kind p)
  then
    ignore (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi);
  complete p marker Syntax_kind2.POLY_VARIANT_EXPR

and parse_label_arg_expr = fun p ~signature ~stop_at_item ~stop_at_semi kind ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind2.IDENT (invalid_expression p);
  if at p Syntax_kind2.COLON then
    (
      bump p;
      ignore (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi)
    );
  complete p marker kind

and parse_parenthesized_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  let opener = current_kind p in
  bump p;
  let at_closer () = at p Syntax_kind2.RPAREN || at p Syntax_kind2.END_KW || is_eof p in
  if opener = Syntax_kind2.LPAREN && at p Syntax_kind2.MODULE_KW then
    (
      bump p;
      consume_balanced_until p ~closer:Syntax_kind2.RPAREN 0;
      expect p Syntax_kind2.RPAREN (invalid_expression p);
      complete p marker Syntax_kind2.FIRST_CLASS_MODULE_EXPR
    )
  else if opener = Syntax_kind2.LPAREN && parenthesized_operator_start p then
    (
      consume_balanced_until p ~closer:Syntax_kind2.RPAREN 0;
      expect p Syntax_kind2.RPAREN (invalid_expression p);
      complete p marker Syntax_kind2.PATH_EXPR
    )
  else (
    if not (at_closer ()) then
      ignore (parse_expression p ~signature ~stop_at_item:false 0);
    let rec parse_comma_tail saw_comma =
      if at p Syntax_kind2.COMMA then
        (
          bump p;
          if not (at_closer ()) then
            ignore (parse_expression p ~signature ~stop_at_item:false 0);
          parse_comma_tail true
        )
      else
        saw_comma
    in
    let saw_comma = parse_comma_tail false in
    (
      match opener with
      | Syntax_kind2.BEGIN_KW -> expect p Syntax_kind2.END_KW (invalid_expression p)
      | _ -> expect p Syntax_kind2.RPAREN (invalid_expression p)
    );
    complete p marker
      (
        if saw_comma then
          Syntax_kind2.TUPLE_EXPR
        else
          Syntax_kind2.PAREN_EXPR
      )
  )

and parse_list_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind2.RBRACKET || is_eof p) then
      (
        let before = p.pos in
        ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
        ensure_progress p before (invalid_expression p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_elements ()
      )
  in
  parse_elements ();
  expect p Syntax_kind2.RBRACKET (invalid_expression p);
  complete p marker Syntax_kind2.LIST_EXPR

and parse_extension_expr = fun p ->
  let marker = start_node p in
  bump p;
  consume_extension_sigils p;
  consume_extension_payload p;
  expect p Syntax_kind2.RBRACKET (invalid_expression p);
  complete p marker Syntax_kind2.EXTENSION_EXPR

and parse_array_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind2.BAR_RBRACKET || is_eof p) then
      (
        let before = p.pos in
        ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
        ensure_progress p before (invalid_expression p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_elements ()
      )
  in
  parse_elements ();
  expect p Syntax_kind2.BAR_RBRACKET (invalid_expression p);
  complete p marker Syntax_kind2.ARRAY_EXPR

and parse_record_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  let rec parse_fields () =
    if not (at p Syntax_kind2.RBRACE || is_eof p) then
      (
        let before = p.pos in
        ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
        if at p Syntax_kind2.EQ then
          (
            bump p;
            ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0)
          );
        ensure_progress p before (invalid_expression p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_fields ()
      )
  in
  let kind =
    if at p Syntax_kind2.RBRACE || is_eof p then
      Syntax_kind2.RECORD_EXPR
    else (
      let before = p.pos in
      ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0);
      if at p Syntax_kind2.WITH_KW then
        (
          bump p;
          parse_fields ();
          Syntax_kind2.RECORD_UPDATE_EXPR
        )
      else (
        if at p Syntax_kind2.EQ then
          (
            bump p;
            ignore (parse_expression p ~signature ~stop_at_item:false ~stop_at_semi:true 0)
          );
        ensure_progress p before (invalid_expression p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_fields ();
        Syntax_kind2.RECORD_EXPR
      )
    )
  in
  expect p Syntax_kind2.RBRACE (invalid_expression p);
  complete p marker kind

and parse_let_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  if binding_operator_suffix (current_kind p) then
    parse_binding_operator_expr p marker ~signature ~stop_at_item
  else if at p Syntax_kind2.OPEN_KW then
    (
      bump p;
      ignore (bump_if p Syntax_kind2.BANG);
      ignore (parse_path_expr p);
      expect p Syntax_kind2.IN_KW (invalid_expression p);
      ignore (parse_expression p ~signature ~stop_at_item 0);
      complete p marker Syntax_kind2.LOCAL_OPEN_EXPR
    )
  else if at p Syntax_kind2.MODULE_KW then
    (
      bump p;
      if at p Syntax_kind2.IDENT || at p Syntax_kind2.PERCENT then
        bump p
      else
        expect p Syntax_kind2.IDENT (invalid_expression p);
      consume_balanced_until p ~closer:Syntax_kind2.EQ 0;
      expect p Syntax_kind2.EQ (invalid_expression p);
      consume_balanced_until p ~closer:Syntax_kind2.IN_KW 0;
      expect p Syntax_kind2.IN_KW (invalid_expression p);
      ignore (parse_expression p ~signature ~stop_at_item 0);
      complete p marker Syntax_kind2.LET_MODULE_EXPR
    )
  else if at p Syntax_kind2.EXCEPTION_KW then
    (
      bump p;
      consume_balanced_until p ~closer:Syntax_kind2.IN_KW 0;
      expect p Syntax_kind2.IN_KW (invalid_expression p);
      ignore (parse_expression p ~signature ~stop_at_item 0);
      complete p marker Syntax_kind2.LET_EXCEPTION_EXPR
    )
  else (
    ignore (bump_if p Syntax_kind2.REC_KW);
    parse_let_binding p ~signature ~top_level:false;
    let rec parse_and_bindings () =
      if at p Syntax_kind2.AND_KW then
        (
          bump p;
          parse_let_binding p ~signature ~top_level:false;
          parse_and_bindings ()
        )
    in
    parse_and_bindings ();
    expect p Syntax_kind2.IN_KW (invalid_expression p);
    ignore (parse_expression p ~signature ~stop_at_item 0);
    complete p marker Syntax_kind2.LET_EXPR
  )

and parse_binding_operator_expr = fun p marker ~signature ~stop_at_item ->
  bump p;
  parse_let_binding p ~signature ~top_level:false;
  let rec parse_parallel_bindings () =
    if at p Syntax_kind2.AND_KW && binding_operator_suffix (peek_kind p 1) then
      (
        bump p;
        bump p;
        parse_let_binding p ~signature ~top_level:false;
        parse_parallel_bindings ()
      )
  in
  parse_parallel_bindings ();
  expect p Syntax_kind2.IN_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  complete p marker Syntax_kind2.BINDING_OPERATOR_EXPR

and parse_dot_bang_expr = fun p lhs ~signature ~stop_at_item ~stop_at_semi ->
  let marker = precede p lhs in
  bump p;
  bump p;
  if at p Syntax_kind2.LPAREN then
    (
      bump p;
      if not (at p Syntax_kind2.RPAREN || is_eof p) then
        ignore (parse_expression p ~signature ~stop_at_item:false 0);
      expect p Syntax_kind2.RPAREN (invalid_expression p)
    )
  else if can_start_atom (current_kind p) then
    ignore (parse_prefix_or_atom p ~signature ~stop_at_item ~stop_at_semi)
  else
    Event.Buffer.error p.events (invalid_expression p);
  complete p marker Syntax_kind2.LOCAL_OPEN_EXPR

and parse_label_pattern = fun p ~stop_type_at_arrow kind ->
  let marker = start_node p in
  bump p;
  if at p Syntax_kind2.LPAREN then
    (
      bump p;
      if not (at p Syntax_kind2.RPAREN || is_eof p) then
        parse_pattern ~stop_type_at_arrow:false p;
      if at p Syntax_kind2.EQ then
        (
          bump p;
          ignore (parse_expression p ~signature:false ~stop_at_item:false ~stop_at_semi:true 0)
        );
      expect p Syntax_kind2.RPAREN (invalid_pattern p);
      complete p marker Syntax_kind2.OPTIONAL_PARAM_DEFAULT
    )
  else (
    expect p Syntax_kind2.IDENT (invalid_pattern p);
    if at p Syntax_kind2.COLON then
      (
        bump p;
        parse_pattern ~stop_type_at_arrow p
      );
    complete p marker kind
  )

and parse_if_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.THEN_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  if at p Syntax_kind2.ELSE_KW then
    (
      bump p;
      ignore (parse_expression p ~signature ~stop_at_item 0)
    );
  complete p marker Syntax_kind2.IF_EXPR

and parse_match_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.WITH_KW (invalid_expression p);
  parse_match_cases p ~signature ~stop_at_item;
  complete p marker Syntax_kind2.MATCH_EXPR

and parse_try_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.WITH_KW (invalid_expression p);
  parse_match_cases p ~signature ~stop_at_item;
  complete p marker Syntax_kind2.TRY_EXPR

and parse_match_cases = fun p ~signature ~stop_at_item ->
  let rec loop () =
    if at p Syntax_kind2.PIPE then
      (
        let marker = start_node p in
        bump p;
        parse_pattern p;
        if at p Syntax_kind2.WHEN_KW then
          (
            bump p;
            ignore (parse_expression p ~signature ~stop_at_item:false 0)
          );
        expect p Syntax_kind2.ARROW (invalid_expression p);
        ignore (parse_expression p ~signature ~stop_at_item 0);
        ignore (complete p marker Syntax_kind2.MATCH_CASE);
        loop ()
      )
  in
  loop ()

and parse_fun_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  let rec parse_params () =
    if not (at p Syntax_kind2.ARROW || is_eof p) then
      (
        parse_pattern ~stop_type_at_arrow:false p;
        parse_params ()
      )
  in
  parse_params ();
  expect p Syntax_kind2.ARROW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  complete p marker Syntax_kind2.FUN_EXPR

and parse_function_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  parse_match_cases p ~signature ~stop_at_item;
  complete p marker Syntax_kind2.FUNCTION_EXPR

and parse_unary_keyword_expr = fun p ~signature ~stop_at_item kind ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item 70);
  complete p marker kind

and parse_while_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.DO_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  expect p Syntax_kind2.DONE_KW (invalid_expression p);
  complete p marker Syntax_kind2.WHILE_EXPR

and parse_for_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  parse_pattern ~stop_type_at_arrow:false p;
  expect p Syntax_kind2.EQ (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  if at p Syntax_kind2.TO_KW || at p Syntax_kind2.DOWNTO_KW then
    bump p
  else
    Event.Buffer.error p.events (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.DO_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  expect p Syntax_kind2.DONE_KW (invalid_expression p);
  complete p marker Syntax_kind2.FOR_EXPR

and parse_pattern = fun ?(stop_type_at_arrow = true) p ->
  ignore (parse_pattern_bp p ~stop_type_at_arrow 0)

and parse_pattern_bp = fun p ~stop_type_at_arrow min_bp ->
  let rec loop lhs =
    if is_attribute_suffix p then
      (
        let marker = precede p lhs in
        bump p;
        consume_attribute_sigils p;
        consume_balanced_until p ~closer:Syntax_kind2.RBRACKET 0;
        expect p Syntax_kind2.RBRACKET (invalid_pattern p);
        loop (complete p marker Syntax_kind2.ATTRIBUTE_PATTERN)
      )
    else
      match pattern_binding_power (current_kind p) with
    | Some bp when bp >= min_bp -> (
        match current_kind p with
        | Syntax_kind2.COLON ->
            let marker = precede p lhs in
            bump p;
            parse_type_expr p ~stop_at_arrow:stop_type_at_arrow;
            loop (complete p marker Syntax_kind2.CONSTRAINT_PATTERN)
        | Syntax_kind2.AS_KW ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_pattern_atom p ~stop_type_at_arrow);
            loop (complete p marker Syntax_kind2.ALIAS_PATTERN)
        | Syntax_kind2.COLONCOLON ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
            loop (complete p marker Syntax_kind2.CONS_PATTERN)
        | Syntax_kind2.DOTDOT ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
            loop (complete p marker Syntax_kind2.INTERVAL_PATTERN)
        | Syntax_kind2.PIPE ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
            loop (complete p marker Syntax_kind2.OR_PATTERN)
        | Syntax_kind2.COMMA ->
            let marker = precede p lhs in
            bump p;
            ignore (parse_pattern_bp p ~stop_type_at_arrow bp);
            loop (complete p marker Syntax_kind2.TUPLE_PATTERN)
        | _ ->
            lhs
      )
    | _ -> lhs
  in
  loop (parse_pattern_apply p ~stop_type_at_arrow)

and parse_pattern_apply = fun p ~stop_type_at_arrow ->
  let rec loop lhs =
    if is_attribute_suffix p then
      (
        let marker = precede p lhs in
        bump p;
        consume_attribute_sigils p;
        consume_balanced_until p ~closer:Syntax_kind2.RBRACKET 0;
        expect p Syntax_kind2.RBRACKET (invalid_pattern p);
        loop (complete p marker Syntax_kind2.ATTRIBUTE_PATTERN)
      )
    else if can_start_pattern_atom (current_kind p) then
      (
        let marker = precede p lhs in
        ignore (parse_pattern_atom p ~stop_type_at_arrow);
        loop (complete p marker Syntax_kind2.APPLY_PATTERN)
      )
    else
      lhs
  in
  loop (parse_pattern_atom p ~stop_type_at_arrow)

and parse_pattern_atom = fun p ~stop_type_at_arrow ->
  match current_kind p with
  | Syntax_kind2.UNDERSCORE -> parse_single_token_pattern p Syntax_kind2.WILDCARD_PATTERN
  | Syntax_kind2.IDENT -> parse_path_pattern p
  | Syntax_kind2.INT
  | Syntax_kind2.FLOAT
  | Syntax_kind2.STRING
  | Syntax_kind2.CHAR
  | Syntax_kind2.TRUE_KW
  | Syntax_kind2.FALSE_KW -> parse_single_token_pattern p Syntax_kind2.LITERAL_PATTERN
  | Syntax_kind2.PERCENT -> parse_single_token_pattern p Syntax_kind2.PATH_PATTERN
  | Syntax_kind2.LPAREN -> parse_parenthesized_pattern p
  | Syntax_kind2.LBRACKET ->
      if is_extension_shell p then
        parse_extension_pattern p
      else
        parse_list_pattern p
  | Syntax_kind2.LBRACKET_BAR -> parse_array_pattern p
  | Syntax_kind2.LBRACE -> parse_record_pattern p
  | Syntax_kind2.TILDE -> parse_label_pattern p ~stop_type_at_arrow Syntax_kind2.LABELED_PARAM
  | Syntax_kind2.QUESTION -> parse_label_pattern p ~stop_type_at_arrow Syntax_kind2.OPTIONAL_PARAM
  | Syntax_kind2.BACKTICK -> parse_poly_variant_pattern p ~stop_type_at_arrow
  | Syntax_kind2.HASH -> parse_poly_variant_inherit_pattern p
  | Syntax_kind2.LAZY_KW -> parse_unary_pattern p ~stop_type_at_arrow Syntax_kind2.LAZY_PATTERN
  | Syntax_kind2.EXCEPTION_KW -> parse_unary_pattern p ~stop_type_at_arrow Syntax_kind2.EXCEPTION_PATTERN
  | _ -> parse_error_pattern p

and parse_single_token_pattern = fun p kind ->
  let marker = start_node p in
  if is_eof p then
    (
      Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
      Event.Buffer.error p.events (invalid_pattern p)
    )
  else
    bump p;
  complete p marker kind

and parse_path_pattern = fun p ->
  let marker = start_node p in
  expect p Syntax_kind2.IDENT (invalid_pattern p);
  consume_path_segments p;
  if at p Syntax_kind2.DOT && peek_kind p 1 = Syntax_kind2.LPAREN then
    (
      bump p;
      bump p;
      if not (at p Syntax_kind2.RPAREN || is_eof p) then
        parse_pattern ~stop_type_at_arrow:false p;
      expect p Syntax_kind2.RPAREN (invalid_pattern p);
      complete p marker Syntax_kind2.LOCAL_OPEN_PATTERN
    )
  else
    complete p marker Syntax_kind2.PATH_PATTERN

and parse_parenthesized_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let at_closer () = at p Syntax_kind2.RPAREN || is_eof p in
  if at p Syntax_kind2.TYPE_KW then
    (
      bump p;
      let rec consume_type_names () =
        if at p Syntax_kind2.IDENT then
          (
            bump p;
            consume_type_names ()
          )
      in
      consume_type_names ();
      expect p Syntax_kind2.RPAREN (invalid_pattern p);
      complete p marker Syntax_kind2.LOCALLY_ABSTRACT_TYPE_PATTERN
    )
  else if at p Syntax_kind2.MODULE_KW then
    (
      bump p;
      if at p Syntax_kind2.IDENT || at p Syntax_kind2.UNDERSCORE then
        bump p
      else
        expect p Syntax_kind2.IDENT (invalid_pattern p);
      consume_balanced_until p ~closer:Syntax_kind2.RPAREN 0;
      expect p Syntax_kind2.RPAREN (invalid_pattern p);
      complete p marker Syntax_kind2.FIRST_CLASS_MODULE_PATTERN
    )
  else if binding_operator_keyword (current_kind p)
          && binding_operator_suffix (peek_kind p 1)
          && peek_kind p 2 = Syntax_kind2.RPAREN then
    (
      bump p;
      bump p;
      expect p Syntax_kind2.RPAREN (invalid_pattern p);
      complete p marker Syntax_kind2.PATH_PATTERN
    )
  else (
    if not (at_closer ()) then
      if parenthesized_operator_start p then
        consume_balanced_until p ~closer:Syntax_kind2.RPAREN 0
      else if operator_pattern_token (current_kind p) && peek_kind p 1 = Syntax_kind2.RPAREN then
        ignore (parse_single_token_pattern p Syntax_kind2.PATH_PATTERN)
      else
        parse_pattern ~stop_type_at_arrow:false p;
    let rec parse_comma_tail saw_comma =
      if at p Syntax_kind2.COMMA then
        (
          bump p;
          if not (at_closer ()) then
            parse_pattern ~stop_type_at_arrow:false p;
          parse_comma_tail true
        )
      else
        saw_comma
    in
    let saw_comma = parse_comma_tail false in
    expect p Syntax_kind2.RPAREN (invalid_pattern p);
    complete p marker
      (
        if saw_comma then
          Syntax_kind2.TUPLE_PATTERN
        else
          Syntax_kind2.PAREN_PATTERN
      )
  )

and parse_extension_pattern = fun p ->
  let marker = start_node p in
  bump p;
  consume_extension_sigils p;
  consume_extension_payload p;
  expect p Syntax_kind2.RBRACKET (invalid_pattern p);
  complete p marker Syntax_kind2.EXTENSION_PATTERN

and parse_list_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind2.RBRACKET || is_eof p) then
      (
        let before = p.pos in
        parse_pattern ~stop_type_at_arrow:false p;
        ensure_progress p before (invalid_pattern p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_elements ()
      )
  in
  parse_elements ();
  expect p Syntax_kind2.RBRACKET (invalid_pattern p);
  complete p marker Syntax_kind2.LIST_PATTERN

and parse_array_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_elements () =
    if not (at p Syntax_kind2.BAR_RBRACKET || is_eof p) then
      (
        let before = p.pos in
        parse_pattern ~stop_type_at_arrow:false p;
        ensure_progress p before (invalid_pattern p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_elements ()
      )
  in
  parse_elements ();
  expect p Syntax_kind2.BAR_RBRACKET (invalid_pattern p);
  complete p marker Syntax_kind2.ARRAY_PATTERN

and parse_record_pattern = fun p ->
  let marker = start_node p in
  bump p;
  let rec parse_fields () =
    if not (at p Syntax_kind2.RBRACE || is_eof p) then
      (
        let before = p.pos in
        if at p Syntax_kind2.UNDERSCORE then
          ignore (parse_single_token_pattern p Syntax_kind2.WILDCARD_PATTERN)
        else (
          ignore (parse_path_pattern p);
          if at p Syntax_kind2.EQ then
            (
              bump p;
              parse_pattern ~stop_type_at_arrow:false p
            )
        );
        ensure_progress p before (invalid_pattern p);
        ignore (bump_if p Syntax_kind2.SEMI);
        parse_fields ()
      )
  in
  parse_fields ();
  expect p Syntax_kind2.RBRACE (invalid_pattern p);
  complete p marker Syntax_kind2.RECORD_PATTERN

and parse_poly_variant_pattern = fun p ~stop_type_at_arrow ->
  let marker = start_node p in
  bump p;
  expect p Syntax_kind2.IDENT (invalid_pattern p);
  if can_start_pattern_atom (current_kind p) then
    parse_pattern ~stop_type_at_arrow p;
  complete p marker Syntax_kind2.POLY_VARIANT_PATTERN

and parse_poly_variant_inherit_pattern = fun p ->
  let marker = start_node p in
  bump p;
  ignore (parse_path_pattern p);
  complete p marker Syntax_kind2.POLY_VARIANT_PATTERN

and parse_unary_pattern = fun p ~stop_type_at_arrow kind ->
  let marker = start_node p in
  bump p;
  ignore (parse_pattern_atom p ~stop_type_at_arrow);
  complete p marker kind

and parse_error_pattern = fun p ->
  let marker = start_node p in
  Event.Buffer.error p.events (invalid_pattern p);
  if is_eof p then
    Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p)
  else
    bump p;
  complete p marker Syntax_kind2.ERROR

and parse_type_expr = fun p ~stop_at_arrow ->
  let marker = start_node p in
  let boundary depth =
    depth = 0
    && match current_kind p with
    | Syntax_kind2.EOF
    | Syntax_kind2.AS_KW
    | Syntax_kind2.PIPE
    | Syntax_kind2.WHEN_KW
    | Syntax_kind2.EQ
    | Syntax_kind2.COMMA
    | Syntax_kind2.RPAREN
    | Syntax_kind2.RBRACKET
    | Syntax_kind2.RBRACE
    | Syntax_kind2.BAR_RBRACKET
    | Syntax_kind2.SEMI -> true
    | Syntax_kind2.ARROW when stop_at_arrow -> true
    | _ -> false
  in
  let next_depth depth =
    match current_kind p with
    | Syntax_kind2.LPAREN
    | Syntax_kind2.LBRACE
    | Syntax_kind2.LBRACKET
    | Syntax_kind2.LBRACKET_BAR
    | Syntax_kind2.BEGIN_KW
    | Syntax_kind2.STRUCT_KW
    | Syntax_kind2.SIG_KW -> depth + 1
    | Syntax_kind2.RPAREN
    | Syntax_kind2.RBRACE
    | Syntax_kind2.RBRACKET
    | Syntax_kind2.BAR_RBRACKET
    | Syntax_kind2.END_KW when depth > 0 -> depth - 1
    | _ -> depth
  in
  let rec consume depth consumed =
    if is_eof p || (consumed && boundary depth) then
      ()
    else if (not consumed) && boundary depth then
      (
        Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
        Event.Buffer.error p.events (invalid_type_expression p)
      )
    else
      (
        let depth = next_depth depth in
        bump p;
        consume depth true
      )
  in
  consume 0 false;
  ignore (complete p marker Syntax_kind2.TYPE_EXPR)

and parse_let_binding = fun p ~signature ~top_level ->
  let marker = start_node p in
  parse_pattern ~stop_type_at_arrow:false p;
  let rec parse_params () =
    if not (at p Syntax_kind2.EQ || is_eof p || (top_level && at_item_boundary p ~signature)) then
      if at p Syntax_kind2.COLON then
        (
          bump p;
          parse_type_expr p ~stop_at_arrow:false
        )
      else (
        parse_pattern ~stop_type_at_arrow:false p;
        parse_params ()
      )
  in
  parse_params ();
  expect p Syntax_kind2.EQ (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:top_level 0);
  ignore (complete p marker Syntax_kind2.LET_BINDING)

let consume_opaque_until_item_boundary = fun p ~signature kind diagnostic ->
  let marker = start_node p in
  let next_depth depth =
    match current_kind p with
    | Syntax_kind2.LPAREN
    | Syntax_kind2.LBRACE
    | Syntax_kind2.LBRACKET
    | Syntax_kind2.LBRACKET_BAR
    | Syntax_kind2.BEGIN_KW
    | Syntax_kind2.STRUCT_KW
    | Syntax_kind2.SIG_KW -> depth + 1
    | Syntax_kind2.RPAREN
    | Syntax_kind2.RBRACE
    | Syntax_kind2.RBRACKET
    | Syntax_kind2.BAR_RBRACKET
    | Syntax_kind2.END_KW when depth > 0 -> depth - 1
    | _ -> depth
  in
  let rec loop depth consumed =
    if not (is_eof p || (consumed && depth = 0 && at_item_boundary p ~signature)) then
      (
        let depth = next_depth depth in
        bump p;
        loop depth true
      )
  in
  loop 0 false;
  if Event.Buffer.length p.events = 0 then
    Event.Buffer.error p.events diagnostic;
  ignore (complete p marker kind)

let parse_let_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind2.LET_KW (invalid_expression p);
  ignore (bump_if p Syntax_kind2.REC_KW);
  parse_let_binding p ~signature ~top_level:true;
  let rec parse_and_bindings () =
    if at p Syntax_kind2.AND_KW then
      (
        bump p;
        parse_let_binding p ~signature ~top_level:true;
        parse_and_bindings ()
      )
  in
  parse_and_bindings ();
  let kind =
    if (not signature) && at p Syntax_kind2.IN_KW then
      (
        bump p;
        ignore (parse_expression p ~signature ~stop_at_item:true 0);
        Syntax_kind2.LET_EXPR
      )
    else
      Syntax_kind2.LET_DECL
  in
  ignore (complete p marker kind)

let parse_opaque_decl = fun p ~signature kind diagnostic ->
  consume_opaque_until_item_boundary p ~signature kind diagnostic

let parse_expr_item = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_expression p ~signature ~stop_at_item:true 0);
  ignore (complete p marker Syntax_kind2.EXPR_ITEM)

let parse_bracketed_item_shell = fun p kind ->
  let marker = start_node p in
  bump p;
  (
    match kind with
    | Syntax_kind2.EXTENSION_ITEM ->
        consume_extension_sigils p;
        consume_extension_payload p
    | Syntax_kind2.ATTRIBUTE_ITEM ->
        consume_attribute_sigils p;
        consume_balanced_until p ~closer:Syntax_kind2.RBRACKET 0
    | _ -> ()
  );
  expect p Syntax_kind2.RBRACKET (invalid_expression p);
  ignore (complete p marker kind)

let parse_structure_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind2.LBRACKET when is_extension_shell p ->
        parse_bracketed_item_shell p Syntax_kind2.EXTENSION_ITEM
    | Syntax_kind2.LBRACKET when is_attribute_shell p ->
        parse_bracketed_item_shell p Syntax_kind2.ATTRIBUTE_ITEM
    | Syntax_kind2.LET_KW when binding_operator_suffix (peek_kind p 1) ->
        parse_expr_item p ~signature:false
    | Syntax_kind2.LET_KW
      when peek_kind p 1 = Syntax_kind2.OPEN_KW || peek_kind p 1 = Syntax_kind2.MODULE_KW ->
        parse_expr_item p ~signature:false
    | Syntax_kind2.LET_KW ->
        parse_let_decl p ~signature:false
    | Syntax_kind2.TYPE_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.TYPE_DECL (invalid_type_expression p)
    | Syntax_kind2.MODULE_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.MODULE_DECL (invalid_expression p)
    | Syntax_kind2.OPEN_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.OPEN_DECL (invalid_expression p)
    | Syntax_kind2.INCLUDE_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.INCLUDE_DECL (invalid_expression p)
    | Syntax_kind2.EXTERNAL_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.EXTERNAL_DECL (invalid_expression p)
    | Syntax_kind2.EXCEPTION_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.EXCEPTION_DECL (invalid_expression p)
    | Syntax_kind2.CLASS_KW ->
        parse_opaque_decl p ~signature:false Syntax_kind2.CLASS_DECL (invalid_expression p)
    | Syntax_kind2.OBJECT_KW ->
        parse_expr_item p ~signature:false
    | _ ->
        parse_expr_item p ~signature:false
  );
  ignore (complete p marker Syntax_kind2.STRUCTURE_ITEM)

let parse_signature_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind2.LBRACKET when is_extension_shell p ->
        parse_bracketed_item_shell p Syntax_kind2.EXTENSION_ITEM
    | Syntax_kind2.LBRACKET when is_attribute_shell p ->
        parse_bracketed_item_shell p Syntax_kind2.ATTRIBUTE_ITEM
    | Syntax_kind2.VAL_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.VAL_DECL (invalid_type_expression p)
    | Syntax_kind2.TYPE_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.TYPE_DECL (invalid_type_expression p)
    | Syntax_kind2.MODULE_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.MODULE_DECL (invalid_expression p)
    | Syntax_kind2.OPEN_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.OPEN_DECL (invalid_expression p)
    | Syntax_kind2.INCLUDE_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.INCLUDE_DECL (invalid_expression p)
    | Syntax_kind2.EXTERNAL_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.EXTERNAL_DECL (invalid_expression p)
    | Syntax_kind2.EXCEPTION_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.EXCEPTION_DECL (invalid_expression p)
    | Syntax_kind2.CLASS_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.CLASS_DECL (invalid_expression p)
    | Syntax_kind2.OBJECT_KW ->
        parse_opaque_decl p ~signature:true Syntax_kind2.CLASS_DECL (invalid_expression p)
    | _ ->
        Event.Buffer.error p.events (invalid_expression p);
        parse_opaque_decl p ~signature:true Syntax_kind2.ERROR (invalid_expression p)
  );
  ignore (complete p marker Syntax_kind2.SIGNATURE_ITEM)

let rec consume_phrase_separators = fun p ->
  if at p Syntax_kind2.SEMI && peek_kind p 1 = Syntax_kind2.SEMI then
    (
      bump p;
      bump p;
      consume_phrase_separators p
    )

let parse_file = fun kind source ->
  let p = create source in
  let root = start_node p in
  let body = start_node p in
  let rec parse_structure_items () =
    consume_phrase_separators p;
    if not (is_eof p) then
      (
        parse_structure_item p;
        parse_structure_items ()
      )
  in
  let rec parse_signature_items () =
    consume_phrase_separators p;
    if not (is_eof p) then
      (
        parse_signature_item p;
        parse_signature_items ()
      )
  in
  (
    match kind with
    | `Implementation ->
        parse_structure_items ();
        ignore (complete p body Syntax_kind2.IMPLEMENTATION)
    | `Interface ->
        parse_signature_items ();
        ignore (complete p body Syntax_kind2.INTERFACE)
  );
  if is_eof p then
    bump p;
  ignore (complete p root Syntax_kind2.SOURCE_FILE);
  let tree = Syntax_tree.build ~source ~token_stream:p.token_stream ~events:p.events in
  {
    source;
    kind;
    tokens = p.token_stream;
    events = p.events;
    tree;
    diagnostics = Event.Buffer.diagnostics p.events;
  }

let parse_implementation = fun source -> parse_file `Implementation source

let parse_interface = fun source -> parse_file `Interface source

let parse = fun ~filename source ->
  match Path.extension filename with
  | Some ".mli" -> parse_interface source
  | _ -> parse_implementation source
