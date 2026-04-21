open Std
open Std.Collections

type file_kind =
  [
    | `Implementation
    | `Interface
  ]

type parse_result = {
  source: string;
  kind: file_kind;
  tokens: Raw_token.stream;
  events: Event.Buffer.t;
  tree: Syntax_tree.t;
  diagnostics: Diagnostic.t Vector.t;
}

type parser = {
  source: string;
  token_stream: Raw_token.stream;
  events: Event.Buffer.t;
  mutable pos: int;
}

let create = fun source ->
  let token_stream = Lexer.tokenize source |> Raw_token.of_lexer_tokens in
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

let token_text = fun p raw -> Raw_token.text ~source:p.source raw

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

let complete = fun p marker kind -> Event.Buffer.complete p.events marker kind

let precede = fun p completed -> Event.Buffer.precede p.events completed

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
  | Syntax_kind2.DOCSTRING -> String.contains (token_text p raw) "\n"
  | _ -> false

let leading_trivia_contains_newline = fun p ->
  let current_raw = current_raw_index p in
  let previous_raw =
    if p.pos <= 0 then
      -1
    else
      significant_raw_at p (p.pos - 1)
  in
  let found = ref false in
  let index = ref (previous_raw + 1) in
  while (not !found) && !index < current_raw && !index < raw_count p do
    if raw_contains_newline p !index then
      found := true;
    index := !index + 1
  done;
  !found

let at_item_boundary = fun p ~signature ->
  if is_eof p then
    true
  else if not (leading_trivia_contains_newline p) then
    false
  else if signature then
    starts_signature_item (current_kind p)
  else
    starts_structure_item (current_kind p)

let expression_boundary = fun p ~stop_at_item ~signature ->
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
  | Syntax_kind2.SEMI
  | Syntax_kind2.RPAREN
  | Syntax_kind2.RBRACKET
  | Syntax_kind2.RBRACE
  | Syntax_kind2.BAR_RBRACKET
  | Syntax_kind2.END_KW
  | Syntax_kind2.DONE_KW -> true
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
  | Syntax_kind2.MINUS
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.BANG -> true
  | _ -> false

let prefix_operator = function
  | Syntax_kind2.MINUS
  | Syntax_kind2.PLUSDOT
  | Syntax_kind2.MINUSDOT
  | Syntax_kind2.BANG -> true
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
  | Syntax_kind2.GTE -> Some 20
  | Syntax_kind2.COLONCOLON
  | Syntax_kind2.AT
  | Syntax_kind2.ATAT
  | Syntax_kind2.PIPEGT -> Some 30
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
  | Syntax_kind2.OPERATOR_KW -> Some 50
  | Syntax_kind2.STARSTAR -> Some 60
  | _ -> None

let rec parse_expression = fun p ~signature ~stop_at_item min_bp ->
  let lhs = ref (parse_prefix_or_atom p ~signature ~stop_at_item) in
  let continue = ref true in
  while !continue do
    if expression_boundary p ~stop_at_item ~signature then
      continue := false
    else if can_start_atom (current_kind p) then
      let marker = precede p !lhs in
      let _argument = parse_prefix_or_atom p ~signature ~stop_at_item in
      lhs := complete p marker Syntax_kind2.APPLY_EXPR
    else
      match infix_binding_power (current_kind p) with
      | Some bp when bp >= min_bp ->
          let marker = precede p !lhs in
          bump p;
          let _rhs = parse_expression p ~signature ~stop_at_item (bp + 1) in
          lhs := complete p marker Syntax_kind2.INFIX_EXPR
      | _ ->
          continue := false
  done;
  !lhs

and parse_prefix_or_atom = fun p ~signature ~stop_at_item ->
  match current_kind p with
  | kind when prefix_operator kind ->
      let marker = start_node p in
      bump p;
      let _operand = parse_expression p ~signature ~stop_at_item 70 in
      complete p marker Syntax_kind2.PREFIX_EXPR
  | Syntax_kind2.LET_KW -> parse_let_expr p ~signature
  | Syntax_kind2.IF_KW -> parse_if_expr p ~signature ~stop_at_item
  | Syntax_kind2.MATCH_KW -> parse_match_expr p ~signature ~stop_at_item
  | Syntax_kind2.FUN_KW -> parse_fun_expr p ~signature ~stop_at_item
  | Syntax_kind2.FUNCTION_KW -> parse_function_expr p ~signature ~stop_at_item
  | Syntax_kind2.TRY_KW -> parse_try_expr p ~signature ~stop_at_item
  | Syntax_kind2.ASSERT_KW -> parse_unary_keyword_expr p ~signature ~stop_at_item Syntax_kind2.ASSERT_EXPR
  | Syntax_kind2.LAZY_KW -> parse_unary_keyword_expr p ~signature ~stop_at_item Syntax_kind2.LAZY_EXPR
  | Syntax_kind2.WHILE_KW -> parse_while_expr p ~signature ~stop_at_item
  | Syntax_kind2.FOR_KW -> parse_for_expr p ~signature ~stop_at_item
  | Syntax_kind2.IDENT -> parse_path_expr p
  | Syntax_kind2.INT
  | Syntax_kind2.FLOAT
  | Syntax_kind2.STRING
  | Syntax_kind2.CHAR
  | Syntax_kind2.TRUE_KW
  | Syntax_kind2.FALSE_KW -> parse_literal_expr p
  | Syntax_kind2.LPAREN
  | Syntax_kind2.BEGIN_KW -> parse_parenthesized_expr p ~signature ~stop_at_item
  | Syntax_kind2.LBRACKET -> parse_list_expr p ~signature
  | Syntax_kind2.LBRACKET_BAR -> parse_array_expr p ~signature
  | Syntax_kind2.LBRACE -> parse_record_expr p ~signature
  | _ ->
      let marker = start_node p in
      Event.Buffer.error p.events (invalid_expression p);
      if not (is_eof p) then bump p else Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
      complete p marker Syntax_kind2.ERROR

and parse_path_expr = fun p ->
  let marker = start_node p in
  expect p Syntax_kind2.IDENT (invalid_expression p);
  while at p Syntax_kind2.DOT && peek_kind p 1 = Syntax_kind2.IDENT do
    bump p;
    bump p
  done;
  complete p marker Syntax_kind2.PATH_EXPR

and parse_literal_expr = fun p ->
  let marker = start_node p in
  bump p;
  complete p marker Syntax_kind2.LITERAL_EXPR

and parse_parenthesized_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  let opener = current_kind p in
  bump p;
  let saw_comma = ref false in
  if not (at p Syntax_kind2.RPAREN || at p Syntax_kind2.END_KW || is_eof p) then
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
  while at p Syntax_kind2.COMMA do
    saw_comma := true;
    bump p;
    if not (at p Syntax_kind2.RPAREN || at p Syntax_kind2.END_KW || is_eof p) then
      ignore (parse_expression p ~signature ~stop_at_item:false 0)
  done;
  (
    match opener with
    | Syntax_kind2.BEGIN_KW -> expect p Syntax_kind2.END_KW (invalid_expression p)
    | _ -> expect p Syntax_kind2.RPAREN (invalid_expression p)
  );
  complete p marker (if !saw_comma then Syntax_kind2.TUPLE_EXPR else Syntax_kind2.PAREN_EXPR)

and parse_list_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  while not (at p Syntax_kind2.RBRACKET || is_eof p) do
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
    ignore (bump_if p Syntax_kind2.SEMI)
  done;
  expect p Syntax_kind2.RBRACKET (invalid_expression p);
  complete p marker Syntax_kind2.LIST_EXPR

and parse_array_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  while not (at p Syntax_kind2.BAR_RBRACKET || is_eof p) do
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
    ignore (bump_if p Syntax_kind2.SEMI)
  done;
  expect p Syntax_kind2.BAR_RBRACKET (invalid_expression p);
  complete p marker Syntax_kind2.ARRAY_EXPR

and parse_record_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  while not (at p Syntax_kind2.RBRACE || is_eof p) do
    ignore (parse_expression p ~signature ~stop_at_item:false 0);
    if at p Syntax_kind2.EQ then
      (
        bump p;
        ignore (parse_expression p ~signature ~stop_at_item:false 0)
      );
    ignore (bump_if p Syntax_kind2.SEMI)
  done;
  expect p Syntax_kind2.RBRACE (invalid_expression p);
  complete p marker Syntax_kind2.RECORD_EXPR

and parse_let_expr = fun p ~signature ->
  let marker = start_node p in
  bump p;
  ignore (bump_if p Syntax_kind2.REC_KW);
  parse_let_binding p ~signature ~top_level:false;
  while at p Syntax_kind2.AND_KW do
    bump p;
    parse_let_binding p ~signature ~top_level:false
  done;
  expect p Syntax_kind2.IN_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  complete p marker Syntax_kind2.LET_EXPR

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
  while at p Syntax_kind2.PIPE do
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
    ignore (complete p marker Syntax_kind2.MATCH_CASE)
  done

and parse_fun_expr = fun p ~signature ~stop_at_item ->
  let marker = start_node p in
  bump p;
  while not (at p Syntax_kind2.ARROW || is_eof p) do
    parse_pattern p
  done;
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
  parse_pattern p;
  expect p Syntax_kind2.EQ (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  if at p Syntax_kind2.TO_KW || at p Syntax_kind2.DOWNTO_KW then bump p else Event.Buffer.error p.events (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:false 0);
  expect p Syntax_kind2.DO_KW (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item 0);
  expect p Syntax_kind2.DONE_KW (invalid_expression p);
  complete p marker Syntax_kind2.FOR_EXPR

and parse_pattern = fun p ->
  let marker = start_node p in
  if is_eof p then
    (
      Event.Buffer.missing p.events ~kind:Syntax_kind2.IDENT ~offset:(current_offset p);
      Event.Buffer.error p.events (invalid_pattern p)
    )
  else
    bump p;
  ignore (complete p marker Syntax_kind2.PATTERN)

and parse_let_binding = fun p ~signature ~top_level ->
  let marker = start_node p in
  parse_pattern p;
  while not (at p Syntax_kind2.EQ || is_eof p || (top_level && at_item_boundary p ~signature)) do
    parse_pattern p
  done;
  expect p Syntax_kind2.EQ (invalid_expression p);
  ignore (parse_expression p ~signature ~stop_at_item:top_level 0);
  ignore (complete p marker Syntax_kind2.LET_BINDING)

let consume_opaque_until_item_boundary = fun p ~signature kind diagnostic ->
  let marker = start_node p in
  let depth = ref 0 in
  while not (is_eof p) && (!depth > 0 || not (at_item_boundary p ~signature)) do
    (
      match current_kind p with
      | Syntax_kind2.LPAREN
      | Syntax_kind2.LBRACE
      | Syntax_kind2.LBRACKET
      | Syntax_kind2.LBRACKET_BAR
      | Syntax_kind2.BEGIN_KW
      | Syntax_kind2.STRUCT_KW
      | Syntax_kind2.SIG_KW -> depth := !depth + 1
      | Syntax_kind2.RPAREN
      | Syntax_kind2.RBRACE
      | Syntax_kind2.RBRACKET
      | Syntax_kind2.BAR_RBRACKET
      | Syntax_kind2.END_KW when !depth > 0 -> depth := !depth - 1
      | _ -> ()
    );
    bump p
  done;
  if Event.Buffer.length p.events = 0 then Event.Buffer.error p.events diagnostic;
  ignore (complete p marker kind)

let parse_let_decl = fun p ~signature ->
  let marker = start_node p in
  expect p Syntax_kind2.LET_KW (invalid_expression p);
  ignore (bump_if p Syntax_kind2.REC_KW);
  parse_let_binding p ~signature ~top_level:true;
  while at p Syntax_kind2.AND_KW do
    bump p;
    parse_let_binding p ~signature ~top_level:true
  done;
  ignore (complete p marker Syntax_kind2.LET_DECL)

let parse_opaque_decl = fun p ~signature kind diagnostic ->
  consume_opaque_until_item_boundary p ~signature kind diagnostic

let parse_expr_item = fun p ~signature ->
  let marker = start_node p in
  ignore (parse_expression p ~signature ~stop_at_item:true 0);
  ignore (complete p marker Syntax_kind2.EXPR_ITEM)

let parse_structure_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind2.LET_KW -> parse_let_decl p ~signature:false
    | Syntax_kind2.TYPE_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.TYPE_DECL (invalid_type_expression p)
    | Syntax_kind2.MODULE_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.MODULE_DECL (invalid_expression p)
    | Syntax_kind2.OPEN_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.OPEN_DECL (invalid_expression p)
    | Syntax_kind2.INCLUDE_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.INCLUDE_DECL (invalid_expression p)
    | Syntax_kind2.EXTERNAL_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.EXTERNAL_DECL (invalid_expression p)
    | Syntax_kind2.EXCEPTION_KW -> parse_opaque_decl p ~signature:false Syntax_kind2.EXCEPTION_DECL (invalid_expression p)
    | Syntax_kind2.CLASS_KW
    | Syntax_kind2.OBJECT_KW ->
        Event.Buffer.error p.events (invalid_expression p);
        parse_opaque_decl p ~signature:false Syntax_kind2.ERROR (invalid_expression p)
    | _ -> parse_expr_item p ~signature:false
  );
  ignore (complete p marker Syntax_kind2.STRUCTURE_ITEM)

let parse_signature_item = fun p ->
  let marker = start_node p in
  (
    match current_kind p with
    | Syntax_kind2.VAL_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.VAL_DECL (invalid_type_expression p)
    | Syntax_kind2.TYPE_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.TYPE_DECL (invalid_type_expression p)
    | Syntax_kind2.MODULE_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.MODULE_DECL (invalid_expression p)
    | Syntax_kind2.OPEN_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.OPEN_DECL (invalid_expression p)
    | Syntax_kind2.INCLUDE_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.INCLUDE_DECL (invalid_expression p)
    | Syntax_kind2.EXTERNAL_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.EXTERNAL_DECL (invalid_expression p)
    | Syntax_kind2.EXCEPTION_KW -> parse_opaque_decl p ~signature:true Syntax_kind2.EXCEPTION_DECL (invalid_expression p)
    | Syntax_kind2.CLASS_KW
    | Syntax_kind2.OBJECT_KW ->
        Event.Buffer.error p.events (invalid_expression p);
        parse_opaque_decl p ~signature:true Syntax_kind2.ERROR (invalid_expression p)
    | _ ->
        Event.Buffer.error p.events (invalid_expression p);
        parse_opaque_decl p ~signature:true Syntax_kind2.ERROR (invalid_expression p)
  );
  ignore (complete p marker Syntax_kind2.SIGNATURE_ITEM)

let parse_file = fun kind source ->
  let p = create source in
  let root = start_node p in
  let body = start_node p in
  (
    match kind with
    | `Implementation ->
        while not (is_eof p) do
          parse_structure_item p
        done;
        ignore (complete p body Syntax_kind2.IMPLEMENTATION)
    | `Interface ->
        while not (is_eof p) do
          parse_signature_item p
        done;
        ignore (complete p body Syntax_kind2.INTERFACE)
  );
  if is_eof p then bump p;
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
