open Std

(** New Parser Implementation

    Following the principles from NEW_PARSER.md:
    - Always return nodes (never options)
    - Explicit trivia control (not everywhere!)
    - Flat function structure (no nesting)
    - Grammar-driven (one function per EBNF rule)
    - TDD approach with test_runner.py *)

type parse_result = {
  tree : (Syntax_kind.t, string) Ceibo.Green.node;
  diagnostics : Diagnostic.t list;
}
(** Parse result type *)

type parser = {
  cursor : Token_cursor.t;
  diagnostics : Diagnostic.t list Cell.t;
}
(** Parser state *)

(** Create a new parser from tokens *)
let create ~source tokens =
  { cursor = Token_cursor.create ~source tokens; diagnostics = Cell.create [] }

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

(** Report a diagnostic *)
let report_diagnostic parser diag =
  let current_diags = Cell.get parser.diagnostics in
  Cell.set parser.diagnostics (diag :: current_diags)

(** Get current span for error reporting *)
let current_span parser =
  let token = peek parser in
  token.Token.span

(** Get span pointing to end of last consumed token (for "expected X" errors) *)
let expected_span parser =
  let last_tok = Token_cursor.last_token parser.cursor in
  let end_pos = last_tok.Token.span.end_ in
  { Ceibo.Span.start = end_pos; end_ = end_pos }

(** Get text of a token from source *)
let token_text parser token = Token_cursor.view parser.cursor token.Token.span

(** Consume a single token WITHOUT consuming trivia after it.

    IMPORTANT: This is the primitive operation. It does NOT auto-consume trivia!
    Call consume_trivia explicitly where grammar allows it. *)
let consume parser =
  let token = peek parser in
  advance parser;
  token

(** Check if a token kind is trivia *)
let is_trivia_kind = function
  | Token.Comment _ | Token.Docstring _ | Token.Whitespace -> true
  | _ -> false

(** Consume trivia tokens (whitespace, comments).

    Only call this where the grammar explicitly allows trivia! Not all positions
    in OCaml allow trivia (e.g., between ' and type var name). *)
let consume_trivia parser =
  let rec loop acc =
    let kind = peek_kind parser in
    if is_trivia_kind kind then
      let token = consume parser in
      loop (token :: acc)
    else List.rev acc
  in
  loop []

(** Error recovery: skip tokens until we reach a synchronization point.

    This helps prevent cascading errors by consuming tokens after an error until
    we reach a point where parsing can meaningfully continue.

    @param parser The parser state
    @param sync_tokens List of token kinds to stop at (but not consume)
    @return List of consumed tokens during recovery *)
let error_recover_until parser ~sync_tokens =
  let is_sync_token kind = List.exists (fun sync -> sync = kind) sync_tokens in
  let rec skip_to_sync acc =
    match peek_kind parser with
    | Token.EOF -> List.rev acc
    | kind when is_sync_token kind -> List.rev acc
    | Token.Whitespace
      when String.contains (token_text parser (peek parser)) '\n' ->
        (* Stop at newline but don't consume it *)
        List.rev acc
    | _ ->
        let tok = consume parser in
        skip_to_sync (tok :: acc)
  in
  skip_to_sync []

(** Expect a specific token kind. If found, consume and return it.
    If not found, report diagnostic and return the found token without consuming.
    
    @param parser The parser state
    @param expected_kind The token kind we expect
    @param diagnostic_fn Function to create diagnostic given the found token
    @return The expected token if found, otherwise the found token (as dummy) *)
let expect parser expected_kind diagnostic_fn =
  if peek_kind parser = expected_kind then consume parser
  else
    let found = peek parser in
    let diagnostic = diagnostic_fn found in
    report_diagnostic parser diagnostic;
    found (* Return found token as dummy - don't consume for error recovery *)

(** Parse content within parentheses: (content)
    Returns all the parts needed to build a node.
    
    @param parser The parser state
    @param content_parser Function to parse the content inside parens
    @return Tuple of (open_paren, trivia_after_open, content, trivia_before_close, close_paren) *)
let parse_parens parser content_parser =
  (* Expect open paren *)
  let open_paren =
    expect parser (Token.OpenDelim Token.Paren) (fun found ->
        Diagnostic.unclosed_delimiter ~opener:"(" ~found
          ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  let trivia_after_open = consume_trivia parser in

  (* Parse content *)
  let content = content_parser parser in
  let trivia_before_close = consume_trivia parser in

  (* Expect close paren *)
  let close_paren =
    expect parser (Token.CloseDelim Token.Paren) (fun found ->
        Diagnostic.unclosed_delimiter ~opener:")" ~found
          ~text:(token_text parser found)
          ~span:(expected_span parser))
  in

  (open_paren, trivia_after_open, content, trivia_before_close, close_paren)

(** Parse and expect an identifier.
    Reports diagnostic if not an identifier.
    
    @param parser The parser state
    @return The identifier token *)
let parse_ident parser =
  match peek_kind parser with
  | Token.Ident _ -> consume parser
  | _ ->
      let found = peek parser in
      report_diagnostic parser
        (Diagnostic.invalid_expression ~found ~text:(token_text parser found)
           ~span:(expected_span parser));
      found (* Return found token as dummy *)

(** Convert Token.token_kind to Syntax_kind.t

    For now, we map all tokens to expression/pattern kinds. TODO: Add proper
    token-level syntax kinds to Syntax_kind.t *)
let token_kind_to_syntax_kind = function
  | Token.Whitespace -> Syntax_kind.WHITESPACE
  | Token.Comment _ -> Syntax_kind.COMMENT
  | Token.Docstring _ -> Syntax_kind.DOCSTRING
  | Token.Literal (Token.Int _) -> Syntax_kind.INT_LITERAL
  | Token.Literal (Token.Float _) -> Syntax_kind.FLOAT_LITERAL
  | Token.Literal (Token.String _) -> Syntax_kind.STRING_LITERAL
  | Token.Literal (Token.Char _) -> Syntax_kind.CHAR_LITERAL
  | Token.Keyword Keyword.True | Token.Keyword Keyword.False ->
      Syntax_kind.BOOL_LITERAL
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Underscore -> Syntax_kind.WILDCARD_PATTERN
  | _ -> Syntax_kind.IDENT_EXPR (* Catch-all: treat as identifier for now *)

(** Make a green tree node from children *)
let make_node kind children =
  let children_array = Array.of_list children in
  Ceibo.Green.make_node ~kind ~children:children_array

(** Make a token green element *)
let make_token parser token =
  let token_kind = token.Token.kind in
  let syntax_kind = token_kind_to_syntax_kind token_kind in
  let text = Token_cursor.view parser.cursor token.Token.span in
  let width = String.length text in
  let green_token = Ceibo.Green.make_token ~kind:syntax_kind ~text ~width in
  Ceibo.Green.Token green_token

(** Convert list of tokens to green elements *)
let tokens_to_green parser tokens = List.map (make_token parser) tokens

(** Make an ERROR node with diagnostic *)
let make_error_node parser ~diagnostic ~consumed_tokens =
  report_diagnostic parser diagnostic;

  (* Wrap consumed tokens in ERROR node *)
  let children = tokens_to_green parser consumed_tokens in
  make_node Syntax_kind.ERROR children

(** *
    ============================================================================
    * GRAMMAR SECTION 1: LEXICAL CONVENTIONS *
    ============================================================================
*)

(** Get operator precedence and associativity. Returns (precedence,
    is_right_associative). Higher precedence = tighter binding. *)
let operator_info = function
  | Token.Or -> Some (1, false)
  | Token.And -> Some (2, false)
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.EqEq | Token.BangEq ->
      Some (3, false)
  | Token.At | Token.Caret | Token.AtAt -> Some (4, true)
  | Token.ColonColon -> Some (5, true)
  | Token.Plus | Token.Minus | Token.PlusDot | Token.MinusDot -> Some (6, false)
  | Token.Star | Token.Slash | Token.Percent | Token.StarDot | Token.SlashDot ->
      Some (7, false)
  | Token.StarStar -> Some (8, true)
  | _ -> None

(** Parse type variable: "'" ident

    CRITICAL: No trivia allowed between ' and ident! Grammar: typexpr ::= "'"
    ident *)
let rec parse_type_variable parser =
  match peek_kind parser with
  | Token.Quote -> (
      let quote = consume parser in

      (* IMMEDIATELY get identifier - NO trivia! *)
      match peek_kind parser with
      | Token.Ident name ->
          let ident = consume parser in
          let ident_text = token_text parser ident in
          (* Check if type variable starts with uppercase *)
          let first_char =
            if String.length ident_text > 0 then String.get ident_text 0
            else 'a'
          in
          if first_char >= 'A' && first_char <= 'Z' then
            let diagnostic =
              Diagnostic.uppercase_type_variable ~text:ident_text ~found:ident
                ~text_found:ident_text
                ~span:
                  (Ceibo.Span.make ~start:quote.Token.span.start
                     ~end_:ident.Token.span.end_)
            in
            make_error_node parser ~diagnostic ~consumed_tokens:[ quote; ident ]
          else
            make_node Syntax_kind.TYPE_VAR
              [ make_token parser quote; make_token parser ident ]
      | found ->
          (* Error: expected identifier after quote *)
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.malformed_type_variable ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(current_span parser)
          in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ])
  | found ->
      (* Error: expected quote to start type variable *)
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.malformed_type_variable ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 4: TYPE EXPRESSIONS *
    ============================================================================
*)

(** Parse type parameters: 'a or ('a, 'b, 'c) Returns (type_params_green,
    trivia_tokens) *)
and parse_type_params parser =
  match peek_kind parser with
  | Token.Quote ->
      (* Single type parameter: 'a *)
      let param = parse_typexpr parser in
      let trivia_tokens = consume_trivia parser in
      ([ Ceibo.Green.Node param ], trivia_tokens)
  | Token.OpenDelim Token.Paren -> (
      (* Multiple type parameters: ('a, 'b, 'c) *)
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in

      (* Parse comma-separated list of type variables *)
      let rec parse_params acc =
        match peek_kind parser with
        | Token.CloseDelim Token.Paren ->
            (* End of parameter list *)
            List.rev acc
        | Token.Quote -> (
            let param = parse_typexpr parser in
            let trivia_tokens = consume_trivia parser in
            let trivia_green = tokens_to_green parser trivia_tokens in
            (* Check for comma or closing paren *)
            match peek_kind parser with
            | Token.Comma ->
                let comma = consume parser in
                let trivia2_tokens = consume_trivia parser in
                let trivia2_green = tokens_to_green parser trivia2_tokens in
                parse_params
                  (trivia2_green
                  @ [ make_token parser comma ]
                  @ trivia_green @ [ Ceibo.Green.Node param ] @ acc)
            | Token.CloseDelim Token.Paren ->
                List.rev (trivia_green @ [ Ceibo.Green.Node param ] @ acc)
            | _ ->
                (* Expected comma or ) *)
                List.rev (trivia_green @ [ Ceibo.Green.Node param ] @ acc))
        | _ ->
            (* Invalid token in type params *)
            List.rev acc
      in

      let params = parse_params [] in
      let trivia_before_close = consume_trivia parser in

      (* Expect closing paren *)
      match peek_kind parser with
      | Token.CloseDelim Token.Paren ->
          let rparen = consume parser in
          let trivia_after_rparen = consume_trivia parser in
          ( [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ params
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser rparen ],
            trivia_after_rparen )
      | _ ->
          (* Missing closing paren *)
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.unclosed_type_params ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser)
          in
          report_diagnostic parser diagnostic;
          ( [ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ params
            @ tokens_to_green parser trivia_before_close,
            [] ))
  | _ ->
      (* No type parameters *)
      ([], [])

(** Parse type expression dispatcher *)
and parse_typexpr parser =
  match peek_kind parser with
  | Token.Quote -> parse_type_variable parser
  | Token.Ident _ ->
      (* Type constructor name: int, string, list, etc. *)
      let ident = consume parser in
      make_node Syntax_kind.TYPE_CONSTR [ make_token parser ident ]
  | _ ->
      (* Error: invalid type expression *)
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_type_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 6: PATTERNS *
    ============================================================================
*)

(** Parse pattern - for now just identifiers *)
and parse_pattern parser = parse_or_pattern parser

(** Parse or-pattern: p1 | p2 | p3 *)
and parse_or_pattern parser =
  let left = parse_cons_pattern parser in

  (* Speculatively consume trivia to check for pipe *)
  let saved_pos = Token_cursor.position parser.cursor in
  let trivia_after_left = consume_trivia parser in

  (* Check for | to continue or-pattern *)
  if peek_kind parser = Token.Pipe then (
    let patterns = ref [ left ] in
    let pipes = ref [] in

    while peek_kind parser = Token.Pipe do
      let pipe = consume parser in
      pipes := pipe :: !pipes;
      let trivia = consume_trivia parser in
      pipes := List.rev_append (List.map (fun t -> t) trivia) !pipes;

      (* Check if we have a valid pattern after | *)
      let pat =
        if can_start_pattern parser then parse_cons_pattern parser
        else if peek_kind parser = Token.Pipe then (
          (* Double pipe || - missing pattern between pipes *)
          let found_tok = consume parser in
          let diagnostic =
            Diagnostic.or_pattern_double ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(current_span parser)
          in
          report_diagnostic parser diagnostic;
          make_error_node parser ~diagnostic ~consumed_tokens:[ found_tok ])
        else
          (* Some other invalid token - missing pattern after pipe *)
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.or_pattern_missing ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser)
          in
          report_diagnostic parser diagnostic;
          make_node Syntax_kind.ERROR []
      in
      patterns := pat :: !patterns
    done;

    let pat_list = List.rev !patterns in
    let pipe_list = List.rev !pipes in
    let rec interleave pats pipes_and_trivia acc =
      match (pats, pipes_and_trivia) with
      | [], _ -> List.rev acc
      | p :: ps, [] -> interleave ps [] (Ceibo.Green.Node p :: acc)
      | p :: ps, pt :: pts ->
          interleave ps pts (Ceibo.Green.Node p :: make_token parser pt :: acc)
    in
    let children =
      (Ceibo.Green.Node left :: tokens_to_green parser trivia_after_left)
      @ interleave (List.tl pat_list) pipe_list []
    in
    make_node Syntax_kind.OR_PATTERN children)
  else (
    (* No pipe - restore position to before trivia *)
    Token_cursor.set_position parser.cursor saved_pos;
    left)

(** Parse cons pattern: x :: xs *)
and parse_cons_pattern parser =
  let left = parse_primary_pattern parser in

  (* Check for :: operator *)
  if peek_kind parser = Token.ColonColon then
    let cons_op = consume parser in
    let trivia = consume_trivia parser in

    (* Check if we have a valid pattern for the tail *)
    let right =
      if can_start_pattern parser then parse_cons_pattern parser
      else
        (* Missing tail pattern *)
        let found_tok = peek parser in
        let diagnostic =
          Diagnostic.cons_pattern_missing_tail ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser)
        in
        report_diagnostic parser diagnostic;
        make_node Syntax_kind.ERROR []
    in

    make_node Syntax_kind.CONS_PATTERN
      ([ Ceibo.Green.Node left ]
      @ [ make_token parser cons_op ]
      @ tokens_to_green parser trivia
      @ [ Ceibo.Green.Node right ])
  else left

(** Parse primary pattern: literals, identifiers, constructors, tuples, lists,
    etc. *)
and parse_primary_pattern parser =
  match peek_kind parser with
  | Token.Underscore ->
      let underscore = consume parser in
      make_node Syntax_kind.WILDCARD_PATTERN [ make_token parser underscore ]
  | Token.Literal (Token.Int _) ->
      let tok = consume parser in
      make_node Syntax_kind.INT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Float _) ->
      let tok = consume parser in
      make_node Syntax_kind.FLOAT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.String _) ->
      let tok = consume parser in
      make_node Syntax_kind.STRING_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Char _) ->
      let tok = consume parser in
      make_node Syntax_kind.CHAR_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.True ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.False ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Ident _ ->
      let ident = consume parser in
      let text = token_text parser ident in
      (* Check if this is a constructor (uppercase) or variable (lowercase) *)
      if String.length text > 0 && Char.uppercase_ascii text.[0] = text.[0] then
        (* Constructor pattern - check if followed by argument *)
        let trivia = consume_trivia parser in
        if can_start_pattern_arg parser then
          let arg = parse_primary_pattern parser in
          make_node Syntax_kind.CONSTRUCTOR_PATTERN
            ([ make_token parser ident ]
            @ tokens_to_green parser trivia
            @ [ Ceibo.Green.Node arg ])
        else
          make_node Syntax_kind.CONSTRUCTOR_PATTERN
            ([ make_token parser ident ] @ tokens_to_green parser trivia)
      else make_node Syntax_kind.IDENT_PATTERN [ make_token parser ident ]
  | Token.OpenDelim Token.Paren -> parse_paren_pattern parser
  | Token.OpenDelim Token.Bracket -> parse_list_pattern parser
  | Token.Keyword _ ->
      (* Keywords cannot be used as identifiers in patterns *)
      let kw_tok = consume parser in
      let diagnostic =
        Diagnostic.invalid_pattern ~found:kw_tok
          ~text:(token_text parser kw_tok) ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ kw_tok ]
  | _ ->
      let found_tok = peek parser in
      let exp_span = expected_span parser in
      let diagnostic =
        Diagnostic.invalid_pattern ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:exp_span
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Check if token can start a pattern *)
and can_start_pattern parser =
  match peek_kind parser with
  | Token.Underscore | Token.Literal _ | Token.Ident _
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.Bracket
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False ->
      true
  | _ -> false

(** Check if token can start a pattern argument (for constructor patterns) *)
and can_start_pattern_arg parser = can_start_pattern parser

(** Parse parenthesized pattern or tuple pattern *)
and parse_paren_pattern parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      let open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Check for empty tuple () *)
      if peek_kind parser = Token.CloseDelim Token.Paren then
        let close_paren = consume parser in
        make_node Syntax_kind.UNIT_LITERAL
          ([ make_token parser open_paren ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_paren ])
      else
        (* Parse first pattern *)
        let first_pat = parse_pattern parser in
        let trivia_after_first = consume_trivia parser in

        (* Check if this is a tuple (has comma) *)
        if peek_kind parser = Token.Comma then (
          let elements = ref [ first_pat ] in
          let commas = ref [] in

          while peek_kind parser = Token.Comma do
            let comma = consume parser in
            commas := comma :: !commas;
            let trivia = consume_trivia parser in

            (* Check for trailing comma before close paren *)
            if peek_kind parser = Token.CloseDelim Token.Paren then ()
            else
              let pat = parse_pattern parser in
              elements := pat :: !elements;
              let trivia2 = consume_trivia parser in
              commas := List.rev_append trivia2 (comma :: !commas)
          done;

          let trivia_before_close = consume_trivia parser in
          let close_paren =
            if peek_kind parser = Token.CloseDelim Token.Paren then
              consume parser
            else peek parser
          in

          let elem_list = List.rev !elements in
          let comma_list = List.rev !commas in
          let interleaved = ref [] in
          List.iteri
            (fun i pat ->
              if i = 0 then interleaved := Ceibo.Green.Node pat :: !interleaved
              else
                let comma = List.nth comma_list (i - 1) in
                interleaved :=
                  Ceibo.Green.Node pat :: make_token parser comma
                  :: !interleaved)
            elem_list;

          make_node Syntax_kind.TUPLE_PATTERN
            ([ make_token parser open_paren ]
            @ tokens_to_green parser trivia_after_open
            @ List.rev !interleaved
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser close_paren ]))
        else
          (* Single pattern in parens *)
          let trivia_before_close = consume_trivia parser in
          let close_paren =
            if peek_kind parser = Token.CloseDelim Token.Paren then
              consume parser
            else peek parser
          in

          make_node Syntax_kind.PAREN_PATTERN
            ([ make_token parser open_paren ]
            @ tokens_to_green parser trivia_after_open
            @ [ Ceibo.Green.Node first_pat ]
            @ tokens_to_green parser trivia_after_first
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser close_paren ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_pattern ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse list pattern: [] or [x; y; z] *)
and parse_list_pattern parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Check for empty list [] *)
      if peek_kind parser = Token.CloseDelim Token.Bracket then
        let close_bracket = consume parser in
        make_node Syntax_kind.LIST_PATTERN
          ([ make_token parser open_bracket ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_bracket ])
      else
        (* Parse list elements *)
        let elements = ref [] in
        let semis = ref [] in

        let first_pat = parse_pattern parser in
        elements := [ first_pat ];
        let trivia_after_first = consume_trivia parser in

        while peek_kind parser = Token.Semi do
          let semi = consume parser in
          semis := semi :: !semis;
          let trivia = consume_trivia parser in

          (* Check for trailing semicolon or end *)
          if peek_kind parser = Token.CloseDelim Token.Bracket then ()
          else
            let pat = parse_pattern parser in
            elements := pat :: !elements;
            let trivia2 = consume_trivia parser in
            semis := List.rev_append trivia2 (semi :: !semis)
        done;

        let trivia_before_close = consume_trivia parser in
        let close_bracket =
          if peek_kind parser = Token.CloseDelim Token.Bracket then
            consume parser
          else peek parser
        in

        let elem_list = List.rev !elements in
        let semi_list = List.rev !semis in
        let rest_parts = ref [] in
        List.iteri
          (fun i pat ->
            if i > 0 then
              let semi = List.nth semi_list (i - 1) in
              rest_parts :=
                Ceibo.Green.Node pat :: make_token parser semi :: !rest_parts)
          elem_list;

        make_node Syntax_kind.LIST_PATTERN
          ([ make_token parser open_bracket ]
          @ tokens_to_green parser trivia_after_open
          @ [ Ceibo.Green.Node first_pat ]
          @ tokens_to_green parser trivia_after_first
          @ List.rev !rest_parts
          @ tokens_to_green parser trivia_before_close
          @ [ make_token parser close_bracket ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_pattern ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 7: EXPRESSIONS *
    ============================================================================
*)

(** Parse constant expression: literals, unit, true, false, etc. *)
and parse_constant parser =
  match peek_kind parser with
  | Token.Literal (Token.Int _) ->
      let tok = consume parser in
      make_node Syntax_kind.INT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Float _) ->
      let tok = consume parser in
      make_node Syntax_kind.FLOAT_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.String _) ->
      let tok = consume parser in
      make_node Syntax_kind.STRING_LITERAL [ make_token parser tok ]
  | Token.Literal (Token.Char _) ->
      let tok = consume parser in
      make_node Syntax_kind.CHAR_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.True ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | Token.Keyword Keyword.False ->
      let tok = consume parser in
      make_node Syntax_kind.BOOL_LITERAL [ make_token parser tok ]
  | _ ->
      let found_tok = peek parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info (peek_kind parser)) then
          Diagnostic.missing_binary_operand ~operator:found_text ~side:"left"
            ~found:found_tok ~text:found_text ~span:(expected_span parser)
        else
          Diagnostic.invalid_expression ~found:found_tok ~text:found_text
            ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse character literal from quote tokens.

    Handles malformed char literals like:
    - 'a (unclosed)
    - '' (empty)
    - 'abc (multiple chars) *)
and parse_char_literal parser =
  match peek_kind parser with
  | Token.Quote -> (
      let quote = consume parser in

      (* Check what follows the quote *)
      match peek_kind parser with
      | Token.Quote ->
          (* Empty char literal: '' *)
          let closing_quote = consume parser in
          let span =
            Ceibo.Span.make ~start:quote.Token.span.start
              ~end_:closing_quote.Token.span.end_
          in
          let diagnostic = Diagnostic.empty_char_literal ~span in
          make_error_node parser ~diagnostic
            ~consumed_tokens:[ quote; closing_quote ]
      | Token.Ident _ -> (
          (* Could be 'a (unclosed char) or 'a (type var used in expr - wrong) *)
          let ident = consume parser in
          let ident_text = token_text parser ident in

          (* Check if there's a closing quote *)
          match peek_kind parser with
          | Token.Quote when String.length ident_text = 1 ->
              (* 'a' - should have been tokenized as Literal (Char) but wasn't *)
              let closing_quote = consume parser in
              make_node Syntax_kind.CHAR_LITERAL
                [
                  make_token parser quote;
                  make_token parser ident;
                  make_token parser closing_quote;
                ]
          | Token.Quote ->
              (* 'abc' - multiple characters *)
              let closing_quote = consume parser in
              let span =
                Ceibo.Span.make ~start:quote.Token.span.start
                  ~end_:closing_quote.Token.span.end_
              in
              let diagnostic =
                Diagnostic.multi_char_literal ~text:ident_text ~span
              in
              make_error_node parser ~diagnostic
                ~consumed_tokens:[ quote; ident; closing_quote ]
          | _ ->
              (* 'a (unclosed) - missing closing quote after the character *)
              let span =
                Ceibo.Span.make ~start:quote.Token.span.start
                  ~end_:ident.Token.span.end_
              in
              let diagnostic =
                Diagnostic.unclosed_char_literal ~text:("'" ^ ident_text) ~span
              in
              make_error_node parser ~diagnostic
                ~consumed_tokens:[ quote; ident ])
      | Token.EOF ->
          (* ' at EOF *)
          let diagnostic =
            Diagnostic.unclosed_char_literal ~text:"'"
              ~span:(expected_span parser)
          in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ]
      | _ ->
          (* Some other token - unexpected *)
          let found = peek parser in
          let diagnostic =
            Diagnostic.unclosed_char_literal ~text:"'"
              ~span:(current_span parser)
          in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ])
  | _ ->
      (* Not a quote - shouldn't be called *)
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse labeled parameter: ~label or ~label:pattern or ~(label) or ~(label:pattern) *)
and parse_labeled_param parser =
  (* MUST start with ~ *)
  let tilde =
    expect parser Token.Tilde (fun found ->
        Diagnostic.invalid_expression ~found ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  let trivia_after_tilde = consume_trivia parser in

  (* Define the core parser for label and optional :pattern *)
  let parse_label_and_pattern parser =
    let label = parse_ident parser in
    let trivia_after_label = consume_trivia parser in

    (* Check for optional :pattern *)
    let colon_pattern_parts =
      if peek_kind parser = Token.Colon then
        let colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        let pattern = parse_pattern parser in
        [ make_token parser colon ]
        @ tokens_to_green parser trivia_after_colon
        @ [ Ceibo.Green.Node pattern ]
      else []
    in

    ([ make_token parser label ] @ tokens_to_green parser trivia_after_label
   @ colon_pattern_parts)
  in

  (* Now decide: parenthesized or not? *)
  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      let open_p, trivia_open, content, trivia_close, close_p =
        parse_parens parser parse_label_and_pattern
      in
      make_node Syntax_kind.LABELED_PARAM
        ([ make_token parser tilde ]
        @ tokens_to_green parser trivia_after_tilde
        @ [ make_token parser open_p ]
        @ tokens_to_green parser trivia_open
        @ content
        @ tokens_to_green parser trivia_close
        @ [ make_token parser close_p ])
  | Token.Ident _ ->
      let content = parse_label_and_pattern parser in
      make_node Syntax_kind.LABELED_PARAM
        ([ make_token parser tilde ] @ tokens_to_green parser trivia_after_tilde
       @ content)
  | _ ->
      let found = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found ~text:(token_text parser found)
          ~span:(expected_span parser)
      in
      report_diagnostic parser diagnostic;
      make_error_node parser ~diagnostic ~consumed_tokens:[ tilde ]

(** Parse optional parameter: ?label, ?label:pattern, or ?(label = expr) *)
and parse_optional_param parser =
  (* MUST start with ? *)
  let question =
    expect parser Token.Question (fun found ->
        Diagnostic.invalid_expression ~found ~text:(token_text parser found)
          ~span:(current_span parser))
  in
  let trivia_after_q = consume_trivia parser in

  match peek_kind parser with
  | Token.OpenDelim Token.Paren ->
      (* ?(label = expr) - with default value *)
      let open_p, trivia_open, content, trivia_close, close_p =
        parse_parens parser (fun parser ->
            let label = parse_ident parser in
            let trivia_after_label = consume_trivia parser in

            (* Optional type annotation :type before = *)
            let type_parts =
              if peek_kind parser = Token.Colon then
                let colon = consume parser in
                let trivia_after_colon = consume_trivia parser in
                (* Parse type expression *)
                let type_expr = parse_typexpr parser in
                [ make_token parser colon ]
                @ tokens_to_green parser trivia_after_colon
                @ [ Ceibo.Green.Node type_expr ]
              else []
            in
            let trivia_before_eq = consume_trivia parser in

            (* MUST have = *)
            let eq =
              expect parser Token.Eq (fun found ->
                  Diagnostic.invalid_expression ~found
                    ~text:(token_text parser found)
                    ~span:(expected_span parser))
            in
            let trivia_after_eq = consume_trivia parser in

            (* Parse default value expression *)
            let default_expr = parse_expr parser in

            (* Return parts *)
            ([ make_token parser label ]
            @ tokens_to_green parser trivia_after_label
            @ type_parts
            @ tokens_to_green parser trivia_before_eq
            @ [ make_token parser eq ]
            @ tokens_to_green parser trivia_after_eq
            @ [ Ceibo.Green.Node default_expr ]))
      in
      make_node Syntax_kind.OPTIONAL_PARAM_DEFAULT
        ([ make_token parser question ]
        @ tokens_to_green parser trivia_after_q
        @ [ make_token parser open_p ]
        @ tokens_to_green parser trivia_open
        @ content
        @ tokens_to_green parser trivia_close
        @ [ make_token parser close_p ])
  | Token.Ident _ ->
      (* ?label or ?label:pattern *)
      let parse_label_and_pattern parser =
        let label = parse_ident parser in
        let trivia_after_label = consume_trivia parser in

        (* Check for optional :pattern *)
        let colon_pattern_parts =
          if peek_kind parser = Token.Colon then
            let colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            let pattern = parse_pattern parser in
            [ make_token parser colon ]
            @ tokens_to_green parser trivia_after_colon
            @ [ Ceibo.Green.Node pattern ]
          else []
        in

        ([ make_token parser label ] @ tokens_to_green parser trivia_after_label
       @ colon_pattern_parts)
      in

      let content = parse_label_and_pattern parser in
      make_node Syntax_kind.OPTIONAL_PARAM
        ([ make_token parser question ] @ tokens_to_green parser trivia_after_q
       @ content)
  | _ ->
      let found = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found ~text:(token_text parser found)
          ~span:(expected_span parser)
      in
      report_diagnostic parser diagnostic;
      make_error_node parser ~diagnostic ~consumed_tokens:[ question ]

(** Parse function parameter - dispatches to specific parser based on prefix *)
and parse_fun_param parser =
  match peek_kind parser with
  | Token.Question -> parse_optional_param parser
  | Token.Tilde -> parse_labeled_param parser
  | _ -> parse_pattern parser

(** Parse function expression: fun p1 p2 ... pn -> expr 
    Grammar: fun { parameter }+ "->" expr *)
and parse_fun_expr parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Fun ->
      let fun_keyword = consume parser in
      let trivia_after_fun = consume_trivia parser in

      (* Parse parameters until we hit -> *)
      let rec collect_params acc =
        let trivia = consume_trivia parser in
        match peek_kind parser with
        | Token.Arrow ->
            (* Done collecting params *)
            List.rev (tokens_to_green parser trivia @ acc)
        | _ ->
            (* Parse one parameter *)
            let param = parse_fun_param parser in
            collect_params
              ([ Ceibo.Green.Node param ] @ tokens_to_green parser trivia @ acc)
      in

      let params = collect_params [] in

      (* Expect -> *)
      let arrow =
        expect parser Token.Arrow (fun found ->
            Diagnostic.invalid_expression ~found ~text:(token_text parser found)
              ~span:(expected_span parser))
      in
      let trivia_after_arrow = consume_trivia parser in

      (* Parse body expression *)
      let body = parse_expr parser in

      make_node Syntax_kind.FUN_EXPR
        ([ make_token parser fun_keyword ]
        @ tokens_to_green parser trivia_after_fun
        @ params
        @ [ make_token parser arrow ]
        @ tokens_to_green parser trivia_after_arrow
        @ [ Ceibo.Green.Node body ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse primary expression (no operators, no function application) *)
and parse_primary_expr parser =
  match peek_kind parser with
  | Token.Ident _ ->
      let ident = consume parser in
      make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
  | Token.Literal _ -> parse_constant parser
  | Token.Keyword Keyword.True -> parse_constant parser
  | Token.Keyword Keyword.False -> parse_constant parser
  | Token.Keyword Keyword.Fun -> parse_fun_expr parser
  | Token.Keyword Keyword.If -> parse_if_expr parser
  | Token.Keyword Keyword.Match -> parse_match_expr parser
  | Token.Keyword Keyword.Let -> parse_let_in_expr parser
  | Token.OpenDelim Token.Paren -> parse_paren_expr parser
  | Token.OpenDelim Token.BeginEnd -> parse_begin_end_expr parser
  | Token.OpenDelim Token.Bracket -> parse_list_expr parser
  | Token.OpenDelim Token.Array -> parse_array_expr parser
  | Token.OpenDelim Token.Brace -> parse_record_expr parser
  | Token.Quote -> parse_char_literal parser
  | Token.Unknown '\'' ->
      (* Malformed char literal from lexer (e.g., '', 'a) *)
      let tok = consume parser in
      let span = tok.Token.span in
      let text = token_text parser tok in
      let diagnostic =
        if text = "''" then Diagnostic.empty_char_literal ~span
        else Diagnostic.unclosed_char_literal ~text ~span
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]
  | _ ->
      (* Unexpected token - consume it to avoid infinite loops *)
      let found_tok = consume parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info found_tok.Token.kind) then
          Diagnostic.missing_binary_operand ~operator:found_text ~side:"left"
            ~found:found_tok ~text:found_text ~span:(current_span parser)
        else
          Diagnostic.invalid_expression ~found:found_tok ~text:found_text
            ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ found_tok ]

(** Check if current token can start an argument expression. We stop at
    operators, keywords, and delimiters. *)
and can_start_arg_expr parser =
  match peek_kind parser with
  (* Can start arguments *)
  | Token.Ident _ | Token.Literal _
  | Token.Keyword Keyword.True
  | Token.Keyword Keyword.False
  | Token.Keyword Keyword.If
  | Token.Keyword Keyword.Match
  | Token.Keyword Keyword.Let
  | Token.OpenDelim Token.Paren
  | Token.OpenDelim Token.BeginEnd
  | Token.OpenDelim Token.Bracket
  | Token.OpenDelim Token.Array
  | Token.OpenDelim Token.Brace
  | Token.Quote ->
      true
  (* Cannot - these are operators or other constructs *)
  | _ -> false

(** Parse postfix expressions: field access (.), array indexing (.), etc.
    This has higher precedence than application.
    Grammar: expr ::= primary_expr { '.' ident | '.' '(' expr ')' | '.' '[' expr ']' }* *)
and parse_postfix_expr parser =
  let base = parse_primary_expr parser in

  let rec parse_postfix expr =
    match peek_kind parser with
    | Token.Dot -> (
        let dot = consume parser in
        let trivia_after_dot = consume_trivia parser in

        match peek_kind parser with
        | Token.Ident _ ->
            let field = consume parser in
            let trivia_after_field = consume_trivia parser in
            let field_access =
              make_node Syntax_kind.FIELD_ACCESS_EXPR
                ([ Ceibo.Green.Node expr ]
                @ [ make_token parser dot ]
                @ tokens_to_green parser trivia_after_dot
                @ [ make_token parser field ]
                @ tokens_to_green parser trivia_after_field)
            in
            parse_postfix field_access
        | Token.OpenDelim Token.Paren -> (
            let open_paren = consume parser in
            let trivia_after_open = consume_trivia parser in
            let index_expr = parse_expr parser in
            let trivia_after_expr = consume_trivia parser in
            match peek_kind parser with
            | Token.CloseDelim Token.Paren ->
                let close_paren = consume parser in
                let trivia_after_close = consume_trivia parser in
                let array_index =
                  make_node Syntax_kind.ARRAY_INDEX_EXPR
                    ([ Ceibo.Green.Node expr ]
                    @ [ make_token parser dot ]
                    @ tokens_to_green parser trivia_after_dot
                    @ [ make_token parser open_paren ]
                    @ tokens_to_green parser trivia_after_open
                    @ [ Ceibo.Green.Node index_expr ]
                    @ tokens_to_green parser trivia_after_expr
                    @ [ make_token parser close_paren ]
                    @ tokens_to_green parser trivia_after_close)
                in
                parse_postfix array_index
            | _ -> expr)
        | Token.OpenDelim Token.Bracket -> (
            let open_bracket = consume parser in
            let trivia_after_open = consume_trivia parser in
            let index_expr = parse_expr parser in
            let trivia_after_expr = consume_trivia parser in
            match peek_kind parser with
            | Token.CloseDelim Token.Bracket ->
                let close_bracket = consume parser in
                let trivia_after_close = consume_trivia parser in
                let string_index =
                  make_node Syntax_kind.STRING_INDEX_EXPR
                    ([ Ceibo.Green.Node expr ]
                    @ [ make_token parser dot ]
                    @ tokens_to_green parser trivia_after_dot
                    @ [ make_token parser open_bracket ]
                    @ tokens_to_green parser trivia_after_open
                    @ [ Ceibo.Green.Node index_expr ]
                    @ tokens_to_green parser trivia_after_expr
                    @ [ make_token parser close_bracket ]
                    @ tokens_to_green parser trivia_after_close)
                in
                parse_postfix string_index
            | _ -> expr)
        | _ -> expr)
    | _ -> expr
  in
  parse_postfix base

(** Parse function application: f x y z
    This has higher precedence than binary operators.
    Grammar: expr ::= expr { argument }+ *)
and parse_application_expr parser =
  let func = parse_postfix_expr parser in
  let trivia_after_func = consume_trivia parser in

  (* Keep parsing arguments while we can *)
  let rec parse_args func_expr func_trivia =
    if can_start_arg_expr parser then
      let arg = parse_postfix_expr parser in
      let trivia_after_arg = consume_trivia parser in
      (* Build application node: (f arg) *)
      let app_expr =
        make_node Syntax_kind.APPLY_EXPR
          (tokens_to_green parser func_trivia
          @ [ Ceibo.Green.Node func_expr ]
          @ [ Ceibo.Green.Node arg ])
      in
      (* Continue parsing more arguments: ((f arg1) arg2) ... *)
      parse_args app_expr trivia_after_arg
    else func_expr
  in
  parse_args func trivia_after_func

(** Parse binary expression with precedence climbing. This handles expressions
    like: 1 + 2 * 3, x || y && z, etc. *)
and parse_binary_expr parser min_prec =
  (* Parse left side (which may include function application) *)
  let left = parse_application_expr parser in
  let trivia_after_left = consume_trivia parser in

  (* Keep parsing operators while they have higher precedence *)
  let rec climb left left_trivia =
    match operator_info (peek_kind parser) with
    | Some (prec, is_right_assoc) when prec >= min_prec ->
        let op = consume parser in
        let op_text = token_text parser op in
        let trivia_after_op = consume_trivia parser in

        (* Check for consecutive operators or missing right operand *)
        let next_is_operator =
          Option.is_some (operator_info (peek_kind parser))
        in
        let at_end = peek_kind parser = Token.EOF in

        if next_is_operator then
          (* Consecutive operators: "1 + +" *)
          let next_op = peek parser in
          let next_op_text = token_text parser next_op in
          let operators = op_text ^ " " ^ next_op_text in
          let diagnostic =
            Diagnostic.consecutive_binary_operators ~operators ~found:next_op
              ~text:next_op_text ~span:(current_span parser)
          in

          (* Skip tokens until recovery point to avoid cascading errors *)
          let rec skip_to_recovery consumed_tokens =
            match peek_kind parser with
            | Token.Keyword Keyword.In (* let x = 1 + + 2 in ... *) | Token.EOF
              ->
                consumed_tokens
            | _ ->
                let tok = consume parser in
                skip_to_recovery (tok :: consumed_tokens)
          in
          let consumed = skip_to_recovery [ next_op ] in

          let error_node =
            make_error_node parser ~diagnostic
              ~consumed_tokens:(List.rev consumed)
          in
          (* Build partial binary expression with error on right *)
          let bin_expr =
            make_node Syntax_kind.INFIX_EXPR
              (tokens_to_green parser left_trivia
              @ [ Ceibo.Green.Node left ]
              @ tokens_to_green parser trivia_after_op
              @ [ make_token parser op ]
              @ [ Ceibo.Green.Node error_node ])
          in
          bin_expr
        else if at_end then
          (* Missing right operand at EOF: "1 +" *)
          let diagnostic =
            Diagnostic.missing_binary_operand ~operator:op_text ~side:"right"
              ~found:(peek parser) ~text:"" ~span:(expected_span parser)
          in
          let error_node =
            make_error_node parser ~diagnostic ~consumed_tokens:[]
          in
          (* Build partial binary expression with error on right *)
          let bin_expr =
            make_node Syntax_kind.INFIX_EXPR
              (tokens_to_green parser left_trivia
              @ [ Ceibo.Green.Node left ]
              @ tokens_to_green parser trivia_after_op
              @ [ make_token parser op ]
              @ [ Ceibo.Green.Node error_node ])
          in
          bin_expr
        else
          (* Normal case: parse right side *)
          let next_min_prec = if is_right_assoc then prec else prec + 1 in
          let right = parse_binary_expr parser next_min_prec in
          let trivia_after_right = consume_trivia parser in

          (* Build binary expression node *)
          let bin_expr =
            make_node Syntax_kind.INFIX_EXPR
              (tokens_to_green parser left_trivia
              @ [ Ceibo.Green.Node left ]
              @ tokens_to_green parser trivia_after_op
              @ [ make_token parser op ]
              @ [ Ceibo.Green.Node right ])
          in

          (* Continue climbing with the new left side *)
          climb bin_expr trivia_after_right
    | _ ->
        (* No more operators, return current expression *)
        left
  in

  climb left trivia_after_left

(** Parse tuple expression: e1, e2, e3
    This is lower precedence than binary operators.
    Grammar: expr ::= expr { "," expr }+ *)
and parse_tuple_expr parser =
  let first = parse_binary_expr parser 0 in
  let trivia_after_first = consume_trivia parser in

  (* Check if we have a comma (tuple) *)
  match peek_kind parser with
  | Token.Comma ->
      (* Parse comma-separated elements *)
      let rec parse_elements acc =
        match peek_kind parser with
        | Token.Comma ->
            let comma = consume parser in
            let trivia_after_comma = consume_trivia parser in
            let elem = parse_binary_expr parser 0 in
            let trivia_after_elem = consume_trivia parser in
            parse_elements
              (tokens_to_green parser trivia_after_elem
              @ [ Ceibo.Green.Node elem ]
              @ tokens_to_green parser trivia_after_comma
              @ [ make_token parser comma ]
              @ acc)
        | _ ->
            (* No more commas *)
            List.rev acc
      in

      let elements =
        parse_elements
          (tokens_to_green parser trivia_after_first
          @ [ Ceibo.Green.Node first ])
      in

      make_node Syntax_kind.TUPLE_EXPR elements
  | _ ->
      (* Not a tuple, just return the expression *)
      first

(** Parse expression (top-level entry point) *)
and parse_expr parser = parse_sequence_expr parser

(** Parse sequence expression: expr1; expr2; expr3 *)
and parse_sequence_expr parser =
  let first = parse_tuple_expr parser in

  (* Check if we have semicolons to make a sequence *)
  if peek_kind parser = Token.Semi then (
    let exprs = ref [ first ] in
    let semis = ref [] in

    while peek_kind parser = Token.Semi do
      let semi = consume parser in
      semis := semi :: !semis;
      let trivia = consume_trivia parser in

      (* Check if we're at the end (trailing semicolon) or next expression *)
      if
        peek_kind parser = Token.EOF
        || peek_kind parser = Token.CloseDelim Token.Paren
        || peek_kind parser = Token.CloseDelim Token.BeginEnd
        || peek_kind parser = Token.Keyword Keyword.In
        || peek_kind parser = Token.Keyword Keyword.Done
        || peek_kind parser = Token.Keyword Keyword.End
      then ()
      else
        let expr = parse_tuple_expr parser in
        exprs := expr :: !exprs;
        let trivia2 = consume_trivia parser in
        semis := List.rev_append trivia2 (semi :: List.rev_append trivia !semis)
    done;

    (* Build sequence node *)
    let expr_list = List.rev !exprs in
    let semi_list = List.rev !semis in
    let parts = ref [] in
    List.iteri
      (fun i expr ->
        parts := Ceibo.Green.Node expr :: !parts;
        if i < List.length semi_list then
          parts := make_token parser (List.nth semi_list i) :: !parts)
      expr_list;

    make_node Syntax_kind.SEQUENCE_EXPR (List.rev !parts))
  else first

(** Parse parenthesized expression or unit: (expr) or () *)
and parse_paren_expr parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Paren -> (
      let lparen = consume parser in
      let trivia_after_lparen = consume_trivia parser in

      (* Check if this is unit () or a parenthesized expression *)
      match peek_kind parser with
      | Token.CloseDelim Token.Paren ->
          (* This is unit: () *)
          let rparen = consume parser in
          make_node Syntax_kind.UNIT_LITERAL
            ([ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ make_token parser rparen ])
      | _ ->
          (* This is a parenthesized expression: (expr) *)
          let expr = parse_expr parser in
          let trivia_after_expr = consume_trivia parser in

          (* Expect ')' *)
          let rparen_children =
            match peek_kind parser with
            | Token.CloseDelim Token.Paren ->
                let rparen = consume parser in
                [ make_token parser rparen ]
            | _ ->
                let found_tok = peek parser in
                (* Point to end of last token for better error location *)
                let diagnostic =
                  Diagnostic.unclosed_delimiter ~opener:"(" ~found:found_tok
                    ~text:(token_text parser found_tok)
                    ~span:(expected_span parser)
                in
                report_diagnostic parser diagnostic;
                []
          in

          make_node Syntax_kind.PAREN_EXPR
            ([ make_token parser lparen ]
            @ tokens_to_green parser trivia_after_lparen
            @ [ Ceibo.Green.Node expr ]
            @ tokens_to_green parser trivia_after_expr
            @ rparen_children))
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse begin...end expression: begin expr end *)
and parse_begin_end_expr parser =
  match peek_kind parser with
  | Token.OpenDelim Token.BeginEnd ->
      let begin_delim = consume parser in
      let trivia_after_begin = consume_trivia parser in

      let expr = parse_expr parser in
      let trivia_after_expr = consume_trivia parser in

      (* Expect 'end' (which is CloseDelim BeginEnd) *)
      let end_children =
        match peek_kind parser with
        | Token.CloseDelim Token.BeginEnd ->
            let end_delim = consume parser in
            [ make_token parser end_delim ]
        | _ ->
            let found_tok = peek parser in
            (* Point to end of last token for better error location *)
            let diagnostic =
              Diagnostic.unclosed_delimiter ~opener:"begin" ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            []
      in

      make_node Syntax_kind.PAREN_EXPR
        ([ make_token parser begin_delim ]
        @ tokens_to_green parser trivia_after_begin
        @ [ Ceibo.Green.Node expr ]
        @ tokens_to_green parser trivia_after_expr
        @ end_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse list expression: [1; 2; 3] or [] *)
and parse_list_expr parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Bracket ->
      let open_bracket = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Parse list elements separated by semicolons *)
      let rec parse_elements acc =
        match peek_kind parser with
        | Token.CloseDelim Token.Bracket | Token.EOF ->
            (* End of list *)
            List.rev acc
        | Token.Semi ->
            (* Semicolon without element - this is an error *)
            let semi = consume parser in
            let diagnostic =
              Diagnostic.list_double_semicolon ~found:semi
                ~text:(token_text parser semi) ~span:semi.Token.span
            in
            let error_node =
              make_error_node parser ~diagnostic ~consumed_tokens:[ semi ]
            in
            let trivia_after = consume_trivia parser in
            parse_elements
              (tokens_to_green parser trivia_after
              @ [ Ceibo.Green.Node error_node ]
              @ acc)
        | _ -> (
            (* Parse an element *)
            let elem = parse_expr parser in

            (* Check for semicolon or closing bracket BEFORE consuming trivia *)
            match peek_kind parser with
            | Token.Semi -> (
                let trivia_after_elem = consume_trivia parser in
                let semi = consume parser in
                let trivia_after_semi = consume_trivia parser in

                (* Check for double semicolon *)
                match peek_kind parser with
                | Token.Semi ->
                    (* Double semicolon! Report error and consume the second one *)
                    let second_semi = consume parser in
                    let diagnostic =
                      Diagnostic.list_double_semicolon ~found:second_semi
                        ~text:(token_text parser second_semi)
                        ~span:second_semi.Token.span
                    in
                    let error_node =
                      make_error_node parser ~diagnostic
                        ~consumed_tokens:[ second_semi ]
                    in
                    let trivia_after_error = consume_trivia parser in
                    parse_elements
                      (tokens_to_green parser trivia_after_error
                      @ [ Ceibo.Green.Node error_node ]
                      @ tokens_to_green parser trivia_after_semi
                      @ [ make_token parser semi ]
                      @ tokens_to_green parser trivia_after_elem
                      @ [ Ceibo.Green.Node elem ] @ acc)
                | _ ->
                    (* Single semicolon, continue normally *)
                    parse_elements
                      (tokens_to_green parser trivia_after_semi
                      @ [ make_token parser semi ]
                      @ tokens_to_green parser trivia_after_elem
                      @ [ Ceibo.Green.Node elem ] @ acc))
            | _ ->
                (* No semicolon - element is complete, recurse to check for bracket *)
                parse_elements ([ Ceibo.Green.Node elem ] @ acc))
      in

      let elements = parse_elements [] in

      (* Expect closing bracket *)
      let close_children =
        match peek_kind parser with
        | Token.CloseDelim Token.Bracket ->
            let close_bracket = consume parser in
            [ make_token parser close_bracket ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic =
              Diagnostic.unclosed_delimiter ~opener:"[" ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            []
      in

      make_node Syntax_kind.LIST_EXPR
        ([ make_token parser open_bracket ]
        @ tokens_to_green parser trivia_after_open
        @ elements @ close_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse array expression: [|1; 2; 3|] or [||] *)
and parse_array_expr parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Array ->
      let open_array = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Parse array elements separated by semicolons *)
      let rec parse_elements acc =
        match peek_kind parser with
        | Token.CloseDelim Token.Array | Token.EOF -> List.rev acc
        | _ -> (
            let elem = parse_expr parser in
            match peek_kind parser with
            | Token.Semi ->
                let trivia_after_elem = consume_trivia parser in
                let semi = consume parser in
                let trivia_after_semi = consume_trivia parser in
                parse_elements
                  (tokens_to_green parser trivia_after_semi
                  @ [ make_token parser semi ]
                  @ tokens_to_green parser trivia_after_elem
                  @ [ Ceibo.Green.Node elem ] @ acc)
            | Token.CloseDelim Token.Array | Token.EOF ->
                let trivia_after_elem = consume_trivia parser in
                List.rev
                  (tokens_to_green parser trivia_after_elem
                  @ [ Ceibo.Green.Node elem ] @ acc)
            | _ ->
                let trivia_after_elem = consume_trivia parser in
                List.rev
                  (tokens_to_green parser trivia_after_elem
                  @ [ Ceibo.Green.Node elem ] @ acc))
      in

      let elements = parse_elements [] in

      (* Parse closing |] *)
      let close_children =
        match peek_kind parser with
        | Token.CloseDelim Token.Array ->
            let close_array = consume parser in
            [ make_token parser close_array ]
        | _ -> []
      in

      make_node Syntax_kind.ARRAY_EXPR
        ([ make_token parser open_array ]
        @ tokens_to_green parser trivia_after_open
        @ elements @ close_children)
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse record expression: { field = value } or { base with field = value } *)
and parse_record_expr parser =
  match peek_kind parser with
  | Token.OpenDelim Token.Brace ->
      let open_brace = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Check for empty record {} *)
      if peek_kind parser = Token.CloseDelim Token.Brace then
        let close_brace = consume parser in
        make_node Syntax_kind.RECORD_EXPR
          ([ make_token parser open_brace ]
          @ tokens_to_green parser trivia_after_open
          @ [ make_token parser close_brace ])
      else
        (* Parse first identifier/expression to determine if it's update syntax *)
        (* { expr with ... } vs { field = ... } *)
        let first_expr = parse_expr parser in
        let trivia_after_first = consume_trivia parser in

        (* Check if this is record update: { expr with ... } *)
        if peek_kind parser = Token.Keyword Keyword.With then (
          let with_kw = consume parser in
          let trivia_after_with = consume_trivia parser in

          (* Parse field assignments *)
          let fields = ref [] in
          let semis = ref [] in

          let rec parse_fields () =
            if
              peek_kind parser = Token.CloseDelim Token.Brace
              || peek_kind parser = Token.EOF
            then ()
            else
              let field = parse_record_field parser in
              fields := field :: !fields;
              let trivia = consume_trivia parser in

              if peek_kind parser = Token.Semi then (
                let semi = consume parser in
                semis := semi :: !semis;
                let trivia2 = consume_trivia parser in
                if peek_kind parser <> Token.CloseDelim Token.Brace then
                  parse_fields ())
          in
          parse_fields ();

          let trivia_before_close = consume_trivia parser in
          let close_brace =
            if peek_kind parser = Token.CloseDelim Token.Brace then
              consume parser
            else peek parser
          in

          make_node Syntax_kind.RECORD_UPDATE_EXPR
            ([ make_token parser open_brace ]
            @ tokens_to_green parser trivia_after_open
            @ [ Ceibo.Green.Node first_expr ]
            @ tokens_to_green parser trivia_after_first
            @ [ make_token parser with_kw ]
            @ tokens_to_green parser trivia_after_with
            @ List.flatten
                (List.mapi
                   (fun i field ->
                     if i = 0 then [ Ceibo.Green.Node field ]
                     else
                       [
                         make_token parser (List.nth (List.rev !semis) (i - 1));
                         Ceibo.Green.Node field;
                       ])
                   (List.rev !fields))
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser close_brace ]))
        else
          (* Regular record: { field = value; ... } *)
          (* The first_expr should be a field assignment *)
          let fields = ref [ first_expr ] in
          let semis = ref [] in

          let rec parse_fields () =
            if peek_kind parser = Token.Semi then (
              let semi = consume parser in
              semis := semi :: !semis;
              let trivia = consume_trivia parser in

              if peek_kind parser = Token.CloseDelim Token.Brace then ()
              else
                let field = parse_record_field parser in
                fields := field :: !fields;
                let trivia2 = consume_trivia parser in
                parse_fields ())
          in
          parse_fields ();

          let trivia_before_close = consume_trivia parser in
          let close_brace =
            if peek_kind parser = Token.CloseDelim Token.Brace then
              consume parser
            else peek parser
          in

          make_node Syntax_kind.RECORD_EXPR
            ([ make_token parser open_brace ]
            @ tokens_to_green parser trivia_after_open
            @ List.flatten
                (List.mapi
                   (fun i field ->
                     if i = 0 then [ Ceibo.Green.Node field ]
                     else
                       [
                         make_token parser (List.nth (List.rev !semis) (i - 1));
                         Ceibo.Green.Node field;
                       ])
                   (List.rev !fields))
            @ tokens_to_green parser trivia_before_close
            @ [ make_token parser close_brace ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a single record field: field = expr or field (shorthand) *)
and parse_record_field parser =
  (* Parse field name *)
  let field_name =
    match peek_kind parser with
    | Token.Ident _ -> consume parser
    | _ -> peek parser
  in

  let trivia_after_name = consume_trivia parser in

  (* Check for = *)
  if peek_kind parser = Token.Eq then
    let eq = consume parser in
    let trivia_after_eq = consume_trivia parser in
    let value_expr = parse_expr parser in

    make_node Syntax_kind.RECORD_FIELD
      ([ make_token parser field_name ]
      @ tokens_to_green parser trivia_after_name
      @ [ make_token parser eq ]
      @ tokens_to_green parser trivia_after_eq
      @ [ Ceibo.Green.Node value_expr ])
  else
    (* Shorthand: just field name, equivalent to field = field *)
    make_node Syntax_kind.RECORD_FIELD
      ([ make_token parser field_name ]
      @ tokens_to_green parser trivia_after_name)

(** Parse if-then-else expression: if cond then e1 else e2 *)
and parse_if_expr parser =
  match peek_kind parser with
  | Token.Keyword Keyword.If ->
      let if_kw = consume parser in
      let trivia_after_if = consume_trivia parser in

      (* Parse condition *)
      let cond = parse_expr parser in
      let trivia_after_cond = consume_trivia parser in

      (* Expect 'then' keyword *)
      let has_then = peek_kind parser = Token.Keyword Keyword.Then in
      let then_children, trivia_after_then, then_expr, trivia_after_then_expr =
        if has_then then
          let then_kw = consume parser in
          let trivia_after_then = consume_trivia parser in
          let then_expr = parse_expr parser in
          let trivia_after_then_expr = consume_trivia parser in
          ( [ make_token parser then_kw ],
            trivia_after_then,
            then_expr,
            trivia_after_then_expr )
        else
          (* Missing 'then' - report error and skip to 'else' *)
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.if_missing_then ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser)
          in
          report_diagnostic parser diagnostic;

          (* Skip tokens until we find 'else' or a stopping point *)
          let error_tokens = ref [] in
          while
            not
              (peek_kind parser = Token.Keyword Keyword.Else
              || peek_kind parser = Token.Keyword Keyword.In
              || peek_kind parser = Token.Semi
              || peek_kind parser = Token.EOF)
          do
            error_tokens := consume parser :: !error_tokens
          done;

          (* Wrap consumed tokens in ERROR node *)
          let error_children =
            tokens_to_green parser (List.rev !error_tokens)
          in
          let error_node = make_node Syntax_kind.ERROR error_children in
          ([], [], error_node, [])
      in

      (* Optional 'else' keyword and branch *)
      let else_parts =
        match peek_kind parser with
        | Token.Keyword Keyword.Else ->
            let else_kw = consume parser in
            let trivia_after_else = consume_trivia parser in
            let else_expr = parse_expr parser in
            [ make_token parser else_kw ]
            @ tokens_to_green parser trivia_after_else
            @ [ Ceibo.Green.Node else_expr ]
        | _ -> []
      in

      make_node Syntax_kind.IF_EXPR
        ([ make_token parser if_kw ]
        @ tokens_to_green parser trivia_after_if
        @ [ Ceibo.Green.Node cond ]
        @ tokens_to_green parser trivia_after_cond
        @ then_children
        @ tokens_to_green parser trivia_after_then
        @ [ Ceibo.Green.Node then_expr ]
        @ tokens_to_green parser trivia_after_then_expr
        @ else_parts)
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse match expression: match expr with | pattern -> expr | ... *)
and parse_match_expr parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Match ->
      let match_kw = consume parser in
      let trivia_after_match = consume_trivia parser in

      (* Parse scrutinee expression - check if 'with' comes immediately *)
      let scrutinee, trivia_after_scrutinee =
        if peek_kind parser = Token.Keyword Keyword.With then (
          (* Missing scrutinee *)
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.match_missing_scrutinee ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser)
          in
          report_diagnostic parser diagnostic;
          let error_node = make_node Syntax_kind.ERROR [] in
          (error_node, []))
        else
          let scrut = parse_expr parser in
          let trivia = consume_trivia parser in
          (scrut, trivia)
      in

      (* Expect 'with' keyword *)
      let with_children =
        match peek_kind parser with
        | Token.Keyword Keyword.With ->
            let with_kw = consume parser in
            [ make_token parser with_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic =
              Diagnostic.match_missing_with ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            []
      in

      let trivia_after_with = consume_trivia parser in

      (* Parse match cases: | pattern -> expr *)
      let cases = ref [] in
      let trivia_parts = ref [] in
      let rec parse_cases () =
        match peek_kind parser with
        | Token.Pipe ->
            let case = parse_match_case parser in
            cases := case :: !cases;
            let trivia = consume_trivia parser in
            trivia_parts := trivia :: !trivia_parts;
            parse_cases ()
        | _ -> ()
      in

      (* First case might not have leading pipe *)
      (match peek_kind parser with
      | Token.Pipe -> parse_cases ()
      | _ ->
          (* Try to parse a case without pipe *)
          if
            peek_kind parser <> Token.EOF
            && peek_kind parser <> Token.Keyword Keyword.In
            && peek_kind parser <> Token.Semi
          then (
            let case = parse_match_case parser in
            cases := [ case ];
            let trivia = consume_trivia parser in
            trivia_parts := [ trivia ];
            parse_cases ()));

      (* Interleave cases with trivia *)
      let case_list = List.rev !cases in
      let trivia_list = List.rev !trivia_parts in
      let rec interleave cases trivias acc =
        match (cases, trivias) with
        | [], [] -> List.rev acc
        | c :: cs, [] -> interleave cs [] (Ceibo.Green.Node c :: acc)
        | c :: cs, t :: ts ->
            interleave cs ts
              (List.rev_append (tokens_to_green parser t)
                 (Ceibo.Green.Node c :: acc))
        | [], _ :: _ -> List.rev acc
      in
      let case_elements = interleave case_list trivia_list [] in

      make_node Syntax_kind.MATCH_EXPR
        ([ make_token parser match_kw ]
        @ tokens_to_green parser trivia_after_match
        @ [ Ceibo.Green.Node scrutinee ]
        @ tokens_to_green parser trivia_after_scrutinee
        @ with_children
        @ tokens_to_green parser trivia_after_with
        @ case_elements)
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse a match case: | pattern -> expr or | pattern when guard -> expr *)
and parse_match_case parser =
  (* Optional leading pipe *)
  let pipe_tok =
    if peek_kind parser = Token.Pipe then
      let tok = consume parser in
      [ make_token parser tok ]
    else []
  in

  let trivia_after_pipe = consume_trivia parser in

  (* Parse pattern - check if arrow comes immediately *)
  let pattern, trivia_after_pattern =
    if peek_kind parser = Token.Arrow then (
      (* Missing pattern *)
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.match_missing_pattern ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      report_diagnostic parser diagnostic;
      let error_node = make_node Syntax_kind.ERROR [] in
      (error_node, []))
    else
      let pat = parse_pattern parser in
      let trivia = consume_trivia parser in
      (pat, trivia)
  in

  (* Optional 'when' guard *)
  let guard_parts =
    match peek_kind parser with
    | Token.Keyword Keyword.When ->
        let when_kw = consume parser in
        let trivia_after_when = consume_trivia parser in
        (* Check if arrow comes immediately after when *)
        let guard_expr, trivia_after_guard =
          if peek_kind parser = Token.Arrow then (
            (* Missing guard expression *)
            let found_tok = peek parser in
            let diagnostic =
              Diagnostic.match_guard_missing_expr ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            let error_node = make_node Syntax_kind.ERROR [] in
            (error_node, []))
          else
            let expr = parse_expr parser in
            let trivia = consume_trivia parser in
            (expr, trivia)
        in
        [ make_token parser when_kw ]
        @ tokens_to_green parser trivia_after_when
        @ [ Ceibo.Green.Node guard_expr ]
        @ tokens_to_green parser trivia_after_guard
    | _ -> []
  in

  (* Expect '->' *)
  let arrow_children =
    match peek_kind parser with
    | Token.Arrow ->
        let arrow = consume parser in
        [ make_token parser arrow ]
    | _ ->
        let found_tok = peek parser in
        let diagnostic =
          Diagnostic.invalid_expression ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(expected_span parser)
        in
        report_diagnostic parser diagnostic;
        []
  in

  let trivia_after_arrow = consume_trivia parser in

  (* Parse case expression *)
  let case_expr = parse_expr parser in

  make_node Syntax_kind.MATCH_CASE
    (pipe_tok
    @ tokens_to_green parser trivia_after_pipe
    @ [ Ceibo.Green.Node pattern ]
    @ tokens_to_green parser trivia_after_pattern
    @ guard_parts @ arrow_children
    @ tokens_to_green parser trivia_after_arrow
    @ [ Ceibo.Green.Node case_expr ])

(** Parse let-in expression: let x = expr1 in expr2 *)
and parse_let_in_expr parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Let ->
      let let_kw = consume parser in
      let trivia_after_let = consume_trivia parser in

      (* Parse pattern *)
      let pattern = parse_pattern parser in
      let trivia_after_pattern = consume_trivia parser in

      (* Expect '=' *)
      let eq_children =
        match peek_kind parser with
        | Token.Eq ->
            let eq = consume parser in
            [ make_token parser eq ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic =
              Diagnostic.missing_let_binding_equals ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            []
      in

      let trivia_after_eq = consume_trivia parser in

      (* Parse bound expression *)
      let bound_expr = parse_expr parser in
      let trivia_after_bound = consume_trivia parser in

      (* Expect 'in' keyword *)
      let in_children =
        match peek_kind parser with
        | Token.Keyword Keyword.In ->
            let in_kw = consume parser in
            [ make_token parser in_kw ]
        | _ ->
            let found_tok = peek parser in
            let diagnostic =
              Diagnostic.invalid_expression ~found:found_tok
                ~text:(token_text parser found_tok)
                ~span:(expected_span parser)
            in
            report_diagnostic parser diagnostic;
            []
      in

      let trivia_after_in = consume_trivia parser in

      (* Parse body expression *)
      let body_expr = parse_expr parser in

      make_node Syntax_kind.LET_EXPR
        ([ make_token parser let_kw ]
        @ tokens_to_green parser trivia_after_let
        @ [ Ceibo.Green.Node pattern ]
        @ tokens_to_green parser trivia_after_pattern
        @ eq_children
        @ tokens_to_green parser trivia_after_eq
        @ [ Ceibo.Green.Node bound_expr ]
        @ tokens_to_green parser trivia_after_bound
        @ in_children
        @ tokens_to_green parser trivia_after_in
        @ [ Ceibo.Green.Node body_expr ])
  | _ ->
      let found_tok = peek parser in
      let diagnostic =
        Diagnostic.invalid_expression ~found:found_tok
          ~text:(token_text parser found_tok)
          ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse let binding: let pattern = expr *)
and parse_let_binding parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Let ->
      let let_kw = consume parser in
      let trivia_after_let = consume_trivia parser in

      let pat = parse_pattern parser in
      let trivia_after_pat = consume_trivia parser in

      (* Expect '=' *)
      let has_eq = peek_kind parser = Token.Eq in
      let eq_tok =
        if has_eq then consume parser
        else
          let found_tok = peek parser in
          let diag =
            Diagnostic.missing_let_binding_equals ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(expected_span parser)
          in
          report_diagnostic parser diag;
          peek parser (* Return a dummy token, don't consume *)
      in

      (* If no =, skip to = or next keyword and return *)
      if not has_eq then
        let skipped_tokens =
          error_recover_until parser
            ~sync_tokens:
              [
                Token.Eq; Token.Keyword Keyword.Let; Token.Keyword Keyword.Type;
              ]
        in
        let trivia_after_skip = consume_trivia parser in

        (* Check if we found = *)
        match peek_kind parser with
        | Token.Eq ->
            (* Found =, continue parsing *)
            let eq = consume parser in
            let trivia_after_eq = consume_trivia parser in
            let expr = parse_expr parser in
            make_node Syntax_kind.LET_BINDING
              ([ make_token parser let_kw ]
              @ tokens_to_green parser trivia_after_let
              @ [ Ceibo.Green.Node pat ]
              @ tokens_to_green parser trivia_after_pat
              @ tokens_to_green parser skipped_tokens
              @ tokens_to_green parser trivia_after_skip
              @ [ make_token parser eq ]
              @ tokens_to_green parser trivia_after_eq
              @ [ Ceibo.Green.Node expr ])
        | _ ->
            (* Didn't find =, stop here *)
            make_node Syntax_kind.LET_BINDING
              ([ make_token parser let_kw ]
              @ tokens_to_green parser trivia_after_let
              @ [ Ceibo.Green.Node pat ]
              @ tokens_to_green parser trivia_after_pat
              @ tokens_to_green parser skipped_tokens
              @ tokens_to_green parser trivia_after_skip)
      else
        (* Have =, continue normally *)
        let trivia_after_eq = consume_trivia parser in
        let expr = parse_expr parser in

        (* Check if expr had error - if so, skip to sync point and return early *)
        if Ceibo.Green.kind (Ceibo.Green.Node expr) = Syntax_kind.ERROR then
          (* Error in expression - skip to next keyword and stop *)
          let skipped_tokens =
            error_recover_until parser
              ~sync_tokens:
                [ Token.Keyword Keyword.Let; Token.Keyword Keyword.Type ]
          in
          let trivia_after_skip = consume_trivia parser in
          make_node Syntax_kind.LET_BINDING
            ([ make_token parser let_kw ]
            @ tokens_to_green parser trivia_after_let
            @ [ Ceibo.Green.Node pat ]
            @ tokens_to_green parser trivia_after_pat
            @ [ make_token parser eq_tok ]
            @ tokens_to_green parser trivia_after_eq
            @ [ Ceibo.Green.Node expr ]
            @ tokens_to_green parser skipped_tokens
            @ tokens_to_green parser trivia_after_skip)
        else
          (* No error in expression - continue normally *)
          make_node Syntax_kind.LET_BINDING
            ([ make_token parser let_kw ]
            @ tokens_to_green parser trivia_after_let
            @ [ Ceibo.Green.Node pat ]
            @ tokens_to_green parser trivia_after_pat
            @ [ make_token parser eq_tok ]
            @ tokens_to_green parser trivia_after_eq
            @ [ Ceibo.Green.Node expr ])
  | _ ->
      let found_tok = peek parser in
      let found_text = token_text parser found_tok in
      (* Check if it's an operator (missing left operand) *)
      let diagnostic =
        if Option.is_some (operator_info (peek_kind parser)) then
          Diagnostic.missing_binary_operand ~operator:found_text ~side:"left"
            ~found:found_tok ~text:found_text ~span:(expected_span parser)
        else
          Diagnostic.invalid_expression ~found:found_tok ~text:found_text
            ~span:(expected_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * TOP-LEVEL PARSING *
    ============================================================================
*)

(** Parse type declaration: type [type_params] name = typexpr *)
and parse_type_decl parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Type -> (
      let type_kw = consume parser in
      let trivia_after_type = consume_trivia parser in

      (* Check for common mistake: bracketed type params like type foo<A, B> *)
      (* This check must come BEFORE type name parsing *)
      let bracket_error_early =
        match peek_kind parser with
        | Token.Ident _ ->
            (* Peek ahead to see if there's a < after the identifier *)
            let next_tok = Token_cursor.peek_n parser.cursor 1 in
            if next_tok.Token.kind = Token.Lt then
              (* This looks like: type name<...> *)
              (* Consume the name first *)
              let name_tok = consume parser in
              let lt_tok = consume parser in
              let type_name = token_text parser name_tok in
              let diagnostic =
                Diagnostic.bracketed_type_parameters ~type_name ~found:lt_tok
                  ~text:"<"
                  ~span:
                    (Ceibo.Span.make ~start:lt_tok.Token.span.start
                       ~end_:lt_tok.Token.span.end_)
              in
              (* Skip until > or = or EOF *)
              let rec skip_bracketed acc =
                match peek_kind parser with
                | Token.Gt ->
                    let gt_tok = consume parser in
                    List.rev (gt_tok :: acc)
                | Token.Eq | Token.EOF -> List.rev acc
                | _ ->
                    let tok = consume parser in
                    skip_bracketed (tok :: acc)
              in
              let consumed = skip_bracketed [ lt_tok ] in
              let error_node =
                make_error_node parser ~diagnostic ~consumed_tokens:consumed
              in
              Some (name_tok, error_node)
            else None
        | _ -> None
      in

      (* Try to parse type parameters (e.g., 'a or ('a, 'b)) - optional *)
      let type_params, trivia_after_params =
        match bracket_error_early with
        | Some _ ->
            (* Already consumed name and brackets, no type params *)
            ([], [])
        | None -> (
            match peek_kind parser with
            | Token.Unknown '\'' ->
                (* Malformed type variable like ' a or '' *)
                let tok = consume parser in
                let diagnostic =
                  Diagnostic.malformed_type_variable ~found:tok
                    ~text:(token_text parser tok) ~span:tok.Token.span
                in
                let error_node =
                  make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]
                in
                let trivia = consume_trivia parser in
                ([ Ceibo.Green.Node error_node ], trivia)
            | Token.Underscore ->
                (* Check if next token is also underscore (__ is invalid) *)
                let next_tok = Token_cursor.peek_n parser.cursor 1 in
                if next_tok.Token.kind = Token.Underscore then
                  (* __ is invalid, should be _ *)
                  let tok1 = consume parser in
                  let tok2 = consume parser in
                  let diagnostic =
                    Diagnostic.invalid_type_parameter ~text:"__" ~found:tok1
                      ~text_found:"__"
                      ~span:
                        (Ceibo.Span.make ~start:tok1.Token.span.start
                           ~end_:tok2.Token.span.end_)
                  in
                  let error_node =
                    make_error_node parser ~diagnostic
                      ~consumed_tokens:[ tok1; tok2 ]
                  in
                  let trivia = consume_trivia parser in
                  ([ Ceibo.Green.Node error_node ], trivia)
                else
                  (* Single _ is not a type param in this position, it's probably the type name *)
                  ([], [])
            | Token.At | Token.Bang | Token.Caret
            | Token.OpenDelim Token.Bracket ->
                (* Invalid type parameter characters *)
                let tok = consume parser in
                let text = token_text parser tok in
                let diagnostic =
                  Diagnostic.invalid_type_parameter ~text ~found:tok
                    ~text_found:text ~span:(current_span parser)
                in
                let error_node =
                  make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]
                in
                let trivia = consume_trivia parser in
                ([ Ceibo.Green.Node error_node ], trivia)
            | _ -> parse_type_params parser)
      in

      (* Check if type param had error - if so, skip to sync point and return early *)
      match type_params with
      | [ Ceibo.Green.Node node ]
        when Ceibo.Green.kind (Ceibo.Green.Node node) = Syntax_kind.ERROR ->
          (* Error in type param - skip to next keyword and stop *)
          let skipped_tokens =
            error_recover_until parser
              ~sync_tokens:
                [ Token.Keyword Keyword.Let; Token.Keyword Keyword.Type ]
          in
          let trivia_after_skip = consume_trivia parser in
          make_node Syntax_kind.TYPE_DECL
            ([ make_token parser type_kw ]
            @ tokens_to_green parser trivia_after_type
            @ type_params
            @ tokens_to_green parser trivia_after_params
            @ tokens_to_green parser skipped_tokens
            @ tokens_to_green parser trivia_after_skip)
      | _ ->
          (* No error in type params - continue normally *)
          (* Check if we already consumed name+brackets in bracket_error_early *)
          let type_name, bracket_error, trivia_after_name, trivia_after_bracket
              =
            match bracket_error_early with
            | Some (name_tok, error_node) ->
                (* Already consumed name and detected bracket error *)
                let name_node =
                  make_node Syntax_kind.IDENT_EXPR
                    [ make_token parser name_tok ]
                in
                let trivia = consume_trivia parser in
                (name_node, Some error_node, [], trivia)
            | None ->
                (* Normal case: parse type name *)
                let name =
                  match peek_kind parser with
                  | Token.Ident _ ->
                      let ident = consume parser in
                      make_node Syntax_kind.IDENT_EXPR
                        [ make_token parser ident ]
                  | _ ->
                      let found_tok = peek parser in
                      let diagnostic =
                        Diagnostic.missing_type_name ~found:found_tok
                          ~text:(token_text parser found_tok)
                          ~span:(current_span parser)
                      in
                      make_error_node parser ~diagnostic ~consumed_tokens:[]
                in
                let trivia = consume_trivia parser in
                (name, None, trivia, [])
          in

          (* Check if type name had error - if so, skip to = or next keyword *)
          if Ceibo.Green.kind (Ceibo.Green.Node type_name) = Syntax_kind.ERROR
          then
            (* Error in type name - skip to = or next keyword *)
            let skipped_tokens =
              error_recover_until parser
                ~sync_tokens:
                  [
                    Token.Eq;
                    Token.Keyword Keyword.Let;
                    Token.Keyword Keyword.Type;
                  ]
            in
            let trivia_after_skip = consume_trivia parser in

            (* Check if we found = *)
            match peek_kind parser with
            | Token.Eq ->
                (* Found =, continue parsing the type definition *)
                let eq = consume parser in
                let trivia_after_eq = consume_trivia parser in
                let type_expr = parse_typexpr parser in
                make_node Syntax_kind.TYPE_DECL
                  ([ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser skipped_tokens
                  @ tokens_to_green parser trivia_after_skip
                  @ [ make_token parser eq ]
                  @ tokens_to_green parser trivia_after_eq
                  @ [ Ceibo.Green.Node type_expr ])
            | _ ->
                (* Didn't find =, stop here *)
                make_node Syntax_kind.TYPE_DECL
                  ([ make_token parser type_kw ]
                  @ tokens_to_green parser trivia_after_type
                  @ type_params
                  @ tokens_to_green parser trivia_after_params
                  @ [ Ceibo.Green.Node type_name ]
                  @ tokens_to_green parser skipped_tokens
                  @ tokens_to_green parser trivia_after_skip)
          else
            (* No error in type name - continue normally *)
            (* trivia_after_name and bracket_error already parsed above *)

            (* Parse '=' (optional for abstract types, or after bracket error) *)
            let eq_children =
              match peek_kind parser with
              | Token.Eq ->
                  let eq = consume parser in
                  [ make_token parser eq ]
              | _ when Option.is_some bracket_error ->
                  (* Already reported bracket error, don't complain about missing = *)
                  []
              | _ ->
                  (* No = found and no bracket error - this might be abstract type or error *)
                  (* For now, don't report error for abstract types *)
                  []
            in

            (* If no =, skip to next keyword and return *)
            if eq_children = [] then
              let skipped_tokens =
                error_recover_until parser
                  ~sync_tokens:
                    [ Token.Keyword Keyword.Let; Token.Keyword Keyword.Type ]
              in
              let trivia_after_skip = consume_trivia parser in
              make_node Syntax_kind.TYPE_DECL
                ([ make_token parser type_kw ]
                @ tokens_to_green parser trivia_after_type
                @ type_params
                @ tokens_to_green parser trivia_after_params
                @ [ Ceibo.Green.Node type_name ]
                @ tokens_to_green parser trivia_after_name
                @ (match bracket_error with
                  | Some e -> [ Ceibo.Green.Node e ]
                  | None -> [])
                @ tokens_to_green parser trivia_after_bracket
                @ tokens_to_green parser skipped_tokens
                @ tokens_to_green parser trivia_after_skip)
            else
              (* Have =, continue parsing type expression *)
              let trivia_after_eq = consume_trivia parser in
              let type_expr = parse_typexpr parser in

              make_node Syntax_kind.TYPE_DECL
                ([ make_token parser type_kw ]
                @ tokens_to_green parser trivia_after_type
                @ type_params
                @ tokens_to_green parser trivia_after_params
                @ [ Ceibo.Green.Node type_name ]
                @ tokens_to_green parser trivia_after_name
                @ (match bracket_error with
                  | Some e -> [ Ceibo.Green.Node e ]
                  | None -> [])
                @ tokens_to_green parser trivia_after_bracket
                @ eq_children
                @ tokens_to_green parser trivia_after_eq
                @ [ Ceibo.Green.Node type_expr ])
      | _ ->
          let found_tok = peek parser in
          let diagnostic =
            Diagnostic.missing_type_keyword ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(current_span parser)
          in
          make_error_node parser ~diagnostic ~consumed_tokens:[])

(** Parse structure item (top-level in .ml files) *)
and parse_structure_item parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Let -> parse_let_binding parser
  | Token.Keyword Keyword.Type -> parse_type_decl parser
  | _ ->
      (* Unknown token - consume it and report error to avoid infinite loop *)
      let tok = consume parser in
      let diagnostic =
        Diagnostic.unexpected_structure_item ~found:tok
          ~text:(token_text parser tok) ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]

(** Parse signature item (top-level in .mli files) *)
and parse_signature_item parser =
  (* Unknown token - consume it and report error to avoid infinite loop *)
  let tok = consume parser in
  let diagnostic =
    Diagnostic.unexpected_signature_item ~found:tok
      ~text:(token_text parser tok) ~span:(current_span parser)
  in
  make_error_node parser ~diagnostic ~consumed_tokens:[ tok ]

and parse ~parse_item ~source ~tokens =
  let parser = create ~source tokens in

  (* Consume all trivia at the start *)
  let leading_trivia = consume_trivia parser in

  (* Parse items until EOF *)
  let rec parse_items acc =
    if is_eof parser then List.rev acc
    else if peek_kind parser = Token.EOF then List.rev acc
    else
      let item = parse_item parser in
      let trivia = consume_trivia parser in
      parse_items
        ((Ceibo.Green.Node item :: tokens_to_green parser trivia) @ acc)
  in
  let items = parse_items [] in

  (* Build SOURCE_FILE with ALL trivia preserved *)
  let children = tokens_to_green parser leading_trivia @ items in

  let tree = make_node Syntax_kind.SOURCE_FILE children in

  let diagnostics = List.rev (Cell.get parser.diagnostics) in
  { tree; diagnostics }

(** Parse interface file (.mli) *)
let parse_interface ~source tokens =
  parse ~parse_item:parse_signature_item ~source ~tokens

(** Parse implementation file (.ml) *)
let parse_implementation ~source tokens =
  parse ~parse_item:parse_structure_item ~source ~tokens
