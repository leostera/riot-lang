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

(** Get text of a token from source *)
let token_text parser token =
  Token_cursor.view parser.cursor token.Token.span

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
    if is_trivia_kind kind then begin
      let token = consume parser in
      loop (token :: acc)
    end else
      List.rev acc
  in
  loop []

(** Error recovery: skip tokens until we reach a synchronization point.
    
    This helps prevent cascading errors by consuming tokens after an error
    until we reach a point where parsing can meaningfully continue.
    
    @param parser The parser state
    @param sync_tokens List of token kinds to stop at (but not consume)
    @return List of consumed tokens during recovery *)
let error_recover_until parser ~sync_tokens =
  let is_sync_token kind =
    List.exists (fun sync -> sync = kind) sync_tokens
  in
  let rec skip_to_sync acc =
    match peek_kind parser with
    | Token.EOF -> List.rev acc
    | kind when is_sync_token kind -> List.rev acc
    | Token.Whitespace when String.contains (token_text parser (peek parser)) '\n' ->
        (* Stop at newline but don't consume it *)
        List.rev acc
    | _ ->
        let tok = consume parser in
        skip_to_sync (tok :: acc)
  in
  skip_to_sync []

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

(** Parse type variable: "'" ident

    CRITICAL: No trivia allowed between ' and ident! Grammar: typexpr ::= "'"
    ident *)
let rec parse_type_variable parser =
  match peek_kind parser with
  | Token.Quote ->
      let quote = consume parser in

      (* IMMEDIATELY get identifier - NO trivia! *)
      (match peek_kind parser with
      | Token.Ident _ ->
          let ident = consume parser in
          make_node Syntax_kind.TYPE_VAR
            [ make_token parser quote; make_token parser ident ]
      | found ->
          (* Error: expected identifier after quote *)
          let found_tok = peek parser in
          let diagnostic = Diagnostic.unexpected_token
            ~expected:"type variable name"
            ~found:found_tok
            ~text:(token_text parser found_tok)
            ~span:(current_span parser)
          in
          make_error_node parser ~diagnostic ~consumed_tokens:[ quote ])
  | found ->
      (* Error: expected quote to start type variable *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"type variable"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 4: TYPE EXPRESSIONS *
    ============================================================================
*)

(** Parse type expression dispatcher *)
and parse_typexpr parser =
  (* For now, just handle type variables *)
  (* TODO: Add all other type expression variants *)
  match peek_kind parser with
  | Token.Quote -> parse_type_variable parser
  | _ ->
      (* Placeholder: return error for unimplemented *)
      let found_tok = peek parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"type expression"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * GRAMMAR SECTION 6: PATTERNS *
    ============================================================================
*)

(** Parse pattern - for now just identifiers *)
and parse_pattern parser =
  match peek_kind parser with
  | Token.Ident _ ->
      let ident = consume parser in
      make_node Syntax_kind.IDENT_PATTERN [ make_token parser ident ]
  | Token.Underscore ->
      let underscore = consume parser in
      make_node Syntax_kind.WILDCARD_PATTERN [ make_token parser underscore ]
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"pattern"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
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
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"constant"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse expression *)
and parse_expr parser =
  match peek_kind parser with
  | Token.Ident _ ->
      let ident = consume parser in
      make_node Syntax_kind.IDENT_EXPR [ make_token parser ident ]
  | Token.Literal _ -> parse_constant parser
  | Token.Keyword Keyword.True -> parse_constant parser
  | Token.Keyword Keyword.False -> parse_constant parser
  | Token.Keyword Keyword.Let -> parse_let_binding parser
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"expression"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
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
      let eq_tok = match peek_kind parser with
        | Token.Eq ->
            consume parser
        | _ ->
            let found_tok = peek parser in
            let diag = Diagnostic.unexpected_token
              ~expected:"="
              ~found:found_tok
              ~text:(token_text parser found_tok)
              ~span:(current_span parser)
            in
            report_diagnostic parser diag;
            consume parser
      in
      let trivia_after_eq = consume_trivia parser in
      
      let expr = parse_expr parser in
      
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
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"let keyword"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** *
    ============================================================================
    * TOP-LEVEL PARSING *
    ============================================================================
*)

(** Parse type declaration: type 'a t = ... *)
and parse_type_decl parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Type ->
      let type_kw = consume parser in
      let trivia_after_type = consume_trivia parser in
      
      (* Parse type parameters like 'a *)
      let type_param = parse_typexpr parser in
      
      (* Check if we got an error - if so, skip to synchronization point *)
      let skipped_tokens = 
        match Ceibo.Green.kind (Ceibo.Green.Node type_param) with
        | Syntax_kind.ERROR ->
            (* After error, skip to where we can meaningfully continue:
               - = (type body definition)
               - keyword (next declaration)
               - newline/EOF (end of incomplete declaration) *)
            error_recover_until parser ~sync_tokens:[Token.Eq; Token.Keyword Keyword.Let; Token.Keyword Keyword.Type]
        | _ -> []
      in
      
      let trivia_after_param = consume_trivia parser in
      
      (* For now, just wrap what we have *)
      make_node Syntax_kind.TYPE_DECL
        ([ make_token parser type_kw ]
         @ tokens_to_green parser trivia_after_type
         @ [ Ceibo.Green.Node type_param ]
         @ tokens_to_green parser skipped_tokens
         @ tokens_to_green parser trivia_after_param)
  | _ ->
      let found_tok = peek parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"type keyword"
        ~found:found_tok
        ~text:(token_text parser found_tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[]

(** Parse structure item (top-level in .ml files) *)
and parse_structure_item parser =
  match peek_kind parser with
  | Token.Keyword Keyword.Let -> parse_let_binding parser
  | Token.Keyword Keyword.Type -> parse_type_decl parser
  | _ ->
      (* Unknown token - consume it and report error to avoid infinite loop *)
      let tok = consume parser in
      let diagnostic = Diagnostic.unexpected_token
        ~expected:"structure item"
        ~found:tok
        ~text:(token_text parser tok)
        ~span:(current_span parser)
      in
      make_error_node parser ~diagnostic ~consumed_tokens:[tok]

(** Parse signature item (top-level in .mli files) *)
and parse_signature_item parser =
  (* Unknown token - consume it and report error to avoid infinite loop *)
  let tok = consume parser in
  let diagnostic = Diagnostic.unexpected_token
    ~expected:"signature item"
    ~found:tok
    ~text:(token_text parser tok)
    ~span:(current_span parser)
  in
  make_error_node parser ~diagnostic ~consumed_tokens:[tok]

and parse ~parse_item ~source ~tokens =
  let parser = create ~source tokens in

  (* Consume all trivia at the start *)
  let leading_trivia = consume_trivia parser in

  (* Parse items until EOF *)
  let rec parse_items acc =
    if is_eof parser then List.rev acc
    else if peek_kind parser = Token.EOF then List.rev acc
    else begin
      let item = parse_item parser in
      let trivia = consume_trivia parser in
      parse_items (Ceibo.Green.Node item :: tokens_to_green parser trivia @ acc)
    end
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
