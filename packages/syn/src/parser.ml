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
let peek_kind parser =
  match peek parser with Some token -> Some token.Token.kind | None -> None

(** Peek n tokens ahead *)
let peek_n parser n = Token_cursor.peek_n parser.cursor n

(** Check if current token matches a specific kind *)
let at parser kind =
  match peek_kind parser with Some k -> k = kind | None -> false

(** Advance to next token *)
let advance parser = Token_cursor.advance parser.cursor

(** Report a diagnostic *)
let report_diagnostic parser diag =
  let current_diags = Cell.get parser.diagnostics in
  Cell.set parser.diagnostics (diag :: current_diags)

(** Get current span for error reporting *)
let current_span parser =
  match peek parser with
  | Some token -> token.Token.span
  | None ->
      (* At EOF - we need to get source length from cursor somehow *)
      (* For now, use position as best guess *)
      let pos = position parser in
      Ceibo.Span.make ~start:pos ~end_:pos

(** Consume a single token WITHOUT consuming trivia after it.

    IMPORTANT: This is the primitive operation. It does NOT auto-consume trivia!
    Call consume_trivia explicitly where grammar allows it. *)
let consume parser =
  match peek parser with
  | Some token ->
      advance parser;
      token
  | None ->
      (* At EOF - create synthetic EOF token *)
      let pos = position parser in
      let span = Ceibo.Span.make ~start:pos ~end_:pos in
      { Token.kind = Token.EOF; span }

(** Check if a token kind is trivia *)
let is_trivia_kind = function
  | Token.Comment _ | Token.Docstring _ | Token.Whitespace -> true
  | _ -> false

(** Consume trivia tokens (whitespace, comments).

    Only call this where the grammar explicitly allows trivia! Not all positions
    in OCaml allow trivia (e.g., between ' and type var name). *)
let consume_trivia parser =
  let rec loop acc =
    match peek_kind parser with
    | Some kind when is_trivia_kind kind ->
        let token = consume parser in
        loop (token :: acc)
    | _ -> List.rev acc
  in
  loop []

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
let make_error_node parser ~diag_kind ~consumed_tokens =
  let span = current_span parser in
  let diag = Diagnostic.make ~kind:diag_kind ~span in
  report_diagnostic parser diag;

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
  | Some Token.Quote -> (
      let quote = consume parser in

      (* IMMEDIATELY get identifier - NO trivia! *)
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let ident = consume parser in
          make_node Syntax_kind.TYPE_VAR
            [ make_token parser quote; make_token parser ident ]
      | found ->
          (* Error: expected identifier after quote *)
          let diag_kind =
            Diagnostic.UnexpectedToken
              {
                expected = Some "identifier after quote";
                found =
                  (match found with
                  | Some k -> "unknown" (* TODO: token kind to string *)
                  | None -> "end of file");
              }
          in
          make_error_node parser ~diag_kind ~consumed_tokens:[ quote ])
  | found ->
      (* Error: expected quote to start type variable *)
      let diag_kind =
        Diagnostic.UnexpectedToken
          {
            expected = Some "type variable";
            found =
              (match found with
              | Some k -> "unknown" (* TODO: token kind to string *)
              | None -> "end of file");
          }
      in
      make_error_node parser ~diag_kind ~consumed_tokens:[]

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
  | Some Token.Quote -> parse_type_variable parser
  | _ ->
      (* Placeholder: return error for unimplemented *)
      let diag_kind =
        Diagnostic.UnexpectedToken
          { expected = Some "type expression"; found = "unimplemented" }
      in
      make_error_node parser ~diag_kind ~consumed_tokens:[]

(** *
    ============================================================================
    * TOP-LEVEL PARSING *
    ============================================================================
*)

(** Placeholder for parsing interface items - to be implemented *)
and parse_interface_item _parser = 
  failwith "parse_interface_item: not yet implemented"

(** Placeholder for parsing implementation items - to be implemented *)
and parse_implementation_item _parser = 
  failwith "parse_implementation_item: not yet implemented"

let parse ~parse_item:_ ~source ~tokens =
  let parser = create ~source tokens in

  (* Consume all trivia at the start *)
  let leading_trivia = consume_trivia parser in

  (* For now: empty items list, but keep ALL trivia! *)
  let items = [] in

  (* Consume trailing trivia before EOF *)
  let trailing_trivia = consume_trivia parser in

  (* Build SOURCE_FILE with ALL trivia preserved *)
  let children =
    tokens_to_green parser leading_trivia
    @ items
    @ tokens_to_green parser trailing_trivia
  in

  let tree = make_node Syntax_kind.SOURCE_FILE children in

  let diagnostics = List.rev (Cell.get parser.diagnostics) in
  { tree; diagnostics }

(** Parse interface file (.mli) *)
let parse_interface ~source tokens =
  parse ~parse_item:parse_interface_item ~source ~tokens

(** Parse implementation file (.ml) *)
let parse_implementation ~source tokens =
  parse ~parse_item:parse_implementation_item ~source ~tokens
