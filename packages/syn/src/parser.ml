open Std

type parse_result = {
  tree : (Syntax_kind.t, string) Ceibo.Green.node;
  diagnostics : Diagnostic.t list;
}

type t = {
  source : string;
  tokens : Token.t array;
  mutable position : int;
  mutable diagnostics : Diagnostic.t list;
}
let create ~source tokens =
  { source; tokens = Array.of_list tokens; position = 0; diagnostics = [] }

(* ========================================================================= *)
(* TOKEN ACCESS *)
(* ========================================================================= *)

let peek parser =
  if parser.position < Array.length parser.tokens then
    Some parser.tokens.(parser.position)
  else None

let peek_kind parser =
  match peek parser with
  | Some tok -> Some tok.Token.kind
  | None -> None

let advance parser =
  if parser.position < Array.length parser.tokens then (
    let tok = parser.tokens.(parser.position) in
    parser.position <- parser.position + 1;
    Some tok
  ) else None

let at parser kind =
  match peek parser with
  | Some tok -> tok.Token.kind = kind
  | None -> false

let at_any parser kinds =
  match peek parser with
  | Some tok -> List.mem tok.Token.kind kinds
  | None -> false

(* ========================================================================= *)
(* TRIVIA HANDLING *)
(* ========================================================================= *)

let is_trivia = function
  | Token.Whitespace | Token.Comment _ | Token.Docstring _ -> true
  | _ -> false

let rec skip_trivia parser =
  match peek parser with
  | Some tok when is_trivia tok.Token.kind ->
      let _ = advance parser in
      skip_trivia parser
  | _ -> ()

(* ========================================================================= *)
(* GREEN TREE CONSTRUCTION *)
(* ========================================================================= *)

let token_kind_to_syntax_kind = function
  | Token.Whitespace -> Syntax_kind.WHITESPACE
  | Token.Comment _ -> Syntax_kind.COMMENT
  | Token.Docstring _ -> Syntax_kind.DOCSTRING
  | Token.Literal (Token.Int _) -> Syntax_kind.INT_LITERAL
  | Token.Literal (Token.Float _) -> Syntax_kind.FLOAT_LITERAL
  | Token.Literal (Token.String _) -> Syntax_kind.STRING_LITERAL
  | Token.Literal (Token.Char _) -> Syntax_kind.CHAR_LITERAL
  | Token.Keyword Keyword.True | Token.Keyword Keyword.False -> Syntax_kind.BOOL_LITERAL
  | Token.Keyword Keyword.Let -> Syntax_kind.LET_BINDING
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Eq -> Syntax_kind.INFIX_EXPR
  | _ -> Syntax_kind.ERROR  (* TODO: Map remaining token kinds *)

let token_to_green_token parser tok =
  let text = String.sub parser.source tok.Token.span.start 
    (tok.Token.span.end_ - tok.Token.span.start) in
  let width = tok.Token.span.end_ - tok.Token.span.start in
  let kind = token_kind_to_syntax_kind tok.Token.kind in
  Ceibo.Green.make_token ~kind ~text ~width

let consume parser =
  match advance parser with
  | Some tok ->
      let green_tok = token_to_green_token parser tok in
      Ceibo.Green.Token green_tok
  | None ->
      Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.ERROR ~text:"" ~width:0)

let make_node ~kind children =
  Ceibo.Green.make_node ~kind ~children

(* ========================================================================= *)
(* ERROR RECOVERY *)
(* ========================================================================= *)

let report_error parser err =
  parser.diagnostics <- err :: parser.diagnostics

let make_error_node parser ~kind ~span =
  report_error parser (Diagnostic.make ~kind ~span);
  (* Create an empty error node *)
  make_node ~kind:Syntax_kind.ERROR [||]

let expect parser kind =
  match peek parser with
  | Some tok when tok.Token.kind = kind ->
      consume parser
  | _ ->
      (* Report missing token *)
      let span = match peek parser with
        | Some tok -> Ceibo.Span.make ~start:tok.Token.span.start ~end_:tok.Token.span.start
        | None -> 
            let pos = if parser.position > 0 then
              parser.tokens.(parser.position - 1).Token.span.end_
            else 0
            in
            Ceibo.Span.make ~start:pos ~end_:pos
      in
      let err = Diagnostic.make_missing_token ~expected:(Token.show_kind kind) ~span in
      report_error parser err;
      (* Create a missing token *)
      Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)

(* ========================================================================= *)
(* LITERALS *)
(* ========================================================================= *)

let parse_literal parser =
  skip_trivia parser;
  match peek_kind parser with
  | Some (Token.Literal (Token.Int _)) ->
      Some (make_node ~kind:Syntax_kind.INT_LITERAL [| consume parser |])
  | Some (Token.Literal (Token.Float _)) ->
      Some (make_node ~kind:Syntax_kind.FLOAT_LITERAL [| consume parser |])
  | Some (Token.Literal (Token.String _)) ->
      Some (make_node ~kind:Syntax_kind.STRING_LITERAL [| consume parser |])
  | Some (Token.Literal (Token.Char _)) ->
      Some (make_node ~kind:Syntax_kind.CHAR_LITERAL [| consume parser |])
  | Some (Token.Keyword Keyword.True) | Some (Token.Keyword Keyword.False) ->
      Some (make_node ~kind:Syntax_kind.BOOL_LITERAL [| consume parser |])
  | Some (Token.OpenDelim Token.Paren) -> (
      let open_tok = consume parser in
      skip_trivia parser;
      match peek_kind parser with
      | Some (Token.CloseDelim Token.Paren) ->
          let close_tok = consume parser in
          Some (make_node ~kind:Syntax_kind.UNIT_LITERAL [| open_tok; close_tok |])
      | _ -> None)
  | _ -> None

(* ========================================================================= *)
(* EXPRESSIONS - Forward declarations *)
(* ========================================================================= *)

let rec parse_expr parser =
  parse_primary parser

and parse_primary parser =
  skip_trivia parser;
  
  (* Try to parse a literal *)
  match parse_literal parser with
  | Some lit -> Some lit
  | None -> (
      match peek_kind parser with
      (* Identifier *)
      | Some (Token.Ident _) ->
          let ident = consume parser in
          Some (make_node ~kind:Syntax_kind.IDENT_EXPR [| ident |])
      
      (* Parenthesized expression *)
      | Some (Token.OpenDelim Token.Paren) ->
          parse_paren_expr parser
      
      (* Let expression *)
      | Some (Token.Keyword Keyword.Let) ->
          parse_let_expr parser
      
      (* If expression *)
      | Some (Token.Keyword Keyword.If) ->
          parse_if_expr parser
      
      (* Match expression *)
      | Some (Token.Keyword Keyword.Match) ->
          parse_match_expr parser
      
      (* Fun/function *)
      | Some (Token.Keyword Keyword.Fun) ->
          parse_fun_expr parser
      
      | Some (Token.Keyword Keyword.Function) ->
          parse_function_expr parser
      
      | _ -> None)

and parse_paren_expr parser =
  let open_paren = consume parser in
  skip_trivia parser;
  
  match parse_expr parser with
  | Some expr ->
      skip_trivia parser;
      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      Some (make_node ~kind:Syntax_kind.PAREN_EXPR [| open_paren; Ceibo.Green.Node expr; close_paren |])
  | None ->
      let span = match peek parser with
        | Some tok -> Ceibo.Span.make ~start:tok.Token.span.start ~end_:tok.Token.span.end_
        | None -> Ceibo.Span.make ~start:0 ~end_:0
      in
      Some (make_error_node parser ~kind:(Diagnostic.InvalidSyntax { context = "parenthesized expression" }) ~span)

and parse_let_expr parser =
  (* TODO: Implement *)
  None

and parse_if_expr parser =
  (* TODO: Implement *)
  None

and parse_match_expr parser =
  (* TODO: Implement *)
  None

and parse_fun_expr parser =
  (* TODO: Implement *)
  None

and parse_function_expr parser =
  (* TODO: Implement *)
  None

(* ========================================================================= *)
(* PATTERNS *)
(* ========================================================================= *)

let rec parse_pattern parser =
  skip_trivia parser;
  
  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let underscore = consume parser in
      Some (make_node ~kind:Syntax_kind.WILDCARD_PATTERN [| underscore |])
  
  (* Identifier *)
  | Some (Token.Ident _) ->
      let ident = consume parser in
      Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |])
  
  (* Literal pattern *)
  | Some (Token.Literal _) | Some (Token.Keyword Keyword.True) 
  | Some (Token.Keyword Keyword.False) -> (
      match parse_literal parser with
      | Some lit -> Some (make_node ~kind:Syntax_kind.LITERAL_PATTERN [| Ceibo.Green.Node lit |])
      | None -> None)
  
  (* Parenthesized pattern *)
  | Some (Token.OpenDelim Token.Paren) ->
      parse_paren_pattern parser
  
  | _ -> None

and parse_paren_pattern parser =
  let open_paren = consume parser in
  skip_trivia parser;
  
  match parse_pattern parser with
  | Some pat ->
      skip_trivia parser;
      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      Some (make_node ~kind:Syntax_kind.PAREN_PATTERN [| open_paren; Ceibo.Green.Node pat; close_paren |])
  | None ->
      let span = match peek parser with
        | Some tok -> Ceibo.Span.make ~start:tok.Token.span.start ~end_:tok.Token.span.end_
        | None -> Ceibo.Span.make ~start:0 ~end_:0
      in
      Some (make_error_node parser ~kind:(Diagnostic.InvalidSyntax { context = "parenthesized pattern" }) ~span)

(* ========================================================================= *)
(* TOP-LEVEL *)
(* ========================================================================= *)

let rec parse_structure_item parser =
  skip_trivia parser;
  
  match peek_kind parser with
  | Some (Token.Keyword Keyword.Let) ->
      parse_let_binding parser
  | Some (Token.Keyword Keyword.Open) ->
      parse_open parser
  | _ -> None

and parse_let_binding parser =
  let let_kw = consume parser in
  skip_trivia parser;
  
  (* Parse pattern (for now, just identifier) *)
  let pattern = match peek_kind parser with
    | Some (Token.Ident _) -> consume parser
    | _ -> 
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"identifier" ~span in
        report_error parser err;
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  skip_trivia parser;
  
  (* Expect '=' *)
  let eq = expect parser Token.Eq in
  
  skip_trivia parser;
  
  (* Parse expression (for now, just literals and identifiers) *)
  let expr = match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"expression" ~span in
        report_error parser err;
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  (* Skip trailing whitespace/newlines *)
  skip_trivia parser;
  
  Some (make_node ~kind:Syntax_kind.LET_BINDING [| let_kw; pattern; eq; expr |])

and parse_open parser =
  let open_kw = consume parser in
  skip_trivia parser;
  
  (* Parse module path *)
  let path = consume parser in
  
  Some (make_node ~kind:Syntax_kind.OPEN_STMT [| open_kw; path |])

let parse_source_file parser =
  let items = ref [] in
  
  while peek parser <> None && not (at parser Token.EOF) do
    match parse_structure_item parser with
    | Some item -> items := Ceibo.Green.Node item :: !items
    | None ->
        (* Skip problematic token *)
        let _ = advance parser in
        ()
  done;
  
  make_node ~kind:Syntax_kind.SOURCE_FILE (Array.of_list (List.rev !items))

(* ========================================================================= *)
(* PUBLIC API *)
(* ========================================================================= *)

let parse ~source tokens =
  let parser = create ~source tokens in
  let green_tree = parse_source_file parser in
  { tree = green_tree; diagnostics = List.rev parser.diagnostics }
