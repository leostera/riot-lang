open Std
open Std.Sync
open Std.Collections
module Syntax_kind = Syntax_kind
module Diagnostic = Diagnostic

type t = { cursor : Token_cursor.t; diagnostics : Diagnostic.t list Cell.t }

let create ~source tokens =
  { cursor = Token_cursor.create ~source tokens; diagnostics = Cell.create [] }

let position p = Token_cursor.position p.cursor
let is_eof p = Token_cursor.is_eof p.cursor
let peek p = Token_cursor.peek p.cursor
let peek_kind p = (peek p).Token.kind
let peek_n p n = Token_cursor.peek_n p.cursor n
let advance p = Token_cursor.advance p.cursor

let report_diagnostic p diag =
  Cell.set p.diagnostics (diag :: Cell.get p.diagnostics)

let current_span p = (peek p).Token.span

let expected_span p =
  let last = Token_cursor.last_token p.cursor in
  let end_pos = last.Token.span.end_ in
  match peek_kind p with
  | Token.Eof -> Ceibo.Span.make ~start:end_pos ~end_:end_pos
  | _ -> Ceibo.Span.make ~start:end_pos ~end_:(end_pos + 1)

let token_text p tok = Token_cursor.view p.cursor tok.Token.span

let consume p =
  let tok = peek p in
  advance p;
  tok

let is_trivia_kind = function
  | Token.Comment _ | Token.Whitespace -> true
  | _ -> false

let consume_trivia p =
  let rec loop acc =
    if is_trivia_kind (peek_kind p) then loop (consume p :: acc)
    else List.rev acc
  in
  loop []

let make_node kind children =
  Ceibo.Green.make_node ~kind ~children:(Array.of_list children)

let make_token p tok =
  let text = Token_cursor.view p.cursor tok.Token.span in
  let width = String.length text in
  let kind =
    match tok.Token.kind with
    | Whitespace -> Syntax_kind.WHITESPACE
    | Comment _ -> Syntax_kind.COMMENT
    | Integer _ -> Syntax_kind.INT_LITERAL
    | String _ -> Syntax_kind.STRING_LITERAL
    | Ident _ -> Syntax_kind.IDENT
    | Variable _ -> Syntax_kind.VARIABLE
    | Wildcard -> Syntax_kind.WILDCARD
    | Dot -> Syntax_kind.DOT
    | Comma -> Syntax_kind.COMMA
    | LParen -> Syntax_kind.LPAREN
    | RParen -> Syntax_kind.RPAREN
    | Bang -> Syntax_kind.BANG
    | ColonDash -> Syntax_kind.COLON_DASH
    | Gt -> Syntax_kind.GT
    | Lt -> Syntax_kind.LT
    | GtEq -> Syntax_kind.GTEQ
    | LtEq -> Syntax_kind.LTEQ
    | Eq -> Syntax_kind.EQ
    | NotEq -> Syntax_kind.NOTEQ
    | _ -> Syntax_kind.ERROR
  in
  Ceibo.Green.Token (Ceibo.Green.make_token ~kind ~text ~width)

let tokens_to_green p tokens = List.map (make_token p) tokens

let make_error_node p ~diagnostic ~consumed_tokens =
  report_diagnostic p diagnostic;
  let children = tokens_to_green p consumed_tokens in
  make_node Syntax_kind.ERROR children

(* Parse term: variable, constant, or wildcard *)
let rec parse_term p =
  let tok = peek p in
  match tok.kind with
  | Variable v ->
      advance p;
      make_node Syntax_kind.VARIABLE [ make_token p tok ]
  | Wildcard ->
      advance p;
      make_node Syntax_kind.WILDCARD [ make_token p tok ]
  | String _ ->
      advance p;
      make_node Syntax_kind.STRING_LITERAL [ make_token p tok ]
  | Integer _ ->
      advance p;
      make_node Syntax_kind.INT_LITERAL [ make_token p tok ]
  | Ident _ ->
      advance p;
      make_node Syntax_kind.CONSTANT [ make_token p tok ]
  | _ ->
      let diag =
        Diagnostic.expected ~expected:"term" ~found:tok ~span:(current_span p)
      in
      make_error_node p ~diagnostic:diag ~consumed_tokens:[]

(* Parse comma-separated terms *)
let parse_args p =
  let rec loop acc =
    let trivia = consume_trivia p in
    match peek_kind p with
    | RParen | Eof -> List.rev acc
    | _ -> (
        let term = parse_term p in
        let acc = term :: acc in
        let trivia2 = consume_trivia p in
        match peek_kind p with
        | Comma ->
            let _ = consume p in
            loop acc
        | _ -> List.rev acc)
  in
  loop []

(* Parse atom: predicate(args) *)
let parse_atom p =
  let start_tok = peek p in
  match start_tok.kind with
  | Ident _ | String _ -> (
      let pred_tok = consume p in
      let trivia1 = consume_trivia p in
      match peek_kind p with
      | LParen -> (
          let lparen = consume p in
          let trivia2 = consume_trivia p in
          let args = parse_args p in
          let trivia3 = consume_trivia p in
          match peek_kind p with
          | RParen ->
              let rparen = consume p in
              make_node Syntax_kind.ATOM
                ([ make_token p pred_tok ]
                @ tokens_to_green p trivia1
                @ [ make_token p lparen ]
                @ tokens_to_green p trivia2
                @ List.map (fun a -> Ceibo.Green.Node a) args
                @ tokens_to_green p trivia3
                @ [ make_token p rparen ])
          | _ ->
              let diag =
                Diagnostic.missing_closing_paren ~span:(expected_span p)
              in
              make_error_node p ~diagnostic:diag ~consumed_tokens:[])
      | _ ->
          let diag =
            Diagnostic.expected ~expected:"(" ~found:(peek p)
              ~span:(current_span p)
          in
          make_error_node p ~diagnostic:diag ~consumed_tokens:[])
  | _ ->
      let diag =
        Diagnostic.expected ~expected:"predicate" ~found:start_tok
          ~span:(current_span p)
      in
      make_error_node p ~diagnostic:diag ~consumed_tokens:[]

(* Parse builtin predicate *)
let parse_builtin p left_term =
  let op_tok = consume p in
  let trivia = consume_trivia p in
  let right_term = parse_term p in
  make_node Syntax_kind.BUILTIN
    ([ Ceibo.Green.Node left_term ]
    @ [ make_token p op_tok ]
    @ tokens_to_green p trivia
    @ [ Ceibo.Green.Node right_term ])

let is_builtin_op = function
  | Token.Gt | Token.Lt | Token.GtEq | Token.LtEq | Token.Eq | Token.NotEq ->
      true
  | _ -> false

(* Parse clause in rule body *)
let parse_clause p =
  let trivia1 = consume_trivia p in
  match peek_kind p with
  | Bang ->
      let bang = consume p in
      let trivia2 = consume_trivia p in
      let atom = parse_atom p in
      make_node Syntax_kind.NEGATED_ATOM
        (tokens_to_green p trivia1
        @ [ make_token p bang ]
        @ tokens_to_green p trivia2 @ [ Ceibo.Green.Node atom ])
  | Variable _ ->
      let term = parse_term p in
      let trivia2 = consume_trivia p in
      if is_builtin_op (peek_kind p) then
        make_node Syntax_kind.BUILTIN
          (tokens_to_green p trivia1 @ [ Ceibo.Green.Node term ]
         @ tokens_to_green p trivia2
          @ [ Ceibo.Green.Node (parse_builtin p term) ])
      else
        let diag =
          Diagnostic.unexpected ~found:(peek p) ~span:(current_span p)
        in
        make_error_node p ~diagnostic:diag ~consumed_tokens:[]
  | _ ->
      let atom = parse_atom p in
      make_node Syntax_kind.ATOM
        (tokens_to_green p trivia1 @ [ Ceibo.Green.Node atom ])

(* Parse rule body clauses *)
let parse_body p =
  let rec loop acc =
    let clause = parse_clause p in
    let acc = clause :: acc in
    let trivia = consume_trivia p in
    match peek_kind p with
    | Comma ->
        let _ = consume p in
        loop acc
    | _ -> List.rev acc
  in
  loop []

(* Parse item: fact or rule *)
let parse_item p =
  consume_trivia p |> ignore;

  match peek_kind p with
  | Eof -> None
  | Comment _ ->
      let tok = consume p in
      Some (make_node Syntax_kind.COMMENT [ make_token p tok ])
  | _ -> (
      let atom = parse_atom p in
      let trivia1 = consume_trivia p in
      match peek_kind p with
      | ColonDash -> (
          (* Rule *)
          let colon_dash = consume p in
          let trivia2 = consume_trivia p in
          let body = parse_body p in
          let trivia3 = consume_trivia p in
          match peek_kind p with
          | Dot ->
              let dot = consume p in
              Some
                (make_node Syntax_kind.RULE
                   ([ Ceibo.Green.Node atom ] @ tokens_to_green p trivia1
                   @ [ make_token p colon_dash ]
                   @ tokens_to_green p trivia2
                   @ List.map (fun c -> Ceibo.Green.Node c) body
                   @ tokens_to_green p trivia3
                   @ [ make_token p dot ]))
          | _ ->
              let diag =
                Diagnostic.expected ~expected:"." ~found:(peek p)
                  ~span:(expected_span p)
              in
              Some (make_error_node p ~diagnostic:diag ~consumed_tokens:[]))
      | Dot ->
          (* Fact *)
          let dot = consume p in
          Some
            (make_node Syntax_kind.FACT
               ([ Ceibo.Green.Node atom ] @ tokens_to_green p trivia1
               @ [ make_token p dot ]))
      | _ ->
          let diag =
            Diagnostic.missing_statement_terminator ~span:(expected_span p)
          in
          Some (make_error_node p ~diagnostic:diag ~consumed_tokens:[]))

(* Parse full program *)
let parse source =
  let tokens = Lexer.tokenize source in
  let p = create ~source tokens in

  let rec loop acc =
    match peek_kind p with
    | Eof -> List.rev acc
    | _ -> (
        match parse_item p with
        | None -> loop acc
        | Some item -> loop (item :: acc))
  in

  let items = loop [] in
  let tree =
    make_node Syntax_kind.PROGRAM (List.map (fun i -> Ceibo.Green.Node i) items)
  in
  let diagnostics = List.rev (Cell.get p.diagnostics) in

  if diagnostics != [] then Error diagnostics else Ok tree

(* Parse query (one or more comma-separated atoms) *)
let parse_query source =
  let tokens = Lexer.tokenize source in
  let p = create ~source tokens in

  consume_trivia p |> ignore;
  
  (* Parse comma-separated clauses (atoms, builtins, negated atoms) *)
  let rec parse_clauses acc =
    let clause = parse_clause p in
    let acc = clause :: acc in
    let trivia = consume_trivia p in
    match peek_kind p with
    | Comma ->
        let _ = consume p in
        consume_trivia p |> ignore;
        parse_clauses acc
    | _ -> List.rev acc
  in
  
  let clauses = parse_clauses [] in
  let diagnostics = List.rev (Cell.get p.diagnostics) in

  if diagnostics != [] then Error diagnostics 
  else
    (* Return single clause or PROGRAM node with multiple clauses *)
    match clauses with
    | [single] -> Ok single
    | multiple -> Ok (make_node Syntax_kind.PROGRAM (List.map (fun c -> Ceibo.Green.Node c) multiple))
