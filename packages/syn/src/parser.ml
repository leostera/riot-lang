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
  | Token.Keyword Keyword.Rec -> Syntax_kind.LET_REC_EXPR
  | Token.Keyword Keyword.In -> Syntax_kind.LET_EXPR
  | Token.Keyword Keyword.If -> Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Then | Token.Keyword Keyword.Else -> Syntax_kind.IF_EXPR
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon
  -> Syntax_kind.INFIX_EXPR
  | Token.Bang -> Syntax_kind.PREFIX_EXPR
  | Token.OpenDelim _ | Token.CloseDelim _ -> Syntax_kind.WHITESPACE
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
(* EXPRESSIONS *)
(* ========================================================================= *)

let is_infix_op = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon
  -> true
  | _ -> false

let get_precedence = function
  | Token.Or -> 1
  | Token.And -> 2
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq -> 3
  | Token.ColonColon -> 4
  | Token.Plus | Token.Minus -> 5
  | Token.Star | Token.Slash | Token.Percent -> 6
  | _ -> 0

let rec parse_expr parser =
  parse_expr_bp parser 0

and parse_expr_bp parser min_bp =
  skip_trivia parser;
  
  match parse_primary parser with
  | None -> None
  | Some lhs ->
      let rec loop lhs =
        skip_trivia parser;
        match peek_kind parser with
        | Some op_kind when is_infix_op op_kind ->
            let prec = get_precedence op_kind in
            if prec < min_bp then
              Some lhs
            else begin
              let op_tok = consume parser in
              skip_trivia parser;
              match parse_expr_bp parser (prec + 1) with
              | Some rhs ->
                  let infix = make_node ~kind:Syntax_kind.INFIX_EXPR 
                    [| Ceibo.Green.Node lhs; op_tok; Ceibo.Green.Node rhs |] in
                  loop infix
              | None -> Some lhs
            end
        | Some _ when can_start_primary parser ->
            (* Function application - juxtaposition *)
            let app_prec = 8 in  (* Highest precedence *)
            if app_prec < min_bp then
              Some lhs
            else begin
              match parse_primary parser with
              | Some rhs ->
                  let app = make_node ~kind:Syntax_kind.APPLY_EXPR
                    [| Ceibo.Green.Node lhs; Ceibo.Green.Node rhs |] in
                  loop app
              | None -> Some lhs
            end
        | Some _ | None -> Some lhs
      in
      loop lhs

and can_start_primary parser =
  match peek_kind parser with
  | Some (Token.Literal _) | Some (Token.Ident _) | Some (Token.OpenDelim Token.Paren)
  | Some (Token.Keyword (Keyword.True | Keyword.False | Keyword.Let | Keyword.If 
                        | Keyword.Match | Keyword.Fun | Keyword.Function))
  | Some (Token.Minus) | Some (Token.Bang)
  -> true
  | _ -> false

and parse_primary parser =
  skip_trivia parser;
  
  (* Check for prefix operators *)
  match peek_kind parser with
  | Some Token.Minus | Some Token.Bang ->
      let op = consume parser in
      skip_trivia parser;
      (match parse_expr_bp parser 7 with  (* Higher precedence for prefix *)
       | Some operand ->
           Some (make_node ~kind:Syntax_kind.PREFIX_EXPR [| op; Ceibo.Green.Node operand |])
       | None -> None)
  | _ ->
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
  let let_kw = consume parser in
  skip_trivia parser;
  
  (* Check for 'rec' *)
  let is_rec = at parser (Token.Keyword Keyword.Rec) in
  let rec_kw = if is_rec then begin
    let kw = consume parser in
    skip_trivia parser;
    Some kw
  end else None in
  
  (* Parse pattern (for now, just identifier) *)
  let pattern = match peek_kind parser with
    | Some (Token.Ident _) -> consume parser
    | _ -> 
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser (Diagnostic.make_missing_token ~expected:"identifier" ~span);
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  skip_trivia parser;
  
  (* Expect '=' *)
  let eq = expect parser Token.Eq in
  
  skip_trivia parser;
  
  (* Parse value expression *)
  let value_expr = match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  skip_trivia parser;
  
  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in
  
  skip_trivia parser;
  
  (* Parse body expression *)
  let body_expr = match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  let kind = if is_rec then Syntax_kind.LET_REC_EXPR else Syntax_kind.LET_EXPR in
  
  match rec_kw with
  | Some kw ->
      Some (make_node ~kind [| let_kw; kw; pattern; eq; value_expr; in_kw; body_expr |])
  | None ->
      Some (make_node ~kind [| let_kw; pattern; eq; value_expr; in_kw; body_expr |])

and parse_if_expr parser =
  let if_kw = consume parser in
  skip_trivia parser;
  
  (* Parse condition *)
  let cond = match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser (Diagnostic.make_missing_token ~expected:"condition" ~span);
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  skip_trivia parser;
  
  (* Expect 'then' *)
  let then_kw = expect parser (Token.Keyword Keyword.Then) in
  
  skip_trivia parser;
  
  (* Parse then branch *)
  let then_expr = match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in
  
  skip_trivia parser;
  
  (* Check for 'else' *)
  let has_else = at parser (Token.Keyword Keyword.Else) in
  if has_else then begin
    let else_kw = consume parser in
    skip_trivia parser;
    
    (* Parse else branch *)
    let else_expr = match parse_expr parser with
      | Some e -> Ceibo.Green.Node e
      | None ->
          let span = match peek parser with
            | Some tok -> tok.Token.span
            | None -> Ceibo.Span.make ~start:0 ~end_:0
          in
          report_error parser (Diagnostic.make_missing_token ~expected:"expression" ~span);
          Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
    in
    
    Some (make_node ~kind:Syntax_kind.IF_EXPR 
      [| if_kw; cond; then_kw; then_expr; else_kw; else_expr |])
  end else begin
    Some (make_node ~kind:Syntax_kind.IF_EXPR 
      [| if_kw; cond; then_kw; then_expr |])
  end

and parse_fun_expr parser =
  (* TODO: Implement *)
  None

and parse_function_expr parser =
  (* TODO: Implement *)
  None

and parse_pattern parser =
  skip_trivia parser;
  
  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let underscore = consume parser in
      Some (make_node ~kind:Syntax_kind.WILDCARD_PATTERN [| underscore |])
  
  (* List pattern [] or [a; b; c] *)
  | Some (Token.OpenDelim Token.Bracket) ->
      parse_list_pattern parser
  
  (* Identifier or constructor pattern *)
  | Some (Token.Ident _) ->
      parse_ident_or_constructor_pattern parser
  
  (* Literal pattern *)
  | Some (Token.Literal _) | Some (Token.Keyword Keyword.True) 
  | Some (Token.Keyword Keyword.False) -> (
      match parse_literal parser with
      | Some lit -> Some (make_node ~kind:Syntax_kind.LITERAL_PATTERN [| Ceibo.Green.Node lit |])
      | None -> None)
  
  (* Parenthesized pattern or tuple *)
  | Some (Token.OpenDelim Token.Paren) ->
      parse_paren_pattern parser
  
  | _ -> None

and parse_list_pattern parser =
  let open_bracket = consume parser in
  skip_trivia parser;
  
  if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    Some (make_node ~kind:Syntax_kind.LIST_PATTERN [| open_bracket; close_bracket |])
  else
    let elements = ref [] in
    (match parse_pattern parser with
     | Some pat -> elements := [Ceibo.Green.Node pat]
     | None -> ());
    
    skip_trivia parser;
    
    while at parser Token.Semi do
      let semi = consume parser in
      skip_trivia parser;
      match parse_pattern parser with
      | Some pat ->
          elements := Ceibo.Green.Node pat :: semi :: !elements;
          skip_trivia parser
      | None -> ()
    done;
    
    let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
    let children = Array.of_list (List.rev (close_bracket :: !elements)) in
    let all_children = Array.append [| open_bracket |] children in
    Some (make_node ~kind:Syntax_kind.LIST_PATTERN all_children)

and parse_ident_or_constructor_pattern parser =
  let ident = consume parser in
  skip_trivia parser;
  
  if at parser Token.ColonColon then
    let cons_op = consume parser in
    skip_trivia parser;
    
    match parse_pattern parser with
    | Some tail_pat ->
        Some (make_node ~kind:Syntax_kind.CONS_PATTERN [| ident; cons_op; Ceibo.Green.Node tail_pat |])
    | None ->
        Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |])
  else
    match peek_kind parser with
    | Some (Token.Ident _) | Some (Token.OpenDelim Token.Paren) 
    | Some Token.Underscore | Some (Token.Literal _) 
    | Some (Token.OpenDelim Token.Bracket) ->
        (match parse_pattern parser with
         | Some arg_pat ->
             Some (make_node ~kind:Syntax_kind.CONSTRUCTOR_PATTERN [| ident; Ceibo.Green.Node arg_pat |])
         | None ->
             Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |]))
    | _ ->
        Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |])

and parse_paren_pattern parser =
  let open_paren = consume parser in
  skip_trivia parser;
  
  match parse_pattern parser with
  | Some first_pat ->
      skip_trivia parser;
      
      if at parser Token.Comma then
        let elements = ref [Ceibo.Green.Node first_pat] in
        while at parser Token.Comma do
          let comma = consume parser in
          skip_trivia parser;
          
          match parse_pattern parser with
          | Some pat ->
              elements := Ceibo.Green.Node pat :: comma :: !elements;
              skip_trivia parser
          | None -> ()
        done;
        
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in
        let children = Array.of_list (List.rev (close_paren :: !elements)) in
        let all_children = Array.append [| open_paren |] children in
        Some (make_node ~kind:Syntax_kind.TUPLE_PATTERN all_children)
      else
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in
        Some (make_node ~kind:Syntax_kind.PAREN_PATTERN [| open_paren; Ceibo.Green.Node first_pat; close_paren |])
  | None ->
      let span = match peek parser with
        | Some tok -> Ceibo.Span.make ~start:tok.Token.span.start ~end_:tok.Token.span.end_
        | None -> Ceibo.Span.make ~start:0 ~end_:0
      in
      Some (make_error_node parser ~kind:(Diagnostic.InvalidSyntax { context = "parenthesized pattern" }) ~span)

and parse_match_expr parser =
  let match_kw = consume parser in
  skip_trivia parser;
  
  let scrutinee = match parse_expr parser with
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
  
  skip_trivia parser;
  
  let with_kw = expect parser (Token.Keyword Keyword.With) in
  
  skip_trivia parser;
  
  let cases = ref [] in
  while at parser Token.Pipe do
    match parse_match_case parser with
    | Some case -> cases := Ceibo.Green.Node case :: !cases
    | None -> ()
  done;
  
  let children = [| match_kw; scrutinee; with_kw |] in
  let all_children = Array.append children (Array.of_list (List.rev !cases)) in
  
  Some (make_node ~kind:Syntax_kind.MATCH_EXPR all_children)

and parse_match_case parser =
  let pipe = consume parser in
  skip_trivia parser;
  
  let first_pattern = match parse_pattern parser with
    | Some pat -> pat
    | None ->
        let span = match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"pattern" ~span in
        report_error parser err;
        make_node ~kind:Syntax_kind.MISSING [||]
  in
  
  skip_trivia parser;
  
  let pattern = 
    if at parser Token.Pipe then
      let patterns = ref [Ceibo.Green.Node first_pattern] in
      while at parser Token.Pipe && not (at_any parser [Token.Arrow]) do
        let pipe_tok = consume parser in
        skip_trivia parser;
        
        match parse_pattern parser with
        | Some pat ->
            patterns := Ceibo.Green.Node pat :: pipe_tok :: !patterns;
            skip_trivia parser
        | None -> ()
      done;
      Ceibo.Green.Node (make_node ~kind:Syntax_kind.OR_PATTERN (Array.of_list (List.rev !patterns)))
    else
      Ceibo.Green.Node first_pattern
  in
  
  let guard = 
    if at parser (Token.Keyword Keyword.When) then
      let when_kw = consume parser in
      skip_trivia parser;
      
      match parse_expr parser with
      | Some e -> 
          skip_trivia parser;
          Some (Ceibo.Green.Node (make_node ~kind:Syntax_kind.PATTERN_GUARD [| when_kw; Ceibo.Green.Node e |]))
      | None ->
          None
    else
      None
  in
  
  let arrow = expect parser Token.Arrow in
  
  skip_trivia parser;
  
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
  
  skip_trivia parser;
  
  let children = match guard with
    | Some g -> [| pipe; pattern; g; arrow; expr |]
    | None -> [| pipe; pattern; arrow; expr |]
  in
  
  Some (make_node ~kind:Syntax_kind.MATCH_CASE children)



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
