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
  mutable pending_trivia : (Syntax_kind.t, string) Ceibo.Green.element list;
}

let create ~source tokens =
  {
    source;
    tokens = Array.of_list tokens;
    position = 0;
    diagnostics = [];
    pending_trivia = [];
  }

(* ========================================================================= *)
(* TOKEN ACCESS *)
(* ========================================================================= *)

let peek parser =
  if parser.position < Array.length parser.tokens then
    Some parser.tokens.(parser.position)
  else None

let peek_nth parser n =
  if parser.position + n < Array.length parser.tokens then
    Some parser.tokens.(parser.position + n).Token.kind
  else None

let peek_kind parser =
  match peek parser with Some tok -> Some tok.Token.kind | None -> None

let advance parser =
  if parser.position < Array.length parser.tokens then (
    let tok = parser.tokens.(parser.position) in
    parser.position <- parser.position + 1;
    Some tok)
  else None

let at parser kind =
  match peek parser with Some tok -> tok.Token.kind = kind | None -> false

let at_any parser kinds =
  match peek parser with
  | Some tok -> List.mem tok.Token.kind kinds
  | None -> false

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
  | Token.Keyword Keyword.True | Token.Keyword Keyword.False ->
      Syntax_kind.BOOL_LITERAL
  | Token.Keyword Keyword.Let -> Syntax_kind.LET_BINDING
  | Token.Keyword Keyword.Rec -> Syntax_kind.LET_REC_EXPR
  | Token.Keyword Keyword.In -> Syntax_kind.LET_EXPR
  | Token.Keyword Keyword.If -> Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Then | Token.Keyword Keyword.Else ->
      Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Fun -> Syntax_kind.FUN_EXPR
  | Token.Keyword Keyword.Function -> Syntax_kind.FUNCTION_EXPR
  | Token.Keyword Keyword.Match -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.With -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.When -> Syntax_kind.MATCH_EXPR
  | Token.Arrow -> Syntax_kind.FUN_EXPR (* -> is part of fun/function syntax *)
  | Token.Pipe ->
      Syntax_kind.MATCH_EXPR (* | is part of match/function syntax *)
  | Token.Semi -> Syntax_kind.SEQUENCE_EXPR
  | Token.Comma -> Syntax_kind.TUPLE_EXPR
  | Token.Dot ->
      Syntax_kind.IDENT_EXPR (* . can be part of operator identifiers like -. *)
  | Token.Underscore -> Syntax_kind.WILDCARD_PATTERN
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon ->
      Syntax_kind.INFIX_EXPR
  | Token.Bang -> Syntax_kind.PREFIX_EXPR
  | Token.OpenDelim _ | Token.CloseDelim _ -> Syntax_kind.WHITESPACE
  | _ -> Syntax_kind.ERROR (* TODO: Map remaining token kinds *)

let token_to_green_token parser tok =
  let text =
    String.sub parser.source tok.Token.span.start
      (tok.Token.span.end_ - tok.Token.span.start)
  in
  let width = tok.Token.span.end_ - tok.Token.span.start in
  let kind = token_kind_to_syntax_kind tok.Token.kind in
  Ceibo.Green.make_token ~kind ~text ~width

let consume parser =
  match advance parser with
  | Some tok ->
      let green_tok = token_to_green_token parser tok in
      Ceibo.Green.Token green_tok
  | None ->
      Ceibo.Green.Token
        (Ceibo.Green.make_token ~kind:Syntax_kind.ERROR ~text:"" ~width:0)

let make_node ~kind children = Ceibo.Green.make_node ~kind ~children

(* ========================================================================= *)
(* TRIVIA HANDLING *)
(* ========================================================================= *)

let consume_trivia parser =
  let rec loop acc =
    match peek parser with
    | Some tok when tok.Token.kind = Token.Whitespace ->
        let _ = advance parser in
        loop acc
    | Some tok -> (
        match tok.Token.kind with
        | Token.Comment _ | Token.Docstring _ ->
            let comment = consume parser in
            loop (comment :: acc)
        | _ ->
            let trivia = List.rev acc in
            parser.pending_trivia <- List.append parser.pending_trivia trivia;
            trivia)
    | None ->
        let trivia = List.rev acc in
        parser.pending_trivia <- List.append parser.pending_trivia trivia;
        trivia
  in
  loop []

let take_pending_trivia parser =
  let trivia = parser.pending_trivia in
  parser.pending_trivia <- [];
  trivia

let prepend_pending_trivia parser arr =
  let trivia = take_pending_trivia parser in
  Array.append (Array.of_list trivia) arr

(* ========================================================================= *)
(* ERROR RECOVERY *)
(* ========================================================================= *)

let report_error parser err = parser.diagnostics <- err :: parser.diagnostics

let make_error_node parser ~kind ~span =
  report_error parser (Diagnostic.make ~kind ~span);
  (* Create an empty error node *)
  make_node ~kind:Syntax_kind.ERROR [||]

let expect parser kind =
  match peek parser with
  | Some tok when tok.Token.kind = kind -> consume parser
  | _ ->
      (* Report missing token *)
      let span =
        match peek parser with
        | Some tok ->
            Ceibo.Span.make ~start:tok.Token.span.start
              ~end_:tok.Token.span.start
        | None ->
            let pos =
              if parser.position > 0 then
                parser.tokens.(parser.position - 1).Token.span.end_
              else 0
            in
            Ceibo.Span.make ~start:pos ~end_:pos
      in
      let err =
        Diagnostic.make_missing_token ~expected:(Token.show_kind kind) ~span
      in
      report_error parser err;
      (* Create a missing token *)
      Ceibo.Green.Token
        (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)

(* ========================================================================= *)
(* LITERALS *)
(* ========================================================================= *)

let parse_literal parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some (Token.Literal (Token.Int _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [| tok |] in
      Some (make_node ~kind:Syntax_kind.INT_LITERAL children)
  | Some (Token.Literal (Token.Float _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [| tok |] in
      Some (make_node ~kind:Syntax_kind.FLOAT_LITERAL children)
  | Some (Token.Literal (Token.String _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [| tok |] in
      Some (make_node ~kind:Syntax_kind.STRING_LITERAL children)
  | Some (Token.Literal (Token.Char _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [| tok |] in
      Some (make_node ~kind:Syntax_kind.CHAR_LITERAL children)
  | Some (Token.Keyword Keyword.True) | Some (Token.Keyword Keyword.False) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [| tok |] in
      Some (make_node ~kind:Syntax_kind.BOOL_LITERAL children)
  | _ -> None

(* ========================================================================= *)
(* EXPRESSIONS *)
(* ========================================================================= *)

let is_infix_op = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon ->
      true
  | _ -> false

let get_precedence = function
  | Token.Or -> 1
  | Token.And -> 2
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq -> 3
  | Token.ColonColon -> 4
  | Token.Plus | Token.Minus -> 5
  | Token.Star | Token.Slash | Token.Percent -> 6
  | _ -> 0

let rec parse_expr parser = parse_expr_bp parser 0

and parse_expr_bp parser min_bp =
  let _ = consume_trivia parser in

  match parse_primary parser with
  | None -> None
  | Some lhs ->
      let rec loop lhs =
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some op_kind when is_infix_op op_kind -> (
            let prec = get_precedence op_kind in
            if prec < min_bp then Some lhs
            else
              let op_tok = consume parser in
              let _ = consume_trivia parser in
              match parse_expr_bp parser (prec + 1) with
              | Some rhs ->
                  let trivia = take_pending_trivia parser in
                  let children =
                    [| Ceibo.Green.Node lhs; op_tok; Ceibo.Green.Node rhs |]
                  in
                  let children = Array.append (Array.of_list trivia) children in
                  let infix = make_node ~kind:Syntax_kind.INFIX_EXPR children in
                  loop infix
              | None -> Some lhs)
        | Some _ when can_start_primary parser -> (
            (* Function application - juxtaposition *)
            let app_prec = 8 in
            (* Highest precedence *)
            if app_prec < min_bp then Some lhs
            else
              match parse_primary parser with
              | Some rhs ->
                  let trivia = take_pending_trivia parser in
                  let children =
                    [| Ceibo.Green.Node lhs; Ceibo.Green.Node rhs |]
                  in
                  let children = Array.append (Array.of_list trivia) children in
                  let app = make_node ~kind:Syntax_kind.APPLY_EXPR children in
                  loop app
              | None -> Some lhs)
        | Some _ | None -> Some lhs
      in
      loop lhs

and can_start_primary parser =
  match peek_kind parser with
  | Some (Token.Literal _)
  | Some (Token.Ident _)
  | Some (Token.OpenDelim Token.Paren)
  | Some
      (Token.Keyword
         ( Keyword.True | Keyword.False | Keyword.If | Keyword.Match
         | Keyword.Fun | Keyword.Function ))
  | Some Token.Minus
  | Some Token.Bang ->
      true
  | _ -> false

and parse_primary parser =
  let _ = consume_trivia parser in

  (* Check for prefix operators *)
  match peek_kind parser with
  | Some Token.Minus -> (
      if
        (* Check if this is a compound operator identifier like -. *)
        peek_nth parser 1 = Some Token.Dot
      then
        (* Parse as operator identifier -. *)
        let minus = consume parser in
        let dot = consume parser in
        let children = prepend_pending_trivia parser [| minus; dot |] in
        Some (make_node ~kind:Syntax_kind.IDENT_EXPR children)
      else
        (* Parse as prefix operator - *)
        let op = consume parser in
        let _ = consume_trivia parser in
        match parse_expr_bp parser 7 with
        | Some operand ->
            let children =
              prepend_pending_trivia parser [| op; Ceibo.Green.Node operand |]
            in
            Some (make_node ~kind:Syntax_kind.PREFIX_EXPR children)
        | None -> None)
  | Some Token.Bang -> (
      let op = consume parser in
      let _ = consume_trivia parser in
      match parse_expr_bp parser 7 with
      (* Higher precedence for prefix *)
      | Some operand ->
          let children =
            prepend_pending_trivia parser [| op; Ceibo.Green.Node operand |]
          in
          Some (make_node ~kind:Syntax_kind.PREFIX_EXPR children)
      | None -> None)
  | _ -> (
      (* Try to parse a literal *)
      match parse_literal parser with
      | Some lit -> Some lit
      | None -> (
          match peek_kind parser with
          (* Identifier *)
          | Some (Token.Ident _) ->
              let ident = consume parser in
              let children = prepend_pending_trivia parser [| ident |] in
              Some (make_node ~kind:Syntax_kind.IDENT_EXPR children)
          (* Parenthesized expression *)
          | Some (Token.OpenDelim Token.Paren) -> parse_paren_expr parser
          (* List literal *)
          | Some (Token.OpenDelim Token.Bracket) -> parse_list_expr parser
          (* Let expression *)
          | Some (Token.Keyword Keyword.Let) -> parse_let_expr parser
          (* If expression *)
          | Some (Token.Keyword Keyword.If) -> parse_if_expr parser
          (* Match expression *)
          | Some (Token.Keyword Keyword.Match) -> parse_match_expr parser
          (* Fun/function *)
          | Some (Token.Keyword Keyword.Fun) -> parse_fun_expr parser
          | Some (Token.Keyword Keyword.Function) -> parse_function_expr parser
          | _ -> None))

and parse_paren_expr parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  (* Check for unit literal () *)
  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children =
      prepend_pending_trivia parser [| open_paren; close_paren |]
    in
    Some (make_node ~kind:Syntax_kind.UNIT_LITERAL children)
  else
    match parse_expr parser with
    | Some expr ->
        let _ = consume_trivia parser in
        (* Check if it's a tuple (has comma), sequence (has semicolon), or just parenthesized expr *)
        if at parser Token.Comma then parse_tuple_rest parser open_paren expr
        else if at parser Token.Semi then
          parse_sequence_rest parser open_paren expr
        else
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let children =
            prepend_pending_trivia parser
              [| open_paren; Ceibo.Green.Node expr; close_paren |]
          in
          Some (make_node ~kind:Syntax_kind.PAREN_EXPR children)
    | None ->
        let span =
          match peek parser with
          | Some tok ->
              Ceibo.Span.make ~start:tok.Token.span.start
                ~end_:tok.Token.span.end_
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        Some
          (make_error_node parser
             ~kind:
               (Diagnostic.InvalidSyntax
                  { context = "parenthesized expression" })
             ~span)

and parse_tuple_rest parser open_paren first_expr =
  let elements = ref [ Ceibo.Green.Node first_expr ] in

  while at parser Token.Comma do
    let comma = consume parser in
    let _ = consume_trivia parser in
    elements := comma :: !elements;

    match parse_expr parser with
    | Some expr ->
        elements := Ceibo.Green.Node expr :: !elements;
        let _ = consume_trivia parser in
        ()
    | None -> ()
  done;

  let close_paren = expect parser (Token.CloseDelim Token.Paren) in
  let children = Array.of_list (List.rev (close_paren :: !elements)) in
  let children = Array.append [| open_paren |] children in
  let children = prepend_pending_trivia parser children in
  Some (make_node ~kind:Syntax_kind.TUPLE_EXPR children)

and parse_sequence_rest parser open_paren first_expr =
  let elements = ref [ Ceibo.Green.Node first_expr ] in

  while at parser Token.Semi do
    let semi = consume parser in
    let _ = consume_trivia parser in
    elements := semi :: !elements;

    match parse_expr parser with
    | Some expr ->
        elements := Ceibo.Green.Node expr :: !elements;
        let _ = consume_trivia parser in
        ()
    | None -> ()
  done;

  let close_paren = expect parser (Token.CloseDelim Token.Paren) in
  let children = Array.of_list (List.rev (close_paren :: !elements)) in
  let children = Array.append [| open_paren |] children in
  let children = prepend_pending_trivia parser children in
  Some (make_node ~kind:Syntax_kind.SEQUENCE_EXPR children)

and parse_list_expr parser =
  let open_bracket = consume parser in
  let _ = consume_trivia parser in

  (* Check for empty list [] *)
  if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    let children =
      prepend_pending_trivia parser [| open_bracket; close_bracket |]
    in
    Some (make_node ~kind:Syntax_kind.LIST_EXPR children)
  else
    match parse_expr parser with
    | Some first_expr ->
        let _ = consume_trivia parser in
        (* List elements are separated by semicolons *)
        let elements = ref [ Ceibo.Green.Node first_expr ] in

        while at parser Token.Semi do
          let semi = consume parser in
          let _ = consume_trivia parser in
          elements := semi :: !elements;

          match parse_expr parser with
          | Some expr ->
              elements := Ceibo.Green.Node expr :: !elements;
              let _ = consume_trivia parser in
              ()
          | None -> ()
        done;

        let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
        let children = Array.of_list (List.rev (close_bracket :: !elements)) in
        let children = Array.append [| open_bracket |] children in
        let children = prepend_pending_trivia parser children in
        Some (make_node ~kind:Syntax_kind.LIST_EXPR children)
    | None ->
        let span =
          match peek parser with
          | Some tok ->
              Ceibo.Span.make ~start:tok.Token.span.start
                ~end_:tok.Token.span.end_
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        Some
          (make_error_node parser
             ~kind:(Diagnostic.InvalidSyntax { context = "list expression" })
             ~span)

and parse_let_expr parser =
  let let_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for 'rec' *)
  let is_rec = at parser (Token.Keyword Keyword.Rec) in
  let rec_kw =
    if is_rec then
      let kw = consume parser in
      let _ = consume_trivia parser in
      Some kw
    else None
  in

  (* Parse pattern (for now, just identifier) *)
  let pattern =
    match peek_kind parser with
    | Some (Token.Ident _) -> consume parser
    | _ ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"identifier" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Expect '=' *)
  let eq = expect parser Token.Eq in

  let _ = consume_trivia parser in

  (* Parse value expression *)
  let value_expr =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in

  let _ = consume_trivia parser in

  (* Parse body expression *)
  let body_expr =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let kind =
    if is_rec then Syntax_kind.LET_REC_EXPR else Syntax_kind.LET_EXPR
  in

  match rec_kw with
  | Some kw ->
      Some
        (make_node ~kind
           [| let_kw; kw; pattern; eq; value_expr; in_kw; body_expr |])
  | None ->
      Some
        (make_node ~kind
           [| let_kw; pattern; eq; value_expr; in_kw; body_expr |])

and parse_if_expr parser =
  let if_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse condition *)
  let cond =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"condition" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Expect 'then' *)
  let then_kw = expect parser (Token.Keyword Keyword.Then) in

  let _ = consume_trivia parser in

  (* Parse then branch *)
  let then_expr =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Check for 'else' *)
  let has_else = at parser (Token.Keyword Keyword.Else) in
  if has_else then
    let else_kw = consume parser in
    let _ = consume_trivia parser in

    (* Parse else branch *)
    let else_expr =
      match parse_expr parser with
      | Some e -> Ceibo.Green.Node e
      | None ->
          let span =
            match peek parser with
            | Some tok -> tok.Token.span
            | None -> Ceibo.Span.make ~start:0 ~end_:0
          in
          report_error parser
            (Diagnostic.make_missing_token ~expected:"expression" ~span);
          Ceibo.Green.Token
            (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
    in

    Some
      (make_node ~kind:Syntax_kind.IF_EXPR
         [| if_kw; cond; then_kw; then_expr; else_kw; else_expr |])
  else
    Some
      (make_node ~kind:Syntax_kind.IF_EXPR
         [| if_kw; cond; then_kw; then_expr |])

and parse_fun_expr parser =
  let fun_kw = consume parser in
  let _ = consume_trivia parser in

  let params = ref [] in
  let continue = ref true in
  while !continue && (not (at parser Token.Arrow)) && peek parser <> None do
    match parse_pattern parser with
    | Some pat ->
        params := Ceibo.Green.Node pat :: !params;
        let _ = consume_trivia parser in
        ()
    | None -> continue := false
  done;

  let arrow = expect parser Token.Arrow in

  let _ = consume_trivia parser in

  let body =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"expression" ~span in
        report_error parser err;
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let children = List.rev (body :: arrow :: !params) in
  let children = prepend_pending_trivia parser (Array.of_list children) in
  let all_children = Array.append [| fun_kw |] children in

  Some (make_node ~kind:Syntax_kind.FUN_EXPR all_children)

and parse_function_expr parser =
  let function_kw = consume parser in
  let _ = consume_trivia parser in

  let cases = ref [] in
  (* Handle first case - may or may not have leading | *)
  if at parser Token.Pipe then (
    (* Has leading | - use normal match case parsing *)
    while at parser Token.Pipe do
      match parse_match_case parser with
      | Some case -> cases := Ceibo.Green.Node case :: !cases
      | None -> ()
    done)
  else (
    (* No leading | - parse pattern -> expr directly *)
    match parse_pattern parser with
    | Some first_pattern ->
        let _ = consume_trivia parser in
        
        (* Check for tuple pattern (comma) or or-pattern (pipe) *)
        let base_pattern =
          if at parser Token.Comma then (
            (* Tuple pattern *)
            let patterns = ref [ Ceibo.Green.Node first_pattern ] in
            while at parser Token.Comma do
              let comma = consume parser in
              let _ = consume_trivia parser in

              match parse_pattern parser with
              | Some pat ->
                  patterns := Ceibo.Green.Node pat :: comma :: !patterns;
                  let trivia = consume_trivia parser in
                  patterns := List.rev_append trivia !patterns
              | None -> ()
            done;
            make_node ~kind:Syntax_kind.TUPLE_PATTERN
              (Array.of_list (List.rev !patterns)))
          else if at parser Token.Pipe && not (at parser Token.Arrow) then (
            (* Or pattern *)
            let patterns = ref [ Ceibo.Green.Node first_pattern ] in
            while at parser Token.Pipe && not (at_any parser [ Token.Arrow ]) do
              let pipe_tok = consume parser in
              let _ = consume_trivia parser in

              match parse_pattern parser with
              | Some pat ->
                  patterns := Ceibo.Green.Node pat :: pipe_tok :: !patterns;
                  let trivia = consume_trivia parser in
                  patterns := List.rev_append trivia !patterns
              | None -> ()
            done;
            make_node ~kind:Syntax_kind.OR_PATTERN
              (Array.of_list (List.rev !patterns)))
          else first_pattern
        in
        
        (* Check for cons pattern (::) *)
        let _ = consume_trivia parser in
        let pattern =
          if at parser Token.ColonColon then
            let cons_op = consume parser in
            let _ = consume_trivia parser in
            
            match parse_pattern parser with
            | Some tail_pat ->
                Ceibo.Green.Node
                  (make_node ~kind:Syntax_kind.CONS_PATTERN
                     [| Ceibo.Green.Node base_pattern; cons_op; Ceibo.Green.Node tail_pat |])
            | None -> Ceibo.Green.Node base_pattern
          else Ceibo.Green.Node base_pattern
        in
        
        (* Handle guard (when clause) *)
        let _ = consume_trivia parser in
        let guard =
          if at parser (Token.Keyword Keyword.When) then
            let when_kw = consume parser in
            let _ = consume_trivia parser in

            match parse_expr parser with
            | Some e ->
                let _ = consume_trivia parser in
                Some
                  (Ceibo.Green.Node
                     (make_node ~kind:Syntax_kind.PATTERN_GUARD
                        [| when_kw; Ceibo.Green.Node e |]))
            | None -> None
          else None
        in
        
        let arrow =
          if at parser Token.Arrow then consume parser
          else
            let span =
              match peek parser with
              | Some tok -> tok.Token.span
              | None -> Ceibo.Span.make ~start:0 ~end_:0
            in
            let err = Diagnostic.make_missing_token ~expected:"'->'" ~span in
            report_error parser err;
            Ceibo.Green.Token (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
        in
        let _ = consume_trivia parser in
        match parse_expr parser with
        | Some expr ->
            let case_children = match guard with
              | Some g -> [| pattern; g; arrow; Ceibo.Green.Node expr |]
              | None -> [| pattern; arrow; Ceibo.Green.Node expr |]
            in
            let first_case = make_node ~kind:Syntax_kind.MATCH_CASE case_children in
            cases := [ Ceibo.Green.Node first_case ];
            (* Parse remaining cases with | *)
            while at parser Token.Pipe do
              match parse_match_case parser with
              | Some case -> cases := Ceibo.Green.Node case :: !cases
              | None -> ()
            done
        | None -> ()
    | None -> ());

  let children = List.rev !cases in
  let children = prepend_pending_trivia parser (Array.of_list children) in
  let all_children = Array.append [| function_kw |] children in

  Some (make_node ~kind:Syntax_kind.FUNCTION_EXPR all_children)

and parse_pattern parser =
  let _ = consume_trivia parser in

  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let underscore = consume parser in
      Some (make_node ~kind:Syntax_kind.WILDCARD_PATTERN [| underscore |])
  (* List pattern [] or [a; b; c] *)
  | Some (Token.OpenDelim Token.Bracket) -> parse_list_pattern parser
  (* Identifier or constructor pattern *)
  | Some (Token.Ident _) -> parse_ident_or_constructor_pattern parser
  (* Literal pattern *)
  | Some (Token.Literal _)
  | Some (Token.Keyword Keyword.True)
  | Some (Token.Keyword Keyword.False) -> (
      match parse_literal parser with
      | Some lit ->
          Some
            (make_node ~kind:Syntax_kind.LITERAL_PATTERN
               [| Ceibo.Green.Node lit |])
      | None -> None)
  (* Parenthesized pattern or tuple *)
  | Some (Token.OpenDelim Token.Paren) -> parse_paren_pattern parser
  | _ -> None

and parse_list_pattern parser =
  let open_bracket = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    let children = [| open_bracket |] in
    let children = Array.append children (Array.of_list comments1) in
    let children = Array.append children [| close_bracket |] in
    Some (make_node ~kind:Syntax_kind.LIST_PATTERN children)
  else
    let elements = ref (List.rev comments1) in
    (match parse_pattern parser with
    | Some pat -> elements := Ceibo.Green.Node pat :: !elements
    | None -> ());

    let comments2 = consume_trivia parser in
    elements := List.rev_append comments2 !elements;

    while at parser Token.Semi do
      let semi = consume parser in
      let comments3 = consume_trivia parser in
      elements := List.rev_append comments3 (semi :: !elements);
      match parse_pattern parser with
      | Some pat ->
          elements := Ceibo.Green.Node pat :: !elements;
          let comments4 = consume_trivia parser in
          elements := List.rev_append comments4 !elements
      | None -> ()
    done;

    let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
    let children = Array.of_list (List.rev (close_bracket :: !elements)) in
    let all_children = Array.append [| open_bracket |] children in
    Some (make_node ~kind:Syntax_kind.LIST_PATTERN all_children)

and parse_ident_or_constructor_pattern parser =
  let ident = consume parser in
  let _ = consume_trivia parser in

  if at parser Token.ColonColon then
    let cons_op = consume parser in
    let _ = consume_trivia parser in

    match parse_pattern parser with
    | Some tail_pat ->
        Some
          (make_node ~kind:Syntax_kind.CONS_PATTERN
             [| ident; cons_op; Ceibo.Green.Node tail_pat |])
    | None -> Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |])
  else
    match peek_kind parser with
    | Some (Token.Ident _)
    | Some (Token.OpenDelim Token.Paren)
    | Some Token.Underscore
    | Some (Token.Literal _)
    | Some (Token.OpenDelim Token.Bracket) -> (
        match parse_pattern parser with
        | Some arg_pat ->
            Some
              (make_node ~kind:Syntax_kind.CONSTRUCTOR_PATTERN
                 [| ident; Ceibo.Green.Node arg_pat |])
        | None -> Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |]))
    | _ -> Some (make_node ~kind:Syntax_kind.IDENT_PATTERN [| ident |])

and parse_paren_pattern parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children =
      prepend_pending_trivia parser [| open_paren; close_paren |]
    in
    Some (make_node ~kind:Syntax_kind.PAREN_PATTERN children)
  else
    match parse_pattern parser with
    | Some first_pat ->
        let _ = consume_trivia parser in

        if at parser Token.Comma then (
          let elements = ref [ Ceibo.Green.Node first_pat ] in
          while at parser Token.Comma do
            let comma = consume parser in
            let _ = consume_trivia parser in

            match parse_pattern parser with
            | Some pat ->
                elements := Ceibo.Green.Node pat :: comma :: !elements;
                let trivia = consume_trivia parser in
                elements := List.rev_append trivia !elements
            | None -> ()
          done;

          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let children = Array.of_list (List.rev (close_paren :: !elements)) in
          let all_children = Array.append [| open_paren |] children in
          Some (make_node ~kind:Syntax_kind.TUPLE_PATTERN all_children))
        else
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          Some
            (make_node ~kind:Syntax_kind.PAREN_PATTERN
               [| open_paren; Ceibo.Green.Node first_pat; close_paren |])
    | None ->
        let span =
          match peek parser with
          | Some tok ->
              Ceibo.Span.make ~start:tok.Token.span.start
                ~end_:tok.Token.span.end_
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        Some
          (make_error_node parser
             ~kind:
               (Diagnostic.InvalidSyntax { context = "parenthesized pattern" })
             ~span)

and parse_match_expr parser =
  let match_kw = consume parser in
  let _ = consume_trivia parser in

  let scrutinee =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"expression" ~span in
        report_error parser err;
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  let with_kw = expect parser (Token.Keyword Keyword.With) in

  let _ = consume_trivia parser in

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
  let _ = consume_trivia parser in

  let first_pattern =
    match parse_pattern parser with
    | Some pat -> pat
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"pattern" ~span in
        report_error parser err;
        make_node ~kind:Syntax_kind.MISSING [||]
  in

  let _ = consume_trivia parser in

  let pattern =
    if at parser Token.Comma then (
      (* Tuple pattern *)
      let patterns = ref [ Ceibo.Green.Node first_pattern ] in
      while at parser Token.Comma do
        let comma = consume parser in
        let _ = consume_trivia parser in

        match parse_pattern parser with
        | Some pat ->
            patterns := Ceibo.Green.Node pat :: comma :: !patterns;
            let trivia = consume_trivia parser in
            patterns := List.rev_append trivia !patterns
        | None -> ()
      done;
      Ceibo.Green.Node
        (make_node ~kind:Syntax_kind.TUPLE_PATTERN
           (Array.of_list (List.rev !patterns))))
    else if at parser Token.Pipe then (
      (* Or pattern *)
      let patterns = ref [ Ceibo.Green.Node first_pattern ] in
      while at parser Token.Pipe && not (at_any parser [ Token.Arrow ]) do
        let pipe_tok = consume parser in
        let _ = consume_trivia parser in

        match parse_pattern parser with
        | Some pat ->
            patterns := Ceibo.Green.Node pat :: pipe_tok :: !patterns;
            let trivia = consume_trivia parser in
            patterns := List.rev_append trivia !patterns
        | None -> ()
      done;
      Ceibo.Green.Node
        (make_node ~kind:Syntax_kind.OR_PATTERN
           (Array.of_list (List.rev !patterns))))
    else Ceibo.Green.Node first_pattern
  in

  (* Check for cons pattern (::) *)
  let _ = consume_trivia parser in
  let pattern =
    match pattern with
    | Ceibo.Green.Node base_pattern ->
        if at parser Token.ColonColon then
          let cons_op = consume parser in
          let _ = consume_trivia parser in
          
          match parse_pattern parser with
          | Some tail_pat ->
              Ceibo.Green.Node
                (make_node ~kind:Syntax_kind.CONS_PATTERN
                   [| Ceibo.Green.Node base_pattern; cons_op; Ceibo.Green.Node tail_pat |])
          | None -> Ceibo.Green.Node base_pattern
        else Ceibo.Green.Node base_pattern
    | other -> other
  in

  let guard =
    if at parser (Token.Keyword Keyword.When) then
      let when_kw = consume parser in
      let _ = consume_trivia parser in

      match parse_expr parser with
      | Some e ->
          let _ = consume_trivia parser in
          Some
            (Ceibo.Green.Node
               (make_node ~kind:Syntax_kind.PATTERN_GUARD
                  [| when_kw; Ceibo.Green.Node e |]))
      | None -> None
    else None
  in

  let arrow = expect parser Token.Arrow in

  let _ = consume_trivia parser in

  let expr =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"expression" ~span in
        report_error parser err;
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  let children =
    match guard with
    | Some g -> [| pipe; pattern; g; arrow; expr |]
    | None -> [| pipe; pattern; arrow; expr |]
  in

  Some (make_node ~kind:Syntax_kind.MATCH_CASE children)

(* ========================================================================= *)
(* TOP-LEVEL *)
(* ========================================================================= *)

let rec parse_structure_item parser =
  let _ = consume_trivia parser in

  match peek_kind parser with
  | Some (Token.Keyword Keyword.Let) -> parse_let_binding parser
  | Some (Token.Keyword Keyword.Open) -> parse_open parser
  | _ -> None

and parse_let_binding parser =
  let let_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for 'rec' *)
  let is_rec = at parser (Token.Keyword Keyword.Rec) in
  let rec_kw =
    if is_rec then
      let kw = consume parser in
      let _ = consume_trivia parser in
      Some kw
    else None
  in

  (* Parse pattern (for now, just identifier) *)
  let pattern =
    match peek_kind parser with
    | Some (Token.Ident _) -> consume parser
    | _ ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"identifier" ~span in
        report_error parser err;
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Expect '=' *)
  let eq = expect parser Token.Eq in

  let _ = consume_trivia parser in

  (* Parse expression (for now, just literals and identifiers) *)
  let expr =
    match parse_expr parser with
    | Some e -> Ceibo.Green.Node e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        let err = Diagnostic.make_missing_token ~expected:"expression" ~span in
        report_error parser err;
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  (* Skip trailing whitespace/newlines *)
  let _ = consume_trivia parser in

  match rec_kw with
  | Some kw ->
      Some
        (make_node ~kind:Syntax_kind.LET_BINDING
           [| let_kw; kw; pattern; eq; expr |])
  | None ->
      Some
        (make_node ~kind:Syntax_kind.LET_BINDING
           [| let_kw; pattern; eq; expr |])

and parse_open parser =
  let open_kw = consume parser in
  let _ = consume_trivia parser in

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
