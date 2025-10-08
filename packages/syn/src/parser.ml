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
  | Token.Keyword Keyword.And ->
      Syntax_kind.LET_EXPR (* 'and' in let bindings *)
  | Token.Keyword Keyword.If -> Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Then | Token.Keyword Keyword.Else ->
      Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Fun -> Syntax_kind.FUN_EXPR
  | Token.Keyword Keyword.Function -> Syntax_kind.FUNCTION_EXPR
  | Token.Keyword Keyword.Match -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.Try -> Syntax_kind.TRY_EXPR
  | Token.Keyword Keyword.With -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.When -> Syntax_kind.MATCH_EXPR
  | Token.Arrow -> Syntax_kind.FUN_EXPR (* -> is part of fun/function syntax *)
  | Token.Pipe ->
      Syntax_kind.MATCH_EXPR (* | is part of match/function syntax *)
  | Token.Semi -> Syntax_kind.SEQUENCE_EXPR
  | Token.Comma -> Syntax_kind.TUPLE_EXPR
  | Token.Colon -> Syntax_kind.TYPED_EXPR
  | Token.Dot ->
      Syntax_kind.IDENT_EXPR (* . can be part of operator identifiers like -. *)
  | Token.Underscore -> Syntax_kind.WILDCARD_PATTERN
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon | Token.Caret | Token.At
  | Token.ColonEq | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq
  | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent
  | Token.Keyword
      ( Keyword.Mod | Keyword.Land | Keyword.Lor | Keyword.Lxor | Keyword.Lsl
      | Keyword.Lsr | Keyword.Asr ) ->
      Syntax_kind.INFIX_EXPR
  | Token.Bang | Token.Keyword Keyword.Lnot -> Syntax_kind.PREFIX_EXPR
  | Token.Tilde | Token.Question -> Syntax_kind.ARGUMENT
  | Token.Backtick -> Syntax_kind.POLY_VARIANT_EXPR
  | Token.Keyword Keyword.Assert -> Syntax_kind.ASSERT_EXPR
  | Token.Keyword Keyword.Lazy -> Syntax_kind.LAZY_EXPR
  | Token.Keyword Keyword.As -> Syntax_kind.AS_PATTERN
  | Token.Keyword Keyword.For
  | Token.Keyword (Keyword.To | Keyword.Downto)
  | Token.Keyword Keyword.Do
  | Token.Keyword Keyword.Done ->
      Syntax_kind.FOR_EXPR
  | Token.Keyword Keyword.While -> Syntax_kind.WHILE_EXPR
  | Token.Keyword (Keyword.Begin | Keyword.End) -> Syntax_kind.PAREN_EXPR
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

let make_node_list ~kind children = Ceibo.Green.make_node_list ~kind children

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

let prepend_pending_trivia parser lst =
  let trivia = take_pending_trivia parser in
  trivia @ lst

(* ========================================================================= *)
(* ERROR RECOVERY *)
(* ========================================================================= *)

let report_error parser err = parser.diagnostics <- err :: parser.diagnostics

let make_error_node parser ~kind ~span =
  report_error parser (Diagnostic.make ~kind ~span);
  (* Create an empty error node *)
  make_node_list ~kind:Syntax_kind.ERROR []

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
      let children = prepend_pending_trivia parser [tok] in
      Some (make_node_list ~kind:Syntax_kind.INT_LITERAL children)
  | Some (Token.Literal (Token.Float _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [tok] in
      Some (make_node_list ~kind:Syntax_kind.FLOAT_LITERAL children)
  | Some (Token.Literal (Token.String _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [tok] in
      Some (make_node_list ~kind:Syntax_kind.STRING_LITERAL children)
  | Some (Token.Literal (Token.Char _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [tok] in
      Some (make_node_list ~kind:Syntax_kind.CHAR_LITERAL children)
  | Some (Token.Keyword Keyword.True) | Some (Token.Keyword Keyword.False) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [tok] in
      Some (make_node_list ~kind:Syntax_kind.BOOL_LITERAL children)
  | _ -> None

(* ========================================================================= *)
(* EXPRESSIONS *)
(* ========================================================================= *)

let is_constructor_ident name =
  match String.get name 0 with 'A' .. 'Z' -> true | _ -> false

let is_infix_op = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon | Token.Caret | Token.At
  | Token.ColonEq | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq
  | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent
  | Token.Keyword
      ( Keyword.Mod | Keyword.Land | Keyword.Lor | Keyword.Lxor | Keyword.Lsl
      | Keyword.Lsr | Keyword.Asr ) ->
      true
  | _ -> false

let get_precedence = function
  | Token.Or -> 1
  | Token.And -> 2
  | Token.LeftArrow | Token.ColonEq | Token.Eq | Token.Ne | Token.Lt | Token.Gt
  | Token.LtEq | Token.GtEq | Token.EqEq | Token.BangEq ->
      3
  | Token.ColonColon -> 4
  | Token.Caret | Token.At | Token.Plus | Token.Minus -> 5
  | Token.Star | Token.Slash | Token.Percent | Token.Keyword Keyword.Mod -> 6
  | Token.StarStar -> 7
  | Token.Keyword (Keyword.Land | Keyword.Lor | Keyword.Lxor) -> 3
  | Token.Keyword (Keyword.Lsl | Keyword.Lsr | Keyword.Asr) -> 6
  | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent -> 0
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
        | Some Token.Dot -> (
            (* Field/array/string access - highest precedence (9) *)
            let access_prec = 9 in
            if access_prec < min_bp then Some lhs
            else
              let dot = consume parser in
              let _ = consume_trivia parser in
              match peek_kind parser with
              | Some (Token.OpenDelim Token.Paren) -> (
                  (* Array indexing: arr.(i) *)
                  let open_paren = consume parser in
                  let _ = consume_trivia parser in
                  match parse_expr parser with
                  | Some index ->
                      let _ = consume_trivia parser in
                      let close_paren =
                        expect parser (Token.CloseDelim Token.Paren)
                      in
                      let trivia = take_pending_trivia parser in
                      let children =
                        [
                          Ceibo.Green.Node lhs;
                          dot;
                          open_paren;
                          Ceibo.Green.Node index;
                          close_paren;
                        ]
                      in
                      let children =
                        trivia @ children
                      in
                      let access =
                        make_node_list ~kind:Syntax_kind.ARRAY_INDEX_EXPR children
                      in
                      loop access
                  | None -> Some lhs)
              | Some (Token.OpenDelim Token.Bracket) -> (
                  (* String indexing: s.[i] *)
                  let open_bracket = consume parser in
                  let _ = consume_trivia parser in
                  match parse_expr parser with
                  | Some index ->
                      let _ = consume_trivia parser in
                      let close_bracket =
                        expect parser (Token.CloseDelim Token.Bracket)
                      in
                      let trivia = take_pending_trivia parser in
                      let children =
                        [
                          Ceibo.Green.Node lhs;
                          dot;
                          open_bracket;
                          Ceibo.Green.Node index;
                          close_bracket;
                        ]
                      in
                      let children =
                        trivia @ children
                      in
                      let access =
                        make_node_list ~kind:Syntax_kind.STRING_INDEX_EXPR children
                      in
                      loop access
                  | None -> Some lhs)
              | Some (Token.Ident _) ->
                  (* Field access: record.field *)
                  let field = consume parser in
                  let trivia = take_pending_trivia parser in
                  let children = [ Ceibo.Green.Node lhs; dot; field ] in
                  let children = trivia @ children in
                  let access =
                    make_node_list ~kind:Syntax_kind.FIELD_ACCESS_EXPR children
                  in
                  loop access
              | _ ->
                  let span =
                    match peek parser with
                    | Some tok -> tok.Token.span
                    | None -> Ceibo.Span.make ~start:0 ~end_:0
                  in
                  report_error parser
                    (Diagnostic.make_missing_token ~expected:"field name" ~span);
                  Some lhs)
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
                    [ Ceibo.Green.Node lhs; op_tok; Ceibo.Green.Node rhs ]
                  in
                  let children = trivia @ children in
                  let infix = make_node_list ~kind:Syntax_kind.INFIX_EXPR children in
                  loop infix
              | None -> Some lhs)
        | (Some Token.Tilde | Some Token.Question) when min_bp <= 8 -> (
            (* Labeled or optional argument *)
            match parse_labeled_or_optional_arg parser with
            | Some arg ->
                let trivia = take_pending_trivia parser in
                let children =
                  [ Ceibo.Green.Node lhs; Ceibo.Green.Node arg ]
                in
                let children = trivia @ children in
                let app = make_node_list ~kind:Syntax_kind.APPLY_EXPR children in
                loop app
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
                    [ Ceibo.Green.Node lhs; Ceibo.Green.Node rhs ]
                  in
                  let children = trivia @ children in
                  let app = make_node_list ~kind:Syntax_kind.APPLY_EXPR children in
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
  | Some (Token.OpenDelim Token.Brace)
  | Some
      (Token.Keyword
         ( Keyword.True | Keyword.False | Keyword.If | Keyword.Match
         | Keyword.Fun | Keyword.Function | Keyword.Lnot | Keyword.Assert
         | Keyword.Lazy | Keyword.For | Keyword.While | Keyword.Begin
         | Keyword.Try ))
  | Some Token.Minus
  | Some Token.Bang
  | Some Token.Backtick ->
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
        let children = prepend_pending_trivia parser [minus; dot] in
        Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
      else
        (* Parse as prefix operator - *)
        let op = consume parser in
        let _ = consume_trivia parser in
        match parse_expr_bp parser 7 with
        | Some operand ->
            let children =
              prepend_pending_trivia parser [ op; Ceibo.Green.Node operand ]
            in
            Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
        | None -> None)
  | Some Token.Bang -> (
      let op = consume parser in
      let _ = consume_trivia parser in
      match parse_expr_bp parser 7 with
      (* Higher precedence for prefix *)
      | Some operand ->
          let children =
            prepend_pending_trivia parser [ op; Ceibo.Green.Node operand ]
          in
          Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
      | None -> None)
  | Some (Token.Keyword Keyword.Lnot) -> (
      let op = consume parser in
      let _ = consume_trivia parser in
      match parse_expr_bp parser 7 with
      | Some operand ->
          let children =
            prepend_pending_trivia parser [ op; Ceibo.Green.Node operand ]
          in
          Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
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
              let children = prepend_pending_trivia parser [ident] in
              Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
          (* Parenthesized expression *)
          | Some (Token.OpenDelim Token.Paren) -> parse_paren_expr parser
          (* List literal *)
          | Some (Token.OpenDelim Token.Bracket) -> parse_list_expr parser
          (* Record literal *)
          | Some (Token.OpenDelim Token.Brace) -> parse_record_expr parser
          (* Let expression *)
          | Some (Token.Keyword Keyword.Let) -> parse_let_expr parser
          (* If expression *)
          | Some (Token.Keyword Keyword.If) -> parse_if_expr parser
          (* Match expression *)
          | Some (Token.Keyword Keyword.Match) -> parse_match_expr parser
          (* Fun/function *)
          | Some (Token.Keyword Keyword.Fun) -> parse_fun_expr parser
          | Some (Token.Keyword Keyword.Function) -> parse_function_expr parser
          (* Assert *)
          | Some (Token.Keyword Keyword.Assert) -> parse_assert_expr parser
          (* Lazy *)
          | Some (Token.Keyword Keyword.Lazy) -> parse_lazy_expr parser
          (* For loop *)
          | Some (Token.Keyword Keyword.For) -> parse_for_expr parser
          (* While loop *)
          | Some (Token.Keyword Keyword.While) -> parse_while_expr parser
          (* Begin/end *)
          | Some (Token.Keyword Keyword.Begin) -> parse_begin_expr parser
          (* Try/catch *)
          | Some (Token.Keyword Keyword.Try) -> parse_try_expr parser
          (* Polymorphic variant *)
          | Some Token.Backtick -> parse_poly_variant_expr parser
          | _ -> None))

and parse_labeled_or_optional_arg parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some Token.Tilde -> (
      (* Labeled argument: ~label or ~label:expr *)
      let tilde = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let label = consume parser in
          let _ = consume_trivia parser in
          if at parser Token.Colon then
            let colon = consume parser in
            let _ = consume_trivia parser in
            match parse_primary parser with
            | Some value ->
                let children =
                  prepend_pending_trivia parser
                    [ tilde; label; colon; Ceibo.Green.Node value ]
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
            | None ->
                let children =
                  prepend_pending_trivia parser [ tilde; label ]
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
          else
            (* Punning: ~label is shorthand for ~label:label *)
            let children = prepend_pending_trivia parser [ tilde; label ] in
            Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
      | _ -> None)
  | Some Token.Question -> (
      (* Optional argument: ?label or ?label:expr *)
      let question = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let label = consume parser in
          let _ = consume_trivia parser in
          if at parser Token.Colon then
            let colon = consume parser in
            let _ = consume_trivia parser in
            match parse_primary parser with
            | Some value ->
                let children =
                  prepend_pending_trivia parser
                    [ question; label; colon; Ceibo.Green.Node value ]
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
            | None ->
                let children =
                  prepend_pending_trivia parser [ question; label ]
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
          else
            (* Punning: ?label is shorthand for ?label:label *)
            let children =
              prepend_pending_trivia parser [ question; label ]
            in
            Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
      | _ -> None)
  | _ -> None

and parse_labeled_or_optional_param parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some Token.Tilde -> (
      (* Labeled parameter: ~label or ~label:pattern or ~(label:pattern) *)
      let tilde = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let label = consume parser in
          let _ = consume_trivia parser in
          if at parser Token.Colon then
            let colon = consume parser in
            let _ = consume_trivia parser in
            match parse_pattern parser with
            | Some pattern ->
                let children =
                  prepend_pending_trivia parser
                    [ tilde; label; colon; Ceibo.Green.Node pattern ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
            | None ->
                let children =
                  prepend_pending_trivia parser [ tilde; label ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          else
            (* Punning: ~label is parameter named label *)
            let children = prepend_pending_trivia parser [ tilde; label ] in
            Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
      | _ -> None)
  | Some Token.Question -> (
      (* Optional parameter: ?label or ?label:pattern or ?(label = default) *)
      let question = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let label = consume parser in
          let _ = consume_trivia parser in
          if at parser Token.Colon then
            let colon = consume parser in
            let _ = consume_trivia parser in
            match parse_pattern parser with
            | Some pattern ->
                let children =
                  prepend_pending_trivia parser
                    [ question; label; colon; Ceibo.Green.Node pattern ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
            | None ->
                let children =
                  prepend_pending_trivia parser [ question; label ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          else
            (* Punning: ?label is optional parameter named label *)
            let children =
              prepend_pending_trivia parser [ question; label ]
            in
            Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
      | Some (Token.OpenDelim Token.Paren) -> (
          (* Parenthesized optional with default: ?(label = default) *)
          let open_paren = consume parser in
          let _ = consume_trivia parser in
          match peek_kind parser with
          | Some (Token.Ident _) ->
              let label = consume parser in
              let _ = consume_trivia parser in
              if at parser Token.Eq then
                let eq = consume parser in
                let _ = consume_trivia parser in
                match parse_expr parser with
                | Some default ->
                    let _ = consume_trivia parser in
                    let close_paren =
                      expect parser (Token.CloseDelim Token.Paren)
                    in
                    let children =
                      prepend_pending_trivia parser
                        [
                          question;
                          open_paren;
                          label;
                          eq;
                          Ceibo.Green.Node default;
                          close_paren;
                        ]
                    in
                    Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
                | None ->
                    let children =
                      prepend_pending_trivia parser
                        [ question; open_paren; label ]
                    in
                    Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
              else
                let children =
                  prepend_pending_trivia parser
                    [ question; open_paren; label ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          | _ -> None)
      | _ -> None)
  | _ -> None

and parse_paren_expr parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  (* Check for unit literal () *)
  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children =
      prepend_pending_trivia parser [ open_paren; close_paren ]
    in
    Some (make_node_list ~kind:Syntax_kind.UNIT_LITERAL children)
  else
    match parse_expr parser with
    | Some expr ->
        let _ = consume_trivia parser in
        (* Check if it's a tuple (has comma), sequence (has semicolon), type annotation (has colon), or just parenthesized expr *)
        if at parser Token.Comma then parse_tuple_rest parser open_paren expr
        else if at parser Token.Semi then
          parse_sequence_rest parser open_paren expr
        else if at parser Token.Colon then (
          (* Type annotation: (expr : type) *)
          let colon = consume parser in
          let _ = consume_trivia parser in
          (* For now, just consume tokens until closing paren as the "type" *)
          (* A proper implementation would parse the type, but we'll keep it simple *)
          let type_tokens = ref [] in
          while
            (not (at parser (Token.CloseDelim Token.Paren)))
            && peek parser <> None
          do
            let tok = consume parser in
            type_tokens := tok :: !type_tokens;
            let _ = consume_trivia parser in
            ()
          done;
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let type_elements = (List.rev !type_tokens) in
          let children =
            List.concat
              [
                [ open_paren; Ceibo.Green.Node expr; colon ];
                type_elements;
                [ close_paren ];
              ]
          in
          let children = prepend_pending_trivia parser children in
          Some (make_node_list ~kind:Syntax_kind.TYPED_EXPR children))
        else
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let children =
            prepend_pending_trivia parser
              [ open_paren; Ceibo.Green.Node expr; close_paren ]
          in
          Some (make_node_list ~kind:Syntax_kind.PAREN_EXPR children)
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
  let rec parse_elements acc =
    if not (at parser Token.Comma) then List.rev acc
    else
      let comma = consume parser in
      let _ = consume_trivia parser in
      let acc = comma :: acc in
      match parse_expr parser with
      | Some expr ->
          let _ = consume_trivia parser in
          parse_elements (Ceibo.Green.Node expr :: acc)
      | None -> List.rev acc
  in

  let elements = parse_elements [ Ceibo.Green.Node first_expr ] in

  let close_paren = expect parser (Token.CloseDelim Token.Paren) in
  let children = (open_paren :: (elements @ [close_paren])) in
  let children = prepend_pending_trivia parser children in
  Some (make_node_list ~kind:Syntax_kind.TUPLE_EXPR children)

and parse_sequence_rest parser open_paren first_expr =
  let rec parse_elements acc =
    if not (at parser Token.Semi) then List.rev acc
    else
      let semi = consume parser in
      let _ = consume_trivia parser in
      let acc = semi :: acc in
      match parse_expr parser with
      | Some expr ->
          let _ = consume_trivia parser in
          parse_elements (Ceibo.Green.Node expr :: acc)
      | None -> List.rev acc
  in

  let elements = parse_elements [ Ceibo.Green.Node first_expr ] in

  let close_paren = expect parser (Token.CloseDelim Token.Paren) in
  let children = (open_paren :: (elements @ [close_paren])) in
  let children = prepend_pending_trivia parser children in
  Some (make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children)

and parse_list_expr parser =
  let open_bracket = consume parser in
  let _ = consume_trivia parser in

  (* Check if this is an array [ ... ] or [] instead of a list *)
  if at parser Token.Pipe || at parser Token.Or then
    parse_array_expr parser open_bracket (* Check for empty list [] *)
  else if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    let children =
      prepend_pending_trivia parser [ open_bracket; close_bracket ]
    in
    Some (make_node_list ~kind:Syntax_kind.LIST_EXPR children)
  else
    match parse_expr parser with
    | Some first_expr ->
        let _ = consume_trivia parser in
        (* List elements are separated by semicolons *)
        let rec parse_elements acc =
          if not (at parser Token.Semi) then List.rev acc
          else
            let semi = consume parser in
            let _ = consume_trivia parser in
            let acc = semi :: acc in
            match parse_expr parser with
            | Some expr ->
                let _ = consume_trivia parser in
                parse_elements (Ceibo.Green.Node expr :: acc)
            | None -> List.rev acc
        in

        let elements = parse_elements [ Ceibo.Green.Node first_expr ] in

        let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
        let children = (open_bracket :: (elements @ [close_bracket])) in
        let children = prepend_pending_trivia parser children in
        Some (make_node_list ~kind:Syntax_kind.LIST_EXPR children)
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

and parse_record_expr parser =
  let open_brace = consume parser in
  let _ = consume_trivia parser in

  (* Check for empty record {} - though this isn't valid OCaml, we'll parse it *)
  if at parser (Token.CloseDelim Token.Brace) then
    let close_brace = consume parser in
    let children =
      prepend_pending_trivia parser [ open_brace; close_brace ]
    in
    Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
  else
    (* Look ahead to determine if this is a record literal or record update *)
    (* Record literal: { field = value; ... } *)
    (* Record update: { expr with field = value; ... } *)

    (* Helper to skip trivia and check next non-trivia token *)
    let rec peek_non_trivia_nth parser n =
      let rec skip_from pos count =
        if pos >= Array.length parser.tokens then None
        else
          let tok = parser.tokens.(pos) in
          match tok.Token.kind with
          | Token.Whitespace | Token.Comment _ | Token.Docstring _ ->
              skip_from (pos + 1) count
          | _ ->
              if count = 0 then Some tok.Token.kind
              else skip_from (pos + 1) (count - 1)
      in
      skip_from parser.position n
    in

    (* Check if first non-trivia token is identifier followed by = (record literal) *)
    let is_record_literal =
      match peek_non_trivia_nth parser 0 with
      | Some (Token.Ident _) -> peek_non_trivia_nth parser 1 = Some Token.Eq
      | _ -> false
    in

    if is_record_literal then
      (* Parse as record literal { field = value; ... } *)
      (* Parse fields *)
      let fields =
        match parse_record_field parser with
        | Some field ->
            let _ = consume_trivia parser in

            (* Parse remaining fields *)
            let rec parse_fields acc =
              if not (at parser Token.Semi) then List.rev acc
              else
                let semi = consume parser in
                let _ = consume_trivia parser in
                let acc = semi :: acc in
                match parse_record_field parser with
                | Some f ->
                    let _ = consume_trivia parser in
                    parse_fields (Ceibo.Green.Node f :: acc)
                | None -> List.rev acc
            in

            parse_fields [ Ceibo.Green.Node field ]
        | None -> []
      in

      let close_brace = expect parser (Token.CloseDelim Token.Brace) in
      let children = (open_brace :: (fields @ [close_brace])) in
      let children = prepend_pending_trivia parser children in
      Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
    else
      (* Parse expression first (for record update) *)
      match parse_expr parser with
      | Some base_expr ->
          let _ = consume_trivia parser in
          if at parser (Token.Keyword Keyword.With) then (
            (* Record update: { expr with field = value; ... } *)
            let with_kw = consume parser in
            let _ = consume_trivia parser in

            (* Parse fields *)
            let fields =
              match parse_record_field parser with
              | Some field ->
                  let _ = consume_trivia parser in

                  (* Parse remaining fields *)
                  let rec parse_update_fields acc =
                    if not (at parser Token.Semi) then List.rev acc
                    else
                      let semi = consume parser in
                      let _ = consume_trivia parser in
                      let acc = semi :: acc in
                      match parse_record_field parser with
                      | Some field ->
                          let _ = consume_trivia parser in
                          parse_update_fields (Ceibo.Green.Node field :: acc)
                      | None -> List.rev acc
                  in

                  parse_update_fields [ Ceibo.Green.Node field ]
              | None -> []
            in

            let close_brace = expect parser (Token.CloseDelim Token.Brace) in
            let children = (open_brace :: Ceibo.Green.Node base_expr :: with_kw :: (fields @ [close_brace])) in
            let children = prepend_pending_trivia parser children in
            Some (make_node_list ~kind:Syntax_kind.RECORD_UPDATE_EXPR children))
          else
            (* Error: expected 'with' in record update *)
            let span =
              match peek parser with
              | Some tok -> tok.Token.span
              | None -> Ceibo.Span.make ~start:0 ~end_:0
            in
            report_error parser
              (Diagnostic.make_missing_token ~expected:"'with'" ~span);
            let close_brace = expect parser (Token.CloseDelim Token.Brace) in
            let children =
              prepend_pending_trivia parser
                [ open_brace; Ceibo.Green.Node base_expr; close_brace ]
            in
            Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
      | None ->
          let close_brace = expect parser (Token.CloseDelim Token.Brace) in
          let children =
            prepend_pending_trivia parser [ open_brace; close_brace ]
          in
          Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)

and parse_record_field parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some (Token.Ident _) ->
      let field_name = consume parser in
      let _ = consume_trivia parser in
      if at parser Token.Eq then
        let eq = consume parser in
        let _ = consume_trivia parser in
        match parse_expr parser with
        | Some value ->
            let children = [ field_name; eq; Ceibo.Green.Node value ] in
            Some (make_node_list ~kind:Syntax_kind.RECORD_FIELD children)
        | None -> None
      else
        (* Punning: { x } is shorthand for { x = x } *)
        let children = [ field_name ] in
        Some (make_node_list ~kind:Syntax_kind.RECORD_FIELD children)
  | _ -> None

and parse_array_expr parser open_bracket =
  (* We've already consumed '[' and we're at '|' or '||' *)
  (* Handle [] - the lexer treats || as Or token *)
  if at parser Token.Or then
    let or_token = consume parser in
    let _ = consume_trivia parser in
    let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
    let children =
      prepend_pending_trivia parser [ open_bracket; or_token; close_bracket ]
    in
    Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)
  else
    (* Normal case: [ ... ] with '|' tokens *)
    let open_pipe = consume parser in
    let _ = consume_trivia parser in

    (* Check for empty array [] - if second | was separate *)
    if at parser Token.Pipe then
      let close_pipe = consume parser in
      let _ = consume_trivia parser in
      let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
      let children =
        prepend_pending_trivia parser
          [ open_bracket; open_pipe; close_pipe; close_bracket ]
      in
      Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)
    else
      match parse_expr parser with
      | Some first_expr ->
          let _ = consume_trivia parser in
          (* Array elements are separated by semicolons *)
          let rec parse_elements acc =
            if not (at parser Token.Semi) then List.rev acc
            else
              let semi = consume parser in
              let _ = consume_trivia parser in
              let acc = semi :: acc in
              (* Allow trailing semicolon *)
              if at parser Token.Pipe then List.rev acc
              else
                match parse_expr parser with
                | Some expr ->
                    let _ = consume_trivia parser in
                    parse_elements (Ceibo.Green.Node expr :: acc)
                | None -> List.rev acc
          in

          let elements = parse_elements [ Ceibo.Green.Node first_expr ] in

          let close_pipe = expect parser Token.Pipe in
          let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
          let children =
            (open_bracket :: open_pipe :: (elements @ [close_pipe; close_bracket]))
          in
          let children = prepend_pending_trivia parser children in
          Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)
      | None ->
          let close_pipe = expect parser Token.Pipe in
          let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
          let children =
            prepend_pending_trivia parser
              [ open_bracket; open_pipe; close_pipe; close_bracket ]
          in
          Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)

and parse_assert_expr parser =
  let assert_kw = consume parser in
  let _ = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let children =
        prepend_pending_trivia parser [ assert_kw; Ceibo.Green.Node expr ]
      in
      Some (make_node_list ~kind:Syntax_kind.ASSERT_EXPR children)
  | None -> None

and parse_lazy_expr parser =
  let lazy_kw = consume parser in
  let _ = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let children =
        prepend_pending_trivia parser [ lazy_kw; Ceibo.Green.Node expr ]
      in
      Some (make_node_list ~kind:Syntax_kind.LAZY_EXPR children)
  | None -> None

and parse_poly_variant_expr parser =
  let backtick = consume parser in
  let _ = consume_trivia parser in

  (* Polymorphic variant tag must be a capitalized identifier *)
  match peek_kind parser with
  | Some (Token.Ident tag)
    when String.length tag > 0 && Char.uppercase_ascii tag.[0] = tag.[0] ->
      let tag_token = consume parser in
      let _ = consume_trivia parser in
      (* Check if there's an argument *)
      if can_start_primary parser then
        match parse_primary parser with
        | Some arg ->
            let children =
              prepend_pending_trivia parser
                [ backtick; tag_token; Ceibo.Green.Node arg ]
            in
            Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
        | None ->
            let children =
              prepend_pending_trivia parser [ backtick; tag_token ]
            in
            Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
      else
        let children =
          prepend_pending_trivia parser [ backtick; tag_token ]
        in
        Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
  | _ ->
      (* Missing or invalid tag *)
      let children = prepend_pending_trivia parser [ backtick ] in
      Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)

and parse_for_expr parser =
  let for_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse: for <ident> = <expr> to/downto <expr> do <expr> done *)
  let ident =
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
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

  let start_expr =
    match parse_expr parser with
    | Some e -> e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        make_error_node parser
          ~kind:(Diagnostic.InvalidSyntax { context = "for loop start" })
          ~span
  in

  let _ = consume_trivia parser in
  let direction =
    match peek_kind parser with
    | Some (Token.Keyword Keyword.To) -> consume parser
    | Some (Token.Keyword Keyword.Downto) -> consume parser
    | _ ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"'to' or 'downto'" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in
  let end_expr =
    match parse_expr parser with
    | Some e -> e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        make_error_node parser
          ~kind:(Diagnostic.InvalidSyntax { context = "for loop end" })
          ~span
  in

  let _ = consume_trivia parser in
  let do_kw = expect parser (Token.Keyword Keyword.Do) in
  let _ = consume_trivia parser in

  let body =
    match parse_expr parser with
    | Some e -> e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        make_error_node parser
          ~kind:(Diagnostic.InvalidSyntax { context = "for loop body" })
          ~span
  in

  let _ = consume_trivia parser in
  let done_kw = expect parser (Token.Keyword Keyword.Done) in

  let children =
    prepend_pending_trivia parser
      [
        for_kw;
        ident;
        eq;
        Ceibo.Green.Node start_expr;
        direction;
        Ceibo.Green.Node end_expr;
        do_kw;
        Ceibo.Green.Node body;
        done_kw;
      ]
  in
  Some (make_node_list ~kind:Syntax_kind.FOR_EXPR children)

and parse_while_expr parser =
  let while_kw = consume parser in
  let _ = consume_trivia parser in

  let cond =
    match parse_expr parser with
    | Some e -> e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        make_error_node parser
          ~kind:(Diagnostic.InvalidSyntax { context = "while condition" })
          ~span
  in

  let _ = consume_trivia parser in
  let do_kw = expect parser (Token.Keyword Keyword.Do) in
  let _ = consume_trivia parser in

  let body =
    match parse_expr parser with
    | Some e -> e
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        make_error_node parser
          ~kind:(Diagnostic.InvalidSyntax { context = "while body" })
          ~span
  in

  let _ = consume_trivia parser in
  let done_kw = expect parser (Token.Keyword Keyword.Done) in

  let children =
    prepend_pending_trivia parser
      [
        while_kw; Ceibo.Green.Node cond; do_kw; Ceibo.Green.Node body; done_kw;
      ]
  in
  Some (make_node_list ~kind:Syntax_kind.WHILE_EXPR children)

and parse_begin_expr parser =
  let begin_kw = consume parser in
  let _ = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let _ = consume_trivia parser in
      let end_kw = expect parser (Token.Keyword Keyword.End) in
      let children =
        prepend_pending_trivia parser
          [ begin_kw; Ceibo.Green.Node expr; end_kw ]
      in
      Some (make_node_list ~kind:Syntax_kind.PAREN_EXPR children)
  | None -> None

and parse_try_expr parser =
  let try_kw = consume parser in
  let _ = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let _ = consume_trivia parser in
      let with_kw = expect parser (Token.Keyword Keyword.With) in
      let _ = consume_trivia parser in

      (* Parse match cases *)
      let first_case =
        if not (at parser Token.Pipe) then
          match parse_match_case_no_pipe parser with
          | Some case ->
              let _ = consume_trivia parser in
              [ Ceibo.Green.Node case ]
          | None -> []
        else []
      in

      (* Remaining cases with | *)
      let rec parse_cases acc =
        if not (at parser Token.Pipe) then List.rev acc
        else
          match parse_match_case parser with
          | Some case -> parse_cases (Ceibo.Green.Node case :: acc)
          | None -> List.rev acc
      in

      let rest_cases = parse_cases first_case in

      let children = try_kw :: rest_cases @ [Ceibo.Green.Node expr; with_kw] in
      let children = prepend_pending_trivia parser children in
      Some (make_node_list ~kind:Syntax_kind.TRY_EXPR children)
  | None -> None

and parse_let_expr parser =
  (* Parse let expression with pattern destructuring support *)
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

  (* Parse pattern - for let expressions, prefer simple identifier *)
  let pattern =
    match peek_kind parser with
    | Some (Token.Ident _) -> (
        (* Could be simple ident or start of tuple/complex pattern *)
        let ident = consume parser in
        let _ = consume_trivia parser in
        
        (* Check if followed by comma (tuple) or other pattern indicators *)
        if at parser Token.Comma then (
          (* Tuple pattern *)
          let rec parse_tuple_patterns acc =
            if not (at parser Token.Comma) then List.rev acc
            else
              let comma = consume parser in
              let _ = consume_trivia parser in
              match parse_pattern parser with
              | Some pat ->
                  let trivia = consume_trivia parser in
                  parse_tuple_patterns (List.rev_append trivia (Ceibo.Green.Node pat :: comma :: acc))
              | None -> List.rev acc
          in
          let patterns = parse_tuple_patterns [ ident ] in
          Ceibo.Green.Node
            (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns))
        else
          (* Simple identifier - keep as token, not wrapped *)
          ident)
    | Some (Token.OpenDelim Token.Paren) -> (
        (* Complex pattern like (a, b) or (Some x) *)
        match parse_pattern parser with
        | Some pat ->
            let _ = consume_trivia parser in
            Ceibo.Green.Node pat
        | None ->
            let span =
              match peek parser with
              | Some tok -> tok.Token.span
              | None -> Ceibo.Span.make ~start:0 ~end_:0
            in
            report_error parser
              (Diagnostic.make_missing_token ~expected:"pattern" ~span);
            Ceibo.Green.Token
              (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0))
    | _ ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"pattern" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let _ = consume_trivia parser in

  (* Check if pattern is a simple identifier (function name) *)
  let is_simple_ident =
    match pattern with
    | Ceibo.Green.Token _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_EXPR
    | _ -> false
  in

  (* Only parse function parameters if:
     1. Pattern was a simple identifier (function name)
     2. Next token is NOT '=' (would indicate simple let binding)
     3. Next token looks like it could start a parameter *)
  let params =
    if is_simple_ident && not (at parser Token.Eq) then
      let rec loop acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          match peek_kind parser with
          | Some Token.Tilde | Some Token.Question -> (
              match parse_labeled_or_optional_param parser with
              | Some param ->
                  let _ = consume_trivia parser in
                  loop (Ceibo.Green.Node param :: acc)
              | None -> List.rev acc)
          | Some (Token.Ident _)
          | Some (Token.OpenDelim Token.Paren)
          | Some Token.Underscore
          | Some (Token.Literal _)
          | Some (Token.OpenDelim Token.Bracket) -> (
              match parse_pattern parser with
              | Some pat ->
                  let _ = consume_trivia parser in
                  loop (Ceibo.Green.Node pat :: acc)
              | None -> List.rev acc)
          | _ -> List.rev acc
      in
      loop []
    else []
  in

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

  (* Check for 'and' bindings *)
  let rec parse_and_bindings acc =
    if not (at parser (Token.Keyword Keyword.And)) then List.rev acc
    else
      let and_kw = consume parser in
      let _ = consume_trivia parser in

      (* Parse pattern *)
      let and_pattern =
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
              (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                 ~width:0)
      in

      let _ = consume_trivia parser in

      (* Check for function parameters before '=' in 'and' binding *)
      let rec parse_and_params acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          match peek_kind parser with
          | Some Token.Tilde | Some Token.Question -> (
              match parse_labeled_or_optional_param parser with
              | Some param ->
                  let _ = consume_trivia parser in
                  parse_and_params (Ceibo.Green.Node param :: acc)
              | None -> List.rev acc)
          | _ -> (
              match parse_pattern parser with
              | Some pat ->
                  let _ = consume_trivia parser in
                  parse_and_params (Ceibo.Green.Node pat :: acc)
              | None -> List.rev acc)
      in

      let and_params = parse_and_params [] in

      let and_eq = expect parser Token.Eq in
      let _ = consume_trivia parser in

      (* Parse value *)
      let and_value =
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
              (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                 ~width:0)
      in

      let _ = consume_trivia parser in

      (* Build and_binding: and_kw, and_pattern, and_params..., and_eq, and_value *)
      let binding_parts =
        and_kw :: and_pattern :: (and_params @ [ and_eq; and_value ])
      in
      parse_and_bindings (List.rev binding_parts @ acc)
  in

  let and_bindings = parse_and_bindings [] in

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
      let children = [let_kw; kw; pattern] @ params @ [eq; value_expr] @ and_bindings @ [in_kw; body_expr] in
      Some (make_node_list ~kind children)
  | None ->
      let children = [let_kw; pattern] @ params @ [eq; value_expr] @ and_bindings @ [in_kw; body_expr] in
      Some (make_node_list ~kind children)

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
      (make_node_list ~kind:Syntax_kind.IF_EXPR
         [ if_kw; cond; then_kw; then_expr; else_kw; else_expr ])
  else
    Some
      (make_node_list ~kind:Syntax_kind.IF_EXPR
         [ if_kw; cond; then_kw; then_expr ])

and parse_fun_expr parser =
  let fun_kw = consume parser in
  let _ = consume_trivia parser in

  let rec parse_params acc =
    if at parser Token.Arrow || peek parser = None then List.rev acc
    else
      match peek_kind parser with
      | Some Token.Tilde | Some Token.Question -> (
          match parse_labeled_or_optional_param parser with
          | Some param ->
              let _ = consume_trivia parser in
              parse_params (Ceibo.Green.Node param :: acc)
          | None -> List.rev acc)
      | _ -> (
          match parse_pattern parser with
          | Some pat ->
              let _ = consume_trivia parser in
              parse_params (Ceibo.Green.Node pat :: acc)
          | None -> List.rev acc)
  in

  let params = parse_params [] in

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

  let children = fun_kw :: (params @ [arrow; body]) in
  let children = prepend_pending_trivia parser children in

  Some (make_node_list ~kind:Syntax_kind.FUN_EXPR children)

and parse_function_expr parser =
  let function_kw = consume parser in
  let _ = consume_trivia parser in

  (* Handle first case - may or may not have leading | *)
  let cases =
    if at parser Token.Pipe then
      (* Has leading | - use normal match case parsing *)
      let rec parse_cases acc =
        if not (at parser Token.Pipe) then List.rev acc
        else
          match parse_match_case parser with
          | Some case -> parse_cases (Ceibo.Green.Node case :: acc)
          | None -> List.rev acc
      in
      parse_cases []
    else
      (* No leading | - parse pattern -> expr directly *)
      match parse_pattern parser with
      | Some first_pattern -> (
          let _ = consume_trivia parser in

          (* Check for tuple pattern (comma) or or-pattern (pipe) *)
          let base_pattern =
            if at parser Token.Comma then
              (* Tuple pattern *)
              let rec parse_tuple_patterns acc =
                if not (at parser Token.Comma) then List.rev acc
                else
                  let comma = consume parser in
                  let _ = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia = consume_trivia parser in
                      parse_tuple_patterns
                        (List.rev_append trivia
                           (Ceibo.Green.Node pat :: comma :: acc))
                  | None -> List.rev acc
              in
              let patterns =
                parse_tuple_patterns [ Ceibo.Green.Node first_pattern ]
              in
              make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns
            else if at parser Token.Pipe && not (at parser Token.Arrow) then
              (* Or pattern *)
              let rec parse_or_patterns acc =
                if (not (at parser Token.Pipe)) || at_any parser [ Token.Arrow ]
                then List.rev acc
                else
                  let pipe_tok = consume parser in
                  let _ = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia = consume_trivia parser in
                      parse_or_patterns
                        (List.rev_append trivia
                           (Ceibo.Green.Node pat :: pipe_tok :: acc))
                  | None -> List.rev acc
              in
              let patterns =
                parse_or_patterns [ Ceibo.Green.Node first_pattern ]
              in
              make_node_list ~kind:Syntax_kind.OR_PATTERN patterns
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
                    (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                       [
                         Ceibo.Green.Node base_pattern;
                         cons_op;
                         Ceibo.Green.Node tail_pat;
                       ])
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
                       (make_node_list ~kind:Syntax_kind.PATTERN_GUARD
                          [ when_kw; Ceibo.Green.Node e ]))
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
              Ceibo.Green.Token
                (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                   ~width:0)
          in
          let _ = consume_trivia parser in
          match parse_expr parser with
          | Some expr ->
              let case_children =
                match guard with
                | Some g -> [ pattern; g; arrow; Ceibo.Green.Node expr ]
                | None -> [ pattern; arrow; Ceibo.Green.Node expr ]
              in
              let first_case =
                make_node_list ~kind:Syntax_kind.MATCH_CASE case_children
              in
              (* Parse remaining cases with | *)
              let rec parse_remaining_cases acc =
                if not (at parser Token.Pipe) then List.rev acc
                else
                  match parse_match_case parser with
                  | Some case ->
                      parse_remaining_cases (Ceibo.Green.Node case :: acc)
                  | None -> List.rev acc
              in
              let rest_cases =
                parse_remaining_cases [ Ceibo.Green.Node first_case ]
              in
              rest_cases
          | None -> []
          | None -> [])
  in

  let children = cases in
  let children = function_kw :: children in
  let children = prepend_pending_trivia parser children in

  Some (make_node_list ~kind:Syntax_kind.FUNCTION_EXPR children)

and parse_pattern parser =
  match parse_base_pattern parser with
  | Some pat ->
      let _ = consume_trivia parser in
      if at parser (Token.Keyword Keyword.As) then
        let as_kw = consume parser in
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let ident = consume parser in
            Some (make_node_list ~kind:Syntax_kind.AS_PATTERN
              [Ceibo.Green.Node pat; as_kw; ident])
        | _ -> Some pat
      else
        Some pat
  | None -> None

and parse_base_pattern parser =
  let _ = consume_trivia parser in

  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let underscore = consume parser in
      Some (make_node_list ~kind:Syntax_kind.WILDCARD_PATTERN [underscore])
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
            (make_node_list ~kind:Syntax_kind.LITERAL_PATTERN
               [ Ceibo.Green.Node lit ])
      | None -> None)
  (* Parenthesized pattern or tuple *)
  | Some (Token.OpenDelim Token.Paren) -> parse_paren_pattern parser
  (* Record pattern *)
  | Some (Token.OpenDelim Token.Brace) -> parse_record_pattern parser
  (* Polymorphic variant pattern *)
  | Some Token.Backtick -> parse_poly_variant_pattern parser
  | _ -> None

and parse_list_pattern parser =
  let open_bracket = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    let children = [open_bracket] @ comments1 @ [close_bracket] in
    Some (make_node_list ~kind:Syntax_kind.LIST_PATTERN children)
  else
    let first_pat =
      match parse_pattern parser with
      | Some pat -> [ Ceibo.Green.Node pat ]
      | None -> []
    in

    let comments2 = consume_trivia parser in

    let rec parse_patterns acc =
      if not (at parser Token.Semi) then List.rev acc
      else
        let semi = consume parser in
        let comments3 = consume_trivia parser in
        let acc = List.rev_append comments3 (semi :: acc) in
        match parse_pattern parser with
        | Some pat ->
            let comments4 = consume_trivia parser in
            parse_patterns
              (List.rev_append comments4 (Ceibo.Green.Node pat :: acc))
        | None -> List.rev acc
    in

    let patterns =
      parse_patterns
        (List.rev_append comments2 (first_pat @ List.rev comments1))
    in

    let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
    let children = (open_bracket :: (patterns @ [close_bracket])) in
    Some (make_node_list ~kind:Syntax_kind.LIST_PATTERN children)

and parse_ident_or_constructor_pattern parser =
  let ident = consume parser in
  let _ = consume_trivia parser in

  (* Check if this identifier is a constructor (starts with uppercase) *)
  let is_constructor =
    match Ceibo.Green.text ident with
    | Some text when Ceibo.Green.kind ident = Syntax_kind.IDENT_EXPR ->
        is_constructor_ident text
    | _ -> false
  in

  if at parser Token.ColonColon then
    let cons_op = consume parser in
    let _ = consume_trivia parser in

    match parse_pattern parser with
    | Some tail_pat ->
        Some
          (make_node_list ~kind:Syntax_kind.CONS_PATTERN
             [ ident; cons_op; Ceibo.Green.Node tail_pat ])
    | None -> Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN [ident])
  else if is_constructor then
    (* Only try to parse as constructor pattern if identifier is uppercase *)
    match peek_kind parser with
    | Some (Token.Ident _)
    | Some (Token.OpenDelim Token.Paren)
    | Some Token.Underscore
    | Some (Token.Literal _)
    | Some (Token.OpenDelim Token.Bracket) -> (
        match parse_pattern parser with
        | Some arg_pat ->
            Some
              (make_node_list ~kind:Syntax_kind.CONSTRUCTOR_PATTERN
                 [ ident; Ceibo.Green.Node arg_pat ])
        | None -> Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN [ident]))
    | _ -> Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN [ident])
  else
    (* Lowercase identifier - always treat as simple ident pattern *)
    Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN [ident])

and parse_paren_pattern parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children =
      prepend_pending_trivia parser [ open_paren; close_paren ]
    in
    Some (make_node_list ~kind:Syntax_kind.PAREN_PATTERN children)
  else if at parser (Token.Keyword Keyword.Lazy) then
    (* Lazy pattern: (lazy pat) *)
    let lazy_kw = consume parser in
    let _ = consume_trivia parser in
    match parse_pattern parser with
    | Some pat ->
        let _ = consume_trivia parser in
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in
        let children =
          prepend_pending_trivia parser
            [ open_paren; lazy_kw; Ceibo.Green.Node pat; close_paren ]
        in
        Some (make_node_list ~kind:Syntax_kind.LAZY_PATTERN children)
    | None ->
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in
        let children =
          prepend_pending_trivia parser [ open_paren; lazy_kw; close_paren ]
        in
        Some (make_node_list ~kind:Syntax_kind.LAZY_PATTERN children)
  else
    match parse_pattern parser with
    | Some first_pat ->
        let _ = consume_trivia parser in

        if at parser Token.Comma then
          let rec parse_tuple_elements acc =
            if not (at parser Token.Comma) then List.rev acc
            else
              let comma = consume parser in
              let _ = consume_trivia parser in
              match parse_pattern parser with
              | Some pat ->
                  let trivia = consume_trivia parser in
                  parse_tuple_elements
                    (List.rev_append trivia
                       (Ceibo.Green.Node pat :: comma :: acc))
              | None -> List.rev acc
          in

          let elements = parse_tuple_elements [ Ceibo.Green.Node first_pat ] in

          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let children = (open_paren :: (elements @ [close_paren])) in
          Some (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN children)
        else
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          Some
            (make_node_list ~kind:Syntax_kind.PAREN_PATTERN
               [ open_paren; Ceibo.Green.Node first_pat; close_paren ])
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

  (* Parse match cases *)
  let first_case =
    if not (at parser Token.Pipe) then
      match parse_match_case_no_pipe parser with
      | Some case ->
          let _ = consume_trivia parser in
          [ Ceibo.Green.Node case ]
      | None -> []
    else []
  in

  (* Remaining cases with | *)
  let rec parse_cases acc =
    if not (at parser Token.Pipe) then List.rev acc
    else
      match parse_match_case parser with
      | Some case -> parse_cases (Ceibo.Green.Node case :: acc)
      | None -> List.rev acc
  in

  let all_cases = parse_cases first_case in

  let children = [match_kw; scrutinee; with_kw] @ all_cases in

  Some (make_node_list ~kind:Syntax_kind.MATCH_EXPR children)

and parse_match_case_no_pipe parser =
  (* Parse a match case without expecting a leading | *)
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
        make_node_list ~kind:Syntax_kind.MISSING []
  in
  parse_match_case_body parser first_pattern

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
        make_node_list ~kind:Syntax_kind.MISSING []
  in

  match parse_match_case_body parser first_pattern with
  | Some case ->
      (* Prepend the pipe token to the match case *)
      let case_children = Ceibo.Green.children case in
      let children_with_pipe = pipe :: Array.to_list case_children in
      Some (make_node_list ~kind:Syntax_kind.MATCH_CASE children_with_pipe)
  | None -> None

and parse_poly_variant_pattern parser =
  let backtick = consume parser in
  let _ = consume_trivia parser in

  (* Polymorphic variant tag must be a capitalized identifier *)
  match peek_kind parser with
  | Some (Token.Ident tag)
    when String.length tag > 0 && Char.uppercase_ascii tag.[0] = tag.[0] -> (
      let tag_token = consume parser in
      let _ = consume_trivia parser in
      (* Check if there's a pattern argument *)
      match peek_kind parser with
      | Some Token.Underscore
      | Some (Token.Ident _)
      | Some (Token.Literal _)
      | Some (Token.OpenDelim Token.Paren)
      | Some (Token.OpenDelim Token.Bracket)
      | Some Token.Backtick -> (
          match parse_pattern parser with
          | Some pat ->
              let children =
                prepend_pending_trivia parser
                  [ backtick; tag_token; Ceibo.Green.Node pat ]
              in
              Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
          | None ->
              let children =
                prepend_pending_trivia parser [ backtick; tag_token ]
              in
              Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children))
      | _ ->
          let children =
            prepend_pending_trivia parser [ backtick; tag_token ]
          in
          Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
      | _ ->
          (* Missing or invalid tag *)
          let children = prepend_pending_trivia parser [ backtick ] in
          Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children))

and parse_record_pattern parser =
  let open_brace = consume parser in
  let _ = consume_trivia parser in

  (* Parse field patterns *)
  let fields = ref [] in

  let rec loop () =
    if at parser (Token.CloseDelim Token.Brace) then ()
    else
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let field_name = consume parser in
          let _ = consume_trivia parser in

          (* Check if there's a '=' for field = pattern or just field (punning) *)
          if at parser Token.Eq then
            let eq = consume parser in
            let _ = consume_trivia parser in
            match parse_pattern parser with
            | Some pat ->
                fields := Ceibo.Green.Node pat :: eq :: field_name :: !fields;
                let _ = consume_trivia parser in
                if at parser Token.Semi then (
                  let semi = consume parser in
                  fields := semi :: !fields;
                  let _ = consume_trivia parser in
                  loop ())
            | None -> ()
          else (
            (* Punning: { x } is shorthand for { x = x } *)
            fields := field_name :: !fields;
            let _ = consume_trivia parser in
            if at parser Token.Semi then (
              let semi = consume parser in
              fields := semi :: !fields;
              let _ = consume_trivia parser in
              loop ()))
      | _ -> ()
  in
  loop ();

  let close_brace = expect parser (Token.CloseDelim Token.Brace) in
  let children = open_brace :: List.rev (close_brace :: !fields) in
  let children = prepend_pending_trivia parser children in
  Some (make_node_list ~kind:Syntax_kind.RECORD_PATTERN children)

and parse_match_case_body parser first_pattern =
  let _ = consume_trivia parser in

  let pattern =
    if at parser Token.Comma then
      (* Tuple pattern *)
      let rec parse_tuple_patterns acc =
        if not (at parser Token.Comma) then List.rev acc
        else
          let comma = consume parser in
          let _ = consume_trivia parser in
          match parse_pattern parser with
          | Some pat ->
              let trivia = consume_trivia parser in
              parse_tuple_patterns
                (List.rev_append trivia (Ceibo.Green.Node pat :: comma :: acc))
          | None -> List.rev acc
      in
      let patterns = parse_tuple_patterns [ Ceibo.Green.Node first_pattern ] in
      Ceibo.Green.Node
        (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
    else if at parser Token.Pipe then
      (* Or pattern *)
      let rec parse_or_patterns acc =
        if (not (at parser Token.Pipe)) || at_any parser [ Token.Arrow ] then
          List.rev acc
        else
          let pipe_tok = consume parser in
          let _ = consume_trivia parser in
          match parse_pattern parser with
          | Some pat ->
              let trivia = consume_trivia parser in
              parse_or_patterns
                (List.rev_append trivia
                   (Ceibo.Green.Node pat :: pipe_tok :: acc))
          | None -> List.rev acc
      in
      let patterns = parse_or_patterns [ Ceibo.Green.Node first_pattern ] in
      Ceibo.Green.Node
        (make_node_list ~kind:Syntax_kind.OR_PATTERN patterns)
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
                (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                   [
                     Ceibo.Green.Node base_pattern;
                     cons_op;
                     Ceibo.Green.Node tail_pat;
                   ])
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
               (make_node_list ~kind:Syntax_kind.PATTERN_GUARD
                  [ when_kw; Ceibo.Green.Node e ]))
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
    | Some g -> [ pattern; g; arrow; expr ]
    | None -> [ pattern; arrow; expr ]
  in

  Some (make_node_list ~kind:Syntax_kind.MATCH_CASE children)

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

  (* Parse pattern - for top-level bindings, prefer simple identifier *)
  let pattern =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        (* Could be simple ident or start of tuple/complex pattern *)
        let ident = consume parser in
        let _ = consume_trivia parser in

        (* Check if followed by comma (tuple) or other pattern indicators *)
        if at parser Token.Comma then
          (* Tuple pattern *)
          let rec parse_tuple_patterns acc =
            if not (at parser Token.Comma) then List.rev acc
            else
              let comma = consume parser in
              let _ = consume_trivia parser in
              match parse_pattern parser with
              | Some pat ->
                  let trivia = consume_trivia parser in
                  parse_tuple_patterns
                    (List.rev_append trivia
                       (Ceibo.Green.Node pat :: comma :: acc))
              | None -> List.rev acc
          in
          let patterns = parse_tuple_patterns [ ident ] in
          Ceibo.Green.Node
            (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
        else
          (* Simple identifier - keep as token, not wrapped *)
          ident
    | Some (Token.OpenDelim Token.Paren)
    | Some (Token.OpenDelim Token.Brace)      (* Record patterns: { x; y } *)
    | Some (Token.OpenDelim Token.Bracket)    (* List patterns: [x; y] *)
    | Some Token.Underscore                   (* Wildcard: _ *)
    | Some (Token.Literal _)                  (* Literal patterns *)
    | Some (Token.Keyword Keyword.Lazy)       (* Lazy patterns *)
    | Some Token.Backtick -> (                (* Polymorphic variant patterns *)
        (* Use parse_pattern for all complex pattern forms *)
        match parse_pattern parser with
        | Some pat ->
            let _ = consume_trivia parser in
            Ceibo.Green.Node pat
        | None ->
            let span =
              match peek parser with
              | Some tok -> tok.Token.span
              | None -> Ceibo.Span.make ~start:0 ~end_:0
            in
            let err = Diagnostic.make_missing_token ~expected:"pattern" ~span in
            report_error parser err;
            Ceibo.Green.Token
              (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                 ~width:0))
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
        (make_node_list ~kind:Syntax_kind.LET_BINDING
           [ let_kw; kw; pattern; eq; expr ])
  | None ->
      Some
        (make_node_list ~kind:Syntax_kind.LET_BINDING
           [ let_kw; pattern; eq; expr ])

and parse_open parser =
  let open_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse module path *)
  let path = consume parser in

  Some (make_node_list ~kind:Syntax_kind.OPEN_STMT [open_kw; path])

let parse_source_file parser =
  let rec parse_items acc =
    if peek parser = None || at parser Token.EOF then List.rev acc
    else
      match parse_structure_item parser with
      | Some item -> parse_items (Ceibo.Green.Node item :: acc)
      | None ->
          (* Skip problematic token *)
          let _ = advance parser in
          parse_items acc
  in

  let items = parse_items [] in

  make_node_list ~kind:Syntax_kind.SOURCE_FILE items

(* ========================================================================= *)
(* PUBLIC API *)
(* ========================================================================= *)

let parse ~source tokens =
  let parser = create ~source tokens in
  let green_tree = parse_source_file parser in
  { tree = green_tree; diagnostics = List.rev parser.diagnostics }
