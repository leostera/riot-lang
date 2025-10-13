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

(* Check if a token kind represents an operator *)
let is_operator_token = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.PlusDot | Token.MinusDot | Token.StarDot | Token.SlashDot | Token.Eq
  | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.And
  | Token.Or | Token.ColonColon | Token.Caret | Token.At | Token.ColonEq
  | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq | Token.AtAt
  | Token.PipeGt | Token.PercentGt | Token.LtPercent | Token.Bang | Token.Tilde
  | Token.Question | Token.Pipe | Token.Arrow | Token.Dot | Token.Semi
  | Token.Comma | Token.Colon | Token.Hash
  | Token.Keyword
      ( Keyword.Mod | Keyword.Land | Keyword.Lor | Keyword.Lxor | Keyword.Lsl
      | Keyword.Lsr | Keyword.Asr | Keyword.Lnot ) ->
      true
  | _ -> false

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
  | Token.Keyword Keyword.Type -> Syntax_kind.TYPE_DECL
  | Token.Keyword Keyword.Val -> Syntax_kind.VAL_DECL
  | Token.Keyword Keyword.External -> Syntax_kind.EXTERNAL_DECL
  | Token.Keyword Keyword.Module -> Syntax_kind.MODULE_DECL
  | Token.Keyword Keyword.Include -> Syntax_kind.INCLUDE_STMT
  | Token.Keyword Keyword.Open -> Syntax_kind.OPEN_STMT
  | Token.Keyword Keyword.Rec -> Syntax_kind.LET_REC_EXPR
  | Token.Keyword Keyword.In -> Syntax_kind.LET_EXPR
  | Token.Keyword Keyword.And ->
      Syntax_kind.LET_EXPR (* 'and' in let bindings *)
  | Token.Quote -> Syntax_kind.TYPE_VAR
  | Token.Keyword Keyword.If -> Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Then | Token.Keyword Keyword.Else ->
      Syntax_kind.IF_EXPR
  | Token.Keyword Keyword.Fun -> Syntax_kind.FUN_EXPR
  | Token.Keyword Keyword.Function -> Syntax_kind.FUNCTION_EXPR
  | Token.Keyword Keyword.Match -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.Try -> Syntax_kind.TRY_EXPR
  | Token.Keyword Keyword.With -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.When -> Syntax_kind.MATCH_EXPR
  | Token.Keyword Keyword.Of -> Syntax_kind.TYPE_VARIANT_CONSTR
  | Token.Arrow -> Syntax_kind.FUN_EXPR (* -> is part of fun/function syntax *)
  | Token.Pipe ->
      Syntax_kind.MATCH_EXPR (* | is part of match/function syntax *)
  | Token.Semi -> Syntax_kind.SEQUENCE_EXPR
  | Token.Comma -> Syntax_kind.TUPLE_EXPR
  | Token.Colon -> Syntax_kind.TYPED_EXPR
  | Token.Dot ->
      Syntax_kind.IDENT_EXPR (* . can be part of operator identifiers like -. *)
  | Token.DotDot -> Syntax_kind.RANGE_PATTERN (* .. for range patterns *)
  | Token.Underscore -> Syntax_kind.WILDCARD_PATTERN
  | Token.Ident _ -> Syntax_kind.IDENT_EXPR
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.PlusDot | Token.MinusDot | Token.StarDot | Token.SlashDot | Token.Eq
  | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.And
  | Token.Or | Token.ColonColon | Token.Caret | Token.At | Token.ColonEq
  | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq | Token.AtAt
  | Token.PipeGt | Token.PercentGt | Token.LtPercent
  | Token.Keyword
      ( Keyword.Mod | Keyword.Land | Keyword.Lor | Keyword.Lxor | Keyword.Lsl
      | Keyword.Lsr | Keyword.Asr ) ->
      Syntax_kind.INFIX_EXPR
  | Token.Bang | Token.Keyword Keyword.Lnot -> Syntax_kind.PREFIX_EXPR
  | Token.Tilde | Token.Question -> Syntax_kind.ARGUMENT
  | Token.Backtick -> Syntax_kind.POLY_VARIANT_EXPR
  | Token.Hash -> Syntax_kind.POLY_VARIANT_TYPE_PATTERN
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
  | Token.OpenDelim Token.StructEnd | Token.CloseDelim Token.StructEnd ->
      Syntax_kind.STRUCTURE
  | Token.OpenDelim Token.SigEnd | Token.CloseDelim Token.SigEnd ->
      Syntax_kind.SIGNATURE
  | Token.OpenDelim Token.Paren | Token.CloseDelim Token.Paren ->
      Syntax_kind.PAREN_EXPR
  | Token.OpenDelim Token.Brace | Token.CloseDelim Token.Brace ->
      Syntax_kind.RECORD_EXPR
  | Token.OpenDelim Token.Bracket | Token.CloseDelim Token.Bracket ->
      Syntax_kind.LIST_EXPR
  | Token.OpenDelim Token.Array | Token.CloseDelim Token.Array ->
      Syntax_kind.ARRAY_EXPR
  | Token.OpenDelim Token.BeginEnd | Token.CloseDelim Token.BeginEnd ->
      Syntax_kind.PAREN_EXPR
  | Token.OpenDelim _ | Token.CloseDelim _ -> Syntax_kind.ERROR
  | Token.Keyword _ ->
      Syntax_kind.WHITESPACE (* Other keywords default to WHITESPACE *)
  | _ -> Syntax_kind.ERROR (* TODO: Map remaining token kinds *)

let token_to_green_token parser tok =
  let text =
    String.sub parser.source tok.Token.span.start
      (tok.Token.span.end_ - tok.Token.span.start)
  in
  let width = tok.Token.span.end_ - tok.Token.span.start in
  let kind = token_kind_to_syntax_kind tok.Token.kind in
  Ceibo.Green.make_token ~kind ~text ~width

let consume_trivia parser =
  let rec loop acc =
    match peek parser with
    | Some Token.{ kind = Whitespace | Comment _ | Docstring _ } ->
        let tok =
          match advance parser with
          | Some tok -> tok
          | None -> failwith "Unexpected end of tokens"
        in
        let green_tok = token_to_green_token parser tok in
        let trivia = Ceibo.Green.Token green_tok in
        loop (trivia :: acc)
    | _ -> acc
  in
  let trivia = loop [] in
  List.rev trivia

let consume parser =
  let do_consume () =
    match advance parser with
    | Some tok ->
        let green_tok = token_to_green_token parser tok in
        Ceibo.Green.Token green_tok
    | None ->
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.ERROR ~text:"" ~width:0)
  in
  let before_trivia = consume_trivia parser in
  let token = do_consume () in
  (before_trivia, token)

let make_node_list ~kind children = Ceibo.Green.make_node_list ~kind children

(* ========================================================================= *)
(* TRIVIA HANDLING *)
(* ========================================================================= *)

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
      let missing_token =
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
      in
      ([], missing_token)

let expect_with_trivia parser kind =
  let before_trivia, token = expect parser kind in
  let after_trivia = consume_trivia parser in
  (before_trivia, token, after_trivia)

(* ========================================================================= *)
(* PARSER COMBINATORS *)
(* ========================================================================= *)

(** Parse a list of elements separated by a specific token. Returns a list of:
    [element; separator; element; separator; element]

    Example: parse_separated_by Token.Comma parse_expr parser Parses: expr,
    expr, expr Returns:
    [Ceibo.Green.Node expr; comma; Ceibo.Green.Node expr; comma;
     Ceibo.Green.Node expr] *)
let parse_separated_by separator parse_element parser =
  let rec loop acc =
    if not (at parser separator) then List.rev acc
    else
      let before_trivia, sep_token = consume parser in
      let trivia_after_sep = consume_trivia parser in
      match parse_element parser with
      | Some elem ->
          let trivia = consume_trivia parser in
          loop
            (List.rev_append trivia
               (Ceibo.Green.Node elem
               :: List.rev_append trivia_after_sep
                    (List.rev_append before_trivia (sep_token :: acc))))
      | None ->
          List.rev
            (List.rev_append trivia_after_sep
               (List.rev_append before_trivia (sep_token :: acc)))
  in
  loop []

(** Parse a list of elements separated by a separator, including the first
    element.

    Example: parse_list_of Token.Semi parse_expr parser first_expr Parses:
    first_expr; expr; expr Returns:
    [Ceibo.Green.Node first_expr; semi; Ceibo.Green.Node expr; semi;
     Ceibo.Green.Node expr] *)
let parse_list_of separator parse_element parser first_elem =
  let rest = parse_separated_by separator parse_element parser in
  Ceibo.Green.Node first_elem :: rest

(** Parse zero or more elements separated by a token, collecting them.

    Example: parse_zero_or_more Token.Comma parse_pattern parser Returns: list
    of all parsed elements (not including separators) *)
let parse_zero_or_more separator parse_element parser =
  let rec loop acc =
    match parse_element parser with
    | None -> List.rev acc
    | Some elem ->
        let trivia_after_elem = consume_trivia parser in
        let acc = elem :: acc in
        if at parser separator then
          let before_trivia, sep = consume parser in
          let trivia_after_sep = consume_trivia parser in
          loop
            (List.rev_append trivia_after_sep
               (sep
               :: List.rev_append before_trivia
                    (List.rev_append trivia_after_elem acc)))
        else List.rev (List.rev_append trivia_after_elem acc)
  in
  loop []

(* ========================================================================= *)
(* IDENTIFIER PARSING *)
(* ========================================================================= *)

let parse_identifier parser =
  (* Parse a simple identifier or qualified identifier (module path):
     - Simple: name
     - Qualified: Module.Name, A.B.C.name
     Returns a list of tokens including trivia: [name] or [Module; trivia; .; trivia; Name] etc.
  *)
  let before_trivia, first = consume parser in
  let trivia_after_first = consume_trivia parser in

  let rec parse_rest acc =
    if at parser Token.Dot then
      let before_trivia, dot = consume parser in
      let trivia_after_dot = consume_trivia parser in
      let before_trivia, name = consume parser in
      let trivia_after_name = consume_trivia parser in
      parse_rest
        (trivia_after_name @ [ name ] @ trivia_after_dot @ [ dot ] @ acc)
    else List.rev acc
  in

  let rest = parse_rest [] in
  first :: (trivia_after_first @ rest)

(* ========================================================================= *)
(* LITERALS *)
(* ========================================================================= *)

let parse_literal parser leading_trivia =
  match peek_kind parser with
  | Some (Token.Literal (Token.Int _)) ->
      let before_trivia, tok = consume parser in
      let children = leading_trivia @ [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.INT_LITERAL children)
  | Some (Token.Literal (Token.Float _)) ->
      let before_trivia, tok = consume parser in
      let children = leading_trivia @ [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.FLOAT_LITERAL children)
  | Some (Token.Literal (Token.String _)) ->
      let before_trivia, tok = consume parser in
      let children = leading_trivia @ [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.STRING_LITERAL children)
  | Some (Token.Literal (Token.Char _)) ->
      let before_trivia, tok = consume parser in
      let children = leading_trivia @ [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.CHAR_LITERAL children)
  | Some (Token.Keyword Keyword.True) | Some (Token.Keyword Keyword.False) ->
      let before_trivia, tok = consume parser in
      let children = leading_trivia @ [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.BOOL_LITERAL children)
  | _ -> None

(* ========================================================================= *)
(* EXPRESSIONS *)
(* ========================================================================= *)

let is_constructor_ident name =
  match String.get name 0 with 'A' .. 'Z' -> true | _ -> false

let is_infix_op = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.PlusDot | Token.MinusDot | Token.StarDot | Token.SlashDot | Token.Eq
  | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq | Token.And
  | Token.Or | Token.ColonColon | Token.Caret | Token.At | Token.ColonEq
  | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq | Token.AtAt
  | Token.PipeGt | Token.PercentGt | Token.LtPercent
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
  | Token.Caret | Token.At | Token.Plus | Token.Minus | Token.PlusDot
  | Token.MinusDot ->
      5
  | Token.Star | Token.Slash | Token.Percent | Token.StarDot | Token.SlashDot
  | Token.Keyword Keyword.Mod ->
      6
  | Token.StarStar -> 7
  | Token.Keyword (Keyword.Land | Keyword.Lor | Keyword.Lxor) -> 3
  | Token.Keyword (Keyword.Lsl | Keyword.Lsr | Keyword.Asr) -> 6
  | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent -> 1
  | _ -> 0

let rec parse_expr parser = parse_expr_bp parser 0

and parse_expr_bp parser min_bp =
  let leading_trivia = consume_trivia parser in

  match parse_primary parser leading_trivia with
  | None -> None
  | Some lhs ->
      let rec loop lhs =
        let trivia_before_op = consume_trivia parser in
        match peek_kind parser with
        | Some Token.Dot -> (
            (* Field/array/string access - highest precedence (9) *)
            let access_prec = 9 in
            if access_prec < min_bp then Some lhs
            else
              let before_trivia, dot = consume parser in
              let trivia_after_dot = consume_trivia parser in
              match peek_kind parser with
              | Some (Token.OpenDelim Token.Paren) -> (
                  (* Could be array indexing arr.(i) OR local open Module.(expr) *)
                  (* Check if lhs is a module identifier (capitalized) *)
                  let is_module_open =
                    if
                      Ceibo.Green.kind (Ceibo.Green.Node lhs)
                      = Syntax_kind.IDENT_EXPR
                    then
                      let children = Ceibo.Green.children lhs in
                      if Array.length children > 0 then
                        match Ceibo.Green.text children.(0) with
                        | Some text ->
                            String.length text > 0
                            && Char.uppercase_ascii text.[0] = text.[0]
                        | None -> false
                      else false
                    else false
                  in
                  if is_module_open then
                    (* Local open: Module.(expr) *)
                    let before_trivia, open_paren = consume parser in
                    let trivia_after_open = consume_trivia parser in
                    match parse_expr parser with
                    | Some expr ->
                        let trivia_before_close, close_paren, trivia_after_close
                            =
                          expect_with_trivia parser
                            (Token.CloseDelim Token.Paren)
                        in
                        let children =
                          [ Ceibo.Green.Node lhs; dot; open_paren ]
                          @ trivia_after_open @ [ Ceibo.Green.Node expr ]
                          @ trivia_before_close @ [ close_paren ]
                          @ trivia_after_close
                        in
                        let local_open =
                          make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                        in
                        loop local_open
                    | None -> Some lhs
                  else
                    (* Array indexing: arr.(i) *)
                    let before_trivia, open_paren = consume parser in
                    let trivia_after_open = consume_trivia parser in
                    match parse_expr parser with
                    | Some index ->
                        let trivia_before_close, close_paren, trivia_after_close
                            =
                          expect_with_trivia parser
                            (Token.CloseDelim Token.Paren)
                        in
                        let children =
                          [ Ceibo.Green.Node lhs; dot; open_paren ]
                          @ trivia_after_open @ [ Ceibo.Green.Node index ]
                          @ trivia_before_close @ [ close_paren ]
                          @ trivia_after_close
                        in
                        let access =
                          make_node_list ~kind:Syntax_kind.ARRAY_INDEX_EXPR
                            children
                        in
                        loop access
                    | None -> Some lhs)
              | Some (Token.OpenDelim Token.Bracket) -> (
                  (* Could be string indexing s.[i] OR local open Module.[...] *)
                  let is_module_open =
                    if
                      Ceibo.Green.kind (Ceibo.Green.Node lhs)
                      = Syntax_kind.IDENT_EXPR
                    then
                      let children = Ceibo.Green.children lhs in
                      if Array.length children > 0 then
                        match Ceibo.Green.text children.(0) with
                        | Some text ->
                            String.length text > 0
                            && Char.uppercase_ascii text.[0] = text.[0]
                        | None -> false
                      else false
                    else false
                  in
                  if is_module_open then
                    (* Local open: Module.[...] *)
                    match parse_list_expr parser [] with
                    | Some list_expr ->
                        let children =
                          [ Ceibo.Green.Node lhs; dot ]
                          @ trivia_after_dot
                          @ [ Ceibo.Green.Node list_expr ]
                        in
                        let local_open =
                          make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                        in
                        loop local_open
                    | None -> Some lhs
                  else
                    (* String indexing: s.[i] *)
                    let before_trivia, open_bracket = consume parser in
                    let trivia_after_open_bracket = consume_trivia parser in
                    match parse_expr parser with
                    | Some index ->
                        let ( trivia_before_close_bracket,
                              close_bracket,
                              trivia_after_close_bracket ) =
                          expect_with_trivia parser
                            (Token.CloseDelim Token.Bracket)
                        in
                        let children =
                          [ Ceibo.Green.Node lhs; dot ]
                          @ trivia_after_dot @ [ open_bracket ]
                          @ trivia_after_open_bracket
                          @ [ Ceibo.Green.Node index ]
                          @ trivia_before_close_bracket @ [ close_bracket ]
                          @ trivia_after_close_bracket
                        in
                        let access =
                          make_node_list ~kind:Syntax_kind.STRING_INDEX_EXPR
                            children
                        in
                        loop access
                    | None -> Some lhs)
              | Some (Token.OpenDelim Token.Array) ->
                  (* Could be local open Module.[|...|] *)
                  let is_module_open =
                    if
                      Ceibo.Green.kind (Ceibo.Green.Node lhs)
                      = Syntax_kind.IDENT_EXPR
                    then
                      let children = Ceibo.Green.children lhs in
                      if Array.length children > 0 then
                        match Ceibo.Green.text children.(0) with
                        | Some text ->
                            String.length text > 0
                            && Char.uppercase_ascii text.[0] = text.[0]
                        | None -> false
                      else false
                    else false
                  in
                  if is_module_open then
                    (* Local open: Module.[|...|] *)
                    match parse_array_expr parser with
                    | Some array_expr ->
                        let children =
                          [ Ceibo.Green.Node lhs; dot ]
                          @ trivia_after_dot
                          @ [ Ceibo.Green.Node array_expr ]
                        in
                        let local_open =
                          make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                        in
                        loop local_open
                    | None -> Some lhs
                  else
                    (* Not a local open - unexpected token after dot *)
                    let span =
                      match peek parser with
                      | Some tok -> tok.Token.span
                      | None -> Ceibo.Span.make ~start:0 ~end_:0
                    in
                    report_error parser
                      (Diagnostic.make_missing_token ~expected:"field name"
                         ~span);
                    Some lhs
              | Some (Token.OpenDelim Token.Brace) -> (
                  (* Module-prefixed record: Module.{ field = value } *)
                  match parse_record_expr parser with
                  | Some record_expr ->
                      let children =
                        [ Ceibo.Green.Node lhs; dot ]
                        @ trivia_after_dot
                        @ [ Ceibo.Green.Node record_expr ]
                      in
                      let prefixed_record =
                        make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                      in
                      loop prefixed_record
                  | None -> Some lhs)
              | Some (Token.Ident _) ->
                  (* Field access: record.field *)
                  let before_trivia, field = consume parser in
                  let children =
                    [ Ceibo.Green.Node lhs ] @ trivia_before_op @ [ dot ]
                    @ trivia_after_dot @ before_trivia @ [ field ]
                  in
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
        | Some Token.Hash -> (
            (* Method call: obj#method *)
            let hash_prec = 9 in
            (* Same as field access *)
            if hash_prec < min_bp then Some lhs
            else
              let before_trivia, hash = consume parser in
              let trivia_after_hash = consume_trivia parser in
              match peek_kind parser with
              | Some (Token.Ident _) ->
                  let before_trivia, method_name = consume parser in
                  let children =
                    [ Ceibo.Green.Node lhs; hash ]
                    @ trivia_after_hash @ [ method_name ]
                  in
                  let method_call =
                    make_node_list ~kind:Syntax_kind.METHOD_CALL_EXPR children
                  in
                  loop method_call
              | _ ->
                  let span =
                    match peek parser with
                    | Some tok -> tok.Token.span
                    | None -> Ceibo.Span.make ~start:0 ~end_:0
                  in
                  report_error parser
                    (Diagnostic.make_missing_token ~expected:"method name" ~span);
                  Some lhs)
        | Some Token.Semi -> (
            (* Sequence expression: e1; e2 - right-associative with lowest precedence *)
            let prec = 0 in
            if prec < min_bp then Some lhs
            else
              let before_trivia, semi = consume parser in
              let trivia_after_semi = consume_trivia parser in
              (* Right-associative: use `prec` not `prec + 1` *)
              match parse_expr_bp parser prec with
              | Some rhs ->
                  let children =
                    [ Ceibo.Green.Node lhs; semi ]
                    @ trivia_after_semi @ [ Ceibo.Green.Node rhs ]
                  in
                  let seq =
                    make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children
                  in
                  loop seq
              | None ->
                  (* RHS parsing failed after consuming semicolon - include it with trailing trivia *)
                  let children =
                    [ Ceibo.Green.Node lhs; semi ] @ trivia_after_semi
                  in
                  Some (make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children)
            )
        | Some op_kind when is_infix_op op_kind -> (
            let prec = get_precedence op_kind in
            if prec < min_bp then
              (* Operator precedence too low - treat trivia_before_op as trailing trivia *)
              if List.length trivia_before_op = 0 then Some lhs
              else
                let children = [ Ceibo.Green.Node lhs ] @ trivia_before_op in
                Some
                  (make_node_list
                     ~kind:(Ceibo.Green.kind (Ceibo.Green.Node lhs))
                     children)
            else
              let before_trivia, op_tok = consume parser in
              let trivia_after_op = consume_trivia parser in
              match parse_expr_bp parser (prec + 1) with
              | Some rhs ->
                  let children =
                    [ Ceibo.Green.Node lhs ] @ trivia_before_op @ [ op_tok ]
                    @ trivia_after_op @ [ Ceibo.Green.Node rhs ]
                  in
                  let infix =
                    make_node_list ~kind:Syntax_kind.INFIX_EXPR children
                  in
                  loop infix
              | None ->
                  (* RHS parsing failed after consuming operator - include it with trivia *)
                  let children =
                    [ Ceibo.Green.Node lhs ] @ trivia_before_op @ [ op_tok ]
                    @ trivia_after_op
                  in
                  Some (make_node_list ~kind:Syntax_kind.INFIX_EXPR children))
        | (Some Token.Tilde | Some Token.Question) when min_bp <= 8 -> (
            (* Labeled or optional argument *)
            match parse_labeled_or_optional_arg parser trivia_before_op with
            | Some arg ->
                let children = [ Ceibo.Green.Node lhs; Ceibo.Green.Node arg ] in
                let app =
                  make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                in
                loop app
            | None -> Some lhs)
        | Some (Token.OpenDelim Token.Bracket) -> (
            (* Could be attribute [@attr] or extension [%ext ...] *)
            match peek_nth parser 1 with
            | (Some Token.At | Some Token.AtAt | Some Token.Percent) as marker
              -> (
                let before_trivia, open_bracket = consume parser in
                let trivia_after_open = consume_trivia parser in
                let before_trivia, attr_marker = consume parser in
                (* @ or @@ or % *)
                let trivia_after_marker = consume_trivia parser in
                (* Parse attribute/extension name *)
                match peek_kind parser with
                | Some (Token.Ident _) ->
                    let before_trivia, name = consume parser in
                    let trivia_after_name = consume_trivia parser in
                    (* Collect optional payload until ] *)
                    let rec collect_payload acc trivia_acc =
                      if at parser (Token.CloseDelim Token.Bracket) then
                        (List.rev acc, List.rev trivia_acc)
                      else
                        let before_trivia, tok = consume parser in
                        let trivia = consume_trivia parser in
                        collect_payload (tok :: acc) (trivia @ trivia_acc)
                    in
                    let payload, payload_trivia = collect_payload [] [] in
                    let trivia_before_close, close_bracket, trivia_after_close =
                      expect_with_trivia parser (Token.CloseDelim Token.Bracket)
                    in
                    let children =
                      [ Ceibo.Green.Node lhs; open_bracket ]
                      @ trivia_after_open @ [ attr_marker ]
                      @ trivia_after_marker @ [ name ] @ trivia_after_name
                      @ payload @ payload_trivia @ trivia_before_close
                      @ [ close_bracket ] @ trivia_after_close
                    in
                    (* Determine kind based on marker *)
                    let kind =
                      match marker with
                      | Some Token.Percent -> Syntax_kind.EXTENSION_EXPR
                      | _ -> Syntax_kind.ATTRIBUTE_EXPR
                    in
                    let attributed = make_node_list ~kind children in
                    loop attributed
                | _ ->
                    (* Not a valid attribute/extension, treat [ as something else *)
                    Some lhs)
            | _ -> (
                (* Not an attribute/extension, could be list in function application: f [1;2] *)
                (* Function application - treat [ as starting a primary (list expr) *)
                let app_prec = 8 in
                if app_prec < min_bp then Some lhs
                else
                  (* Use trivia_before_op that was already consumed at top of loop *)
                  match parse_primary parser trivia_before_op with
                  | Some rhs ->
                      let children =
                        [ Ceibo.Green.Node lhs; Ceibo.Green.Node rhs ]
                      in
                      let app =
                        make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                      in
                      loop app
                  | None -> Some lhs))
        | Some _ when can_start_primary parser -> (
            (* Function application - juxtaposition *)
            let app_prec = 8 in
            (* Highest precedence *)
            if app_prec < min_bp then Some lhs
            else
              match parse_primary parser [] with
              | Some rhs ->
                  let children =
                    [ Ceibo.Green.Node lhs ] @ trivia_before_op
                    @ [ Ceibo.Green.Node rhs ]
                  in
                  let app =
                    make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                  in
                  loop app
              | None -> Some lhs)
        | Some _ | None ->
            (* No operator found - trivia_before_op is trailing trivia *)
            (* We need to include it in the result somehow *)
            if List.length trivia_before_op = 0 then Some lhs
            else
              (* Wrap lhs with trailing trivia *)
              let children = [ Ceibo.Green.Node lhs ] @ trivia_before_op in
              Some
                (make_node_list
                   ~kind:(Ceibo.Green.kind (Ceibo.Green.Node lhs))
                   children)
      in
      loop lhs

and can_start_primary parser =
  match peek_kind parser with
  | Some (Token.Literal _)
  | Some (Token.Ident _)
  | Some (Token.OpenDelim Token.Paren)
  | Some (Token.OpenDelim Token.Brace)
  | Some (Token.OpenDelim Token.Bracket)
  | Some (Token.OpenDelim Token.Array)
  | Some (Token.OpenDelim Token.BeginEnd)
  | Some
      (Token.Keyword
         ( Keyword.True | Keyword.False | Keyword.If | Keyword.Match
         | Keyword.Fun | Keyword.Function | Keyword.Lnot | Keyword.Assert
         | Keyword.Lazy | Keyword.For | Keyword.While | Keyword.Try ))
  | Some Token.Minus
  | Some Token.Bang
  | Some Token.Backtick ->
      true
  | _ -> false

and parse_primary parser leading_trivia =
  (* Check for prefix operators *)
  match peek_kind parser with
  | Some Token.Minus -> (
      if
        (* Check if this is a compound operator identifier like -. *)
        peek_nth parser 1 = Some Token.Dot
      then
        (* Parse as operator identifier -. *)
        let before_trivia, minus = consume parser in
        let before_trivia, dot = consume parser in
        let children = leading_trivia @ [ minus; dot ] in
        Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
      else
        (* Parse as prefix operator - *)
        let before_trivia, op = consume parser in
        let trivia_after_op = consume_trivia parser in
        match parse_expr_bp parser 7 with
        | Some operand ->
            let children =
              leading_trivia @ [ op ] @ trivia_after_op
              @ [ Ceibo.Green.Node operand ]
            in
            Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
        | None -> None)
  | Some Token.Bang -> (
      let before_trivia, op = consume parser in
      let trivia_after_op = consume_trivia parser in
      match parse_expr_bp parser 7 with
      (* Higher precedence for prefix *)
      | Some operand ->
          let children =
            leading_trivia @ [ op ] @ trivia_after_op
            @ [ Ceibo.Green.Node operand ]
          in
          Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
      | None -> None)
  | Some (Token.Keyword Keyword.Lnot) -> (
      let before_trivia, op = consume parser in
      let trivia_after_op = consume_trivia parser in
      match parse_expr_bp parser 7 with
      | Some operand ->
          let children =
            leading_trivia @ [ op ] @ trivia_after_op
            @ [ Ceibo.Green.Node operand ]
          in
          Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
      | None -> None)
  | Some Token.Tilde
    when peek_nth parser 1 = Some Token.Minus
         || peek_nth parser 1 = Some Token.Dot -> (
      (* Floating-point negation operators: ~- or ~-. *)
      let before_trivia, tilde = consume parser in
      let before_trivia, next_tok = consume parser in
      let trivia_after_op = consume_trivia parser in
      match parse_expr_bp parser 7 with
      | Some operand ->
          let children =
            leading_trivia @ [ tilde; next_tok ] @ trivia_after_op
            @ [ Ceibo.Green.Node operand ]
          in
          Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
      | None -> None)
  | _ -> (
      (* Try to parse a literal *)
      match parse_literal parser leading_trivia with
      | Some lit -> Some lit
      | None -> (
          match peek_kind parser with
          (* Identifier *)
          | Some (Token.Ident _) ->
              (* Check if this is a module-qualified literal: M.[|...|], M.[...], M.{...}, M.(...) *)
              (* We need to check BEFORE consuming the identifier *)
              let ident_text =
                match peek parser with
                | Some tok -> (
                    match tok.Token.kind with
                    | Token.Ident name -> name
                    | _ -> "")
                | None -> ""
              in
              let is_capitalized =
                String.length ident_text > 0
                && Char.uppercase_ascii ident_text.[0] = ident_text.[0]
              in
              let is_module_qualified_literal =
                is_capitalized
                && peek_nth parser 1 = Some Token.Dot
                &&
                match peek_nth parser 2 with
                | Some (Token.OpenDelim (Array | Bracket | Brace | Paren)) ->
                    true
                | _ -> false
              in
              if is_module_qualified_literal then
                (* Parse Module.[|...|] or Module.[...] or Module.{...} or Module.(...) as a whole *)
                let before_trivia_ident, ident = consume parser in
                let trivia_after_ident = consume_trivia parser in
                let before_trivia_dot, dot = consume parser in
                let trivia_after_dot = consume_trivia parser in
                match peek_kind parser with
                | Some (Token.OpenDelim Token.Array) -> (
                    match parse_array_expr parser with
                    | Some array_expr ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                          @ trivia_after_ident @ before_trivia_dot @ [ dot ]
                          @ trivia_after_dot
                          @ [ Ceibo.Green.Node array_expr ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
                    | None ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
                    )
                | Some (Token.OpenDelim Token.Bracket) -> (
                    match parse_list_expr parser [] with
                    | Some list_expr ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                          @ trivia_after_ident @ before_trivia_dot @ [ dot ]
                          @ trivia_after_dot
                          @ [ Ceibo.Green.Node list_expr ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
                    | None ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
                    )
                | Some (Token.OpenDelim Token.Brace) -> (
                    match parse_record_expr parser with
                    | Some record_expr ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                          @ trivia_after_ident @ before_trivia_dot @ [ dot ]
                          @ trivia_after_dot
                          @ [ Ceibo.Green.Node record_expr ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
                    | None ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
                    )
                | Some (Token.OpenDelim Token.Paren) -> (
                    let before_trivia_paren, open_paren = consume parser in
                    let trivia_after_open = consume_trivia parser in
                    match parse_expr parser with
                    | Some expr ->
                        let trivia_before_close, close_paren, trivia_after_close
                            =
                          expect_with_trivia parser
                            (Token.CloseDelim Token.Paren)
                        in
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                          @ trivia_after_ident @ before_trivia_dot @ [ dot ]
                          @ trivia_after_dot @ before_trivia_paren
                          @ [ open_paren ] @ trivia_after_open
                          @ [ Ceibo.Green.Node expr ] @ trivia_before_close
                          @ [ close_paren ] @ trivia_after_close
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
                    | None ->
                        let children =
                          leading_trivia @ before_trivia_ident @ [ ident ]
                        in
                        Some
                          (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
                    )
                | _ ->
                    let children =
                      leading_trivia @ before_trivia_ident @ [ ident ]
                    in
                    Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
              else
                let before_trivia, ident = consume parser in
                let children = leading_trivia @ before_trivia @ [ ident ] in
                Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
          (* Parenthesized expression *)
          | Some (Token.OpenDelim Token.Paren) ->
              parse_paren_expr parser leading_trivia
          (* List literal *)
          | Some (Token.OpenDelim Token.Bracket) -> (
              (* Could be list [x; y] or extension [%ext ...] *)
              match peek_nth parser 1 with
              | Some Token.Percent -> (
                  (* Extension expression [%ext ...] *)
                  let before_trivia, open_bracket = consume parser in
                  let trivia_after_open = consume_trivia parser in
                  let before_trivia, percent = consume parser in
                  let trivia_after_percent = consume_trivia parser in
                  match peek_kind parser with
                  | Some (Token.Ident _) ->
                      let before_trivia, name = consume parser in
                      let trivia_after_name = consume_trivia parser in
                      (* Collect payload until ] *)
                      let rec collect_payload acc trivia_acc =
                        if at parser (Token.CloseDelim Token.Bracket) then
                          (List.rev acc, List.rev trivia_acc)
                        else
                          let before_trivia, tok = consume parser in
                          let trivia = consume_trivia parser in
                          collect_payload (tok :: acc) (trivia @ trivia_acc)
                      in
                      let payload, payload_trivia = collect_payload [] [] in
                      let trivia_before_close, close_bracket, trivia_after_close
                          =
                        expect_with_trivia parser
                          (Token.CloseDelim Token.Bracket)
                      in
                      let children =
                        leading_trivia @ [ open_bracket ] @ trivia_after_open
                        @ [ percent ] @ trivia_after_percent @ [ name ]
                        @ trivia_after_name @ payload @ payload_trivia
                        @ trivia_before_close @ [ close_bracket ]
                        @ trivia_after_close
                      in
                      Some
                        (make_node_list ~kind:Syntax_kind.EXTENSION_EXPR
                           children)
                  | _ -> parse_list_expr parser leading_trivia)
              | _ -> parse_list_expr parser leading_trivia)
          (* Array literal *)
          | Some (Token.OpenDelim Token.Array) -> parse_array_expr parser
          (* Record literal or object update *)
          | Some (Token.OpenDelim Token.Brace) ->
              if
                (* Check if this is object update {< ... >} *)
                peek_nth parser 1 = Some Token.Lt
              then parse_object_update_expr parser leading_trivia
              else parse_record_expr parser
          (* Let expression *)
          | Some (Token.Keyword Keyword.Let) ->
              parse_let_expr parser leading_trivia
          (* If expression *)
          | Some (Token.Keyword Keyword.If) ->
              parse_if_expr parser leading_trivia
          (* Match expression *)
          | Some (Token.Keyword Keyword.Match) ->
              parse_match_expr parser leading_trivia
          (* Fun/function *)
          | Some (Token.Keyword Keyword.Fun) ->
              parse_fun_expr parser leading_trivia
          | Some (Token.Keyword Keyword.Function) ->
              parse_function_expr parser leading_trivia
          (* Assert *)
          | Some (Token.Keyword Keyword.Assert) ->
              parse_assert_expr parser leading_trivia
          (* Lazy *)
          | Some (Token.Keyword Keyword.Lazy) ->
              parse_lazy_expr parser leading_trivia
          (* For loop *)
          | Some (Token.Keyword Keyword.For) ->
              parse_for_expr parser leading_trivia
          (* While loop *)
          | Some (Token.Keyword Keyword.While) ->
              parse_while_expr parser leading_trivia
          (* Begin/end *)
          | Some (Token.OpenDelim Token.BeginEnd) ->
              parse_begin_expr parser leading_trivia
          (* Try/catch *)
          | Some (Token.Keyword Keyword.Try) ->
              parse_try_expr parser leading_trivia
          (* Polymorphic variant *)
          | Some Token.Backtick -> parse_poly_variant_expr parser leading_trivia
          (* Object expression *)
          | Some (Token.OpenDelim Token.ObjectEnd) ->
              parse_object_expr parser leading_trivia
          (* New expression *)
          | Some (Token.Keyword Keyword.New) ->
              parse_new_expr parser leading_trivia
          | _ -> None))

and parse_labeled_or_optional_arg parser leading_trivia =
  match peek_kind parser with
  | Some Token.Tilde -> (
      if
        (* Check if this is ~- or ~-. (float negation operators) *)
        peek_nth parser 1 = Some Token.Minus
        || peek_nth parser 1 = Some Token.Dot
      then
        (* Parse as prefix operator ~- or ~-. *)
        let before_trivia_tilde, tilde = consume parser in
        let before_trivia_next, next_tok = consume parser in
        let trivia_after_op = consume_trivia parser in
        match parse_expr_bp parser 7 with
        | Some operand ->
            let children =
              leading_trivia @ before_trivia_tilde @ [ tilde ]
              @ before_trivia_next @ [ next_tok ] @ trivia_after_op
              @ [ Ceibo.Green.Node operand ]
            in
            Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
        | None -> None
      else
        (* Labeled argument: ~label or ~label:expr *)
        let before_trivia_tilde, tilde = consume parser in
        let trivia_after_tilde = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let before_trivia_label, label = consume parser in
            let trivia_after_label = consume_trivia parser in
            if at parser Token.Colon then
              let before_trivia_colon, colon = consume parser in
              let trivia_after_colon = consume_trivia parser in
              match parse_primary parser trivia_after_colon with
              | Some value ->
                  let children =
                    leading_trivia @ before_trivia_tilde @ [ tilde ]
                    @ trivia_after_tilde @ before_trivia_label @ [ label ]
                    @ trivia_after_label @ before_trivia_colon @ [ colon ]
                    @ [ Ceibo.Green.Node value ]
                  in
                  Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
              | None ->
                  let children =
                    leading_trivia @ before_trivia_tilde @ [ tilde ]
                    @ trivia_after_tilde @ before_trivia_label @ [ label ]
                    @ trivia_after_label
                  in
                  Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
            else
              (* Punning: ~label is shorthand for ~label:label *)
              let children =
                leading_trivia @ before_trivia_tilde @ [ tilde ]
                @ trivia_after_tilde @ before_trivia_label @ [ label ]
                @ trivia_after_label
              in
              Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
        | _ -> None)
  | Some Token.Question -> (
      (* Optional argument: ?label or ?label:expr *)
      let before_trivia_question, question = consume parser in
      let trivia_after_question = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let before_trivia_label, label = consume parser in
          let trivia_after_label = consume_trivia parser in
          if at parser Token.Colon then
            let before_trivia_colon, colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            match parse_primary parser trivia_after_colon with
            | Some value ->
                let children =
                  leading_trivia @ before_trivia_question @ [ question ]
                  @ trivia_after_question @ before_trivia_label @ [ label ]
                  @ trivia_after_label @ before_trivia_colon @ [ colon ]
                  @ [ Ceibo.Green.Node value ]
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
            | None ->
                let children =
                  leading_trivia @ before_trivia_question @ [ question ]
                  @ trivia_after_question @ before_trivia_label @ [ label ]
                  @ trivia_after_label
                in
                Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
          else
            (* Punning: ?label is shorthand for ?label:label *)
            let children =
              leading_trivia @ before_trivia_question @ [ question ]
              @ trivia_after_question @ before_trivia_label @ [ label ]
              @ trivia_after_label
            in
            Some (make_node_list ~kind:Syntax_kind.ARGUMENT children)
      | _ -> None)
  | _ -> None

and parse_labeled_or_optional_param parser =
  let leading_trivia = consume_trivia parser in
  match peek_kind parser with
  | Some Token.Tilde -> (
      (* Labeled parameter: ~label or ~label:pattern or ~(label:pattern) *)
      let before_trivia, tilde = consume parser in
      let trivia_after_tilde = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.OpenDelim Token.Paren) -> (
          (* Parenthesized labeled parameter: ~(label : type) *)
          let before_trivia, open_paren = consume parser in
          let trivia_after_open = consume_trivia parser in
          match peek_kind parser with
          | Some (Token.Ident _) ->
              let before_trivia, label = consume parser in
              let trivia_after_label = consume_trivia parser in
              if at parser Token.Colon then
                let before_trivia, colon = consume parser in
                let trivia_after_colon = consume_trivia parser in
                (* Consume type tokens until closing paren *)
                let rec consume_type_tokens acc trivia_acc depth =
                  match peek_kind parser with
                  | Some (Token.CloseDelim Token.Paren) when depth = 0 ->
                      (List.rev acc, List.rev trivia_acc)
                  | Some (Token.OpenDelim Token.Paren) ->
                      let before_trivia, tok = consume parser in
                      let trivia = consume_trivia parser in
                      consume_type_tokens (tok :: acc) (trivia @ trivia_acc)
                        (depth + 1)
                  | Some (Token.CloseDelim Token.Paren) ->
                      let before_trivia, tok = consume parser in
                      let trivia = consume_trivia parser in
                      consume_type_tokens (tok :: acc) (trivia @ trivia_acc)
                        (depth - 1)
                  | Some _ ->
                      let before_trivia, tok = consume parser in
                      let trivia = consume_trivia parser in
                      consume_type_tokens (tok :: acc) (trivia @ trivia_acc)
                        depth
                  | None -> (List.rev acc, List.rev trivia_acc)
                in
                let type_tokens, type_trivia = consume_type_tokens [] [] 0 in
                let trivia_before_close, close_paren, trivia_after_close =
                  expect_with_trivia parser (Token.CloseDelim Token.Paren)
                in
                let children =
                  leading_trivia @ [ tilde ] @ trivia_after_tilde
                  @ [ open_paren ] @ trivia_after_open @ [ label ]
                  @ trivia_after_label @ [ colon ] @ trivia_after_colon
                  @ type_tokens @ type_trivia @ trivia_before_close
                  @ [ close_paren ] @ trivia_after_close
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
              else
                let trivia_before_close, close_paren, trivia_after_close =
                  expect_with_trivia parser (Token.CloseDelim Token.Paren)
                in
                let children =
                  leading_trivia @ [ tilde ] @ trivia_after_tilde
                  @ [ open_paren ] @ trivia_after_open @ [ label ]
                  @ trivia_before_close @ [ close_paren ] @ trivia_after_close
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          | _ -> None)
      | Some (Token.Ident _) ->
          let before_trivia, label = consume parser in
          let trivia_after_label = consume_trivia parser in
          if at parser Token.Colon then
            let before_trivia, colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            match parse_pattern parser with
            | Some pattern ->
                let children =
                  leading_trivia @ [ tilde ] @ trivia_after_tilde @ [ label ]
                  @ trivia_after_label @ [ colon ] @ trivia_after_colon
                  @ [ Ceibo.Green.Node pattern ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
            | None ->
                let children =
                  leading_trivia @ [ tilde ] @ trivia_after_tilde @ [ label ]
                  @ trivia_after_label
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          else
            (* Punning: ~label is parameter named label *)
            let children =
              leading_trivia @ [ tilde ] @ trivia_after_tilde @ [ label ]
              @ trivia_after_label
            in
            Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
      | _ -> None)
  | Some Token.Question -> (
      (* Optional parameter: ?label or ?label:pattern or ?(label = default) *)
      let before_trivia, question = consume parser in
      let trivia_after_question = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let before_trivia, label = consume parser in
          let trivia_after_label = consume_trivia parser in
          if at parser Token.Colon then
            let before_trivia, colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            match parse_pattern parser with
            | Some pattern ->
                let children =
                  leading_trivia @ [ question ] @ trivia_after_question
                  @ [ label ] @ trivia_after_label @ [ colon ]
                  @ trivia_after_colon
                  @ [ Ceibo.Green.Node pattern ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
            | None ->
                let children =
                  leading_trivia @ [ question ] @ trivia_after_question
                  @ [ label ] @ trivia_after_label
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          else
            (* Punning: ?label is optional parameter named label *)
            let children =
              leading_trivia @ [ question ] @ trivia_after_question @ [ label ]
              @ trivia_after_label
            in
            Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
      | Some (Token.OpenDelim Token.Paren) -> (
          (* Parenthesized optional with default: ?(label = default) *)
          let before_trivia, open_paren = consume parser in
          let trivia_after_open = consume_trivia parser in
          match peek_kind parser with
          | Some (Token.Ident _) ->
              let before_trivia, label = consume parser in
              let trivia_after_label = consume_trivia parser in
              if at parser Token.Eq then
                let before_trivia, eq = consume parser in
                let trivia_after_eq = consume_trivia parser in
                match parse_expr parser with
                | Some default ->
                    let trivia_before_close, close_paren, trivia_after_close =
                      expect_with_trivia parser (Token.CloseDelim Token.Paren)
                    in
                    let children =
                      leading_trivia @ [ question ] @ trivia_after_question
                      @ [ open_paren ] @ trivia_after_open @ [ label ]
                      @ trivia_after_label @ [ eq ] @ trivia_after_eq
                      @ [ Ceibo.Green.Node default ]
                      @ trivia_before_close @ [ close_paren ]
                      @ trivia_after_close
                    in
                    Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
                | None ->
                    let children =
                      leading_trivia @ [ question ] @ trivia_after_question
                      @ [ open_paren ] @ trivia_after_open @ [ label ]
                    in
                    Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
              else
                let children =
                  leading_trivia @ [ question ] @ trivia_after_question
                  @ [ open_paren ] @ trivia_after_open @ [ label ]
                in
                Some (make_node_list ~kind:Syntax_kind.PARAMETER children)
          | _ -> None)
      | _ -> None)
  | _ -> None

and parse_module_type_expr parser =
  (* Parse a module type expression: S | S with type t = int | sig ... end *)
  (* Note: For sig...end, caller should handle it before calling this function *)
  if at parser (Token.OpenDelim Token.SigEnd) then
    (* This case should be handled by caller, but we'll return a placeholder *)
    (* Consume sig...end as raw tokens for now *)
    let before_trivia, sig_kw = consume parser in
    let trivia_after_sig = consume_trivia parser in

    let rec consume_until_end acc =
      if at parser (Token.CloseDelim Token.SigEnd) then List.rev acc
      else if peek parser = None then List.rev acc
      else
        let before_trivia, tok = consume parser in
        let trivia_after_tok = consume_trivia parser in
        consume_until_end (List.rev_append trivia_after_tok (tok :: acc))
    in

    let items = consume_until_end [] in
    let before_trivia, end_kw = consume parser in

    make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR
      ([ sig_kw ] @ trivia_after_sig @ items @ [ end_kw ])
  else
    (* Module type identifier or path *)
    let type_ident = parse_identifier parser in
    let trivia_after_ident = consume_trivia parser in

    (* Check for 'with' constraints *)
    if at parser (Token.Keyword Keyword.With) then
      let before_trivia, with_kw = consume parser in
      let trivia_after_with = consume_trivia parser in

      (* Parse 'with type t = ...' constraints *)
      let rec parse_with_constraints acc =
        if not (at parser (Token.Keyword Keyword.Type)) then List.rev acc
        else
          let before_trivia, type_kw = consume parser in
          let trivia_after_type = consume_trivia parser in

          (* Parse type path (t or M.t) *)
          let type_path = parse_identifier parser in
          let trivia_after_path = consume_trivia parser in

          (* Expect = *)
          let before_trivia, eq = expect parser Token.Eq in
          let trivia_after_eq = consume_trivia parser in

          (* Parse type expression - handle simple types and wildcards *)
          (* This is a simplified parser since we can't call parse_type_expr due to ordering *)
          let rec consume_type_tokens acc depth =
            match peek_kind parser with
            | Some (Token.Keyword Keyword.And) when depth = 0 ->
                (* Stop at 'and' when not inside parens *)
                List.rev acc
            | Some (Token.CloseDelim Token.Paren) when depth = 0 ->
                (* Stop at ')' when not inside parens *)
                List.rev acc
            | Some (Token.OpenDelim Token.Paren) ->
                let before_trivia, tok = consume parser in
                let trivia_after_tok = consume_trivia parser in
                consume_type_tokens
                  (List.rev_append trivia_after_tok (tok :: acc))
                  (depth + 1)
            | Some (Token.CloseDelim Token.Paren) ->
                let before_trivia, tok = consume parser in
                let trivia_after_tok = consume_trivia parser in
                consume_type_tokens
                  (List.rev_append trivia_after_tok (tok :: acc))
                  (depth - 1)
            | Some Token.Underscore
            | Some Token.Quote
            | Some (Token.Ident _)
            | Some Token.Arrow
            | Some Token.Star
            | Some Token.Dot
            | Some (Token.Literal _) ->
                let before_trivia, tok = consume parser in
                let trivia_after_tok = consume_trivia parser in
                consume_type_tokens
                  (List.rev_append trivia_after_tok (tok :: acc))
                  depth
            | None -> List.rev acc
            | _ ->
                (* Stop at other tokens *)
                List.rev acc
          in
          let type_tokens = consume_type_tokens [] 0 in

          let constraint_node =
            make_node_list ~kind:Syntax_kind.TYPE_CONSTRAINT
              ([ type_kw ] @ trivia_after_type @ type_path @ trivia_after_path
             @ [ eq ] @ trivia_after_eq @ type_tokens)
          in

          (* Check if there's another 'and' constraint *)
          if at parser (Token.Keyword Keyword.And) then
            let before_trivia, and_kw = consume parser in
            let trivia_after_and = consume_trivia parser in
            parse_with_constraints
              (List.rev_append trivia_after_and
                 (and_kw :: Ceibo.Green.Node constraint_node :: acc))
          else parse_with_constraints (Ceibo.Green.Node constraint_node :: acc)
      in

      let constraints = parse_with_constraints [] in

      make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR
        (type_ident @ trivia_after_ident @ [ with_kw ] @ trivia_after_with
       @ constraints)
    else
      (* Simple module type reference *)
      make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR type_ident

and parse_paren_expr parser leading_trivia =
  let before_trivia, open_paren = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Check for unit literal () *)
  if at parser (Token.CloseDelim Token.Paren) then
    let before_trivia, close_paren = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      leading_trivia @ [ open_paren ] @ trivia_after_open @ [ close_paren ]
      @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.UNIT_LITERAL children)
    (* Check for first-class module pack: (module M : S) or (module struct ... end) *)
  else if at parser (Token.Keyword Keyword.Module) then
    let before_trivia, module_kw = consume parser in
    let trivia_after_module = consume_trivia parser in

    (* Check if it's a struct expression or a typed module *)
    if at parser (Token.OpenDelim Token.StructEnd) then (
      (* (module struct ... end) - module expression without type annotation *)
      let struct_tokens = ref [] in
      let trivia_tokens = ref [] in
      let depth = ref 1 in
      let before_trivia, struct_kw = consume parser in
      struct_tokens := struct_kw :: !struct_tokens;
      let trivia = consume_trivia parser in
      trivia_tokens := trivia @ !trivia_tokens;

      (* Consume until matching 'end' *)
      while !depth > 0 && peek parser <> None do
        match peek_kind parser with
        | Some (Token.OpenDelim Token.StructEnd) ->
            depth := !depth + 1;
            let before_trivia, tok = consume parser in
            trivia_tokens := before_trivia @ !trivia_tokens;
            struct_tokens := tok :: !struct_tokens;
            let trivia = consume_trivia parser in
            trivia_tokens := trivia @ !trivia_tokens;
            ()
        | Some (Token.CloseDelim Token.StructEnd) ->
            depth := !depth - 1;
            let before_trivia, tok = consume parser in
            trivia_tokens := before_trivia @ !trivia_tokens;
            struct_tokens := tok :: !struct_tokens;
            if !depth > 0 then (
              let trivia = consume_trivia parser in
              trivia_tokens := trivia @ !trivia_tokens;
              ())
        | _ ->
            let before_trivia, tok = consume parser in
            trivia_tokens := before_trivia @ !trivia_tokens;
            struct_tokens := tok :: !struct_tokens;
            let trivia = consume_trivia parser in
            trivia_tokens := trivia @ !trivia_tokens;
            ()
      done;

      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in
      let children =
        leading_trivia @ [ open_paren ] @ trivia_after_open @ [ module_kw ]
        @ List.rev !struct_tokens @ List.rev !trivia_tokens
        @ trivia_before_close @ [ close_paren ] @ trivia_after_close
      in
      Some (make_node_list ~kind:Syntax_kind.APPLY_EXPR children))
    else
      (* (module M : S) or (module M) - first-class module *)
      (* Parse module name (can be qualified: A.B.C) *)
      let trivia_after_module = consume_trivia parser in
      let module_name_parts = parse_identifier parser in
      let trivia_after_name = consume_trivia parser in

      (* Optional type annotation *)
      let type_annotation, type_trivia =
        if at parser Token.Colon then
          let before_trivia, colon = consume parser in
          let trivia_after_colon = consume_trivia parser in
          let module_type = parse_module_type_expr parser in
          let trivia_after_type = consume_trivia parser in
          ( [ colon ] @ trivia_after_colon @ [ Ceibo.Green.Node module_type ],
            trivia_after_type )
        else ([], [])
      in

      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in
      let children =
        leading_trivia @ [ open_paren ] @ trivia_after_open @ [ module_kw ]
        @ trivia_after_module @ module_name_parts @ trivia_after_name
        @ type_annotation @ type_trivia @ trivia_before_close @ [ close_paren ]
        @ trivia_after_close
      in
      Some (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
    (* Check for parenthesized operator: ( + ), ( - ), ( * ), etc. *)
    (* Only if the operator is immediately followed by ) (possibly with whitespace) *)
  else if
    (match peek_kind parser with Some k -> is_infix_op k | None -> false)
    (* Check if closing paren follows the operator (accounting for whitespace) *)
    &&
    (* Save position to restore if this isn't a parenthesized operator *)
    let saved_pos = parser.position in
    let before_trivia, _ = consume parser in
    (* consume the operator *)
    let _trivia_lookahead = consume_trivia parser in
    (* skip any whitespace *)
    let is_paren_op = at parser (Token.CloseDelim Token.Paren) in
    parser.position <- saved_pos;
    (* restore position *)
    is_paren_op
  then
    (* It's a parenthesized operator like ( + ) *)
    let before_trivia, op = consume parser in
    let trivia_after_op = consume_trivia parser in
    let trivia_before_close, close_paren, trivia_after_close =
      expect_with_trivia parser (Token.CloseDelim Token.Paren)
    in
    let children =
      leading_trivia @ [ open_paren ] @ trivia_after_open @ [ op ]
      @ trivia_after_op @ trivia_before_close @ [ close_paren ]
      @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.PAREN_EXPR children)
  else
    match parse_expr parser with
    | Some expr ->
        let trivia_after_expr = consume_trivia parser in
        (* Check if it's a tuple (has comma), sequence (has semicolon), type annotation (has colon), or just parenthesized expr *)
        if at parser Token.Comma then
          parse_tuple_rest parser trivia_after_open open_paren trivia_after_expr
            expr
        else if at parser Token.Semi then
          parse_sequence_rest parser trivia_after_open open_paren
            trivia_after_expr expr
        else if at parser Token.Colon then
          (* Type annotation: (expr : type) *)
          let before_trivia, colon = consume parser in
          let trivia_after_colon = consume_trivia parser in
          (* For now, just consume tokens until closing paren as the "type" *)
          (* A proper implementation would parse the type, but we'll keep it simple *)
          (* Parse type tokens until closing paren - functional approach *)
          let rec consume_type_tokens acc trivia_acc =
            if at parser (Token.CloseDelim Token.Paren) || peek parser = None
            then (List.rev acc, List.rev trivia_acc)
            else
              let before_trivia, tok = consume parser in
              let trivia = consume_trivia parser in
              consume_type_tokens (tok :: acc) (trivia @ trivia_acc)
          in
          let type_elements, type_trivia = consume_type_tokens [] [] in
          let trivia_before_close, close_paren, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Paren)
          in
          let children =
            leading_trivia @ [ open_paren ] @ trivia_after_open
            @ [ Ceibo.Green.Node expr; colon ]
            @ trivia_after_colon @ type_elements @ type_trivia
            @ trivia_before_close @ [ close_paren ] @ trivia_after_close
          in
          Some (make_node_list ~kind:Syntax_kind.TYPED_EXPR children)
        else
          let trivia_before_close, close_paren, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Paren)
          in
          let children =
            leading_trivia @ [ open_paren ] @ trivia_after_open
            @ [ Ceibo.Green.Node expr ] @ trivia_after_expr
            @ trivia_before_close @ [ close_paren ] @ trivia_after_close
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

and parse_tuple_rest parser trivia_after_open open_paren trivia_after_first
    first_expr =
  let rec parse_elements acc =
    if not (at parser Token.Comma) then List.rev acc
    else
      let _before_trivia, comma = consume parser in
      let trivia_after_comma = consume_trivia parser in
      let acc = trivia_after_comma @ (comma :: acc) in
      match parse_expr parser with
      | Some expr ->
          let trivia_after_expr = consume_trivia parser in
          parse_elements (trivia_after_expr @ (Ceibo.Green.Node expr :: acc))
      | None -> List.rev acc
  in

  let elements =
    parse_elements (trivia_after_first @ [ Ceibo.Green.Node first_expr ])
  in

  let trivia_before_close, close_paren, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Paren)
  in
  let children =
    [ open_paren ] @ trivia_after_open @ elements @ trivia_before_close
    @ [ close_paren ] @ trivia_after_close
  in
  Some (make_node_list ~kind:Syntax_kind.TUPLE_EXPR children)

and parse_sequence_rest parser trivia_after_open open_paren trivia_after_first
    first_expr =
  let rec parse_elements acc trivia_acc =
    if not (at parser Token.Semi) then (List.rev acc, List.rev trivia_acc)
    else
      let before_trivia, semi = consume parser in
      let trivia_after_semi = consume_trivia parser in
      let acc = semi :: acc in
      let trivia_acc = trivia_after_semi @ trivia_acc in
      match parse_expr parser with
      | Some expr ->
          let trivia_after_expr = consume_trivia parser in
          parse_elements
            (Ceibo.Green.Node expr :: acc)
            (trivia_after_expr @ trivia_acc)
      | None -> (List.rev acc, List.rev trivia_acc)
  in

  let elements, elements_trivia =
    parse_elements [ Ceibo.Green.Node first_expr ] trivia_after_first
  in

  let trivia_before_close, close_paren, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Paren)
  in
  let children =
    [ open_paren ] @ trivia_after_open @ elements @ elements_trivia
    @ trivia_before_close @ [ close_paren ] @ trivia_after_close
  in
  Some (make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children)

and parse_list_expr parser leading_trivia =
  let before_trivia, open_bracket = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Check for empty list [] *)
  if at parser (Token.CloseDelim Token.Bracket) then
    let before_trivia_close, close_bracket = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      leading_trivia @ [ open_bracket ] @ trivia_after_open
      @ before_trivia_close @ [ close_bracket ] @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.LIST_EXPR children)
  else
    match parse_expr parser with
    | Some first_expr ->
        let trivia_after_first = consume_trivia parser in
        (* List elements are separated by semicolons *)
        let rec parse_elements acc trivia_acc =
          if not (at parser Token.Semi) then (List.rev acc, List.rev trivia_acc)
          else
            let before_trivia, semi = consume parser in
            let trivia_after_semi = consume_trivia parser in
            let acc = semi :: acc in
            let trivia_acc = trivia_after_semi @ trivia_acc in
            match parse_expr parser with
            | Some expr ->
                let trivia_after_expr = consume_trivia parser in
                parse_elements
                  (Ceibo.Green.Node expr :: acc)
                  (trivia_after_expr @ trivia_acc)
            | None -> (List.rev acc, List.rev trivia_acc)
        in

        let elements, elements_trivia =
          parse_elements [ Ceibo.Green.Node first_expr ] trivia_after_first
        in

        let trivia_before_close, close_bracket, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Bracket)
        in
        let children =
          leading_trivia @ [ open_bracket ] @ trivia_after_open @ elements
          @ elements_trivia @ trivia_before_close @ [ close_bracket ]
          @ trivia_after_close
        in
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

and parse_array_expr parser =
  let leading_trivia, open_array = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Check for empty array [| |] *)
  if at parser (Token.CloseDelim Token.Array) then
    let before_trivia_close, close_array = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      leading_trivia @ [ open_array ] @ trivia_after_open @ before_trivia_close
      @ [ close_array ] @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)
  else
    match parse_expr parser with
    | Some first_expr ->
        let trivia_after_first = consume_trivia parser in
        (* Array elements are separated by semicolons *)
        let rec parse_elements acc trivia_acc =
          if not (at parser Token.Semi) then (List.rev acc, List.rev trivia_acc)
          else
            let before_trivia, semi = consume parser in
            let trivia_after_semi = consume_trivia parser in
            let acc = semi :: acc in
            let trivia_acc = trivia_after_semi @ trivia_acc in
            (* Allow trailing semicolon *)
            if at parser (Token.CloseDelim Token.Array) then
              (List.rev acc, List.rev trivia_acc)
            else
              match parse_expr parser with
              | Some expr ->
                  let trivia_after_expr = consume_trivia parser in
                  parse_elements
                    (Ceibo.Green.Node expr :: acc)
                    (trivia_after_expr @ trivia_acc)
              | None -> (List.rev acc, List.rev trivia_acc)
        in

        let elements, elements_trivia =
          parse_elements [ Ceibo.Green.Node first_expr ] trivia_after_first
        in

        let trivia_before_close, close_array, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Array)
        in
        let children =
          leading_trivia @ [ open_array ] @ trivia_after_open @ elements
          @ elements_trivia @ trivia_before_close @ [ close_array ]
          @ trivia_after_close
        in
        Some (make_node_list ~kind:Syntax_kind.ARRAY_EXPR children)
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
             ~kind:(Diagnostic.InvalidSyntax { context = "array expression" })
             ~span)

and parse_record_expr parser =
  let before_trivia, open_brace = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Check for empty record {} - though this isn't valid OCaml, we'll parse it *)
  if at parser (Token.CloseDelim Token.Brace) then
    let before_trivia, close_brace = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      [ open_brace ] @ trivia_after_open @ [ close_brace ] @ trivia_after_close
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

    (* Check if first non-trivia token is identifier followed by = or ; or } (record literal) *)
    (* = means field = value, ; or } means field punning *)
    (* Also handle qualified field names like Module.field = value *)
    let is_record_literal =
      match peek_non_trivia_nth parser 0 with
      | Some (Token.Ident _) ->
          (* Skip over potential module path (Ident . Ident . ... . Ident) *)
          let rec find_after_path n =
            match peek_non_trivia_nth parser n with
            | Some Token.Dot ->
                find_after_path (n + 2) (* Skip . and next ident *)
            | Some Token.Eq -> true (* field = value *)
            | Some Token.Semi -> true (* field; (punning) *)
            | Some (Token.CloseDelim Token.Brace) ->
                true (* field } (punning at end) *)
            | _ -> false
          in
          find_after_path 1
      | _ -> false
    in

    if is_record_literal then
      (* Parse as record literal { field = value; ... } *)
      (* Parse fields *)
      let fields, fields_trivia =
        match parse_record_field parser with
        | Some field ->
            let trivia_after_field = consume_trivia parser in

            (* Parse remaining fields *)
            let rec parse_fields acc trivia_acc =
              if not (at parser Token.Semi) then
                (List.rev acc, List.rev trivia_acc)
              else
                let before_trivia, semi = consume parser in
                let acc = semi :: acc in
                match parse_record_field parser with
                | Some f ->
                    let trivia_after_f = consume_trivia parser in
                    parse_fields
                      (Ceibo.Green.Node f :: acc)
                      (trivia_after_f @ trivia_acc)
                | None -> (List.rev acc, List.rev trivia_acc)
            in

            parse_fields [ Ceibo.Green.Node field ] trivia_after_field
        | None -> ([], [])
      in

      let trivia_before_close, close_brace, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Brace)
      in
      let children =
        [ open_brace ] @ trivia_after_open @ fields @ fields_trivia
        @ trivia_before_close @ [ close_brace ] @ trivia_after_close
      in
      Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
    else
      (* Parse expression first (for record update) *)
      match parse_expr parser with
      | Some base_expr ->
          let trivia_after_base = consume_trivia parser in
          if at parser (Token.Keyword Keyword.With) then
            (* Record update: { expr with field = value; ... } *)
            let before_trivia, with_kw = consume parser in
            let trivia_after_with = consume_trivia parser in

            (* Parse fields *)
            let fields, fields_trivia =
              match parse_record_field parser with
              | Some field ->
                  let trivia_after_field = consume_trivia parser in

                  (* Parse remaining fields *)
                  let rec parse_update_fields acc trivia_acc =
                    if not (at parser Token.Semi) then
                      (List.rev acc, List.rev trivia_acc)
                    else
                      let before_trivia, semi = consume parser in
                      let trivia_after_semi = consume_trivia parser in
                      let acc = semi :: acc in
                      let trivia_acc = trivia_after_semi @ trivia_acc in
                      match parse_record_field parser with
                      | Some field ->
                          let trivia_after_f = consume_trivia parser in
                          parse_update_fields
                            (Ceibo.Green.Node field :: acc)
                            (trivia_after_f @ trivia_acc)
                      | None -> (List.rev acc, List.rev trivia_acc)
                  in

                  parse_update_fields [ Ceibo.Green.Node field ]
                    trivia_after_field
              | None -> ([], [])
            in

            let trivia_before_close, close_brace, trivia_after_close =
              expect_with_trivia parser (Token.CloseDelim Token.Brace)
            in
            let children =
              [ open_brace ] @ trivia_after_open
              @ [ Ceibo.Green.Node base_expr ]
              @ trivia_after_base @ [ with_kw ] @ trivia_after_with @ fields
              @ fields_trivia @ trivia_before_close @ [ close_brace ]
              @ trivia_after_close
            in
            Some (make_node_list ~kind:Syntax_kind.RECORD_UPDATE_EXPR children)
          else
            (* Error: expected 'with' in record update *)
            let span =
              match peek parser with
              | Some tok -> tok.Token.span
              | None -> Ceibo.Span.make ~start:0 ~end_:0
            in
            report_error parser
              (Diagnostic.make_missing_token ~expected:"'with'" ~span);
            let trivia_before_close, close_brace, trivia_after_close =
              expect_with_trivia parser (Token.CloseDelim Token.Brace)
            in
            let children =
              [ open_brace ] @ trivia_after_open
              @ [ Ceibo.Green.Node base_expr ]
              @ trivia_before_close @ [ close_brace ] @ trivia_after_close
            in
            Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
      | None ->
          let trivia_before_close, close_brace, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Brace)
          in
          let children =
            [ open_brace ] @ trivia_after_open @ trivia_before_close
            @ [ close_brace ] @ trivia_after_close
          in
          Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)

and parse_record_field parser =
  let leading_trivia = consume_trivia parser in
  match peek_kind parser with
  | Some (Token.Ident _) ->
      (* Parse field name - could be qualified like Module.field *)
      let field_name_parts = parse_identifier parser in
      let trivia_after_field = consume_trivia parser in
      if at parser Token.Eq then
        let before_trivia, eq = consume parser in
        let trivia_after_eq = consume_trivia parser in
        (* Parse field value with min_bp=1 to stop at semicolons (precedence 0) *)
        match parse_expr_bp parser 1 with
        | Some value ->
            let children =
              leading_trivia @ field_name_parts @ trivia_after_field @ [ eq ]
              @ trivia_after_eq @ [ Ceibo.Green.Node value ]
            in
            Some (make_node_list ~kind:Syntax_kind.RECORD_FIELD children)
        | None -> None
      else
        (* Punning: { x } is shorthand for { x = x } *)
        let children = leading_trivia @ field_name_parts @ trivia_after_field in
        Some (make_node_list ~kind:Syntax_kind.RECORD_FIELD children)
  | _ -> None

and parse_assert_expr parser leading_trivia =
  let before_trivia, assert_kw = consume parser in
  let trivia_after_assert = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let children =
        leading_trivia @ [ assert_kw ] @ trivia_after_assert
        @ [ Ceibo.Green.Node expr ]
      in
      Some (make_node_list ~kind:Syntax_kind.ASSERT_EXPR children)
  | None -> None

and parse_lazy_expr parser leading_trivia =
  let before_trivia, lazy_kw = consume parser in
  let trivia_after_lazy = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let children =
        leading_trivia @ [ lazy_kw ] @ trivia_after_lazy
        @ [ Ceibo.Green.Node expr ]
      in
      Some (make_node_list ~kind:Syntax_kind.LAZY_EXPR children)
  | None -> None

and parse_poly_variant_expr parser leading_trivia =
  let before_trivia, backtick = consume parser in
  let trivia_after_backtick = consume_trivia parser in

  (* Polymorphic variant tag - can be any identifier (lowercase or uppercase) *)
  match peek_kind parser with
  | Some (Token.Ident _) ->
      let before_trivia, tag_token = consume parser in
      let trivia_after_tag = consume_trivia parser in
      (* Check if there's an argument *)
      if can_start_primary parser then
        match parse_primary parser trivia_after_tag with
        | Some arg ->
            let children =
              leading_trivia @ [ backtick ] @ trivia_after_backtick
              @ [ tag_token ] @ [ Ceibo.Green.Node arg ]
            in
            Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
        | None ->
            let children =
              leading_trivia @ [ backtick ] @ trivia_after_backtick
              @ [ tag_token ]
            in
            Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
      else
        let children =
          leading_trivia @ [ backtick ] @ trivia_after_backtick @ [ tag_token ]
        in
        Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)
  | _ ->
      (* Missing or invalid tag *)
      let children = leading_trivia @ [ backtick ] in
      Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_EXPR children)

and parse_for_expr parser leading_trivia =
  let before_trivia, for_kw = consume parser in
  let trivia_after_for = consume_trivia parser in

  (* Parse: for <ident> = <expr> to/downto <expr> do <expr> done *)
  (* Loop variable can be an identifier or _ *)
  let ident_before_trivia, ident =
    match peek_kind parser with
    | Some (Token.Ident _) | Some Token.Underscore -> consume parser
    | _ ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"identifier" ~span);
        let missing_tok =
          Ceibo.Green.Token
            (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
        in
        ([], missing_tok)
  in

  let trivia_after_ident = consume_trivia parser in
  let eq_before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

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

  let trivia_after_start = consume_trivia parser in
  let direction_before_trivia, direction =
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
        let missing_tok =
          Ceibo.Green.Token
            (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
        in
        ([], missing_tok)
  in

  let trivia_after_direction = consume_trivia parser in
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

  let trivia_after_end = consume_trivia parser in
  let do_before_trivia, do_kw = expect parser (Token.Keyword Keyword.Do) in
  let trivia_after_do = consume_trivia parser in

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

  let trivia_after_body = consume_trivia parser in
  let done_before_trivia, done_kw =
    expect parser (Token.Keyword Keyword.Done)
  in

  let children =
    leading_trivia @ [ for_kw ] @ trivia_after_for @ ident_before_trivia
    @ [ ident ] @ trivia_after_ident @ eq_before_trivia @ [ eq ]
    @ trivia_after_eq
    @ [ Ceibo.Green.Node start_expr ]
    @ trivia_after_start @ direction_before_trivia @ [ direction ]
    @ trivia_after_direction
    @ [ Ceibo.Green.Node end_expr ]
    @ trivia_after_end @ do_before_trivia @ [ do_kw ] @ trivia_after_do
    @ [ Ceibo.Green.Node body ] @ trivia_after_body @ done_before_trivia
    @ [ done_kw ]
  in
  Some (make_node_list ~kind:Syntax_kind.FOR_EXPR children)

and parse_while_expr parser leading_trivia =
  let before_trivia, while_kw = consume parser in
  let trivia_after_while = consume_trivia parser in

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

  let trivia_after_cond = consume_trivia parser in
  let before_trivia, do_kw = expect parser (Token.Keyword Keyword.Do) in
  let trivia_after_do = consume_trivia parser in

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

  let trivia_after_body = consume_trivia parser in
  let before_trivia, done_kw = expect parser (Token.Keyword Keyword.Done) in

  let children =
    leading_trivia @ [ while_kw ] @ trivia_after_while
    @ [ Ceibo.Green.Node cond ] @ trivia_after_cond @ [ do_kw ]
    @ trivia_after_do @ [ Ceibo.Green.Node body ] @ trivia_after_body
    @ [ done_kw ]
  in
  Some (make_node_list ~kind:Syntax_kind.WHILE_EXPR children)

and parse_begin_expr parser leading_trivia =
  let before_trivia, begin_kw = consume parser in
  let trivia_after_begin = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let trivia_after_expr = consume_trivia parser in
      let trivia_before_end, end_kw, trivia_after_end =
        expect_with_trivia parser (Token.CloseDelim Token.BeginEnd)
      in
      let children =
        leading_trivia @ [ begin_kw ] @ trivia_after_begin
        @ [ Ceibo.Green.Node expr ] @ trivia_after_expr @ trivia_before_end
        @ [ end_kw ] @ trivia_after_end
      in
      Some (make_node_list ~kind:Syntax_kind.PAREN_EXPR children)
  | None -> None

and parse_try_expr parser leading_trivia =
  let before_trivia, try_kw = consume parser in
  let trivia_after_try = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let trivia_after_expr = consume_trivia parser in
      let before_trivia, with_kw = expect parser (Token.Keyword Keyword.With) in
      let trivia_after_with = consume_trivia parser in

      (* Parse match cases *)
      let first_case =
        if not (at parser Token.Pipe) then
          match parse_match_case_no_pipe parser with
          | Some case ->
              let trivia_after_case = consume_trivia parser in
              trivia_after_case @ [ Ceibo.Green.Node case ]
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

      let children =
        leading_trivia @ [ try_kw ] @ trivia_after_try
        @ [ Ceibo.Green.Node expr ] @ trivia_after_expr @ [ with_kw ]
        @ trivia_after_with @ rest_cases
      in
      Some (make_node_list ~kind:Syntax_kind.TRY_EXPR children)
  | None -> None

and parse_new_expr parser leading_trivia =
  let before_trivia, new_kw = consume parser in
  let trivia_after_new = consume_trivia parser in

  (* Parse class path (might be Module.class_name) *)
  let rec parse_class_path acc trivia_acc =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let before_trivia, ident = consume parser in
        let trivia_after_ident = consume_trivia parser in
        let acc = ident :: acc in
        let trivia_acc = trivia_after_ident @ trivia_acc in
        if at parser Token.Dot then
          let before_trivia, dot = consume parser in
          let trivia_after_dot = consume_trivia parser in
          parse_class_path (dot :: acc) (trivia_after_dot @ trivia_acc)
        else (List.rev acc, List.rev trivia_acc)
    | _ -> (List.rev acc, List.rev trivia_acc)
  in

  let class_path, class_path_trivia = parse_class_path [] [] in
  let children =
    leading_trivia @ [ new_kw ] @ trivia_after_new @ class_path
    @ class_path_trivia
  in
  Some (make_node_list ~kind:Syntax_kind.NEW_EXPR children)

and parse_object_update_expr parser leading_trivia =
  let before_trivia, open_brace = consume parser in
  let trivia_after_open = consume_trivia parser in
  let before_trivia, lt = consume parser in
  (* Consume < *)
  let trivia_after_lt = consume_trivia parser in

  (* Parse field updates: field = value; ... until > *)
  let rec parse_updates acc trivia_acc =
    if at parser Token.Gt then (List.rev acc, List.rev trivia_acc)
    else
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let before_trivia, field_name = consume parser in
          let trivia_after_field = consume_trivia parser in
          let eq_and_value, eq_trivia =
            if at parser Token.Eq then
              let before_trivia, eq = consume parser in
              let trivia_after_eq = consume_trivia parser in
              (* Parse with min_bp=4 to avoid consuming > as infix operator *)
              match parse_expr_bp parser 4 with
              | Some value ->
                  let trivia_after_value = consume_trivia parser in
                  ( [ eq; Ceibo.Green.Node value ],
                    trivia_after_eq @ trivia_after_value )
              | None -> ([ eq ], [])
            else ([], [])
          in
          let semi, semi_trivia =
            if at parser Token.Semi then
              let before_trivia, s = consume parser in
              let trivia_after_semi = consume_trivia parser in
              ([ s ], trivia_after_semi)
            else ([], [])
          in
          let new_acc =
            List.rev_append semi
              (List.rev_append eq_and_value (field_name :: acc))
          in
          let new_trivia_acc =
            semi_trivia @ eq_trivia @ trivia_after_field @ trivia_acc
          in
          parse_updates new_acc new_trivia_acc
      | _ -> (List.rev acc, List.rev trivia_acc)
  in

  let updates, updates_trivia = parse_updates [] [] in
  let before_trivia, gt = expect parser Token.Gt in
  let trivia_after_gt = consume_trivia parser in
  let trivia_before_close, close_brace, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Brace)
  in

  let children =
    leading_trivia @ [ open_brace ] @ trivia_after_open @ [ lt ]
    @ trivia_after_lt @ updates @ updates_trivia @ [ gt ] @ trivia_after_gt
    @ trivia_before_close @ [ close_brace ] @ trivia_after_close
  in
  Some (make_node_list ~kind:Syntax_kind.OBJECT_UPDATE_EXPR children)

and parse_object_expr parser leading_trivia =
  let before_trivia, object_kw = consume parser in
  let trivia_after_object = consume_trivia parser in

  (* Check for optional self parameter: object (self) ... end *)
  let self_param, self_trivia =
    if at parser (Token.OpenDelim Token.Paren) then
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in
      let self_ident, ident_trivia =
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let before_trivia, ident = consume parser in
            let trivia_after_ident = consume_trivia parser in
            ([ ident ], trivia_after_ident)
        | _ -> ([], [])
      in
      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in
      ( [ open_paren ] @ trivia_after_open @ self_ident @ trivia_before_close
        @ [ close_paren ],
        ident_trivia @ trivia_after_close )
    else ([], [])
  in

  (* Parse object items until 'end' keyword *)
  let rec parse_object_items acc trivia_acc =
    if at parser (Token.CloseDelim Token.ObjectEnd) then
      (List.rev acc, List.rev trivia_acc)
    else
      let trivia = consume_trivia parser in
      let trivia_acc = trivia @ trivia_acc in
      match peek_kind parser with
      | Some (Token.CloseDelim Token.ObjectEnd) ->
          (List.rev acc, List.rev trivia_acc)
      | Some (Token.Keyword Keyword.Method) ->
          let method_item = parse_object_method parser in
          parse_object_items (method_item @ acc) trivia_acc
      | Some (Token.Keyword Keyword.Val) ->
          let val_item = parse_object_val parser in
          parse_object_items (val_item @ acc) trivia_acc
      | Some (Token.Keyword Keyword.Inherit) ->
          let inherit_item = parse_object_inherit parser in
          parse_object_items (inherit_item @ acc) trivia_acc
      | Some (Token.Keyword Keyword.Constraint) ->
          let constraint_item = parse_object_constraint parser in
          parse_object_items (constraint_item @ acc) trivia_acc
      | Some (Token.Keyword Keyword.Initializer) ->
          let initializer_item = parse_object_initializer parser in
          parse_object_items (initializer_item @ acc) trivia_acc
      | _ -> (List.rev acc, List.rev trivia_acc)
  in

  let items, items_trivia = parse_object_items [] [] in
  let trivia_before_end, end_kw, trivia_after_end =
    expect_with_trivia parser (Token.CloseDelim Token.ObjectEnd)
  in

  let children =
    leading_trivia @ [ object_kw ] @ trivia_after_object @ self_param
    @ self_trivia @ items @ items_trivia @ trivia_before_end @ [ end_kw ]
    @ trivia_after_end
  in
  Some (make_node_list ~kind:Syntax_kind.OBJECT_EXPR children)

and parse_object_method parser =
  let before_trivia, method_kw = consume parser in
  let trivia_after_method = consume_trivia parser in

  (* Check for private *)
  let private_kw, trivia_after_private =
    if at parser (Token.Keyword Keyword.Private) then
      let before_trivia, priv = consume parser in
      let trivia = consume_trivia parser in
      ([ priv ], trivia)
    else ([], [])
  in

  (* Parse method name *)
  let method_name, trivia_after_name =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let before_trivia, name = consume parser in
        let trivia = consume_trivia parser in
        ([ name ], trivia)
    | _ -> ([], [])
  in

  (* Consume tokens until = or end/next keyword *)
  let rec consume_until_eq acc =
    if
      at parser Token.Eq
      || at parser (Token.CloseDelim Token.ObjectEnd)
      || at parser (Token.Keyword Keyword.Method)
      || at parser (Token.Keyword Keyword.Val)
      || at parser (Token.Keyword Keyword.Inherit)
    then List.rev acc
    else
      let before_trivia, tok = consume parser in
      let trivia_after_tok = consume_trivia parser in
      consume_until_eq (List.rev_append trivia_after_tok (tok :: acc))
  in

  let params = consume_until_eq [] in

  (* Parse = and method body *)
  let eq_and_body =
    if at parser Token.Eq then
      let before_trivia, eq = consume parser in
      let trivia_after_eq = consume_trivia parser in
      match parse_expr parser with
      | Some body -> [ eq ] @ trivia_after_eq @ [ Ceibo.Green.Node body ]
      | None -> [ eq ]
    else []
  in

  [ method_kw ] @ trivia_after_method @ private_kw @ trivia_after_private
  @ method_name @ trivia_after_name @ params @ eq_and_body

and parse_object_val parser =
  let before_trivia, val_kw = consume parser in
  let trivia_after_val = consume_trivia parser in

  (* Check for mutable *)
  let mutable_kw, trivia_after_mutable =
    if at parser (Token.Keyword Keyword.Mutable) then
      let before_trivia, mut = consume parser in
      let trivia = consume_trivia parser in
      ([ mut ], trivia)
    else ([], [])
  in

  (* Parse field name *)
  let field_name, trivia_after_name =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let before_trivia, name = consume parser in
        let trivia = consume_trivia parser in
        ([ name ], trivia)
    | _ -> ([], [])
  in

  (* Parse = and value *)
  let eq_and_value =
    if at parser Token.Eq then
      let before_trivia, eq = consume parser in
      let trivia_after_eq = consume_trivia parser in
      match parse_expr parser with
      | Some value -> [ eq ] @ trivia_after_eq @ [ Ceibo.Green.Node value ]
      | None -> [ eq ]
    else []
  in

  [ val_kw ] @ trivia_after_val @ mutable_kw @ trivia_after_mutable @ field_name
  @ trivia_after_name @ eq_and_value

and parse_object_inherit parser =
  let before_trivia, inherit_kw = consume parser in
  let trivia_after_inherit = consume_trivia parser in

  (* Parse class expression (simplified - just consume until end/next keyword) *)
  let rec consume_class_expr acc =
    if
      at parser (Token.CloseDelim Token.ObjectEnd)
      || at parser (Token.Keyword Keyword.Method)
      || at parser (Token.Keyword Keyword.Val)
      || at parser (Token.Keyword Keyword.Inherit)
      || at parser (Token.Keyword Keyword.Constraint)
    then List.rev acc
    else
      match parse_expr_bp parser 0 with
      | Some expr ->
          let trivia_after_expr = consume_trivia parser in
          List.rev
            (List.rev_append trivia_after_expr (Ceibo.Green.Node expr :: acc))
      | None -> List.rev acc
  in

  let class_expr = consume_class_expr [] in
  [ inherit_kw ] @ trivia_after_inherit @ class_expr

and parse_object_constraint parser =
  let before_trivia, constraint_kw = consume parser in
  let trivia_after_constraint = consume_trivia parser in

  (* Consume tokens until end/next keyword *)
  let rec consume_until_next acc =
    if
      at parser (Token.CloseDelim Token.ObjectEnd)
      || at parser (Token.Keyword Keyword.Method)
      || at parser (Token.Keyword Keyword.Val)
      || at parser (Token.Keyword Keyword.Inherit)
      || at parser (Token.Keyword Keyword.Constraint)
    then List.rev acc
    else
      let before_trivia, tok = consume parser in
      let trivia_after_tok = consume_trivia parser in
      consume_until_next (List.rev_append trivia_after_tok (tok :: acc))
  in

  let constraint_tokens = consume_until_next [] in
  [ constraint_kw ] @ trivia_after_constraint @ constraint_tokens

and parse_object_initializer parser =
  let before_trivia, initializer_kw = consume parser in
  let trivia_after_initializer = consume_trivia parser in

  (* Parse initializer expression *)
  let init_expr =
    match parse_expr parser with
    | Some expr -> [ Ceibo.Green.Node expr ]
    | None -> []
  in

  [ initializer_kw ] @ trivia_after_initializer @ init_expr

and parse_let_expr parser leading_trivia =
  (* Parse let expression with pattern destructuring support *)
  let before_trivia, let_kw = consume parser in
  let trivia_after_let = consume_trivia parser in

  (* Check for binding operator: let*, let+, etc. *)
  let is_binding_op =
    match peek_kind parser with
    | Some
        ( Token.Star | Token.Plus | Token.Minus | Token.Ampersand | Token.Pipe
        | Token.Dollar | Token.Percent | Token.At | Token.Eq ) ->
        true
    | _ -> false
  in

  if is_binding_op then
    parse_binding_operator_expr parser leading_trivia let_kw trivia_after_let
    (* Check for 'let open' or 'let module' *)
  else if at parser (Token.Keyword Keyword.Open) then
    parse_let_open_expr parser leading_trivia let_kw trivia_after_let ()
  else if at parser (Token.Keyword Keyword.Module) then
    parse_let_module_expr parser leading_trivia let_kw trivia_after_let ()
  else if at parser (Token.Keyword Keyword.Exception) then
    parse_let_exception_expr parser leading_trivia let_kw trivia_after_let ()
  else parse_regular_let_expr parser leading_trivia let_kw trivia_after_let

and parse_let_open_expr parser leading_trivia let_kw trivia_after_let
    ?(attributes = []) () =
  (* let open Module in expr *)
  let before_trivia, open_kw = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Parse module path *)
  let module_path = parse_identifier parser in
  let trivia_after_path = consume_trivia parser in

  (* Expect 'in' *)
  let before_trivia, in_kw = expect parser (Token.Keyword Keyword.In) in
  let trivia_after_in = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        leading_trivia @ [ let_kw ] @ trivia_after_let @ attributes
        @ [ open_kw ] @ trivia_after_open @ module_path @ trivia_after_path
        @ [ in_kw ] @ trivia_after_in @ [ Ceibo.Green.Node body ]
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_let_module_expr parser leading_trivia let_kw trivia_after_let
    ?(attributes = []) () =
  (* let module M = (val m : S) in expr *)
  let before_trivia, module_kw = consume parser in
  let trivia_after_module = consume_trivia parser in

  (* Parse module name *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Expect '=' *)
  let before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

  (* Parse module expression: (val expr : ModType) or other module expression *)
  let module_expr =
    if at parser (Token.OpenDelim Token.Paren) then
      (* Could be (val expr : ModType) *)
      let before_trivia, open_paren = consume parser in
      let trivia_after_paren = consume_trivia parser in

      if at parser (Token.Keyword Keyword.Val) then
        (* (val expr : ModType) - unpack first-class module *)
        let before_trivia, val_kw = consume parser in
        let trivia_after_val = consume_trivia parser in

        (* Parse expression (module value) - use parse_expr for proper parsing *)
        let expr_result = parse_expr parser in
        let trivia_after_expr = consume_trivia parser in

        (* Expect : *)
        let before_trivia, colon = expect parser Token.Colon in
        let trivia_after_colon = consume_trivia parser in

        (* Parse module type expression *)
        let module_type = parse_module_type_expr parser in
        let trivia_after_type = consume_trivia parser in

        (* Expect ) *)
        let trivia_before_close, close_paren, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Paren)
        in

        let expr_nodes =
          match expr_result with
          | Some expr -> [ Ceibo.Green.Node expr ]
          | None -> []
        in

        [ open_paren ] @ trivia_after_paren @ [ val_kw ] @ trivia_after_val
        @ expr_nodes @ trivia_after_expr @ [ colon ] @ trivia_after_colon
        @ [ Ceibo.Green.Node module_type ]
        @ trivia_after_type @ trivia_before_close @ [ close_paren ]
        @ trivia_after_close
      else
        (* Other parenthesized module expression - consume until 'in' *)
        let rec consume_until_in acc =
          if at parser (Token.Keyword Keyword.In) || peek parser = None then
            List.rev acc
          else
            let before_trivia, tok = consume parser in
            let trivia_after_tok = consume_trivia parser in
            consume_until_in (List.rev_append trivia_after_tok (tok :: acc))
        in
        [ open_paren ] @ trivia_after_paren @ consume_until_in []
    else
      (* Module path or other expression - consume tokens until 'in' *)
      let rec consume_until_in acc =
        if at parser (Token.Keyword Keyword.In) || peek parser = None then
          List.rev acc
        else
          let before_trivia, tok = consume parser in
          let trivia_after_tok = consume_trivia parser in
          consume_until_in (List.rev_append trivia_after_tok (tok :: acc))
      in
      consume_until_in []
  in

  let trivia_before_in = consume_trivia parser in

  (* Expect 'in' *)
  let before_trivia, in_kw = expect parser (Token.Keyword Keyword.In) in
  let trivia_after_in = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        leading_trivia @ [ let_kw ] @ trivia_after_let @ attributes
        @ [ module_kw ] @ trivia_after_module @ [ name ] @ trivia_after_name
        @ [ eq ] @ trivia_after_eq @ module_expr @ trivia_before_in @ [ in_kw ]
        @ trivia_after_in @ [ Ceibo.Green.Node body ]
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_let_exception_expr parser leading_trivia let_kw trivia_after_let
    ?(attributes = []) () =
  (* let exception E of type in expr *)
  let before_trivia, exception_kw = consume parser in
  let trivia_after_exception = consume_trivia parser in

  (* Parse exception constructor name *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Parse optional 'of type' clause - consume tokens until 'in' *)
  let rec consume_until_in acc trivia_acc =
    if at parser (Token.Keyword Keyword.In) || peek parser = None then
      (List.rev acc, List.rev trivia_acc)
    else
      let before_trivia, tok = consume parser in
      let trivia = consume_trivia parser in
      consume_until_in (tok :: acc) (trivia @ trivia_acc)
  in
  let type_tokens, type_trivia = consume_until_in [] [] in

  let trivia_before_in = consume_trivia parser in

  (* Expect 'in' *)
  let before_trivia, in_kw = expect parser (Token.Keyword Keyword.In) in
  let trivia_after_in = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        leading_trivia @ [ let_kw ] @ trivia_after_let @ attributes
        @ [ exception_kw ] @ trivia_after_exception @ [ name ]
        @ trivia_after_name @ type_tokens @ type_trivia @ trivia_before_in
        @ [ in_kw ] @ trivia_after_in @ [ Ceibo.Green.Node body ]
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_binding_operator_expr parser leading_trivia let_kw trivia_after_let =
  (* Binding operator: let* pattern = expr in body *)
  (* The 'let' keyword has already been consumed *)

  (* Consume the operator symbol: *, +, -, etc. *)
  let before_trivia, op_token = consume parser in
  let trivia_after_op = consume_trivia parser in

  (* Parse pattern *)
  let pattern =
    match parse_pattern parser with
    | Some pat -> Ceibo.Green.Node pat
    | None ->
        (* Report error and create placeholder *)
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"pattern" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.IDENT_EXPR ~text:"_"
             ~width:1)
  in
  let trivia_after_pattern = consume_trivia parser in

  (* Expect '=' *)
  let before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

  (* Parse the RHS expression *)
  let rhs_expr =
    match parse_expr parser with
    | Some expr -> Ceibo.Green.Node expr
    | None ->
        let span =
          match peek parser with
          | Some tok -> tok.Token.span
          | None -> Ceibo.Span.make ~start:0 ~end_:0
        in
        report_error parser
          (Diagnostic.make_missing_token ~expected:"expression" ~span);
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.UNIT_LITERAL ~text:"()"
             ~width:2)
  in
  let trivia_after_rhs = consume_trivia parser in

  (* Check for 'and<op>' to parse additional bindings *)
  let rec parse_and_bindings acc =
    if at parser (Token.Keyword Keyword.And) then
      (* Check if next token is an operator *)
      match peek_nth parser 1 with
      | Some
          ( Token.Star | Token.Plus | Token.Minus | Token.Ampersand | Token.Pipe
          | Token.Dollar | Token.Percent | Token.At | Token.Eq ) ->
          let before_trivia, and_kw = consume parser in
          let trivia_after_and = consume_trivia parser in
          let before_trivia, and_op = consume parser in
          let trivia_after_and_op = consume_trivia parser in

          (* Parse pattern *)
          let and_pattern_before_trivia, and_pattern =
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
                let missing_tok =
                  Ceibo.Green.Token
                    (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                       ~width:0)
                in
                ([], missing_tok)
          in
          let trivia_after_and_pattern = consume_trivia parser in

          (* Expect '=' *)
          let before_trivia, and_eq = expect parser Token.Eq in
          let trivia_after_and_eq = consume_trivia parser in

          (* Parse expression *)
          let and_expr =
            match parse_expr parser with
            | Some expr -> Ceibo.Green.Node expr
            | None ->
                Ceibo.Green.Token
                  (Ceibo.Green.make_token ~kind:Syntax_kind.UNIT_LITERAL
                     ~text:"()" ~width:2)
          in
          let trivia_after_and_expr = consume_trivia parser in

          parse_and_bindings
            (List.rev_append trivia_after_and_expr
               (and_expr
               :: List.rev_append trivia_after_and_eq
                    (and_eq
                    :: List.rev_append trivia_after_and_pattern
                         (and_pattern
                         :: List.rev_append trivia_after_and_op
                              (and_op
                              :: List.rev_append trivia_after_and (and_kw :: acc)
                              )))))
      | _ -> List.rev acc
    else List.rev acc
  in
  let and_bindings = parse_and_bindings [] in

  (* Expect 'in' *)
  let before_trivia, in_kw = expect parser (Token.Keyword Keyword.In) in
  let trivia_after_in = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        leading_trivia @ [ let_kw ] @ trivia_after_let @ [ op_token ]
        @ trivia_after_op @ [ pattern ] @ trivia_after_pattern @ [ eq ]
        @ trivia_after_eq @ [ rhs_expr ] @ trivia_after_rhs @ and_bindings
        @ [ in_kw ] @ trivia_after_in @ [ Ceibo.Green.Node body ]
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_regular_let_expr parser leading_trivia let_kw trivia_after_let =
  (* Regular let binding *)

  (* Check for 'rec' *)
  let is_rec = at parser (Token.Keyword Keyword.Rec) in
  let rec_kw, trivia_after_rec =
    if is_rec then
      let before_trivia, kw = consume parser in
      let trivia = consume_trivia parser in
      (Some kw, trivia)
    else (None, [])
  in

  (* Parse pattern - use parse_pattern to handle all pattern types including underscore *)
  let pattern_leading_trivia =
    if is_rec then trivia_after_rec else trivia_after_let
  in
  let pattern =
    match
      parse_pattern ~leading_trivia:(Some pattern_leading_trivia) parser
    with
    | Some first_pat -> (
        let trivia_after_first_pat = consume_trivia parser in
        (* Check if followed by comma (tuple pattern) *)
        if at parser Token.Comma then
          (* Tuple pattern *)
          let rec parse_tuple_patterns acc =
            if not (at parser Token.Comma) then List.rev acc
            else
              let before_trivia, comma = consume parser in
              let trivia_after_comma = consume_trivia parser in
              match parse_pattern parser with
              | Some pat ->
                  let trivia = consume_trivia parser in
                  parse_tuple_patterns
                    (List.rev_append trivia
                       ((Ceibo.Green.Node pat :: trivia_after_comma)
                       @ [ comma ] @ acc))
              | None -> List.rev acc
          in
          let patterns = parse_tuple_patterns [ Ceibo.Green.Node first_pat ] in
          Ceibo.Green.Node
            (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
        else
          (* Unwrap simple IDENT_PATTERN to keep just the token for backward compatibility *)
          match first_pat with
          | node
            when Ceibo.Green.kind (Ceibo.Green.Node node)
                 = Syntax_kind.IDENT_PATTERN -> (
              match Ceibo.Green.children node with
              | [| Ceibo.Green.Token tok |] -> Ceibo.Green.Token tok
              | _ -> Ceibo.Green.Node first_pat)
          | _ -> Ceibo.Green.Node first_pat)
    | None ->
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

  let trivia_after_pattern = consume_trivia parser in

  (* Check if pattern is a simple identifier (function name) *)
  let is_simple_ident =
    match pattern with
    | Ceibo.Green.Token _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_EXPR
    | Ceibo.Green.Node _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_PATTERN
  in

  (* Check for optional type annotation: let f : int -> int = ... *)
  let type_annotation =
    if is_simple_ident && at parser Token.Colon then
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let before_trivia, tok = consume parser in
          let trivia_after_tok = consume_trivia parser in
          consume_type_tokens (List.rev_append trivia_after_tok (tok :: acc))
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ trivia_after_colon @ type_tokens)
    else None
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
                  let trivia_after_param = consume_trivia parser in
                  loop
                    (List.rev_append trivia_after_param
                       (Ceibo.Green.Node param :: acc))
              | None -> List.rev acc)
          | Some (Token.Ident _) -> (
              (* Check if this identifier is followed by tokens that suggest it's
                  an expression (function application) rather than a parameter *)
              (* Exception: Module.{...} and Module.(...) are parameters (local open) *)
              let is_local_open_param =
                match peek_nth parser 1 with
                | Some Token.Dot -> (
                    match peek_nth parser 2 with
                    | Some (Token.OpenDelim Token.Paren)
                    | Some (Token.OpenDelim Token.Brace) ->
                        true
                    | _ -> false)
                | _ -> false
              in
              let looks_like_application =
                if is_local_open_param then false
                else
                  match peek_nth parser 1 with
                  | Some (Token.OpenDelim Token.Paren)
                  | Some (Token.OpenDelim Token.Bracket)
                  | Some (Token.OpenDelim Token.Brace)
                  | Some Token.Dot ->
                      true
                  | _ -> false
              in
              if looks_like_application then
                (* This is likely the start of the function body, stop parsing params *)
                List.rev acc
              else
                (* Parse as parameter *)
                match parse_pattern parser with
                | Some pat ->
                    let trivia_after_pat = consume_trivia parser in
                    loop
                      (List.rev_append trivia_after_pat
                         (Ceibo.Green.Node pat :: acc))
                | None -> List.rev acc)
          | Some (Token.OpenDelim Token.Paren)
          | Some Token.Underscore
          | Some (Token.Literal _)
          | Some (Token.OpenDelim Token.Bracket) -> (
              match parse_pattern parser with
              | Some pat ->
                  let trivia_after_pat = consume_trivia parser in
                  loop
                    (List.rev_append trivia_after_pat
                       (Ceibo.Green.Node pat :: acc))
              | None -> List.rev acc)
          | _ -> List.rev acc
      in
      loop []
    else []
  in

  (* Expect '=' *)
  let before_trivia, eq = expect parser Token.Eq in

  let trivia_after_eq = consume_trivia parser in

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

  let trivia_after_value = consume_trivia parser in

  (* Check for 'and' bindings *)
  let rec parse_and_bindings acc =
    if not (at parser (Token.Keyword Keyword.And)) then List.rev acc
    else
      let before_trivia, and_kw = consume parser in
      let trivia_after_and = consume_trivia parser in

      (* Parse pattern *)
      let and_pattern_before_trivia, and_pattern =
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
            let missing_tok =
              Ceibo.Green.Token
                (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                   ~width:0)
            in
            ([], missing_tok)
      in

      let trivia_after_and_pattern = consume_trivia parser in

      (* Check for function parameters before '=' in 'and' binding *)
      let rec parse_and_params acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          match peek_kind parser with
          | Some Token.Tilde | Some Token.Question -> (
              match parse_labeled_or_optional_param parser with
              | Some param ->
                  let trivia_after_param = consume_trivia parser in
                  parse_and_params
                    (List.rev_append trivia_after_param
                       (Ceibo.Green.Node param :: acc))
              | None -> List.rev acc)
          | _ -> (
              match parse_pattern parser with
              | Some pat ->
                  let trivia_after_pat = consume_trivia parser in
                  parse_and_params
                    (List.rev_append trivia_after_pat
                       (Ceibo.Green.Node pat :: acc))
              | None -> List.rev acc)
      in

      let and_params = parse_and_params [] in

      let before_trivia, and_eq = expect parser Token.Eq in
      let trivia_after_and_eq = consume_trivia parser in

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

      let trivia_after_and_value = consume_trivia parser in

      (* Build and_binding: and_kw, and_pattern, and_params..., and_eq, and_value *)
      let binding_parts =
        [ and_kw ] @ trivia_after_and @ and_pattern_before_trivia
        @ [ and_pattern ] @ trivia_after_and_pattern @ and_params
        @ before_trivia @ [ and_eq ] @ trivia_after_and_eq @ [ and_value ]
        @ trivia_after_and_value
      in
      parse_and_bindings (List.rev binding_parts @ acc)
  in

  let and_bindings = parse_and_bindings [] in

  (* Expect 'in' *)
  let before_trivia, in_kw = expect parser (Token.Keyword Keyword.In) in

  let trivia_after_in = consume_trivia parser in

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
      let children =
        [ let_kw ] @ trivia_after_let @ [ kw ] @ [ pattern ]
        @ (match type_annotation with Some t -> t | None -> [])
        @ params @ [ eq ] @ trivia_after_eq @ [ value_expr ]
        @ trivia_after_value @ and_bindings @ [ in_kw ] @ trivia_after_in
        @ [ body_expr ]
      in
      Some (make_node_list ~kind children)
  | None ->
      let children =
        [ let_kw ] @ [ pattern ]
        @ (match type_annotation with Some t -> t | None -> [])
        @ params @ [ eq ] @ trivia_after_eq @ [ value_expr ]
        @ trivia_after_value @ and_bindings @ [ in_kw ] @ trivia_after_in
        @ [ body_expr ]
      in
      Some (make_node_list ~kind children)

and parse_if_expr parser leading_trivia =
  let before_trivia, if_kw = consume parser in
  let trivia_after_if = consume_trivia parser in

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

  let trivia_after_cond = consume_trivia parser in

  (* Expect 'then' *)
  let before_trivia, then_kw = expect parser (Token.Keyword Keyword.Then) in

  let trivia_after_then = consume_trivia parser in

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

  let trivia_after_then_expr = consume_trivia parser in

  (* Check for 'else' *)
  let has_else = at parser (Token.Keyword Keyword.Else) in
  if has_else then
    let before_trivia, else_kw = consume parser in
    let trivia_after_else = consume_trivia parser in

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
         (leading_trivia @ before_trivia @ [ if_kw ] @ trivia_after_if
        @ [ cond ] @ trivia_after_cond @ [ then_kw ] @ trivia_after_then
        @ [ then_expr ] @ trivia_after_then_expr @ [ else_kw ]
        @ trivia_after_else @ [ else_expr ]))
  else
    Some
      (make_node_list ~kind:Syntax_kind.IF_EXPR
         (leading_trivia @ before_trivia @ [ if_kw ] @ trivia_after_if
        @ [ cond ] @ trivia_after_cond @ [ then_kw ] @ trivia_after_then
        @ [ then_expr ] @ trivia_after_then_expr))

and parse_fun_expr parser leading_trivia =
  let before_trivia, fun_kw = consume parser in
  let trivia_after_fun = consume_trivia parser in

  let rec parse_params acc =
    if at parser Token.Arrow || peek parser = None then List.rev acc
    else
      match peek_kind parser with
      | Some Token.Tilde | Some Token.Question -> (
          match parse_labeled_or_optional_param parser with
          | Some param ->
              let trivia_after_param = consume_trivia parser in
              parse_params
                (List.rev_append trivia_after_param
                   (Ceibo.Green.Node param :: acc))
          | None -> List.rev acc)
      | _ -> (
          (* For function parameters, we want trivia as separate tokens between parameters *)
          (* Parse pattern directly without consuming trivia into it *)
          match peek_kind parser with
          | Some Token.Underscore ->
              (* Wildcard pattern - parse directly *)
              let before_trivia, underscore = consume parser in
              let pat =
                make_node_list ~kind:Syntax_kind.WILDCARD_PATTERN [ underscore ]
              in
              let trivia_after_pat = consume_trivia parser in
              parse_params
                (List.rev_append trivia_after_pat
                   ((Ceibo.Green.Node pat :: before_trivia) @ acc))
          | Some (Token.Ident _) ->
              (* Identifier pattern - parse directly *)
              let before_trivia, ident = consume parser in
              let pat =
                make_node_list ~kind:Syntax_kind.IDENT_PATTERN [ ident ]
              in
              let trivia_after_pat = consume_trivia parser in
              parse_params
                (List.rev_append trivia_after_pat
                   ((Ceibo.Green.Node pat :: before_trivia) @ acc))
          | _ -> (
              (* Complex pattern - need to use parse_pattern *)
              let trivia_before_pat = consume_trivia parser in
              match parse_pattern ~leading_trivia:(Some []) parser with
              | Some pat ->
                  let trivia_after_pat = consume_trivia parser in
                  parse_params
                    (List.rev_append trivia_after_pat
                       ((Ceibo.Green.Node pat :: trivia_before_pat) @ acc))
              | None -> List.rev acc))
  in

  let params = parse_params [] in

  let trivia_before_arrow, arrow, trivia_after_arrow =
    expect_with_trivia parser Token.Arrow
  in

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

  let children =
    leading_trivia @ [ fun_kw ] @ trivia_after_fun @ params
    @ trivia_before_arrow @ [ arrow ] @ trivia_after_arrow @ [ body ]
  in

  Some (make_node_list ~kind:Syntax_kind.FUN_EXPR children)

and parse_function_expr parser leading_trivia =
  let before_trivia, function_kw = consume parser in
  let trivia_after_function = consume_trivia parser in

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
          let trivia_after_first_pattern = consume_trivia parser in

          (* Check for tuple pattern (comma) or or-pattern (pipe) *)
          let base_pattern =
            if at parser Token.Comma then
              (* Tuple pattern *)
              let rec parse_tuple_patterns acc =
                if not (at parser Token.Comma) then List.rev acc
                else
                  let before_trivia, comma = consume parser in
                  let trivia_after_comma = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia = consume_trivia parser in
                      parse_tuple_patterns
                        (List.rev_append trivia
                           (Ceibo.Green.Node pat
                           :: List.rev_append trivia_after_comma (comma :: acc)
                           ))
                  | None -> List.rev acc
              in
              let patterns =
                parse_tuple_patterns
                  (trivia_after_first_pattern
                  @ [ Ceibo.Green.Node first_pattern ])
              in
              make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns
            else if at parser Token.Pipe && not (at parser Token.Arrow) then
              (* Or pattern *)
              let rec parse_or_patterns acc =
                if (not (at parser Token.Pipe)) || at_any parser [ Token.Arrow ]
                then List.rev acc
                else
                  let before_trivia, pipe_tok = consume parser in
                  let trivia_after_pipe = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia = consume_trivia parser in
                      parse_or_patterns
                        (List.rev_append trivia
                           (Ceibo.Green.Node pat
                           :: List.rev_append trivia_after_pipe (pipe_tok :: acc)
                           ))
                  | None -> List.rev acc
              in
              let patterns =
                parse_or_patterns
                  (trivia_after_first_pattern
                  @ [ Ceibo.Green.Node first_pattern ])
              in
              make_node_list ~kind:Syntax_kind.OR_PATTERN patterns
            else first_pattern
          in

          (* Check for cons pattern (::) *)
          let trivia_after_base_pattern =
            if base_pattern = first_pattern then trivia_after_first_pattern
            else consume_trivia parser
          in
          let pattern =
            if at parser Token.ColonColon then
              let before_trivia, cons_op = consume parser in
              let trivia_after_cons = consume_trivia parser in

              match parse_pattern parser with
              | Some tail_pat ->
                  Ceibo.Green.Node
                    (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                       (trivia_after_base_pattern
                       @ [ Ceibo.Green.Node base_pattern; cons_op ]
                       @ trivia_after_cons
                       @ [ Ceibo.Green.Node tail_pat ]))
              | None -> Ceibo.Green.Node base_pattern
            else Ceibo.Green.Node base_pattern
          in

          (* Handle guard (when clause) *)
          let trivia_after_pattern =
            if
              pattern = Ceibo.Green.Node base_pattern
              && base_pattern = first_pattern
            then trivia_after_first_pattern
            else consume_trivia parser
          in
          let guard =
            if at parser (Token.Keyword Keyword.When) then
              let before_trivia, when_kw = consume parser in
              let trivia_after_when = consume_trivia parser in

              match parse_expr parser with
              | Some e ->
                  let trivia_after_guard_expr = consume_trivia parser in
                  Some
                    (Ceibo.Green.Node
                       (make_node_list ~kind:Syntax_kind.PATTERN_GUARD
                          (trivia_after_pattern @ [ when_kw ]
                         @ trivia_after_when @ [ Ceibo.Green.Node e ]
                         @ trivia_after_guard_expr)))
              | None -> None
            else None
          in

          let arrow_before_trivia, arrow =
            if at parser Token.Arrow then consume parser
            else
              let span =
                match peek parser with
                | Some tok -> tok.Token.span
                | None -> Ceibo.Span.make ~start:0 ~end_:0
              in
              let err = Diagnostic.make_missing_token ~expected:"'->'" ~span in
              report_error parser err;
              let missing_tok =
                Ceibo.Green.Token
                  (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                     ~width:0)
              in
              ([], missing_tok)
          in
          let trivia_after_arrow = consume_trivia parser in
          match parse_expr parser with
          | Some expr ->
              let case_children =
                match guard with
                | Some g ->
                    [ pattern; g ] @ arrow_before_trivia
                    @ [ arrow; Ceibo.Green.Node expr ]
                | None ->
                    [ pattern ] @ arrow_before_trivia
                    @ [ arrow; Ceibo.Green.Node expr ]
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
          | None -> [])
  in

  let children =
    leading_trivia @ [ function_kw ] @ trivia_after_function @ cases
  in

  Some (make_node_list ~kind:Syntax_kind.FUNCTION_EXPR children)

and parse_pattern ?(leading_trivia = None) parser =
  let leading_trivia =
    match leading_trivia with Some t -> t | None -> consume_trivia parser
  in
  match parse_base_pattern parser leading_trivia with
  | Some pat ->
      if at parser Token.ColonColon then
        (* Cons pattern: a :: b or "x" :: rest *)
        let trivia_after_pat = consume_trivia parser in
        let trivia_before_cons, cons_op = consume parser in
        let trivia_after_cons = consume_trivia parser in
        match parse_pattern parser with
        | Some tail_pat ->
            Some
              (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                 ([ Ceibo.Green.Node pat ] @ trivia_after_pat
                @ trivia_before_cons @ [ cons_op ] @ trivia_after_cons
                 @ [ Ceibo.Green.Node tail_pat ]))
        | None -> Some pat
      else if at parser Token.DotDot then
        (* Range pattern: 'a' .. 'z' or 0 .. 9 *)
        let trivia_after_pat = consume_trivia parser in
        let before_trivia, dotdot = consume parser in
        let trivia_after_dotdot = consume_trivia parser in
        let leading_trivia_for_end = consume_trivia parser in
        match parse_base_pattern parser leading_trivia_for_end with
        | Some end_pat ->
            let trivia_after_end = consume_trivia parser in
            let range_pat =
              make_node_list ~kind:Syntax_kind.RANGE_PATTERN
                (trivia_after_pat
                @ [ Ceibo.Green.Node pat; dotdot ]
                @ trivia_after_dotdot
                @ [ Ceibo.Green.Node end_pat ]
                @ trivia_after_end)
            in
            (* Check for 'as' binding after range pattern *)
            if at parser (Token.Keyword Keyword.As) then
              let before_trivia, as_kw = consume parser in
              let trivia_after_as = consume_trivia parser in
              match peek_kind parser with
              | Some (Token.Ident _) ->
                  let before_trivia, ident = consume parser in
                  Some
                    (make_node_list ~kind:Syntax_kind.AS_PATTERN
                       ([ Ceibo.Green.Node range_pat; as_kw ]
                       @ trivia_after_as @ [ ident ]))
              | _ -> Some range_pat
            else Some range_pat
        | None -> Some pat
      else if at parser Token.Pipe then
        (* OR pattern: A | B | C, where alternatives can be ranges, cons, etc. *)
        let trivia_after_pat = consume_trivia parser in
        let rec collect_or_patterns acc trivia_acc =
          if not (at parser Token.Pipe) then (List.rev acc, List.rev trivia_acc)
          else
            let before_trivia, pipe = consume parser in
            let trivia_after_pipe = consume_trivia parser in
            (* Parse alternative: could be range like '0' .. '9' or cons pattern *)
            let leading_trivia_alt = consume_trivia parser in
            match parse_base_pattern parser leading_trivia_alt with
            | Some p ->
                let trivia_after_p = consume_trivia parser in
                (* Check if this alternative is a range pattern *)
                let alternative =
                  if at parser Token.DotDot then
                    let before_trivia, dotdot = consume parser in
                    let trivia_after_dotdot2 = consume_trivia parser in
                    let leading_trivia_end = consume_trivia parser in
                    match parse_base_pattern parser leading_trivia_end with
                    | Some end_pat ->
                        Ceibo.Green.Node
                          (make_node_list ~kind:Syntax_kind.RANGE_PATTERN
                             ([ Ceibo.Green.Node p; dotdot ]
                             @ trivia_after_dotdot2
                             @ [ Ceibo.Green.Node end_pat ]))
                    | None -> Ceibo.Green.Node p
                  else Ceibo.Green.Node p
                in
                collect_or_patterns
                  (alternative :: pipe :: acc)
                  (trivia_after_p @ trivia_after_pipe @ trivia_acc)
            | None -> (List.rev acc, List.rev trivia_acc)
        in
        let patterns, patterns_trivia =
          collect_or_patterns [ Ceibo.Green.Node pat ] trivia_after_pat
        in
        Some
          (make_node_list ~kind:Syntax_kind.OR_PATTERN
             (patterns_trivia @ patterns))
      else if at parser (Token.Keyword Keyword.As) then
        let trivia_after_pat = consume_trivia parser in
        let before_trivia, as_kw = consume parser in
        let trivia_after_as = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let before_trivia, ident = consume parser in
            Some
              (make_node_list ~kind:Syntax_kind.AS_PATTERN
                 (trivia_after_pat
                 @ [ Ceibo.Green.Node pat; as_kw ]
                 @ trivia_after_as @ [ ident ]))
        | _ -> Some pat
      else Some pat
  | None -> None

and parse_base_pattern parser leading_trivia =
  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let before_trivia, underscore = consume parser in
      Some
        (make_node_list ~kind:Syntax_kind.WILDCARD_PATTERN
           (leading_trivia @ [ underscore ]))
  (* List pattern [] or [a; b; c] *)
  | Some (Token.OpenDelim Token.Bracket) -> parse_list_pattern parser
  (* Array pattern [| |] or [| a; b; c |] *)
  | Some (Token.OpenDelim Token.Array) -> parse_array_pattern parser
  (* Identifier or constructor pattern *)
  | Some (Token.Ident _) ->
      parse_ident_or_constructor_pattern parser leading_trivia
  (* Negative number pattern: -1, -32700, etc *)
  | Some Token.Minus -> (
      let before_trivia, minus = consume parser in
      let trivia_after_minus = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Literal (Token.Int _ | Token.Float _)) ->
          let before_trivia, number = consume parser in
          Some
            (make_node_list ~kind:Syntax_kind.LITERAL_PATTERN
               ([ minus ] @ trivia_after_minus @ [ number ]))
      | _ ->
          (* Not a negative number literal, might be a prefix operator in expression context *)
          (* For now, return None to let error handling kick in *)
          None)
  (* Literal pattern *)
  | Some (Token.Literal _)
  | Some (Token.Keyword Keyword.True)
  | Some (Token.Keyword Keyword.False) -> (
      (* Use the leading_trivia passed in, don't re-consume *)
      match parse_literal parser leading_trivia with
      | Some lit ->
          Some
            (make_node_list ~kind:Syntax_kind.LITERAL_PATTERN
               [ Ceibo.Green.Node lit ])
      | None -> None)
  (* Parenthesized pattern or tuple *)
  | Some (Token.OpenDelim Token.Paren) ->
      parse_paren_pattern parser leading_trivia
  (* Record pattern *)
  | Some (Token.OpenDelim Token.Brace) -> parse_record_pattern parser
  (* Polymorphic variant pattern *)
  | Some Token.Backtick -> parse_poly_variant_pattern parser
  (* Polymorphic variant type pattern: #color *)
  | Some Token.Hash ->
      let before_trivia, hash = consume parser in
      let trivia_after_hash = consume_trivia parser in
      let before_trivia, type_name = consume parser in
      Some
        (make_node_list ~kind:Syntax_kind.POLY_VARIANT_TYPE_PATTERN
           ([ hash ] @ trivia_after_hash @ [ type_name ]))
  (* Exception pattern: exception E or exception E of t *)
  | Some (Token.Keyword Keyword.Exception) -> (
      let before_trivia, exception_kw = consume parser in
      let trivia_after_exception = consume_trivia parser in
      (* Parse the exception constructor pattern *)
      let leading_trivia_pat = consume_trivia parser in
      match parse_base_pattern parser leading_trivia_pat with
      | Some pat ->
          Some
            (make_node_list ~kind:Syntax_kind.EXCEPTION_PATTERN
               ([ exception_kw ] @ trivia_after_exception
              @ [ Ceibo.Green.Node pat ]))
      | None ->
          Some
            (make_node_list ~kind:Syntax_kind.EXCEPTION_PATTERN
               ([ exception_kw ] @ trivia_after_exception)))
  (* Lazy pattern: lazy p *)
  | Some (Token.Keyword Keyword.Lazy) -> (
      let before_trivia, lazy_kw = consume parser in
      let trivia_after_lazy = consume_trivia parser in
      (* Parse the inner pattern *)
      match parse_pattern parser with
      | Some pat ->
          Some
            (make_node_list ~kind:Syntax_kind.LAZY_PATTERN
               ([ lazy_kw ] @ trivia_after_lazy @ [ Ceibo.Green.Node pat ]))
      | None ->
          Some
            (make_node_list ~kind:Syntax_kind.LAZY_PATTERN
               ([ lazy_kw ] @ trivia_after_lazy)))
  | _ -> None

and parse_list_pattern parser =
  let before_trivia, open_bracket = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Bracket) then
    let before_trivia, close_bracket = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      [ open_bracket ] @ comments1 @ [ close_bracket ] @ trivia_after_close
    in
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
        let before_trivia, semi = consume parser in
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

    let trivia_before_close, close_bracket, trivia_after_close =
      expect_with_trivia parser (Token.CloseDelim Token.Bracket)
    in
    let children =
      open_bracket
      :: (patterns @ trivia_before_close @ [ close_bracket ]
        @ trivia_after_close)
    in
    Some (make_node_list ~kind:Syntax_kind.LIST_PATTERN children)

and parse_array_pattern parser =
  let before_trivia, open_array = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Array) then
    let before_trivia, close_array = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      [ open_array ] @ comments1 @ [ close_array ] @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.ARRAY_PATTERN children)
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
        let before_trivia, semi = consume parser in
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

    let trivia_before_close, close_array, trivia_after_close =
      expect_with_trivia parser (Token.CloseDelim Token.Array)
    in
    let children =
      open_array
      :: (patterns @ trivia_before_close @ [ close_array ] @ trivia_after_close)
    in
    Some (make_node_list ~kind:Syntax_kind.ARRAY_PATTERN children)

and parse_ident_or_constructor_pattern parser leading_trivia =
  (* Check for local module open pattern: Module.(pattern) or Module.{fields} *)
  (* Peek ahead to see if we have Ident . ( or Ident . { *)
  let is_local_open_paren =
    match peek_kind parser with
    | Some (Token.Ident _) -> (
        match peek_nth parser 1 with
        | Some Token.Dot -> (
            match peek_nth parser 2 with
            | Some (Token.OpenDelim Token.Paren) -> true
            | _ -> false)
        | _ -> false)
    | _ -> false
  in

  let is_local_open_brace =
    match peek_kind parser with
    | Some (Token.Ident _) -> (
        match peek_nth parser 1 with
        | Some Token.Dot -> (
            match peek_nth parser 2 with
            | Some (Token.OpenDelim Token.Brace) -> true
            | _ -> false)
        | _ -> false)
    | _ -> false
  in

  if is_local_open_paren then
    (* Parse as local open: Module.(pattern) *)
    let before_trivia, module_name = consume parser in
    let trivia_after_module = consume_trivia parser in
    let before_trivia, dot = consume parser in
    let trivia_after_dot = consume_trivia parser in
    let before_trivia, open_paren = consume parser in
    let trivia_after_open = consume_trivia parser in
    match parse_pattern parser with
    | Some pat ->
        let trivia_after_pat = consume_trivia parser in
        let trivia_before_close, close_paren, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Paren)
        in
        Some
          (make_node_list ~kind:Syntax_kind.LOCAL_OPEN_PATTERN
             ([ module_name ] @ trivia_after_module @ [ dot ] @ trivia_after_dot
            @ [ open_paren ] @ trivia_after_open @ [ Ceibo.Green.Node pat ]
            @ trivia_after_pat @ trivia_before_close @ [ close_paren ]
            @ trivia_after_close))
    | None ->
        (* Failed to parse pattern - this is an error *)
        None
  else if is_local_open_brace then
    (* Parse as local open with record: Module.{field1; field2} *)
    let before_trivia, module_name = consume parser in
    let trivia_after_module = consume_trivia parser in
    let before_trivia, dot = consume parser in
    let trivia_after_dot = consume_trivia parser in
    (* Now parse the record pattern *)
    match parse_record_pattern parser with
    | Some record_pat ->
        Some
          (make_node_list ~kind:Syntax_kind.LOCAL_OPEN_PATTERN
             ([ module_name ] @ trivia_after_module @ [ dot ] @ trivia_after_dot
             @ [ Ceibo.Green.Node record_pat ]))
    | None ->
        (* Failed to parse record - this is an error *)
        None
  else
    (* Parse identifier or module path (A.B.C) *)
    let ident_parts = leading_trivia @ parse_identifier parser in

    (* Get last identifier in path to check if it's a constructor *)
    (* Note: ident_parts includes trivia, so we need to find the last actual IDENT token *)
    let last_ident =
      List.fold_left
        (fun acc part ->
          match Ceibo.Green.kind part with
          | Syntax_kind.IDENT_EXPR -> part
          | _ -> acc)
        (List.hd ident_parts) ident_parts
    in
    let is_constructor =
      match Ceibo.Green.text last_ident with
      | Some text when Ceibo.Green.kind last_ident = Syntax_kind.IDENT_EXPR ->
          is_constructor_ident text
      | _ -> false
    in

    if at parser Token.ColonColon then
      let before_trivia, cons_op = consume parser in
      let trivia_after_cons = consume_trivia parser in

      match parse_pattern parser with
      | Some tail_pat ->
          Some
            (make_node_list ~kind:Syntax_kind.CONS_PATTERN
               (ident_parts @ [ cons_op ] @ trivia_after_cons
               @ [ Ceibo.Green.Node tail_pat ]))
      | None ->
          Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN ident_parts)
    else if is_constructor then
      (* Only try to parse as constructor pattern if identifier is uppercase *)
      match peek_kind parser with
      | Some (Token.Ident _)
      | Some (Token.OpenDelim Token.Paren)
      | Some (Token.OpenDelim Token.Brace)
      | Some Token.Underscore
      | Some (Token.Literal _)
      | Some (Token.OpenDelim Token.Bracket)
      | Some (Token.Keyword Keyword.True)
      | Some (Token.Keyword Keyword.False)
      | Some Token.Backtick -> (
          (* Constructor with argument pattern, including polymorphic variants *)
          match parse_pattern parser with
          | Some arg_pat ->
              Some
                (make_node_list ~kind:Syntax_kind.CONSTRUCTOR_PATTERN
                   (ident_parts @ [ Ceibo.Green.Node arg_pat ]))
          | None ->
              Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN ident_parts))
      | _ -> Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN ident_parts)
    else
      (* Lowercase identifier - always treat as simple ident pattern *)
      Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN ident_parts)

and parse_paren_pattern parser leading_trivia =
  let _before_trivia, open_paren = consume parser in
  let trivia_after_open = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Paren) then
    let before_trivia, close_paren = consume parser in
    let trivia_after_close = consume_trivia parser in
    let children =
      leading_trivia @ [ open_paren ] @ trivia_after_open @ [ close_paren ]
      @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.PAREN_PATTERN children)
  else if at parser (Token.Keyword Keyword.Lazy) then
    (* Lazy pattern: (lazy pat) *)
    let before_trivia, lazy_kw = consume parser in
    let trivia_after_lazy = consume_trivia parser in
    match parse_pattern parser with
    | Some pat ->
        let trivia_after_pat = consume_trivia parser in
        let trivia_before_close, close_paren, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Paren)
        in
        let children =
          leading_trivia @ [ open_paren ] @ trivia_after_open @ [ lazy_kw ]
          @ trivia_after_lazy @ [ Ceibo.Green.Node pat ] @ trivia_after_pat
          @ trivia_before_close @ [ close_paren ] @ trivia_after_close
        in
        Some (make_node_list ~kind:Syntax_kind.LAZY_PATTERN children)
    | None ->
        let trivia_before_close, close_paren, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Paren)
        in
        let children =
          leading_trivia @ [ open_paren ] @ trivia_after_open @ [ lazy_kw ]
          @ trivia_after_lazy @ trivia_before_close @ [ close_paren ]
          @ trivia_after_close
        in
        Some (make_node_list ~kind:Syntax_kind.LAZY_PATTERN children)
  else if at parser (Token.Keyword Keyword.Module) then
    (* First-class module pattern: (module M : S) *)
    let before_trivia, module_kw = consume parser in
    let trivia_after_module = consume_trivia parser in

    (* Parse module name *)
    let before_trivia, module_name = consume parser in
    let trivia_after_name = consume_trivia parser in

    (* Check for optional type constraint *)
    let constraint_nodes, constraint_trivia =
      if at parser Token.Colon then
        let before_trivia, colon = consume parser in
        let trivia_after_colon = consume_trivia parser in
        (* Parse module type expression (handles 'with type' constraints) *)
        let module_type = parse_module_type_expr parser in
        let trivia_after_type = consume_trivia parser in
        ( [ colon; Ceibo.Green.Node module_type ],
          trivia_after_colon @ trivia_after_type )
      else ([], [])
    in

    let trivia_before_close, close_paren, trivia_after_close =
      expect_with_trivia parser (Token.CloseDelim Token.Paren)
    in
    let children =
      leading_trivia @ [ open_paren ] @ trivia_after_open @ [ module_kw ]
      @ trivia_after_module @ [ module_name ] @ trivia_after_name
      @ constraint_nodes @ constraint_trivia @ trivia_before_close
      @ [ close_paren ] @ trivia_after_close
    in
    Some (make_node_list ~kind:Syntax_kind.PAREN_PATTERN children)
  else
    (* Check for operator identifier: ( ! ), ( + ), ( := ), ( ~- ), etc. *)
    (* These are valid in let bindings: let ( + ) = ... *)
    (* Operators can be single or multiple tokens: ( + ), ( := ), ( ~- ) *)
    match peek_kind parser with
    | Some tok_kind when is_operator_token tok_kind ->
        (* Collect operator tokens until we hit ) or whitespace + ) *)
        let rec collect_op_tokens last_end acc =
          match peek parser with
          | Some tok when is_operator_token tok.Token.kind ->
              (* Check if this token is adjacent to the previous one (no whitespace) *)
              let is_adjacent = last_end = tok.Token.span.start in
              if last_end >= 0 && not is_adjacent then
                (* Gap found - stop collecting *)
                List.rev acc
              else
                let before_trivia, green_tok = consume parser in
                collect_op_tokens tok.Token.span.end_ (green_tok :: acc)
          | _ -> List.rev acc
        in
        (* Start with -1 to accept the first operator token *)
        let op_tokens = collect_op_tokens (-1) [] in
        let trivia_after_op = consume_trivia parser in
        let trivia_before_close, close_paren, trivia_after_close =
          expect_with_trivia parser (Token.CloseDelim Token.Paren)
        in
        let ident_pat =
          make_node_list ~kind:Syntax_kind.IDENT_PATTERN op_tokens
        in
        Some
          (make_node_list ~kind:Syntax_kind.PAREN_PATTERN
             (leading_trivia @ [ open_paren ] @ trivia_after_open
             @ [ Ceibo.Green.Node ident_pat ]
             @ trivia_after_op @ trivia_before_close @ [ close_paren ]
             @ trivia_after_close))
    | _ -> (
        match parse_pattern parser with
        | Some first_pat ->
            let trivia_after_first = consume_trivia parser in

            if at parser Token.Pipe then
              (* Or-pattern: (A | B) *)
              let rec parse_or_patterns acc trivia_acc =
                if not (at parser Token.Pipe) then
                  (List.rev acc, List.rev trivia_acc)
                else
                  let before_trivia, pipe = consume parser in
                  let trivia_after_pipe = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia_after_pat = consume_trivia parser in
                      parse_or_patterns
                        (Ceibo.Green.Node pat :: pipe :: acc)
                        (trivia_after_pat @ trivia_after_pipe @ trivia_acc)
                  | None -> (List.rev acc, List.rev trivia_acc)
              in
              let patterns, patterns_trivia =
                parse_or_patterns
                  [ Ceibo.Green.Node first_pat ]
                  trivia_after_first
              in
              let trivia_before_close, close_paren, trivia_after_close =
                expect_with_trivia parser (Token.CloseDelim Token.Paren)
              in
              let children =
                leading_trivia @ [ open_paren ] @ trivia_after_open @ patterns
                @ patterns_trivia @ trivia_before_close @ [ close_paren ]
                @ trivia_after_close
              in
              Some (make_node_list ~kind:Syntax_kind.OR_PATTERN children)
            else if at parser Token.Colon then
              (* Type annotation: (p : type) *)
              let before_trivia, colon = consume parser in
              let trivia_after_colon = consume_trivia parser in

              (* Parse type tokens until closing paren, tracking depth for nested parens *)
              let rec consume_type_tokens depth acc trivia_acc =
                if peek parser = None then (List.rev acc, List.rev trivia_acc)
                else if at parser (Token.CloseDelim Token.Paren) && depth = 0
                then (List.rev acc, List.rev trivia_acc)
                else
                  (* Check current token kind before consuming *)
                  let new_depth =
                    match peek_kind parser with
                    | Some (Token.OpenDelim Token.Paren) -> depth + 1
                    | Some (Token.CloseDelim Token.Paren) -> depth - 1
                    | _ -> depth
                  in
                  let before_trivia, tok = consume parser in
                  let trivia_after_tok = consume_trivia parser in
                  consume_type_tokens new_depth (tok :: acc)
                    (trivia_after_tok @ trivia_acc)
              in
              let type_elements, type_trivia = consume_type_tokens 0 [] [] in

              let trivia_before_close, close_paren, trivia_after_close =
                expect_with_trivia parser (Token.CloseDelim Token.Paren)
              in
              let children =
                leading_trivia @ [ open_paren ] @ trivia_after_open
                @ [ Ceibo.Green.Node first_pat ]
                @ trivia_after_first @ [ colon ] @ trivia_after_colon
                @ type_elements @ type_trivia @ trivia_before_close
                @ [ close_paren ] @ trivia_after_close
              in
              Some (make_node_list ~kind:Syntax_kind.TYPED_PATTERN children)
            else if at parser Token.Comma then
              let rec parse_tuple_elements acc =
                if not (at parser Token.Comma) then List.rev acc
                else
                  let before_trivia, comma = consume parser in
                  let trivia_after_comma = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let trivia_after_pat = consume_trivia parser in
                      parse_tuple_elements
                        (List.rev_append trivia_after_pat
                           ((Ceibo.Green.Node pat :: trivia_after_comma)
                           @ [ comma ] @ acc))
                  | None -> List.rev acc
              in

              let elements =
                parse_tuple_elements
                  (trivia_after_first @ [ Ceibo.Green.Node first_pat ])
              in

              let trivia_before_close, close_paren, trivia_after_close =
                expect_with_trivia parser (Token.CloseDelim Token.Paren)
              in
              let children =
                leading_trivia @ [ open_paren ] @ trivia_after_open @ elements
                @ trivia_before_close @ [ close_paren ] @ trivia_after_close
              in
              Some (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN children)
            else
              let trivia_before_close, close_paren, trivia_after_close =
                expect_with_trivia parser (Token.CloseDelim Token.Paren)
              in
              Some
                (make_node_list ~kind:Syntax_kind.PAREN_PATTERN
                   (leading_trivia @ [ open_paren ] @ trivia_after_open
                   @ [ Ceibo.Green.Node first_pat ]
                   @ trivia_after_first @ trivia_before_close @ [ close_paren ]
                   @ trivia_after_close))
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
                      { context = "parenthesized pattern" })
                 ~span))

and parse_match_expr parser leading_trivia =
  let before_trivia, match_kw = consume parser in
  let trivia_after_match = consume_trivia parser in

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

  let trivia_after_scrutinee = consume_trivia parser in

  let before_trivia, with_kw = expect parser (Token.Keyword Keyword.With) in

  let trivia_after_with = consume_trivia parser in

  (* Parse match cases *)
  let first_case, first_trivia =
    if not (at parser Token.Pipe) then
      match parse_match_case_no_pipe parser with
      | Some case ->
          let trivia = consume_trivia parser in
          ([ Ceibo.Green.Node case ], trivia)
      | None -> ([], [])
    else ([], [])
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

  let children =
    leading_trivia @ [ match_kw ] @ trivia_after_match @ [ scrutinee ]
    @ trivia_after_scrutinee @ [ with_kw ] @ trivia_after_with @ first_trivia
    @ all_cases
  in

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
  let before_trivia, pipe = consume parser in
  let trivia_after_pipe = consume_trivia parser in

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
      (* Prepend trivia and pipe token to the match case *)
      let case_children = Ceibo.Green.children case in
      let children_with_pipe =
        before_trivia
        @ (pipe :: trivia_after_pipe)
        @ Array.to_list case_children
      in
      Some (make_node_list ~kind:Syntax_kind.MATCH_CASE children_with_pipe)
  | None -> None

and parse_poly_variant_pattern parser =
  let before_trivia, backtick = consume parser in
  let trivia_after_backtick = consume_trivia parser in

  (* Polymorphic variant tag - can be any identifier (lowercase or uppercase) *)
  match peek_kind parser with
  | Some (Token.Ident _) -> (
      let before_trivia, tag_token = consume parser in
      let trivia_after_tag = consume_trivia parser in
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
                [ backtick ] @ trivia_after_backtick @ [ tag_token ]
                @ trivia_after_tag @ [ Ceibo.Green.Node pat ]
              in
              Some
                (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
          | None ->
              let children =
                [ backtick ] @ trivia_after_backtick @ [ tag_token ]
              in
              Some
                (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
          )
      | _ ->
          let children = [ backtick ] @ trivia_after_backtick @ [ tag_token ] in
          Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children))
  | _ ->
      (* Missing or invalid tag *)
      let children = [ backtick ] in
      Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)

and parse_record_pattern parser =
  let before_trivia, open_brace = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Parse field patterns *)
  let fields = ref [] in

  let rec loop () =
    if at parser (Token.CloseDelim Token.Brace) then ()
    else
      match peek_kind parser with
      | Some Token.Underscore ->
          (* Wildcard pattern to ignore remaining fields: { x; _ } *)
          let before_trivia, wildcard = consume parser in
          fields := wildcard :: !fields;
          let trivia = consume_trivia parser in
          fields := List.rev_append trivia !fields;
          (* Wildcard should be last, but consume semicolon if present *)
          if at parser Token.Semi then (
            let before_trivia, semi = consume parser in
            fields := semi :: !fields;
            let trivia2 = consume_trivia parser in
            fields := List.rev_append trivia2 !fields;
            ())
      | Some (Token.Ident _) ->
          (* Parse field name (can be qualified: Module.field or just field) *)
          let field_name_parts = parse_identifier parser in
          fields := List.rev_append field_name_parts !fields;
          let trivia1 = consume_trivia parser in
          fields := List.rev_append trivia1 !fields;

          (* Check if there's a '=' for field = pattern or just field (punning) *)
          if at parser Token.Eq then (
            let before_trivia, eq = consume parser in
            let trivia2 = consume_trivia parser in
            fields := List.rev_append trivia2 (eq :: !fields);
            match parse_pattern parser with
            | Some pat ->
                fields := Ceibo.Green.Node pat :: !fields;
                let trivia3 = consume_trivia parser in
                fields := List.rev_append trivia3 !fields;
                if at parser Token.Semi then (
                  let before_trivia, semi = consume parser in
                  fields := semi :: !fields;
                  let trivia4 = consume_trivia parser in
                  fields := List.rev_append trivia4 !fields;
                  loop ())
            | None -> ())
          else
            (* Punning: { x } is shorthand for { x = x } *)
            let trivia2 = consume_trivia parser in
            fields := List.rev_append trivia2 !fields;
            if at parser Token.Semi then (
              let before_trivia, semi = consume parser in
              fields := semi :: !fields;
              let trivia3 = consume_trivia parser in
              fields := List.rev_append trivia3 !fields;
              loop ())
      | _ -> ()
  in
  loop ();

  let trivia_before_close, close_brace, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Brace)
  in
  let children =
    [ open_brace ] @ trivia_after_open @ List.rev !fields @ trivia_before_close
    @ [ close_brace ] @ trivia_after_close
  in
  Some (make_node_list ~kind:Syntax_kind.RECORD_PATTERN children)

and parse_match_case_body parser first_pattern =
  let trivia_after_first = consume_trivia parser in

  (* First, parse tuple if we see a comma *)
  let base_pattern =
    if at parser Token.Comma then
      (* Tuple pattern *)
      let rec parse_tuple_patterns acc =
        if not (at parser Token.Comma) then List.rev acc
        else
          let before_trivia, comma = consume parser in
          let trivia_after_comma = consume_trivia parser in
          match parse_pattern parser with
          | Some pat ->
              let trivia = consume_trivia parser in
              parse_tuple_patterns
                (List.rev_append trivia
                   (Ceibo.Green.Node pat
                   :: List.rev_append trivia_after_comma (comma :: acc)))
          | None -> List.rev acc
      in
      let patterns =
        parse_tuple_patterns
          (trivia_after_first @ [ Ceibo.Green.Node first_pattern ])
      in
      Ceibo.Green.Node (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
    else Ceibo.Green.Node first_pattern
  in

  (* Then, check for or-pattern which can combine tuples or other patterns *)
  let trivia_after_base_pattern =
    if base_pattern = Ceibo.Green.Node first_pattern then trivia_after_first
    else consume_trivia parser
  in
  let pattern =
    if at parser Token.Pipe && not (at_any parser [ Token.Arrow ]) then
      (* Or pattern - can combine tuples or other patterns *)
      let rec parse_or_patterns acc =
        if (not (at parser Token.Pipe)) || at_any parser [ Token.Arrow ] then
          List.rev acc
        else
          let before_trivia, pipe_tok = consume parser in
          let trivia_after_pipe = consume_trivia parser in
          (* Parse next pattern which might also be a tuple *)
          match parse_pattern parser with
          | Some next_pat ->
              let trivia_after_next = consume_trivia parser in
              (* Check if this pattern is also a tuple *)
              let next_pattern_with_tuple =
                if at parser Token.Comma then
                  let rec parse_tuple_patterns acc =
                    if not (at parser Token.Comma) then List.rev acc
                    else
                      let before_trivia, comma = consume parser in
                      let trivia_after_comma = consume_trivia parser in
                      match parse_pattern parser with
                      | Some pat ->
                          let trivia = consume_trivia parser in
                          parse_tuple_patterns
                            (List.rev_append trivia
                               (Ceibo.Green.Node pat
                               :: List.rev_append trivia_after_comma
                                    (comma :: acc)))
                      | None -> List.rev acc
                  in
                  let patterns =
                    parse_tuple_patterns
                      (trivia_after_next @ [ Ceibo.Green.Node next_pat ])
                  in
                  Ceibo.Green.Node
                    (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
                else Ceibo.Green.Node next_pat
              in
              let trivia =
                if next_pattern_with_tuple = Ceibo.Green.Node next_pat then
                  trivia_after_next
                else consume_trivia parser
              in
              parse_or_patterns
                (List.rev_append trivia
                   (next_pattern_with_tuple :: pipe_tok :: acc))
          | None -> List.rev acc
      in
      let patterns = parse_or_patterns [ base_pattern ] in
      Ceibo.Green.Node (make_node_list ~kind:Syntax_kind.OR_PATTERN patterns)
    else base_pattern
  in

  (* Check for cons pattern (::) *)
  let pattern, trivia_after_pattern =
    match pattern with
    | Ceibo.Green.Node base_pattern ->
        if at parser Token.ColonColon then
          let trivia_after_or_pattern = consume_trivia parser in
          let before_trivia, cons_op = consume parser in
          let trivia_after_cons = consume_trivia parser in

          match parse_pattern parser with
          | Some tail_pat ->
              let trivia_after_tail = consume_trivia parser in
              ( Ceibo.Green.Node
                  (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                     (trivia_after_or_pattern
                     @ [ Ceibo.Green.Node base_pattern; cons_op ]
                     @ trivia_after_cons
                     @ [ Ceibo.Green.Node tail_pat ])),
                trivia_after_tail )
          | None -> (Ceibo.Green.Node base_pattern, trivia_after_base_pattern)
        else (Ceibo.Green.Node base_pattern, trivia_after_base_pattern)
    | other -> (other, trivia_after_base_pattern)
  in

  let guard =
    if at parser (Token.Keyword Keyword.When) then
      let before_trivia, when_kw = consume parser in
      let trivia_after_when = consume_trivia parser in

      match parse_expr parser with
      | Some e ->
          let trivia_after_guard_expr = consume_trivia parser in
          Some
            (Ceibo.Green.Node
               (make_node_list ~kind:Syntax_kind.PATTERN_GUARD
                  ([ when_kw ] @ trivia_after_when @ [ Ceibo.Green.Node e ]
                 @ trivia_after_guard_expr)))
      | None -> None
    else None
  in

  let trivia_before_arrow, arrow, trivia_after_arrow =
    expect_with_trivia parser Token.Arrow
  in

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

  let trivia_after_expr = consume_trivia parser in

  let children =
    match guard with
    | Some g ->
        [ pattern ] @ trivia_after_pattern @ [ g ] @ trivia_before_arrow
        @ [ arrow ] @ trivia_after_arrow @ [ expr ] @ trivia_after_expr
    | None ->
        [ pattern ] @ trivia_after_pattern @ trivia_before_arrow @ [ arrow ]
        @ trivia_after_arrow @ [ expr ] @ trivia_after_expr
  in

  Some (make_node_list ~kind:Syntax_kind.MATCH_CASE children)

(* ========================================================================= *)
(* TOP-LEVEL *)
(* ========================================================================= *)

let rec parse_structure_item parser =
  (* For .ml files (implementations): let, type, open, external, module, include *)
  (* Consume leading trivia first - but be careful not to lose it if we return None *)
  let leading_trivia = consume_trivia parser in

  match peek_kind parser with
  | Some (Token.Keyword Keyword.Let) -> parse_let_binding parser leading_trivia
  | Some (Token.Keyword Keyword.Type) -> parse_type_decl parser
  | Some (Token.Keyword Keyword.Open) -> parse_open parser
  | Some (Token.Keyword Keyword.External) -> parse_external_decl parser
  | Some (Token.Keyword Keyword.Module) ->
      parse_module_decl_structure parser leading_trivia
  | Some (Token.Keyword Keyword.Include) -> parse_include parser leading_trivia
  | _ ->
      (* Return a dummy node containing the leading trivia so it's not lost *)
      if leading_trivia = [] then None
      else Some (make_node_list ~kind:Syntax_kind.ERROR leading_trivia)

and parse_let_binding parser leading_trivia =
  let before_trivia, let_kw = consume parser in
  let trivia_after_let = consume_trivia parser in

  (* Parse any attributes like [@@@attr] *)
  let rec parse_attributes acc trivia_acc =
    if at parser (Token.OpenDelim Token.Bracket) then
      (* Check for attribute: [@...] or [@@...] *)
      match peek_nth parser 1 with
      | Some Token.At | Some Token.AtAt ->
          let before_trivia, open_bracket = consume parser in
          let trivia1 = consume_trivia parser in
          let before_trivia, at_token = consume parser in
          let trivia2 = consume_trivia parser in

          (* Consume attribute name and any following tokens until ] *)
          let rec consume_attr_tokens acc trivia_acc2 =
            if at parser (Token.CloseDelim Token.Bracket) || peek parser = None
            then (List.rev acc, List.rev trivia_acc2)
            else
              let before_trivia, tok = consume parser in
              let trivia = consume_trivia parser in
              consume_attr_tokens (tok :: acc) (trivia @ trivia_acc2)
          in
          let attr_tokens, attr_trivia = consume_attr_tokens [] [] in
          let trivia_before_close, close_bracket, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Bracket)
          in
          let new_attrs =
            [ open_bracket ] @ trivia1 @ [ at_token ] @ trivia2 @ attr_tokens
            @ attr_trivia @ trivia_before_close @ [ close_bracket ]
            @ trivia_after_close
          in
          parse_attributes (new_attrs @ acc) trivia_acc
      | _ -> (List.rev acc, List.rev trivia_acc)
    else (List.rev acc, List.rev trivia_acc)
  in
  let attributes, attr_trivia = parse_attributes [] [] in

  (* Check for 'let open' or 'let module' at structure level *)
  (* NOTE: 'let exception' is NOT allowed at top level - use 'exception' instead *)
  if at parser (Token.Keyword Keyword.Open) then
    parse_let_open_expr parser leading_trivia let_kw
      (trivia_after_let @ attributes @ attr_trivia)
      ()
  else if at parser (Token.Keyword Keyword.Module) then
    parse_let_module_expr parser leading_trivia let_kw
      (trivia_after_let @ attributes @ attr_trivia)
      ()
  else parse_regular_let_binding parser let_kw trivia_after_let ~attributes ()

and parse_regular_let_binding parser let_kw trivia_after_let ?(attributes = [])
    () =
  (* Check for 'rec' *)
  let is_rec = at parser (Token.Keyword Keyword.Rec) in
  let rec_kw, trivia_after_rec =
    if is_rec then
      let before_trivia, kw = consume parser in
      let trivia = consume_trivia parser in
      (Some kw, trivia)
    else (None, [])
  in

  (* Check for operator name: let ( + ) = ... or let ( let* ) = ... *)
  let pattern =
    if at parser (Token.OpenDelim Token.Paren) then (
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Check for unit pattern: () *)
      if at parser (Token.CloseDelim Token.Paren) then
        let before_trivia, close_paren = consume parser in
        let trivia_after_close = consume_trivia parser in
        Ceibo.Green.Node
          (make_node_list ~kind:Syntax_kind.UNIT_LITERAL
             ([ open_paren ] @ trivia_after_open @ [ close_paren ]
            @ trivia_after_close))
      else
        (* Check if this is a first-class module pattern: (module M) *)
        let is_module_pattern =
          match peek_kind parser with
          | Some (Token.Keyword Keyword.Module) -> true
          | _ -> false
        in

        (* Check if this is an operator name by looking at the content *)
        let is_operator_name =
          match peek_kind parser with
          (* Simple operators: +, -, *, /, etc. *)
          | Some
              ( Token.Plus | Token.Minus | Token.Star | Token.Slash
              | Token.Percent | Token.PlusDot | Token.MinusDot | Token.StarDot
              | Token.SlashDot | Token.StarStar | Token.At | Token.Caret
              | Token.Pipe | Token.Ampersand | Token.Lt | Token.Gt | Token.Bang
              | Token.Question | Token.Tilde | Token.Colon | Token.Dollar
              | Token.Hash | Token.Eq | Token.And | Token.Or ) ->
              true
          (* Compound operators: :=, <-, ->, ::, <>, <=, >=, ==, !=, @@, |>, %>, <% *)
          | Some
              ( Token.ColonEq | Token.LeftArrow | Token.Arrow | Token.ColonColon
              | Token.Ne | Token.LtEq | Token.GtEq | Token.EqEq | Token.BangEq
              | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent
              | Token.FatArrow | Token.DotDot ) ->
              true
          (* Binding operators: let*, and+, etc. *)
          | Some (Token.Keyword (Keyword.Let | Keyword.And)) -> true
          (* Index operators start with dot *)
          | Some Token.Dot -> true
          | _ -> false
        in

        if is_module_pattern then
          (* First-class module pattern: (module M : S) *)
          let before_trivia, module_kw = consume parser in
          let trivia_after_module = consume_trivia parser in
          let before_trivia, module_name = consume parser in
          let trivia_after_name = consume_trivia parser in
          (* Check for optional type constraint *)
          let constraint_nodes =
            if at parser Token.Colon then
              let before_trivia, colon = consume parser in
              let trivia_after_colon = consume_trivia parser in
              (* Consume tokens until closing paren *)
              let rec consume_until_close acc =
                if at parser (Token.CloseDelim Token.Paren) then List.rev acc
                else
                  let before_trivia, tok = consume parser in
                  let trivia_after_tok = consume_trivia parser in
                  consume_until_close
                    (List.rev_append trivia_after_tok (tok :: acc))
              in
              let type_tokens = consume_until_close [] in
              [ colon ] @ trivia_after_colon @ type_tokens
            else []
          in
          let trivia_before_close, close_paren, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Paren)
          in
          let all_tokens =
            [ open_paren ] @ trivia_after_open @ [ module_kw ]
            @ trivia_after_module @ [ module_name ] @ trivia_after_name
            @ constraint_nodes @ trivia_before_close @ [ close_paren ]
            @ trivia_after_close
          in
          Ceibo.Green.Node
            (make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_tokens)
        else if is_operator_name then
          (* Parse as operator identifier - collect all tokens until ) *)
          let rec collect_operator_tokens acc =
            if at parser (Token.CloseDelim Token.Paren) then List.rev acc
            else
              let before_trivia, tok = consume parser in
              let trivia = consume_trivia parser in
              collect_operator_tokens (List.rev_append trivia (tok :: acc))
          in
          let op_tokens = collect_operator_tokens [] in
          let trivia_before_close, close_paren, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Paren)
          in
          let all_tokens =
            [ open_paren ] @ trivia_after_open @ op_tokens @ trivia_before_close
            @ [ close_paren ] @ trivia_after_close
          in
          Ceibo.Green.Token
            (Ceibo.Green.make_token ~kind:Syntax_kind.IDENT_EXPR
               ~text:
                 (String.concat ""
                    (List.filter_map (fun t -> Ceibo.Green.text t) all_tokens))
               ~width:
                 (List.fold_left
                    (fun acc t -> acc + Ceibo.Green.width t)
                    0 all_tokens))
        else
          (* Not an operator - parse the paren as start of a pattern *)
          (* We've already consumed the open paren and trivia, so parse what's inside *)
          match parse_pattern parser with
          | Some inner_pat ->
              let trivia_after_inner = consume_trivia parser in
              (* Check if it's a tuple inside parens: (a, b) *)
              let paren_pattern =
                if at parser Token.Comma then
                  (* Tuple pattern inside parens *)
                  let rec parse_tuple_patterns acc =
                    if not (at parser Token.Comma) then List.rev acc
                    else
                      let before_trivia, comma = consume parser in
                      let trivia_after_comma = consume_trivia parser in
                      match parse_pattern parser with
                      | Some pat ->
                          let trivia = consume_trivia parser in
                          parse_tuple_patterns
                            (List.rev_append trivia
                               (Ceibo.Green.Node pat
                               :: List.rev_append trivia_after_comma
                                    (comma :: acc)))
                      | None -> List.rev acc
                  in
                  let patterns =
                    parse_tuple_patterns
                      (trivia_after_inner @ [ Ceibo.Green.Node inner_pat ])
                  in
                  let trivia_before_close, close_paren, trivia_after_close =
                    expect_with_trivia parser (Token.CloseDelim Token.Paren)
                  in
                  let all_children =
                    [ open_paren ] @ trivia_after_open @ patterns
                    @ trivia_before_close @ [ close_paren ] @ trivia_after_close
                  in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_children
                else if at parser Token.Colon then
                  (* Type annotation: (x : int) *)
                  let before_trivia, colon = consume parser in
                  let trivia_after_colon = consume_trivia parser in
                  (* Collect type tokens until ) *)
                  let rec collect_type_tokens acc =
                    if
                      at parser (Token.CloseDelim Token.Paren)
                      || peek parser = None
                    then List.rev acc
                    else
                      let before_trivia, tok = consume parser in
                      let trivia_after_tok = consume_trivia parser in
                      collect_type_tokens
                        (List.rev_append trivia_after_tok (tok :: acc))
                  in
                  let type_tokens = collect_type_tokens [] in
                  let trivia_before_close, close_paren, trivia_after_close =
                    expect_with_trivia parser (Token.CloseDelim Token.Paren)
                  in
                  let all_children =
                    [ open_paren ] @ trivia_after_open
                    @ [ Ceibo.Green.Node inner_pat ]
                    @ trivia_after_inner @ [ colon ] @ trivia_after_colon
                    @ type_tokens @ trivia_before_close @ [ close_paren ]
                    @ trivia_after_close
                  in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_children
                else
                  (* Simple parenthesized pattern *)
                  let trivia_before_close, close_paren, trivia_after_close =
                    expect_with_trivia parser (Token.CloseDelim Token.Paren)
                  in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN
                    ([ open_paren ] @ trivia_after_open
                    @ [ Ceibo.Green.Node inner_pat ]
                    @ trivia_after_inner @ trivia_before_close @ [ close_paren ]
                    @ trivia_after_close)
              in
              (* Now check if this paren pattern is part of a larger tuple: (a, b), (c, d) *)
              if at parser Token.Comma then
                (* Tuple pattern with paren as first element *)
                let rec parse_tuple_patterns acc =
                  if not (at parser Token.Comma) then List.rev acc
                  else
                    let before_trivia, comma = consume parser in
                    let trivia_after_comma = consume_trivia parser in
                    match parse_pattern parser with
                    | Some pat ->
                        let trivia = consume_trivia parser in
                        parse_tuple_patterns
                          (List.rev_append trivia
                             (Ceibo.Green.Node pat
                             :: List.rev_append trivia_after_comma (comma :: acc)
                             ))
                    | None -> List.rev acc
                in
                let patterns =
                  parse_tuple_patterns [ Ceibo.Green.Node paren_pattern ]
                in
                Ceibo.Green.Node
                  (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
              else Ceibo.Green.Node paren_pattern
          | None ->
              let span =
                match peek parser with
                | Some tok -> tok.Token.span
                | None -> Ceibo.Span.make ~start:0 ~end_:0
              in
              let err =
                Diagnostic.make_missing_token ~expected:"pattern" ~span
              in
              report_error parser err;
              Ceibo.Green.Token
                (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:""
                   ~width:0))
    else
      (* Not starting with paren - use regular pattern parsing *)
      match parse_pattern ~leading_trivia:(Some trivia_after_let) parser with
      | Some first_pat -> (
          let trivia_after_first = consume_trivia parser in
          (* Check if followed by comma (tuple pattern) *)
          if at parser Token.Comma then
            (* Tuple pattern *)
            let rec parse_tuple_patterns acc =
              if not (at parser Token.Comma) then List.rev acc
              else
                let before_trivia, comma = consume parser in
                let trivia_after_comma = consume_trivia parser in
                match parse_pattern parser with
                | Some pat ->
                    let trivia = consume_trivia parser in
                    parse_tuple_patterns
                      (List.rev_append trivia
                         (Ceibo.Green.Node pat
                         :: List.rev_append trivia_after_comma (comma :: acc)))
                | None -> List.rev acc
            in
            let patterns =
              parse_tuple_patterns [ Ceibo.Green.Node first_pat ]
            in
            Ceibo.Green.Node
              (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
          else
            (* Unwrap simple IDENT_PATTERN to keep just the token for backward compatibility *)
            match first_pat with
            | node
              when Ceibo.Green.kind (Ceibo.Green.Node node)
                   = Syntax_kind.IDENT_PATTERN -> (
                match Ceibo.Green.children node with
                | [| Ceibo.Green.Token tok |] -> Ceibo.Green.Token tok
                | _ -> Ceibo.Green.Node first_pat)
            | _ -> Ceibo.Green.Node first_pat)
      | None ->
          let span =
            match peek parser with
            | Some tok -> tok.Token.span
            | None -> Ceibo.Span.make ~start:0 ~end_:0
          in
          let err = Diagnostic.make_missing_token ~expected:"pattern" ~span in
          report_error parser err;
          Ceibo.Green.Token
            (Ceibo.Green.make_token ~kind:Syntax_kind.MISSING ~text:"" ~width:0)
  in

  let trivia_after_pattern = consume_trivia parser in

  (* Check if pattern is a simple identifier (function name) *)
  let is_simple_ident =
    match pattern with
    | Ceibo.Green.Token _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_EXPR
    | Ceibo.Green.Node _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_PATTERN
  in

  (* Check for optional type annotation: let f : int -> int = ... *)
  let type_annotation =
    if is_simple_ident && at parser Token.Colon then
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let before_trivia, tok = consume parser in
          let trivia_after_tok = consume_trivia parser in
          consume_type_tokens (List.rev_append trivia_after_tok (tok :: acc))
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ trivia_after_colon @ type_tokens)
    else None
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
                  let trivia_after_param = consume_trivia parser in
                  loop
                    (List.rev_append trivia_after_param
                       (Ceibo.Green.Node param :: acc))
              | None -> List.rev acc)
          | Some (Token.OpenDelim Token.Paren) -> (
              if
                (* Check if it's (type ...) for locally abstract types *)
                peek_nth parser 1 = Some (Token.Keyword Keyword.Type)
              then
                (* Locally abstract type: (type a) or (type a b c) *)
                let before_trivia, open_paren = consume parser in
                let trivia_after_open = consume_trivia parser in
                let before_trivia, type_kw = consume parser in
                let trivia_after_type = consume_trivia parser in
                (* Collect all type variables until ) *)
                let rec collect_type_vars acc =
                  if at parser (Token.CloseDelim Token.Paren) then List.rev acc
                  else
                    let before_trivia, type_var = consume parser in
                    let trivia_after_var = consume_trivia parser in
                    collect_type_vars
                      (List.rev_append trivia_after_var (type_var :: acc))
                in
                let type_vars = collect_type_vars [] in
                let trivia_before_close, close_paren, trivia_after_close =
                  expect_with_trivia parser (Token.CloseDelim Token.Paren)
                in
                let param =
                  make_node_list ~kind:Syntax_kind.TYPE_PARAM
                    ([ open_paren ] @ trivia_after_open @ [ type_kw ]
                   @ trivia_after_type @ type_vars @ trivia_before_close
                   @ [ close_paren ])
                in
                loop
                  (List.rev_append trivia_after_close
                     (Ceibo.Green.Node param :: acc))
              else
                (* Parse as parameter *)
                match parse_pattern parser with
                | Some pat ->
                    let trivia_after_pat = consume_trivia parser in
                    loop
                      (List.rev_append trivia_after_pat
                         (Ceibo.Green.Node pat :: acc))
                | None -> List.rev acc)
          | Some (Token.Ident _) -> (
              (* Check if this identifier is followed by tokens that suggest it's
                  an expression (function application) rather than a parameter *)
              (* Exception: Module.{...} and Module.(...) are parameters (local open) *)
              let is_local_open_param =
                match peek_nth parser 1 with
                | Some Token.Dot -> (
                    match peek_nth parser 2 with
                    | Some (Token.OpenDelim Token.Paren)
                    | Some (Token.OpenDelim Token.Brace) ->
                        true
                    | _ -> false)
                | _ -> false
              in
              let looks_like_application =
                if is_local_open_param then false
                else
                  match peek_nth parser 1 with
                  | Some (Token.OpenDelim Token.Paren)
                  | Some (Token.OpenDelim Token.Bracket)
                  | Some (Token.OpenDelim Token.Brace)
                  | Some Token.Dot ->
                      true
                  | _ -> false
              in
              if looks_like_application then
                (* This is likely the start of the function body, stop parsing params *)
                List.rev acc
              else
                (* Parse as parameter *)
                match parse_pattern parser with
                | Some pat ->
                    let trivia_after_pat = consume_trivia parser in
                    loop
                      (List.rev_append trivia_after_pat
                         (Ceibo.Green.Node pat :: acc))
                | None -> List.rev acc)
          | Some Token.Underscore
          | Some (Token.Literal _)
          | Some (Token.OpenDelim Token.Bracket)
          | Some (Token.OpenDelim Token.Brace) -> (
              match parse_pattern parser with
              | Some pat ->
                  let trivia_after_pat = consume_trivia parser in
                  loop
                    (List.rev_append trivia_after_pat
                       (Ceibo.Green.Node pat :: acc))
              | None -> List.rev acc)
          | _ -> List.rev acc
      in
      loop []
    else []
  in

  (* Check for return type annotation after parameters: let f (x : int) : int = ... *)
  let return_type_annotation =
    if params <> [] && at parser Token.Colon then
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let before_trivia, tok = consume parser in
          let trivia_after_tok = consume_trivia parser in
          consume_type_tokens (List.rev_append trivia_after_tok (tok :: acc))
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ trivia_after_colon @ type_tokens)
    else None
  in

  (* Consume trivia before '=' *)
  let trivia_before_eq = consume_trivia parser in

  (* Expect '=' *)
  let before_trivia, eq = expect parser Token.Eq in

  let trivia_after_eq = consume_trivia parser in

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
  let trivia_after_expr = consume_trivia parser in

  (* Don't transform - keep the original syntax as-is for lossless CST *)
  let final_expr = expr in

  let type_annot_tokens =
    match type_annotation with Some tokens -> tokens | None -> []
  in

  let return_type_annot_tokens =
    match return_type_annotation with Some tokens -> tokens | None -> []
  in

  (* Check if this is a let expression (has 'in' keyword) *)
  if at parser (Token.Keyword Keyword.In) then
    (* This is a let expression: let x = expr in body *)
    let before_trivia, in_kw = consume parser in
    let trivia_after_in = consume_trivia parser in
    match parse_expr parser with
    | Some body ->
        let kind =
          match rec_kw with
          | Some _ -> Syntax_kind.LET_REC_EXPR
          | None -> Syntax_kind.LET_EXPR
        in
        let children =
          match rec_kw with
          | Some kw ->
              [ let_kw ] @ attributes @ [ kw; pattern ] @ type_annot_tokens
              @ params @ return_type_annot_tokens @ [ eq ] @ trivia_after_eq
              @ [ final_expr; in_kw; Ceibo.Green.Node body ]
          | None ->
              [ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
              @ return_type_annot_tokens @ [ eq ] @ trivia_after_eq
              @ [ final_expr; in_kw; Ceibo.Green.Node body ]
        in
        Some (make_node_list ~kind children)
    | None ->
        (* Missing body expression *)
        let kind =
          match rec_kw with
          | Some _ -> Syntax_kind.LET_REC_EXPR
          | None -> Syntax_kind.LET_EXPR
        in
        let children =
          match rec_kw with
          | Some kw ->
              [ let_kw ] @ attributes @ [ kw; pattern ] @ type_annot_tokens
              @ params @ return_type_annot_tokens @ [ eq ] @ trivia_after_eq
              @ [ final_expr; in_kw ]
          | None ->
              [ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
              @ return_type_annot_tokens @ [ eq ] @ trivia_after_eq
              @ [ final_expr; in_kw ]
        in
        Some (make_node_list ~kind children)
  else
    (* This is a let binding: let x = expr *)
    match rec_kw with
    | Some kw ->
        Some
          (make_node_list ~kind:Syntax_kind.LET_BINDING
             ([ let_kw ] @ attributes @ [ kw; pattern ] @ type_annot_tokens
            @ params @ return_type_annot_tokens @ trivia_before_eq @ [ eq ]
            @ trivia_after_eq @ [ final_expr ] @ trivia_after_expr))
    | None ->
        Some
          (make_node_list ~kind:Syntax_kind.LET_BINDING
             ([ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
            @ return_type_annot_tokens @ trivia_before_eq @ [ eq ]
            @ trivia_after_eq @ [ final_expr ] @ trivia_after_expr))

and parse_type_decl parser =
  (* type 'a t = ... | type t += ... | type t *)
  let before_trivia, type_kw = consume parser in
  let trivia_after_type = consume_trivia parser in

  (* Parse type parameters like 'a, _, or ('a, 'b) *)
  let params = parse_type_params parser in

  (* Parse type name (can be module path like Effect.t or Message.t) *)
  let type_name_parts = parse_type_name parser in

  (* Check what comes after the name: +=, =, or nothing *)
  match peek_kind parser with
  | Some Token.Plus when peek_nth parser 1 = Some Token.Eq ->
      (* Extensible type: type t += A | B *)
      let before_trivia, plus = consume parser in
      let trivia_after_plus = consume_trivia parser in
      let before_trivia, eq = consume parser in
      let trivia_after_eq = consume_trivia parser in

      let type_body = parse_variant_type parser in

      let children =
        match params with
        | Some p ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts @ [ plus ] @ trivia_after_plus @ [ eq ]
            @ trivia_after_eq
            @ [ Ceibo.Green.Node type_body ]
        | None ->
            [ type_kw ] @ trivia_after_type @ type_name_parts @ [ plus ]
            @ trivia_after_plus @ [ eq ] @ trivia_after_eq
            @ [ Ceibo.Green.Node type_body ]
      in

      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)
  | Some Token.Eq ->
      (* Regular type definition: type t = ... *)
      let before_trivia, eq = consume parser in
      let trivia_after_eq = consume_trivia parser in

      (* Check for 'private' keyword *)
      let private_kw, trivia_after_private =
        if at parser (Token.Keyword Keyword.Private) then
          let before_trivia, priv = consume parser in
          let trivia = consume_trivia parser in
          (Some priv, trivia)
        else (None, [])
      in

      (* Check if there's a type body after private, or if it's abstract *)
      let type_body_opt =
        match private_kw with
        | Some _ ->
            (* After 'private', check if there's actually a type body *)
            if can_start_type_body parser then
              Some (parse_type_decl_body parser)
            else None (* Abstract private type: type t = private *)
        | None ->
            (* No 'private', always parse type body *)
            Some (parse_type_decl_body parser)
      in

      let children =
        match (params, private_kw, type_body_opt) with
        | Some p, Some priv, Some type_body ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts @ [ eq ] @ trivia_after_eq @ [ priv ]
            @ trivia_after_private
            @ [ Ceibo.Green.Node type_body ]
        | Some p, Some priv, None ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts @ [ eq ] @ trivia_after_eq @ [ priv ]
            @ trivia_after_private
        | Some p, None, Some type_body ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts @ [ eq ] @ trivia_after_eq
            @ [ Ceibo.Green.Node type_body ]
        | Some p, None, None ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts @ [ eq ] @ trivia_after_eq
        | None, Some priv, Some type_body ->
            [ type_kw ] @ trivia_after_type @ type_name_parts @ [ eq ]
            @ trivia_after_eq @ [ priv ] @ trivia_after_private
            @ [ Ceibo.Green.Node type_body ]
        | None, Some priv, None ->
            [ type_kw ] @ trivia_after_type @ type_name_parts @ [ eq ]
            @ trivia_after_eq @ [ priv ] @ trivia_after_private
        | None, None, Some type_body ->
            [ type_kw ] @ trivia_after_type @ type_name_parts @ [ eq ]
            @ trivia_after_eq
            @ [ Ceibo.Green.Node type_body ]
        | None, None, None ->
            [ type_kw ] @ trivia_after_type @ type_name_parts @ [ eq ]
            @ trivia_after_eq
      in

      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)
  | _ ->
      (* Abstract type (no = present, used in signatures): type t *)
      let children =
        match params with
        | Some p ->
            [ type_kw ] @ trivia_after_type @ [ Ceibo.Green.Node p ]
            @ type_name_parts
        | None -> [ type_kw ] @ trivia_after_type @ type_name_parts
      in
      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)

and parse_type_name parser =
  (* Parse type name, which can be a module path: t or Effect.t or Message.t *)
  parse_identifier parser

and can_start_type_body parser =
  (* Check if the current token can start a type body *)
  match peek_kind parser with
  | Some Token.DotDot -> true (* Extensible variant *)
  | Some (Token.OpenDelim Token.Brace) -> true (* Record *)
  | Some Token.Pipe -> true (* Variant *)
  | Some (Token.Ident tag)
    when String.length tag > 0 && Char.uppercase_ascii tag.[0] = tag.[0] ->
      peek_nth parser 1
      <> Some Token.Dot (* Variant constructor, not Module.path *)
  | Some Token.Quote -> true (* Type variable *)
  | Some (Token.Ident _) -> true (* Type name or type constructor *)
  | Some (Token.OpenDelim Token.Paren) -> true (* Tuple or parenthesized type *)
  | Some (Token.OpenDelim Token.Bracket) -> true (* Polymorphic variant *)
  | _ -> false

and parse_type_decl_body parser =
  (* Determine what kind of type body this is *)
  match peek_kind parser with
  | Some Token.DotDot ->
      (* Extensible variant type: .. *)
      let before_trivia, dotdot = consume parser in
      make_node_list ~kind:Syntax_kind.TYPE_EXTENSIBLE [ dotdot ]
  | Some (Token.OpenDelim Token.Brace) ->
      (* Record type: { field: int } *)
      parse_record_type parser
  | Some (Token.Ident tag)
    when String.length tag > 0
         && Char.uppercase_ascii tag.[0] = tag.[0]
         && peek_nth parser 1 <> Some Token.Dot ->
      (* Variant type: A | B (but not Module.path) *)
      parse_variant_type parser
  | Some Token.Pipe ->
      (* Variant type starting with |: | A | B *)
      parse_variant_type parser
  | _ ->
      (* Type expression (alias): int, 'a, int -> string, Module.t *)
      let leading_trivia = consume_trivia parser in
      parse_type_expr parser leading_trivia

and parse_type_params parser =
  (* Handle 'a or ('a, 'b) or _ or no params *)
  match peek_kind parser with
  | Some Token.Underscore ->
      (* Wildcard type parameter: _ *)
      let before_trivia, underscore = consume parser in
      let trivia_after_underscore = consume_trivia parser in
      Some
        (make_node_list ~kind:Syntax_kind.TYPE_PARAM
           ([ underscore ] @ trivia_after_underscore))
  | Some Token.Plus | Some Token.Minus -> (
      (* Variance annotation: +'a or -'a *)
      let before_trivia, variance = consume parser in
      let trivia_after_variance = consume_trivia parser in
      match peek_kind parser with
      | Some Token.Quote ->
          let before_trivia, quote = consume parser in
          let trivia_after_quote = consume_trivia parser in
          let before_trivia, name = consume parser in
          let trivia_after_name = consume_trivia parser in
          Some
            (make_node_list ~kind:Syntax_kind.TYPE_PARAM
               ([ variance ] @ trivia_after_variance @ [ quote ]
              @ trivia_after_quote @ [ name ] @ trivia_after_name))
      | _ -> None)
  | Some Token.Quote ->
      (* Single type variable: 'a *)
      let before_trivia, quote = consume parser in
      let trivia_after_quote = consume_trivia parser in
      let before_trivia, name = consume parser in
      let trivia_after_name = consume_trivia parser in
      Some
        (make_node_list ~kind:Syntax_kind.TYPE_PARAM
           ([ quote ] @ trivia_after_quote @ [ name ] @ trivia_after_name))
  | Some (Token.OpenDelim Token.Paren) ->
      (* Multiple type variables: ('a, 'b) *)
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      let rec parse_param_list acc =
        match peek_kind parser with
        | Some Token.Plus | Some Token.Minus -> (
            (* Variance annotation: +' or -' *)
            let before_trivia, variance = consume parser in
            let trivia_after_variance = consume_trivia parser in
            match peek_kind parser with
            | Some Token.Quote ->
                let before_trivia, quote = consume parser in
                let trivia_after_quote = consume_trivia parser in
                let before_trivia, name = consume parser in
                let trivia_after_name = consume_trivia parser in
                let param =
                  make_node_list ~kind:Syntax_kind.TYPE_PARAM
                    ([ variance ] @ trivia_after_variance @ [ quote ]
                   @ trivia_after_quote @ [ name ] @ trivia_after_name)
                in

                (* Check for comma *)
                if at parser Token.Comma then
                  let before_trivia, comma = consume parser in
                  let trivia_after_comma = consume_trivia parser in
                  parse_param_list
                    (List.rev_append trivia_after_comma
                       (comma :: Ceibo.Green.Node param :: acc))
                else List.rev (Ceibo.Green.Node param :: acc)
            | _ -> List.rev acc)
        | Some Token.Quote ->
            let before_trivia, quote = consume parser in
            let trivia_after_quote = consume_trivia parser in
            let before_trivia, name = consume parser in
            let trivia_after_name = consume_trivia parser in
            let param =
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                ([ quote ] @ trivia_after_quote @ [ name ] @ trivia_after_name)
            in

            (* Check for comma *)
            if at parser Token.Comma then
              let before_trivia, comma = consume parser in
              let trivia_after_comma = consume_trivia parser in
              parse_param_list
                (List.rev_append trivia_after_comma
                   (comma :: Ceibo.Green.Node param :: acc))
            else List.rev (Ceibo.Green.Node param :: acc)
        | _ -> List.rev acc
      in

      let params = parse_param_list [] in
      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in

      Some
        (make_node_list ~kind:Syntax_kind.TYPE_PARAMS
           ([ open_paren ] @ trivia_after_open @ params @ trivia_before_close
          @ [ close_paren ] @ trivia_after_close))
  | _ -> None

and parse_type_expr parser leading_trivia =
  (* Type expressions with proper precedence:
     - Arrow types: int -> string (right-associative, higher precedence)
     - Tuple types: int * string (left-associative, lower precedence)
     - Atomic types: int, 'a, (int -> string)
  *)
  parse_type_arrow parser leading_trivia

and parse_type_arrow parser leading_trivia =
  (* Parse arrow types (right-associative): int -> string -> bool 
     Also handles labeled/optional params: ?x:int -> string or ~label:int -> string *)

  (* Check for labeled or optional parameter *)
  let left =
    match peek_kind parser with
    | Some Token.Question -> (
        (* Optional parameter: ?label:type *)
        let before_trivia, question = consume parser in
        let trivia_after_question = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let before_trivia, label = consume parser in
            let trivia_after_label = consume_trivia parser in
            if at parser Token.Colon then
              let before_trivia, colon = consume parser in
              let trivia_after_colon = consume_trivia parser in
              let typ = parse_type_tuple parser trivia_after_colon in
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                ([ question ] @ trivia_after_question @ [ label ]
               @ trivia_after_label @ [ colon ] @ [ Ceibo.Green.Node typ ])
            else
              (* Just ?label without type *)
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                ([ question ] @ trivia_after_question @ [ label ]
               @ trivia_after_label)
        | _ ->
            (* Malformed optional param *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAM
              ([ question ] @ trivia_after_question))
    | Some Token.Tilde -> (
        (* Labeled parameter: ~label:type *)
        let before_trivia, tilde = consume parser in
        let trivia_after_tilde = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let before_trivia, label = consume parser in
            let trivia_after_label = consume_trivia parser in
            if at parser Token.Colon then
              let before_trivia, colon = consume parser in
              let trivia_after_colon = consume_trivia parser in
              let typ = parse_type_tuple parser trivia_after_colon in
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                ([ tilde ] @ trivia_after_tilde @ [ label ] @ trivia_after_label
               @ [ colon ] @ [ Ceibo.Green.Node typ ])
            else
              (* Just ~label without type *)
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                ([ tilde ] @ trivia_after_tilde @ [ label ] @ trivia_after_label)
        | _ ->
            (* Malformed labeled param *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAM
              ([ tilde ] @ trivia_after_tilde))
    | _ ->
        (* Regular type *)
        parse_type_tuple parser leading_trivia
  in
  let trivia_after_left = consume_trivia parser in

  (* Check if we have a labeled parameter without tilde: label:type -> ... *)
  let left =
    match peek_kind parser with
    | Some Token.Colon -> (
        (* Check if left is a simple identifier (TYPE_CONSTR with single ident) *)
        let children = Ceibo.Green.children left in
        (* Skip leading trivia to find the actual identifier *)
        let is_trivia_kind kind =
          match kind with
          | Syntax_kind.WHITESPACE | Syntax_kind.COMMENT | Syntax_kind.DOCSTRING
            ->
              true
          | _ -> false
        in
        let rec find_first_non_trivia idx =
          if idx >= Array.length children then None
          else
            let elem = children.(idx) in
            let kind = Ceibo.Green.kind elem in
            if is_trivia_kind kind then find_first_non_trivia (idx + 1)
            else
              match elem with
              | Ceibo.Green.Token _ -> Some (idx, elem)
              | Ceibo.Green.Node _ -> Some (idx, elem)
        in
        match find_first_non_trivia 0 with
        | Some (idx, label_elem) -> (
            match label_elem with
            | Ceibo.Green.Token _ ->
                (* Check if there's only one non-trivia token *)
                let has_more_non_trivia =
                  let rec check i =
                    if i >= Array.length children then false
                    else
                      let kind = Ceibo.Green.kind children.(i) in
                      if is_trivia_kind kind then check (i + 1) else true
                  in
                  check (idx + 1)
                in
                if not has_more_non_trivia then
                  (* It's a simple identifier followed by :, reparse as labeled param *)
                  let before_trivia, colon = consume parser in
                  let trivia_after_colon = consume_trivia parser in
                  let typ = parse_type_tuple parser trivia_after_colon in
                  make_node_list ~kind:Syntax_kind.TYPE_PARAM
                    ([ label_elem ] @ trivia_after_left @ [ colon ]
                   @ [ Ceibo.Green.Node typ ])
                else
                  (* Multiple non-trivia children, not a simple identifier *)
                  left
            | _ ->
                (* Not a token, must be a node - complex type *)
                left)
        | None ->
            (* No non-trivia children found *)
            left)
    | _ -> left
  in
  let trivia_after_left2 = consume_trivia parser in

  match peek_kind parser with
  | Some Token.Arrow ->
      let before_trivia, arrow = consume parser in
      let trivia_after_arrow = consume_trivia parser in
      let right = parse_type_arrow parser trivia_after_arrow in
      (* Right-associative recursion *)
      make_node_list ~kind:Syntax_kind.TYPE_ARROW
        ([ Ceibo.Green.Node left ] @ trivia_after_left2 @ [ arrow ]
       @ [ Ceibo.Green.Node right ])
  | _ -> left

and parse_type_tuple parser leading_trivia =
  (* Parse tuple types (left-associative): int * string * bool *)
  let first = parse_type_atomic parser leading_trivia in
  let trivia_after_first = consume_trivia parser in

  match peek_kind parser with
  | Some Token.Star ->
      let rec collect_tuple_parts acc =
        let trivia_before_star = consume_trivia parser in
        match peek_kind parser with
        | Some Token.Star ->
            let before_trivia, star = consume parser in
            let trivia_after_star = consume_trivia parser in
            let next = parse_type_atomic parser trivia_after_star in
            collect_tuple_parts
              (Ceibo.Green.Node next :: ([ star ] @ trivia_before_star @ acc))
        | _ -> List.rev (trivia_before_star @ acc)
      in
      let before_trivia, star = consume parser in
      let trivia_after_star = consume_trivia parser in
      let second = parse_type_atomic parser trivia_after_star in
      let parts =
        collect_tuple_parts
          ([ Ceibo.Green.Node second ]
          @ [ star ] @ trivia_after_first @ [ Ceibo.Green.Node first ])
      in
      make_node_list ~kind:Syntax_kind.TYPE_TUPLE parts
  | _ -> first

and parse_type_atomic parser leading_trivia =
  (* Parse atomic type expressions with optional type application:
      - Type variables: 'a
      - Type constructors: int, string
      - Type application: 'a list, int option, ('a, 'b) result
      - Parenthesized types: (int -> string)
   *)
  let base_type =
    match peek_kind parser with
    | Some Token.Quote ->
        (* Type variable: 'a *)
        let before_trivia, quote = consume parser in
        let trivia_after_quote = consume_trivia parser in
        let before_trivia, name = consume parser in
        make_node_list ~kind:Syntax_kind.TYPE_VAR
          (leading_trivia @ [ quote ] @ trivia_after_quote @ [ name ])
    | Some Token.Underscore ->
        (* Wildcard type: _ *)
        let before_trivia, underscore = consume parser in
        make_node_list ~kind:Syntax_kind.TYPE_VAR
          (leading_trivia @ [ underscore ])
    | Some (Token.Ident _) ->
        (* Type constructor: int, string, list, or Module.path.t *)
        let path_parts = parse_identifier parser in
        make_node_list ~kind:Syntax_kind.TYPE_CONSTR
          (leading_trivia @ path_parts)
    | Some (Token.OpenDelim Token.Brace) ->
        (* Inline record type: { field: int } *)
        parse_record_type parser
    | Some (Token.OpenDelim Token.Bracket) ->
        (* Polymorphic variant type: [ `A | `B ] *)
        parse_poly_variant_type parser
    | Some (Token.OpenDelim Token.Paren) ->
        (* Could be:
            - First-class module type: (module S) or (module S with type t = int)
            - Parenthesized type: (int -> string)
            - Multiple type args: (int, string) result
         *)
        let before_trivia, open_paren = consume parser in
        let trivia_after_open = consume_trivia parser in

        (* Check for (module ...) first-class module type *)
        if at parser (Token.Keyword Keyword.Module) then
          let before_trivia, module_kw = consume parser in
          let trivia_after_module = consume_trivia parser in

          (* Parse module type expression *)
          let module_type = parse_module_type_expr parser in
          let trivia_before_close, close_paren, trivia_after_close =
            expect_with_trivia parser (Token.CloseDelim Token.Paren)
          in
          make_node_list ~kind:Syntax_kind.TYPE_CONSTR
            (leading_trivia @ [ open_paren ] @ trivia_after_open @ [ module_kw ]
           @ trivia_after_module
            @ [ Ceibo.Green.Node module_type ]
            @ trivia_before_close @ [ close_paren ] @ trivia_after_close)
        else
          let leading_trivia_first = consume_trivia parser in
          let first = parse_type_expr parser leading_trivia_first in
          let trivia_after_first = consume_trivia parser in

          (* Check if this is a tuple of type args (comma follows) *)
          if at parser Token.Comma then
            (* Multiple type arguments: (int, string) *)
            let rec collect_args acc =
              let trivia_before_comma = consume_trivia parser in
              if at parser Token.Comma then
                let before_trivia, comma = consume parser in
                let trivia_after_comma = consume_trivia parser in
                let next = parse_type_expr parser trivia_after_comma in
                collect_args
                  (Ceibo.Green.Node next
                  :: ([ comma ] @ trivia_before_comma @ acc))
              else List.rev (trivia_before_comma @ acc)
            in
            let before_trivia, comma = consume parser in
            let trivia_after_comma = consume_trivia parser in
            let second = parse_type_expr parser trivia_after_comma in
            let args =
              collect_args
                ([ Ceibo.Green.Node second ]
                @ [ comma ] @ trivia_after_first @ [ Ceibo.Green.Node first ])
            in
            let trivia_before_close, close_paren, trivia_after_close =
              expect_with_trivia parser (Token.CloseDelim Token.Paren)
            in
            (* Return the args tuple - will be used for type application *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAMS
              (leading_trivia @ [ open_paren ] @ trivia_after_open @ args
             @ trivia_before_close @ [ close_paren ] @ trivia_after_close)
          else
            (* Single parenthesized type: (int -> string) or ([> t] as 'a) *)
            (* Check for 'as' constraint *)
            let constraint_parts, trivia_after_constraint =
              if at parser (Token.Keyword Keyword.As) then
                let before_trivia, as_kw = consume parser in
                let trivia_after_as = consume_trivia parser in
                (* Parse the constraint type variable *)
                let type_var =
                  if at parser Token.Quote then
                    let before_trivia, quote = consume parser in
                    let trivia_after_quote = consume_trivia parser in
                    let before_trivia, name = consume parser in
                    make_node_list ~kind:Syntax_kind.TYPE_VAR
                      ([ quote ] @ trivia_after_quote @ [ name ])
                  else
                    let before_trivia, name = consume parser in
                    make_node_list ~kind:Syntax_kind.TYPE_VAR [ name ]
                in
                let trivia_after_var = consume_trivia parser in
                ( trivia_after_first @ [ as_kw ] @ trivia_after_as
                  @ [ Ceibo.Green.Node type_var ],
                  trivia_after_var )
              else (trivia_after_first, [])
            in
            let trivia_before_close, close_paren, trivia_after_close =
              expect_with_trivia parser (Token.CloseDelim Token.Paren)
            in
            make_node_list ~kind:Syntax_kind.TYPE_PAREN
              (leading_trivia @ [ open_paren ] @ trivia_after_open
             @ [ Ceibo.Green.Node first ] @ constraint_parts
             @ trivia_after_constraint @ trivia_before_close @ [ close_paren ]
             @ trivia_after_close)
    | _ ->
        (* Error: couldn't parse type *)
        make_node_list ~kind:Syntax_kind.ERROR leading_trivia
  in

  let trivia_after_base = consume_trivia parser in

  (* Check for type application: 'a list, (int, string) result *)
  (* Can be chained: 'a tree list, int option list *)
  (* Also handles module paths: 'a Queue.t, Message.envelope Queue.t *)
  let rec parse_type_applications current_type trivia_before =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        (* Type application: current_type constructor_name *)
        (* This can be a simple constructor (list, option) or module path (Queue.t, Stdlib.List.t) *)
        let constructor_path = parse_identifier parser in
        let trivia_after_constructor = consume_trivia parser in
        let applied_type =
          make_node_list ~kind:Syntax_kind.TYPE_CONSTR
            ([ Ceibo.Green.Node current_type ]
            @ trivia_before @ constructor_path)
        in
        (* Check for more applications *)
        parse_type_applications applied_type trivia_after_constructor
    | _ -> current_type
  in

  parse_type_applications base_type trivia_after_base

and parse_variant_type parser =
  (* Parse variant type: A | B | C of int | D of string * bool *)
  (* Note: leading trivia should already be consumed by caller *)

  (* Optional leading pipe *)
  let leading_pipe, leading_pipe_trivia, trivia_after_pipe =
    if at parser Token.Pipe then
      let before_trivia, tok = consume parser in
      let trivia_after = consume_trivia parser in
      (Some tok, before_trivia, trivia_after)
    else (None, [], [])
  in

  let rec parse_constructors acc =
    match peek_kind parser with
    | Some (Token.Ident tag)
      when String.length tag > 0 && Char.uppercase_ascii tag.[0] = tag.[0] ->
        (* Constructor name *)
        let before_trivia, constructor_name = consume parser in
        let trivia_after_name = consume_trivia parser in

        (* Check for payload: 'of type' or GADT ': type' *)
        let payload =
          if at parser Token.Colon then
            (* GADT syntax: Constructor : type *)
            let before_trivia, colon = consume parser in
            let trivia_after_colon = consume_trivia parser in
            let gadt_type = parse_type_expr parser trivia_after_colon in
            Some (trivia_after_name @ [ colon ] @ [ Ceibo.Green.Node gadt_type ])
          else if at parser (Token.Keyword Keyword.Of) then
            (* Regular syntax: Constructor of type *)
            let before_trivia, of_kw = consume parser in
            let trivia_after_of = consume_trivia parser in
            let payload_type = parse_type_expr parser [] in
            Some
              (trivia_after_name @ [ of_kw ] @ trivia_after_of
              @ [ Ceibo.Green.Node payload_type ])
          else None
        in

        let constructor_parts =
          match payload with
          | Some parts -> constructor_name :: parts
          | None -> [ constructor_name ] @ trivia_after_name
        in

        let constructor =
          make_node_list ~kind:Syntax_kind.TYPE_VARIANT_CONSTR constructor_parts
        in

        let trivia_after_constructor = consume_trivia parser in

        (* Check for more constructors *)
        if at parser Token.Pipe then
          let before_trivia, pipe = consume parser in
          let trivia_after_pipe = consume_trivia parser in
          parse_constructors
            (trivia_after_pipe @ [ pipe ] @ trivia_after_constructor
            @ [ Ceibo.Green.Node constructor ]
            @ acc)
        else
          List.rev
            (trivia_after_constructor @ [ Ceibo.Green.Node constructor ] @ acc)
    | _ -> List.rev acc
  in

  let constructors = parse_constructors [] in
  let all_parts =
    match leading_pipe with
    | Some pipe ->
        leading_pipe_trivia @ [ pipe ] @ trivia_after_pipe @ constructors
    | None -> constructors
  in

  make_node_list ~kind:Syntax_kind.TYPE_CONSTR all_parts

and parse_poly_variant_type parser =
  (* Parse polymorphic variant type: [ `A | `B of int ] or [> `A ] or [< `A ] *)
  let before_trivia, open_bracket = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Check for open [> or closed [< *)
  let variance, variance_trivia, trivia_after_variance =
    if at parser Token.Gt then
      let before_trivia, tok = consume parser in
      let trivia_after = consume_trivia parser in
      (Some tok, before_trivia, trivia_after)
    else if at parser Token.Lt then
      let before_trivia, tok = consume parser in
      let trivia_after = consume_trivia parser in
      (Some tok, before_trivia, trivia_after)
    else (None, [], [])
  in

  let rec parse_variants acc =
    match peek_kind parser with
    | Some Token.Backtick ->
        (* Variant constructor: `A or `B of int *)
        let before_trivia, backtick = consume parser in
        let trivia_after_backtick = consume_trivia parser in

        (* Constructor name *)
        let before_trivia, name = consume parser in
        let trivia_after_name = consume_trivia parser in

        (* Check for payload: of type *)
        let payload =
          if at parser (Token.Keyword Keyword.Of) then
            let before_trivia, of_kw = consume parser in
            let trivia_after_of = consume_trivia parser in
            let payload_type = parse_type_expr parser trivia_after_of in
            Some
              (trivia_after_name @ [ of_kw ] @ [ Ceibo.Green.Node payload_type ])
          else None
        in

        let variant_parts =
          match payload with
          | Some parts ->
              [ backtick ] @ trivia_after_backtick @ [ name ] @ parts
          | None ->
              [ backtick ] @ trivia_after_backtick @ [ name ]
              @ trivia_after_name
        in

        let variant =
          make_node_list ~kind:Syntax_kind.TYPE_VARIANT_CONSTR variant_parts
        in

        let trivia_after_variant = consume_trivia parser in

        (* Check for more variants *)
        if at parser Token.Pipe then
          let before_trivia, pipe = consume parser in
          let trivia_after_pipe = consume_trivia parser in
          parse_variants
            (Ceibo.Green.Node variant
            :: (trivia_after_variant @ [ pipe ] @ trivia_after_pipe @ acc))
        else List.rev (Ceibo.Green.Node variant :: (trivia_after_variant @ acc))
    | Some (Token.Ident _) ->
        (* Type name reference: [> io_error ] means "io_error and possibly more" *)
        let leading_trivia = consume_trivia parser in
        let type_name = parse_type_atomic parser leading_trivia in
        let trivia_after_type = consume_trivia parser in

        (* Check for more types after pipe *)
        if at parser Token.Pipe then
          let before_trivia, pipe = consume parser in
          let trivia_after_pipe = consume_trivia parser in
          parse_variants
            (Ceibo.Green.Node type_name
            :: (trivia_after_type @ [ pipe ] @ trivia_after_pipe @ acc))
        else List.rev (Ceibo.Green.Node type_name :: (trivia_after_type @ acc))
    | Some Token.Pipe ->
        (* Leading pipe *)
        let before_trivia, pipe = consume parser in
        let trivia_after_pipe = consume_trivia parser in
        parse_variants ([ pipe ] @ trivia_after_pipe @ acc)
    | _ -> List.rev acc
  in

  let variants = parse_variants [] in
  let trivia_before_close, close_bracket, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Bracket)
  in

  let children =
    match variance with
    | Some var ->
        [ open_bracket ] @ trivia_after_open @ variance_trivia @ [ var ]
        @ trivia_after_variance @ variants @ trivia_before_close
        @ [ close_bracket ] @ trivia_after_close
    | None ->
        [ open_bracket ] @ trivia_after_open @ variance_trivia
        @ trivia_after_variance @ variants @ trivia_before_close
        @ [ close_bracket ] @ trivia_after_close
  in

  make_node_list ~kind:Syntax_kind.TYPE_POLY_VARIANT children

and parse_record_type parser =
  (* Parse record type: { field1: int; field2: string } *)
  let before_trivia, open_brace = consume parser in
  let trivia_after_open = consume_trivia parser in

  let rec parse_fields acc =
    match peek_kind parser with
    | Some (Token.Keyword Keyword.Mutable) | Some (Token.Ident _) ->
        (* Optional mutable keyword *)
        let mutable_kw, trivia_after_mutable =
          if at parser (Token.Keyword Keyword.Mutable) then
            let before_trivia, kw = consume parser in
            let trivia = consume_trivia parser in
            (Some kw, trivia)
          else (None, [])
        in

        (* Field name *)
        let before_trivia, field_name = consume parser in
        let trivia_after_name = consume_trivia parser in

        (* Expect : *)
        let before_trivia, colon = expect parser Token.Colon in
        let trivia_after_colon = consume_trivia parser in

        (* Check for explicit type quantification: 'a 'b. type
            We need to look ahead to see if type vars are followed by a dot.
            Only consume them if they're quantifiers (followed by dot). *)
        let type_quant_parts, quantifier_dot, trivia_after_dot =
          (* Try to parse type quantifiers by looking ahead *)
          let saved_position = parser.position in
          let temp_vars = ref [] in

          (* Consume type vars *)
          while peek_kind parser = Some Token.Quote do
            let before_trivia, quote = consume parser in
            let trivia_after_quote = consume_trivia parser in
            match peek_kind parser with
            | Some (Token.Ident _) ->
                let before_trivia, var_name = consume parser in
                let trivia_after_var = consume_trivia parser in
                temp_vars :=
                  (trivia_after_var @ [ var_name ] @ trivia_after_quote
                 @ [ quote ])
                  @ !temp_vars
            | _ -> ()
          done;

          (* Check if followed by dot *)
          if !temp_vars <> [] && peek_kind parser = Some Token.Dot then
            (* These are quantifiers! Keep them and consume the dot *)
            let before_trivia, dot = consume parser in
            let trivia = consume_trivia parser in
            (List.rev !temp_vars, Some dot, trivia)
          else (
            (* Not quantifiers - backtrack by resetting position *)
            parser.position <- saved_position;
            ([], None, []))
        in

        (* Parse field type *)
        let field_type = parse_type_expr parser trivia_after_dot in
        let trivia_after_type = consume_trivia parser in

        let field_parts =
          let quant_tokens =
            match quantifier_dot with
            | Some dot -> type_quant_parts @ [ dot ] @ trivia_after_dot
            | None -> []
          in
          let base_parts =
            match mutable_kw with
            | Some kw ->
                [ kw ] @ trivia_after_mutable @ [ field_name ]
                @ trivia_after_name @ [ colon ] @ trivia_after_colon
            | None ->
                [ field_name ] @ trivia_after_name @ [ colon ]
                @ trivia_after_colon
          in
          base_parts @ quant_tokens
          @ [ Ceibo.Green.Node field_type ]
          @ trivia_after_type
        in

        let field =
          make_node_list ~kind:Syntax_kind.TYPE_RECORD_FIELD field_parts
        in

        (* Check for semicolon or more fields *)
        if at parser Token.Semi then
          let before_trivia, semi = consume parser in
          let trivia_after_semi = consume_trivia parser in
          parse_fields
            (trivia_after_semi @ (semi :: Ceibo.Green.Node field :: acc))
        else List.rev (Ceibo.Green.Node field :: acc)
    | _ -> List.rev acc
  in

  let fields = parse_fields [] in
  let trivia_before_trailing = consume_trivia parser in

  (* Optional trailing semicolon *)
  let trailing_semi, trailing_semi_trivia =
    if at parser Token.Semi then
      let before_trivia, tok = consume parser in
      (Some tok, before_trivia)
    else (None, [])
  in
  let trivia_after_trailing = consume_trivia parser in

  let trivia_before_close, close_brace, trivia_after_close =
    expect_with_trivia parser (Token.CloseDelim Token.Brace)
  in

  let all_parts =
    match trailing_semi with
    | Some semi ->
        [ open_brace ] @ trivia_after_open @ fields @ trivia_before_trailing
        @ trailing_semi_trivia @ [ semi ] @ trivia_after_trailing
        @ trivia_before_close @ [ close_brace ] @ trivia_after_close
    | None ->
        [ open_brace ] @ trivia_after_open @ fields @ trivia_before_trailing
        @ trailing_semi_trivia @ trivia_after_trailing @ trivia_before_close
        @ [ close_brace ] @ trivia_after_close
  in

  make_node_list ~kind:Syntax_kind.TYPE_CONSTR all_parts

and parse_open parser =
  let before_trivia, open_kw = consume parser in
  let trivia_after_open = consume_trivia parser in

  (* Parse module path: Unix, Unix.File, A.B.C *)
  let path = parse_identifier parser in

  Some
    (make_node_list ~kind:Syntax_kind.OPEN_STMT
       ([ open_kw ] @ trivia_after_open @ path))

and parse_val_decl parser =
  (* val name : type or val ( op ) : type *)
  let before_trivia, val_kw = consume parser in
  let trivia_after_val = consume_trivia parser in

  (* Parse value name - could be identifier or operator in parentheses *)
  let name_tokens, trivia_after_name =
    if at parser (Token.OpenDelim Token.Paren) then
      (* Operator name: ( op ) *)
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Collect operator tokens until closing paren *)
      let rec collect_op_tokens acc =
        if at parser (Token.CloseDelim Token.Paren) then List.rev acc
        else
          let before_trivia, tok = consume parser in
          let trivia_after_tok = consume_trivia parser in
          collect_op_tokens (trivia_after_tok @ [ tok ] @ acc)
      in
      let op_tokens = collect_op_tokens [] in
      let before_trivia, close_paren = consume parser in
      let trivia_after_close = consume_trivia parser in
      ( [ open_paren ] @ trivia_after_open @ op_tokens @ [ close_paren ],
        trivia_after_close )
    else
      (* Regular identifier *)
      let before_trivia, name = consume parser in
      let trivia = consume_trivia parser in
      ([ name ], trivia)
  in

  (* Expect : *)
  let before_trivia, colon = expect parser Token.Colon in
  let trivia_after_colon = consume_trivia parser in

  (* Parse type expression *)
  let type_expr = parse_type_expr parser trivia_after_colon in

  Some
    (make_node_list ~kind:Syntax_kind.VAL_DECL
       ([ val_kw ] @ trivia_after_val @ name_tokens @ trivia_after_name
      @ [ colon ]
       @ [ Ceibo.Green.Node type_expr ]))

and parse_external_decl parser =
  (* external name : type = "c_name" *)
  let before_trivia, external_kw = consume parser in
  let trivia_after_external = consume_trivia parser in

  (* Parse function name *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Expect : *)
  let before_trivia, colon = expect parser Token.Colon in
  let trivia_after_colon = consume_trivia parser in

  (* Parse type expression *)
  let type_expr = parse_type_expr parser trivia_after_colon in
  let trivia_after_type = consume_trivia parser in

  (* Expect = *)
  let before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

  (* Parse C function names (one or more string literals) *)
  let rec parse_c_names acc =
    match peek_kind parser with
    | Some (Token.Literal (Token.String _)) ->
        let before_trivia, str = consume parser in
        let trivia_after_str = consume_trivia parser in
        parse_c_names (trivia_after_str @ [ str ] @ acc)
    | _ -> List.rev acc
  in

  let c_names = parse_c_names [] in

  Some
    (make_node_list ~kind:Syntax_kind.EXTERNAL_DECL
       ([ external_kw ] @ trivia_after_external @ [ name ] @ trivia_after_name
      @ [ colon ]
       @ [ Ceibo.Green.Node type_expr ]
       @ trivia_after_type @ [ eq ] @ trivia_after_eq @ c_names))

and parse_module_decl_structure parser leading_trivia =
  (* For .ml files: module M = struct ... end  OR  module type S = sig ... end *)
  let before_trivia, module_kw = consume parser in
  let trivia_after_module = consume_trivia parser in

  (* Check if this is a module type declaration *)
  if at parser (Token.Keyword Keyword.Type) then
    parse_module_type_decl parser leading_trivia module_kw trivia_after_module
  else
    (* Regular module declaration: module M = ... OR module M (X : S) = ... *)
    parse_regular_module_decl_structure parser leading_trivia module_kw
      trivia_after_module

and parse_module_decl_signature parser leading_trivia =
  (* For .mli files: module M : sig ... end  OR  module type S = sig ... end *)
  let before_trivia, module_kw = consume parser in
  let trivia_after_module = consume_trivia parser in

  (* Check if this is a module type declaration *)
  if at parser (Token.Keyword Keyword.Type) then
    parse_module_type_decl parser leading_trivia module_kw trivia_after_module
  else
    (* Module signature: module M : S  OR  module M (X : S) : S *)
    parse_regular_module_decl_signature parser leading_trivia module_kw
      trivia_after_module

and parse_module_type_decl parser leading_trivia module_kw trivia_after_module =
  (* module type S = sig ... end *)
  let before_trivia, type_kw = consume parser in
  let trivia_after_type = consume_trivia parser in

  (* Parse module type name *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Expect = *)
  let before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

  (* Parse signature *)
  let signature = parse_signature parser in

  Some
    (make_node_list ~kind:Syntax_kind.MODULE_TYPE_DECL
       (leading_trivia @ [ module_kw ] @ trivia_after_module @ [ type_kw ]
      @ trivia_after_type @ [ name ] @ trivia_after_name @ [ eq ]
      @ trivia_after_eq
       @ [ Ceibo.Green.Node signature ]))

and parse_signature parser =
  (* sig ... end *)
  let before_trivia, sig_kw = consume parser in
  (* We know it's 'sig' from caller context *)
  let trivia_after_sig = consume_trivia parser in

  (* Parse signature items until 'end' *)
  let rec parse_sig_items acc trivia_acc =
    if at parser (Token.CloseDelim Token.SigEnd) then
      (List.rev acc, List.rev trivia_acc)
    else
      match parse_signature_item parser with
      | Some item ->
          let trivia = consume_trivia parser in
          parse_sig_items (Ceibo.Green.Node item :: acc) (trivia @ trivia_acc)
      | None ->
          (* Skip if we can't parse this item *)
          if at parser (Token.CloseDelim Token.SigEnd) || peek parser = None
          then (List.rev acc, List.rev trivia_acc)
          else
            let _ = advance parser in
            parse_sig_items acc trivia_acc
  in

  let items, items_trivia = parse_sig_items [] [] in

  let trivia_before_end = consume_trivia parser in
  let before_trivia, end_kw = consume parser in
  (* Consume 'end' keyword *)

  let children =
    [ sig_kw ] @ trivia_after_sig @ items @ items_trivia @ trivia_before_end
    @ [ end_kw ]
  in
  make_node_list ~kind:Syntax_kind.SIGNATURE children

and parse_regular_module_decl_structure parser leading_trivia module_kw
    trivia_after_module =
  (* For .ml files: module M = ... OR module M (X : S) = ... (functor) *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Check for functor parameters: (X : S) or (X : S with type t = int) *)
  let rec parse_functor_params acc =
    if at parser (Token.OpenDelim Token.Paren) then
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Parse parameter name *)
      let before_trivia, param_name = consume parser in
      let trivia_after_param = consume_trivia parser in

      (* Expect : *)
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in

      (* Parse module type expression (can be S or S with type t = int) *)
      let module_type = parse_module_type_expr parser in
      let trivia_before_close = consume_trivia parser in

      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in

      let param =
        make_node_list ~kind:Syntax_kind.TYPE_PARAM
          ([ open_paren ] @ trivia_after_open @ [ param_name ]
         @ trivia_after_param @ [ colon ] @ trivia_after_colon
          @ [ Ceibo.Green.Node module_type ]
          @ trivia_before_close @ [ close_paren ] @ trivia_after_close)
      in

      parse_functor_params (Ceibo.Green.Node param :: acc)
    else List.rev acc
  in

  let params = parse_functor_params [] in

  (* Check for optional module type constraint: : S or : sig ... end or : S with type t = int *)
  let constraint_opt =
    if at parser Token.Colon then
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in
      (* Parse module type expression (handles signatures, identifiers, and 'with' constraints) *)
      let module_type = parse_module_type_expr parser in
      let trivia_after_type = consume_trivia parser in
      Some
        ([ colon ] @ trivia_after_colon
        @ [ Ceibo.Green.Node module_type ]
        @ trivia_after_type)
    else None
  in

  (* Expect = (always required in .ml files) *)
  let before_trivia, eq = expect parser Token.Eq in
  let trivia_after_eq = consume_trivia parser in

  (* Parse module expression (struct...end, or identifier, or functor application) *)
  let module_expr =
    match peek_kind parser with
    | Some (Token.OpenDelim Token.StructEnd) ->
        (* struct ... end *)
        let before_trivia, struct_kw = consume parser in
        let trivia_after_struct = consume_trivia parser in

        (* Parse structure items until 'end' *)
        let rec parse_struct_items acc =
          if at parser (Token.CloseDelim Token.StructEnd) then List.rev acc
          else
            match parse_structure_item parser with
            | Some item ->
                let trivia_after_item = consume_trivia parser in
                parse_struct_items
                  (Ceibo.Green.Node item :: (trivia_after_item @ acc))
            | None ->
                if
                  at parser (Token.CloseDelim Token.StructEnd)
                  || peek parser = None
                then List.rev acc
                else
                  let _ = advance parser in
                  parse_struct_items acc
        in

        let items = parse_struct_items [] in
        let trivia_before_end = consume_trivia parser in
        let before_trivia, end_kw = consume parser in

        make_node_list ~kind:Syntax_kind.STRUCTURE
          ([ struct_kw ] @ trivia_after_struct @ items @ trivia_before_end
         @ [ end_kw ])
    | _ ->
        (* Module identifier or functor application: M or F(X) *)
        let path = parse_identifier parser in
        make_node_list ~kind:Syntax_kind.IDENT_EXPR path
  in

  let children =
    let base =
      match params with
      | [] ->
          leading_trivia @ [ module_kw ] @ trivia_after_module @ [ name ]
          @ trivia_after_name
      | _ ->
          leading_trivia @ [ module_kw ] @ trivia_after_module @ [ name ]
          @ trivia_after_name @ params
    in
    let with_constraint =
      match constraint_opt with
      | None -> base
      | Some constraint_tokens -> base @ constraint_tokens
    in
    with_constraint @ [ eq ] @ trivia_after_eq
    @ [ Ceibo.Green.Node module_expr ]
  in

  Some (make_node_list ~kind:Syntax_kind.MODULE_DECL children)

and parse_regular_module_decl_signature parser leading_trivia module_kw
    trivia_after_module =
  (* For .mli files: module M : S  OR  module M (X : S) : S *)
  let before_trivia, name = consume parser in
  let trivia_after_name = consume_trivia parser in

  (* Check for functor parameters: (X : S) or (X : S with type t = int) *)
  let rec parse_functor_params acc trivia_acc =
    if at parser (Token.OpenDelim Token.Paren) then
      let before_trivia, open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Parse parameter name *)
      let before_trivia, param_name = consume parser in
      let trivia_after_param = consume_trivia parser in

      (* Expect : *)
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in

      (* Parse module type expression (can be S or S with type t = int) *)
      let module_type = parse_module_type_expr parser in
      let trivia_after_type = consume_trivia parser in

      let trivia_before_close, close_paren, trivia_after_close =
        expect_with_trivia parser (Token.CloseDelim Token.Paren)
      in

      let param =
        make_node_list ~kind:Syntax_kind.TYPE_PARAM
          ([ open_paren ] @ trivia_after_open @ [ param_name ]
         @ trivia_after_param @ [ colon ] @ trivia_after_colon
          @ [ Ceibo.Green.Node module_type ]
          @ trivia_after_type @ trivia_before_close @ [ close_paren ]
          @ trivia_after_close)
      in

      parse_functor_params
        (Ceibo.Green.Node param :: acc)
        (trivia_after_close @ trivia_acc)
    else (List.rev acc, List.rev trivia_acc)
  in

  let params, params_trivia = parse_functor_params [] [] in

  (* In .mli files, module declarations can be:
     - Module type ascription: module M : S  or  module M : sig ... end
     - Module alias: module M = OtherModule
  *)
  let children =
    if at parser Token.Colon then
      (* Module type ascription: : S or : sig ... end *)
      let before_trivia, colon = consume parser in
      let trivia_after_colon = consume_trivia parser in

      (* Parse module type expression (handles signatures, identifiers, and 'with' constraints) *)
      let module_type = parse_module_type_expr parser in
      let trivia_after_type = consume_trivia parser in

      leading_trivia @ [ module_kw ] @ trivia_after_module @ [ name ]
      @ trivia_after_name @ params @ params_trivia @ [ colon ]
      @ trivia_after_colon
      @ [ Ceibo.Green.Node module_type ]
      @ trivia_after_type
    else if at parser Token.Eq then
      (* Module alias: = M *)
      let before_trivia, eq = consume parser in
      let trivia_after_eq = consume_trivia parser in

      (* Parse module path (e.g., Build or Std.Path) - just consume as identifier for now *)
      let before_trivia, module_id = consume parser in
      let trivia_after_id = consume_trivia parser in

      leading_trivia @ [ module_kw ] @ trivia_after_module @ [ name ]
      @ trivia_after_name @ params @ params_trivia @ [ eq ] @ trivia_after_eq
      @ [ module_id ] @ trivia_after_id
    else
      (* Malformed: missing : or = *)
      let before_trivia, missing = expect parser Token.Colon in
      leading_trivia @ [ module_kw ] @ trivia_after_module @ [ name ]
      @ trivia_after_name @ params @ params_trivia @ [ missing ]
  in

  Some (make_node_list ~kind:Syntax_kind.MODULE_DECL children)

and parse_signature_item parser =
  (* Signature items: type, val, external, exception, module, etc. *)
  (* Consume leading trivia first - but be careful not to lose it if we return None *)
  let leading_trivia = consume_trivia parser in

  match peek_kind parser with
  | Some (Token.Keyword Keyword.Type) -> parse_type_decl parser
  | Some (Token.Keyword Keyword.Val) -> parse_val_decl parser
  | Some (Token.Keyword Keyword.External) -> parse_external_decl parser
  | Some (Token.Keyword Keyword.Module) ->
      parse_module_decl_signature parser leading_trivia
  | Some (Token.Keyword Keyword.Open) -> parse_open parser
  | Some (Token.Keyword Keyword.Include) -> parse_include parser leading_trivia
  | _ ->
      (* Return a dummy node containing the leading trivia so it's not lost *)
      if leading_trivia = [] then None
      else Some (make_node_list ~kind:Syntax_kind.ERROR leading_trivia)

and parse_include parser leading_trivia =
  (* include Module  OR  include module type of Module *)
  let before_trivia, include_kw = consume parser in
  let trivia_after_include = consume_trivia parser in

  (* Check if this is 'include module type of' *)
  if at parser (Token.Keyword Keyword.Module) then
    (* Might be 'include module type of' *)
    let before_trivia, module_kw = consume parser in
    let trivia_after_module = consume_trivia parser in

    if at parser (Token.Keyword Keyword.Type) then
      (* It is 'include module type of' *)
      let before_trivia, type_kw = consume parser in
      let trivia_after_type = consume_trivia parser in

      let before_trivia, of_kw = consume parser in
      (* Expect 'of' keyword *)
      let trivia_after_of = consume_trivia parser in

      (* Parse module path after 'of' *)
      let path = parse_identifier parser in
      let children =
        leading_trivia @ [ include_kw ] @ trivia_after_include @ [ module_kw ]
        @ trivia_after_module @ [ type_kw ] @ trivia_after_type @ [ of_kw ]
        @ trivia_after_of @ path
      in
      Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)
    else
      (* Just 'include module' - treat module as start of path *)
      let path = parse_identifier parser in
      let children =
        leading_trivia @ [ include_kw ] @ trivia_after_include @ [ module_kw ]
        @ trivia_after_module @ path
      in
      Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)
  else
    (* Simple include: include Module.Path *)
    let path = parse_identifier parser in
    let children =
      leading_trivia @ [ include_kw ] @ trivia_after_include @ path
    in
    Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)

let parse_implementation parser =
  (* Parse .ml file (implementation) *)
  (* Consume leading trivia at the beginning of the file *)
  let leading_trivia = consume_trivia parser in

  let rec parse_items acc =
    if peek parser = None then List.rev acc
    else
      match parse_structure_item parser with
      | Some item ->
          (* Consume trailing trivia after each structure item *)
          let trivia_after_item = consume_trivia parser in
          parse_items (trivia_after_item @ [ Ceibo.Green.Node item ] @ acc)
      | None ->
          (* If we can't parse an item, consume remaining trivia and return *)
          let remaining_trivia = consume_trivia parser in
          List.rev (remaining_trivia @ acc)
  in

  let items = parse_items [] in

  (* Consume any remaining trailing trivia at end of file *)
  let trailing_trivia = consume_trivia parser in

  (* Consume EOF token if present - NEVER DROP TRIVIA! *)
  let eof_token =
    if at parser Token.EOF then
      let before_trivia, eof = consume parser in
      before_trivia @ [ eof ]
    else []
  in

  make_node_list ~kind:Syntax_kind.SOURCE_FILE
    (leading_trivia @ items @ trailing_trivia @ eof_token)

let parse_interface parser =
  (* Parse .mli file (interface/signature) *)
  (* Consume leading trivia at the beginning of the file *)
  let leading_trivia = consume_trivia parser in

  let rec parse_items acc =
    if peek parser = None then List.rev acc
    else
      match parse_signature_item parser with
      | Some item ->
          (* Consume trailing trivia after each signature item *)
          let trivia_after_item = consume_trivia parser in
          parse_items (trivia_after_item @ [ Ceibo.Green.Node item ] @ acc)
      | None ->
          (* If we can't parse an item, consume remaining trivia and return *)
          let remaining_trivia = consume_trivia parser in
          List.rev (remaining_trivia @ acc)
  in

  let items = parse_items [] in

  (* Consume any remaining trailing trivia at end of file *)
  let trailing_trivia = consume_trivia parser in

  (* Consume EOF token if present - NEVER DROP TRIVIA! *)
  let eof_token =
    if at parser Token.EOF then
      let before_trivia, eof = consume parser in
      before_trivia @ [ eof ]
    else []
  in

  make_node_list ~kind:Syntax_kind.SOURCE_FILE
    (leading_trivia @ items @ trailing_trivia @ eof_token)

let parse_source_file parser filename =
  (* Determine if this is an interface or implementation based on extension *)
  if String.ends_with ~suffix:".mli" filename then parse_interface parser
  else parse_implementation parser

(* ========================================================================= *)
(* PUBLIC API *)
(* ========================================================================= *)

let parse ~source ?(filename = "input.ml") tokens =
  let parser = create ~source tokens in
  let green_tree = parse_source_file parser filename in
  { tree = green_tree; diagnostics = List.rev parser.diagnostics }
