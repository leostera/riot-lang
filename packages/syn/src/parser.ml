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

(* Check if a token kind represents an operator *)
let is_operator_token = function
  | Token.Plus | Token.Minus | Token.Star | Token.Slash | Token.Percent
  | Token.Eq | Token.Ne | Token.Lt | Token.Gt | Token.LtEq | Token.GtEq
  | Token.And | Token.Or | Token.ColonColon | Token.Caret | Token.At
  | Token.ColonEq | Token.LeftArrow | Token.StarStar | Token.EqEq | Token.BangEq
  | Token.AtAt | Token.PipeGt | Token.PercentGt | Token.LtPercent | Token.Bang
  | Token.Tilde | Token.Question | Token.Pipe | Token.Arrow | Token.Dot
  | Token.Semi | Token.Comma | Token.Colon | Token.Hash
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
  | Token.OpenDelim _ | Token.CloseDelim _ -> Syntax_kind.WHITESPACE
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
      let sep_token = consume parser in
      let _ = consume_trivia parser in
      match parse_element parser with
      | Some elem ->
          let trivia = consume_trivia parser in
          loop
            (List.rev_append trivia (Ceibo.Green.Node elem :: sep_token :: acc))
      | None -> List.rev acc
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
        let _ = consume_trivia parser in
        let acc = elem :: acc in
        if at parser separator then
          let _ = consume parser in
          let _ = consume_trivia parser in
          loop acc
        else List.rev acc
  in
  loop []

(* ========================================================================= *)
(* IDENTIFIER PARSING *)
(* ========================================================================= *)

let parse_identifier parser =
  (* Parse a simple identifier or qualified identifier (module path):
     - Simple: name
     - Qualified: Module.Name, A.B.C.name
     Returns a list of tokens: [name] or [Module; .; Name] or [A; .; B; .; C; .; name]
  *)
  let first = consume parser in
  let _ = consume_trivia parser in

  let rec parse_rest acc =
    if at parser Token.Dot then
      let dot = consume parser in
      let _ = consume_trivia parser in
      let name = consume parser in
      let _ = consume_trivia parser in
      parse_rest (name :: dot :: acc)
    else List.rev acc
  in

  let rest = parse_rest [] in
  first :: rest

(* ========================================================================= *)
(* LITERALS *)
(* ========================================================================= *)

let parse_literal parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some (Token.Literal (Token.Int _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.INT_LITERAL children)
  | Some (Token.Literal (Token.Float _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.FLOAT_LITERAL children)
  | Some (Token.Literal (Token.String _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.STRING_LITERAL children)
  | Some (Token.Literal (Token.Char _)) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [ tok ] in
      Some (make_node_list ~kind:Syntax_kind.CHAR_LITERAL children)
  | Some (Token.Keyword Keyword.True) | Some (Token.Keyword Keyword.False) ->
      let tok = consume parser in
      let children = prepend_pending_trivia parser [ tok ] in
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
                    let open_paren = consume parser in
                    let _ = consume_trivia parser in
                    match parse_expr parser with
                    | Some expr ->
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
                            Ceibo.Green.Node expr;
                            close_paren;
                          ]
                        in
                        let children = trivia @ children in
                        let local_open =
                          make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                        in
                        loop local_open
                    | None -> Some lhs
                  else
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
                        let children = trivia @ children in
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
                    match parse_list_expr parser with
                    | Some list_expr ->
                        let trivia = take_pending_trivia parser in
                        let children =
                          [
                            Ceibo.Green.Node lhs;
                            dot;
                            Ceibo.Green.Node list_expr;
                          ]
                        in
                        let children = trivia @ children in
                        let local_open =
                          make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                        in
                        loop local_open
                    | None -> Some lhs
                  else
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
                        let children = trivia @ children in
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
                        let trivia = take_pending_trivia parser in
                        let children =
                          [
                            Ceibo.Green.Node lhs;
                            dot;
                            Ceibo.Green.Node array_expr;
                          ]
                        in
                        let children = trivia @ children in
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
                      let trivia = take_pending_trivia parser in
                      let children =
                        [
                          Ceibo.Green.Node lhs;
                          dot;
                          Ceibo.Green.Node record_expr;
                        ]
                      in
                      let children = trivia @ children in
                      let prefixed_record =
                        make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                      in
                      loop prefixed_record
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
        | Some Token.Hash -> (
            (* Method call: obj#method *)
            let hash_prec = 9 in
            (* Same as field access *)
            if hash_prec < min_bp then Some lhs
            else
              let hash = consume parser in
              let _ = consume_trivia parser in
              match peek_kind parser with
              | Some (Token.Ident _) ->
                  let method_name = consume parser in
                  let trivia = take_pending_trivia parser in
                  let children = [ Ceibo.Green.Node lhs; hash; method_name ] in
                  let children = trivia @ children in
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
              let semi = consume parser in
              let _ = consume_trivia parser in
              (* Right-associative: use `prec` not `prec + 1` *)
              match parse_expr_bp parser prec with
              | Some rhs ->
                  let trivia = take_pending_trivia parser in
                  let children =
                    [ Ceibo.Green.Node lhs; semi; Ceibo.Green.Node rhs ]
                  in
                  let children = trivia @ children in
                  let seq =
                    make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children
                  in
                  loop seq
              | None -> Some lhs)
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
                  let infix =
                    make_node_list ~kind:Syntax_kind.INFIX_EXPR children
                  in
                  loop infix
              | None -> Some lhs)
        | (Some Token.Tilde | Some Token.Question) when min_bp <= 8 -> (
            (* Labeled or optional argument *)
            match parse_labeled_or_optional_arg parser with
            | Some arg ->
                let trivia = take_pending_trivia parser in
                let children = [ Ceibo.Green.Node lhs; Ceibo.Green.Node arg ] in
                let children = trivia @ children in
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
                let open_bracket = consume parser in
                let _ = consume_trivia parser in
                let attr_marker = consume parser in
                (* @ or @@ or % *)
                let _ = consume_trivia parser in
                (* Parse attribute/extension name *)
                match peek_kind parser with
                | Some (Token.Ident _) ->
                    let name = consume parser in
                    let _ = consume_trivia parser in
                    (* Collect optional payload until ] *)
                    let rec collect_payload acc =
                      if at parser (Token.CloseDelim Token.Bracket) then
                        List.rev acc
                      else
                        let tok = consume parser in
                        let _ = consume_trivia parser in
                        collect_payload (tok :: acc)
                    in
                    let payload = collect_payload [] in
                    let close_bracket =
                      expect parser (Token.CloseDelim Token.Bracket)
                    in
                    let children =
                      [ Ceibo.Green.Node lhs; open_bracket; attr_marker; name ]
                      @ payload @ [ close_bracket ]
                    in
                    (* Determine kind based on marker *)
                    let kind =
                      match marker with
                      | Some Token.Percent -> Syntax_kind.EXTENSION_EXPR
                      | _ -> Syntax_kind.ATTRIBUTE_EXPR
                    in
                    let attributed =
                      make_node_list ~kind
                        (prepend_pending_trivia parser children)
                    in
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
                  match parse_primary parser with
                  | Some rhs ->
                      let trivia = take_pending_trivia parser in
                      let children =
                        [ Ceibo.Green.Node lhs; Ceibo.Green.Node rhs ]
                      in
                      let children = trivia @ children in
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
              match parse_primary parser with
              | Some rhs ->
                  let trivia = take_pending_trivia parser in
                  let children =
                    [ Ceibo.Green.Node lhs; Ceibo.Green.Node rhs ]
                  in
                  let children = trivia @ children in
                  let app =
                    make_node_list ~kind:Syntax_kind.APPLY_EXPR children
                  in
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
        let children = prepend_pending_trivia parser [ minus; dot ] in
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
  | Some Token.Tilde
    when peek_nth parser 1 = Some Token.Minus
         || peek_nth parser 1 = Some Token.Dot -> (
      (* Floating-point negation operators: ~- or ~-. *)
      let tilde = consume parser in
      let next_tok = consume parser in
      let _ = consume_trivia parser in
      match parse_expr_bp parser 7 with
      | Some operand ->
          let children =
            prepend_pending_trivia parser
              [ tilde; next_tok; Ceibo.Green.Node operand ]
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
              let children = prepend_pending_trivia parser [ ident ] in
              Some (make_node_list ~kind:Syntax_kind.IDENT_EXPR children)
          (* Parenthesized expression *)
          | Some (Token.OpenDelim Token.Paren) -> parse_paren_expr parser
          (* List literal *)
          | Some (Token.OpenDelim Token.Bracket) -> (
              (* Could be list [x; y] or extension [%ext ...] *)
              match peek_nth parser 1 with
              | Some Token.Percent -> (
                  (* Extension expression [%ext ...] *)
                  let open_bracket = consume parser in
                  let _ = consume_trivia parser in
                  let percent = consume parser in
                  let _ = consume_trivia parser in
                  match peek_kind parser with
                  | Some (Token.Ident _) ->
                      let name = consume parser in
                      let _ = consume_trivia parser in
                      (* Collect payload until ] *)
                      let rec collect_payload acc =
                        if at parser (Token.CloseDelim Token.Bracket) then
                          List.rev acc
                        else
                          let tok = consume parser in
                          let _ = consume_trivia parser in
                          collect_payload (tok :: acc)
                      in
                      let payload = collect_payload [] in
                      let close_bracket =
                        expect parser (Token.CloseDelim Token.Bracket)
                      in
                      let children =
                        [ open_bracket; percent; name ]
                        @ payload @ [ close_bracket ]
                      in
                      Some
                        (make_node_list ~kind:Syntax_kind.EXTENSION_EXPR
                           (prepend_pending_trivia parser children))
                  | _ -> parse_list_expr parser)
              | _ -> parse_list_expr parser)
          (* Array literal *)
          | Some (Token.OpenDelim Token.Array) -> parse_array_expr parser
          (* Record literal or object update *)
          | Some (Token.OpenDelim Token.Brace) ->
              if
                (* Check if this is object update {< ... >} *)
                peek_nth parser 1 = Some Token.Lt
              then parse_object_update_expr parser
              else parse_record_expr parser
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
          | Some (Token.OpenDelim Token.BeginEnd) -> parse_begin_expr parser
          (* Try/catch *)
          | Some (Token.Keyword Keyword.Try) -> parse_try_expr parser
          (* Polymorphic variant *)
          | Some Token.Backtick -> parse_poly_variant_expr parser
          (* Object expression *)
          | Some (Token.OpenDelim Token.ObjectEnd) -> parse_object_expr parser
          (* New expression *)
          | Some (Token.Keyword Keyword.New) -> parse_new_expr parser
          | _ -> None))

and parse_labeled_or_optional_arg parser =
  let _ = consume_trivia parser in
  match peek_kind parser with
  | Some Token.Tilde -> (
      if
        (* Check if this is ~- or ~-. (float negation operators) *)
        peek_nth parser 1 = Some Token.Minus
        || peek_nth parser 1 = Some Token.Dot
      then
        (* Parse as prefix operator ~- or ~-. *)
        let tilde = consume parser in
        let next_tok = consume parser in
        let _ = consume_trivia parser in
        match parse_expr_bp parser 7 with
        | Some operand ->
            let children =
              prepend_pending_trivia parser
                [ tilde; next_tok; Ceibo.Green.Node operand ]
            in
            Some (make_node_list ~kind:Syntax_kind.PREFIX_EXPR children)
        | None -> None
      else
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
            let children = prepend_pending_trivia parser [ question; label ] in
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
                let children = prepend_pending_trivia parser [ tilde; label ] in
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
            let children = prepend_pending_trivia parser [ question; label ] in
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
                  prepend_pending_trivia parser [ question; open_paren; label ]
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
    let sig_kw = consume parser in
    let _ = consume_trivia parser in

    let rec consume_until_end acc =
      if at parser (Token.CloseDelim Token.SigEnd) then List.rev acc
      else if peek parser = None then List.rev acc
      else
        let tok = consume parser in
        let _ = consume_trivia parser in
        consume_until_end (tok :: acc)
    in

    let items = consume_until_end [] in
    let end_kw = consume parser in

    make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR
      ([ sig_kw ] @ items @ [ end_kw ])
  else
    (* Module type identifier or path *)
    let type_ident = parse_identifier parser in
    let _ = consume_trivia parser in

    (* Check for 'with' constraints *)
    if at parser (Token.Keyword Keyword.With) then
      let with_kw = consume parser in
      let _ = consume_trivia parser in

      (* Parse 'with type t = ...' constraints *)
      let rec parse_with_constraints acc =
        if not (at parser (Token.Keyword Keyword.Type)) then List.rev acc
        else
          let type_kw = consume parser in
          let _ = consume_trivia parser in

          (* Parse type path (t or M.t) *)
          let type_path = parse_identifier parser in
          let _ = consume_trivia parser in

          (* Expect = *)
          let eq = expect parser Token.Eq in
          let _ = consume_trivia parser in

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
                let tok = consume parser in
                let _ = consume_trivia parser in
                consume_type_tokens (tok :: acc) (depth + 1)
            | Some (Token.CloseDelim Token.Paren) ->
                let tok = consume parser in
                let _ = consume_trivia parser in
                consume_type_tokens (tok :: acc) (depth - 1)
            | Some Token.Underscore
            | Some Token.Quote
            | Some (Token.Ident _)
            | Some Token.Arrow
            | Some Token.Star
            | Some (Token.Literal _) ->
                let tok = consume parser in
                let _ = consume_trivia parser in
                consume_type_tokens (tok :: acc) depth
            | None -> List.rev acc
            | _ ->
                (* Stop at other tokens *)
                List.rev acc
          in
          let type_tokens = consume_type_tokens [] 0 in

          let constraint_node =
            make_node_list ~kind:Syntax_kind.TYPE_CONSTRAINT
              ([ type_kw ] @ type_path @ [ eq ] @ type_tokens)
          in

          (* Check if there's another 'and' constraint *)
          if at parser (Token.Keyword Keyword.And) then
            let and_kw = consume parser in
            let _ = consume_trivia parser in
            parse_with_constraints
              (Ceibo.Green.Node constraint_node :: and_kw :: acc)
          else parse_with_constraints (Ceibo.Green.Node constraint_node :: acc)
      in

      let constraints = parse_with_constraints [] in

      make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR
        (type_ident @ [ with_kw ] @ constraints)
    else
      (* Simple module type reference *)
      make_node_list ~kind:Syntax_kind.MODULE_TYPE_EXPR type_ident

and parse_paren_expr parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  (* Check for unit literal () *)
  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children = prepend_pending_trivia parser [ open_paren; close_paren ] in
    Some (make_node_list ~kind:Syntax_kind.UNIT_LITERAL children)
    (* Check for first-class module pack: (module M : S) or (module struct ... end) *)
  else if at parser (Token.Keyword Keyword.Module) then
    let module_kw = consume parser in
    let _ = consume_trivia parser in

    (* Check if it's a struct expression or a typed module *)
    if at parser (Token.OpenDelim Token.StructEnd) then (
      (* (module struct ... end) - module expression without type annotation *)
      let struct_tokens = ref [] in
      let depth = ref 1 in
      let struct_kw = consume parser in
      struct_tokens := struct_kw :: !struct_tokens;
      let _ = consume_trivia parser in

      (* Consume until matching 'end' *)
      while !depth > 0 && peek parser <> None do
        match peek_kind parser with
        | Some (Token.OpenDelim Token.StructEnd) ->
            depth := !depth + 1;
            struct_tokens := consume parser :: !struct_tokens;
            let _ = consume_trivia parser in
            ()
        | Some (Token.CloseDelim Token.StructEnd) ->
            depth := !depth - 1;
            struct_tokens := consume parser :: !struct_tokens;
            if !depth > 0 then
              let _ = consume_trivia parser in
              ()
        | _ ->
            struct_tokens := consume parser :: !struct_tokens;
            let _ = consume_trivia parser in
            ()
      done;

      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let children =
        prepend_pending_trivia parser
          ([ open_paren; module_kw ] @ List.rev !struct_tokens @ [ close_paren ])
      in
      Some (make_node_list ~kind:Syntax_kind.APPLY_EXPR children))
    else
      (* (module M : S) or (module M) - first-class module *)
      (* Parse module name *)
      let module_name = consume parser in
      let _ = consume_trivia parser in

      (* Optional type annotation *)
      let type_annotation =
        if at parser Token.Colon then
          let colon = consume parser in
          let _ = consume_trivia parser in
          let module_type = parse_module_type_expr parser in
          let _ = consume_trivia parser in
          [ colon; Ceibo.Green.Node module_type ]
        else []
      in

      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let children =
        prepend_pending_trivia parser
          ([ open_paren; module_kw; module_name ]
          @ type_annotation @ [ close_paren ])
      in
      Some (make_node_list ~kind:Syntax_kind.APPLY_EXPR children)
  else
    match parse_expr parser with
    | Some expr ->
        let _ = consume_trivia parser in
        (* Check if it's a tuple (has comma), sequence (has semicolon), type annotation (has colon), or just parenthesized expr *)
        if at parser Token.Comma then parse_tuple_rest parser open_paren expr
        else if at parser Token.Semi then
          parse_sequence_rest parser open_paren expr
        else if at parser Token.Colon then
          (* Type annotation: (expr : type) *)
          let colon = consume parser in
          let _ = consume_trivia parser in
          (* For now, just consume tokens until closing paren as the "type" *)
          (* A proper implementation would parse the type, but we'll keep it simple *)
          (* Parse type tokens until closing paren - functional approach *)
          let rec consume_type_tokens acc =
            if at parser (Token.CloseDelim Token.Paren) || peek parser = None
            then List.rev acc
            else
              let tok = consume parser in
              let _ = consume_trivia parser in
              consume_type_tokens (tok :: acc)
          in
          let type_elements = consume_type_tokens [] in
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let children =
            List.concat
              [
                [ open_paren; Ceibo.Green.Node expr; colon ];
                type_elements;
                [ close_paren ];
              ]
          in
          let children = prepend_pending_trivia parser children in
          Some (make_node_list ~kind:Syntax_kind.TYPED_EXPR children)
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
  let children = open_paren :: (elements @ [ close_paren ]) in
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
  let children = open_paren :: (elements @ [ close_paren ]) in
  let children = prepend_pending_trivia parser children in
  Some (make_node_list ~kind:Syntax_kind.SEQUENCE_EXPR children)

and parse_list_expr parser =
  let open_bracket = consume parser in
  let _ = consume_trivia parser in

  (* Check for empty list [] *)
  if at parser (Token.CloseDelim Token.Bracket) then
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
        let children = open_bracket :: (elements @ [ close_bracket ]) in
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

and parse_array_expr parser =
  let open_array = consume parser in
  let _ = consume_trivia parser in

  (* Check for empty array [| |] *)
  if at parser (Token.CloseDelim Token.Array) then
    let close_array = consume parser in
    let children = prepend_pending_trivia parser [ open_array; close_array ] in
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
            if at parser (Token.CloseDelim Token.Array) then List.rev acc
            else
              match parse_expr parser with
              | Some expr ->
                  let _ = consume_trivia parser in
                  parse_elements (Ceibo.Green.Node expr :: acc)
              | None -> List.rev acc
        in

        let elements = parse_elements [ Ceibo.Green.Node first_expr ] in

        let close_array = expect parser (Token.CloseDelim Token.Array) in
        let children = open_array :: (elements @ [ close_array ]) in
        let children = prepend_pending_trivia parser children in
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
  let open_brace = consume parser in
  let _ = consume_trivia parser in

  (* Check for empty record {} - though this isn't valid OCaml, we'll parse it *)
  if at parser (Token.CloseDelim Token.Brace) then
    let close_brace = consume parser in
    let children = prepend_pending_trivia parser [ open_brace; close_brace ] in
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
    let is_record_literal =
      match peek_non_trivia_nth parser 0 with
      | Some (Token.Ident _) -> (
          match peek_non_trivia_nth parser 1 with
          | Some Token.Eq -> true (* field = value *)
          | Some Token.Semi -> true (* field; (punning) *)
          | Some (Token.CloseDelim Token.Brace) ->
              true (* field } (punning at end) *)
          | _ -> false)
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
      let children = open_brace :: (fields @ [ close_brace ]) in
      let children = prepend_pending_trivia parser children in
      Some (make_node_list ~kind:Syntax_kind.RECORD_EXPR children)
    else
      (* Parse expression first (for record update) *)
      match parse_expr parser with
      | Some base_expr ->
          let _ = consume_trivia parser in
          if at parser (Token.Keyword Keyword.With) then
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
            let children =
              open_brace :: Ceibo.Green.Node base_expr :: with_kw
              :: (fields @ [ close_brace ])
            in
            let children = prepend_pending_trivia parser children in
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
        let children = prepend_pending_trivia parser [ backtick; tag_token ] in
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
      [ while_kw; Ceibo.Green.Node cond; do_kw; Ceibo.Green.Node body; done_kw ]
  in
  Some (make_node_list ~kind:Syntax_kind.WHILE_EXPR children)

and parse_begin_expr parser =
  let begin_kw = consume parser in
  let _ = consume_trivia parser in

  match parse_expr parser with
  | Some expr ->
      let _ = consume_trivia parser in
      let end_kw = expect parser (Token.CloseDelim Token.BeginEnd) in
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

      let children =
        (try_kw :: rest_cases) @ [ Ceibo.Green.Node expr; with_kw ]
      in
      let children = prepend_pending_trivia parser children in
      Some (make_node_list ~kind:Syntax_kind.TRY_EXPR children)
  | None -> None

and parse_new_expr parser =
  let new_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse class path (might be Module.class_name) *)
  let rec parse_class_path acc =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let ident = consume parser in
        let _ = consume_trivia parser in
        let acc = ident :: acc in
        if at parser Token.Dot then
          let dot = consume parser in
          let _ = consume_trivia parser in
          parse_class_path (dot :: acc)
        else List.rev acc
    | _ -> List.rev acc
  in

  let class_path = parse_class_path [] in
  let children = prepend_pending_trivia parser (new_kw :: class_path) in
  Some (make_node_list ~kind:Syntax_kind.NEW_EXPR children)

and parse_object_update_expr parser =
  let open_brace = consume parser in
  let _ = consume_trivia parser in
  let lt = consume parser in
  (* Consume < *)
  let _ = consume_trivia parser in

  (* Parse field updates: field = value; ... until > *)
  let rec parse_updates acc =
    if at parser Token.Gt then List.rev acc
    else
      match peek_kind parser with
      | Some (Token.Ident _) ->
          let field_name = consume parser in
          let _ = consume_trivia parser in
          let eq_and_value =
            if at parser Token.Eq then
              let eq = consume parser in
              let _ = consume_trivia parser in
              (* Parse with min_bp=4 to avoid consuming > as infix operator *)
              match parse_expr_bp parser 4 with
              | Some value ->
                  let _ = consume_trivia parser in
                  [ eq; Ceibo.Green.Node value ]
              | None -> [ eq ]
            else []
          in
          let semi =
            if at parser Token.Semi then
              let s = consume parser in
              let _ = consume_trivia parser in
              [ s ]
            else []
          in
          parse_updates
            (List.rev_append semi
               (List.rev_append eq_and_value (field_name :: acc)))
      | _ -> List.rev acc
  in

  let updates = parse_updates [] in
  let gt = expect parser Token.Gt in
  let _ = consume_trivia parser in
  let close_brace = expect parser (Token.CloseDelim Token.Brace) in

  let children =
    prepend_pending_trivia parser
      ([ open_brace; lt ] @ updates @ [ gt; close_brace ])
  in
  Some (make_node_list ~kind:Syntax_kind.OBJECT_UPDATE_EXPR children)

and parse_object_expr parser =
  let object_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for optional self parameter: object (self) ... end *)
  let self_param =
    if at parser (Token.OpenDelim Token.Paren) then
      let open_paren = consume parser in
      let _ = consume_trivia parser in
      let self_ident =
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let ident = consume parser in
            let _ = consume_trivia parser in
            [ ident ]
        | _ -> []
      in
      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let _ = consume_trivia parser in
      [ open_paren ] @ self_ident @ [ close_paren ]
    else []
  in

  (* Parse object items until 'end' keyword *)
  let rec parse_object_items acc =
    if at parser (Token.CloseDelim Token.ObjectEnd) then List.rev acc
    else
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.CloseDelim Token.ObjectEnd) -> List.rev acc
      | Some (Token.Keyword Keyword.Method) ->
          let method_item = parse_object_method parser in
          parse_object_items (method_item @ acc)
      | Some (Token.Keyword Keyword.Val) ->
          let val_item = parse_object_val parser in
          parse_object_items (val_item @ acc)
      | Some (Token.Keyword Keyword.Inherit) ->
          let inherit_item = parse_object_inherit parser in
          parse_object_items (inherit_item @ acc)
      | Some (Token.Keyword Keyword.Constraint) ->
          let constraint_item = parse_object_constraint parser in
          parse_object_items (constraint_item @ acc)
      | Some (Token.Keyword Keyword.Initializer) ->
          let initializer_item = parse_object_initializer parser in
          parse_object_items (initializer_item @ acc)
      | _ -> List.rev acc
  in

  let items = parse_object_items [] in
  let end_kw = expect parser (Token.CloseDelim Token.ObjectEnd) in

  let children =
    prepend_pending_trivia parser
      ([ object_kw ] @ self_param @ items @ [ end_kw ])
  in
  Some (make_node_list ~kind:Syntax_kind.OBJECT_EXPR children)

and parse_object_method parser =
  let method_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for private *)
  let private_kw =
    if at parser (Token.Keyword Keyword.Private) then
      let priv = consume parser in
      let _ = consume_trivia parser in
      [ priv ]
    else []
  in

  (* Parse method name *)
  let method_name =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let name = consume parser in
        let _ = consume_trivia parser in
        [ name ]
    | _ -> []
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
      let tok = consume parser in
      let _ = consume_trivia parser in
      consume_until_eq (tok :: acc)
  in

  let params = consume_until_eq [] in

  (* Parse = and method body *)
  let eq_and_body =
    if at parser Token.Eq then
      let eq = consume parser in
      let _ = consume_trivia parser in
      match parse_expr parser with
      | Some body -> [ eq; Ceibo.Green.Node body ]
      | None -> [ eq ]
    else []
  in

  [ method_kw ] @ private_kw @ method_name @ params @ eq_and_body

and parse_object_val parser =
  let val_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for mutable *)
  let mutable_kw =
    if at parser (Token.Keyword Keyword.Mutable) then
      let mut = consume parser in
      let _ = consume_trivia parser in
      [ mut ]
    else []
  in

  (* Parse field name *)
  let field_name =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        let name = consume parser in
        let _ = consume_trivia parser in
        [ name ]
    | _ -> []
  in

  (* Parse = and value *)
  let eq_and_value =
    if at parser Token.Eq then
      let eq = consume parser in
      let _ = consume_trivia parser in
      match parse_expr parser with
      | Some value -> [ eq; Ceibo.Green.Node value ]
      | None -> [ eq ]
    else []
  in

  [ val_kw ] @ mutable_kw @ field_name @ eq_and_value

and parse_object_inherit parser =
  let inherit_kw = consume parser in
  let _ = consume_trivia parser in

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
          let _ = consume_trivia parser in
          List.rev (Ceibo.Green.Node expr :: acc)
      | None -> List.rev acc
  in

  let class_expr = consume_class_expr [] in
  [ inherit_kw ] @ class_expr

and parse_object_constraint parser =
  let constraint_kw = consume parser in
  let _ = consume_trivia parser in

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
      let tok = consume parser in
      let _ = consume_trivia parser in
      consume_until_next (tok :: acc)
  in

  let constraint_tokens = consume_until_next [] in
  [ constraint_kw ] @ constraint_tokens

and parse_object_initializer parser =
  let initializer_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse initializer expression *)
  let init_expr =
    match parse_expr parser with
    | Some expr -> [ Ceibo.Green.Node expr ]
    | None -> []
  in

  [ initializer_kw ] @ init_expr

and parse_let_expr parser =
  (* Parse let expression with pattern destructuring support *)
  let let_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check for binding operator: let*, let+, etc. *)
  let is_binding_op =
    match peek_kind parser with
    | Some
        ( Token.Star | Token.Plus | Token.Minus | Token.Ampersand | Token.Pipe
        | Token.Dollar | Token.Percent | Token.At | Token.Eq ) ->
        true
    | _ -> false
  in

  if is_binding_op then parse_binding_operator_expr parser let_kw
    (* Check for 'let open' or 'let module' *)
  else if at parser (Token.Keyword Keyword.Open) then
    parse_let_open_expr parser let_kw ()
  else if at parser (Token.Keyword Keyword.Module) then
    parse_let_module_expr parser let_kw ()
  else if at parser (Token.Keyword Keyword.Exception) then
    parse_let_exception_expr parser let_kw ()
  else parse_regular_let_expr parser let_kw

and parse_let_open_expr parser let_kw ?(attributes = []) () =
  (* let open Module in expr *)
  let open_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse module path *)
  let module_path = parse_identifier parser in
  let _ = consume_trivia parser in

  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in
  let _ = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        prepend_pending_trivia parser
          ([ let_kw ] @ attributes @ [ open_kw ] @ module_path
          @ [ in_kw; Ceibo.Green.Node body ])
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_let_module_expr parser let_kw ?(attributes = []) () =
  (* let module M = (val m : S) in expr *)
  let module_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse module name *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Expect '=' *)
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

  (* Parse module expression: (val expr : ModType) or other module expression *)
  let module_expr =
    if at parser (Token.OpenDelim Token.Paren) then
      (* Could be (val expr : ModType) *)
      let open_paren = consume parser in
      let _ = consume_trivia parser in

      if at parser (Token.Keyword Keyword.Val) then
        (* (val expr : ModType) - unpack first-class module *)
        let val_kw = consume parser in
        let _ = consume_trivia parser in

        (* Parse expression (module value) *)
        let rec consume_until_colon acc =
          if at parser Token.Colon || peek parser = None then List.rev acc
          else
            let tok = consume parser in
            let _ = consume_trivia parser in
            consume_until_colon (tok :: acc)
        in
        let expr_tokens = consume_until_colon [] in

        (* Expect : *)
        let colon = expect parser Token.Colon in
        let _ = consume_trivia parser in

        (* Parse module type expression *)
        let module_type = parse_module_type_expr parser in

        (* Expect ) *)
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in

        [ open_paren; val_kw ] @ expr_tokens
        @ [ colon; Ceibo.Green.Node module_type; close_paren ]
      else
        (* Other parenthesized module expression - consume until 'in' *)
        let rec consume_until_in acc =
          if at parser (Token.Keyword Keyword.In) || peek parser = None then
            List.rev acc
          else
            let tok = consume parser in
            let _ = consume_trivia parser in
            consume_until_in (tok :: acc)
        in
        open_paren :: consume_until_in []
    else
      (* Module path or other expression - consume tokens until 'in' *)
      let rec consume_until_in acc =
        if at parser (Token.Keyword Keyword.In) || peek parser = None then
          List.rev acc
        else
          let tok = consume parser in
          let _ = consume_trivia parser in
          consume_until_in (tok :: acc)
      in
      consume_until_in []
  in

  let _ = consume_trivia parser in

  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in
  let _ = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        prepend_pending_trivia parser
          ([ let_kw ] @ attributes @ [ module_kw; name; eq ] @ module_expr
          @ [ in_kw; Ceibo.Green.Node body ])
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_let_exception_expr parser let_kw ?(attributes = []) () =
  (* let exception E of type in expr *)
  let exception_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse exception constructor name *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Parse optional 'of type' clause - consume tokens until 'in' *)
  let rec consume_until_in acc =
    if at parser (Token.Keyword Keyword.In) || peek parser = None then
      List.rev acc
    else
      let tok = consume parser in
      let _ = consume_trivia parser in
      consume_until_in (tok :: acc)
  in
  let type_tokens = consume_until_in [] in

  let _ = consume_trivia parser in

  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in
  let _ = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        prepend_pending_trivia parser
          ([ let_kw ] @ attributes @ [ exception_kw; name ] @ type_tokens
          @ [ in_kw; Ceibo.Green.Node body ])
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_binding_operator_expr parser let_kw =
  (* Binding operator: let* pattern = expr in body *)
  (* The 'let' keyword has already been consumed *)

  (* Consume the operator symbol: *, +, -, etc. *)
  let op_token = consume parser in
  let _ = consume_trivia parser in

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
  let _ = consume_trivia parser in

  (* Expect '=' *)
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

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
  let _ = consume_trivia parser in

  (* Check for 'and<op>' to parse additional bindings *)
  let rec parse_and_bindings acc =
    if at parser (Token.Keyword Keyword.And) then
      (* Check if next token is an operator *)
      match peek_nth parser 1 with
      | Some
          ( Token.Star | Token.Plus | Token.Minus | Token.Ampersand | Token.Pipe
          | Token.Dollar | Token.Percent | Token.At | Token.Eq ) ->
          let and_kw = consume parser in
          let _ = consume_trivia parser in
          let and_op = consume parser in
          let _ = consume_trivia parser in

          (* Parse pattern *)
          let and_pattern =
            match parse_pattern parser with
            | Some pat -> Ceibo.Green.Node pat
            | None ->
                Ceibo.Green.Token
                  (Ceibo.Green.make_token ~kind:Syntax_kind.IDENT_EXPR ~text:"_"
                     ~width:1)
          in
          let _ = consume_trivia parser in

          (* Expect '=' *)
          let and_eq = expect parser Token.Eq in
          let _ = consume_trivia parser in

          (* Parse expression *)
          let and_expr =
            match parse_expr parser with
            | Some expr -> Ceibo.Green.Node expr
            | None ->
                Ceibo.Green.Token
                  (Ceibo.Green.make_token ~kind:Syntax_kind.UNIT_LITERAL
                     ~text:"()" ~width:2)
          in
          let _ = consume_trivia parser in

          parse_and_bindings
            ([ and_kw; and_op; and_pattern; and_eq; and_expr ] @ acc)
      | _ -> List.rev acc
    else List.rev acc
  in
  let and_bindings = parse_and_bindings [] in

  (* Expect 'in' *)
  let in_kw = expect parser (Token.Keyword Keyword.In) in
  let _ = consume_trivia parser in

  (* Parse body expression *)
  match parse_expr parser with
  | Some body ->
      let children =
        prepend_pending_trivia parser
          ([ let_kw; op_token; pattern; eq; rhs_expr ]
          @ and_bindings
          @ [ in_kw; Ceibo.Green.Node body ])
      in
      Some (make_node_list ~kind:Syntax_kind.LET_EXPR children)
  | None -> None

and parse_regular_let_expr parser let_kw =
  (* Regular let binding *)
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

  (* Parse pattern - use parse_pattern to handle all pattern types including underscore *)
  let pattern =
    match parse_pattern parser with
    | Some first_pat -> (
        let _ = consume_trivia parser in
        (* Check if followed by comma (tuple pattern) *)
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

  let _ = consume_trivia parser in

  (* Check if pattern is a simple identifier (function name) *)
  let is_simple_ident =
    match pattern with
    | Ceibo.Green.Token _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_EXPR
    | _ -> false
  in

  (* Check for optional type annotation: let f : int -> int = ... *)
  let type_annotation =
    if is_simple_ident && at parser Token.Colon then
      let colon = consume parser in
      let _ = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let tok = consume parser in
          let _ = consume_trivia parser in
          consume_type_tokens (tok :: acc)
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ type_tokens)
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
                  let _ = consume_trivia parser in
                  loop (Ceibo.Green.Node param :: acc)
              | None -> List.rev acc)
          | Some (Token.Ident _) -> (
              (* Check if this identifier is followed by tokens that suggest it's
                 an expression (function application) rather than a parameter *)
              let looks_like_application =
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
                    let _ = consume_trivia parser in
                    loop (Ceibo.Green.Node pat :: acc)
                | None -> List.rev acc)
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
      let children =
        [ let_kw; kw; pattern ]
        @ (match type_annotation with Some t -> t | None -> [])
        @ params @ [ eq; value_expr ] @ and_bindings @ [ in_kw; body_expr ]
      in
      Some (make_node_list ~kind children)
  | None ->
      let children =
        [ let_kw; pattern ]
        @ (match type_annotation with Some t -> t | None -> [])
        @ params @ [ eq; value_expr ] @ and_bindings @ [ in_kw; body_expr ]
      in
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

  let children = fun_kw :: (params @ [ arrow; body ]) in
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
      if at parser Token.ColonColon then
        (* Cons pattern: a :: b or "x" :: rest *)
        let cons_op = consume parser in
        let _ = consume_trivia parser in
        match parse_pattern parser with
        | Some tail_pat ->
            Some
              (make_node_list ~kind:Syntax_kind.CONS_PATTERN
                 [ Ceibo.Green.Node pat; cons_op; Ceibo.Green.Node tail_pat ])
        | None -> Some pat
      else if at parser Token.DotDot then
        (* Range pattern: 'a' .. 'z' or 0 .. 9 *)
        let dotdot = consume parser in
        let _ = consume_trivia parser in
        match parse_base_pattern parser with
        | Some end_pat ->
            Some
              (make_node_list ~kind:Syntax_kind.RANGE_PATTERN
                 [ Ceibo.Green.Node pat; dotdot; Ceibo.Green.Node end_pat ])
        | None -> Some pat
      else if at parser Token.Pipe then
        (* OR pattern: A | B | C *)
        let rec collect_or_patterns acc =
          if not (at parser Token.Pipe) then List.rev acc
          else
            let pipe = consume parser in
            let _ = consume_trivia parser in
            match parse_base_pattern parser with
            | Some p ->
                let _ = consume_trivia parser in
                collect_or_patterns (Ceibo.Green.Node p :: pipe :: acc)
            | None -> List.rev acc
        in
        let patterns = collect_or_patterns [ Ceibo.Green.Node pat ] in
        Some (make_node_list ~kind:Syntax_kind.OR_PATTERN patterns)
      else if at parser (Token.Keyword Keyword.As) then
        let as_kw = consume parser in
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let ident = consume parser in
            Some
              (make_node_list ~kind:Syntax_kind.AS_PATTERN
                 [ Ceibo.Green.Node pat; as_kw; ident ])
        | _ -> Some pat
      else Some pat
  | None -> None

and parse_base_pattern parser =
  let _ = consume_trivia parser in

  match peek_kind parser with
  (* Wildcard *)
  | Some Token.Underscore ->
      let underscore = consume parser in
      Some (make_node_list ~kind:Syntax_kind.WILDCARD_PATTERN [ underscore ])
  (* List pattern [] or [a; b; c] *)
  | Some (Token.OpenDelim Token.Bracket) -> parse_list_pattern parser
  (* Array pattern [| |] or [| a; b; c |] *)
  | Some (Token.OpenDelim Token.Array) -> parse_array_pattern parser
  (* Identifier or constructor pattern *)
  | Some (Token.Ident _) -> parse_ident_or_constructor_pattern parser
  (* Negative number pattern: -1, -32700, etc *)
  | Some Token.Minus -> (
      let minus = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some (Token.Literal (Token.Int _ | Token.Float _)) ->
          let number = consume parser in
          Some
            (make_node_list ~kind:Syntax_kind.LITERAL_PATTERN [ minus; number ])
      | _ ->
          (* Not a negative number literal, might be a prefix operator in expression context *)
          (* For now, return None to let error handling kick in *)
          None)
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
  (* Polymorphic variant type pattern: #color *)
  | Some Token.Hash ->
      let hash = consume parser in
      let _ = consume_trivia parser in
      let type_name = consume parser in
      Some
        (make_node_list ~kind:Syntax_kind.POLY_VARIANT_TYPE_PATTERN
           [ hash; type_name ])
  (* Exception pattern: exception E or exception E of t *)
  | Some (Token.Keyword Keyword.Exception) -> (
      let exception_kw = consume parser in
      let _ = consume_trivia parser in
      (* Parse the exception constructor pattern *)
      match parse_base_pattern parser with
      | Some pat ->
          Some
            (make_node_list ~kind:Syntax_kind.EXCEPTION_PATTERN
               [ exception_kw; Ceibo.Green.Node pat ])
      | None ->
          Some
            (make_node_list ~kind:Syntax_kind.EXCEPTION_PATTERN [ exception_kw ])
      )
  (* Lazy pattern: lazy p *)
  | Some (Token.Keyword Keyword.Lazy) -> (
      let lazy_kw = consume parser in
      let _ = consume_trivia parser in
      (* Parse the inner pattern *)
      match parse_pattern parser with
      | Some pat ->
          Some
            (make_node_list ~kind:Syntax_kind.LAZY_PATTERN
               [ lazy_kw; Ceibo.Green.Node pat ])
      | None -> Some (make_node_list ~kind:Syntax_kind.LAZY_PATTERN [ lazy_kw ])
      )
  | _ -> None

and parse_list_pattern parser =
  let open_bracket = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Bracket) then
    let close_bracket = consume parser in
    let children = [ open_bracket ] @ comments1 @ [ close_bracket ] in
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
    let children = open_bracket :: (patterns @ [ close_bracket ]) in
    Some (make_node_list ~kind:Syntax_kind.LIST_PATTERN children)

and parse_array_pattern parser =
  let open_array = consume parser in
  let comments1 = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Array) then
    let close_array = consume parser in
    let children = [ open_array ] @ comments1 @ [ close_array ] in
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

    let close_array = expect parser (Token.CloseDelim Token.Array) in
    let children = open_array :: (patterns @ [ close_array ]) in
    Some (make_node_list ~kind:Syntax_kind.ARRAY_PATTERN children)

and parse_ident_or_constructor_pattern parser =
  (* Parse identifier or module path (A.B.C) *)
  let ident_parts = parse_identifier parser in

  (* Get last identifier in path to check if it's a constructor *)
  let last_ident = List.hd (List.rev ident_parts) in
  let is_constructor =
    match Ceibo.Green.text last_ident with
    | Some text when Ceibo.Green.kind last_ident = Syntax_kind.IDENT_EXPR ->
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
             (ident_parts @ [ cons_op; Ceibo.Green.Node tail_pat ]))
    | None -> Some (make_node_list ~kind:Syntax_kind.IDENT_PATTERN ident_parts)
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
    | Some (Token.Keyword Keyword.False) -> (
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

and parse_paren_pattern parser =
  let open_paren = consume parser in
  let _ = consume_trivia parser in

  if at parser (Token.CloseDelim Token.Paren) then
    let close_paren = consume parser in
    let children = prepend_pending_trivia parser [ open_paren; close_paren ] in
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
  else if at parser (Token.Keyword Keyword.Module) then
    (* First-class module pattern: (module M : S) *)
    let module_kw = consume parser in
    let _ = consume_trivia parser in

    (* Parse module name *)
    let module_name = consume parser in
    let _ = consume_trivia parser in

    (* Check for optional type constraint *)
    let constraint_nodes =
      if at parser Token.Colon then
        let colon = consume parser in
        let _ = consume_trivia parser in
        (* Parse module type expression (handles 'with type' constraints) *)
        let module_type = parse_module_type_expr parser in
        let _ = consume_trivia parser in
        [ colon; Ceibo.Green.Node module_type ]
      else []
    in

    let close_paren = expect parser (Token.CloseDelim Token.Paren) in
    let children =
      prepend_pending_trivia parser
        ([ open_paren; module_kw; module_name ]
        @ constraint_nodes @ [ close_paren ])
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
                let green_tok = consume parser in
                collect_op_tokens tok.Token.span.end_ (green_tok :: acc)
          | _ -> List.rev acc
        in
        (* Start with -1 to accept the first operator token *)
        let op_tokens = collect_op_tokens (-1) [] in
        let _ = consume_trivia parser in
        let close_paren = expect parser (Token.CloseDelim Token.Paren) in
        let ident_pat =
          make_node_list ~kind:Syntax_kind.IDENT_PATTERN op_tokens
        in
        Some
          (make_node_list ~kind:Syntax_kind.PAREN_PATTERN
             [ open_paren; Ceibo.Green.Node ident_pat; close_paren ])
    | _ -> (
        match parse_pattern parser with
        | Some first_pat ->
            let _ = consume_trivia parser in

            if at parser Token.Pipe then
              (* Or-pattern: (A | B) *)
              let rec parse_or_patterns acc =
                if not (at parser Token.Pipe) then List.rev acc
                else
                  let pipe = consume parser in
                  let _ = consume_trivia parser in
                  match parse_pattern parser with
                  | Some pat ->
                      let _ = consume_trivia parser in
                      parse_or_patterns (Ceibo.Green.Node pat :: pipe :: acc)
                  | None -> List.rev acc
              in
              let patterns = parse_or_patterns [ Ceibo.Green.Node first_pat ] in
              let close_paren = expect parser (Token.CloseDelim Token.Paren) in
              let children = (open_paren :: patterns) @ [ close_paren ] in
              Some (make_node_list ~kind:Syntax_kind.OR_PATTERN children)
            else if at parser Token.Colon then
              (* Type annotation: (p : type) *)
              let colon = consume parser in
              let _ = consume_trivia parser in

              (* Parse type tokens until closing paren, tracking depth for nested parens *)
              let rec consume_type_tokens depth acc =
                if peek parser = None then List.rev acc
                else if at parser (Token.CloseDelim Token.Paren) && depth = 0
                then List.rev acc
                else
                  (* Check current token kind before consuming *)
                  let new_depth =
                    match peek_kind parser with
                    | Some (Token.OpenDelim Token.Paren) -> depth + 1
                    | Some (Token.CloseDelim Token.Paren) -> depth - 1
                    | _ -> depth
                  in
                  let tok = consume parser in
                  let _ = consume_trivia parser in
                  consume_type_tokens new_depth (tok :: acc)
              in
              let type_elements = consume_type_tokens 0 [] in

              let close_paren = expect parser (Token.CloseDelim Token.Paren) in
              let children =
                open_paren :: Ceibo.Green.Node first_pat :: colon
                :: type_elements
                @ [ close_paren ]
              in
              Some (make_node_list ~kind:Syntax_kind.TYPED_PATTERN children)
            else if at parser Token.Comma then
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

              let elements =
                parse_tuple_elements [ Ceibo.Green.Node first_pat ]
              in

              let close_paren = expect parser (Token.CloseDelim Token.Paren) in
              let children = open_paren :: (elements @ [ close_paren ]) in
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
                   (Diagnostic.InvalidSyntax
                      { context = "parenthesized pattern" })
                 ~span))

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

  let children = [ match_kw; scrutinee; with_kw ] @ all_cases in

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
              Some
                (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
          | None ->
              let children =
                prepend_pending_trivia parser [ backtick; tag_token ]
              in
              Some
                (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)
          )
      | _ ->
          let children =
            prepend_pending_trivia parser [ backtick; tag_token ]
          in
          Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children))
  | _ ->
      (* Missing or invalid tag *)
      let children = prepend_pending_trivia parser [ backtick ] in
      Some (make_node_list ~kind:Syntax_kind.POLY_VARIANT_PATTERN children)

and parse_record_pattern parser =
  let open_brace = consume parser in
  let _ = consume_trivia parser in

  (* Parse field patterns *)
  let fields = ref [] in

  let rec loop () =
    if at parser (Token.CloseDelim Token.Brace) then ()
    else
      match peek_kind parser with
      | Some Token.Underscore ->
          (* Wildcard pattern to ignore remaining fields: { x; _ } *)
          let wildcard = consume parser in
          fields := wildcard :: !fields;
          let _ = consume_trivia parser in
          (* Wildcard should be last, but consume semicolon if present *)
          if at parser Token.Semi then (
            let semi = consume parser in
            fields := semi :: !fields;
            let _ = consume_trivia parser in
            ())
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

  (* First, parse tuple if we see a comma *)
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
                (List.rev_append trivia (Ceibo.Green.Node pat :: comma :: acc))
          | None -> List.rev acc
      in
      let patterns = parse_tuple_patterns [ Ceibo.Green.Node first_pattern ] in
      Ceibo.Green.Node (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
    else Ceibo.Green.Node first_pattern
  in

  (* Then, check for or-pattern which can combine tuples or other patterns *)
  let _ = consume_trivia parser in
  let pattern =
    if at parser Token.Pipe && not (at_any parser [ Token.Arrow ]) then
      (* Or pattern - can combine tuples or other patterns *)
      let rec parse_or_patterns acc =
        if (not (at parser Token.Pipe)) || at_any parser [ Token.Arrow ] then
          List.rev acc
        else
          let pipe_tok = consume parser in
          let _ = consume_trivia parser in
          (* Parse next pattern which might also be a tuple *)
          match parse_pattern parser with
          | Some next_pat ->
              let _ = consume_trivia parser in
              (* Check if this pattern is also a tuple *)
              let next_pattern_with_tuple =
                if at parser Token.Comma then
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
                    parse_tuple_patterns [ Ceibo.Green.Node next_pat ]
                  in
                  Ceibo.Green.Node
                    (make_node_list ~kind:Syntax_kind.TUPLE_PATTERN patterns)
                else Ceibo.Green.Node next_pat
              in
              let trivia = consume_trivia parser in
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
  (* For .ml files (implementations): let, type, open, external, module *)
  let _ = consume_trivia parser in

  match peek_kind parser with
  | Some (Token.Keyword Keyword.Let) -> parse_let_binding parser
  | Some (Token.Keyword Keyword.Type) -> parse_type_decl parser
  | Some (Token.Keyword Keyword.Open) -> parse_open parser
  | Some (Token.Keyword Keyword.External) -> parse_external_decl parser
  | Some (Token.Keyword Keyword.Module) -> parse_module_decl_structure parser
  | _ -> None

and parse_let_binding parser =
  let let_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse optional attributes: [@inline], [@tailcall], etc. *)
  let rec parse_attributes acc =
    if at parser (Token.OpenDelim Token.Bracket) then
      (* Check if next token is @ (attribute) or @@ (item attribute) *)
      match peek_nth parser 1 with
      | Some Token.At ->
          let open_bracket = consume parser in
          let at_token = consume parser in
          let _ = consume_trivia parser in
          (* Consume attribute name and any following tokens until ] *)
          let rec consume_attr_tokens acc =
            if at parser (Token.CloseDelim Token.Bracket) || peek parser = None
            then List.rev acc
            else
              let tok = consume parser in
              let _ = consume_trivia parser in
              consume_attr_tokens (tok :: acc)
          in
          let attr_tokens = consume_attr_tokens [] in
          let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in
          let _ = consume_trivia parser in
          parse_attributes
            (List.rev_append [ open_bracket; at_token ]
               (attr_tokens @ [ close_bracket ])
            @ acc)
      | _ -> List.rev acc
    else List.rev acc
  in
  let attributes = parse_attributes [] in

  (* Check for 'let open' or 'let module' at structure level *)
  (* NOTE: 'let exception' is NOT allowed at top level - use 'exception' instead *)
  if at parser (Token.Keyword Keyword.Open) then
    parse_let_open_expr parser let_kw ~attributes ()
  else if at parser (Token.Keyword Keyword.Module) then
    parse_let_module_expr parser let_kw ~attributes ()
  else parse_regular_let_binding parser let_kw ~attributes ()

and parse_regular_let_binding parser let_kw ?(attributes = []) () =
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

  (* Check for operator name: let ( + ) = ... or let ( let* ) = ... *)
  let pattern =
    if at parser (Token.OpenDelim Token.Paren) then (
      let open_paren = consume parser in
      let trivia_after_open = consume_trivia parser in

      (* Check for unit pattern: () *)
      if at parser (Token.CloseDelim Token.Paren) then
        let close_paren = consume parser in
        let _ = consume_trivia parser in
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.UNIT_LITERAL ~text:"()"
             ~width:2)
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
              | Token.Percent | Token.StarStar | Token.At | Token.Caret
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
          let module_kw = consume parser in
          let _ = consume_trivia parser in
          let module_name = consume parser in
          let _ = consume_trivia parser in
          (* Check for optional type constraint *)
          let constraint_nodes =
            if at parser Token.Colon then
              let colon = consume parser in
              let _ = consume_trivia parser in
              (* Consume tokens until closing paren *)
              let rec consume_until_close acc =
                if at parser (Token.CloseDelim Token.Paren) then List.rev acc
                else
                  let tok = consume parser in
                  let _ = consume_trivia parser in
                  consume_until_close (tok :: acc)
              in
              let type_tokens = consume_until_close [] in
              [ colon ] @ type_tokens
            else []
          in
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let _ = consume_trivia parser in
          let all_tokens =
            [ open_paren ] @ trivia_after_open @ [ module_kw; module_name ]
            @ constraint_nodes @ [ close_paren ]
          in
          Ceibo.Green.Node
            (make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_tokens)
        else if is_operator_name then
          (* Parse as operator identifier - collect all tokens until ) *)
          let rec collect_operator_tokens acc =
            if at parser (Token.CloseDelim Token.Paren) then List.rev acc
            else
              let tok = consume parser in
              let trivia = consume_trivia parser in
              collect_operator_tokens (List.rev_append trivia (tok :: acc))
          in
          let op_tokens = collect_operator_tokens [] in
          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          let all_tokens =
            [ open_paren ] @ trivia_after_open @ op_tokens @ [ close_paren ]
          in
          let children = prepend_pending_trivia parser all_tokens in
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
              let _ = consume_trivia parser in
              (* Check if it's a tuple inside parens: (a, b) *)
              let paren_pattern =
                if at parser Token.Comma then
                  (* Tuple pattern inside parens *)
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
                    parse_tuple_patterns [ Ceibo.Green.Node inner_pat ]
                  in
                  let close_paren =
                    expect parser (Token.CloseDelim Token.Paren)
                  in
                  let all_children =
                    [ open_paren ] @ trivia_after_open @ patterns
                    @ [ close_paren ]
                  in
                  let _ = consume_trivia parser in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_children
                else if at parser Token.Colon then
                  (* Type annotation: (x : int) *)
                  let colon = consume parser in
                  let _ = consume_trivia parser in
                  (* Collect type tokens until ) *)
                  let rec collect_type_tokens acc =
                    if
                      at parser (Token.CloseDelim Token.Paren)
                      || peek parser = None
                    then List.rev acc
                    else
                      let tok = consume parser in
                      let _ = consume_trivia parser in
                      collect_type_tokens (tok :: acc)
                  in
                  let type_tokens = collect_type_tokens [] in
                  let close_paren =
                    expect parser (Token.CloseDelim Token.Paren)
                  in
                  let all_children =
                    [ open_paren ] @ trivia_after_open
                    @ [ Ceibo.Green.Node inner_pat; colon ]
                    @ type_tokens @ [ close_paren ]
                  in
                  let _ = consume_trivia parser in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN all_children
                else
                  (* Simple parenthesized pattern *)
                  let close_paren =
                    expect parser (Token.CloseDelim Token.Paren)
                  in
                  let _ = consume_trivia parser in
                  make_node_list ~kind:Syntax_kind.PAREN_PATTERN
                    (List.rev_append trivia_after_open
                       [ open_paren; Ceibo.Green.Node inner_pat; close_paren ])
              in
              (* Now check if this paren pattern is part of a larger tuple: (a, b), (c, d) *)
              if at parser Token.Comma then
                (* Tuple pattern with paren as first element *)
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
      match parse_pattern parser with
      | Some first_pat -> (
          let _ = consume_trivia parser in
          (* Check if followed by comma (tuple pattern) *)
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

  let _ = consume_trivia parser in

  (* Check if pattern is a simple identifier (function name) *)
  let is_simple_ident =
    match pattern with
    | Ceibo.Green.Token _ -> Ceibo.Green.kind pattern = Syntax_kind.IDENT_EXPR
    | Ceibo.Green.Node node -> (
        (* Check if it's an IDENT_PATTERN with a single IDENT_EXPR token *)
        match Ceibo.Green.children node with
        | [| Ceibo.Green.Token tok |] ->
            Ceibo.Green.kind (Ceibo.Green.Token tok) = Syntax_kind.IDENT_EXPR
        | _ -> false)
  in

  (* Check for optional type annotation: let f : int -> int = ... *)
  let type_annotation =
    if is_simple_ident && at parser Token.Colon then
      let colon = consume parser in
      let _ = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let tok = consume parser in
          let _ = consume_trivia parser in
          consume_type_tokens (tok :: acc)
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ type_tokens)
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
                  let _ = consume_trivia parser in
                  loop (Ceibo.Green.Node param :: acc)
              | None -> List.rev acc)
          | Some (Token.OpenDelim Token.Paren) -> (
              if
                (* Check if it's (type ...) for locally abstract types *)
                peek_nth parser 1 = Some (Token.Keyword Keyword.Type)
              then
                (* Locally abstract type: (type a) or (type a b c) *)
                let open_paren = consume parser in
                let _ = consume_trivia parser in
                let type_kw = consume parser in
                let _ = consume_trivia parser in
                (* Collect all type variables until ) *)
                let rec collect_type_vars acc =
                  if at parser (Token.CloseDelim Token.Paren) then List.rev acc
                  else
                    let type_var = consume parser in
                    let _ = consume_trivia parser in
                    collect_type_vars (type_var :: acc)
                in
                let type_vars = collect_type_vars [] in
                let close_paren =
                  expect parser (Token.CloseDelim Token.Paren)
                in
                let _ = consume_trivia parser in
                let param =
                  make_node_list ~kind:Syntax_kind.TYPE_PARAM
                    ([ open_paren; type_kw ] @ type_vars @ [ close_paren ])
                in
                loop (Ceibo.Green.Node param :: acc)
              else
                (* Regular pattern *)
                match parse_pattern parser with
                | Some pat ->
                    let _ = consume_trivia parser in
                    loop (Ceibo.Green.Node pat :: acc)
                | None -> List.rev acc)
          | Some (Token.Ident _) -> (
              (* Check if this identifier is followed by tokens that suggest it's
                 an expression (function application) rather than a parameter *)
              let looks_like_application =
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
                    let _ = consume_trivia parser in
                    loop (Ceibo.Green.Node pat :: acc)
                | None -> List.rev acc)
          | Some Token.Underscore
          | Some (Token.Literal _)
          | Some (Token.OpenDelim Token.Bracket)
          | Some (Token.OpenDelim Token.Brace) -> (
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

  (* Check for return type annotation after parameters: let f (x : int) : int = ... *)
  let return_type_annotation =
    if params <> [] && at parser Token.Colon then
      let colon = consume parser in
      let _ = consume_trivia parser in
      (* Parse type tokens until '=' *)
      let rec consume_type_tokens acc =
        if at parser Token.Eq || peek parser = None then List.rev acc
        else
          let tok = consume parser in
          let _ = consume_trivia parser in
          consume_type_tokens (tok :: acc)
      in
      let type_tokens = consume_type_tokens [] in
      Some ([ colon ] @ type_tokens)
    else None
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

  (* If we have params, wrap expr in a fun expression *)
  let final_expr =
    if params = [] then expr
    else
      let arrow =
        Ceibo.Green.Token
          (Ceibo.Green.make_token ~kind:Syntax_kind.FUN_EXPR ~text:"->" ~width:2)
      in
      let children = params @ [ arrow; expr ] in
      Ceibo.Green.Node (make_node_list ~kind:Syntax_kind.FUN_EXPR children)
  in

  let type_annot_tokens =
    match type_annotation with Some tokens -> tokens | None -> []
  in

  let return_type_annot_tokens =
    match return_type_annotation with Some tokens -> tokens | None -> []
  in

  (* Check if this is a let expression (has 'in' keyword) *)
  if at parser (Token.Keyword Keyword.In) then
    (* This is a let expression: let x = expr in body *)
    let in_kw = consume parser in
    let _ = consume_trivia parser in
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
              @ params @ return_type_annot_tokens
              @ [ eq; final_expr; in_kw; Ceibo.Green.Node body ]
          | None ->
              [ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
              @ return_type_annot_tokens
              @ [ eq; final_expr; in_kw; Ceibo.Green.Node body ]
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
              @ params @ return_type_annot_tokens @ [ eq; final_expr; in_kw ]
          | None ->
              [ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
              @ return_type_annot_tokens @ [ eq; final_expr; in_kw ]
        in
        Some (make_node_list ~kind children)
  else
    (* This is a let binding: let x = expr *)
    match rec_kw with
    | Some kw ->
        Some
          (make_node_list ~kind:Syntax_kind.LET_BINDING
             ([ let_kw ] @ attributes @ [ kw; pattern ] @ type_annot_tokens
            @ params @ return_type_annot_tokens @ [ eq; final_expr ]))
    | None ->
        Some
          (make_node_list ~kind:Syntax_kind.LET_BINDING
             ([ let_kw ] @ attributes @ [ pattern ] @ type_annot_tokens @ params
            @ return_type_annot_tokens @ [ eq; final_expr ]))

and parse_type_decl parser =
  (* type 'a t = ... | type t += ... | type t *)
  let type_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse type parameters like 'a, _, or ('a, 'b) *)
  let params = parse_type_params parser in

  (* Parse type name (can be module path like Effect.t or Message.t) *)
  let type_name_parts = parse_type_name parser in

  (* Check what comes after the name: +=, =, or nothing *)
  match peek_kind parser with
  | Some Token.Plus when peek_nth parser 1 = Some Token.Eq ->
      (* Extensible type: type t += A | B *)
      let plus = consume parser in
      let _ = consume_trivia parser in
      let eq = consume parser in
      let _ = consume_trivia parser in

      let type_body = parse_variant_type parser in

      let children =
        match params with
        | Some p ->
            type_kw :: Ceibo.Green.Node p
            :: (type_name_parts @ [ plus; eq; Ceibo.Green.Node type_body ])
        | None ->
            type_kw
            :: (type_name_parts @ [ plus; eq; Ceibo.Green.Node type_body ])
      in

      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)
  | Some Token.Eq ->
      (* Regular type definition: type t = ... *)
      let eq = consume parser in
      let _ = consume_trivia parser in

      let type_body = parse_type_decl_body parser in

      let children =
        match params with
        | Some p ->
            type_kw :: Ceibo.Green.Node p
            :: (type_name_parts @ [ eq; Ceibo.Green.Node type_body ])
        | None ->
            type_kw :: (type_name_parts @ [ eq; Ceibo.Green.Node type_body ])
      in

      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)
  | _ ->
      (* Abstract type (no = present, used in signatures): type t *)
      let children =
        match params with
        | Some p -> type_kw :: Ceibo.Green.Node p :: type_name_parts
        | None -> type_kw :: type_name_parts
      in
      Some (make_node_list ~kind:Syntax_kind.TYPE_DECL children)

and parse_type_name parser =
  (* Parse type name, which can be a module path: t or Effect.t or Message.t *)
  parse_identifier parser

and parse_type_decl_body parser =
  (* Determine what kind of type body this is *)
  match peek_kind parser with
  | Some Token.DotDot ->
      (* Extensible variant type: .. *)
      let dotdot = consume parser in
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
      parse_type_expr parser

and parse_type_params parser =
  (* Handle 'a or ('a, 'b) or _ or no params *)
  match peek_kind parser with
  | Some Token.Underscore ->
      (* Wildcard type parameter: _ *)
      let underscore = consume parser in
      let _ = consume_trivia parser in
      Some (make_node_list ~kind:Syntax_kind.TYPE_PARAM [ underscore ])
  | Some Token.Plus | Some Token.Minus -> (
      (* Variance annotation: +'a or -'a *)
      let variance = consume parser in
      let _ = consume_trivia parser in
      match peek_kind parser with
      | Some Token.Quote ->
          let quote = consume parser in
          let _ = consume_trivia parser in
          let name = consume parser in
          let _ = consume_trivia parser in
          Some
            (make_node_list ~kind:Syntax_kind.TYPE_PARAM
               [ variance; quote; name ])
      | _ -> None)
  | Some Token.Quote ->
      (* Single type variable: 'a *)
      let quote = consume parser in
      let _ = consume_trivia parser in
      let name = consume parser in
      let _ = consume_trivia parser in
      Some (make_node_list ~kind:Syntax_kind.TYPE_PARAM [ quote; name ])
  | Some (Token.OpenDelim Token.Paren) ->
      (* Multiple type variables: ('a, 'b) *)
      let open_paren = consume parser in
      let _ = consume_trivia parser in

      let rec parse_param_list acc =
        match peek_kind parser with
        | Some Token.Plus | Some Token.Minus -> (
            (* Variance annotation: +' or -' *)
            let variance = consume parser in
            let _ = consume_trivia parser in
            match peek_kind parser with
            | Some Token.Quote ->
                let quote = consume parser in
                let _ = consume_trivia parser in
                let name = consume parser in
                let _ = consume_trivia parser in
                let param =
                  make_node_list ~kind:Syntax_kind.TYPE_PARAM
                    [ variance; quote; name ]
                in

                (* Check for comma *)
                if at parser Token.Comma then
                  let comma = consume parser in
                  let _ = consume_trivia parser in
                  parse_param_list (Ceibo.Green.Node param :: comma :: acc)
                else List.rev (Ceibo.Green.Node param :: acc)
            | _ -> List.rev acc)
        | Some Token.Quote ->
            let quote = consume parser in
            let _ = consume_trivia parser in
            let name = consume parser in
            let _ = consume_trivia parser in
            let param =
              make_node_list ~kind:Syntax_kind.TYPE_PARAM [ quote; name ]
            in

            (* Check for comma *)
            if at parser Token.Comma then
              let comma = consume parser in
              let _ = consume_trivia parser in
              parse_param_list (Ceibo.Green.Node param :: comma :: acc)
            else List.rev (Ceibo.Green.Node param :: acc)
        | _ -> List.rev acc
      in

      let params = parse_param_list [] in
      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let _ = consume_trivia parser in

      Some
        (make_node_list ~kind:Syntax_kind.TYPE_PARAMS
           ((open_paren :: params) @ [ close_paren ]))
  | _ -> None

and parse_type_expr parser =
  (* Type expressions with proper precedence:
     - Arrow types: int -> string (right-associative, higher precedence)
     - Tuple types: int * string (left-associative, lower precedence)
     - Atomic types: int, 'a, (int -> string)
  *)
  parse_type_arrow parser

and parse_type_arrow parser =
  (* Parse arrow types (right-associative): int -> string -> bool 
     Also handles labeled/optional params: ?x:int -> string or ~label:int -> string *)
  let _ = consume_trivia parser in

  (* Check for labeled or optional parameter *)
  let left =
    match peek_kind parser with
    | Some Token.Question -> (
        (* Optional parameter: ?label:type *)
        let question = consume parser in
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let label = consume parser in
            let _ = consume_trivia parser in
            if at parser Token.Colon then
              let colon = consume parser in
              let _ = consume_trivia parser in
              let typ = parse_type_tuple parser in
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                [ question; label; colon; Ceibo.Green.Node typ ]
            else
              (* Just ?label without type *)
              make_node_list ~kind:Syntax_kind.TYPE_PARAM [ question; label ]
        | _ ->
            (* Malformed optional param *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAM [ question ])
    | Some Token.Tilde -> (
        (* Labeled parameter: ~label:type *)
        let tilde = consume parser in
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some (Token.Ident _) ->
            let label = consume parser in
            let _ = consume_trivia parser in
            if at parser Token.Colon then
              let colon = consume parser in
              let _ = consume_trivia parser in
              let typ = parse_type_tuple parser in
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                [ tilde; label; colon; Ceibo.Green.Node typ ]
            else
              (* Just ~label without type *)
              make_node_list ~kind:Syntax_kind.TYPE_PARAM [ tilde; label ]
        | _ ->
            (* Malformed labeled param *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAM [ tilde ])
    | _ ->
        (* Regular type *)
        parse_type_tuple parser
  in
  let _ = consume_trivia parser in

  (* Check if we have a labeled parameter without tilde: label:type -> ... *)
  let left =
    match peek_kind parser with
    | Some Token.Colon ->
        (* Check if left is a simple identifier (TYPE_CONSTR with single ident) *)
        let children = Ceibo.Green.children left in
        if Array.length children = 1 then
          match children.(0) with
          | Ceibo.Green.Token _ as label_tok ->
              (* It's a simple identifier followed by :, reparse as labeled param *)
              let colon = consume parser in
              let _ = consume_trivia parser in
              let typ = parse_type_tuple parser in
              make_node_list ~kind:Syntax_kind.TYPE_PARAM
                [ label_tok; colon; Ceibo.Green.Node typ ]
          | _ ->
              (* Complex type followed by :, not a labeled param *)
              left
        else
          (* Multiple children, not a simple identifier *)
          left
    | _ -> left
  in
  let _ = consume_trivia parser in

  match peek_kind parser with
  | Some Token.Arrow ->
      let arrow = consume parser in
      let _ = consume_trivia parser in
      let right = parse_type_arrow parser in
      (* Right-associative recursion *)
      make_node_list ~kind:Syntax_kind.TYPE_ARROW
        [ Ceibo.Green.Node left; arrow; Ceibo.Green.Node right ]
  | _ -> left

and parse_type_tuple parser =
  (* Parse tuple types (left-associative): int * string * bool *)
  let first = parse_type_atomic parser in
  let _ = consume_trivia parser in

  match peek_kind parser with
  | Some Token.Star ->
      let rec collect_tuple_parts acc =
        let _ = consume_trivia parser in
        match peek_kind parser with
        | Some Token.Star ->
            let star = consume parser in
            let _ = consume_trivia parser in
            let next = parse_type_atomic parser in
            collect_tuple_parts (Ceibo.Green.Node next :: star :: acc)
        | _ -> List.rev acc
      in
      let star = consume parser in
      let _ = consume_trivia parser in
      let second = parse_type_atomic parser in
      let parts =
        collect_tuple_parts
          [ Ceibo.Green.Node second; star; Ceibo.Green.Node first ]
      in
      make_node_list ~kind:Syntax_kind.TYPE_TUPLE parts
  | _ -> first

and parse_type_atomic parser =
  (* Parse atomic type expressions with optional type application:
     - Type variables: 'a
     - Type constructors: int, string
     - Type application: 'a list, int option, ('a, 'b) result
     - Parenthesized types: (int -> string)
  *)
  let _ = consume_trivia parser in

  let base_type =
    match peek_kind parser with
    | Some Token.Quote ->
        (* Type variable: 'a *)
        let quote = consume parser in
        let _ = consume_trivia parser in
        let name = consume parser in
        make_node_list ~kind:Syntax_kind.TYPE_VAR [ quote; name ]
    | Some Token.Underscore ->
        (* Wildcard type: _ *)
        let underscore = consume parser in
        make_node_list ~kind:Syntax_kind.TYPE_VAR [ underscore ]
    | Some (Token.Ident _) ->
        (* Type constructor: int, string, list, or Module.path.t *)
        let path_parts = parse_identifier parser in
        make_node_list ~kind:Syntax_kind.TYPE_CONSTR path_parts
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
        let open_paren = consume parser in
        let _ = consume_trivia parser in

        (* Check for (module ...) first-class module type *)
        if at parser (Token.Keyword Keyword.Module) then
          let module_kw = consume parser in
          let _ = consume_trivia parser in

          (* Parse module type expression *)
          let module_type = parse_module_type_expr parser in
          let _ = consume_trivia parser in

          let close_paren = expect parser (Token.CloseDelim Token.Paren) in
          make_node_list ~kind:Syntax_kind.TYPE_CONSTR
            [ open_paren; module_kw; Ceibo.Green.Node module_type; close_paren ]
        else
          let first = parse_type_expr parser in
          let _ = consume_trivia parser in

          (* Check if this is a tuple of type args (comma follows) *)
          if at parser Token.Comma then
            (* Multiple type arguments: (int, string) *)
            let rec collect_args acc =
              let _ = consume_trivia parser in
              if at parser Token.Comma then
                let comma = consume parser in
                let _ = consume_trivia parser in
                let next = parse_type_expr parser in
                collect_args (Ceibo.Green.Node next :: comma :: acc)
              else List.rev acc
            in
            let comma = consume parser in
            let _ = consume_trivia parser in
            let second = parse_type_expr parser in
            let args =
              collect_args
                [ Ceibo.Green.Node second; comma; Ceibo.Green.Node first ]
            in
            let _ = consume_trivia parser in
            let close_paren = expect parser (Token.CloseDelim Token.Paren) in
            (* Return the args tuple - will be used for type application *)
            make_node_list ~kind:Syntax_kind.TYPE_PARAMS
              ((open_paren :: args) @ [ close_paren ])
          else
            (* Single parenthesized type: (int -> string) *)
            let close_paren = expect parser (Token.CloseDelim Token.Paren) in
            make_node_list ~kind:Syntax_kind.TYPE_PAREN
              [ open_paren; Ceibo.Green.Node first; close_paren ]
    | _ ->
        (* Error: couldn't parse type *)
        make_node_list ~kind:Syntax_kind.ERROR []
  in

  let _ = consume_trivia parser in

  (* Check for type application: 'a list, (int, string) result *)
  (* Can be chained: 'a tree list, int option list *)
  (* Also handles module paths: 'a Queue.t, Message.envelope Queue.t *)
  let rec parse_type_applications current_type =
    match peek_kind parser with
    | Some (Token.Ident _) ->
        (* Type application: current_type constructor_name *)
        (* This can be a simple constructor (list, option) or module path (Queue.t, Stdlib.List.t) *)
        let constructor_path = parse_identifier parser in
        let _ = consume_trivia parser in
        let applied_type =
          make_node_list ~kind:Syntax_kind.TYPE_CONSTR
            (Ceibo.Green.Node current_type :: constructor_path)
        in
        (* Check for more applications *)
        parse_type_applications applied_type
    | _ -> current_type
  in

  parse_type_applications base_type

and parse_variant_type parser =
  (* Parse variant type: A | B | C of int | D of string * bool *)
  let _ = consume_trivia parser in

  (* Optional leading pipe *)
  let leading_pipe =
    if at parser Token.Pipe then Some (consume parser) else None
  in
  let _ = consume_trivia parser in

  let rec parse_constructors acc =
    match peek_kind parser with
    | Some (Token.Ident tag)
      when String.length tag > 0 && Char.uppercase_ascii tag.[0] = tag.[0] ->
        (* Constructor name *)
        let constructor_name = consume parser in
        let _ = consume_trivia parser in

        (* Check for payload: 'of type' or GADT ': type' *)
        let payload =
          if at parser Token.Colon then
            (* GADT syntax: Constructor : type *)
            let colon = consume parser in
            let _ = consume_trivia parser in
            let gadt_type = parse_type_expr parser in
            Some [ colon; Ceibo.Green.Node gadt_type ]
          else if at parser (Token.Keyword Keyword.Of) then
            (* Regular syntax: Constructor of type *)
            let of_kw = consume parser in
            let _ = consume_trivia parser in
            let payload_type = parse_type_expr parser in
            Some [ of_kw; Ceibo.Green.Node payload_type ]
          else None
        in

        let constructor_parts =
          match payload with
          | Some parts -> constructor_name :: parts
          | None -> [ constructor_name ]
        in

        let constructor =
          make_node_list ~kind:Syntax_kind.TYPE_VARIANT_CONSTR constructor_parts
        in

        let _ = consume_trivia parser in

        (* Check for more constructors *)
        if at parser Token.Pipe then
          let pipe = consume parser in
          let _ = consume_trivia parser in
          parse_constructors (Ceibo.Green.Node constructor :: pipe :: acc)
        else List.rev (Ceibo.Green.Node constructor :: acc)
    | _ -> List.rev acc
  in

  let constructors = parse_constructors [] in
  let all_parts =
    match leading_pipe with
    | Some pipe -> pipe :: constructors
    | None -> constructors
  in

  make_node_list ~kind:Syntax_kind.TYPE_CONSTR all_parts

and parse_poly_variant_type parser =
  (* Parse polymorphic variant type: [ `A | `B of int ] or [> `A ] or [< `A ] *)
  let open_bracket = consume parser in
  let _ = consume_trivia parser in

  (* Check for open [> or closed [< *)
  let variance =
    if at parser Token.Gt then Some (consume parser)
    else if at parser Token.Lt then Some (consume parser)
    else None
  in

  let _ = consume_trivia parser in

  let rec parse_variants acc =
    match peek_kind parser with
    | Some Token.Backtick ->
        (* Variant constructor: `A or `B of int *)
        let backtick = consume parser in
        let _ = consume_trivia parser in

        (* Constructor name *)
        let name = consume parser in
        let _ = consume_trivia parser in

        (* Check for payload: of type *)
        let payload =
          if at parser (Token.Keyword Keyword.Of) then
            let of_kw = consume parser in
            let _ = consume_trivia parser in
            let payload_type = parse_type_expr parser in
            Some [ of_kw; Ceibo.Green.Node payload_type ]
          else None
        in

        let variant_parts =
          match payload with
          | Some parts -> backtick :: name :: parts
          | None -> [ backtick; name ]
        in

        let variant =
          make_node_list ~kind:Syntax_kind.TYPE_VARIANT_CONSTR variant_parts
        in

        let _ = consume_trivia parser in

        (* Check for more variants *)
        if at parser Token.Pipe then
          let pipe = consume parser in
          let _ = consume_trivia parser in
          parse_variants (Ceibo.Green.Node variant :: pipe :: acc)
        else List.rev (Ceibo.Green.Node variant :: acc)
    | Some Token.Pipe ->
        (* Leading pipe *)
        let pipe = consume parser in
        let _ = consume_trivia parser in
        parse_variants (pipe :: acc)
    | _ -> List.rev acc
  in

  let variants = parse_variants [] in
  let _ = consume_trivia parser in

  let close_bracket = expect parser (Token.CloseDelim Token.Bracket) in

  let children =
    match variance with
    | Some var -> (open_bracket :: var :: variants) @ [ close_bracket ]
    | None -> (open_bracket :: variants) @ [ close_bracket ]
  in

  make_node_list ~kind:Syntax_kind.TYPE_POLY_VARIANT children

and parse_record_type parser =
  (* Parse record type: { field1: int; field2: string } *)
  let open_brace = consume parser in
  let _ = consume_trivia parser in

  let rec parse_fields acc =
    match peek_kind parser with
    | Some (Token.Keyword Keyword.Mutable) | Some (Token.Ident _) ->
        (* Optional mutable keyword *)
        let mutable_kw =
          if at parser (Token.Keyword Keyword.Mutable) then
            let kw = consume parser in
            let _ = consume_trivia parser in
            Some kw
          else None
        in

        (* Field name *)
        let field_name = consume parser in
        let _ = consume_trivia parser in

        (* Expect : *)
        let colon = expect parser Token.Colon in
        let _ = consume_trivia parser in

        (* Parse field type *)
        let field_type = parse_type_expr parser in
        let _ = consume_trivia parser in

        let field_parts =
          match mutable_kw with
          | Some kw -> [ kw; field_name; colon; Ceibo.Green.Node field_type ]
          | None -> [ field_name; colon; Ceibo.Green.Node field_type ]
        in

        let field =
          make_node_list ~kind:Syntax_kind.TYPE_RECORD_FIELD field_parts
        in

        (* Check for semicolon or more fields *)
        if at parser Token.Semi then
          let semi = consume parser in
          let _ = consume_trivia parser in
          parse_fields (Ceibo.Green.Node field :: semi :: acc)
        else List.rev (Ceibo.Green.Node field :: acc)
    | _ -> List.rev acc
  in

  let fields = parse_fields [] in
  let _ = consume_trivia parser in

  (* Optional trailing semicolon *)
  let trailing_semi =
    if at parser Token.Semi then Some (consume parser) else None
  in
  let _ = consume_trivia parser in

  let close_brace = expect parser (Token.CloseDelim Token.Brace) in

  let all_parts =
    match trailing_semi with
    | Some semi -> (open_brace :: fields) @ [ semi; close_brace ]
    | None -> (open_brace :: fields) @ [ close_brace ]
  in

  make_node_list ~kind:Syntax_kind.TYPE_CONSTR all_parts

and parse_open parser =
  let open_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse module path: Unix, Unix.File, A.B.C *)
  let path = parse_identifier parser in

  Some (make_node_list ~kind:Syntax_kind.OPEN_STMT (open_kw :: path))

and parse_val_decl parser =
  (* val name : type or val ( op ) : type *)
  let val_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse value name - could be identifier or operator in parentheses *)
  let name_tokens =
    if at parser (Token.OpenDelim Token.Paren) then
      (* Operator name: ( op ) *)
      let open_paren = consume parser in
      let _ = consume_trivia parser in

      (* Collect operator tokens until closing paren *)
      let rec collect_op_tokens acc =
        if at parser (Token.CloseDelim Token.Paren) then List.rev acc
        else
          let tok = consume parser in
          let _ = consume_trivia parser in
          collect_op_tokens (tok :: acc)
      in
      let op_tokens = collect_op_tokens [] in
      let close_paren = consume parser in
      let _ = consume_trivia parser in
      [ open_paren ] @ op_tokens @ [ close_paren ]
    else
      (* Regular identifier *)
      let name = consume parser in
      let _ = consume_trivia parser in
      [ name ]
  in

  (* Expect : *)
  let colon = expect parser Token.Colon in
  let _ = consume_trivia parser in

  (* Parse type expression *)
  let type_expr = parse_type_expr parser in

  Some
    (make_node_list ~kind:Syntax_kind.VAL_DECL
       ([ val_kw ] @ name_tokens @ [ colon; Ceibo.Green.Node type_expr ]))

and parse_external_decl parser =
  (* external name : type = "c_name" *)
  let external_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse function name *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Expect : *)
  let colon = expect parser Token.Colon in
  let _ = consume_trivia parser in

  (* Parse type expression *)
  let type_expr = parse_type_expr parser in
  let _ = consume_trivia parser in

  (* Expect = *)
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

  (* Parse C function names (one or more string literals) *)
  let rec parse_c_names acc =
    match peek_kind parser with
    | Some (Token.Literal (Token.String _)) ->
        let str = consume parser in
        let _ = consume_trivia parser in
        parse_c_names (str :: acc)
    | _ -> List.rev acc
  in

  let c_names = parse_c_names [] in

  Some
    (make_node_list ~kind:Syntax_kind.EXTERNAL_DECL
       ([ external_kw; name; colon; Ceibo.Green.Node type_expr; eq ] @ c_names))

and parse_module_decl_structure parser =
  (* For .ml files: module M = struct ... end  OR  module type S = sig ... end *)
  let module_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check if this is a module type declaration *)
  if at parser (Token.Keyword Keyword.Type) then
    parse_module_type_decl parser module_kw
  else
    (* Regular module declaration: module M = ... OR module M (X : S) = ... *)
    parse_regular_module_decl_structure parser module_kw

and parse_module_decl_signature parser =
  (* For .mli files: module M : sig ... end  OR  module type S = sig ... end *)
  let module_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check if this is a module type declaration *)
  if at parser (Token.Keyword Keyword.Type) then
    parse_module_type_decl parser module_kw
  else
    (* Module signature: module M : S  OR  module M (X : S) : S *)
    parse_regular_module_decl_signature parser module_kw

and parse_module_type_decl parser module_kw =
  (* module type S = sig ... end *)
  let type_kw = consume parser in
  let _ = consume_trivia parser in

  (* Parse module type name *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Expect = *)
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

  (* Parse signature *)
  let signature = parse_signature parser in

  Some
    (make_node_list ~kind:Syntax_kind.MODULE_TYPE_DECL
       [ module_kw; type_kw; name; eq; Ceibo.Green.Node signature ])

and parse_signature parser =
  (* sig ... end *)
  let sig_kw = consume parser in
  (* We know it's 'sig' from caller context *)
  let _ = consume_trivia parser in

  (* Parse signature items until 'end' *)
  let rec parse_sig_items acc =
    if at parser (Token.CloseDelim Token.SigEnd) then List.rev acc
    else
      match parse_signature_item parser with
      | Some item ->
          let _ = consume_trivia parser in
          parse_sig_items (Ceibo.Green.Node item :: acc)
      | None ->
          (* Skip if we can't parse this item *)
          if at parser (Token.CloseDelim Token.SigEnd) || peek parser = None
          then List.rev acc
          else
            let _ = advance parser in
            parse_sig_items acc
  in

  let items = parse_sig_items [] in

  let _ = consume_trivia parser in
  let end_kw = consume parser in
  (* Consume 'end' keyword *)

  let children =
    prepend_pending_trivia parser ([ sig_kw ] @ items @ [ end_kw ])
  in
  make_node_list ~kind:Syntax_kind.SIGNATURE children

and parse_regular_module_decl_structure parser module_kw =
  (* For .ml files: module M = ... OR module M (X : S) = ... (functor) *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Check for functor parameters: (X : S) or (X : S with type t = int) *)
  let rec parse_functor_params acc =
    if at parser (Token.OpenDelim Token.Paren) then
      let open_paren = consume parser in
      let _ = consume_trivia parser in

      (* Parse parameter name *)
      let param_name = consume parser in
      let _ = consume_trivia parser in

      (* Expect : *)
      let colon = consume parser in
      let _ = consume_trivia parser in

      (* Parse module type expression (can be S or S with type t = int) *)
      let module_type = parse_module_type_expr parser in
      let _ = consume_trivia parser in

      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let _ = consume_trivia parser in

      let param =
        make_node_list ~kind:Syntax_kind.TYPE_PARAM
          [
            open_paren;
            param_name;
            colon;
            Ceibo.Green.Node module_type;
            close_paren;
          ]
      in

      parse_functor_params (Ceibo.Green.Node param :: acc)
    else List.rev acc
  in

  let params = parse_functor_params [] in

  (* Check for optional module type constraint: : S or : sig ... end or : S with type t = int *)
  let constraint_opt =
    if at parser Token.Colon then
      let colon = consume parser in
      let _ = consume_trivia parser in
      (* Parse module type expression (handles signatures, identifiers, and 'with' constraints) *)
      let module_type = parse_module_type_expr parser in
      let _ = consume_trivia parser in
      Some [ colon; Ceibo.Green.Node module_type ]
    else None
  in

  (* Expect = (always required in .ml files) *)
  let eq = expect parser Token.Eq in
  let _ = consume_trivia parser in

  (* Parse module expression (struct...end, or identifier, or functor application) *)
  let module_expr =
    match peek_kind parser with
    | Some (Token.OpenDelim Token.StructEnd) ->
        (* struct ... end *)
        let struct_kw = consume parser in
        let _ = consume_trivia parser in

        (* Parse structure items until 'end' *)
        let rec parse_struct_items acc =
          if at parser (Token.CloseDelim Token.StructEnd) then List.rev acc
          else
            match parse_structure_item parser with
            | Some item ->
                let _ = consume_trivia parser in
                parse_struct_items (Ceibo.Green.Node item :: acc)
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
        let _ = consume_trivia parser in
        let end_kw = consume parser in

        make_node_list ~kind:Syntax_kind.STRUCTURE
          ([ struct_kw ] @ items @ [ end_kw ])
    | _ ->
        (* Module identifier or functor application: M or F(X) *)
        let path = parse_identifier parser in
        make_node_list ~kind:Syntax_kind.IDENT_EXPR path
  in

  let children =
    let base =
      match params with
      | [] -> [ module_kw; name ]
      | _ -> [ module_kw; name ] @ params
    in
    let with_constraint =
      match constraint_opt with
      | None -> base
      | Some constraint_tokens -> base @ constraint_tokens
    in
    with_constraint @ [ eq; Ceibo.Green.Node module_expr ]
  in

  Some (make_node_list ~kind:Syntax_kind.MODULE_DECL children)

and parse_regular_module_decl_signature parser module_kw =
  (* For .mli files: module M : S  OR  module M (X : S) : S *)
  let name = consume parser in
  let _ = consume_trivia parser in

  (* Check for functor parameters: (X : S) or (X : S with type t = int) *)
  let rec parse_functor_params acc =
    if at parser (Token.OpenDelim Token.Paren) then
      let open_paren = consume parser in
      let _ = consume_trivia parser in

      (* Parse parameter name *)
      let param_name = consume parser in
      let _ = consume_trivia parser in

      (* Expect : *)
      let colon = consume parser in
      let _ = consume_trivia parser in

      (* Parse module type expression (can be S or S with type t = int) *)
      let module_type = parse_module_type_expr parser in
      let _ = consume_trivia parser in

      let close_paren = expect parser (Token.CloseDelim Token.Paren) in
      let _ = consume_trivia parser in

      let param =
        make_node_list ~kind:Syntax_kind.TYPE_PARAM
          [
            open_paren;
            param_name;
            colon;
            Ceibo.Green.Node module_type;
            close_paren;
          ]
      in

      parse_functor_params (Ceibo.Green.Node param :: acc)
    else List.rev acc
  in

  let params = parse_functor_params [] in

  (* In .mli files, module declarations can be:
     - Module type ascription: module M : S  or  module M : sig ... end
     - Module alias: module M = OtherModule
  *)
  let children =
    if at parser Token.Colon then
      (* Module type ascription: : S or : sig ... end *)
      let colon = consume parser in
      let _ = consume_trivia parser in

      (* Parse module type expression (handles signatures, identifiers, and 'with' constraints) *)
      let module_type = parse_module_type_expr parser in
      let _ = consume_trivia parser in

      prepend_pending_trivia parser
        ([ module_kw; name ] @ params @ [ colon; Ceibo.Green.Node module_type ])
    else if at parser Token.Eq then
      (* Module alias: = M *)
      let eq = consume parser in
      let _ = consume_trivia parser in

      (* Parse module path (e.g., Build or Std.Path) - just consume as identifier for now *)
      let module_id = consume parser in
      let _ = consume_trivia parser in

      prepend_pending_trivia parser
        ([ module_kw; name ] @ params @ [ eq; module_id ])
    else
      (* Malformed: missing : or = *)
      let missing = expect parser Token.Colon in
      prepend_pending_trivia parser ([ module_kw; name ] @ params @ [ missing ])
  in

  Some (make_node_list ~kind:Syntax_kind.MODULE_DECL children)

and parse_signature_item parser =
  (* Signature items: type, val, external, exception, module, etc. *)
  let _ = consume_trivia parser in

  match peek_kind parser with
  | Some (Token.Keyword Keyword.Type) -> parse_type_decl parser
  | Some (Token.Keyword Keyword.Val) -> parse_val_decl parser
  | Some (Token.Keyword Keyword.External) -> parse_external_decl parser
  | Some (Token.Keyword Keyword.Include) -> parse_include parser
  | Some (Token.Keyword Keyword.Module) -> parse_module_decl_signature parser
  | _ -> None

and parse_include parser =
  (* include Module  OR  include module type of Module *)
  let include_kw = consume parser in
  let _ = consume_trivia parser in

  (* Check if this is 'include module type of' *)
  if at parser (Token.Keyword Keyword.Module) then
    (* Might be 'include module type of' *)
    let module_kw = consume parser in
    let _ = consume_trivia parser in

    if at parser (Token.Keyword Keyword.Type) then
      (* It is 'include module type of' *)
      let type_kw = consume parser in
      let _ = consume_trivia parser in

      let of_kw = consume parser in
      (* Expect 'of' keyword *)
      let _ = consume_trivia parser in

      (* Parse module path after 'of' *)
      let path = parse_identifier parser in
      let children = include_kw :: module_kw :: type_kw :: of_kw :: path in
      Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)
    else
      (* Just 'include module' - treat module as start of path *)
      let path = parse_identifier parser in
      let children = include_kw :: module_kw :: path in
      Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)
  else
    (* Simple include: include Module.Path *)
    let path = parse_identifier parser in
    let children = include_kw :: path in
    Some (make_node_list ~kind:Syntax_kind.INCLUDE_STMT children)

let parse_implementation parser =
  (* Parse .ml file (implementation) *)
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

let parse_interface parser =
  (* Parse .mli file (interface/signature) *)
  let rec parse_items acc =
    if peek parser = None || at parser Token.EOF then List.rev acc
    else
      match parse_signature_item parser with
      | Some item -> parse_items (Ceibo.Green.Node item :: acc)
      | None ->
          (* Skip problematic token *)
          let _ = advance parser in
          parse_items acc
  in

  let items = parse_items [] in

  make_node_list ~kind:Syntax_kind.SOURCE_FILE items

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
