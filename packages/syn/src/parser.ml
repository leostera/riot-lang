open Std

let span start_pos stream =
  let start_pos, end_pos = Parse_stream.span start_pos stream in
  { Ast.start_pos; Ast.end_pos }

(* Helper to skip whitespace and comments *)
let rec skip_trivia stream =
  match Parse_stream.peek stream with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Whitespace | Token.Comment _ | Token.Docstring _ ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          skip_trivia stream
      | _ -> stream)
  | None -> stream

(* Parse literals *)
let parse_literal stream =
  let stream = skip_trivia stream in
  match Parse_stream.peek stream with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Literal (Token.Int i) ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.Int i, stream)
      | Token.Literal (Token.Float f) ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.Float f, stream)
      | Token.Literal (Token.String { value; _ }) ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.String value, stream)
      | Token.Literal (Token.Char c) ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.Char c, stream)
      | Token.Keyword Token.True ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.Bool true, stream)
      | Token.Keyword Token.False ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok (Ast.Bool false, stream)
      | Token.OpenDelim Token.Paren ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          let stream = skip_trivia stream in
          (match Parse_stream.parse_token stream (Token.CloseDelim Token.Paren) with
           | Ok (_, stream) -> Ok (Ast.Unit, stream)
           | Error e -> Error e)
      | _ ->
          Error {
            Parse_stream.message = "Expected literal";
            position = tok.Token.span.start;
            expected = None;
          })
  | None ->
      Error {
        Parse_stream.message = "Expected literal";
        position = Parse_stream.position stream;
        expected = None;
      }

(* Forward declarations for mutual recursion *)
let parse_expr_ref = ref (fun _ -> Error { Parse_stream.message = "Not implemented"; position = 0; expected = None })

(* Parse primary expressions *)
let parse_primary stream =
  let stream = skip_trivia stream in
  let start_pos = Parse_stream.position stream in
  
  match Parse_stream.peek stream with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Literal _ | Token.Keyword (Token.True | Token.False) ->
          (match parse_literal stream with
           | Ok (lit, stream) ->
               Ok ({ 
                 Ast.expr_desc = Ast.ExprLiteral lit;
                 expr_span = span start_pos stream;
               }, stream)
           | Error e -> Error e)
      
      | Token.Ident name ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          Ok ({
            Ast.expr_desc = Ast.ExprIdent name;
            expr_span = span start_pos stream;
          }, stream)
      
      | Token.OpenDelim Token.Paren ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
          let stream = skip_trivia stream in
          
          (* Check for unit literal () *)
          (match Parse_stream.peek stream with
           | Some tok2 when (match tok2.Token.kind with Token.CloseDelim Token.Paren -> true | _ -> false) ->
               let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
               Ok ({
                 Ast.expr_desc = Ast.ExprLiteral Ast.Unit;
                 expr_span = span start_pos stream;
               }, stream)
           | _ ->
               (* Parse expression inside parens *)
               (match !parse_expr_ref stream with
                | Error e -> Error e
                | Ok (expr, stream) ->
                    let stream = skip_trivia stream in
                    match Parse_stream.parse_token stream (Token.CloseDelim Token.Paren) with
                    | Ok (_, stream) -> Ok (expr, stream)
                    | Error e -> Error e))
      
      | _ ->
          Error {
            Parse_stream.message = "Expected expression";
            position = tok.Token.span.start;
            expected = None;
          })
  | None ->
      Error {
        Parse_stream.message = "Expected expression";
        position = Parse_stream.position stream;
        expected = None;
      }

(* Parse binary operators *)
let parse_binop = function
  | Token.Plus -> Some Ast.Add
  | Token.Minus -> Some Ast.Sub
  | Token.Star -> Some Ast.Mul
  | Token.Slash -> Some Ast.Div
  | Token.Percent -> Some Ast.Mod
  | Token.Lt -> Some Ast.Lt
  | Token.LtEq -> Some Ast.Le
  | Token.Gt -> Some Ast.Gt
  | Token.GtEq -> Some Ast.Ge
  | Token.Eq -> Some Ast.Eq
  | Token.Ne -> Some Ast.Ne
  | Token.And -> Some Ast.And
  | Token.Or -> Some Ast.Or
  | Token.ColonColon -> Some Ast.Cons
  | Token.Caret -> Some Ast.Concat
  | _ -> None

(* Parse expressions with precedence climbing *)
let rec parse_expr stream =
  parse_expr_prec stream 0

and parse_expr_prec stream min_prec =
  let stream = skip_trivia stream in
  match parse_primary stream with
  | Error e -> Error e
  | Ok (left, stream) ->
      parse_expr_prec_loop stream left min_prec

and parse_expr_prec_loop stream left min_prec =
  let stream = skip_trivia stream in
  match Parse_stream.peek stream with
  | Some tok ->
      (match parse_binop tok.Token.kind with
       | None -> Ok (left, stream)
       | Some op ->
           let prec = precedence op in
           if prec < min_prec then
             Ok (left, stream)
           else
             let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
             let start_pos = Parse_stream.position stream in
             (match parse_expr_prec stream (prec + 1) with
              | Error e -> Error e
              | Ok (right, stream) ->
                  let expr = {
                    Ast.expr_desc = Ast.ExprBinaryOp (op, left, right);
                    expr_span = {
                      start_pos = left.Ast.expr_span.start_pos;
                      end_pos = right.Ast.expr_span.end_pos;
                    };
                  } in
                  parse_expr_prec_loop stream expr min_prec))
  | None -> Ok (left, stream)

and precedence = function
  | Ast.Mul | Ast.Div | Ast.Mod -> 10
  | Ast.Add | Ast.Sub -> 9
  | Ast.Concat -> 8
  | Ast.Cons -> 7
  | Ast.Lt | Ast.Le | Ast.Gt | Ast.Ge | Ast.Eq | Ast.Ne -> 6
  | Ast.And -> 5
  | Ast.Or -> 4

(* Set the forward reference *)
let () = parse_expr_ref := parse_expr

(* Parse a let binding *)
let parse_let_binding stream =
  let stream = skip_trivia stream in
  let start_pos = Parse_stream.position stream in
  
  match Parse_stream.parse_keyword stream Token.Let with
  | Error e -> Error e
  | Ok (_, stream) ->
      let stream = skip_trivia stream in
      
      (* Check for rec flag *)
  let rec_flag, stream =
    match Parse_stream.peek stream with
    | Some tok -> (
        match tok.Token.kind with
        | Token.Keyword Token.Rec ->
            let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
            (Ast.Recursive, stream)
        | _ -> (Ast.NonRecursive, stream))
    | None -> (Ast.NonRecursive, stream)
  in
      
      let stream = skip_trivia stream in
      
      (* Parse pattern (simplified to just identifiers for now) *)
       (match Parse_stream.parse_ident stream with
        | Error e -> Error e
        | Ok (name, stream) ->
            let pattern = {
              Ast.pat_desc = Ast.PatVar name;
              pat_span = span start_pos stream;
            } in
           
           let stream = skip_trivia stream in
           
           (* Parse = *)
           (match Parse_stream.parse_token stream Token.Eq with
            | Error e -> Error e
            | Ok (_, stream) ->
                (* Parse value expression *)
                (match parse_expr stream with
                 | Error e -> Error e
                 | Ok (value_expr, stream) ->
                     let stream = skip_trivia stream in
                     
                     (* Check for 'in' for let-in expression *)
                     (match Parse_stream.peek stream with
                      | Some tok when (match tok.Token.kind with Token.Keyword Token.In -> true | _ -> false) ->
                          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
                           (match parse_expr stream with
                            | Error e -> Error e
                            | Ok (body_expr, stream) ->
                                Ok ({
                                  Ast.expr_desc = Ast.ExprLet (rec_flag, [(pattern, value_expr)], body_expr);
                                  expr_span = span start_pos stream;
                                }, stream))
                       | _ ->
                           (* Top-level let - return as a let with unit body for now *)
                           Ok ({
                             Ast.expr_desc = Ast.ExprLet (rec_flag, [(pattern, value_expr)], {
                               Ast.expr_desc = Ast.ExprLiteral Ast.Unit;
                               expr_span = span start_pos stream;
                             });
                             expr_span = span start_pos stream;
                           }, stream)))))

(* Parse structure item *)
let parse_structure_item stream =
  let stream = skip_trivia stream in
  let start_pos = Parse_stream.position stream in
  
  match Parse_stream.peek stream with
  | Some tok -> (
      match tok.Token.kind with
      | Token.Keyword Token.Let ->
          (match parse_let_binding stream with
           | Error e -> Error e
           | Ok (expr, stream) ->
               (* Convert let expression to structure item *)
                (match expr.Ast.expr_desc with
                 | Ast.ExprLet (rec_flag, bindings, _) ->
                     Ok ({
                       Ast.str_desc = Ast.StrLet (rec_flag, bindings);
                       str_span = span start_pos stream;
                     }, stream)
                 | _ ->
                     Ok ({
                       Ast.str_desc = Ast.StrEval expr;
                       str_span = span start_pos stream;
                     }, stream)))
      
      | Token.Keyword Token.Open ->
          let _, stream = Option.expect ~msg:"" (Parse_stream.next stream) in
           (match Parse_stream.parse_ident stream with
            | Error e -> Error e
            | Ok (name, stream) ->
                Ok ({
                  Ast.str_desc = Ast.StrOpen name;
                  str_span = span start_pos stream;
                }, stream))
      
      | _ ->
           (* Try to parse as expression *)
           (match parse_expr stream with
            | Error e -> Error e
            | Ok (expr, stream) ->
                Ok ({
                  Ast.str_desc = Ast.StrEval expr;
                  str_span = span start_pos stream;
                }, stream)))
  | None ->
      Error {
        Parse_stream.message = "Expected structure item";
        position = start_pos;
        expected = None;
      }

(* Main parse functions *)
let parse_program tokens =
  let token_array = Array.of_list tokens in
  let stream = Parse_stream.create token_array in
  
  let rec parse_items stream acc =
    let stream = skip_trivia stream in
    if Parse_stream.is_empty stream then
      Ok (List.rev acc)
    else
      match parse_structure_item stream with
      | Error e -> 
          if Parse_stream.errors stream = [] then
            Error [e]
          else
            Error (Parse_stream.errors stream)
      | Ok (item, stream) ->
          parse_items stream (item :: acc)
  in
  
  parse_items stream []

let parse_expr tokens =
  let token_array = Array.of_list tokens in
  let stream = Parse_stream.create token_array in
  
  match parse_expr stream with
  | Error e -> Error [e]
  | Ok (expr, _) -> Ok expr

let parse_type _tokens =
  (* Type parsing not implemented yet *)
  Error [{
    Parse_stream.message = "Type parsing not implemented";
    position = 0;
    expected = None;
  }]