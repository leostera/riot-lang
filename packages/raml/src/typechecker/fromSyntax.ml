open Std

(** FromSyntax - Convert Syn Red Tree to UntypedTree AST

    This module converts from Syn's Red tree (positioned CST) to our clean
    untyped abstract syntax tree (AST) for type checking.

    Using Red tree gives us:
    - Proper parent pointers
    - Accurate source positions
    - Easy traversal with accessors *)

module Red = Syn.Ceibo.Red
module SyntaxNode = Red.SyntaxNode
module SyntaxToken = Red.SyntaxToken
module Span = Syn.Ceibo.Span
module Kind = Syn.SyntaxKind

type syntax_node = (Kind.t, string) Red.syntax_node
type syntax_token = (Kind.t, string) Red.syntax_token
type syntax_element = (Kind.t, string) Red.syntax_element

type error =
  | UnexpectedNode of { expected : string; got : Kind.t; span : Span.t }
  | MissingNode of { expected : string; span : Span.t }
  | UnsupportedFeature of { feature : string; span : Span.t }

let make_location span = UntypedTree.make_location ~source_id:None span

(** Convert constants *)
let convert_constant kind text span : (UntypedTree.constant, error) Result.t =
  match kind with
  | Kind.INT_LITERAL -> (
      match int_of_string_opt text with
      | Some i -> Ok (UntypedTree.ConstantInt i)
      | None ->
          Error
            (UnsupportedFeature
               { feature = format "int literal: %s" text; span }))
  | Kind.FLOAT_LITERAL -> (
      match float_of_string_opt text with
      | Some f -> Ok (UntypedTree.ConstantFloat f)
      | None ->
          Error
            (UnsupportedFeature
               { feature = format "float literal: %s" text; span }))
  | Kind.STRING_LITERAL ->
      let unquoted =
        if String.length text >= 2 then
          String.sub text 1 (String.length text - 2)
        else text
      in
      Ok (UntypedTree.ConstantString unquoted)
  | Kind.CHAR_LITERAL ->
      if String.length text >= 3 then
        let c = String.get text 1 in
        Ok (UntypedTree.ConstantChar c)
      else
        Error
          (UnsupportedFeature { feature = format "char literal: %s" text; span })
  | Kind.BOOL_LITERAL ->
      if text = "true" then Ok (UntypedTree.ConstantBool true)
      else if text = "false" then Ok (UntypedTree.ConstantBool false)
      else
        Error
          (UnsupportedFeature { feature = format "bool literal: %s" text; span })
  | Kind.UNIT_LITERAL -> Ok UntypedTree.ConstantUnit
  | _ ->
      Error (UnexpectedNode { expected = "constant literal"; got = kind; span })

(** Convert binary operators *)
let convert_binary_op text : UntypedTree.binary_op option =
  match text with
  | "+" -> Some Add
  | "-" -> Some Sub
  | "*" -> Some Mul
  | "/" -> Some Div
  | "mod" -> Some Mod
  | "=" -> Some Eq
  | "<>" -> Some Neq
  | "<" -> Some Lt
  | "<=" -> Some Le
  | ">" -> Some Gt
  | ">=" -> Some Ge
  | "&&" -> Some And
  | "||" -> Some Or
  | "::" -> Some Cons
  | "@" -> Some At
  | _ -> None

(** Find first child node of given kind *)
let find_child kind (node : syntax_node) : syntax_element option =
  let count = SyntaxNode.child_count node in
  let rec search i =
    if i >= count then None
    else
      match SyntaxNode.child node i with
      | None -> search (i + 1)
      | Some (Red.Node n as elem) ->
          if SyntaxNode.kind n = kind then Some elem else search (i + 1)
      | Some (Red.Token _) -> search (i + 1)
  in
  search 0

(** Get all child nodes of given kind *)
let get_children_by_kind kind (node : syntax_node) : syntax_node list =
  let count = SyntaxNode.child_count node in
  let rec collect i acc =
    if i >= count then List.rev acc
    else
      match SyntaxNode.child node i with
      | None -> collect (i + 1) acc
      | Some (Red.Node n) ->
          if SyntaxNode.kind n = kind then collect (i + 1) (n :: acc)
          else collect (i + 1) acc
      | Some (Red.Token _) -> collect (i + 1) acc
  in
  collect 0 []

(** Find first token matching predicate *)
let rec find_token pred (node : syntax_node) : string option =
  let count = SyntaxNode.child_count node in
  let rec search i =
    if i >= count then None
    else
      match SyntaxNode.child node i with
      | None -> search (i + 1)
      | Some (Red.Token t) ->
          if pred (SyntaxToken.kind t) then Some (SyntaxToken.text t)
          else search (i + 1)
      | Some (Red.Node n) -> (
          match find_token pred n with
          | Some text -> Some text
          | None -> search (i + 1))
  in
  search 0

(** Convert pattern *)
let rec convert_pattern (node : syntax_node) :
    (UntypedTree.pattern, error) Result.t =
  let span = SyntaxNode.span node in
  let loc = make_location span in

  match SyntaxNode.kind node with
  | Kind.WILDCARD_PATTERN -> Ok (UntypedTree.make_pattern_any ~loc)
  | Kind.IDENT_PATTERN -> (
      match find_token (fun _ -> true) node with
      | Some name -> Ok (UntypedTree.make_pattern_var ~name ~loc)
      | None -> Error (MissingNode { expected = "identifier"; span }))
  | Kind.LITERAL_PATTERN -> (
      (* Find literal token child *)
      let count = SyntaxNode.child_count node in
      let rec find_literal i =
        if i >= count then None
        else
          match SyntaxNode.child node i with
          | Some (Red.Token t) -> (
              let tok_kind = SyntaxToken.kind t in
              let tok_text = SyntaxToken.text t in
              let tok_span = SyntaxToken.span t in
              match convert_constant tok_kind tok_text tok_span with
              | Ok const -> Some const
              | Error _ -> find_literal (i + 1))
          | _ -> find_literal (i + 1)
      in
      match find_literal 0 with
      | Some const -> Ok (UntypedTree.make_pattern_constant ~const ~loc)
      | None -> Error (MissingNode { expected = "literal"; span }))
  | Kind.TUPLE_PATTERN -> (
      let pattern_children = get_children_by_kind Kind.IDENT_PATTERN node in
      let converted = List.map (fun p -> convert_pattern p) pattern_children in
      match Result.all converted with
      | Ok elements -> Ok (UntypedTree.make_pattern_tuple ~elements ~loc)
      | Error e -> Error e)
  | _ ->
      Error
        (UnsupportedFeature
           {
             feature =
               format "pattern kind: %s" (Kind.to_string (SyntaxNode.kind node));
             span;
           })

(** Convert expression *)
let rec convert_expression (node : syntax_node) :
    (UntypedTree.expression, error) Result.t =
  let span = SyntaxNode.span node in
  let loc = make_location span in

  match SyntaxNode.kind node with
  (* Literals *)
  | Kind.INT_LITERAL | Kind.FLOAT_LITERAL | Kind.STRING_LITERAL
  | Kind.CHAR_LITERAL | Kind.BOOL_LITERAL | Kind.UNIT_LITERAL -> (
      let text =
        find_token (fun _ -> true) node |> Option.unwrap_or ~default:""
      in
      match convert_constant (SyntaxNode.kind node) text span with
      | Ok const -> Ok (UntypedTree.make_constant ~const ~loc)
      | Error e -> Error e)
  (* Identifiers *)
  | Kind.IDENT_EXPR -> (
      match find_token (fun _ -> true) node with
      | Some name -> Ok (UntypedTree.make_ident ~name ~loc)
      | None -> Error (MissingNode { expected = "identifier"; span }))
  (* Let expressions *)
  | Kind.LET_EXPR -> (
      (* TODO: Check for 'rec' keyword *)
      let recursive = false in

      (* Find pattern and value (simplified for now) *)
      match find_child Kind.IDENT_PATTERN node with
      | Some (Red.Node pattern_node) -> (
          match convert_pattern pattern_node with
          | Ok pattern ->
              (* For now, create a stub value and body *)
              let value =
                UntypedTree.make_constant ~const:(ConstantInt 0) ~loc
              in
              let body = value in
              Ok (UntypedTree.make_let ~recursive ~pattern ~value ~body ~loc)
          | Error e -> Error e)
      | _ -> Error (MissingNode { expected = "let pattern"; span }))
  (* Function application *)
  | Kind.APPLY_EXPR -> (
      (* Find function and argument expressions *)
      let count = SyntaxNode.child_count node in
      let rec find_exprs acc i =
        if i >= count then List.rev acc
        else
          match SyntaxNode.child node i with
          | Some (Red.Node child) -> (
              let child_kind = SyntaxNode.kind child in
              match child_kind with
              | Kind.INT_LITERAL | Kind.STRING_LITERAL | Kind.IDENT_EXPR
              | Kind.LET_EXPR | Kind.TUPLE_EXPR | Kind.PAREN_EXPR
              | Kind.INFIX_EXPR | Kind.FUN_EXPR | Kind.APPLY_EXPR -> (
                  match convert_expression child with
                  | Ok expr -> find_exprs (expr :: acc) (i + 1)
                  | Error _ -> find_exprs acc (i + 1))
              | _ -> find_exprs acc (i + 1))
          | _ -> find_exprs acc (i + 1)
      in
      let exprs = find_exprs [] 0 in
      match exprs with
      | func :: args ->
          (* Build nested applications for multiple arguments *)
          let rec build_apply func = function
            | [] -> func
            | arg :: rest ->
                let app = UntypedTree.make_apply ~func ~arg ~loc in
                build_apply app rest
          in
          Ok (build_apply func args)
      | [] -> Error (MissingNode { expected = "function and arguments"; span }))
  (* Functions *)
  | Kind.FUN_EXPR -> (
      (* Find parameter pattern and body *)
      match find_child Kind.IDENT_PATTERN node with
      | Some (Red.Node param_node) -> (
          match convert_pattern param_node with
          | Ok param -> (
              (* Find body expression - look for expression after pattern *)
              let expr_result =
                let count = SyntaxNode.child_count node in
                let rec find_body i found_param =
                  if i >= count then
                    Error (MissingNode { expected = "function body"; span })
                  else
                    match SyntaxNode.child node i with
                    | Some (Red.Node child) ->
                        let child_kind = SyntaxNode.kind child in
                        if child_kind = Kind.IDENT_PATTERN then
                          find_body (i + 1) true
                        else if found_param then
                          match child_kind with
                          | Kind.INT_LITERAL | Kind.STRING_LITERAL
                          | Kind.IDENT_EXPR | Kind.LET_EXPR | Kind.TUPLE_EXPR
                          | Kind.PAREN_EXPR | Kind.INFIX_EXPR | Kind.FUN_EXPR ->
                              convert_expression child
                          | _ -> find_body (i + 1) found_param
                        else find_body (i + 1) found_param
                    | _ -> find_body (i + 1) found_param
                in
                find_body 0 false
              in
              match expr_result with
              | Ok body -> Ok (UntypedTree.make_function ~param ~body ~loc)
              | Error e -> Error e)
          | Error e -> Error e)
      | _ -> Error (MissingNode { expected = "function parameter"; span }))
  (* Binary operations *)
  | Kind.INFIX_EXPR ->
      (* TODO: Find operator and operands properly *)
      Error (UnsupportedFeature { feature = "infix operators (stub)"; span })
  (* Tuples *)
  | Kind.TUPLE_EXPR -> (
      let expr_children = get_children_by_kind Kind.IDENT_EXPR node in
      let converted = List.map convert_expression expr_children in
      match Result.all converted with
      | Ok elements -> Ok (UntypedTree.make_tuple ~elements ~loc)
      | Error e -> Error e)
  (* Parenthesized expressions - unwrap *)
  | Kind.PAREN_EXPR -> (
      match SyntaxNode.child node 0 with
      | Some (Red.Node inner) -> convert_expression inner
      | _ -> Error (MissingNode { expected = "inner expression"; span }))
  | _ ->
      Error
        (UnsupportedFeature
           {
             feature =
               format "expression kind: %s"
                 (Kind.to_string (SyntaxNode.kind node));
             span;
           })

(** Convert structure item (top-level) *)
let convert_structure_item (node : syntax_node) :
    (UntypedTree.structure_item, error) Result.t =
  let span = SyntaxNode.span node in
  let loc = make_location span in

  match SyntaxNode.kind node with
  | Kind.LET_BINDING -> (
      let recursive = false in
      match find_child Kind.IDENT_PATTERN node with
      | Some (Red.Node pattern_node) -> (
          match convert_pattern pattern_node with
          | Ok pattern -> (
              (* Find the expression child - look for any expression node *)
              let expr_result =
                let count = SyntaxNode.child_count node in
                let rec find_expr i =
                  if i >= count then
                    Error
                      (MissingNode
                         { expected = "expression in let binding"; span })
                  else
                    match SyntaxNode.child node i with
                    | Some (Red.Node child) -> (
                        let child_kind = SyntaxNode.kind child in
                        match child_kind with
                        | Kind.INT_LITERAL | Kind.STRING_LITERAL
                        | Kind.IDENT_EXPR | Kind.LET_EXPR | Kind.TUPLE_EXPR
                        | Kind.PAREN_EXPR | Kind.INFIX_EXPR | Kind.FUN_EXPR
                        | Kind.APPLY_EXPR ->
                            convert_expression child
                        | _ -> find_expr (i + 1))
                    | _ -> find_expr (i + 1)
                in
                find_expr 0
              in
              match expr_result with
              | Ok expr ->
                  Ok
                    (UntypedTree.make_structure_item_value ~recursive ~pattern
                       ~expr ~loc)
              | Error e -> Error e)
          | Error e -> Error e)
      | _ -> Error (MissingNode { expected = "let pattern"; span }))
  | _ ->
      Error
        (UnsupportedFeature
           {
             feature =
               format "structure item: %s"
                 (Kind.to_string (SyntaxNode.kind node));
             span;
           })

(** Convert from Red tree root *)
let from_red_tree (root : syntax_node) :
    (UntypedTree.structure, error list) Result.t =
  (* Get all top-level items *)
  let items = get_children_by_kind Kind.LET_BINDING root in

  (* Convert each item *)
  let results = List.map convert_structure_item items in

  let successes, errors =
    List.partition_map
      (fun r ->
        match r with Ok x -> Either.Left x | Error e -> Either.Right e)
      results
  in

  if List.is_empty errors then Ok successes else Error errors

(** Convert from Syn parse result *)
let from_parse_result (result : Syn.Parser.parse_result) :
    (UntypedTree.structure, error list) Result.t =
  let root = Red.new_root result.tree in
  from_red_tree root
