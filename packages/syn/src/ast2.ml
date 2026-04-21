open Std

type node = {
  tree: Syntax_tree.t;
  id: int;
}

type token = {
  tree: Syntax_tree.t;
  id: int;
}

let root = fun tree -> ({ tree; id = tree.Syntax_tree.root }: node)

let wrap_node = fun tree id -> ({ tree; id }: node)

let wrap_token = fun tree id -> ({ tree; id }: token)

let syntax_node = fun (node: node) ->
  Syntax_tree.node node.tree node.id

module Node = struct
  type t = node

  let kind = fun (node: t) -> (syntax_node node).Syntax_tree.kind

  let text = fun (node: t) ->
    Syntax_tree.node_text node.tree (syntax_node node)

  let for_each_child = fun (node: t) ~fn ->
    Syntax_tree.for_each_child node.tree (syntax_node node) ~fn

  let for_each_child_node = fun (node: t) ~fn ->
    for_each_child node
      ~fn:(
        function
        | Syntax_tree.Node id -> fn (wrap_node node.tree id)
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> ()
      )

  let first_child_node = fun (node: t) ~kind:expected_kind ->
    let found = ref None in
    for_each_child_node node
      ~fn:(fun child ->
        match !found with
        | Some _ -> ()
        | None ->
            if kind child = expected_kind then
              found := Some child);
    !found
end

module Token = struct
  type t = token

  let leaf = fun (token: t) ->
    Syntax_tree.token token.tree token.id

  let kind = fun (token: t) -> (leaf token).Syntax_tree.kind

  let text = fun (token: t) ->
    Syntax_tree.token_text token.tree (leaf token)
end

module Expr = struct
  type t = node

  type view =
    | Let of { binding: Node.t option; body: t option }
    | If of { condition: t option; then_branch: t option; else_branch: t option }
    | Match of { scrutinee: t option }
    | Fun of { body: t option }
    | Apply of { callee: t option; argument: t option }
    | Infix of { left: t option; operator: Token.t option; right: t option }
    | Prefix of { operator: Token.t option; operand: t option }
    | Path
    | Literal
    | Tuple
    | List
    | Array
    | Record
    | Parenthesized of { inner: t option }
    | Unknown of Node.t

  let is_expr_kind = function
    | Syntax_kind2.LET_EXPR
    | Syntax_kind2.LOCAL_OPEN_EXPR
    | Syntax_kind2.LET_MODULE_EXPR
    | Syntax_kind2.FIRST_CLASS_MODULE_EXPR
    | Syntax_kind2.IF_EXPR
    | Syntax_kind2.MATCH_EXPR
    | Syntax_kind2.FUN_EXPR
    | Syntax_kind2.FUNCTION_EXPR
    | Syntax_kind2.TRY_EXPR
    | Syntax_kind2.WHILE_EXPR
    | Syntax_kind2.FOR_EXPR
    | Syntax_kind2.ASSERT_EXPR
    | Syntax_kind2.LAZY_EXPR
    | Syntax_kind2.ATTRIBUTE_EXPR
    | Syntax_kind2.SEQUENCE_EXPR
    | Syntax_kind2.APPLY_EXPR
    | Syntax_kind2.INFIX_EXPR
    | Syntax_kind2.PREFIX_EXPR
    | Syntax_kind2.ASSIGN_EXPR
    | Syntax_kind2.FIELD_ACCESS_EXPR
    | Syntax_kind2.METHOD_CALL_EXPR
    | Syntax_kind2.POLY_VARIANT_EXPR
    | Syntax_kind2.LABELED_ARG
    | Syntax_kind2.OPTIONAL_ARG
    | Syntax_kind2.ARRAY_INDEX_EXPR
    | Syntax_kind2.STRING_INDEX_EXPR
    | Syntax_kind2.TYPED_EXPR
    | Syntax_kind2.PATH_EXPR
    | Syntax_kind2.LITERAL_EXPR
    | Syntax_kind2.PAREN_EXPR
    | Syntax_kind2.TUPLE_EXPR
    | Syntax_kind2.LIST_EXPR
    | Syntax_kind2.ARRAY_EXPR
    | Syntax_kind2.RECORD_EXPR
    | Syntax_kind2.RECORD_UPDATE_EXPR -> true
    | _ -> false

  let cast = fun node ->
    if is_expr_kind (Node.kind node) then
      Some node
    else
      None

  let first_expr_child = fun node ->
    let found = ref None in
    Node.for_each_child_node node
      ~fn:(fun child ->
        match !found, cast child with
        | None, Some expr -> found := Some expr
        | _ -> ());
    !found

  let nth_expr_child = fun node target ->
    let found = ref None in
    let seen = ref 0 in
    Node.for_each_child_node node
      ~fn:(fun child ->
        match !found, cast child with
        | None, Some expr ->
            if !seen = target then
              found := Some expr
            else
              seen := !seen + 1
        | _ -> ());
    !found

  let first_token_child = fun node ->
    let found = ref None in
    Node.for_each_child node
      ~fn:(fun child ->
        match !found, child with
        | None, Syntax_tree.Token id -> found := Some (wrap_token node.tree id)
        | _ -> ());
    !found

  let view = fun expr ->
    match Node.kind expr with
    | Syntax_kind2.LET_EXPR -> Let {
      binding = Node.first_child_node expr ~kind:Syntax_kind2.LET_BINDING;
      body = nth_expr_child expr 0
    }
    | Syntax_kind2.IF_EXPR -> If {
      condition = nth_expr_child expr 0;
      then_branch = nth_expr_child expr 1;
      else_branch = nth_expr_child expr 2
    }
    | Syntax_kind2.MATCH_EXPR -> Match { scrutinee = first_expr_child expr }
    | Syntax_kind2.FUN_EXPR
    | Syntax_kind2.FUNCTION_EXPR -> Fun { body = first_expr_child expr }
    | Syntax_kind2.APPLY_EXPR -> Apply {
      callee = nth_expr_child expr 0;
      argument = nth_expr_child expr 1
    }
    | Syntax_kind2.INFIX_EXPR -> Infix {
      left = nth_expr_child expr 0;
      operator = first_token_child expr;
      right = nth_expr_child expr 1
    }
    | Syntax_kind2.PREFIX_EXPR -> Prefix {
      operator = first_token_child expr;
      operand = first_expr_child expr
    }
    | Syntax_kind2.PATH_EXPR -> Path
    | Syntax_kind2.LITERAL_EXPR -> Literal
    | Syntax_kind2.TUPLE_EXPR -> Tuple
    | Syntax_kind2.LIST_EXPR -> List
    | Syntax_kind2.ARRAY_EXPR -> Array
    | Syntax_kind2.RECORD_EXPR -> Record
    | Syntax_kind2.PAREN_EXPR -> Parenthesized { inner = first_expr_child expr }
    | _ -> Unknown expr
end

module SourceFile = struct
  type t = node

  let make = root

  let for_each_item = fun source_file ~fn ->
    Node.for_each_child_node source_file
      ~fn:(fun child ->
        match Node.kind child with
        | Syntax_kind2.IMPLEMENTATION
        | Syntax_kind2.INTERFACE ->
            Node.for_each_child_node child
              ~fn:(fun item ->
                match Node.kind item with
                | Syntax_kind2.STRUCTURE_ITEM
                | Syntax_kind2.SIGNATURE_ITEM -> fn item
                | _ -> ())
        | _ -> ())
end
