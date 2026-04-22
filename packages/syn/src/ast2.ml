open Std

type node = {
  tree: Syntax_tree.t;
  id: int;
}

type token = {
  tree: Syntax_tree.t;
  id: int;
}

type source_file = node

type implementation = node

type interface = node

type structure_item = node

type signature_item = node

type let_declaration = node

type let_binding = node

type type_declaration = node

type module_declaration = node

type module_type_declaration = node

type open_declaration = node

type include_declaration = node

type value_declaration = node

type external_declaration = node

type exception_declaration = node

type class_declaration = node

type extension_item = node

type attribute_item = node

type expr_item = node

type expr = node

type pattern = node

type parameter = node

type match_case = node

type type_expr = node

type path = node

let root = fun tree -> ({ tree; id = tree.Syntax_tree.root }: node)

let wrap_node = fun tree id -> ({ tree; id }: node)

let wrap_token = fun tree id -> ({ tree; id }: token)

let syntax_node = fun (node: node) ->
  Syntax_tree.node node.tree node.id

let syntax_token = fun (token: token) ->
  Syntax_tree.token token.tree token.id

let kind_is = Syntax_kind2.( = )

let node_kind_is = fun (node: node) kind -> kind_is (syntax_node node).Syntax_tree.kind kind

let token_kind_is = fun (token: token) kind -> kind_is (syntax_token token).Syntax_tree.kind kind

let is_expr_kind = function
  | Syntax_kind2.LET_EXPR
  | Syntax_kind2.LOCAL_OPEN_EXPR
  | Syntax_kind2.LET_MODULE_EXPR
  | Syntax_kind2.LET_EXCEPTION_EXPR
  | Syntax_kind2.BINDING_OPERATOR_EXPR
  | Syntax_kind2.FIRST_CLASS_MODULE_EXPR
  | Syntax_kind2.EXTENSION_EXPR
  | Syntax_kind2.UNREACHABLE_EXPR
  | Syntax_kind2.OBJECT_EXPR
  | Syntax_kind2.NEW_EXPR
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

let is_pattern_kind = function
  | Syntax_kind2.WILDCARD_PATTERN
  | Syntax_kind2.PATH_PATTERN
  | Syntax_kind2.APPLY_PATTERN
  | Syntax_kind2.LITERAL_PATTERN
  | Syntax_kind2.PAREN_PATTERN
  | Syntax_kind2.TUPLE_PATTERN
  | Syntax_kind2.LIST_PATTERN
  | Syntax_kind2.ARRAY_PATTERN
  | Syntax_kind2.RECORD_PATTERN
  | Syntax_kind2.POLY_VARIANT_PATTERN
  | Syntax_kind2.EXTENSION_PATTERN
  | Syntax_kind2.ATTRIBUTE_PATTERN
  | Syntax_kind2.LOCAL_OPEN_PATTERN
  | Syntax_kind2.LOCALLY_ABSTRACT_TYPE_PATTERN
  | Syntax_kind2.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind2.INTERVAL_PATTERN
  | Syntax_kind2.CONSTRAINT_PATTERN
  | Syntax_kind2.ALIAS_PATTERN
  | Syntax_kind2.OR_PATTERN
  | Syntax_kind2.CONS_PATTERN
  | Syntax_kind2.LAZY_PATTERN
  | Syntax_kind2.EXCEPTION_PATTERN
  | Syntax_kind2.LABELED_PARAM
  | Syntax_kind2.OPTIONAL_PARAM
  | Syntax_kind2.OPTIONAL_PARAM_DEFAULT -> true
  | _ -> false

let is_parameter_kind = function
  | Syntax_kind2.LABELED_PARAM
  | Syntax_kind2.OPTIONAL_PARAM
  | Syntax_kind2.OPTIONAL_PARAM_DEFAULT -> true
  | _ -> false

let is_path_kind = function
  | Syntax_kind2.PATH_EXPR
  | Syntax_kind2.PATH_PATTERN
  | Syntax_kind2.PATH_TYPE -> true
  | _ -> false

let is_type_expr_kind = function
  | Syntax_kind2.TYPE_EXPR
  | Syntax_kind2.PATH_TYPE
  | Syntax_kind2.VAR_TYPE
  | Syntax_kind2.WILDCARD_TYPE
  | Syntax_kind2.ARROW_TYPE
  | Syntax_kind2.TUPLE_TYPE
  | Syntax_kind2.APPLY_TYPE
  | Syntax_kind2.PAREN_TYPE
  | Syntax_kind2.OPAQUE_TYPE -> true
  | _ -> false

let is_match_case_kind = function
  | Syntax_kind2.MATCH_CASE -> true
  | _ -> false

let is_let_binding_kind = function
  | Syntax_kind2.LET_BINDING -> true
  | _ -> false

let node_matches = fun (node: node) matches -> matches (syntax_node node).Syntax_tree.kind

let token_matches = fun (token: token) matches -> matches (syntax_token token).Syntax_tree.kind

let first_child_node_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let child = wrap_node node.tree id in
              if node_matches child matches then
                found := Some child
        )
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let nth_child_node_matching = fun (node: node) target ~matches ->
  let found = ref None in
  let seen = ref 0 in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let child = wrap_node node.tree id in
              if node_matches child matches then
                if Int.equal !seen target then
                  found := Some child
                else
                  seen := !seen + 1
        )
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let for_each_child_node_matching = fun (node: node) ~matches ~fn ->
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Node id ->
          let child = wrap_node node.tree id in
          if node_matches child matches then
            fn child
      | Syntax_tree.Token _
      | Syntax_tree.Missing _ -> ()
    )

let first_child_token_matching = fun (node: node) ~matches ->
  let found = ref None in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let token = wrap_token node.tree id in
              if token_matches token matches then
                found := Some token
        )
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let has_child_token_kind = fun (node: node) expected_kind ->
  let found = ref false in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id ->
          let token = wrap_token node.tree id in
          if token_kind_is token expected_kind then
            found := true
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let nth_child_token_matching = fun (node: node) target ~matches ->
  let found = ref None in
  let seen = ref 0 in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id -> (
          match !found with
          | Some _ -> ()
          | None ->
              let token = wrap_token node.tree id in
              if token_matches token matches then
                if Int.equal !seen target then
                  found := Some token
                else
                  seen := !seen + 1
        )
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let first_ident_token = fun (node: node) ->
  first_child_token_matching node ~matches:(fun kind -> Syntax_kind2.(kind = IDENT))

let last_ident_token = fun (node: node) ->
  let found = ref None in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id ->
          let token = wrap_token node.tree id in
          if token_kind_is token Syntax_kind2.IDENT then
            found := Some token
      | Syntax_tree.Node _
      | Syntax_tree.Missing _ -> ()
    );
  !found

let first_expr_child = fun (node: node) -> first_child_node_matching node ~matches:is_expr_kind

let nth_expr_child = fun (node: node) target -> nth_child_node_matching node target ~matches:is_expr_kind

let first_pattern_child = fun (node: node) -> first_child_node_matching node ~matches:is_pattern_kind

let nth_pattern_child = fun (node: node) target -> nth_child_node_matching node target ~matches:is_pattern_kind

let first_type_expr_child = fun (node: node) -> first_child_node_matching node ~matches:is_type_expr_kind

let rec first_type_expr_descendant_of_pattern = fun (node: node) ->
  match first_type_expr_child node with
  | Some type_expr -> Some type_expr
  | None ->
      let found = ref None in
      Syntax_tree.for_each_child node.tree (syntax_node node)
        ~fn:(
          function
          | Syntax_tree.Node id -> (
              match !found with
              | Some _ -> ()
              | None ->
                  let child = wrap_node node.tree id in
                  if node_matches child is_pattern_kind then
                    found := first_type_expr_descendant_of_pattern child
            )
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> ()
        );
      !found

let first_match_case_child = fun (node: node) -> first_child_node_matching node ~matches:is_match_case_kind

let first_let_binding_child = fun (node: node) -> first_child_node_matching node ~matches:is_let_binding_kind

module Token = struct
  type t = token

  let kind = fun (token: token) -> (syntax_token token).Syntax_tree.kind

  let text = fun (token: token) ->
    Syntax_tree.token_text token.tree (syntax_token token)

  let raw_range = fun (token: token) ->
    let token = syntax_token token in
    (token.Syntax_tree.raw_lo, token.Syntax_tree.raw_hi)
end

module Node = struct
  type t = node

  let kind = fun (node: node) -> (syntax_node node).Syntax_tree.kind

  let text = fun (node: node) ->
    Syntax_tree.node_text node.tree (syntax_node node)

  let raw_range = fun (node: node) ->
    let node = syntax_node node in
    (node.Syntax_tree.raw_lo, node.Syntax_tree.raw_hi)

  let full_width = fun (node: node) -> (syntax_node node).Syntax_tree.full_width

  let child_count = fun (node: node) -> (syntax_node node).Syntax_tree.child_count

  let child_at = fun (node: node) index ->
    Syntax_tree.child_at node.tree (syntax_node node) index

  let for_each_child = fun (node: node) ~fn ->
    Syntax_tree.for_each_child node.tree (syntax_node node) ~fn

  let for_each_child_node = fun (node: node) ~fn ->
    for_each_child node
      ~fn:(
        function
        | Syntax_tree.Node id -> fn (wrap_node node.tree id)
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> ()
      )

  let for_each_child_token = fun (node: node) ~fn ->
    for_each_child node
      ~fn:(
        function
        | Syntax_tree.Token id -> fn (wrap_token node.tree id)
        | Syntax_tree.Node _
        | Syntax_tree.Missing _ -> ()
      )

  let first_child_node = fun (node: node) ~kind:expected_kind ->
    first_child_node_matching node ~matches:(fun kind -> Syntax_kind2.(kind = expected_kind))

  let first_child_token = fun (node: node) ~kind:expected_kind ->
    first_child_token_matching node ~matches:(fun kind -> Syntax_kind2.(kind = expected_kind))

  let first_token = fun (node: node) -> first_child_token_matching node ~matches:(fun _ -> true)
end

module TypeExpr = struct
  type t = type_expr

  type view =
    | Path of { path: path }
    | Var of { name: Token.t option }
    | Wildcard
    | Arrow of { left: t option; right: t option }
    | Tuple of { left: t option; right: t option }
    | Apply of { argument: t option; constructor: t option }
    | Parenthesized of { inner: t option }
    | Opaque of Node.t
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_matches node is_type_expr_kind then
      Some node
    else
      None

  let rec view = fun (type_expr: type_expr) ->
    match Node.kind type_expr with
    | Syntax_kind2.TYPE_EXPR -> (
        match first_child_node_matching type_expr ~matches:is_type_expr_kind with
        | Some child -> (
            match Node.kind child with
            | Syntax_kind2.TYPE_EXPR -> Opaque type_expr
            | _ -> view child
          )
        | None -> Opaque type_expr
      )
    | Syntax_kind2.PATH_TYPE ->
        Path { path = type_expr }
    | Syntax_kind2.VAR_TYPE ->
        Var { name = last_ident_token type_expr }
    | Syntax_kind2.WILDCARD_TYPE ->
        Wildcard
    | Syntax_kind2.ARROW_TYPE ->
        Arrow {
          left = nth_child_node_matching type_expr 0 ~matches:is_type_expr_kind;
          right = nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind
        }
    | Syntax_kind2.TUPLE_TYPE ->
        Tuple {
          left = nth_child_node_matching type_expr 0 ~matches:is_type_expr_kind;
          right = nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind
        }
    | Syntax_kind2.APPLY_TYPE ->
        Apply {
          argument = nth_child_node_matching type_expr 0 ~matches:is_type_expr_kind;
          constructor = nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind
        }
    | Syntax_kind2.PAREN_TYPE ->
        Parenthesized { inner = first_child_node_matching type_expr ~matches:is_type_expr_kind }
    | Syntax_kind2.OPAQUE_TYPE ->
        Opaque type_expr
    | Syntax_kind2.ERROR ->
        Error type_expr
    | _ ->
        Unknown type_expr

  let for_each_child_type = fun (type_expr: type_expr) ~fn ->
    for_each_child_node_matching type_expr ~matches:is_type_expr_kind ~fn
end

module Path = struct
  type t = path

  let cast = fun (node: node) ->
    if node_matches node is_path_kind then
      Some node
    else
      None

  let text = Node.text

  let first_ident = first_ident_token

  let last_ident = last_ident_token

  let for_each_ident = fun (path: path) ~fn ->
    Node.for_each_child_token path
      ~fn:(fun token ->
        if token_kind_is token Syntax_kind2.IDENT then
          fn token)
end

module Expr: sig
  type t = expr
  type view =
    | Let of { first_binding: let_binding option; body: t option }
    | LocalOpen of { body: t option }
    | LetModule of { body: t option }
    | LetException of { body: t option }
    | BindingOperator of { first_binding: let_binding option; body: t option }
    | FirstClassModule
    | Extension
    | Unreachable
    | Object
    | New
    | If of { condition: t option; then_branch: t option; else_branch: t option }
    | Match of { scrutinee: t option; first_case: match_case option }
    | Fun of { body: t option }
    | Function of { first_case: match_case option }
    | Try of { body: t option; first_case: match_case option }
    | While of { condition: t option; body: t option }
    | For of { pattern: pattern option; start_: t option; stop: t option; body: t option }
    | Assert of { argument: t option }
    | Lazy of { argument: t option }
    | Attribute of { inner: t option }
    | Sequence of { left: t option; right: t option }
    | Apply of { callee: t option; argument: t option }
    | Infix of { left: t option; operator: token option; right: t option }
    | Prefix of { operator: token option; operand: t option }
    | Assign of { target: t option; value: t option }
    | FieldAccess of { target: t option; field: token option }
    | MethodCall of { target: t option; method_: token option }
    | PolyVariant of { payload: t option }
    | Path of { path: path }
    | Literal of { token: token option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | RecordUpdate
    | ArrayIndex of { target: t option; index: t option }
    | StringIndex of { target: t option; index: t option }
    | Typed of { expr: t option; annotation: type_expr option }
    | LabeledArg of { label: token option; value: t option }
    | OptionalArg of { label: token option; value: t option }
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> token option

  val for_each_child_expr: t -> fn:(t -> unit) -> unit

  val for_each_match_case: t -> fn:(match_case -> unit) -> unit
end = struct
  type t = expr

  type view =
    | Let of { first_binding: let_binding option; body: t option }
    | LocalOpen of { body: t option }
    | LetModule of { body: t option }
    | LetException of { body: t option }
    | BindingOperator of { first_binding: let_binding option; body: t option }
    | FirstClassModule
    | Extension
    | Unreachable
    | Object
    | New
    | If of { condition: t option; then_branch: t option; else_branch: t option }
    | Match of { scrutinee: t option; first_case: match_case option }
    | Fun of { body: t option }
    | Function of { first_case: match_case option }
    | Try of { body: t option; first_case: match_case option }
    | While of { condition: t option; body: t option }
    | For of { pattern: pattern option; start_: t option; stop: t option; body: t option }
    | Assert of { argument: t option }
    | Lazy of { argument: t option }
    | Attribute of { inner: t option }
    | Sequence of { left: t option; right: t option }
    | Apply of { callee: t option; argument: t option }
    | Infix of { left: t option; operator: token option; right: t option }
    | Prefix of { operator: token option; operand: t option }
    | Assign of { target: t option; value: t option }
    | FieldAccess of { target: t option; field: token option }
    | MethodCall of { target: t option; method_: token option }
    | PolyVariant of { payload: t option }
    | Path of { path: path }
    | Literal of { token: token option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | RecordUpdate
    | ArrayIndex of { target: t option; index: t option }
    | StringIndex of { target: t option; index: t option }
    | Typed of { expr: t option; annotation: type_expr option }
    | LabeledArg of { label: token option; value: t option }
    | OptionalArg of { label: token option; value: t option }
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_matches node is_expr_kind then
      Some node
    else
      None

  let first_operator_token = fun (node: node) ->
    first_child_token_matching
      node
      ~matches:(fun kind ->
        not
          (Syntax_kind2.(kind = IDENT)
          || Syntax_kind2.(kind = INT)
          || Syntax_kind2.(kind = FLOAT)
          || Syntax_kind2.(kind = STRING)
          || Syntax_kind2.(kind = CHAR)
          || Syntax_kind2.(kind = TRUE_KW)
          || Syntax_kind2.(kind = FALSE_KW)))

  let literal_token = Node.first_token

  let view = fun (expr: expr) ->
    match Node.kind expr with
    | Syntax_kind2.LET_EXPR -> Let {
      first_binding = first_let_binding_child expr;
      body = nth_expr_child expr 0
    }
    | Syntax_kind2.LOCAL_OPEN_EXPR -> LocalOpen { body = nth_expr_child expr 1 }
    | Syntax_kind2.LET_MODULE_EXPR -> LetModule { body = first_expr_child expr }
    | Syntax_kind2.LET_EXCEPTION_EXPR -> LetException { body = first_expr_child expr }
    | Syntax_kind2.BINDING_OPERATOR_EXPR -> BindingOperator {
      first_binding = first_let_binding_child expr;
      body = nth_expr_child expr 0
    }
    | Syntax_kind2.FIRST_CLASS_MODULE_EXPR -> FirstClassModule
    | Syntax_kind2.EXTENSION_EXPR -> Extension
    | Syntax_kind2.UNREACHABLE_EXPR -> Unreachable
    | Syntax_kind2.OBJECT_EXPR -> Object
    | Syntax_kind2.NEW_EXPR -> New
    | Syntax_kind2.IF_EXPR -> If {
      condition = nth_expr_child expr 0;
      then_branch = nth_expr_child expr 1;
      else_branch = nth_expr_child expr 2
    }
    | Syntax_kind2.MATCH_EXPR -> Match {
      scrutinee = nth_expr_child expr 0;
      first_case = first_match_case_child expr
    }
    | Syntax_kind2.FUN_EXPR -> Fun { body = nth_expr_child expr 0 }
    | Syntax_kind2.FUNCTION_EXPR -> Function { first_case = first_match_case_child expr }
    | Syntax_kind2.TRY_EXPR -> Try {
      body = nth_expr_child expr 0;
      first_case = first_match_case_child expr
    }
    | Syntax_kind2.WHILE_EXPR -> While {
      condition = nth_expr_child expr 0;
      body = nth_expr_child expr 1
    }
    | Syntax_kind2.FOR_EXPR -> For {
      pattern = first_pattern_child expr;
      start_ = nth_expr_child expr 0;
      stop = nth_expr_child expr 1;
      body = nth_expr_child expr 2
    }
    | Syntax_kind2.ASSERT_EXPR -> Assert { argument = first_expr_child expr }
    | Syntax_kind2.LAZY_EXPR -> Lazy { argument = first_expr_child expr }
    | Syntax_kind2.ATTRIBUTE_EXPR -> Attribute { inner = first_expr_child expr }
    | Syntax_kind2.SEQUENCE_EXPR -> Sequence {
      left = nth_expr_child expr 0;
      right = nth_expr_child expr 1
    }
    | Syntax_kind2.APPLY_EXPR -> Apply {
      callee = nth_expr_child expr 0;
      argument = nth_expr_child expr 1
    }
    | Syntax_kind2.INFIX_EXPR -> Infix {
      left = nth_expr_child expr 0;
      operator = first_operator_token expr;
      right = nth_expr_child expr 1
    }
    | Syntax_kind2.PREFIX_EXPR -> Prefix {
      operator = first_operator_token expr;
      operand = first_expr_child expr
    }
    | Syntax_kind2.ASSIGN_EXPR -> Assign {
      target = nth_expr_child expr 0;
      value = nth_expr_child expr 1
    }
    | Syntax_kind2.FIELD_ACCESS_EXPR -> FieldAccess {
      target = nth_expr_child expr 0;
      field = last_ident_token expr
    }
    | Syntax_kind2.METHOD_CALL_EXPR -> MethodCall {
      target = nth_expr_child expr 0;
      method_ = last_ident_token expr
    }
    | Syntax_kind2.POLY_VARIANT_EXPR -> PolyVariant { payload = first_expr_child expr }
    | Syntax_kind2.PATH_EXPR -> Path { path = expr }
    | Syntax_kind2.LITERAL_EXPR -> Literal { token = literal_token expr }
    | Syntax_kind2.PAREN_EXPR -> Parenthesized { inner = first_expr_child expr }
    | Syntax_kind2.TUPLE_EXPR -> Tuple
    | Syntax_kind2.LIST_EXPR -> List
    | Syntax_kind2.ARRAY_EXPR -> Array
    | Syntax_kind2.RECORD_EXPR -> Record
    | Syntax_kind2.RECORD_UPDATE_EXPR -> RecordUpdate
    | Syntax_kind2.ARRAY_INDEX_EXPR -> ArrayIndex {
      target = nth_expr_child expr 0;
      index = nth_expr_child expr 1
    }
    | Syntax_kind2.STRING_INDEX_EXPR -> StringIndex {
      target = nth_expr_child expr 0;
      index = nth_expr_child expr 1
    }
    | Syntax_kind2.TYPED_EXPR -> Typed {
      expr = first_expr_child expr;
      annotation = first_type_expr_child expr
    }
    | Syntax_kind2.LABELED_ARG -> LabeledArg {
      label = first_ident_token expr;
      value = first_expr_child expr
    }
    | Syntax_kind2.OPTIONAL_ARG -> OptionalArg {
      label = first_ident_token expr;
      value = first_expr_child expr
    }
    | Syntax_kind2.ERROR -> Error expr
    | _ -> Unknown expr

  let for_each_child_expr = fun (expr: expr) ~fn ->
    for_each_child_node_matching expr ~matches:is_expr_kind ~fn

  let for_each_match_case = fun (expr: expr) ~fn ->
    for_each_child_node_matching expr ~matches:is_match_case_kind ~fn
end

module Pattern: sig
  type t = pattern
  type view =
    | Wildcard
    | Path of { path: path }
    | Apply of { callee: t option; argument: t option }
    | Literal of { token: token option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | PolyVariant
    | Extension
    | Attribute of { inner: t option }
    | LocalOpen
    | LocallyAbstractType
    | FirstClassModule
    | Interval of { left: t option; right: t option }
    | Constraint of { pattern: t option; annotation: type_expr option }
    | Alias of { pattern: t option; alias: t option }
    | Or of { left: t option; right: t option }
    | Cons of { head: t option; tail: t option }
    | Lazy of { pattern: t option }
    | Exception of { pattern: t option }
    | LabeledParam of parameter
    | OptionalParam of parameter
    | OptionalParamDefault of parameter
    | Error of Node.t
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view

  val literal_token: t -> token option

  val for_each_child_pattern: t -> fn:(t -> unit) -> unit
end = struct
  type t = pattern

  type view =
    | Wildcard
    | Path of { path: path }
    | Apply of { callee: t option; argument: t option }
    | Literal of { token: token option }
    | Parenthesized of { inner: t option }
    | Tuple
    | List
    | Array
    | Record
    | PolyVariant
    | Extension
    | Attribute of { inner: t option }
    | LocalOpen
    | LocallyAbstractType
    | FirstClassModule
    | Interval of { left: t option; right: t option }
    | Constraint of { pattern: t option; annotation: type_expr option }
    | Alias of { pattern: t option; alias: t option }
    | Or of { left: t option; right: t option }
    | Cons of { head: t option; tail: t option }
    | Lazy of { pattern: t option }
    | Exception of { pattern: t option }
    | LabeledParam of parameter
    | OptionalParam of parameter
    | OptionalParamDefault of parameter
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_matches node is_pattern_kind then
      Some node
    else
      None

  let literal_token = Node.first_token

  let view = fun (pattern: pattern) ->
    match Node.kind pattern with
    | Syntax_kind2.WILDCARD_PATTERN -> Wildcard
    | Syntax_kind2.PATH_PATTERN -> Path { path = pattern }
    | Syntax_kind2.APPLY_PATTERN -> Apply {
      callee = nth_pattern_child pattern 0;
      argument = nth_pattern_child pattern 1
    }
    | Syntax_kind2.LITERAL_PATTERN -> Literal { token = literal_token pattern }
    | Syntax_kind2.PAREN_PATTERN -> Parenthesized { inner = first_pattern_child pattern }
    | Syntax_kind2.TUPLE_PATTERN -> Tuple
    | Syntax_kind2.LIST_PATTERN -> List
    | Syntax_kind2.ARRAY_PATTERN -> Array
    | Syntax_kind2.RECORD_PATTERN -> Record
    | Syntax_kind2.POLY_VARIANT_PATTERN -> PolyVariant
    | Syntax_kind2.EXTENSION_PATTERN -> Extension
    | Syntax_kind2.ATTRIBUTE_PATTERN -> Attribute { inner = first_pattern_child pattern }
    | Syntax_kind2.LOCAL_OPEN_PATTERN -> LocalOpen
    | Syntax_kind2.LOCALLY_ABSTRACT_TYPE_PATTERN -> LocallyAbstractType
    | Syntax_kind2.FIRST_CLASS_MODULE_PATTERN -> FirstClassModule
    | Syntax_kind2.INTERVAL_PATTERN -> Interval {
      left = nth_pattern_child pattern 0;
      right = nth_pattern_child pattern 1
    }
    | Syntax_kind2.CONSTRAINT_PATTERN -> Constraint {
      pattern = first_pattern_child pattern;
      annotation = first_type_expr_child pattern
    }
    | Syntax_kind2.ALIAS_PATTERN -> Alias {
      pattern = nth_pattern_child pattern 0;
      alias = nth_pattern_child pattern 1
    }
    | Syntax_kind2.OR_PATTERN -> Or {
      left = nth_pattern_child pattern 0;
      right = nth_pattern_child pattern 1
    }
    | Syntax_kind2.CONS_PATTERN -> Cons {
      head = nth_pattern_child pattern 0;
      tail = nth_pattern_child pattern 1
    }
    | Syntax_kind2.LAZY_PATTERN -> Lazy { pattern = first_pattern_child pattern }
    | Syntax_kind2.EXCEPTION_PATTERN -> Exception { pattern = first_pattern_child pattern }
    | Syntax_kind2.LABELED_PARAM -> LabeledParam pattern
    | Syntax_kind2.OPTIONAL_PARAM -> OptionalParam pattern
    | Syntax_kind2.OPTIONAL_PARAM_DEFAULT -> OptionalParamDefault pattern
    | Syntax_kind2.ERROR -> Error pattern
    | _ -> Unknown pattern

  let for_each_child_pattern = fun (pattern: pattern) ~fn ->
    for_each_child_node_matching pattern ~matches:is_pattern_kind ~fn
end

module Parameter: sig
  type t = parameter
  type view =
    | Labeled of { label: token option; pattern: pattern option }
    | Optional of { label: token option; pattern: pattern option }
    | OptionalDefault of { label: token option; pattern: pattern option; default: expr option }
    | Unknown of Node.t
  val cast: Node.t -> t option

  val view: t -> view
end = struct
  type t = parameter

  type view =
    | Labeled of { label: token option; pattern: pattern option }
    | Optional of { label: token option; pattern: pattern option }
    | OptionalDefault of { label: token option; pattern: pattern option; default: expr option }
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_matches node is_parameter_kind then
      Some node
    else
      None

  let view = fun (parameter: parameter) ->
    match Node.kind parameter with
    | Syntax_kind2.LABELED_PARAM -> Labeled {
      label = first_ident_token parameter;
      pattern = first_pattern_child parameter
    }
    | Syntax_kind2.OPTIONAL_PARAM -> Optional {
      label = first_ident_token parameter;
      pattern = first_pattern_child parameter
    }
    | Syntax_kind2.OPTIONAL_PARAM_DEFAULT -> OptionalDefault {
      label = first_ident_token parameter;
      pattern = first_pattern_child parameter;
      default = first_expr_child parameter
    }
    | _ -> Unknown parameter
end

module MatchCase: sig
  type t = match_case
  type view = {
    pattern: pattern option;
    guard: expr option;
    body: expr option;
  }
  val cast: Node.t -> t option

  val view: t -> view
end = struct
  type t = match_case

  type view = {
    pattern: pattern option;
    guard: expr option;
    body: expr option;
  }

  let cast = fun (node: node) ->
    if node_matches node is_match_case_kind then
      Some node
    else
      None

  let view = fun (match_case: match_case) ->
    let guard, body =
      if has_child_token_kind match_case Syntax_kind2.WHEN_KW then
        (nth_expr_child match_case 0, nth_expr_child match_case 1)
      else
        (None, nth_expr_child match_case 0)
    in
    { pattern = first_pattern_child match_case; guard; body }
end

module LetBinding: sig
  type t = let_binding
  type view = {
    pattern: pattern option;
    body: expr option;
  }
  val cast: Node.t -> t option

  val view: t -> view

  val pattern: t -> pattern option

  val body: t -> expr option

  val for_each_parameter: t -> fn:(pattern -> unit) -> unit

  val type_annotation: t -> type_expr option
end = struct
  type t = let_binding

  type view = {
    pattern: pattern option;
    body: expr option;
  }

  let cast = fun (node: node) ->
    if node_matches node is_let_binding_kind then
      Some node
    else
      None

  let pattern = first_pattern_child

  let body = first_expr_child

  let view = fun (binding: let_binding) -> { pattern = pattern binding; body = body binding }

  let for_each_parameter = fun (binding: let_binding) ~fn ->
    let seen_first = ref false in
    for_each_child_node_matching binding ~matches:is_pattern_kind
      ~fn:(fun pattern ->
        if !seen_first then
          fn pattern
        else
          seen_first := true)

  let type_annotation = fun (binding: let_binding) ->
    match first_type_expr_child binding with
    | Some type_expr -> Some type_expr
    | None -> (
        match pattern binding with
        | Some pattern -> first_type_expr_descendant_of_pattern pattern
        | None -> None
      )
end

module LetDeclaration = struct
  type t = let_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.LET_DECL then
      Some node
    else
      None

  let rec_token = fun (decl: let_declaration) -> Node.first_child_token decl ~kind:Syntax_kind2.REC_KW

  let first_binding = first_let_binding_child

  let for_each_binding = fun (decl: let_declaration) ~fn ->
    for_each_child_node_matching decl ~matches:is_let_binding_kind ~fn
end

module TypeDeclaration = struct
  type t = type_declaration

  type parameter =
    | Named of {
      name: Token.t;
      quote: Token.t option;
      variance: Token.t option;
      injective: Token.t option;
    }
    | Wildcard of {
      wildcard: Token.t;
      variance: Token.t option;
      injective: Token.t option;
    }

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.TYPE_DECL then
      Some node
    else
      None

  let child_token_at = fun (decl: type_declaration) index ->
    match Node.child_at decl index with
    | Some (Syntax_tree.Token id) -> Some (wrap_token decl.tree id)
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_kind_at = fun decl index ->
    match child_token_at decl index with
    | Some token -> Some (Token.kind token)
    | None -> None

  let rec collect_type_parameter_modifiers = fun decl index variance injective ->
    match child_token_at decl index with
    | Some token when token_kind_is token Syntax_kind2.PLUS || token_kind_is token Syntax_kind2.MINUS ->
        collect_type_parameter_modifiers decl (index + 1) (Some token) injective
    | Some token when token_kind_is token Syntax_kind2.BANG ->
        collect_type_parameter_modifiers decl (index + 1) variance (Some token)
    | _ ->
        (index, variance, injective)

  let skip_type_parameter = fun decl index ->
    let index, _, _ = collect_type_parameter_modifiers decl index None None in
    match child_token_kind_at decl index with
    | Some Syntax_kind2.QUOTE -> (
        match child_token_kind_at decl (index + 1) with
        | Some Syntax_kind2.IDENT -> index + 2
        | _ -> index + 1
      )
    | Some Syntax_kind2.UNDERSCORE ->
        index + 1
    | _ ->
        index

  let emit_type_parameter = fun decl index ~fn ->
    let index, variance, injective = collect_type_parameter_modifiers decl index None None in
    match child_token_at decl index with
    | Some quote when token_kind_is quote Syntax_kind2.QUOTE -> (
        match child_token_at decl (index + 1) with
        | Some name when token_kind_is name Syntax_kind2.IDENT ->
            fn (Named { name; quote = Some quote; variance; injective });
            index + 2
        | _ ->
            index + 1
      )
    | Some wildcard when token_kind_is wildcard Syntax_kind2.UNDERSCORE ->
        fn (Wildcard { wildcard; variance; injective });
        index + 1
    | _ ->
        index

  let rec skip_parenthesized_type_parameters = fun decl index ->
    match child_token_kind_at decl index with
    | Some Syntax_kind2.RPAREN ->
        index + 1
    | Some Syntax_kind2.EOF
    | None ->
        index
    | _ ->
        skip_parenthesized_type_parameters decl (index + 1)

  let name = fun decl ->
    let rec loop index =
      match child_token_at decl index with
      | Some token when token_kind_is token Syntax_kind2.TYPE_KW || token_kind_is token Syntax_kind2.NONREC_KW ->
          loop (index + 1)
      | Some token when token_kind_is token Syntax_kind2.LPAREN ->
          loop (skip_parenthesized_type_parameters decl (index + 1))
      | Some token when
        token_kind_is token Syntax_kind2.PLUS
        || token_kind_is token Syntax_kind2.MINUS
        || token_kind_is token Syntax_kind2.BANG
        || token_kind_is token Syntax_kind2.QUOTE
        || token_kind_is token Syntax_kind2.UNDERSCORE ->
          let next = skip_type_parameter decl index in
          if next > index then
            loop next
          else
            None
      | Some token when token_kind_is token Syntax_kind2.IDENT ->
          Some token
      | _ ->
          None
    in
    loop 0

  let for_each_parameter = fun decl ~fn ->
    let rec parse_parenthesized index =
      match child_token_kind_at decl index with
      | Some Syntax_kind2.RPAREN ->
          index + 1
      | Some Syntax_kind2.COMMA ->
          parse_parenthesized (index + 1)
      | Some Syntax_kind2.EOF
      | None ->
          index
      | _ ->
          let next = emit_type_parameter decl index ~fn in
          parse_parenthesized
            (
              if next > index then
                next
              else
                index + 1
            )
    in
    let rec parse_head index =
      match child_token_at decl index with
      | Some token when token_kind_is token Syntax_kind2.TYPE_KW || token_kind_is token Syntax_kind2.NONREC_KW ->
          parse_head (index + 1)
      | Some token when token_kind_is token Syntax_kind2.LPAREN ->
          parse_head (parse_parenthesized (index + 1))
      | Some token when
        token_kind_is token Syntax_kind2.PLUS
        || token_kind_is token Syntax_kind2.MINUS
        || token_kind_is token Syntax_kind2.BANG
        || token_kind_is token Syntax_kind2.QUOTE
        || token_kind_is token Syntax_kind2.UNDERSCORE ->
          let next = emit_type_parameter decl index ~fn in
          if next > index then
            parse_head next
      | _ ->
          ()
    in
    parse_head 0

  let has_direct_pipe = fun decl ->
    let found = ref false in
    Node.for_each_child_token decl
      ~fn:(fun token ->
        if token_kind_is token Syntax_kind2.PIPE then
          found := true);
    !found

  let manifest = fun decl ->
    if has_direct_pipe decl then
      None
    else
      first_type_expr_child decl
end

module ModuleDeclaration = struct
  type t = module_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.MODULE_DECL then
      Some node
    else
      None

  let name = first_ident_token
end

module ModuleTypeDeclaration = struct
  type t = module_type_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.MODULE_TYPE_DECL then
      Some node
    else
      None

  let name = first_ident_token
end

module OpenDeclaration = struct
  type t = open_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.OPEN_DECL then
      Some node
    else
      None

  let path_text = Node.text
end

module IncludeDeclaration = struct
  type t = include_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.INCLUDE_DECL then
      Some node
    else
      None
end

module ValueDeclaration = struct
  type t = value_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.VAL_DECL then
      Some node
    else
      None

  let name = first_ident_token

  let type_annotation = first_type_expr_child
end

module ExternalDeclaration = struct
  type t = external_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXTERNAL_DECL then
      Some node
    else
      None

  let name = first_ident_token

  let type_annotation = first_type_expr_child
end

module ExceptionDeclaration = struct
  type t = exception_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXCEPTION_DECL then
      Some node
    else
      None

  let name = first_ident_token
end

module ClassDeclaration = struct
  type t = class_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.CLASS_DECL then
      Some node
    else
      None

  let name = first_ident_token
end

module ExtensionItem = struct
  type t = extension_item

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXTENSION_ITEM then
      Some node
    else
      None
end

module AttributeItem = struct
  type t = attribute_item

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.ATTRIBUTE_ITEM then
      Some node
    else
      None
end

module ExprItem = struct
  type t = expr_item

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXPR_ITEM then
      Some node
    else
      None

  let expr = first_expr_child
end

module StructureItem = struct
  type t = structure_item

  type view =
    | Let of let_declaration
    | Type of type_declaration
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Class of class_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Expr of expr_item
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.STRUCTURE_ITEM then
      Some node
    else
      None

  let declaration = fun (item: structure_item) ->
    first_child_node_matching item ~matches:(fun kind -> not (Syntax_kind2.(kind = ERROR)))

  let view = fun (item: structure_item) ->
    match declaration item with
    | Some node -> (
        match Node.kind node with
        | Syntax_kind2.LET_DECL -> Let node
        | Syntax_kind2.TYPE_DECL -> Type node
        | Syntax_kind2.MODULE_DECL -> Module node
        | Syntax_kind2.MODULE_TYPE_DECL -> ModuleType node
        | Syntax_kind2.OPEN_DECL -> Open node
        | Syntax_kind2.INCLUDE_DECL -> Include node
        | Syntax_kind2.EXTERNAL_DECL -> External node
        | Syntax_kind2.EXCEPTION_DECL -> Exception node
        | Syntax_kind2.CLASS_DECL -> Class node
        | Syntax_kind2.EXTENSION_ITEM -> Extension node
        | Syntax_kind2.ATTRIBUTE_ITEM -> Attribute node
        | Syntax_kind2.EXPR_ITEM -> Expr node
        | Syntax_kind2.ERROR -> Error node
        | kind when is_expr_kind kind -> Expr node
        | _ -> Unknown node
      )
    | None -> Unknown item
end

module SignatureItem = struct
  type t = signature_item

  type view =
    | Value of value_declaration
    | Type of type_declaration
    | Module of module_declaration
    | ModuleType of module_type_declaration
    | Open of open_declaration
    | Include of include_declaration
    | External of external_declaration
    | Exception of exception_declaration
    | Class of class_declaration
    | Extension of extension_item
    | Attribute of attribute_item
    | Error of Node.t
    | Unknown of Node.t

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.SIGNATURE_ITEM then
      Some node
    else
      None

  let declaration = fun (item: signature_item) ->
    first_child_node_matching item ~matches:(fun _ -> true)

  let view = fun (item: signature_item) ->
    match declaration item with
    | Some node -> (
        match Node.kind node with
        | Syntax_kind2.VAL_DECL -> Value node
        | Syntax_kind2.TYPE_DECL -> Type node
        | Syntax_kind2.MODULE_DECL -> Module node
        | Syntax_kind2.MODULE_TYPE_DECL -> ModuleType node
        | Syntax_kind2.OPEN_DECL -> Open node
        | Syntax_kind2.INCLUDE_DECL -> Include node
        | Syntax_kind2.EXTERNAL_DECL -> External node
        | Syntax_kind2.EXCEPTION_DECL -> Exception node
        | Syntax_kind2.CLASS_DECL -> Class node
        | Syntax_kind2.EXTENSION_ITEM -> Extension node
        | Syntax_kind2.ATTRIBUTE_ITEM -> Attribute node
        | Syntax_kind2.ERROR -> Error node
        | _ -> Unknown node
      )
    | None -> Unknown item
end

module Implementation = struct
  type t = implementation

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.IMPLEMENTATION then
      Some node
    else
      None

  let for_each_item = fun (impl: implementation) ~fn ->
    for_each_child_node_matching impl ~matches:(fun kind -> Syntax_kind2.(kind = STRUCTURE_ITEM)) ~fn
end

module Interface = struct
  type t = interface

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.INTERFACE then
      Some node
    else
      None

  let for_each_item = fun (interface: interface) ~fn ->
    for_each_child_node_matching
      interface
      ~matches:(fun kind -> Syntax_kind2.(kind = SIGNATURE_ITEM))
      ~fn
end

module SourceFile = struct
  type t = source_file

  type view =
    | Implementation of implementation
    | Interface of interface
    | Empty

  let make = root

  let implementation = fun (source_file: source_file) ->
    Node.first_child_node source_file ~kind:Syntax_kind2.IMPLEMENTATION

  let interface = fun (source_file: source_file) ->
    Node.first_child_node source_file ~kind:Syntax_kind2.INTERFACE

  let view = fun (source_file: source_file) ->
    match implementation source_file with
    | Some impl -> Implementation impl
    | None -> (
        match interface source_file with
        | Some interface -> Interface interface
        | None -> Empty
      )

  let for_each_item = fun (source_file: source_file) ~fn ->
    (
      match implementation source_file with
      | Some impl -> Implementation.for_each_item impl ~fn:(fun item -> fn item)
      | None -> ()
    );
    match interface source_file with
    | Some interface -> Interface.for_each_item interface ~fn:(fun item -> fn item)
    | None -> ()

  let for_each_structure_item = fun (source_file: source_file) ~fn ->
    match implementation source_file with
    | Some impl -> Implementation.for_each_item impl ~fn
    | None -> ()

  let for_each_signature_item = fun (source_file: source_file) ~fn ->
    match interface source_file with
    | Some interface -> Interface.for_each_item interface ~fn
    | None -> ()
end
