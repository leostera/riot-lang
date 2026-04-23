open Std
open Std.Collections

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

type type_extension_declaration = node

type module_declaration = node

type module_type_declaration = node

type module_type_constraint = node

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

type record_type = node

type record_field = node

type record_expr_field = node

type variant_type = node

type variant_constructor = node

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
  | Syntax_kind2.PATH_TYPE
  | Syntax_kind2.PATH_MODULE_EXPR
  | Syntax_kind2.PATH_MODULE_TYPE -> true
  | _ -> false

let is_type_expr_kind = function
  | Syntax_kind2.TYPE_EXPR
  | Syntax_kind2.PATH_TYPE
  | Syntax_kind2.VAR_TYPE
  | Syntax_kind2.WILDCARD_TYPE
  | Syntax_kind2.ARROW_TYPE
  | Syntax_kind2.POLY_TYPE
  | Syntax_kind2.LABELED_TYPE
  | Syntax_kind2.TUPLE_TYPE
  | Syntax_kind2.APPLY_TYPE
  | Syntax_kind2.PAREN_TYPE
  | Syntax_kind2.OPAQUE_TYPE -> true
  | _ -> false

let is_record_type_kind = function
  | Syntax_kind2.RECORD_TYPE -> true
  | _ -> false

let is_record_field_kind = function
  | Syntax_kind2.RECORD_FIELD -> true
  | _ -> false

let is_record_expr_field_kind = function
  | Syntax_kind2.RECORD_EXPR_FIELD -> true
  | _ -> false

let is_variant_type_kind = function
  | Syntax_kind2.VARIANT_TYPE -> true
  | _ -> false

let is_variant_constructor_kind = function
  | Syntax_kind2.VARIANT_CONSTRUCTOR -> true
  | _ -> false

let is_module_expr_kind = function
  | Syntax_kind2.MODULE_EXPR
  | Syntax_kind2.PATH_MODULE_EXPR
  | Syntax_kind2.STRUCT_MODULE_EXPR
  | Syntax_kind2.FUNCTOR_MODULE_EXPR
  | Syntax_kind2.APPLY_MODULE_EXPR
  | Syntax_kind2.CONSTRAINT_MODULE_EXPR
  | Syntax_kind2.PAREN_MODULE_EXPR
  | Syntax_kind2.OPAQUE_MODULE_EXPR -> true
  | _ -> false

let is_module_type_kind = function
  | Syntax_kind2.MODULE_TYPE_EXPR
  | Syntax_kind2.PATH_MODULE_TYPE
  | Syntax_kind2.SIGNATURE_MODULE_TYPE
  | Syntax_kind2.TYPEOF_MODULE_TYPE
  | Syntax_kind2.FUNCTOR_MODULE_TYPE
  | Syntax_kind2.WITH_MODULE_TYPE
  | Syntax_kind2.PAREN_MODULE_TYPE
  | Syntax_kind2.OPAQUE_MODULE_TYPE -> true
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

let rec first_descendant_token_matching = fun (node: node) ~matches ->
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
      | Syntax_tree.Node id -> (
          match !found with
          | Some _ -> ()
          | None -> (
              match first_descendant_token_matching (wrap_node node.tree id) ~matches with
              | Some token -> found := Some token
              | None -> ()
            )
        )
      | Syntax_tree.Missing _ ->
          ()
    );
  !found

let child_token_at = fun (node: node) index ->
  match Syntax_tree.child_at node.tree (syntax_node node) index with
  | Some (Syntax_tree.Token id) -> Some (wrap_token node.tree id)
  | Some (Syntax_tree.Node _)
  | Some (Syntax_tree.Missing _)
  | None -> None

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

let child_token_kind_at = fun (node: node) index ->
  match child_token_at node index with
  | Some token -> Some (syntax_token token).Syntax_tree.kind
  | None -> None

let first_ident_token = fun (node: node) ->
  first_child_token_matching node ~matches:(fun kind -> Syntax_kind2.(kind = IDENT))

let first_ident_or_underscore_token = fun (node: node) ->
  first_child_token_matching
    node
    ~matches:(fun kind -> Syntax_kind2.(kind = IDENT || kind = UNDERSCORE))

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

let rec for_each_token_in_node = fun (node: node) ~fn ->
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id -> fn (wrap_token node.tree id)
      | Syntax_tree.Node id -> for_each_token_in_node (wrap_node node.tree id) ~fn
      | Syntax_tree.Missing _ -> ()
    )

let for_each_token_after_child_token = fun (node: node) ~matches ~fn ->
  let seen_boundary = ref false in
  Syntax_tree.for_each_child node.tree (syntax_node node)
    ~fn:(
      function
      | Syntax_tree.Token id ->
          let token = wrap_token node.tree id in
          if !seen_boundary then
            fn token
          else if token_matches token matches then
            seen_boundary := true
      | Syntax_tree.Node id ->
          if !seen_boundary then
            for_each_token_in_node (wrap_node node.tree id) ~fn
      | Syntax_tree.Missing _ ->
          ()
    )

module Token = struct
  type t = token

  let kind = fun (token: token) -> (syntax_token token).Syntax_tree.kind

  let text = fun (token: token) ->
    Syntax_tree.token_text token.tree (syntax_token token)

  let leading_text = fun (token: token) ->
    let syntax_token = syntax_token token in
    Syntax_tree.raw_range_text
      token.tree
      ~raw_lo:syntax_token.Syntax_tree.raw_lo
      ~raw_hi:syntax_token.Syntax_tree.body_raw

  let for_each_leading_trivia = fun (token: token) ~fn ->
    let syntax_token = syntax_token token in
    let rec loop raw_index =
      if Int.(raw_index < syntax_token.Syntax_tree.body_raw) then
        (
          let raw = Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:raw_index in
          fn
            ~kind:raw.Raw_token.kind
            ~text:(Raw_token.text_slice ~source:token.tree.Syntax_tree.source raw);
          loop Int.(raw_index + 1)
        )
    in
    loop syntax_token.Syntax_tree.raw_lo

  let has_leading_raw = fun (token: token) ~matches ->
    let syntax_token = syntax_token token in
    let rec loop raw_index =
      if Int.(raw_index >= syntax_token.Syntax_tree.body_raw) then
        false
      else
        let raw = Vector.get_unchecked token.tree.Syntax_tree.raw_tokens ~at:raw_index in
        if matches raw.Raw_token.kind then
          true
        else
          loop Int.(raw_index + 1)
    in
    loop syntax_token.Syntax_tree.raw_lo

  let has_leading_comment = fun token ->
    has_leading_raw token ~matches:(fun kind -> Syntax_kind2.(kind = COMMENT || kind = DOCSTRING))

  let has_leading_docstring = fun token ->
    has_leading_raw token ~matches:(fun kind -> Syntax_kind2.(kind = DOCSTRING))

  let full_text = fun (token: token) ->
    let raw_lo, raw_hi =
      let token = syntax_token token in
      (token.Syntax_tree.raw_lo, token.Syntax_tree.raw_hi)
    in
    Syntax_tree.raw_range_text token.tree ~raw_lo ~raw_hi

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

  let for_each_token = for_each_token_in_node

  let first_child_node = fun (node: node) ~kind:expected_kind ->
    first_child_node_matching node ~matches:(fun kind -> Syntax_kind2.(kind = expected_kind))

  let first_child_token = fun (node: node) ~kind:expected_kind ->
    first_child_token_matching node ~matches:(fun kind -> Syntax_kind2.(kind = expected_kind))

  let first_token = fun (node: node) -> first_child_token_matching node ~matches:(fun _ -> true)

  let first_descendant_token = fun (node: node) ->
    first_descendant_token_matching node ~matches:(fun _ -> true)
end

module TypeExpr = struct
  type t = type_expr

  type tuple_separator =
    | Star
    | Comma
    | UnknownSeparator

  type view =
    | Path of { path: path }
    | Var of { name: Token.t option }
    | Wildcard
    | Arrow of { left: t option; right: t option }
    | Poly of { body: t option }
    | Labeled of { optional_token: Token.t option; label: Token.t option; annotation: t option }
    | Tuple of { left: t option; right: t option; separator: tuple_separator }
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

  let tuple_separator = fun type_expr ->
    let rec loop index =
      if index >= Node.child_count type_expr then
        UnknownSeparator
      else
        match child_token_at type_expr index with
        | Some token when token_kind_is token Syntax_kind2.STAR -> Star
        | Some token when token_kind_is token Syntax_kind2.COMMA -> Comma
        | _ -> loop (index + 1)
    in
    loop 0

  let rec unwrap_poly_node = fun type_expr ->
    if node_kind_is type_expr Syntax_kind2.TYPE_EXPR then
      match first_child_node_matching type_expr ~matches:is_type_expr_kind with
      | Some child -> unwrap_poly_node child
      | None -> type_expr
    else
      type_expr

  let rec view = fun (type_expr: type_expr) ->
    match Node.kind type_expr with
    | Syntax_kind2.TYPE_EXPR -> (
        match first_child_node_matching type_expr ~matches:is_type_expr_kind, nth_child_node_matching
          type_expr
          1
          ~matches:is_type_expr_kind with
        | Some child, None -> (
            match Node.kind child with
            | Syntax_kind2.TYPE_EXPR -> Opaque type_expr
            | _ -> view child
          )
        | _ -> Opaque type_expr
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
    | Syntax_kind2.POLY_TYPE ->
        Poly { body = first_child_node_matching type_expr ~matches:is_type_expr_kind }
    | Syntax_kind2.LABELED_TYPE ->
        Labeled {
          optional_token = first_child_token_matching
            type_expr
            ~matches:(fun kind -> Syntax_kind2.(kind = QUESTION));
          label = first_ident_token type_expr;
          annotation = first_child_node_matching type_expr ~matches:is_type_expr_kind
        }
    | Syntax_kind2.TUPLE_TYPE ->
        Tuple {
          left = nth_child_node_matching type_expr 0 ~matches:is_type_expr_kind;
          right = nth_child_node_matching type_expr 1 ~matches:is_type_expr_kind;
          separator = tuple_separator type_expr
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

  let for_each_poly_type_name = fun (type_expr: type_expr) ~fn ->
    let type_expr = unwrap_poly_node type_expr in
    let before_dot = ref true in
    Syntax_tree.for_each_child type_expr.tree (syntax_node type_expr)
      ~fn:(
        function
        | Syntax_tree.Token id ->
            let token = wrap_token type_expr.tree id in
            if token_kind_is token Syntax_kind2.DOT then
              before_dot := false
            else if !before_dot && token_kind_is token Syntax_kind2.IDENT then
              fn token
        | Syntax_tree.Node _
        | Syntax_tree.Missing _ -> ()
      )

  let poly_type_keyword_token = fun (type_expr: type_expr) ->
    let type_expr = unwrap_poly_node type_expr in
    first_child_token_matching type_expr ~matches:(fun kind -> Syntax_kind2.(kind = TYPE_KW))
end

module RecordField = struct
  type t = record_field

  let cast = fun (node: node) ->
    if node_matches node is_record_field_kind then
      Some node
    else
      None

  let mutable_token = fun field -> Node.first_child_token field ~kind:Syntax_kind2.MUTABLE_KW

  let name = first_ident_token

  let colon_token = fun field -> Node.first_child_token field ~kind:Syntax_kind2.COLON

  let type_annotation = fun field -> first_child_node_matching field ~matches:is_type_expr_kind
end

module RecordType = struct
  type t = record_type

  let cast = fun (node: node) ->
    if node_matches node is_record_type_kind then
      Some node
    else
      None

  let private_token = fun record_type -> Node.first_child_token record_type ~kind:Syntax_kind2.PRIVATE_KW

  let opening_token = fun record_type -> Node.first_child_token record_type ~kind:Syntax_kind2.LBRACE

  let closing_token = fun record_type -> Node.first_child_token record_type ~kind:Syntax_kind2.RBRACE

  let for_each_field = fun record_type ~fn ->
    for_each_child_node_matching record_type ~matches:is_record_field_kind ~fn
end

module VariantConstructor = struct
  type t = variant_constructor

  let cast = fun (node: node) ->
    if node_matches node is_variant_constructor_kind then
      Some node
    else
      None

  let pipe_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind2.PIPE

  let name = first_ident_token

  let of_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind2.OF_KW

  let colon_token = fun constructor -> Node.first_child_token constructor ~kind:Syntax_kind2.COLON

  let payload_type = fun constructor ->
    match of_token constructor with
    | Some _ -> first_child_node_matching constructor ~matches:is_type_expr_kind
    | None -> None

  let result_type = fun constructor ->
    match colon_token constructor with
    | Some _ -> first_child_node_matching constructor ~matches:is_type_expr_kind
    | None -> None

  let record_payload = fun constructor -> first_child_node_matching constructor ~matches:is_record_type_kind
end

module VariantType = struct
  type t = variant_type

  let cast = fun (node: node) ->
    if node_matches node is_variant_type_kind then
      Some node
    else
      None

  let private_token = fun variant_type -> Node.first_child_token variant_type ~kind:Syntax_kind2.PRIVATE_KW

  let for_each_constructor = fun variant_type ~fn ->
    for_each_child_node_matching variant_type ~matches:is_variant_constructor_kind ~fn
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

  let first_direct_token = fun (node: node) ->
    first_child_token_matching node ~matches:(fun _kind -> true)

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
      operator = first_direct_token expr;
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

module AttributeExpr: sig
  type t = expr
  val cast: expr -> t option

  val inner: t -> expr option

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.ATTRIBUTE_EXPR then
      Some expr
    else
      None

  let inner = first_expr_child

  let for_each_shell_token = Node.for_each_child_token
end

module ExtensionExpr: sig
  type t = expr
  val cast: expr -> t option

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.EXTENSION_EXPR then
      Some expr
    else
      None

  let for_each_shell_token = Node.for_each_child_token
end

module RecordExpr: sig
  type t = expr
  type field = {
    path: path option;
    value: expr option;
    node: record_expr_field;
  }
  val cast: expr -> t option

  val base: t -> expr option

  val for_each_field: t -> fn:(field -> unit) -> unit
end = struct
  type t = expr

  type field = {
    path: path option;
    value: expr option;
    node: record_expr_field;
  }

  let cast = fun (expr: expr) ->
    if
      node_kind_is expr Syntax_kind2.RECORD_EXPR || node_kind_is expr Syntax_kind2.RECORD_UPDATE_EXPR
    then
      Some expr
    else
      None

  let base = fun (record: t) ->
    if node_kind_is record Syntax_kind2.RECORD_UPDATE_EXPR then
      nth_expr_child record 0
    else
      None

  let field_of_node = fun (field: record_expr_field) ->
    match first_expr_child field with
    | Some expr when node_kind_is expr Syntax_kind2.INFIX_EXPR -> (
        match nth_expr_child expr 0, nth_expr_child expr 1 with
        | Some left, Some right ->
            let path =
              if node_kind_is left Syntax_kind2.PATH_EXPR then
                Path.cast left
              else
                None
            in
            { path; value = Some right; node = field }
        | _ -> { path = None; value = None; node = field }
      )
    | Some expr ->
        let path =
          if node_kind_is expr Syntax_kind2.PATH_EXPR then
            Path.cast expr
          else
            None
        in
        let value =
          if Option.is_some (Node.first_child_token field ~kind:Syntax_kind2.EQ) then
            nth_expr_child field 1
          else
            None
        in
        { path; value; node = field }
    | None ->
        { path = None; value = None; node = field }

  let for_each_field = fun (record: t) ~fn ->
    Syntax_tree.for_each_child record.tree (syntax_node record)
      ~fn:(
        function
        | Syntax_tree.Node id ->
            let child = wrap_node record.tree id in
            if node_matches child is_record_expr_field_kind then
              fn (field_of_node child)
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> ()
      )
end

module LocalOpenExpr: sig
  type t = expr
  type view =
    | LetOpen of {
        let_token: token option;
        open_token: token option;
        bang_token: token option;
        module_path: path option;
        in_token: token option;
        body: expr option
      }
    | Delimited of {
        module_path: path option;
        dot_token: token option;
        opening_token: token option;
        body: expr option;
        closing_token: token option
      }
  val cast: expr -> t option

  val view: t -> view
end = struct
  type t = expr

  type view =
    | LetOpen of {
        let_token: token option;
        open_token: token option;
        bang_token: token option;
        module_path: path option;
        in_token: token option;
        body: expr option
      }
    | Delimited of {
        module_path: path option;
        dot_token: token option;
        opening_token: token option;
        body: expr option;
        closing_token: token option
      }

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.LOCAL_OPEN_EXPR then
      Some expr
    else
      None

  let path_expr_child = fun expr index ->
    match nth_expr_child expr index with
    | Some child -> Path.cast child
    | None -> None

  let opening_token = fun expr ->
    match first_child_token_matching
      expr
      ~matches:(fun kind ->
        Syntax_kind2.(kind = LPAREN || kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE)) with
    | Some token -> Some token
    | None -> (
        match nth_expr_child expr 1 with
        | Some body -> first_child_token_matching
          body
          ~matches:(fun kind ->
            Syntax_kind2.(kind = LBRACKET || kind = LBRACKET_BAR || kind = LBRACE))
        | None -> None
      )

  let closing_token = fun expr ->
    match first_child_token_matching
      expr
      ~matches:(fun kind ->
        Syntax_kind2.(kind = RPAREN || kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE)) with
    | Some token -> Some token
    | None -> (
        match nth_expr_child expr 1 with
        | Some body -> first_child_token_matching
          body
          ~matches:(fun kind ->
            Syntax_kind2.(kind = RBRACKET || kind = BAR_RBRACKET || kind = RBRACE))
        | None -> None
      )

  let view = fun expr ->
    if has_child_token_kind expr Syntax_kind2.LET_KW then
      LetOpen {
        let_token = Node.first_child_token expr ~kind:Syntax_kind2.LET_KW;
        open_token = Node.first_child_token expr ~kind:Syntax_kind2.OPEN_KW;
        bang_token = Node.first_child_token expr ~kind:Syntax_kind2.BANG;
        module_path = path_expr_child expr 0;
        in_token = Node.first_child_token expr ~kind:Syntax_kind2.IN_KW;
        body = nth_expr_child expr 1;
      }
    else
      Delimited {
        module_path = path_expr_child expr 0;
        dot_token = Node.first_child_token expr ~kind:Syntax_kind2.DOT;
        opening_token = opening_token expr;
        body = nth_expr_child expr 1;
        closing_token = closing_token expr;
      }
end

module LetModuleExpr: sig
  type t = expr
  type module_body =
    | Path
    | EmptyStruct
    | Unsupported
  val cast: expr -> t option

  val let_token: t -> token option

  val module_token: t -> token option

  val name: t -> token option

  val equals_token: t -> token option

  val in_token: t -> token option

  val module_body: t -> module_body

  val body: t -> expr option

  val for_each_module_body_path_ident: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  type module_body =
    | Path
    | EmptyStruct
    | Unsupported

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.LET_MODULE_EXPR then
      Some expr
    else
      None

  let let_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.LET_KW

  let module_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.MODULE_KW

  let equals_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.EQ

  let in_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.IN_KW

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let module_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_KW))

  let equals_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = EQ))

  let in_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = IN_KW))

  let name = fun expr ->
    match module_index expr with
    | Some module_index -> (
        match child_token_at expr (module_index + 1) with
        | Some token when token_kind_is token Syntax_kind2.IDENT -> Some token
        | _ -> None
      )
    | None -> None

  let range_is_path = fun expr start stop ->
    let rec loop index saw_ident expect_ident =
      if index >= stop then
        saw_ident && not expect_ident
      else
        match child_token_at expr index with
        | Some token when token_kind_is token Syntax_kind2.IDENT && expect_ident -> loop
          (index + 1)
          true
          false
        | Some token when token_kind_is token Syntax_kind2.DOT && saw_ident && not expect_ident -> loop
          (index + 1)
          saw_ident
          true
        | _ -> false
    in
    loop start false true

  let range_has_exact_tokens = fun expr start stop left right ->
    Int.equal (start + 2) stop
    && match child_token_at expr start, child_token_at expr (start + 1) with
    | Some left_token, Some right_token -> token_kind_is left_token left
    && token_kind_is right_token right
    | _ -> false

  let module_body_bounds = fun expr ->
    match equals_index expr, in_index expr with
    | Some equals_index, Some in_index when equals_index < in_index -> Some (
      equals_index + 1,
      in_index
    )
    | _ -> None

  let module_body = fun expr ->
    match module_body_bounds expr with
    | Some (start, stop) when range_is_path expr start stop -> Path
    | Some (start, stop) when range_has_exact_tokens
      expr
      start
      stop
      Syntax_kind2.STRUCT_KW
      Syntax_kind2.END_KW -> EmptyStruct
    | _ -> Unsupported

  let body = first_expr_child

  let for_each_module_body_path_ident = fun expr ~fn ->
    match module_body_bounds expr with
    | Some (start, stop) when range_is_path expr start stop ->
        let rec loop index =
          if index < stop then
            (
              match child_token_at expr index with
              | Some token when token_kind_is token Syntax_kind2.IDENT ->
                  fn token;
                  loop (index + 1)
              | _ -> loop (index + 1)
            )
        in
        loop start
    | _ -> ()
end

module LetExceptionExpr: sig
  type t = expr
  val cast: expr -> t option

  val let_token: t -> token option

  val exception_token: t -> token option

  val name: t -> token option

  val of_token: t -> token option

  val in_token: t -> token option

  val body: t -> expr option

  val for_each_payload_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.LET_EXCEPTION_EXPR then
      Some expr
    else
      None

  let let_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.LET_KW

  let exception_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.EXCEPTION_KW

  let of_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.OF_KW

  let in_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.IN_KW

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let exception_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = EXCEPTION_KW))

  let of_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = OF_KW))

  let in_index = fun expr ->
    token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = IN_KW))

  let name = fun expr ->
    match exception_index expr with
    | Some exception_index -> (
        match child_token_at expr (exception_index + 1) with
        | Some token when token_kind_is token Syntax_kind2.IDENT -> Some token
        | _ -> None
      )
    | None -> None

  let body = first_expr_child

  let payload_bounds = fun expr ->
    match of_index expr, in_index expr with
    | Some of_index, Some in_index when of_index < in_index -> Some (of_index + 1, in_index)
    | _ -> None

  let for_each_payload_token = fun expr ~fn ->
    match payload_bounds expr with
    | None -> ()
    | Some (start, stop) ->
        let rec loop index =
          if index < stop then
            (
              match child_token_at expr index with
              | Some token ->
                  fn token;
                  loop (index + 1)
              | None -> loop (index + 1)
            )
        in
        loop start
end

module UnreachableExpr: sig
  type t = expr
  val cast: expr -> t option

  val dot_token: t -> token option
end = struct
  type t = expr

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.UNREACHABLE_EXPR then
      Some expr
    else
      None

  let dot_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.DOT
end

module FirstClassModuleExpr: sig
  type t = expr
  type module_path =
    | ModulePath
    | UnsupportedModulePath
  type ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription
  val cast: expr -> t option

  val opening_token: t -> token option

  val module_token: t -> token option

  val colon_token: t -> token option

  val closing_token: t -> token option

  val module_path: t -> module_path

  val ascription: t -> ascription

  val for_each_module_path_ident: t -> fn:(token -> unit) -> unit

  val for_each_ascription_path_ident: t -> fn:(token -> unit) -> unit
end = struct
  type t = expr

  type module_path =
    | ModulePath
    | UnsupportedModulePath

  type ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.FIRST_CLASS_MODULE_EXPR then
      Some expr
    else
      None

  let opening_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.LPAREN

  let module_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.MODULE_KW

  let colon_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.COLON

  let closing_token = fun expr -> Node.first_child_token expr ~kind:Syntax_kind2.RPAREN

  let token_index = fun expr ~from ~matches ->
    let count = Node.child_count expr in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at expr index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let range_is_path = fun expr start stop ->
    let rec loop index saw_ident expect_ident =
      if index >= stop then
        saw_ident && not expect_ident
      else
        match child_token_at expr index with
        | Some token when token_kind_is token Syntax_kind2.IDENT && expect_ident -> loop
          (index + 1)
          true
          false
        | Some token when token_kind_is token Syntax_kind2.DOT && saw_ident && not expect_ident -> loop
          (index + 1)
          saw_ident
          true
        | _ -> false
    in
    loop start false true

  let module_path_bounds = fun expr ->
    match token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_KW)) with
    | None -> None
    | Some module_index ->
        let start = module_index + 1 in
        token_index
          expr
          ~from:start
          ~matches:(fun kind -> Syntax_kind2.(kind = COLON || kind = RPAREN))
        |> Option.map ~fn:(fun stop -> (start, stop))

  let ascription_bounds = fun expr ->
    match token_index expr ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = COLON)) with
    | None -> None
    | Some colon_index ->
        let start = colon_index + 1 in
        token_index expr ~from:start ~matches:(fun kind -> Syntax_kind2.(kind = RPAREN))
        |> Option.map ~fn:(fun stop -> (start, stop))

  let module_path = fun expr ->
    match module_path_bounds expr with
    | Some (start, stop) when range_is_path expr start stop -> ModulePath
    | _ -> UnsupportedModulePath

  let ascription = fun expr ->
    match colon_token expr, ascription_bounds expr with
    | None, _ -> NoAscription
    | Some _, Some (start, stop) when range_is_path expr start stop -> PathAscription
    | Some _, _ -> UnsupportedAscription

  let for_each_ident_in_range = fun expr start stop ~fn ->
    let rec loop index =
      if index < stop then
        (
          match child_token_at expr index with
          | Some token when token_kind_is token Syntax_kind2.IDENT ->
              fn token;
              loop (index + 1)
          | _ -> loop (index + 1)
        )
    in
    loop start

  let for_each_module_path_ident = fun expr ~fn ->
    match module_path_bounds expr with
    | Some (start, stop) when range_is_path expr start stop -> for_each_ident_in_range
      expr
      start
      stop
      ~fn
    | _ -> ()

  let for_each_ascription_path_ident = fun expr ~fn ->
    match ascription_bounds expr with
    | Some (start, stop) when range_is_path expr start stop -> for_each_ident_in_range
      expr
      start
      stop
      ~fn
    | _ -> ()
end

module BindingOperatorExpr: sig
  type t = expr
  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }
  val cast: expr -> t option

  val in_token: t -> Token.t option

  val body: t -> expr option

  val for_each_clause: t -> fn:(clause -> unit) -> unit
end = struct
  type t = expr

  type clause = {
    keyword: Token.t option;
    operator: Token.t option;
    binding: let_binding;
  }

  let cast = fun (expr: expr) ->
    if node_kind_is expr Syntax_kind2.BINDING_OPERATOR_EXPR then
      Some expr
    else
      None

  let in_token = fun (expr: t) -> Node.first_child_token expr ~kind:Syntax_kind2.IN_KW

  let body = first_expr_child

  let binding_operator_keyword = fun token ->
    token_kind_is token Syntax_kind2.LET_KW || token_kind_is token Syntax_kind2.AND_KW

  let binding_operator_suffix = fun token ->
    token_kind_is token Syntax_kind2.STAR || token_kind_is token Syntax_kind2.PLUS

  let for_each_clause = fun (expr: t) ~fn ->
    let child_count = Node.child_count expr in
    let rec loop index keyword operator =
      if index >= child_count then
        ()
      else
        match Node.child_at expr index with
        | Some (Syntax_tree.Token id) ->
            let token = wrap_token expr.tree id in
            if binding_operator_keyword token then
              loop (index + 1) (Some token) None
            else if binding_operator_suffix token then
              loop (index + 1) keyword (Some token)
            else
              loop (index + 1) keyword operator
        | Some (Syntax_tree.Node id) ->
            let child = wrap_node expr.tree id in
            if node_matches child is_let_binding_kind then
              (
                fn { keyword; operator; binding = child };
                loop (index + 1) None None
              )
            else
              loop (index + 1) keyword operator
        | Some (Syntax_tree.Missing _)
        | None ->
            loop (index + 1) keyword operator
    in
    loop 0 None None
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

  val literal_sign_token: t -> token option

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

  let literal_token = fun pattern ->
    first_child_token_matching
      pattern
      ~matches:(fun kind ->
        Syntax_kind2.(kind = INT
        || kind = FLOAT
        || kind = STRING
        || kind = CHAR
        || kind = TRUE_KW
        || kind = FALSE_KW))

  let literal_sign_token = fun pattern ->
    first_child_token_matching
      pattern
      ~matches:(fun kind ->
        Syntax_kind2.(kind = PLUS || kind = MINUS || kind = PLUSDOT || kind = MINUSDOT))

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

module AttributePattern: sig
  type t = pattern
  val cast: pattern -> t option

  val inner: t -> pattern option

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.ATTRIBUTE_PATTERN then
      Some pattern
    else
      None

  let inner = first_pattern_child

  let for_each_shell_token = Node.for_each_child_token
end

module ExtensionPattern: sig
  type t = pattern
  val cast: pattern -> t option

  val for_each_shell_token: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.EXTENSION_PATTERN then
      Some pattern
    else
      None

  let for_each_shell_token = Node.for_each_child_token
end

module LocallyAbstractTypePattern: sig
  type t = pattern
  val cast: pattern -> t option

  val opening_token: t -> token option

  val type_token: t -> token option

  val closing_token: t -> token option

  val for_each_type_name: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.LOCALLY_ABSTRACT_TYPE_PATTERN then
      Some pattern
    else
      None

  let opening_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.LPAREN

  let type_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.TYPE_KW

  let closing_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.RPAREN

  let for_each_type_name = fun pattern ~fn ->
    Node.for_each_child_token pattern
      ~fn:(fun token ->
        if token_kind_is token Syntax_kind2.IDENT then
          fn token)
end

module FirstClassModulePattern: sig
  type t = pattern
  type ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription
  val cast: pattern -> t option

  val opening_token: t -> token option

  val module_token: t -> token option

  val binder: t -> token option

  val colon_token: t -> token option

  val closing_token: t -> token option

  val ascription: t -> ascription

  val for_each_ascription_path_ident: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  type ascription =
    | NoAscription
    | PathAscription
    | UnsupportedAscription

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.FIRST_CLASS_MODULE_PATTERN then
      Some pattern
    else
      None

  let opening_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.LPAREN

  let module_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.MODULE_KW

  let colon_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.COLON

  let closing_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.RPAREN

  let token_index = fun pattern ~from ~matches ->
    let count = Node.child_count pattern in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at pattern index with
        | Some token when matches (Token.kind token) -> Some index
        | _ -> loop (index + 1)
    in
    loop from

  let module_index = fun pattern ->
    token_index pattern ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_KW))

  let binder = fun pattern ->
    match module_index pattern with
    | Some module_index -> (
        match child_token_at pattern (module_index + 1) with
        | Some token when token_kind_is token Syntax_kind2.IDENT
        || token_kind_is token Syntax_kind2.UNDERSCORE -> Some token
        | _ -> None
      )
    | None -> None

  let range_is_path = fun pattern start stop ->
    let rec loop index saw_ident expect_ident =
      if index >= stop then
        saw_ident && not expect_ident
      else
        match child_token_at pattern index with
        | Some token when token_kind_is token Syntax_kind2.IDENT && expect_ident -> loop
          (index + 1)
          true
          false
        | Some token when token_kind_is token Syntax_kind2.DOT && saw_ident && not expect_ident -> loop
          (index + 1)
          saw_ident
          true
        | _ -> false
    in
    loop start false true

  let ascription_bounds = fun pattern ->
    match token_index pattern ~from:0 ~matches:(fun kind -> Syntax_kind2.(kind = COLON)) with
    | None -> None
    | Some colon_index ->
        let start = colon_index + 1 in
        token_index pattern ~from:start ~matches:(fun kind -> Syntax_kind2.(kind = RPAREN))
        |> Option.map ~fn:(fun stop -> (start, stop))

  let ascription = fun pattern ->
    match colon_token pattern, ascription_bounds pattern with
    | None, _ -> NoAscription
    | Some _, Some (start, stop) when range_is_path pattern start stop -> PathAscription
    | Some _, _ -> UnsupportedAscription

  let for_each_ident_in_range = fun pattern start stop ~fn ->
    let rec loop index =
      if index < stop then
        (
          match child_token_at pattern index with
          | Some token when token_kind_is token Syntax_kind2.IDENT ->
              fn token;
              loop (index + 1)
          | _ -> loop (index + 1)
        )
    in
    loop start

  let for_each_ascription_path_ident = fun pattern ~fn ->
    match ascription_bounds pattern with
    | Some (start, stop) when range_is_path pattern start stop -> for_each_ident_in_range
      pattern
      start
      stop
      ~fn
    | _ -> ()
end

module RecordPattern: sig
  type t = pattern
  type field = {
    path: path option;
    pattern: pattern option;
    node: pattern;
  }
  val cast: pattern -> t option

  val open_wildcard: t -> Token.t option

  val for_each_field: t -> fn:(field -> unit) -> unit
end = struct
  type t = pattern

  type field = {
    path: path option;
    pattern: pattern option;
    node: pattern;
  }

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.RECORD_PATTERN then
      Some pattern
    else
      None

  let open_wildcard = fun (record: t) ->
    let found = ref None in
    Node.for_each_child_node record
      ~fn:(fun child ->
        match !found with
        | Some _ -> ()
        | None ->
            if node_kind_is child Syntax_kind2.WILDCARD_PATTERN then
              found := Node.first_child_token child ~kind:Syntax_kind2.UNDERSCORE);
    !found

  let child_node_at = fun (record: t) index ->
    match Node.child_at record index with
    | Some (Syntax_tree.Node id) -> Some (wrap_node record.tree id)
    | Some (Syntax_tree.Token _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_kind_at = fun (record: t) index ->
    match Node.child_at record index with
    | Some (Syntax_tree.Token id) -> Some (Token.kind (wrap_token record.tree id))
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let for_each_field = fun (record: t) ~fn ->
    let child_count = Node.child_count record in
    let rec loop index =
      if index >= child_count then
        ()
      else
        match child_node_at record index with
        | Some child when node_kind_is child Syntax_kind2.PATH_PATTERN ->
            let pattern, next =
              match child_token_kind_at record (index + 1) with
              | Some kind when Syntax_kind2.(kind = EQ) -> (
                  match child_node_at record (index + 2) with
                  | Some value when node_matches value is_pattern_kind -> (Some value, index + 3)
                  | _ -> (None, index + 2)
                )
              | _ -> (None, index + 1)
            in
            fn { path = Path.cast child; pattern; node = child };
            loop next
        | Some child when node_kind_is child Syntax_kind2.WILDCARD_PATTERN ->
            loop (index + 1)
        | Some child when node_matches child is_pattern_kind ->
            fn { path = None; pattern = None; node = child };
            loop (index + 1)
        | Some _
        | None ->
            loop (index + 1)
    in
    loop 0
end

module LocalOpenPattern: sig
  type t = pattern
  val cast: pattern -> t option

  val dot_token: t -> token option

  val opening_token: t -> token option

  val closing_token: t -> token option

  val pattern: t -> pattern option

  val for_each_module_path_ident: t -> fn:(token -> unit) -> unit
end = struct
  type t = pattern

  let cast = fun (pattern: pattern) ->
    if node_kind_is pattern Syntax_kind2.LOCAL_OPEN_PATTERN then
      Some pattern
    else
      None

  let dot_token = fun pattern -> Node.first_child_token pattern ~kind:Syntax_kind2.DOT

  let delimiter_child = first_pattern_child

  let opening_token = fun pattern ->
    let matches kind =
      Syntax_kind2.(kind = LPAREN || kind = LBRACE || kind = LBRACKET || kind = LBRACKET_BAR) in
    match first_child_token_matching pattern ~matches with
    | Some token -> Some token
    | None -> (
        match delimiter_child pattern with
        | Some child -> first_child_token_matching child ~matches
        | None -> None
      )

  let closing_token = fun pattern ->
    let matches kind =
      Syntax_kind2.(kind = RPAREN || kind = RBRACE || kind = RBRACKET || kind = BAR_RBRACKET) in
    match first_child_token_matching pattern ~matches with
    | Some token -> Some token
    | None -> (
        match delimiter_child pattern with
        | Some child -> first_child_token_matching child ~matches
        | None -> None
      )

  let pattern = first_pattern_child

  let for_each_module_path_ident = fun pattern ~fn ->
    let child_count = Node.child_count pattern in
    let rec loop index =
      if index >= child_count then
        ()
      else
        match child_token_at pattern index with
        | Some token when token_kind_is token Syntax_kind2.LPAREN ->
            ()
        | Some token ->
            if token_kind_is token Syntax_kind2.IDENT then
              fn token;
            loop (index + 1)
        | None ->
            loop (index + 1)
    in
    loop 0
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

  let parameter_label_token = fun parameter ->
    match first_ident_token parameter with
    | Some token -> Some token
    | None -> first_descendant_token_matching
      parameter
      ~matches:(fun kind -> Syntax_kind2.(kind = IDENT))

  let view = fun (parameter: parameter) ->
    match Node.kind parameter with
    | Syntax_kind2.LABELED_PARAM -> Labeled {
      label = parameter_label_token parameter;
      pattern = first_pattern_child parameter
    }
    | Syntax_kind2.OPTIONAL_PARAM -> Optional {
      label = parameter_label_token parameter;
      pattern = first_pattern_child parameter
    }
    | Syntax_kind2.OPTIONAL_PARAM_DEFAULT -> OptionalDefault {
      label = parameter_label_token parameter;
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

  let rec for_each_parameter_pattern = fun pattern ~fn ->
    match Pattern.view pattern with
    | Apply { callee=Some callee; argument=Some argument } ->
        for_each_parameter_pattern callee ~fn;
        for_each_parameter_pattern argument ~fn
    | _ -> fn pattern

  let for_each_parameter = fun (binding: let_binding) ~fn ->
    let seen_first = ref false in
    for_each_child_node_matching binding ~matches:is_pattern_kind
      ~fn:(fun pattern ->
        if !seen_first then
          for_each_parameter_pattern pattern ~fn
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

  type member = {
    declaration: type_declaration;
    node: node;
    start_index: int;
    stop_index: int;
  }

  type parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option
      }
    | Wildcard of { wildcard: Token.t; variance: Token.t option; injective: Token.t option }

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.TYPE_DECL then
      Some node
    else
      None

  let first_member_node = fun decl ->
    first_child_node_matching decl ~matches:(fun kind -> Syntax_kind2.(kind = TYPE_DECL_MEMBER))

  let member_or_decl = fun decl ->
    match first_member_node decl with
    | Some member -> member
    | None -> decl

  let for_each_token = fun decl ~fn -> for_each_token_in_node decl ~fn

  let keyword_token = fun decl -> Node.first_child_token (member_or_decl decl) ~kind:Syntax_kind2.TYPE_KW

  let nonrec_token = fun decl -> Node.first_child_token (member_or_decl decl) ~kind:Syntax_kind2.NONREC_KW

  let child_token_at_node = fun node index ->
    match Node.child_at node index with
    | Some (Syntax_tree.Token id) -> Some (wrap_token node.tree id)
    | Some (Syntax_tree.Node _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let child_token_at = fun (decl: type_declaration) index -> child_token_at_node decl index

  let child_token_kind_at_node = fun node index ->
    match child_token_at_node node index with
    | Some token -> Some (Token.kind token)
    | None -> None

  let child_token_kind_at = fun decl index ->
    match child_token_at decl index with
    | Some token -> Some (Token.kind token)
    | None -> None

  let rec collect_type_parameter_modifiers_in = fun node index variance injective ->
    match child_token_at_node node index with
    | Some token when token_kind_is token Syntax_kind2.PLUS || token_kind_is token Syntax_kind2.MINUS -> collect_type_parameter_modifiers_in
      node
      (index + 1)
      (Some token)
      injective
    | Some token when token_kind_is token Syntax_kind2.BANG -> collect_type_parameter_modifiers_in
      node
      (index + 1)
      variance
      (Some token)
    | _ -> (index, variance, injective)

  let skip_type_parameter_in = fun node index ->
    let index, _, _ = collect_type_parameter_modifiers_in node index None None in
    match child_token_kind_at_node node index with
    | Some Syntax_kind2.QUOTE -> (
        match child_token_kind_at_node node (index + 1) with
        | Some Syntax_kind2.IDENT -> index + 2
        | _ -> index + 1
      )
    | Some Syntax_kind2.UNDERSCORE ->
        index + 1
    | _ ->
        index

  let emit_type_parameter_in = fun node index ~fn ->
    let index, variance, injective = collect_type_parameter_modifiers_in node index None None in
    match child_token_at_node node index with
    | Some quote when token_kind_is quote Syntax_kind2.QUOTE -> (
        match child_token_at_node node (index + 1) with
        | Some name when token_kind_is name Syntax_kind2.IDENT ->
            fn (Named { name; quote = Some quote; variance; injective });
            index + 2
        | _ -> index + 1
      )
    | Some wildcard when token_kind_is wildcard Syntax_kind2.UNDERSCORE ->
        fn (Wildcard { wildcard; variance; injective });
        index + 1
    | _ ->
        index

  let rec skip_parenthesized_type_parameters_in = fun node index ->
    match child_token_kind_at_node node index with
    | Some Syntax_kind2.RPAREN -> index + 1
    | Some Syntax_kind2.EOF
    | None -> index
    | _ -> skip_parenthesized_type_parameters_in node (index + 1)

  let rec find_node_in = fun node index ~matches ->
    if index >= Node.child_count node then
      None
    else
      match Node.child_at node index with
      | Some (Syntax_tree.Node id) ->
          let child = wrap_node node.tree id in
          if node_matches child matches then
            Some child
          else
            find_node_in node (index + 1) ~matches
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> find_node_in node (index + 1) ~matches

  let name = fun decl ->
    let node = member_or_decl decl in
    let rec loop index =
      match child_token_at_node node index with
      | Some token when token_kind_is token Syntax_kind2.TYPE_KW
      || token_kind_is token Syntax_kind2.NONREC_KW ->
          loop (index + 1)
      | Some token when token_kind_is token Syntax_kind2.LPAREN ->
          loop (skip_parenthesized_type_parameters_in node (index + 1))
      | Some token when token_kind_is token Syntax_kind2.PLUS
      || token_kind_is token Syntax_kind2.MINUS
      || token_kind_is token Syntax_kind2.BANG
      || token_kind_is token Syntax_kind2.QUOTE
      || token_kind_is token Syntax_kind2.UNDERSCORE ->
          let next = skip_type_parameter_in node index in
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
    let node = member_or_decl decl in
    let rec parse_parenthesized index =
      match child_token_kind_at_node node index with
      | Some Syntax_kind2.RPAREN ->
          index + 1
      | Some Syntax_kind2.COMMA ->
          parse_parenthesized (index + 1)
      | Some Syntax_kind2.EOF
      | None ->
          index
      | _ ->
          let next = emit_type_parameter_in node index ~fn in
          parse_parenthesized
            (
              if next > index then
                next
              else
                index + 1
            )
    in
    let rec parse_head index =
      match child_token_at_node node index with
      | Some token when token_kind_is token Syntax_kind2.TYPE_KW
      || token_kind_is token Syntax_kind2.NONREC_KW ->
          parse_head (index + 1)
      | Some token when token_kind_is token Syntax_kind2.LPAREN ->
          parse_head (parse_parenthesized (index + 1))
      | Some token when token_kind_is token Syntax_kind2.PLUS
      || token_kind_is token Syntax_kind2.MINUS
      || token_kind_is token Syntax_kind2.BANG
      || token_kind_is token Syntax_kind2.QUOTE
      || token_kind_is token Syntax_kind2.UNDERSCORE ->
          let next = emit_type_parameter_in node index ~fn in
          if next > index then
            parse_head next
      | _ ->
          ()
    in
    parse_head 0

  let manifest = fun decl -> find_node_in (member_or_decl decl) 0 ~matches:is_type_expr_kind

  module Member = struct
    type t = member

    let declaration = fun member -> member.declaration

    let start_index = fun member -> member.start_index

    let stop_index = fun member -> member.stop_index

    let child_count = fun member -> Node.child_count member.node

    let child_at = fun member index ->
      if index < 0 || index >= child_count member then
        None
      else
        Node.child_at member.node index

    let child_token_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Token id) -> Some (wrap_token member.node.tree id)
      | Some (Syntax_tree.Node _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_node_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Node id) -> Some (wrap_node member.node.tree id)
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_token_kind_at = fun member index ->
      match child_token_at member index with
      | Some token -> Some (Token.kind token)
      | None -> None

    let child_token_kind_is = fun member index kind ->
      match child_token_at member index with
      | Some token -> token_kind_is token kind
      | None -> false

    let for_each_child = fun member ~fn ->
      let rec loop index =
        if index < Node.child_count member.node then
          (
            (
              match Node.child_at member.node index with
              | Some child -> fn child
              | None -> ()
            );
            loop (index + 1)
          )
      in
      loop 0

    let for_each_child_token = fun member ~fn ->
      for_each_child member
        ~fn:(
          function
          | Syntax_tree.Token id -> fn (wrap_token member.node.tree id)
          | Syntax_tree.Node _
          | Syntax_tree.Missing _ -> ()
        )

    let for_each_child_node = fun member ~fn ->
      for_each_child member
        ~fn:(
          function
          | Syntax_tree.Node id -> fn (wrap_node member.node.tree id)
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> ()
        )

    let find_node = fun member ~matches -> find_node_in member.node 0 ~matches

    let record_type = fun member -> find_node member ~matches:is_record_type_kind

    let variant_type = fun member -> find_node member ~matches:is_variant_type_kind

    let shell_token = fun member -> child_token_at member 0

    let nonrec_token = fun member ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else
          match child_token_at member index with
          | Some token when token_kind_is token Syntax_kind2.NONREC_KW -> Some token
          | Some token when token_kind_is token Syntax_kind2.IDENT -> None
          | _ -> loop (index + 1)
      in
      loop 0

    let name = fun member ->
      let rec loop index =
        match child_token_at member index with
        | Some token when token_kind_is token Syntax_kind2.TYPE_KW
        || token_kind_is token Syntax_kind2.AND_KW
        || token_kind_is token Syntax_kind2.NONREC_KW ->
            loop (index + 1)
        | Some token when token_kind_is token Syntax_kind2.LPAREN ->
            loop (skip_parenthesized_type_parameters_in member.node (index + 1))
        | Some token when token_kind_is token Syntax_kind2.PLUS
        || token_kind_is token Syntax_kind2.MINUS
        || token_kind_is token Syntax_kind2.BANG
        || token_kind_is token Syntax_kind2.QUOTE
        || token_kind_is token Syntax_kind2.UNDERSCORE ->
            let next = skip_type_parameter_in member.node index in
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

    let for_each_parameter = fun member ~fn ->
      let rec parse_parenthesized index =
        match child_token_kind_at member index with
        | Some Syntax_kind2.RPAREN ->
            index + 1
        | Some Syntax_kind2.COMMA ->
            parse_parenthesized (index + 1)
        | Some Syntax_kind2.EOF
        | None ->
            index
        | _ ->
            let next = emit_type_parameter_in member.node index ~fn in
            parse_parenthesized
              (
                if next > index then
                  next
                else
                  index + 1
              )
      in
      let rec parse_head index =
        match child_token_at member index with
        | Some token when token_kind_is token Syntax_kind2.TYPE_KW
        || token_kind_is token Syntax_kind2.AND_KW
        || token_kind_is token Syntax_kind2.NONREC_KW ->
            parse_head (index + 1)
        | Some token when token_kind_is token Syntax_kind2.LPAREN ->
            parse_head (parse_parenthesized (index + 1))
        | Some token when token_kind_is token Syntax_kind2.PLUS
        || token_kind_is token Syntax_kind2.MINUS
        || token_kind_is token Syntax_kind2.BANG
        || token_kind_is token Syntax_kind2.QUOTE
        || token_kind_is token Syntax_kind2.UNDERSCORE ->
            let next = emit_type_parameter_in member.node index ~fn in
            if next > index then
              parse_head next
        | _ ->
            ()
      in
      parse_head 0

    let manifest = fun member -> find_node_in member.node 0 ~matches:is_type_expr_kind
  end

  let for_each_member = fun decl ~fn ->
    let saw_member = ref false in
    let index = ref 0 in
    Node.for_each_child decl
      ~fn:(
        function
        | Syntax_tree.Node id ->
            let node = wrap_node decl.tree id in
            if node_kind_is node Syntax_kind2.TYPE_DECL_MEMBER then
              (
                saw_member := true;
                fn { declaration = decl; node; start_index = !index; stop_index = !index + 1 }
              );
            index := !index + 1
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> index := !index + 1
      );
    if not !saw_member then
      fn { declaration = decl; node = decl; start_index = 0; stop_index = Node.child_count decl }

  let fold_members = fun decl init fn ->
    let acc = ref init in
    for_each_member decl ~fn:(fun member -> acc := fn !acc member);
    !acc
end

module TypeExtensionDeclaration = struct
  type t = type_extension_declaration

  type parameter = TypeDeclaration.parameter =
    | Named of {
        name: Token.t;
        quote: Token.t option;
        variance: Token.t option;
        injective: Token.t option
      }
    | Wildcard of { wildcard: Token.t; variance: Token.t option; injective: Token.t option }

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.TYPE_EXTENSION_DECL then
      Some node
    else
      None

  let head_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind2.(kind = TYPE_EXTENSION_DECL_HEAD))

  let body_node = fun decl ->
    first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind2.(kind = TYPE_EXTENSION_DECL_BODY))

  let keyword_token = fun decl ->
    match head_node decl with
    | Some head -> Node.first_child_token head ~kind:Syntax_kind2.TYPE_KW
    | None -> None

  let plus_token = fun decl ->
    match body_node decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind2.PLUS
    | None -> None

  let equals_token = fun decl ->
    match body_node decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind2.EQ
    | None -> None

  let rec collect_type_parameter_modifiers = fun decl index variance injective ->
    match child_token_at decl index with
    | Some token when token_kind_is token Syntax_kind2.PLUS || token_kind_is token Syntax_kind2.MINUS -> collect_type_parameter_modifiers
      decl
      (index + 1)
      (Some token)
      injective
    | Some token when token_kind_is token Syntax_kind2.BANG -> collect_type_parameter_modifiers
      decl
      (index + 1)
      variance
      (Some token)
    | _ -> (index, variance, injective)

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
        | _ -> index + 1
      )
    | Some wildcard when token_kind_is wildcard Syntax_kind2.UNDERSCORE ->
        fn (Wildcard { wildcard; variance; injective });
        index + 1
    | _ ->
        index

  let rec skip_parenthesized_type_parameters = fun decl index ->
    match child_token_kind_at decl index with
    | Some Syntax_kind2.RPAREN -> index + 1
    | Some Syntax_kind2.EOF
    | None -> index
    | _ -> skip_parenthesized_type_parameters decl (index + 1)

  let for_each_parameter = fun decl ~fn ->
    match head_node decl with
    | None -> ()
    | Some head ->
        let rec parse_parenthesized index =
          match child_token_kind_at head index with
          | Some Syntax_kind2.RPAREN ->
              index + 1
          | Some Syntax_kind2.COMMA ->
              parse_parenthesized (index + 1)
          | Some Syntax_kind2.EOF
          | None ->
              index
          | _ ->
              let next = emit_type_parameter head index ~fn in
              parse_parenthesized
                (
                  if next > index then
                    next
                  else
                    index + 1
                )
        in
        let rec parse_head index =
          match child_token_at head index with
          | Some token when token_kind_is token Syntax_kind2.TYPE_KW ->
              parse_head (index + 1)
          | Some token when token_kind_is token Syntax_kind2.LPAREN ->
              parse_head (parse_parenthesized (index + 1))
          | Some token when token_kind_is token Syntax_kind2.PLUS
          || token_kind_is token Syntax_kind2.MINUS
          || token_kind_is token Syntax_kind2.BANG
          || token_kind_is token Syntax_kind2.QUOTE
          || token_kind_is token Syntax_kind2.UNDERSCORE ->
              let next = emit_type_parameter head index ~fn in
              if next > index then
                parse_head next
          | _ ->
              ()
        in
        parse_head 0

  let for_each_name_ident = fun decl ~fn ->
    match head_node decl with
    | None -> ()
    | Some head ->
        let rec parse_name index =
          match child_token_at head index with
          | Some token when token_kind_is token Syntax_kind2.IDENT ->
              fn token;
              parse_name (index + 1)
          | Some token when token_kind_is token Syntax_kind2.DOT ->
              parse_name (index + 1)
          | Some _ ->
              parse_name (index + 1)
          | None ->
              ()
        in
        let rec parse_head index =
          match child_token_at head index with
          | Some token when token_kind_is token Syntax_kind2.TYPE_KW ->
              parse_head (index + 1)
          | Some token when token_kind_is token Syntax_kind2.LPAREN ->
              parse_head (skip_parenthesized_type_parameters head (index + 1))
          | Some token when token_kind_is token Syntax_kind2.PLUS
          || token_kind_is token Syntax_kind2.MINUS
          || token_kind_is token Syntax_kind2.BANG
          || token_kind_is token Syntax_kind2.QUOTE
          || token_kind_is token Syntax_kind2.UNDERSCORE ->
              let next = skip_type_parameter head index in
              if next > index then
                parse_head next
          | _ ->
              parse_name index
        in
        parse_head 0

  let name = fun decl ->
    let found = ref None in
    for_each_name_ident decl ~fn:(fun token -> found := Some token);
    !found

  let variant_type = fun decl ->
    match body_node decl with
    | Some body -> first_child_node_matching body ~matches:is_variant_type_kind
    | None -> None
end

let child_token_kind_is = fun node index kind ->
  match child_token_at node index with
  | Some token -> token_kind_is token kind
  | None -> false

let attribute_suffix_start_at = fun node close_index ->
  if not (child_token_kind_is node close_index Syntax_kind2.RBRACKET) then
    None
  else
    let rec loop index depth =
      if Int.(index < 0) then
        None
      else if child_token_kind_is node index Syntax_kind2.RBRACKET then
        loop Int.(index - 1) Int.(depth + 1)
      else if child_token_kind_is node index Syntax_kind2.LBRACKET then
        if Int.equal depth 1 then
          let next = Int.(index + 1) in
          if
            child_token_kind_is node next Syntax_kind2.AT
            || child_token_kind_is node next Syntax_kind2.ATAT
          then
            Some index
          else
            None
        else
          loop Int.(index - 1) Int.(depth - 1)
      else
        loop Int.(index - 1) depth
    in
    loop Int.(close_index - 1) 1

let last_non_attribute_suffix_token_index = fun node ->
  let rec loop index =
    if Int.(index < 0) then
      (-1)
    else
      match attribute_suffix_start_at node index with
      | Some start -> loop Int.(start - 1)
      | None -> index
  in
  loop Int.(Node.child_count node - 1)

module ModuleDeclaration = struct
  type t = module_declaration

  type member = {
    declaration: module_declaration;
    node: node;
    start_index: int;
    stop_index: int;
  }

  type body =
    | Path
    | EmptyStruct
    | EmptySig
    | Sig
    | Unsupported

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.MODULE_DECL then
      Some node
    else
      None

  let first_member_node = fun decl ->
    first_child_node_matching decl ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_DECL_MEMBER))

  let member_or_decl = fun decl ->
    match first_member_node decl with
    | Some member -> member
    | None -> decl

  let name = fun decl -> first_ident_or_underscore_token (member_or_decl decl)

  let rec_token = fun decl -> Node.first_child_token (member_or_decl decl) ~kind:Syntax_kind2.REC_KW

  let is_recursive = fun decl ->
    match rec_token decl with
    | Some _ -> true
    | None -> false

  let separator_token = fun decl ->
    first_child_token_matching
      (member_or_decl decl)
      ~matches:(fun kind -> Syntax_kind2.(kind = EQ || kind = COLON))

  module Member = struct
    type t = member

    let declaration = fun member -> member.declaration

    let start_index = fun member -> member.start_index

    let stop_index = fun member -> member.stop_index

    let child_count = fun member -> Node.child_count member.node

    let child_at = fun member index ->
      Node.child_at member.node index

    let child_token_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Token id) -> Some (wrap_token member.node.tree id)
      | Some (Syntax_tree.Node _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_node_at = fun member index ->
      match child_at member index with
      | Some (Syntax_tree.Node id) -> Some (wrap_node member.node.tree id)
      | Some (Syntax_tree.Token _)
      | Some (Syntax_tree.Missing _)
      | None -> None

    let child_token_kind_is = fun member index kind ->
      match child_token_at member index with
      | Some token -> token_kind_is token kind
      | None -> false

    let for_each_child = fun member ~fn ->
      let rec loop index =
        if index < Node.child_count member.node then
          (
            (
              match Node.child_at member.node index with
              | Some child -> fn child
              | None -> ()
            );
            loop (index + 1)
          )
      in
      loop 0

    let for_each_child_token = fun member ~fn ->
      for_each_child member
        ~fn:(
          function
          | Syntax_tree.Token id -> fn (wrap_token member.node.tree id)
          | Syntax_tree.Node _
          | Syntax_tree.Missing _ -> ()
        )

    let for_each_child_node = fun member ~fn ->
      for_each_child member
        ~fn:(
          function
          | Syntax_tree.Node id -> fn (wrap_node member.node.tree id)
          | Syntax_tree.Token _
          | Syntax_tree.Missing _ -> ()
        )

    let name = fun member -> first_ident_or_underscore_token member.node

    let find_token = fun member kind ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else if child_token_kind_is member index kind then
          Some index
        else
          loop (index + 1)
      in
      loop 0

    let find_node = fun member ~matches ->
      let count = child_count member in
      let rec loop index =
        if index >= count then
          None
        else
          match child_node_at member index with
          | Some node when node_matches node matches -> Some node
          | _ -> loop (index + 1)
      in
      loop 0

    let first_specific_module_expr = fun node ->
      match Node.kind node with
      | Syntax_kind2.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
      | kind when is_module_expr_kind kind -> Some node
      | _ -> None

    let first_specific_module_type = fun node ->
      match Node.kind node with
      | Syntax_kind2.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
      | kind when is_module_type_kind kind -> Some node
      | _ -> None

    let module_expr = fun member ->
      match find_node member ~matches:is_module_expr_kind with
      | Some node -> first_specific_module_expr node
      | None -> None

    let module_type = fun member ->
      match find_node member ~matches:is_module_type_kind with
      | Some node -> first_specific_module_type node
      | None -> None
  end

  let for_each_member = fun decl ~fn ->
    let saw_member = ref false in
    let index = ref 0 in
    Node.for_each_child decl
      ~fn:(
        function
        | Syntax_tree.Node id ->
            let node = wrap_node decl.tree id in
            if node_kind_is node Syntax_kind2.MODULE_DECL_MEMBER then
              (
                saw_member := true;
                fn { declaration = decl; node; start_index = !index; stop_index = !index + 1 }
              );
            index := !index + 1
        | Syntax_tree.Token _
        | Syntax_tree.Missing _ -> index := !index + 1
      );
    if not !saw_member then
      fn { declaration = decl; node = decl; start_index = 0; stop_index = Node.child_count decl }

  let fold_members = fun decl init fn ->
    let acc = ref init in
    for_each_member decl ~fn:(fun member -> acc := fn !acc member);
    !acc

  let child_node_at = fun (decl: module_declaration) index ->
    match Node.child_at decl index with
    | Some (Syntax_tree.Node id) -> Some (wrap_node decl.tree id)
    | Some (Syntax_tree.Token _)
    | Some (Syntax_tree.Missing _)
    | None -> None

  let separator_index = fun node ->
    let count = Node.child_count node in
    let rec loop index =
      if index >= count then
        None
      else
        match child_token_at node index with
        | Some token when token_kind_is token Syntax_kind2.EQ || token_kind_is token Syntax_kind2.COLON -> Some index
        | _ -> loop (index + 1)
    in
    loop 0

  let body_node = fun decl ->
    let node = member_or_decl decl in
    match separator_index node with
    | None -> None
    | Some separator_index ->
        let count = Node.child_count node in
        let rec loop index =
          if index >= count then
            None
          else
            match child_node_at node index with
            | Some node when node_matches
              node
              (fun kind -> is_module_expr_kind kind || is_module_type_kind kind) -> Some node
            | _ -> loop (index + 1)
        in
        loop (separator_index + 1)

  let first_specific_module_body = fun node ->
    match Node.kind node with
    | Syntax_kind2.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
    | Syntax_kind2.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
    | kind when is_module_expr_kind kind || is_module_type_kind kind -> Some node
    | _ -> None

  let body_specific_node = fun decl ->
    match body_node decl with
    | Some node -> first_specific_module_body node
    | None -> None

  let has_child_node_kind = fun node expected ->
    let found = ref false in
    Node.for_each_child_node node
      ~fn:(fun child ->
        if node_kind_is child expected then
          found := true);
    !found

  let body = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.PATH_MODULE_EXPR
    || node_kind_is node Syntax_kind2.PATH_MODULE_TYPE -> Path
    | Some node when node_kind_is node Syntax_kind2.STRUCT_MODULE_EXPR ->
        if has_child_node_kind node Syntax_kind2.STRUCTURE_ITEM then
          Unsupported
        else
          EmptyStruct
    | Some node when node_kind_is node Syntax_kind2.SIGNATURE_MODULE_TYPE ->
        if has_child_node_kind node Syntax_kind2.SIGNATURE_ITEM then
          Sig
        else
          EmptySig
    | _ -> Unsupported

  let signature_body_node = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.SIGNATURE_MODULE_TYPE -> Some node
    | _ -> None

  let sig_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind2.SIG_KW
    | None -> None

  let end_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind2.END_KW
    | None -> None

  let for_each_body_path_ident = fun decl ~fn ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.PATH_MODULE_EXPR
    || node_kind_is node Syntax_kind2.PATH_MODULE_TYPE ->
        for_each_token_in_node node
          ~fn:(fun token ->
            if token_kind_is token Syntax_kind2.IDENT then
              fn token)
    | _ -> ()

  let for_each_sig_body_token = fun decl ~fn ->
    match signature_body_node decl with
    | None -> ()
    | Some node ->
        let inside = ref false in
        Node.for_each_child node
          ~fn:(
            function
            | Syntax_tree.Token id ->
                let token = wrap_token node.tree id in
                if token_kind_is token Syntax_kind2.SIG_KW then
                  inside := true
                else if token_kind_is token Syntax_kind2.END_KW then
                  inside := false
                else if !inside then
                  fn token
            | Syntax_tree.Node id ->
                if !inside then
                  for_each_token_in_node (wrap_node node.tree id) ~fn
            | Syntax_tree.Missing _ ->
                ()
          )
end

module ModuleTypeDeclaration = struct
  type t = module_type_declaration

  type body =
    | Abstract
    | Path
    | EmptySig
    | Sig
    | With
    | Unsupported

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.MODULE_TYPE_DECL then
      Some node
    else
      None

  let head_node = fun decl ->
    first_child_node_matching decl ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_TYPE_DECL_HEAD))

  let body_group = fun decl ->
    first_child_node_matching decl ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_TYPE_DECL_BODY))

  let name = fun decl ->
    match head_node decl with
    | Some head -> last_ident_token head
    | None -> None

  let equals_token = fun decl ->
    match body_group decl with
    | Some body -> Node.first_child_token body ~kind:Syntax_kind2.EQ
    | None -> None

  let for_each_head_token = fun decl ~fn ->
    match head_node decl with
    | Some head -> Node.for_each_child_token head ~fn
    | None -> ()

  let body_node = fun decl ->
    match body_group decl with
    | Some body -> first_child_node_matching
      body
      ~matches:(fun kind -> Syntax_kind2.(kind = MODULE_TYPE_EXPR))
    | None -> None

  let first_specific_module_type = fun node ->
    match Node.kind node with
    | Syntax_kind2.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
    | kind when is_module_type_kind kind -> Some node
    | _ -> None

  let body_specific_node = fun decl ->
    match body_node decl with
    | Some node -> first_specific_module_type node
    | None -> None

  let has_child_node_kind = fun node expected ->
    let found = ref false in
    Node.for_each_child_node node
      ~fn:(fun child ->
        if node_kind_is child expected then
          found := true);
    !found

  let body = fun decl ->
    match body_group decl, body_specific_node decl with
    | None, _ -> Abstract
    | Some _, Some node when node_kind_is node Syntax_kind2.PATH_MODULE_TYPE -> Path
    | Some _, Some node when node_kind_is node Syntax_kind2.SIGNATURE_MODULE_TYPE ->
        if has_child_node_kind node Syntax_kind2.SIGNATURE_ITEM then
          Sig
        else
          EmptySig
    | Some _, Some node when node_kind_is node Syntax_kind2.WITH_MODULE_TYPE -> With
    | Some _, _ -> Unsupported

  let signature_body_node = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.SIGNATURE_MODULE_TYPE -> Some node
    | _ -> None

  let sig_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind2.SIG_KW
    | None -> None

  let end_token = fun decl ->
    match signature_body_node decl with
    | Some node -> Node.first_child_token node ~kind:Syntax_kind2.END_KW
    | None -> None

  let for_each_body_path_ident = fun decl ~fn ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.PATH_MODULE_TYPE ->
        for_each_token_in_node node
          ~fn:(fun token ->
            if token_kind_is token Syntax_kind2.IDENT then
              fn token)
    | _ -> ()

  let for_each_sig_body_token = fun decl ~fn ->
    match signature_body_node decl with
    | None -> ()
    | Some node ->
        let inside = ref false in
        Node.for_each_child node
          ~fn:(
            function
            | Syntax_tree.Token id ->
                let token = wrap_token node.tree id in
                if token_kind_is token Syntax_kind2.SIG_KW then
                  inside := true
                else if token_kind_is token Syntax_kind2.END_KW then
                  inside := false
                else if !inside then
                  fn token
            | Syntax_tree.Node id ->
                if !inside then
                  for_each_token_in_node (wrap_node node.tree id) ~fn
            | Syntax_tree.Missing _ ->
                ()
          )

  let constrained_body_node = fun decl ->
    match body_specific_node decl with
    | Some node when node_kind_is node Syntax_kind2.WITH_MODULE_TYPE -> Some node
    | _ -> None

  let base_module_type = fun decl ->
    match constrained_body_node decl with
    | Some node -> first_child_node_matching node ~matches:is_module_type_kind
    | None -> None

  let for_each_constraint = fun decl ~fn ->
    match constrained_body_node decl with
    | None -> ()
    | Some node ->
        Node.for_each_child_node node
          ~fn:(fun child ->
            if
              node_kind_is child Syntax_kind2.WITH_TYPE_CONSTRAINT
              || node_kind_is child Syntax_kind2.WITH_MODULE_CONSTRAINT
            then
              fn child)
end

module ModuleTypeConstraint = struct
  type t = module_type_constraint

  type view =
    | Type of { path: path option; operator: token option; body: type_expr option }
    | Module of { path: path option; body: node option }
    | Unknown of node

  let cast = fun (node: node) ->
    if
      node_kind_is node Syntax_kind2.WITH_TYPE_CONSTRAINT || node_kind_is node Syntax_kind2.WITH_MODULE_CONSTRAINT
    then
      Some node
    else
      None

  let type_path = fun constraint_ ->
    match first_child_node_matching constraint_ ~matches:is_path_kind with
    | Some node -> Path.cast node
    | None -> None

  let type_body = fun constraint_ ->
    match nth_child_node_matching constraint_ 1 ~matches:is_type_expr_kind with
    | Some node -> TypeExpr.cast node
    | None -> None

  let module_path = fun constraint_ ->
    match first_child_node_matching constraint_ ~matches:is_path_kind with
    | Some node -> Path.cast node
    | None -> None

  let module_body = fun constraint_ -> nth_child_node_matching constraint_ 1 ~matches:is_module_expr_kind

  let view = fun (constraint_: t) ->
    if node_kind_is constraint_ Syntax_kind2.WITH_TYPE_CONSTRAINT then
      Type {
        path = type_path constraint_;
        operator = first_child_token_matching
          constraint_
          ~matches:(fun kind -> Syntax_kind2.(kind = EQ || kind = COLONEQ || kind = PLUS));
        body = type_body constraint_
      }
    else if node_kind_is constraint_ Syntax_kind2.WITH_MODULE_CONSTRAINT then
      Module { path = module_path constraint_; body = module_body constraint_ }
    else
      Unknown constraint_
end

module OpenDeclaration = struct
  type t = open_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.OPEN_DECL then
      Some node
    else
      None

  let path_text = Node.text

  let first_path_ident = first_ident_token

  let last_path_ident = last_ident_token

  let for_each_path_ident = fun decl ~fn ->
    Node.for_each_child_token decl
      ~fn:(fun token ->
        if token_kind_is token Syntax_kind2.IDENT then
          fn token)
end

module IncludeDeclaration = struct
  type t = include_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.INCLUDE_DECL then
      Some node
    else
      None

  let path_text = Node.text

  let first_specific_body = fun decl ->
    let rec find_body index =
      if index >= Node.child_count decl then
        None
      else
        match Node.child_at decl index with
        | Some (Syntax_tree.Node id) ->
            let node = wrap_node decl.tree id in
            if node_matches node (fun kind -> is_module_expr_kind kind || is_module_type_kind kind) then
              (
                match Node.kind node with
                | Syntax_kind2.MODULE_EXPR -> first_child_node_matching node ~matches:is_module_expr_kind
                | Syntax_kind2.MODULE_TYPE_EXPR -> first_child_node_matching node ~matches:is_module_type_kind
                | kind when is_module_expr_kind kind || is_module_type_kind kind -> Some node
                | _ -> None
              )
            else
              find_body (index + 1)
        | Some (Syntax_tree.Token _)
        | Some (Syntax_tree.Missing _)
        | None -> find_body (index + 1)
    in
    find_body 0

  let first_path_ident = fun decl ->
    match first_specific_body decl with
    | Some node -> first_ident_token node
    | None -> None

  let last_path_ident = fun decl ->
    match first_specific_body decl with
    | Some node -> last_ident_token node
    | None -> None

  let for_each_path_ident = fun decl ~fn ->
    match first_specific_body decl with
    | Some node ->
        for_each_token_in_node node
          ~fn:(fun token ->
            if token_kind_is token Syntax_kind2.IDENT then
              fn token)
    | None -> ()
end

let for_each_declaration_name_token = fun decl ~keyword ~fn ->
  let seen_keyword = ref false in
  let done_ = ref false in
  Node.for_each_child_token decl
    ~fn:(fun token ->
      if not !done_ then
        if token_kind_is token Syntax_kind2.COLON then
          done_ := true
        else if !seen_keyword then
          fn token
        else if token_kind_is token keyword then
          seen_keyword := true)

module ValueDeclaration = struct
  type t = value_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.VAL_DECL then
      Some node
    else
      None

  let name = first_ident_token

  let colon_token = fun decl -> Node.first_child_token decl ~kind:Syntax_kind2.COLON

  let type_annotation = first_type_expr_child

  let for_each_name_token = fun decl ~fn ->
    for_each_declaration_name_token decl ~keyword:Syntax_kind2.VAL_KW ~fn

  let for_each_annotation_token = fun decl ~fn ->
    for_each_token_after_child_token decl ~matches:(fun kind -> Syntax_kind2.(kind = COLON)) ~fn
end

module ExternalDeclaration = struct
  type t = external_declaration

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXTERNAL_DECL then
      Some node
    else
      None

  let name = first_ident_token

  let colon_token = fun decl -> Node.first_child_token decl ~kind:Syntax_kind2.COLON

  let type_annotation = first_type_expr_child

  let for_each_name_token = fun decl ~fn ->
    for_each_declaration_name_token decl ~keyword:Syntax_kind2.EXTERNAL_KW ~fn

  let for_each_primitive_string = fun decl ~fn ->
    Node.for_each_child_token decl
      ~fn:(fun token ->
        if token_kind_is token Syntax_kind2.STRING then
          fn token)

  let for_each_attribute_token = fun decl ~fn ->
    let seen_primitive = ref false in
    let after_primitives = ref false in
    Node.for_each_child_token decl
      ~fn:(fun token ->
        if !after_primitives then
          fn token
        else if token_kind_is token Syntax_kind2.STRING then
          seen_primitive := true
        else if !seen_primitive then
          (
            after_primitives := true;
            fn token
          ))
end

module ExceptionDeclaration = struct
  type t = exception_declaration

  type payload =
    | TypeExpr of type_expr
    | Record of record_type

  type view =
    | Bare
    | Alias of { equals_token: token option; path: path option }
    | Payload of { of_token: token option; payload: payload option }

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.EXCEPTION_DECL then
      Some node
    else
      None

  let head_node = fun decl ->
    first_child_node_matching decl ~matches:(fun kind -> Syntax_kind2.(kind = EXCEPTION_DECL_HEAD))

  let keyword_token = fun decl ->
    match head_node decl with
    | Some head -> Node.first_child_token head ~kind:Syntax_kind2.EXCEPTION_KW
    | None -> None

  let name = fun decl ->
    match head_node decl with
    | Some head -> last_ident_token head
    | None -> None

  let view = fun decl ->
    match first_child_node_matching
      decl
      ~matches:(fun kind -> Syntax_kind2.(kind = EXCEPTION_ALIAS || kind = EXCEPTION_PAYLOAD)) with
    | Some rhs when node_kind_is rhs Syntax_kind2.EXCEPTION_ALIAS ->
        let path =
          match first_child_node_matching rhs ~matches:is_path_kind with
          | Some path -> Path.cast path
          | None -> None
        in
        Alias { equals_token = Node.first_child_token rhs ~kind:Syntax_kind2.EQ; path }
    | Some rhs when node_kind_is rhs Syntax_kind2.EXCEPTION_PAYLOAD ->
        let payload =
          match first_child_node_matching rhs ~matches:is_record_type_kind with
          | Some record -> Some (Record record)
          | None -> (
              match first_child_node_matching rhs ~matches:is_type_expr_kind with
              | Some type_expr -> Some (TypeExpr type_expr)
              | None -> None
            )
        in
        Payload { of_token = Node.first_child_token rhs ~kind:Syntax_kind2.OF_KW; payload }
    | _ ->
        Bare
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

  let for_each_shell_token = Node.for_each_child_token
end

module AttributeItem = struct
  type t = attribute_item

  let cast = fun (node: node) ->
    if node_kind_is node Syntax_kind2.ATTRIBUTE_ITEM then
      Some node
    else
      None

  let for_each_shell_token = Node.for_each_child_token
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
    | TypeExtension of type_extension_declaration
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
        | Syntax_kind2.TYPE_EXTENSION_DECL -> TypeExtension node
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
    | TypeExtension of type_extension_declaration
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
        | Syntax_kind2.TYPE_EXTENSION_DECL -> TypeExtension node
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
