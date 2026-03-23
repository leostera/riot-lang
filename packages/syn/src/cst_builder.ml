open Std
open Std.Collections

type error = {
  message : string;
  syntax_kind : Syntax_kind.t;
  span : Ceibo.Span.t;
  context : string list;
}

exception Bail of error

let bail ~message ~syntax_node ~context =
  raise
    (Bail
       {
         message;
         syntax_kind = Ceibo.Red.SyntaxNode.kind syntax_node;
         span = Ceibo.Red.SyntaxNode.span syntax_node;
         context;
       })

let unsupported_parameter node =
  bail ~message:"unsupported parameter shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "parameter" ]

let unsupported_pattern node =
  bail ~message:"unsupported pattern shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "pattern" ]

let unsupported_expression node =
  bail ~message:"unsupported expression shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "expression" ]

let unsupported_module_expression node =
  bail ~message:"unsupported module expression shape during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "module_expression" ]

let unsupported_item node =
  bail ~message:"unsupported structure item during Ceibo -> CST lifting"
    ~syntax_node:node ~context:[ "item" ]

let is_trivia kind =
  let open Syntax_kind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let token syntax_tok = Cst.Token.{ syntax_token = syntax_tok }

let direct_non_trivia_nodes node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Node child -> Some child
       | Ceibo.Red.Token tok
         when is_trivia (Ceibo.Red.SyntaxToken.kind tok) ->
           None
       | Ceibo.Red.Token _ -> None)

let direct_non_trivia_tokens node =
  Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Ceibo.Red.Token tok
         when not (is_trivia (Ceibo.Red.SyntaxToken.kind tok)) ->
           Some tok
       | _ -> None)

let module_path_from_tokens ~syntax_node syntax_tokens =
  let rec skip_to_name = function
    | [] -> None
    | syntax_token :: rest ->
        if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "." then
          skip_to_name rest
        else Some (syntax_token, rest)
  in
  let rec build prefix = function
    | [] -> prefix
    | dot_token :: rest
      when String.equal (Ceibo.Red.SyntaxToken.text dot_token) "." -> (
        match skip_to_name rest with
        | Some (name_syntax_token, tail) ->
            build
              (Cst.Ident.Qualified
                {
                  syntax_node;
                  prefix;
                  dot_token = token dot_token;
                  name_token = token name_syntax_token;
                })
              tail
        | None -> prefix)
    | _unexpected :: rest ->
        build prefix rest
  in
  match skip_to_name syntax_tokens with
  | Some (name_syntax_token, rest) ->
      build
        (Cst.Ident.Ident
           { syntax_node; name_token = token name_syntax_token })
        rest
  | None ->
      bail ~message:"expected at least one path segment during Ceibo -> CST lifting"
        ~syntax_node ~context:[ "module_path" ]

let module_path_from_node node =
  let parts = direct_non_trivia_tokens node in
  module_path_from_tokens ~syntax_node:node parts

let ident_path_from_node node =
  match direct_non_trivia_tokens node with
  | first :: _ ->
      Cst.Ident.Ident
        { syntax_node = node; name_token = token first }
  | [] ->
      bail ~message:"expected identifier path segment during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_path"; "ident" ]

let module_path_like_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH | Syntax_kind.MODULE_TYPE_PATH ->
      module_path_from_node node
  | Syntax_kind.IDENT_EXPR ->
      ident_path_from_node node
  | _ ->
      module_path_from_tokens ~syntax_node:node (direct_non_trivia_tokens node)

let annotation_shell_and_payload ~annotation_kind ~sigils node =
  let direct_tokens = direct_non_trivia_tokens node in
  let has_sigil tokens =
    tokens
    |> List.exists (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           List.exists (String.equal text) sigils)
  in
  if has_sigil direct_tokens then
    (node, None)
  else
    let direct_children = direct_non_trivia_nodes node in
    let shell_node_opt =
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child = annotation_kind
             && has_sigil (direct_non_trivia_tokens child))
    in
    let payload_node_opt =
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child != annotation_kind)
    in
    match shell_node_opt with
    | Some shell_node -> (shell_node, payload_node_opt)
    | None ->
        bail ~message:"expected attribute or extension shell during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "annotation" ]

let annotation_name_from_tokens ~syntax_node ~sigils syntax_tokens =
  let name_tokens =
    syntax_tokens
    |> List.filter (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not
             (String.equal text "["
             || String.equal text "]"
             || List.exists (String.equal text) sigils))
  in
  module_path_from_tokens ~syntax_node name_tokens

let attribute_from_node node : Cst.attribute =
  let shell_node, payload_syntax_node =
    annotation_shell_and_payload ~annotation_kind:Syntax_kind.ATTRIBUTE_EXPR
      ~sigils:[ "@"; "@@"; "@@@" ] node
  in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_token =
    shell_tokens
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           String.equal text "@" || String.equal text "@@" || String.equal text "@@@")
  in
  match sigil_syntax_token with
  | Some sigil_syntax_token ->
      {
        Cst.syntax_node = node;
        sigil_token = token sigil_syntax_token;
        name =
          annotation_name_from_tokens ~syntax_node:shell_node
            ~sigils:[ "@"; "@@"; "@@@" ] shell_tokens;
        payload_syntax_node;
      }
  | None ->
      bail ~message:"expected attribute sigil during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "attribute" ]

let extension_from_node node : Cst.extension =
  let shell_node, payload_syntax_node =
    annotation_shell_and_payload ~annotation_kind:Syntax_kind.EXTENSION_EXPR
      ~sigils:[ "%"; "%%"; "%%%" ] node
  in
  let shell_tokens = direct_non_trivia_tokens shell_node in
  let sigil_syntax_token =
    shell_tokens
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           String.equal text "%" || String.equal text "%%" || String.equal text "%%%")
  in
  match sigil_syntax_token with
  | Some sigil_syntax_token ->
      {
        Cst.syntax_node = node;
        sigil_token = token sigil_syntax_token;
        name =
          annotation_name_from_tokens ~syntax_node:shell_node
            ~sigils:[ "%"; "%%"; "%%%" ] shell_tokens;
        payload_syntax_node;
      }
  | None ->
      bail ~message:"expected extension sigil during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "extension" ]

let attributes_from_node node =
  direct_non_trivia_nodes node
  |> List.filter (fun child ->
         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.ATTRIBUTE_EXPR)
  |> List.map attribute_from_node

let non_paren_tokens node =
  direct_non_trivia_tokens node
  |> List.filter (fun syntax_token ->
         let text = Ceibo.Red.SyntaxToken.text syntax_token in
         not (String.equal text "(" || String.equal text ")"))

let is_type_syntax_kind = function
  | Syntax_kind.TYPE_VAR
  | Syntax_kind.TYPE_CONSTR
  | Syntax_kind.TYPE_RECORD
  | Syntax_kind.TYPE_TUPLE
  | Syntax_kind.TYPE_ALIAS
  | Syntax_kind.TYPE_ARROW
  | Syntax_kind.TYPE_PAREN
  | Syntax_kind.TYPE_POLY_VARIANT
  | Syntax_kind.POLY_TYPE
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE
  | Syntax_kind.OBJECT_TYPE
  | Syntax_kind.LOCAL_OPEN_TYPE
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | _ -> false

let rec can_lift_core_type_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_core_type_node
  | kind ->
      is_type_syntax_kind kind

let rec can_lift_module_type_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH
  | Syntax_kind.MODULE_TYPE_OF
  | Syntax_kind.MODULE_TYPE_EXPR
  | Syntax_kind.FUNCTOR_TYPE
  | Syntax_kind.EXTENSION_EXPR ->
      true
  | Syntax_kind.PAREN_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_type_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          String.equal (Ceibo.Red.SyntaxToken.text first) "sig"
      | [] -> false)
  | _ ->
      false

let rec can_lift_module_expression_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.STRUCT_EXPR
  | Syntax_kind.MODULE_APPLICATION
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR ->
      true
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          String.equal (Ceibo.Red.SyntaxToken.text first) "functor"
      | [] -> false)
  | Syntax_kind.PAREN_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.ATTRIBUTE_EXPR ->
      direct_non_trivia_nodes node |> List.exists can_lift_module_expression_node
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          not (String.equal (Ceibo.Red.SyntaxToken.text first) "sig")
      | [] -> false)
  | _ ->
      false

let is_pattern_syntax_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.EFFECT_PATTERN
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN ->
      true
  | _ -> false

let is_expression_syntax_kind = function
  | Syntax_kind.IDENT_EXPR
  | Syntax_kind.MODULE_PATH
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.ATTRIBUTE_EXPR
  | Syntax_kind.EXTENSION_EXPR
  | Syntax_kind.OBJECT_EXPR
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.METHOD_CALL_EXPR
  | Syntax_kind.NEW_EXPR
  | Syntax_kind.FIELD_ACCESS_EXPR
  | Syntax_kind.ARRAY_INDEX_EXPR
  | Syntax_kind.STRING_INDEX_EXPR
  | Syntax_kind.ASSIGN_EXPR
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.ASSERT_EXPR
  | Syntax_kind.LAZY_EXPR
  | Syntax_kind.WHILE_EXPR
  | Syntax_kind.FOR_EXPR
  | Syntax_kind.APPLY_EXPR
  | Syntax_kind.POLY_VARIANT_EXPR
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR
  | Syntax_kind.LET_MODULE_EXPR
  | Syntax_kind.LET_EXPR
  | Syntax_kind.LET_REC_EXPR
  | Syntax_kind.TYPED_EXPR
  | Syntax_kind.COERCE_EXPR
  | Syntax_kind.PREFIX_EXPR
  | Syntax_kind.INFIX_EXPR
  | Syntax_kind.SEQUENCE_EXPR
  | Syntax_kind.TUPLE_EXPR
  | Syntax_kind.LIST_EXPR
  | Syntax_kind.ARRAY_EXPR
  | Syntax_kind.RECORD_EXPR
  | Syntax_kind.RECORD_UPDATE_EXPR
  | Syntax_kind.UNREACHABLE_EXPR
  | Syntax_kind.OBJECT_UPDATE_EXPR
  | Syntax_kind.LOCAL_OPEN_EXPR
  | Syntax_kind.FUN_EXPR
  | Syntax_kind.FUNCTION_EXPR
  | Syntax_kind.MATCH_EXPR
  | Syntax_kind.TRY_EXPR
  | Syntax_kind.IF_EXPR
  | Syntax_kind.PAREN_EXPR ->
      true
  | _ -> false

let is_parameter_like_kind = function
  | Syntax_kind.IDENT_PATTERN
  | Syntax_kind.WILDCARD_PATTERN
  | Syntax_kind.LITERAL_PATTERN
  | Syntax_kind.STRING_LITERAL
  | Syntax_kind.INT_LITERAL
  | Syntax_kind.FLOAT_LITERAL
  | Syntax_kind.CHAR_LITERAL
  | Syntax_kind.BOOL_LITERAL
  | Syntax_kind.UNIT_LITERAL
  | Syntax_kind.CONSTRUCTOR_PATTERN
  | Syntax_kind.TUPLE_PATTERN
  | Syntax_kind.LIST_PATTERN
  | Syntax_kind.ARRAY_PATTERN
  | Syntax_kind.CONS_PATTERN
  | Syntax_kind.RECORD_PATTERN
  | Syntax_kind.OR_PATTERN
  | Syntax_kind.AS_PATTERN
  | Syntax_kind.RANGE_PATTERN
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.LAZY_PATTERN
  | Syntax_kind.EXCEPTION_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.POLY_VARIANT_PATTERN
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN
  | Syntax_kind.LOCAL_OPEN_PATTERN
  | Syntax_kind.OPERATOR_PATTERN
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN
  | Syntax_kind.LABELED_PARAM
  | Syntax_kind.OPTIONAL_PARAM
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      true
  | _ -> false

let name_token_from_ident_pattern node =
  match direct_non_trivia_tokens node with
  | first :: _ -> Some (token first)
  | [] -> None

let is_identifier_like_text text =
  let is_alpha_or_underscore ch =
    (ch >= 'a' && ch <= 'z')
    || (ch >= 'A' && ch <= 'Z')
    || ch = '_'
  in
  let len = String.length text in
  if len = 0 then
    false
  else
    let ch = String.get text 0 in
    is_alpha_or_underscore ch
    || if ch = '#' && len > 1 then
      let next = String.get text 1 in
      is_alpha_or_underscore next
    else if ch = '\\' then
      if len > 2 && String.get text 1 = '#' then
        let next = String.get text 2 in
        is_alpha_or_underscore next
      else
        false
    else
      false

let rec simple_pattern_name_token node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN ->
      name_token_from_ident_pattern node
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: _ -> simple_pattern_name_token first_child
      | [] -> None)
  | Syntax_kind.TYPED_PATTERN
  | Syntax_kind.PAREN_PATTERN
  | Syntax_kind.LAZY_PATTERN ->
      (match direct_non_trivia_nodes node |> List.find_opt (fun _ -> true) with
      | Some child -> simple_pattern_name_token child
      | None -> None)
  | Syntax_kind.LOCAL_OPEN_PATTERN -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt (fun child ->
               is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
      with
      | Some child -> simple_pattern_name_token child
      | None -> None)
  | Syntax_kind.AS_PATTERN ->
      (match
         direct_non_trivia_nodes node
         |> List.rev
         |> List.find_opt (fun child ->
                Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_PATTERN)
      with
      | Some child -> name_token_from_ident_pattern child
      | None -> None)
  | _ -> None

let is_attribute_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.ATTRIBUTE_EXPR

let is_let_binding_node node =
  let kind = Ceibo.Red.SyntaxNode.kind node in
  kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING

let split_at_first_and_binding nodes =
  let rec loop acc = function
    | child :: rest when is_let_binding_node child ->
        (List.rev acc, child :: rest)
    | child :: rest ->
        loop (child :: acc) rest
    | [] -> (List.rev acc, [])
  in
  loop [] nodes

let let_expression_parts ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
         (direct_non_trivia_tokens node)
  in
  let binding_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | exception_decl :: rest
    when Ceibo.Red.SyntaxNode.kind exception_decl = Syntax_kind.EXCEPTION_DECL ->
      (match List.rev rest with
      | body_node :: _ ->
          Some (`Exception (exception_decl, body_node))
      | [] -> None)
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | body_node :: rev_prefix ->
          let prefix = List.rev rev_prefix in
          let binding_prefix, and_binding_nodes =
            split_at_first_and_binding prefix
          in
          (match List.rev binding_prefix with
          | bound_value_node :: rev_param_nodes ->
              Some
                (`Value
                  ( is_recursive_binding,
                    binding_pattern_node,
                    List.rev rev_param_nodes,
                    bound_value_node,
                    and_binding_nodes,
                    body_node ))
          | [] -> None)
      | [] -> None)
  | [] -> None

let is_binding_operator_expression_node node =
  let non_trivia_children =
    Ceibo.Red.SyntaxNode.children node
    |> Array.to_list
    |> List.filter (function
         | Ceibo.Red.Token syntax_token
           when is_trivia (Ceibo.Red.SyntaxToken.kind syntax_token) ->
             false
         | _ -> true)
  in
  match Ceibo.Red.SyntaxNode.kind node, non_trivia_children with
  | Syntax_kind.LET_EXPR,
    Ceibo.Red.Token let_kw
    :: Ceibo.Red.Token operator_syntax_token
    :: Ceibo.Red.Node pattern_node
    :: Ceibo.Red.Token equals_syntax_token
    :: _
    when String.equal (Ceibo.Red.SyntaxToken.text let_kw) "let"
         && is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind pattern_node)
         && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) "="
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "rec")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "open")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "module")
         && not (String.equal (Ceibo.Red.SyntaxToken.text operator_syntax_token) "exception") ->
      true
  | _ ->
      false

let binding_operator_pairs_from_tokens node =
  let rec loop acc = function
    | keyword_syntax_token :: operator_syntax_token :: equals_syntax_token :: rest
      when (String.equal (Ceibo.Red.SyntaxToken.text keyword_syntax_token) "let"
            || String.equal (Ceibo.Red.SyntaxToken.text keyword_syntax_token) "and")
           && String.equal (Ceibo.Red.SyntaxToken.text equals_syntax_token) "=" ->
        loop
          ((token keyword_syntax_token, token operator_syntax_token) :: acc)
          rest
    | in_syntax_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text in_syntax_token) "in" ->
        List.rev acc
    | [] ->
        List.rev acc
    | _ ->
        bail
          ~message:
            "expected binding-operator token sequence during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "expression"; "let_operator"; "tokens" ]
  in
  loop [] (direct_non_trivia_tokens node)

let first_ident_token_in_subtree node =
  let rec go_node node =
    match
      direct_non_trivia_tokens node
      |> List.find_opt (fun tok ->
             is_identifier_like_text (Ceibo.Red.SyntaxToken.text tok))
    with
    | Some tok -> Some (token tok)
    | None -> direct_non_trivia_nodes node |> List.find_map go_node
  in
  go_node node

let quoted_type_binder_from_node node =
  match first_ident_token_in_subtree node with
  | Some name_token ->
      Cst.TypeBinder.Quoted { syntax_node = node; name_token }
  | None ->
      bail ~message:"expected quantified type binder name during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_binder.quoted" ]

let bare_type_binders_from_tokens syntax_tokens =
  let rec collect started acc = function
    | [] ->
        List.rev acc
    | syntax_token :: rest ->
        let text = Ceibo.Red.SyntaxToken.text syntax_token in
        if started then
          if String.equal text "." then
            List.rev acc
          else if is_identifier_like_text text then
            collect true
              (Cst.TypeBinder.Bare { name_token = token syntax_token } :: acc)
              rest
          else
            collect true acc rest
        else if String.equal text "type" then
          collect true acc rest
        else
          collect false acc rest
  in
  collect false [] syntax_tokens

let locally_abstract_type_parameter_from_node node =
  let binders = bare_type_binders_from_tokens (direct_non_trivia_tokens node) in
  if List.length binders = 0 then
    bail
      ~message:
        "expected locally abstract type binders during Ceibo -> CST lifting"
      ~syntax_node:node ~context:[ "parameter.locally_abstract" ]
  else
    { Cst.syntax_node = node; binders }

let parameter_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_PARAM -> (
      match first_ident_token_in_subtree node with
      | Some label_name_token ->
          Cst.Parameter.Labeled
            {
              syntax_node = node;
              label_token = label_name_token;
              binding_name_token = None;
            }
      | None -> unsupported_parameter node)
  | Syntax_kind.OPTIONAL_PARAM -> (
      match first_ident_token_in_subtree node with
      | Some label_name_token ->
          Cst.Parameter.Optional
            {
              syntax_node = node;
              label_token = label_name_token;
              binding_name_token = None;
              has_default = false;
            }
      | None -> unsupported_parameter node)
  | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> (
      match direct_non_trivia_nodes node |> List.find_map first_ident_token_in_subtree with
      | Some label_name_token ->
          Cst.Parameter.Optional
            {
              syntax_node = node;
              label_token = label_name_token;
              binding_name_token = None;
              has_default = true;
            }
      | None -> unsupported_parameter node)
  | Syntax_kind.LOCALLY_ABSTRACT_TYPE_PARAM ->
      Cst.Parameter.LocallyAbstract
        (locally_abstract_type_parameter_from_node node)
  | _ ->
      Cst.Parameter.Positional
        {
          syntax_node = node;
          name_token = simple_pattern_name_token node;
        }

let rec take_tokens_until_equals acc = function
  | [] -> List.rev acc
  | syntax_token :: rest ->
      if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=" then
        List.rev acc
      else
        take_tokens_until_equals (syntax_token :: acc) rest

let operator_tokens_from_node node =
  direct_non_trivia_tokens node
  |> List.filter (fun syntax_token ->
         let text = Ceibo.Red.SyntaxToken.text syntax_token in
         not
           (String.equal text "("
           || String.equal text ")"
           || String.equal text " "))
  |> List.map token

let rec module_type_constraint_from_node node =
  let type_name =
    match direct_non_trivia_tokens node with
    | _type_kw :: type_name :: _ ->
        token type_name
    | _ ->
        bail ~message:"expected type name in module type constraint during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.constraint" ]
  in
  let replacement_type =
    match direct_non_trivia_nodes node |> List.find_opt can_lift_core_type_node with
    | Some type_node ->
        core_type_from_node type_node
    | None ->
        bail
          ~message:
            "expected replacement type in module type constraint during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.constraint" ]
  in
  let is_destructive =
    direct_non_trivia_tokens node
    |> List.exists (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) ":=")
  in
  Cst.ModuleTypeConstraint.{ syntax_node = node; type_name; replacement_type; is_destructive }

and functor_parameter_from_node node =
  let name_token =
    match direct_non_trivia_tokens node with
    | _lparen :: name_token :: _ ->
        token name_token
    | _ ->
        bail ~message:"expected functor parameter name during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.functor.parameter" ]
  in
  let module_type =
    match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node with
    | Some module_type_node ->
        module_type_from_node module_type_node
    | None ->
        bail
          ~message:
            "expected functor parameter module type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "module_type.functor.parameter" ]
  in
  Cst.FunctorParameter.{ syntax_node = node; name_token; module_type }

and module_type_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.MODULE_TYPE_PATH ->
      Cst.ModuleType.Path (module_path_from_node node)
  | Syntax_kind.MODULE_TYPE_OF -> (
      match direct_non_trivia_nodes node with
      | module_path_node :: _ ->
          Cst.ModuleType.TypeOf
            {
              syntax_node = node;
              module_path = module_path_like_from_node module_path_node;
            }
      | [] ->
          bail
            ~message:
              "expected module path in module type of expression during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.type_of" ])
  | Syntax_kind.MODULE_TYPE_EXPR -> (
      match direct_non_trivia_nodes node with
      | base_node :: constraint_nodes ->
          Cst.ModuleType.With
            {
              syntax_node = node;
              base = module_type_from_node base_node;
              constraints =
                constraint_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
                |> List.map module_type_constraint_from_node;
            }
      | [] ->
          bail
            ~message:
              "expected base module type in constrained module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.with" ])
  | Syntax_kind.FUNCTOR_TYPE -> (
      match List.rev (direct_non_trivia_nodes node) with
      | result_node :: rev_parameter_nodes ->
          Cst.ModuleType.Functor
            {
              syntax_node = node;
              parameters =
                List.rev rev_parameter_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
                |> List.map functor_parameter_from_node;
              result = module_type_from_node result_node;
            }
      | [] ->
          bail
            ~message:
              "expected functor parameters and result module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.functor" ])
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_type_node with
      | Some inner_node ->
          Cst.ModuleType.Parenthesized
            { syntax_node = node; inner = module_type_from_node inner_node }
      | None ->
          bail
            ~message:
              "expected inner module type in parenthesized module type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.parenthesized" ])
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_module_type_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ModuleType.Attribute
                {
                  syntax_node = node;
                  module_type = module_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              bail
                ~message:
                  "expected attributed module type payload during Ceibo -> CST lifting"
                ~syntax_node:node ~context:[ "module_type.attribute" ])
      | [] ->
          bail
            ~message:
              "expected attributed module type contents during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type.attribute" ])
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.ModuleType.Extension (extension_from_node node)
  | Syntax_kind.IDENT_EXPR -> (
      match direct_non_trivia_tokens node with
      | sig_kw :: _ when String.equal (Ceibo.Red.SyntaxToken.text sig_kw) "sig" ->
          Cst.ModuleType.Signature
            { syntax_node = node; signature_syntax_node = node }
      | _ ->
          bail
            ~message:"unsupported module type identifier shape during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "module_type" ])
  | _ ->
      bail ~message:"unsupported module type shape during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_type" ]

and module_type_from_first_class_module_type_node node =
  match direct_non_trivia_nodes node with
  | base_node :: constraint_nodes ->
      let base = module_type_from_node base_node in
      let constraints =
        constraint_nodes
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_CONSTRAINT)
        |> List.map module_type_constraint_from_node
      in
      if List.length constraints = 0 then
        base
      else
        Cst.ModuleType.With { syntax_node = node; base; constraints }
  | [] ->
      bail
        ~message:
          "expected module type inside first-class module type during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "module_type.first_class_module" ]

and core_type_from_node node =
  let child_type_nodes node =
    direct_non_trivia_nodes node
    |> List.filter can_lift_core_type_node
  in
  let type_binders_from_poly_type_node node =
    let quoted_binders =
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
      |> List.map quoted_type_binder_from_node
    in
    if List.length quoted_binders > 0 then
      quoted_binders
    else
      bare_type_binders_from_tokens (direct_non_trivia_tokens node)
  in
  let rec type_path_from_node node =
    match
      direct_non_trivia_nodes node
      |> List.find_opt (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.MODULE_PATH
             || kind = Syntax_kind.MODULE_TYPE_PATH
             || kind = Syntax_kind.IDENT_EXPR)
    with
    | Some path_node -> module_path_from_node path_node
    | None ->
        let path_tokens =
          direct_non_trivia_tokens node
          |> List.filter (fun syntax_token ->
                 let text = Ceibo.Red.SyntaxToken.text syntax_token in
                 is_identifier_like_text text || String.equal text ".")
        in
        if List.length path_tokens = 0 then
          bail ~message:"expected type constructor path during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.constr" ]
        else
          module_path_from_tokens ~syntax_node:node path_tokens
  and class_type_path_from_node node =
    let rec after_hash = function
      | [] -> None
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#" then
            Some (syntax_token, rest)
          else
            after_hash rest
    in
    match after_hash (direct_non_trivia_tokens node) with
    | Some (hash_syntax_token, class_path_tokens) ->
        if List.length class_path_tokens > 0 then
          ( token hash_syntax_token,
            module_path_from_tokens ~syntax_node:node class_path_tokens )
        else
          bail ~message:"expected class type path after # during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.class" ]
    | None ->
        bail ~message:"expected class type marker during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.class" ]
  and local_open_core_type_from_node node =
    match direct_non_trivia_nodes node with
    | module_path_node :: type_node :: _ ->
        {
          Cst.syntax_node = node;
          module_path = module_path_like_from_node module_path_node;
          type_ = core_type_from_node type_node;
        }
    | _ ->
        bail ~message:"expected local-open module path and body during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.local_open" ]
  and object_type_field_from_node node =
    match
      first_ident_token_in_subtree node,
      direct_non_trivia_nodes node
      |> List.find_opt can_lift_core_type_node
    with
    | Some field_name, Some field_type_node ->
        {
          Cst.syntax_node = node;
          field_name;
          field_type = core_type_from_node field_type_node;
        }
    | _ ->
        bail ~message:"expected object type field name and type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.object_field" ]
  and record_type_field_from_node node =
    let field_name =
      match direct_non_trivia_tokens node with
      | mutable_kw :: name_token :: _
        when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" ->
          Some (token name_token)
      | name_token :: _ -> Some (token name_token)
      | [] -> None
    in
    let mutable_field =
      match direct_non_trivia_tokens node with
      | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "mutable"
      | [] -> false
    in
    match
      field_name,
      direct_non_trivia_nodes node
      |> List.find_opt can_lift_core_type_node
    with
    | Some field_name, Some field_type_node ->
        {
          Cst.syntax_node = node;
          field_name;
          field_type = core_type_from_node field_type_node;
          is_mutable = mutable_field;
        }
    | _ ->
        bail ~message:"expected record type field name and type during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.record_field" ]
  and poly_variant_tag_from_node node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_name :: _ ->
        {
          Cst.syntax_node = node;
          tag_name = token tag_name;
          payload_type =
            (direct_non_trivia_nodes node
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
    | tag_name :: _ ->
        {
          Cst.syntax_node = node;
          tag_name = token tag_name;
          payload_type =
            (direct_non_trivia_nodes node
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
    | [] ->
        bail ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.poly_variant_tag" ]
  and poly_variant_bound_from_node node =
    match direct_non_trivia_tokens node with
    | _open_bracket :: marker_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "<" ->
        Cst.PolyVariantBound.UpperBound { marker_token = token marker_token }
    | _open_bracket :: marker_token :: _
      when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ">" ->
        Cst.PolyVariantBound.LowerBound { marker_token = token marker_token }
    | _ ->
        Cst.PolyVariantBound.Exact
  and row_field_from_node node =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.POLY_VARIANT_TAG ->
        Cst.RowField.Tag (poly_variant_tag_from_node node)
    | _ when can_lift_core_type_node node ->
        Cst.RowField.Inherit
          { syntax_node = node; type_ = core_type_from_node node }
    | _ ->
        bail ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
          ~syntax_node:node ~context:[ "core_type.poly_variant.row_field" ]
  and poly_variant_from_node node =
    {
      Cst.syntax_node = node;
      kind = poly_variant_bound_from_node node;
      fields =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               let kind = Ceibo.Red.SyntaxNode.kind child in
               kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child)
        |> List.map row_field_from_node;
    }
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_VAR -> (
      match List.rev (direct_non_trivia_tokens node) with
      | syntax_token :: _ ->
          let lifted = token syntax_token in
          if String.equal (Cst.Token.text lifted) "_" then
            Cst.CoreType.Wildcard { syntax_node = node; wildcard_token = lifted }
          else
            Cst.CoreType.Var { syntax_node = node; name_token = lifted }
      | [] ->
          bail ~message:"expected type variable token during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.var" ])
  | Syntax_kind.TYPE_CONSTR ->
      let child_types = child_type_nodes node in
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let opens_with_lparen =
        match non_trivia_tokens with
        | first :: _ -> String.equal (Ceibo.Red.SyntaxToken.text first) "("
        | [] -> false
      in
      let closes_with_rparen =
        match List.rev non_trivia_tokens with
        | last :: _ -> String.equal (Ceibo.Red.SyntaxToken.text last) ")"
        | [] -> false
      in
      if opens_with_lparen && closes_with_rparen && List.length child_types = 1 then
        match child_types with
        | [ inner_type ] ->
            Cst.CoreType.Parenthesized
              { syntax_node = node; inner = core_type_from_node inner_type }
        | _ ->
            bail ~message:"expected a single inner type inside parenthesized type"
              ~syntax_node:node ~context:[ "core_type.parenthesized" ]
      else if
        non_trivia_tokens
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "#")
      then
        let hash_token, class_path = class_type_path_from_node node in
        Cst.CoreType.Class
          {
            syntax_node = node;
            hash_token;
            class_path;
            arguments = child_types |> List.map core_type_from_node;
          }
      else
        Cst.CoreType.Constr
          {
            syntax_node = node;
            constructor_path = type_path_from_node node;
            arguments = child_types |> List.map core_type_from_node;
          }
  | Syntax_kind.TYPE_ALIAS -> (
      match child_type_nodes node with
      | type_node :: alias_node :: _ -> (
          match List.rev (direct_non_trivia_tokens alias_node) with
          | alias_token :: _ ->
              Cst.CoreType.Alias
                {
                  syntax_node = node;
                  type_ = core_type_from_node type_node;
                  name_token = token alias_token;
                }
          | [] ->
              bail ~message:"expected alias name token during Ceibo -> CST lifting"
                ~syntax_node:alias_node ~context:[ "core_type.alias" ])
      | _ ->
          bail ~message:"expected aliased type and alias variable during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.alias" ])
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_core_type_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.CoreType.Attribute
                {
                  syntax_node = node;
                  type_ = core_type_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              bail ~message:"expected attribute payload during Ceibo -> CST lifting"
                ~syntax_node:node ~context:[ "core_type.attribute" ])
      | [] ->
          bail ~message:"expected attributed type payload during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.attribute" ])
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.CoreType.Extension (extension_from_node node)
  | Syntax_kind.POLY_TYPE -> (
      let binders = type_binders_from_poly_type_node node in
      let body_node =
        direct_non_trivia_nodes node
        |> List.rev
        |> List.find_opt (fun child ->
               can_lift_core_type_node child
               && Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_VAR)
      in
      match binders, body_node with
      | _ :: _, Some body_node ->
          Cst.CoreType.Poly
            {
              syntax_node = node;
              binders;
              body = core_type_from_node body_node;
            }
      | [], _ ->
          bail
            ~message:
              "expected quantified type binders during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.poly" ]
      | _, None ->
          bail
            ~message:"expected quantified type body during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.poly" ])
  | Syntax_kind.TYPE_ARROW -> (
      match child_type_nodes node with
      | parameter_node :: result_node :: _ ->
          Cst.CoreType.Arrow
            {
              syntax_node = node;
              parameter_type = core_type_from_node parameter_node;
              result_type = core_type_from_node result_node;
            }
      | _ ->
          bail ~message:"expected arrow parameter and result types during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.arrow" ])
  | Syntax_kind.TYPE_TUPLE ->
      Cst.CoreType.Tuple
        {
          syntax_node = node;
          elements = child_type_nodes node |> List.map core_type_from_node;
        }
  | Syntax_kind.TYPE_PAREN -> (
      match child_type_nodes node with
      | inner_node :: _ ->
          Cst.CoreType.Parenthesized
            { syntax_node = node; inner = core_type_from_node inner_node }
      | [] ->
          bail ~message:"expected inner type inside parenthesized type during Ceibo -> CST lifting"
            ~syntax_node:node ~context:[ "core_type.parenthesized" ])
  | Syntax_kind.LOCAL_OPEN_TYPE ->
      Cst.CoreType.LocalOpen (local_open_core_type_from_node node)
  | Syntax_kind.TYPE_POLY_VARIANT ->
      Cst.CoreType.PolyVariant (poly_variant_from_node node)
  | Syntax_kind.TYPE_RECORD ->
      Cst.CoreType.Record
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
            |> List.map record_type_field_from_node;
        }
  | Syntax_kind.FIRST_CLASS_MODULE_TYPE -> (
      Cst.CoreType.FirstClassModule
        {
          syntax_node = node;
          module_type = module_type_from_first_class_module_type_node node;
        })
  | Syntax_kind.OBJECT_TYPE ->
      Cst.CoreType.Object
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.OBJECT_TYPE_FIELD)
            |> List.map object_type_field_from_node;
        }
  | _ ->
      bail ~message:"unsupported core type shape during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "core_type" ]

let rec pattern_from_node node =
  let pattern_children node = direct_non_trivia_nodes node |> List.map pattern_from_node in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_PATTERN -> (
      match direct_non_trivia_tokens node with
      | first :: _ ->
          Cst.Pattern.Identifier { syntax_node = node; name_token = token first }
      | [] -> unsupported_pattern node)
  | Syntax_kind.WILDCARD_PATTERN ->
      Cst.Pattern.Wildcard { syntax_node = node }
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | pattern_node :: rest -> (
          match List.find_opt is_attribute_node rest with
          | Some attribute_node ->
          Cst.Pattern.Attribute
                {
                  syntax_node = node;
                  pattern = pattern_from_node pattern_node;
                  attribute = attribute_from_node attribute_node;
                }
          | None -> unsupported_pattern node)
      | [] -> unsupported_pattern node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.Pattern.Extension (extension_from_node node)
  | Syntax_kind.LAZY_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Lazy
            {
              syntax_node = node;
              pattern = pattern_from_node inner_node;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.EXCEPTION_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Exception
            {
              syntax_node = node;
              pattern = pattern_from_node inner_node;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.RANGE_PATTERN -> (
      match direct_non_trivia_tokens node with
      | lower_syntax_token :: _range_syntax_token :: upper_syntax_token :: _ ->
          Cst.Pattern.Range
            {
              syntax_node = node;
              lower_token = token lower_syntax_token;
              upper_token = token upper_syntax_token;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.OPERATOR_PATTERN ->
      Cst.Pattern.Operator
        { syntax_node = node; operator_tokens = operator_tokens_from_node node }
  | Syntax_kind.FIRST_CLASS_MODULE_PATTERN -> (
      match direct_non_trivia_tokens node with
      | _lparen :: _module_kw :: name_syntax_token :: _ ->
          Cst.Pattern.FirstClassModule
            {
              syntax_node = node;
              name_token = token name_syntax_token;
              module_type =
                (direct_non_trivia_nodes node
                 |> List.find_opt (fun child ->
                        can_lift_module_type_node child)
                 |> Option.map module_type_from_node);
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_pattern node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_pattern node)
  | Syntax_kind.FLOAT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Float
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_pattern node)
  | Syntax_kind.CHAR_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Char
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_pattern node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Pattern.Literal
            (Cst.PatternLiteral.Bool
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_pattern node)
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Pattern.Literal (Cst.PatternLiteral.Unit { syntax_node = node })
  | Syntax_kind.POLY_VARIANT_PATTERN -> (
      match poly_variant_tag_token node with
      | Some tag_token ->
          Cst.Pattern.PolyVariant
            {
              syntax_node = node;
              tag_token;
              payload =
                (direct_non_trivia_nodes node
                |> List.find_opt (fun child ->
                       is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                |> Option.map pattern_from_node);
            }
      | None -> unsupported_pattern node)
  | Syntax_kind.POLY_VARIANT_TYPE_PATTERN ->
      Cst.Pattern.PolyVariantInherit
        { syntax_node = node; type_path = module_path_like_from_node node }
  | Syntax_kind.CONSTRUCTOR_PATTERN ->
      Cst.Pattern.Constructor
        {
          syntax_node = node;
          constructor_path = module_path_from_node node;
          arguments = pattern_children node;
        }
  | Syntax_kind.TUPLE_PATTERN ->
      Cst.Pattern.Tuple { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.LIST_PATTERN ->
      Cst.Pattern.List { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.ARRAY_PATTERN ->
      Cst.Pattern.Array { syntax_node = node; elements = pattern_children node }
  | Syntax_kind.RECORD_PATTERN ->
      Cst.Pattern.Record
        {
          syntax_node = node;
          fields =
            direct_non_trivia_nodes node
            |> List.filter (fun child ->
                   Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD_PATTERN)
            |> List.filter_map record_pattern_field_from_node;
        }
  | Syntax_kind.CONS_PATTERN -> (
      match direct_non_trivia_nodes node with
      | head_node :: tail_node :: _ ->
          Cst.Pattern.Cons
            {
              syntax_node = node;
              head = pattern_from_node head_node;
              tail = pattern_from_node tail_node;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.OR_PATTERN ->
      Cst.Pattern.Or
        { syntax_node = node; alternatives = pattern_children node }
  | Syntax_kind.AS_PATTERN -> (
      match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
      | pattern_node :: _, name_syntax_token :: _ ->
          Cst.Pattern.Alias
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              name_token = token name_syntax_token;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.TYPED_PATTERN -> (
      match direct_non_trivia_nodes node with
      | pattern_node :: type_node :: _ ->
          Cst.Pattern.Typed
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              type_ = core_type_from_node type_node;
            }
      | _ -> unsupported_pattern node)
  | Syntax_kind.EFFECT_PATTERN -> (
      match effect_pattern_from_node node with
      | Some pattern -> Cst.Pattern.Effect pattern
      | None -> unsupported_pattern node)
  | Syntax_kind.LOCAL_OPEN_PATTERN -> (
      match local_open_pattern_from_node node with
      | Some pattern -> Cst.Pattern.LocalOpen pattern
      | None -> unsupported_pattern node)
  | Syntax_kind.PAREN_PATTERN -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Pattern.Parenthesized
            { syntax_node = node; inner = pattern_from_node inner_node }
      | [] -> unsupported_pattern node)
  | _ -> unsupported_pattern node

and record_pattern_field_from_node node =
  let lifted_field_path =
    let tokens =
      direct_non_trivia_tokens node |> take_tokens_until_equals []
    in
    module_path_from_tokens ~syntax_node:node tokens
  in
  match Cst.Ident.segments lifted_field_path with
  | [] -> None
  | _ ->
      Some
        {
          syntax_node = node;
          field_path = lifted_field_path;
          pattern =
            (direct_non_trivia_nodes node
            |> List.find_opt (fun child ->
                   is_pattern_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
            |> Option.map pattern_from_node);
        }

and local_open_pattern_from_node node =
  match direct_non_trivia_nodes node with
  | module_path_node :: pattern_node :: _ ->
      Some
        {
          syntax_node = node;
          module_path = module_path_like_from_node module_path_node;
          pattern = pattern_from_node pattern_node;
        }
  | _ -> None

and effect_pattern_from_node node =
  match direct_non_trivia_nodes node with
  | effect_node :: continuation_node :: _ ->
      Some
        {
          syntax_node = node;
          effect_pattern = pattern_from_node effect_node;
          continuation = pattern_from_node continuation_node;
        }
  | _ -> None

let rec apply_argument_from_node node =
  let first_nontrivia_expression_child node =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
    |> Option.map expression_from_node
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.LABELED_ARG -> (
      match direct_non_trivia_tokens node with
      | _sigil :: label_syntax_token :: _ ->
          Cst.Labeled
            {
              syntax_node = node;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.OPTIONAL_ARG -> (
      match direct_non_trivia_tokens node with
      | _sigil :: label_syntax_token :: _ ->
          Cst.Optional
            {
              syntax_node = node;
              label_token = token label_syntax_token;
              value = first_nontrivia_expression_child node;
            }
      | _ -> unsupported_expression node)
  | _ -> Cst.Positional (expression_from_node node)

(* Let-binding annotations do not have dedicated expression nodes, so the CST
   reconstructs typed/polymorphic wrappers around the bound value. *)
and expression_with_type_annotation ~syntax_node ~expression type_node =
  let type_ = core_type_from_node type_node in
  match type_ with
  | Cst.CoreType.Poly { binders; _ }
    when List.exists Cst.TypeBinder.is_quoted binders ->
      Cst.Expression.Polymorphic { syntax_node; expression; type_ }
  | _ ->
      Cst.Expression.Typed { syntax_node; expression; type_ }

and binding_type_annotation_node prefix_nodes =
  prefix_nodes |> List.find_opt can_lift_core_type_node

and binding_value_from_prefix ~binding_syntax_node ~prefix_nodes ~value_node =
  let value = expression_from_node value_node in
  match binding_type_annotation_node prefix_nodes with
  | Some type_node ->
      expression_with_type_annotation ~syntax_node:binding_syntax_node
        ~expression:value type_node
  | None ->
      value

and module_expression_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.ModuleExpression.Path (ident_path_from_node node)
  | Syntax_kind.MODULE_PATH ->
      Cst.ModuleExpression.Path (module_path_from_node node)
  | Syntax_kind.STRUCT_EXPR ->
      Cst.ModuleExpression.Structure
        {
          syntax_node = node;
          item_syntax_nodes = direct_non_trivia_nodes node;
        }
  | Syntax_kind.FUNCTOR_TYPE -> (
      match direct_non_trivia_tokens node, List.rev (direct_non_trivia_nodes node) with
      | functor_kw :: _, body_node :: rev_parameter_nodes
        when String.equal (Ceibo.Red.SyntaxToken.text functor_kw) "functor" ->
          Cst.ModuleExpression.Functor
            {
              syntax_node = node;
              parameters =
                List.rev rev_parameter_nodes
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
                |> List.map functor_parameter_from_node;
              body = module_expression_from_node body_node;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.MODULE_APPLICATION -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Cst.ModuleExpression.Apply
            {
              syntax_node = node;
              callee = module_expression_from_node callee_node;
              argument = module_expression_from_node argument_node;
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match non_paren_tokens node, direct_non_trivia_nodes node with
      | val_kw :: _, expression_node :: _
        when String.equal (Ceibo.Red.SyntaxToken.text val_kw) "val" ->
          Cst.ModuleExpression.Unpack
            {
              syntax_node = node;
              expression = expression_from_node expression_node;
              module_type =
                (direct_non_trivia_nodes node
                |> List.find_opt can_lift_module_type_node
                |> Option.map module_type_from_node);
            }
      | _ ->
          unsupported_module_expression node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_opt can_lift_module_expression_node with
      | Some inner_node ->
          Cst.ModuleExpression.Parenthesized
            { syntax_node = node; inner = module_expression_from_node inner_node }
      | None ->
          unsupported_module_expression node)
  | Syntax_kind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node with
      | first_child :: rest -> (
          match
            List.find_opt can_lift_module_expression_node (first_child :: rest),
            List.find_opt is_attribute_node rest
          with
          | Some payload_node, Some attribute_node ->
              Cst.ModuleExpression.Attribute
                {
                  syntax_node = node;
                  module_expression = module_expression_from_node payload_node;
                  attribute = attribute_from_node attribute_node;
                }
          | _ ->
              unsupported_module_expression node)
      | [] ->
          unsupported_module_expression node)
  | _ ->
      unsupported_module_expression node

and expression_from_node node =
  let known_expression_children node =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
    |> List.map expression_from_node
  in
  let poly_variant_tag_token node =
    match direct_non_trivia_tokens node with
    | _backtick :: tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | tag_syntax_token :: _ -> Some (token tag_syntax_token)
    | [] -> None
  in
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.IDENT_EXPR ->
      Cst.Expression.Path
        { syntax_node = node; path = ident_path_from_node node }
  | Syntax_kind.MODULE_PATH ->
      Cst.Expression.Path
        { syntax_node = node; path = module_path_from_node node }
  | Syntax_kind.OPERATOR_PATTERN ->
      Cst.Expression.Operator
        { syntax_node = node; operator_tokens = operator_tokens_from_node node }
  | Syntax_kind.UNREACHABLE_EXPR -> (
      match direct_non_trivia_tokens node with
      | dot_syntax_token :: _ ->
          Cst.Expression.Unreachable
            { syntax_node = node; dot_token = token dot_syntax_token }
      | [] -> unsupported_expression node)
  | Syntax_kind.ATTRIBUTE_EXPR ->
      Cst.Expression.Attribute (attribute_from_node node)
  | Syntax_kind.EXTENSION_EXPR ->
      Cst.Expression.Extension (extension_from_node node)
  | Syntax_kind.OBJECT_EXPR -> (
      match object_expression_from_node node with
      | Some expr -> Cst.Expression.Object expr
      | None -> unsupported_expression node)
  | Syntax_kind.UNIT_LITERAL ->
      Cst.Expression.Literal (Cst.Literal.Unit { syntax_node = node })
  | Syntax_kind.METHOD_CALL_EXPR -> (
      match method_call_expression_from_node node with
      | Some expr -> Cst.Expression.MethodCall expr
      | None -> unsupported_expression node)
  | Syntax_kind.NEW_EXPR -> (
      match new_expression_from_node node with
      | Some expr -> Cst.Expression.New expr
      | None -> unsupported_expression node)
  | Syntax_kind.FIELD_ACCESS_EXPR -> (
      match field_access_expression_from_node node with
      | Some expr -> expr
      | None -> unsupported_expression node)
  | Syntax_kind.ARRAY_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> unsupported_expression node)
  | Syntax_kind.STRING_INDEX_EXPR -> (
      match index_expression_from_node node with
      | Some expr -> Cst.Expression.Index expr
      | None -> unsupported_expression node)
  | Syntax_kind.ASSIGN_EXPR -> (
      match assign_expression_from_node node with
      | Some expr -> expr
      | None -> unsupported_expression node)
  | Syntax_kind.STRING_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.String
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_expression node)
  | Syntax_kind.INT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Int
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_expression node)
  | Syntax_kind.FLOAT_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Float
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_expression node)
  | Syntax_kind.CHAR_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Char
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_expression node)
  | Syntax_kind.BOOL_LITERAL -> (
      match direct_non_trivia_tokens node with
      | literal_syntax_token :: _ ->
          Cst.Expression.Literal
            (Cst.Literal.Bool
               {
                 syntax_node = node;
                 literal_token = token literal_syntax_token;
               })
      | [] -> unsupported_expression node)
  | Syntax_kind.ASSERT_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt (fun child ->
               is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
        |> Option.map expression_from_node
      with
      | Some asserted ->
          Cst.Expression.Assert { syntax_node = node; asserted }
      | None -> unsupported_expression node)
  | Syntax_kind.LAZY_EXPR -> (
      match
        direct_non_trivia_nodes node
        |> List.find_opt (fun child ->
               is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
        |> Option.map expression_from_node
      with
      | Some body -> Cst.Expression.Lazy { syntax_node = node; body }
      | None -> unsupported_expression node)
  | Syntax_kind.WHILE_EXPR -> (
      match direct_non_trivia_nodes node with
      | condition_node :: body_node :: _ ->
          Cst.Expression.While
            {
              syntax_node = node;
              condition = expression_from_node condition_node;
              body = expression_from_node body_node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.FOR_EXPR -> (
      let non_trivia_tokens = direct_non_trivia_tokens node in
      let direction_token =
        non_trivia_tokens
        |> List.find_opt (fun syntax_token ->
               let text = Ceibo.Red.SyntaxToken.text syntax_token in
               String.equal text "to" || String.equal text "downto")
      in
      match direct_non_trivia_nodes node, non_trivia_tokens, direction_token with
      | start_node :: end_node :: body_node :: _, _for_kw :: iterator_syntax_token :: _, Some direction_syntax_token ->
          Cst.Expression.For
            {
              syntax_node = node;
              iterator_token = token iterator_syntax_token;
              start_expr = expression_from_node start_node;
              direction_token = token direction_syntax_token;
              end_expr = expression_from_node end_node;
              body = expression_from_node body_node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.APPLY_EXPR -> (
      match direct_non_trivia_nodes node with
      | callee_node :: argument_node :: _ ->
          Cst.Expression.Apply
            {
              syntax_node = node;
              callee = expression_from_node callee_node;
              argument = apply_argument_from_node argument_node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.POLY_VARIANT_EXPR -> (
      match poly_variant_tag_token node with
      | Some tag_token ->
          Cst.Expression.PolyVariant
            {
              syntax_node = node;
              tag_token;
              payload =
                (direct_non_trivia_nodes node
                |> List.find_opt (fun child ->
                       is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                |> Option.map expression_from_node);
            }
      | None -> unsupported_expression node)
  | Syntax_kind.FIRST_CLASS_MODULE_EXPR -> (
      match non_paren_tokens node, direct_non_trivia_nodes node with
      | module_kw :: _, module_expression_node :: _
        when String.equal (Ceibo.Red.SyntaxToken.text module_kw) "module" ->
          Cst.Expression.FirstClassModule
            {
              syntax_node = node;
              module_expression = module_expression_from_node module_expression_node;
              module_type =
                (direct_non_trivia_nodes node
                |> List.find_opt can_lift_module_type_node
                |> Option.map module_type_from_node);
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.LET_MODULE_EXPR -> (
      match let_module_expression_from_node node with
      | Some expr -> Cst.Expression.LetModule expr
      | None -> unsupported_expression node)
  | Syntax_kind.LET_EXPR -> (
      if is_binding_operator_expression_node node then
        match let_operator_expression_from_node node with
        | Some expr -> Cst.Expression.LetOperator expr
        | None -> unsupported_expression node
      else
        match let_expression_parts ~is_recursive_binding:false node with
        | Some (`Exception (exception_decl_node, body_node)) -> (
            match direct_non_trivia_tokens exception_decl_node with
            | _exception_kw :: name_syntax_token :: _ ->
                Cst.Expression.LetException
                  {
                    syntax_node = node;
                    exception_declaration =
                      {
                        syntax_node = exception_decl_node;
                        name_token = token name_syntax_token;
                      };
                    body = expression_from_node body_node;
                  }
            | _ -> unsupported_expression node)
        | _ -> (
            match let_expression_from_node ~is_recursive_binding:false node with
            | Some expr -> Cst.Expression.Let expr
            | None -> unsupported_expression node))
  | Syntax_kind.LET_REC_EXPR -> (
      match let_expression_from_node ~is_recursive_binding:true node with
      | Some expr -> Cst.Expression.Let expr
      | None -> unsupported_expression node)
  | Syntax_kind.TYPED_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: type_node :: _ ->
          expression_with_type_annotation ~syntax_node:node
            ~expression:(expression_from_node expr_node) type_node
      | _ -> unsupported_expression node)
  | Syntax_kind.COERCE_EXPR -> (
      match direct_non_trivia_nodes node with
      | expr_node :: to_type_node :: [] ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type = None;
              to_type = core_type_from_node to_type_node;
            }
      | expr_node :: from_type_node :: to_type_node :: _ ->
          Cst.Expression.Coerce
            {
              syntax_node = node;
              expression = expression_from_node expr_node;
              from_type = Some (core_type_from_node from_type_node);
              to_type = core_type_from_node to_type_node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.PREFIX_EXPR -> (
      match prefix_expression_from_node node with
      | Some expr -> Cst.Expression.Prefix expr
      | None -> unsupported_expression node)
  | Syntax_kind.INFIX_EXPR -> (
      match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
      | left_node :: right_node :: _, operator_syntax_token :: _ ->
          Cst.Expression.Infix
            {
              syntax_node = node;
              left = expression_from_node left_node;
              operator_token = token operator_syntax_token;
              right = expression_from_node right_node;
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.SEQUENCE_EXPR -> (
      match known_expression_children node with
      | expr :: [] -> expr
      | _ -> (
          match sequence_expression_from_node node with
          | Some expr -> Cst.Expression.Sequence expr
          | None -> unsupported_expression node))
  | Syntax_kind.TUPLE_EXPR ->
      Cst.Expression.Tuple { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.LIST_EXPR ->
      Cst.Expression.List { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.ARRAY_EXPR ->
      Cst.Expression.Array { syntax_node = node; elements = known_expression_children node }
  | Syntax_kind.RECORD_EXPR -> (
      match record_literal_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Literal expr)
      | None -> unsupported_expression node)
  | Syntax_kind.RECORD_UPDATE_EXPR -> (
      match record_update_expression_from_node node with
      | Some expr -> Cst.Expression.Record (Cst.RecordExpression.Update expr)
      | None -> unsupported_expression node)
  | Syntax_kind.OBJECT_UPDATE_EXPR -> (
      match object_update_expression_from_node node with
      | Some expr -> Cst.Expression.ObjectUpdate expr
      | None -> unsupported_expression node)
  | Syntax_kind.LOCAL_OPEN_EXPR -> (
      match local_open_expression_from_node node with
      | Some expr -> Cst.Expression.LocalOpen expr
      | None -> unsupported_expression node)
  | Syntax_kind.FUN_EXPR -> (
      match fun_expression_from_node node with
      | Some expr -> Cst.Expression.Fun expr
      | None -> unsupported_expression node)
  | Syntax_kind.FUNCTION_EXPR -> (
      match function_expression_from_node node with
      | Some expr -> Cst.Expression.Function expr
      | None -> unsupported_expression node)
  | Syntax_kind.MATCH_EXPR -> (
      match match_expression_from_node node with
      | Some expr -> Cst.Expression.Match expr
      | None -> unsupported_expression node)
  | Syntax_kind.TRY_EXPR -> (
      match try_expression_from_node node with
      | Some expr -> Cst.Expression.Try expr
      | None -> unsupported_expression node)
  | Syntax_kind.IF_EXPR -> (
      let expression_children =
        direct_non_trivia_nodes node
        |> List.filter (fun child ->
               is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
      in
      match expression_children with
      | condition_node :: then_node :: else_nodes ->
          Cst.Expression.If
            {
              syntax_node = node;
              condition = expression_from_node condition_node;
              then_branch = expression_from_node then_node;
              else_branch =
                (match else_nodes with
                | else_node :: _ -> Some (expression_from_node else_node)
                | [] -> None);
            }
      | _ -> unsupported_expression node)
  | Syntax_kind.PAREN_EXPR -> (
      match direct_non_trivia_nodes node with
      | inner_node :: _ ->
          Cst.Expression.Parenthesized
            { syntax_node = node; inner = expression_from_node inner_node }
      | [] -> unsupported_expression node)
  | _ -> unsupported_expression node

and object_method_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          Some
            {
              Cst.syntax_node = node;
              attributes = attributes_from_node node;
              name_token;
              body =
                (remainder
                |> List.rev
                |> List.find_opt (fun child ->
                       is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                |> Option.map expression_from_node);
              type_ =
                List.find_opt
                  can_lift_core_type_node
                  remainder
                |> Option.map core_type_from_node;
              is_private =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "private")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
      | None -> None)
  | _ -> None

and object_value_from_node node =
  let children_without_attributes =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
  in
  match children_without_attributes with
  | name_node :: remainder
    when Ceibo.Red.SyntaxNode.kind name_node = Syntax_kind.IDENT_EXPR -> (
      match first_ident_token_in_subtree name_node with
      | Some name_token ->
          Some
            {
              Cst.syntax_node = node;
              attributes = attributes_from_node node;
              name_token;
              value =
                (remainder
                |> List.rev
                |> List.find_opt (fun child ->
                       is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
                |> Option.map expression_from_node);
              type_ =
                List.find_opt
                  can_lift_core_type_node
                  remainder
                |> Option.map core_type_from_node;
              is_mutable =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "mutable")
                  (direct_non_trivia_tokens node);
              is_virtual =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "virtual")
                  (direct_non_trivia_tokens node);
              is_override =
                List.exists
                  (fun tok -> String.equal (Ceibo.Red.SyntaxToken.text tok) "!")
                  (direct_non_trivia_tokens node);
            }
      | None -> None)
  | _ -> None

and object_inherit_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.ATTRIBUTE_EXPR)
    |> List.find_map (fun child ->
           if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
             Some (expression_from_node child)
           else
             None)
  with
  | Some expression ->
      Some
        {
          Cst.syntax_node = node;
          attributes = attributes_from_node node;
          expression;
        }
  | None -> None

and object_initializer_from_node node =
  match direct_non_trivia_tokens node, direct_non_trivia_nodes node with
  | initializer_kw :: _, children
    when String.equal (Ceibo.Red.SyntaxToken.text initializer_kw) "initializer" ->
      let body =
        children
        |> List.filter (fun child -> not (is_attribute_node child))
        |> List.find_map (fun child ->
               if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
                 Some (expression_from_node child)
               else
                 None)
      in
      Some ({ syntax_node = node; body } : Cst.object_initializer)
  | _ -> None

and object_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let self_pattern, member_children =
    match non_trivia_children with
    | self_node :: rest
      when Ceibo.Red.SyntaxNode.kind self_node = Syntax_kind.OBJECT_SELF -> (
        match direct_non_trivia_nodes self_node with
        | pattern_node :: _ -> (Some (pattern_from_node pattern_node), rest)
        | [] -> (None, rest))
    | _ -> (None, non_trivia_children)
  in
  let rec lift_members acc = function
    | [] -> Some (List.rev acc)
    | child :: rest -> (
        match Ceibo.Red.SyntaxNode.kind child with
        | Syntax_kind.OBJECT_METHOD -> (
            match object_method_from_node child with
            | Some member -> lift_members (Cst.Method member :: acc) rest
            | None -> None)
        | Syntax_kind.OBJECT_VAL -> (
            match object_value_from_node child with
            | Some member -> lift_members (Cst.Value member :: acc) rest
            | None -> None)
        | Syntax_kind.OBJECT_INHERIT -> (
            match object_inherit_from_node child with
            | Some member -> lift_members (Cst.Inherit member :: acc) rest
            | None -> None)
        | Syntax_kind.IDENT_EXPR -> (
            match object_initializer_from_node child with
            | Some member -> lift_members (Cst.Initializer member :: acc) rest
            | None -> None)
        | Syntax_kind.ATTRIBUTE_EXPR ->
            lift_members acc rest
        | _ -> None)
  in
  match lift_members [] member_children with
  | Some members ->
      Some ({ syntax_node = node; self_pattern; members } : Cst.object_expression)
  | None -> None

and method_call_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, method_name_tok :: _ ->
      Some
        {
          Cst.syntax_node = node;
          receiver = expression_from_node receiver_node;
          method_name = token method_name_tok;
        }
  | _ -> None

and new_expression_from_node node =
  match direct_non_trivia_nodes node with
  | class_path_node :: _ ->
      Some
        {
          Cst.syntax_node = node;
          class_path = module_path_like_from_node class_path_node;
        }
  | [] -> None

and object_update_expression_from_node node =
  let children = direct_non_trivia_nodes node in
  if
    List.for_all
      (fun child -> Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
      children
  then
    Some
      {
        Cst.syntax_node = node;
        fields = List.filter_map record_expression_field_from_node children;
      }
  else
    None

and field_access_expression_from_node node =
  match direct_non_trivia_nodes node, List.rev (direct_non_trivia_tokens node) with
  | receiver_node :: _, field_token :: _ ->
      Some
        (Cst.Expression.FieldAccess
           {
             syntax_node = node;
             receiver = expression_from_node receiver_node;
             field_name = token field_token;
           })
  | _ -> None

and index_expression_from_node node =
  match direct_non_trivia_nodes node with
  | collection_node :: index_node :: _ ->
      Some
        {
          syntax_node = node;
          collection = expression_from_node collection_node;
          index = expression_from_node index_node;
        }
  | _ -> None

and assign_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | target_node :: value_node :: _, operator_syntax_token :: _ ->
      let operator_token = token operator_syntax_token in
      Some
        (match
           Ceibo.Red.SyntaxNode.kind target_node,
           direct_non_trivia_tokens target_node
         with
        | Syntax_kind.IDENT_EXPR, name_syntax_token :: _
          when String.equal (Cst.Token.text operator_token) "<-" ->
            Cst.Expression.InstanceVariableAssign
              {
                syntax_node = node;
                name_token = token name_syntax_token;
                operator_token;
                value = expression_from_node value_node;
              }
        | _ ->
            Cst.Expression.Assign
              {
                syntax_node = node;
                target = expression_from_node target_node;
                operator_token;
                value = expression_from_node value_node;
              })
  | _ -> None

and prefix_expression_from_node node =
  match direct_non_trivia_nodes node, direct_non_trivia_tokens node with
  | operand_node :: _, operator_syntax_token :: _ ->
      Some
        {
          syntax_node = node;
          operator_token = token operator_syntax_token;
          operand = expression_from_node operand_node;
        }
  | _ -> None

and sequence_expression_from_node node =
  match direct_non_trivia_nodes node with
  | left_node :: right_node :: _ ->
      Some
        {
          syntax_node = node;
          left = expression_from_node left_node;
          right = expression_from_node right_node;
        }
  | _ -> None

and record_field_path_from_node node =
  let tokens =
    direct_non_trivia_tokens node |> take_tokens_until_equals []
  in
  module_path_from_tokens ~syntax_node:node tokens

and record_field_value_from_node node =
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
           Some (expression_from_node child)
         else
           None)

and record_expression_field_from_node node =
  let lifted_field_path = record_field_path_from_node node in
  match Cst.Ident.segments lifted_field_path with
  | [] -> None
  | _ ->
      Some
        {
          syntax_node = node;
          field_path = lifted_field_path;
          value = record_field_value_from_node node;
        }

and record_literal_expression_from_node node =
  let fields =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
    |> List.filter_map record_expression_field_from_node
  in
  Some ({ syntax_node = node; fields } : Cst.record_literal_expression)

and record_update_expression_from_node node =
  match direct_non_trivia_nodes node with
  | base_node :: rest -> (
      let lifted_base =
        match Ceibo.Red.SyntaxNode.kind base_node with
        | Syntax_kind.RECORD_FIELD -> (
            match record_expression_field_from_node base_node with
            | Some { field_path; value = None; _ } ->
                Some
                  (Cst.Expression.Path
                     { syntax_node = base_node; path = field_path })
            | _ -> None)
        | _ ->
            if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind base_node) then
              Some (expression_from_node base_node)
            else
              None
      in
      match lifted_base with
      | Some base ->
          Some
            ( {
                syntax_node = node;
                base;
                fields =
                  rest
                  |> List.filter (fun child ->
                         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.RECORD_FIELD)
                  |> List.filter_map record_expression_field_from_node;
              }
              : Cst.record_update_expression )
      | None -> None)
  | [] -> None

and local_open_expression_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let non_trivia_tokens = direct_non_trivia_tokens node in
  let via_let_open =
    non_trivia_tokens
    |> List.exists (fun tok ->
           String.equal (Ceibo.Red.SyntaxToken.text tok) "let")
  in
  let module_path =
    let rec collect_after_open = function
      | [] -> []
      | syntax_token :: rest ->
          if String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "open" then
            collect_module_tokens [] rest
          else collect_after_open rest
    and collect_module_tokens acc = function
      | [] -> List.rev acc
      | syntax_token :: rest ->
          let text = Ceibo.Red.SyntaxToken.text syntax_token in
          if String.equal text "in" then
            List.rev acc
          else collect_module_tokens (syntax_token :: acc) rest
    in
    match collect_after_open non_trivia_tokens with
    | [] -> None
    | lifted_tokens ->
        Some (module_path_from_tokens ~syntax_node:node lifted_tokens)
  in
  let prefix_module_path =
    match non_trivia_children with
    | module_path_node :: _body_node :: _ ->
        Some (module_path_like_from_node module_path_node)
    | _ -> None
  in
  let module_path =
    if via_let_open then module_path else prefix_module_path
  in
  let body_expr =
    if via_let_open then
      List.rev non_trivia_children
      |> List.find_map (fun child ->
             if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child) then
               Some (expression_from_node child)
             else
               None)
    else
      match non_trivia_children with
      | _module_path_node :: body_node :: _ ->
          if is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind body_node) then
            Some (expression_from_node body_node)
          else
            None
      | _ -> None
  in
  match module_path, body_expr with
  | Some lifted_module_path, Some lifted_body ->
      Some
        {
          syntax_node = node;
          module_path = lifted_module_path;
          body = lifted_body;
          via_let_open;
        }
  | _ -> None

and let_module_expression_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let payload_children =
    direct_children |> List.filter (fun child -> not (is_attribute_node child))
  in
  match direct_non_trivia_tokens node, payload_children with
  | _let_kw :: _module_kw :: module_name_token :: _, module_expression_node :: body_node :: _ ->
      Some
        {
          syntax_node = node;
          module_name_token = token module_name_token;
          module_expression = module_expression_from_node module_expression_node;
          body = expression_from_node body_node;
        }
  | _ -> None

and fun_expression_from_node node =
  match List.rev (direct_non_trivia_nodes node) with
  | body_node :: rev_param_nodes ->
      Some
        {
          syntax_node = node;
          parameters =
            rev_param_nodes
            |> List.rev
            |> List.map parameter_from_node;
          body = expression_from_node body_node;
        }
  | [] -> None

and function_expression_from_node node =
  let match_cases =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
    |> List.filter_map match_case_from_node
  in
  Some { Cst.syntax_node = node; cases = match_cases }

and let_operator_expression_from_node node =
  let operator_pairs = binding_operator_pairs_from_tokens node in
  let lifted_children =
    direct_non_trivia_nodes node
    |> List.filter (fun child -> not (is_attribute_node child))
  in
  match List.rev lifted_children with
  | body_node :: rev_binding_nodes ->
      let binding_nodes = List.rev rev_binding_nodes in
      let expected_binding_nodes = 2 * List.length operator_pairs in
      if
        List.length operator_pairs = 0
        || not (Int.equal (List.length binding_nodes) expected_binding_nodes)
      then
        None
      else
        let rec lift_bindings acc token_pairs nodes =
          match token_pairs, nodes with
          | [], [] ->
              List.rev acc
          | (keyword_token, operator_token) :: rest_pairs, binding_pattern_node :: bound_value_node :: rest_nodes ->
              let binding : Cst.binding_operator_binding =
                {
                  keyword_token;
                  operator_token;
                  binding_pattern = pattern_from_node binding_pattern_node;
                  bound_value = expression_from_node bound_value_node;
                }
              in
              lift_bindings (binding :: acc) rest_pairs rest_nodes
          | _ ->
              bail
                ~message:
                  "expected alternating binding-operator pattern/value nodes during Ceibo -> CST lifting"
                ~syntax_node:node
                ~context:[ "expression"; "let_operator"; "bindings" ]
        in
        (match lift_bindings [] operator_pairs binding_nodes with
        | binding :: and_bindings ->
            Some
              {
                Cst.syntax_node = node;
                binding;
                and_bindings;
                body = expression_from_node body_node;
              }
        | [] -> None)
  | [] ->
      None

and let_expression_from_node ~is_recursive_binding node =
  if is_binding_operator_expression_node node then
    None
  else
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          prefix_nodes,
          bound_value_node,
          and_binding_nodes,
          body_node )) ->
      let lift_and_binding node =
        let direct_children = direct_non_trivia_nodes node in
        let binding_attributes =
          direct_children
          |> List.filter is_attribute_node
          |> List.map attribute_from_node
        in
        let binding_children =
          direct_children |> List.filter (fun child -> not (is_attribute_node child))
        in
        match binding_children with
        | nested_binding_pattern_node :: rest -> (
            match List.rev rest with
            | value_node :: rev_param_nodes ->
                let prefix_nodes = List.rev rev_param_nodes in
                Some
                  Cst.LetBinding.
                    {
                      syntax_node = node;
                      attributes = binding_attributes;
                      binding_pattern = pattern_from_node nested_binding_pattern_node;
                      binding_name =
                        simple_pattern_name_token nested_binding_pattern_node;
                      parameters =
                        prefix_nodes
                        |> List.filter (fun child ->
                               is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                        |> List.map parameter_from_node;
                      value =
                        binding_value_from_prefix ~binding_syntax_node:node
                          ~prefix_nodes:prefix_nodes ~value_node:value_node;
                      is_recursive = is_recursive_binding;
                    }
            | [] -> None)
        | [] -> None
      in
      Some
        {
          syntax_node = node;
          binding_pattern = pattern_from_node binding_pattern_node;
          bound_value =
            binding_value_from_prefix ~binding_syntax_node:node
              ~prefix_nodes:prefix_nodes ~value_node:bound_value_node;
          and_bindings =
            and_binding_nodes
            |> List.filter_map lift_and_binding;
          body = expression_from_node body_node;
          is_recursive = is_recursive_binding;
        }
  | _ -> None

and let_binding_from_binding_operator_binding ~binding_syntax_node
    ( { binding_pattern = clause_pattern; bound_value = clause_value; _ } :
        Cst.binding_operator_binding ) =
  Cst.LetBinding.
    {
      syntax_node = binding_syntax_node;
      attributes = [];
      binding_pattern = clause_pattern;
      binding_name =
        simple_pattern_name_token (Cst.Pattern.syntax_node clause_pattern);
      parameters = [];
      value = clause_value;
      is_recursive = false;
    }

and match_case_from_node node =
  let non_trivia_children = direct_non_trivia_nodes node in
  let expression_children =
    non_trivia_children
    |> List.filter (fun child ->
           is_expression_syntax_kind (Ceibo.Red.SyntaxNode.kind child))
    |> List.map expression_from_node
  in
  let has_guard =
    List.exists
      (fun child ->
        Ceibo.Red.SyntaxNode.kind child = Syntax_kind.PATTERN_GUARD)
      non_trivia_children
  in
  match non_trivia_children with
  | pattern_node :: _ -> (
      match expression_children, has_guard with
      | [], _ -> None
      | body_exprs, false -> (
          match List.rev body_exprs with
          | body_expr :: _ ->
              Some
                {
                  syntax_node = node;
                  pattern = pattern_from_node pattern_node;
                  guard = None;
                  body = body_expr;
                }
          | [] -> None)
      | guard_expr :: body_expr :: _, true ->
          Some
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              guard = Some guard_expr;
              body = body_expr;
            }
      | body_expr :: _, true ->
          Some
            {
              syntax_node = node;
              pattern = pattern_from_node pattern_node;
              guard = None;
              body = body_expr;
            })
  | [] -> None

and match_expression_from_node node =
  match direct_non_trivia_nodes node with
  | scrutinee_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        {
          syntax_node = node;
          scrutinee = expression_from_node scrutinee_node;
          cases = match_cases;
        }
  | [] -> None

and try_expression_from_node node =
  match direct_non_trivia_nodes node with
  | body_node :: rest ->
      let match_cases =
        rest
        |> List.filter (fun child ->
               Ceibo.Red.SyntaxNode.kind child = Syntax_kind.MATCH_CASE)
        |> List.filter_map match_case_from_node
      in
      Some
        {
          syntax_node = node;
          body = expression_from_node body_node;
          cases = match_cases;
        }
  | [] -> None

and type_variable_from_node node =
  match List.rev (direct_non_trivia_tokens node) with
  | name_tok :: _ ->
      Some Cst.TypeVariable.{ syntax_node = node; name_token = token name_tok }
  | [] -> None

and type_parameter_from_node node =
  let lifted_type_variable =
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VAR)
    |> function
    | Some child -> type_variable_from_node child
    | None -> None
  in
  Cst.TypeParameter.{ syntax_node = node; type_variable = lifted_type_variable }

and type_parameters_from_node node =
  direct_non_trivia_nodes node
  |> List.filter (fun child ->
         Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
  |> List.map type_parameter_from_node

let record_field_name_token node =
  match direct_non_trivia_tokens node with
  | mutable_kw :: field_name :: _
    when String.equal (Ceibo.Red.SyntaxToken.text mutable_kw) "mutable" ->
      Some (token field_name)
  | field_name :: _ -> Some (token field_name)
  | [] -> None

let record_field_from_node node =
  record_field_name_token node
  |> Option.map (fun field_name ->
         let mutable_field =
           match direct_non_trivia_tokens node with
           | first :: _ ->
               String.equal (Ceibo.Red.SyntaxToken.text first) "mutable"
           | [] -> false
         in
         Cst.RecordField.
           {
             syntax_node = node;
              field_name = field_name;
              field_type =
                (direct_non_trivia_nodes node
                |> List.find_opt can_lift_core_type_node
                |> function
                | Some field_type_node -> core_type_from_node field_type_node
                | None ->
                    bail
                      ~message:
                        "expected record field type during Ceibo -> CST lifting"
                      ~syntax_node:node ~context:[ "type_definition.record_field" ]);
             is_mutable = mutable_field;
           })

let variant_constructor_from_node node =
  match direct_non_trivia_nodes node with
  | first_child :: _ -> (
      match direct_non_trivia_tokens first_child with
      | constructor_name :: _ ->
          Some
            Cst.VariantConstructor.
              {
                syntax_node = node;
                constructor_name = token constructor_name;
                payload_type =
                  (direct_non_trivia_nodes node
                  |> List.find_opt (fun child ->
                         let kind = Ceibo.Red.SyntaxNode.kind child in
                         can_lift_core_type_node child
                         && kind != Syntax_kind.IDENT_EXPR)
                  |> Option.map core_type_from_node);
              }
      | [] -> None)
  | [] -> None

let poly_variant_tag_from_node node =
  match direct_non_trivia_tokens node with
  | _backtick :: tag_name :: _ ->
      Cst.PolyVariantTag.
        {
          syntax_node = node;
          tag_name = token tag_name;
          payload_type =
            (direct_non_trivia_nodes node
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
  | tag_name :: _ ->
      Cst.PolyVariantTag.
        {
          syntax_node = node;
          tag_name = token tag_name;
          payload_type =
            (direct_non_trivia_nodes node
            |> List.find_opt can_lift_core_type_node
            |> Option.map core_type_from_node);
        }
  | [] ->
      bail ~message:"expected poly-variant tag token during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_definition.poly_variant_tag" ]

let poly_variant_bound_from_node node =
  match direct_non_trivia_tokens node with
  | _open_bracket :: marker_token :: _
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) "<" ->
      Cst.PolyVariantBound.UpperBound { marker_token = token marker_token }
  | _open_bracket :: marker_token :: _
    when String.equal (Ceibo.Red.SyntaxToken.text marker_token) ">" ->
      Cst.PolyVariantBound.LowerBound { marker_token = token marker_token }
  | _ ->
      Cst.PolyVariantBound.Exact

let row_field_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.POLY_VARIANT_TAG ->
      Cst.RowField.Tag (poly_variant_tag_from_node node)
  | _ when can_lift_core_type_node node ->
      Cst.RowField.Inherit
        { syntax_node = node; type_ = core_type_from_node node }
  | _ ->
      bail ~message:"expected polymorphic variant row field during Ceibo -> CST lifting"
        ~syntax_node:node ~context:[ "type_definition.poly_variant.row_field" ]

let poly_variant_from_node node =
  {
    Cst.syntax_node = node;
    kind = poly_variant_bound_from_node node;
    fields =
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.POLY_VARIANT_TAG || can_lift_core_type_node child)
      |> List.map row_field_from_node;
  }

let type_declaration_name_path node =
  let is_name_node child =
    let kind = Ceibo.Red.SyntaxNode.kind child in
    kind = Syntax_kind.IDENT_EXPR || kind = Syntax_kind.MODULE_PATH
  in
  direct_non_trivia_nodes node
  |> List.find_opt is_name_node
  |> Option.map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.MODULE_PATH -> module_path_from_node child
         | Syntax_kind.IDENT_EXPR -> ident_path_from_node child
         | _ ->
             bail ~message:"expected type declaration name path during Ceibo -> CST lifting"
               ~syntax_node:child ~context:[ "type_declaration"; "name" ])

let is_type_extension_node node =
  Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_DECL
  &&
  (direct_non_trivia_tokens node
  |> List.exists (fun syntax_token ->
         String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "+"))

let type_definition_from_node node =
  if Ceibo.Red.SyntaxNode.kind node = Syntax_kind.TYPE_EXTENSIBLE then
    Cst.TypeDefinition.Extensible { syntax_node = node }
  else
  let direct_children = direct_non_trivia_nodes node in
  let variant_constructors =
    direct_children
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  if List.length variant_constructors > 0 then
    Cst.TypeDefinition.Variant variant_constructors
  else
    match
      direct_children
      |> List.find_opt (fun child ->
             Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD)
    with
    | Some record_node ->
        let fields =
          direct_non_trivia_nodes record_node
          |> List.filter (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_RECORD_FIELD)
          |> List.filter_map record_field_from_node
        in
        Cst.TypeDefinition.Record fields
    | None -> (
        match
          direct_children
          |> List.find_opt (fun child ->
                 Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_POLY_VARIANT)
        with
        | Some poly_variant_node ->
            Cst.TypeDefinition.PolyVariant (poly_variant_from_node poly_variant_node)
        | None ->
            let remaining_nodes =
              direct_children
              |> List.filter (fun child ->
                     let kind = Ceibo.Red.SyntaxNode.kind child in
                     kind != Syntax_kind.TYPE_PARAM
                     && kind != Syntax_kind.IDENT_EXPR
                     && kind != Syntax_kind.MODULE_PATH
                     && not (kind = Syntax_kind.ATTRIBUTE_EXPR && not (can_lift_core_type_node child)))
            in
            match remaining_nodes with
            | [] -> Cst.TypeDefinition.Abstract
            | first :: _ ->
                let kind = Ceibo.Red.SyntaxNode.kind first in
        if kind = Syntax_kind.TYPE_EXTENSIBLE then
          Cst.TypeDefinition.Extensible { syntax_node = first }
        else if kind = Syntax_kind.TYPE_CONSTR || kind = Syntax_kind.TYPE_ARROW
           || kind = Syntax_kind.TYPE_TUPLE || kind = Syntax_kind.TYPE_VAR
           || kind = Syntax_kind.TYPE_ALIAS || kind = Syntax_kind.TYPE_PAREN
           || kind = Syntax_kind.ATTRIBUTE_EXPR
           || kind = Syntax_kind.EXTENSION_EXPR
        then
          Cst.TypeDefinition.Alias
            { syntax_node = first; manifest = core_type_from_node first }
        else if kind = Syntax_kind.OBJECT_TYPE then
          Cst.TypeDefinition.Object
            {
              syntax_node = first;
              fields =
                direct_non_trivia_nodes first
                |> List.filter (fun child ->
                       Ceibo.Red.SyntaxNode.kind child = Syntax_kind.OBJECT_TYPE_FIELD)
                |> List.map (fun field_node ->
                       match
                         first_ident_token_in_subtree field_node,
                         direct_non_trivia_nodes field_node
                         |> List.find_opt can_lift_core_type_node
                       with
                       | Some field_name, Some field_type_node ->
                           {
                             Cst.syntax_node = field_node;
                             field_name;
                             field_type = core_type_from_node field_type_node;
                           }
                       | _ ->
                           bail
                             ~message:
                               "expected object type field name and type during Ceibo -> CST lifting"
                             ~syntax_node:field_node
                             ~context:[ "type_definition.object_field" ]);
            }
        else if kind = Syntax_kind.FIRST_CLASS_MODULE_TYPE then
          Cst.TypeDefinition.FirstClassModule
            {
              syntax_node = first;
              module_type = module_type_from_first_class_module_type_node first;
            }
        else Cst.TypeDefinition.Other first)

let type_declaration_from_node node =
  let lifted_type_params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  match type_declaration_name_path node with
  | Some lifted_type_name -> (
      match Cst.Ident.last_segment lifted_type_name with
      | Some _ ->
          Some
            Cst.TypeDeclaration.
              {
                syntax_node = node;
                type_name = lifted_type_name;
                type_params = lifted_type_params;
                type_definition = type_definition_from_node node;
              }
      | None -> None)
  | None -> None

let type_extension_from_node node =
  let extension_type_params =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_PARAM)
    |> List.map type_parameter_from_node
  in
  let extension_constructors =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child = Syntax_kind.TYPE_VARIANT_CONSTR)
    |> List.filter_map variant_constructor_from_node
  in
  match type_declaration_name_path node with
  | Some extension_type_name when List.length extension_constructors > 0 -> (
      match Cst.Ident.last_segment extension_type_name with
      | Some _ ->
          Some
            Cst.TypeExtension.
              {
                syntax_node = node;
                type_name = extension_type_name;
                type_params = extension_type_params;
                constructors = extension_constructors;
              }
      | None -> None)
  | _ -> None

let let_binding_from_node ~is_recursive_binding node =
  let is_recursive_binding =
    is_recursive_binding
    || List.exists
         (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "rec")
         (direct_non_trivia_tokens node)
  in
  let direct_children = direct_non_trivia_nodes node in
  let binding_attributes =
    direct_children
    |> List.filter is_attribute_node
    |> List.map attribute_from_node
  in
  let binding_children =
    direct_children |> List.filter (fun child -> not (is_attribute_node child))
  in
  match binding_children with
  | binding_pattern_node :: rest -> (
      match List.rev rest with
      | value_node :: rev_param_nodes ->
          let prefix_nodes = List.rev rev_param_nodes in
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                attributes = binding_attributes;
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters =
                  prefix_nodes
                  |> List.filter (fun child ->
                         is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                  |> List.map parameter_from_node;
                value =
                  binding_value_from_prefix ~binding_syntax_node:node
                    ~prefix_nodes:prefix_nodes ~value_node:value_node;
                is_recursive = is_recursive_binding;
              }
      | [] -> None)
  | [] -> None

let let_expression_binding_from_node ~is_recursive_binding node =
  if (not is_recursive_binding) && is_binding_operator_expression_node node then
    match let_operator_expression_from_node node with
    | Some expr ->
        Some
          (let_binding_from_binding_operator_binding ~binding_syntax_node:node
             expr.binding)
    | None ->
        None
  else
  match let_expression_parts ~is_recursive_binding node with
  | Some
      (`Value
        ( is_recursive_binding,
          binding_pattern_node,
          prefix_nodes,
          bound_value_node,
          _and_binding_nodes,
          _body_node )) ->
          Some
            Cst.LetBinding.
              {
                syntax_node = node;
                attributes = [];
                binding_pattern = pattern_from_node binding_pattern_node;
                binding_name = simple_pattern_name_token binding_pattern_node;
                parameters =
                  prefix_nodes
                  |> List.filter (fun child ->
                         is_parameter_like_kind (Ceibo.Red.SyntaxNode.kind child))
                  |> List.map parameter_from_node;
                value =
                  binding_value_from_prefix ~binding_syntax_node:node
                    ~prefix_nodes:prefix_nodes ~value_node:bound_value_node;
                is_recursive = is_recursive_binding;
              }
  | _ -> None

let module_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: rest -> (
      let is_recursive_declaration, module_name =
        match rest with
        | rec_kw :: module_name :: _
          when String.equal (Ceibo.Red.SyntaxToken.text rec_kw) "rec" ->
            (true, module_name)
        | module_name :: _ ->
            (false, module_name)
        | [] ->
            bail ~message:"expected module name during Ceibo -> CST lifting"
              ~syntax_node:node ~context:[ "item"; "module_declaration" ]
      in
      let direct_children = direct_non_trivia_nodes node in
      let has_equals =
        direct_non_trivia_tokens node
        |> List.exists (fun syntax_token ->
               String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "=")
      in
      let lifted_module_expression =
        if has_equals then
          direct_children
          |> List.rev
          |> List.find_opt can_lift_module_expression_node
          |> Option.map module_expression_from_node
        else
          None
      in
      let module_type_search_children =
        if has_equals then
          match
            direct_children
            |> List.rev
            |> List.find_opt can_lift_module_expression_node
          with
          | Some module_expression_node ->
              direct_children
              |> List.filter (fun child ->
                     child != module_expression_node)
          | None ->
              direct_children
        else
          direct_children
      in
      let lifted_module_type =
        module_type_search_children
        |> List.find_opt can_lift_module_type_node
        |> Option.map module_type_from_node
      in
      Some
        Cst.ModuleDeclaration.
          {
            syntax_node = node;
            module_name = token module_name;
            functor_parameters =
              direct_children
              |> List.filter (fun child ->
                     Ceibo.Red.SyntaxNode.kind child = Syntax_kind.FUNCTOR_PARAM)
              |> List.map functor_parameter_from_node;
            module_type = lifted_module_type;
            module_expression = lifted_module_expression;
            is_recursive = is_recursive_declaration;
          })
  | _ -> None

let module_type_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _module_kw :: _type_kw :: module_type_name :: _ ->
      Some
        Cst.ModuleTypeDeclaration.
          {
            syntax_node = node;
            module_type_name = token module_type_name;
            module_type =
              (direct_non_trivia_nodes node
              |> List.rev
              |> List.find_opt can_lift_module_type_node
              |> Option.map module_type_from_node);
          }
  | _ -> None

let class_declaration_from_node node =
  let children =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM)
  in
  let rec split_at_name acc = function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR ->
        Some (child, List.rev acc, rest)
    | child :: rest ->
        split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, _prefix, remainder) -> (
      match first_ident_token_in_subtree name_node, List.rev remainder with
      | Some class_name, class_body_node :: rev_prefix ->
          Some
            {
              Cst.syntax_node = node;
              type_params = type_parameters_from_node node;
              class_name;
              class_type_syntax_node =
                (match List.rev rev_prefix with
                | class_type_syntax_node :: _ -> Some class_type_syntax_node
                | [] -> None);
              class_body = expression_from_node class_body_node;
            }
      | _ -> None)
  | None -> None

let class_type_declaration_from_node node =
  let children =
    direct_non_trivia_nodes node
    |> List.filter (fun child ->
           Ceibo.Red.SyntaxNode.kind child != Syntax_kind.TYPE_PARAM)
  in
  let rec split_at_name acc = function
    | child :: rest when Ceibo.Red.SyntaxNode.kind child = Syntax_kind.IDENT_EXPR ->
        Some (child, List.rev acc, rest)
    | child :: rest ->
        split_at_name (child :: acc) rest
    | [] -> None
  in
  match split_at_name [] children with
  | Some (name_node, _prefix, body_node :: _) -> (
      match first_ident_token_in_subtree name_node with
      | Some class_type_name ->
          Some
            {
              Cst.syntax_node = node;
              type_params = type_parameters_from_node node;
              class_type_name;
              class_type_body_syntax_node = body_node;
            }
      | None -> None)
  | _ -> None

let open_statement_from_node node =
  let tokens = direct_non_trivia_tokens node in
  let bang_token_opt =
    tokens
    |> List.find_opt (fun syntax_token ->
           String.equal (Ceibo.Red.SyntaxToken.text syntax_token) "!")
    |> Option.map token
  in
  let module_tokens =
    tokens
    |> List.filter (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not
             (String.equal text "open"
             || String.equal text "!"))
  in
  match module_tokens with
  | [] -> None
  | _ ->
      Some
        Cst.OpenStatement.
          {
            syntax_node = node;
            module_path = module_path_from_tokens ~syntax_node:node module_tokens;
            bang_token = bang_token_opt;
          }

let declaration_name_token_from_node node =
  let operator_token_from_node node =
    direct_non_trivia_tokens node
    |> List.find_opt (fun syntax_token ->
           let text = Ceibo.Red.SyntaxToken.text syntax_token in
           not (String.equal text "(" || String.equal text ")"))
    |> Option.map token
  in
  direct_non_trivia_nodes node
  |> List.find_map (fun child ->
         match Ceibo.Red.SyntaxNode.kind child with
         | Syntax_kind.IDENT_EXPR ->
             direct_non_trivia_tokens child |> List.find_opt (fun _ -> true) |> Option.map token
         | Syntax_kind.OPERATOR_PATTERN -> operator_token_from_node child
         | _ -> None)

let value_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  match declaration_name_token_from_node node with
  | Some lifted_name_token -> (
      match
        List.rev direct_children
        |> List.find_opt can_lift_core_type_node
      with
      | Some lifted_type_node ->
          Some
            ({ syntax_node = node; name_token = lifted_name_token; type_ = core_type_from_node lifted_type_node }
              : Cst.value_declaration)
      | None -> None)
  | None -> None

let external_declaration_from_node node =
  let direct_children = direct_non_trivia_nodes node in
  let lifted_primitive_name_tokens =
    direct_non_trivia_tokens node
    |> List.filter (fun syntax_token ->
           Ceibo.Red.SyntaxToken.kind syntax_token = Syntax_kind.STRING_LITERAL)
    |> List.map token
  in
  let external_name_token =
    match direct_non_trivia_tokens node with
    | _external_kw :: name_syntax_token :: _ -> Some (token name_syntax_token)
    | _ -> None
  in
  match external_name_token with
  | Some lifted_name_token -> (
      match
        direct_children
        |> List.find_opt can_lift_core_type_node
      with
      | Some lifted_type_node ->
          Some
            ({
               syntax_node = node;
               name_token = lifted_name_token;
               type_ = core_type_from_node lifted_type_node;
               primitive_name_tokens = lifted_primitive_name_tokens;
             }
              : Cst.external_declaration)
      | None -> None)
  | None -> None

let include_statement_from_node node =
  match
    direct_non_trivia_nodes node
    |> List.find_opt (fun child ->
           can_lift_module_expression_node child || can_lift_module_type_node child)
  with
  | Some included_node ->
      Some
        ({
           syntax_node = node;
           target =
             if can_lift_module_expression_node included_node then
               Cst.ModuleExpression (module_expression_from_node included_node)
             else if can_lift_module_type_node included_node then
               Cst.ModuleType (module_type_from_node included_node)
             else
               bail ~message:"expected include target during Ceibo -> CST lifting"
                 ~syntax_node:included_node ~context:[ "include_statement" ];
         }
          : Cst.include_statement)
  | None -> None

let exception_declaration_from_node node =
  match direct_non_trivia_tokens node with
  | _exception_kw :: name_syntax_token :: _ ->
      Some
        ({ syntax_node = node; name_token = token name_syntax_token }
          : Cst.exception_declaration)
  | _ -> None

let rec collect_let_bindings node =
  let bindings_here =
    match Ceibo.Red.SyntaxNode.kind node with
    | Syntax_kind.LET_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:false node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_REC_BINDING ->
        Option.to_list (let_binding_from_node ~is_recursive_binding:true node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:false node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | Syntax_kind.LET_REC_EXPR ->
        Option.to_list
          (let_expression_binding_from_node ~is_recursive_binding:true node)
        |> List.filter (fun binding ->
               Option.is_some (Cst.LetBinding.binding_name_token binding))
    | _ -> []
  in
  let nested =
    direct_non_trivia_nodes node |> List.concat_map collect_let_bindings
  in
  bindings_here @ nested

let rec collect_expressions_from_expression expr =
  let nested =
    match expr with
    | Cst.Expression.Path _ | Cst.Expression.Operator _
    | Cst.Expression.Literal _ | Cst.Expression.Unreachable _
    | Cst.Expression.Attribute _
    | Cst.Expression.Extension _ | Cst.Expression.New _ ->
        []
    | Cst.Expression.Object { members; _ } ->
        members
        |> List.concat_map (function
             | Cst.Method { body; _ } ->
                 Option.to_list body |> List.concat_map collect_expressions_from_expression
             | Cst.Value { value; _ } ->
                 Option.to_list value |> List.concat_map collect_expressions_from_expression
             | Cst.Inherit { expression; _ } ->
                 collect_expressions_from_expression expression
             | Cst.Initializer { body; _ } ->
                 Option.to_list body |> List.concat_map collect_expressions_from_expression)
    | Cst.Expression.PolyVariant { payload; _ } ->
        Option.to_list payload |> List.concat_map collect_expressions_from_expression
    | Cst.Expression.FirstClassModule _ ->
        []
    | Cst.Expression.LetModule { body; _ } ->
        collect_expressions_from_expression body
    | Cst.Expression.LetException { body; _ } ->
        collect_expressions_from_expression body
    | Cst.Expression.Assert { asserted; _ } ->
        collect_expressions_from_expression asserted
    | Cst.Expression.Lazy { body; _ } ->
        collect_expressions_from_expression body
    | Cst.Expression.While { condition; body; _ } ->
        collect_expressions_from_expression condition
        @ collect_expressions_from_expression body
    | Cst.Expression.For { start_expr; end_expr; body; _ } ->
        collect_expressions_from_expression start_expr
        @ collect_expressions_from_expression end_expr
        @ collect_expressions_from_expression body
    | Cst.Expression.Apply { callee; argument; _ } ->
        collect_expressions_from_expression callee
        @
        (match argument with
        | Cst.Positional argument ->
            collect_expressions_from_expression argument
        | Cst.Labeled { value; _ } | Cst.Optional { value; _ } ->
            Option.to_list value
            |> List.concat_map collect_expressions_from_expression)
    | Cst.Expression.MethodCall { receiver; _ } ->
        collect_expressions_from_expression receiver
    | Cst.Expression.Prefix { operand; _ } ->
        collect_expressions_from_expression operand
    | Cst.Expression.FieldAccess { receiver; _ } ->
        collect_expressions_from_expression receiver
    | Cst.Expression.Index { collection; index; _ } ->
        collect_expressions_from_expression collection
        @ collect_expressions_from_expression index
    | Cst.Expression.ObjectUpdate { fields; _ } ->
        fields
        |> List.concat_map (fun (field : Cst.record_expression_field) ->
               Option.to_list (field.value)
               |> List.concat_map collect_expressions_from_expression)
    | Cst.Expression.InstanceVariableAssign { value; _ } ->
        collect_expressions_from_expression value
    | Cst.Expression.Assign { target; value; _ } ->
        collect_expressions_from_expression target
        @ collect_expressions_from_expression value
    | Cst.Expression.Infix { left; right; _ } ->
        collect_expressions_from_expression left
        @ collect_expressions_from_expression right
    | Cst.Expression.Typed { expression; _ }
    | Cst.Expression.Polymorphic { expression; _ } ->
        collect_expressions_from_expression expression
    | Cst.Expression.Coerce { expression; _ } ->
        collect_expressions_from_expression expression
    | Cst.Expression.Sequence { left; right; _ } ->
        collect_expressions_from_expression left
        @ collect_expressions_from_expression right
    | Cst.Expression.Tuple { elements; _ }
    | Cst.Expression.List { elements; _ }
    | Cst.Expression.Array { elements; _ } ->
        elements |> List.concat_map collect_expressions_from_expression
    | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
        fields
        |> List.concat_map (fun (field : Cst.record_expression_field) ->
               Option.to_list (field.value)
               |> List.concat_map collect_expressions_from_expression)
    | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
        collect_expressions_from_expression base
        @
        (fields
        |> List.concat_map (fun (field : Cst.record_expression_field) ->
               Option.to_list (field.value)
               |> List.concat_map collect_expressions_from_expression))
    | Cst.Expression.LocalOpen { body; _ } ->
        collect_expressions_from_expression body
    | Cst.Expression.Fun { body; _ } ->
        collect_expressions_from_expression body
    | Cst.Expression.Function { cases; _ } ->
        cases |> List.concat_map collect_expressions_from_match_case
    | Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
        collect_expressions_from_expression binding.bound_value
        @ (and_bindings
          |> List.concat_map (fun ({ bound_value; _ } : Cst.binding_operator_binding) ->
                 collect_expressions_from_expression bound_value))
        @ collect_expressions_from_expression body
    | Cst.Expression.Let { bound_value; and_bindings; body; _ } ->
        collect_expressions_from_expression bound_value
        @ (and_bindings |> List.concat_map collect_expressions_from_let_binding)
        @ collect_expressions_from_expression body
    | Cst.Expression.Match { scrutinee; cases; _ } ->
        collect_expressions_from_expression scrutinee
        @ (cases |> List.concat_map collect_expressions_from_match_case)
    | Cst.Expression.Try { body; cases; _ } ->
        collect_expressions_from_expression body
        @ (cases |> List.concat_map collect_expressions_from_match_case)
    | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
        collect_expressions_from_expression condition
        @ collect_expressions_from_expression then_branch
        @
        (Option.to_list else_branch
        |> List.concat_map collect_expressions_from_expression)
    | Cst.Expression.Parenthesized { inner; _ } ->
        collect_expressions_from_expression inner
  in
  expr :: nested

and collect_expressions_from_let_binding binding =
  collect_expressions_from_expression (Cst.LetBinding.value binding)

and collect_expressions_from_match_case { guard; body; _ } =
  (Option.to_list guard |> List.concat_map collect_expressions_from_expression)
  @ collect_expressions_from_expression body

let collect_expressions_from_item = function
  | Cst.Item.TypeDeclaration _ | Cst.Item.TypeExtension _
  | Cst.Item.ModuleDeclaration _
  | Cst.Item.ModuleTypeDeclaration _ | Cst.Item.OpenStatement _
  | Cst.Item.ValueDeclaration _ | Cst.Item.ExternalDeclaration _
  | Cst.Item.IncludeStatement _ | Cst.Item.ExceptionDeclaration _
  | Cst.Item.ClassTypeDeclaration _ | Cst.Item.Attribute _
  | Cst.Item.Extension _ ->
      []
  | Cst.Item.LetBinding binding ->
      collect_expressions_from_let_binding binding
  | Cst.Item.Expression expr ->
      collect_expressions_from_expression expr
  | Cst.Item.ClassDeclaration { class_body; _ } ->
      collect_expressions_from_expression class_body

let rec items_from_node node =
  match Ceibo.Red.SyntaxNode.kind node with
  | Syntax_kind.TYPE_DECL -> (
      if is_type_extension_node node then
        match type_extension_from_node node with
        | Some decl -> [ Cst.Item.TypeExtension decl ]
        | None -> unsupported_item node
      else
        match type_declaration_from_node node with
        | Some decl -> [ Cst.Item.TypeDeclaration decl ]
        | None -> unsupported_item node)
  | Syntax_kind.TYPE_MUTUAL_DECL ->
      direct_non_trivia_nodes node |> List.concat_map items_from_node
  | Syntax_kind.LET_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:false node with
      | Some binding -> [ Cst.Item.LetBinding binding ]
      | None -> unsupported_item node)
  | Syntax_kind.LET_REC_BINDING -> (
      match let_binding_from_node ~is_recursive_binding:true node with
      | Some binding -> [ Cst.Item.LetBinding binding ]
      | None -> unsupported_item node)
  | Syntax_kind.LET_MUTUAL_DECL ->
      direct_non_trivia_nodes node
      |> List.filter (fun child ->
             let kind = Ceibo.Red.SyntaxNode.kind child in
             kind = Syntax_kind.LET_BINDING || kind = Syntax_kind.LET_REC_BINDING)
      |> List.concat_map items_from_node
  | Syntax_kind.CLASS_DECL -> (
      match class_declaration_from_node node with
      | Some decl -> [ Cst.Item.ClassDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.CLASS_TYPE_DECL -> (
      match class_type_declaration_from_node node with
      | Some decl -> [ Cst.Item.ClassTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_DECL -> (
      match module_declaration_from_node node with
      | Some decl -> [ Cst.Item.ModuleDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.MODULE_TYPE_DECL -> (
      match module_type_declaration_from_node node with
      | Some decl -> [ Cst.Item.ModuleTypeDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.OPEN_STMT -> (
      match open_statement_from_node node with
      | Some stmt -> [ Cst.Item.OpenStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.VAL_DECL -> (
      match value_declaration_from_node node with
      | Some decl -> [ Cst.Item.ValueDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.EXTERNAL_DECL -> (
      match external_declaration_from_node node with
      | Some decl -> [ Cst.Item.ExternalDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.INCLUDE_STMT -> (
      match include_statement_from_node node with
      | Some stmt -> [ Cst.Item.IncludeStatement stmt ]
      | None -> unsupported_item node)
  | Syntax_kind.EXCEPTION_DECL -> (
      match exception_declaration_from_node node with
      | Some decl -> [ Cst.Item.ExceptionDeclaration decl ]
      | None -> unsupported_item node)
  | Syntax_kind.ATTRIBUTE_EXPR ->
      [ Cst.Item.Attribute (attribute_from_node node) ]
  | Syntax_kind.EXTENSION_EXPR ->
      [ Cst.Item.Extension (extension_from_node node) ]
  | Syntax_kind.SEQUENCE_EXPR -> (
      match direct_non_trivia_nodes node with
      | only_expr :: [] -> (
          [ Cst.Item.Expression (expression_from_node only_expr) ])
      | _ -> (
          [ Cst.Item.Expression (expression_from_node node) ]))
  | _ -> (
      [ Cst.Item.Expression (expression_from_node node) ])

let build_source_file_body tree =
  let root = Ceibo.Red.new_root tree in
  let file_items =
    direct_non_trivia_nodes root
    |> List.concat_map items_from_node
  in
  let file_let_bindings = collect_let_bindings root in
  (root, file_items, file_let_bindings)

let rec validate_pattern ~context = function
  | Cst.Pattern.Identifier _ | Cst.Pattern.Wildcard _ | Cst.Pattern.Literal _
  | Cst.Pattern.Extension _ ->
      ()
  | Cst.Pattern.Attribute { pattern; _ } ->
      validate_pattern ~context:("pattern.attribute.pattern" :: context) pattern
  | Cst.Pattern.Lazy { pattern; _ } ->
      validate_pattern ~context:("pattern.lazy" :: context) pattern
  | Cst.Pattern.Exception { pattern; _ } ->
      validate_pattern ~context:("pattern.exception" :: context) pattern
  | Cst.Pattern.Range _ | Cst.Pattern.Operator _
  | Cst.Pattern.PolyVariantInherit _ ->
      ()
  | Cst.Pattern.FirstClassModule { module_type; _ } ->
      Option.iter
        (validate_module_type ~context:("pattern.first_class_module.type" :: context))
        module_type
  | Cst.Pattern.PolyVariant { payload; _ } ->
      Option.iter
        (validate_pattern ~context:("pattern.poly_variant.payload" :: context))
        payload
  | Cst.Pattern.Constructor { arguments; _ } ->
      List.iteri
        (fun index argument ->
          validate_pattern
            ~context:
              (("pattern.constructor.argument[" ^ Int.to_string index ^ "]")
             :: context)
            argument)
        arguments
  | Cst.Pattern.Tuple { elements; _ }
  | Cst.Pattern.List { elements; _ }
  | Cst.Pattern.Array { elements; _ }
  | Cst.Pattern.Or { alternatives = elements; _ } ->
      List.iteri
        (fun index pattern ->
          validate_pattern
            ~context:(("pattern.element[" ^ Int.to_string index ^ "]") :: context)
            pattern)
        elements
  | Cst.Pattern.Record { fields; _ } ->
      List.iteri
        (fun index (field : Cst.record_pattern_field) ->
          Option.iter
            (validate_pattern
               ~context:
                 (("pattern.record.field[" ^ Int.to_string index ^ "].pattern")
                 :: context))
            field.pattern)
        fields
  | Cst.Pattern.Cons { head; tail; _ } ->
      validate_pattern ~context:("pattern.cons.head" :: context) head;
      validate_pattern ~context:("pattern.cons.tail" :: context) tail
  | Cst.Pattern.Alias { pattern; _ } ->
      validate_pattern ~context:("pattern.alias.pattern" :: context) pattern
  | Cst.Pattern.Typed { pattern; type_; _ } ->
      validate_pattern ~context:("pattern.typed.pattern" :: context) pattern;
      validate_core_type ~context:("pattern.typed.type" :: context) type_
  | Cst.Pattern.Effect { effect_pattern; continuation; _ } ->
      validate_pattern ~context:("pattern.effect.effect" :: context)
        effect_pattern;
      validate_pattern ~context:("pattern.effect.continuation" :: context)
        continuation
  | Cst.Pattern.LocalOpen { pattern; _ } ->
      validate_pattern ~context:("pattern.local_open.pattern" :: context) pattern
  | Cst.Pattern.Parenthesized { inner; _ } ->
      validate_pattern ~context:("pattern.parenthesized" :: context) inner

and validate_parameter ~context = function
  | Cst.Parameter.Positional _ | Cst.Parameter.Labeled _
  | Cst.Parameter.Optional _ | Cst.Parameter.LocallyAbstract _ ->
      ()

and validate_module_type ~context = function
  | Cst.ModuleType.Path _ | Cst.ModuleType.TypeOf _ | Cst.ModuleType.Signature _
  | Cst.ModuleType.Extension _ ->
      ()
  | Cst.ModuleType.Parenthesized { inner; _ } ->
      validate_module_type ~context:("module_type.parenthesized" :: context) inner
  | Cst.ModuleType.Attribute { module_type; _ } ->
      validate_module_type ~context:("module_type.attribute" :: context) module_type
  | Cst.ModuleType.With { base; constraints; _ } ->
      validate_module_type ~context:("module_type.with.base" :: context) base;
      List.iteri
        (fun index ({ replacement_type; _ } : Cst.module_type_constraint) ->
          validate_core_type
            ~context:
              (("module_type.with.constraint[" ^ Int.to_string index ^ "].type")
              :: context)
            replacement_type)
        constraints
  | Cst.ModuleType.Functor { parameters; result; _ } ->
      List.iteri
        (fun index ({ module_type; _ } : Cst.functor_parameter) ->
          validate_module_type
            ~context:
              (("module_type.functor.parameter[" ^ Int.to_string index ^ "]")
              :: context)
            module_type)
        parameters;
      validate_module_type ~context:("module_type.functor.result" :: context) result

and validate_core_type ~context = function
  | Cst.CoreType.Wildcard _
  | Cst.CoreType.Var _
  | Cst.CoreType.Extension _ ->
      ()
  | Cst.CoreType.Poly { body; _ } ->
      validate_core_type ~context:("core_type.poly.body" :: context) body
  | Cst.CoreType.FirstClassModule { module_type; _ } ->
      validate_module_type ~context:("core_type.first_class_module" :: context)
        module_type
  | Cst.CoreType.Constr { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.constr.arg[" ^ Int.to_string index ^ "]") :: context)
            type_)
        arguments
  | Cst.CoreType.Class { arguments; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.class.arg[" ^ Int.to_string index ^ "]") :: context)
            type_)
        arguments
  | Cst.CoreType.Alias { type_; _ } ->
      validate_core_type ~context:("core_type.alias.type" :: context) type_
  | Cst.CoreType.Attribute { type_; _ } ->
      validate_core_type ~context:("core_type.attribute.type" :: context) type_
  | Cst.CoreType.Arrow { parameter_type; result_type; _ } ->
      validate_core_type
        ~context:("core_type.arrow.parameter" :: context) parameter_type;
      validate_core_type ~context:("core_type.arrow.result" :: context) result_type
  | Cst.CoreType.Tuple { elements; _ } ->
      List.iteri
        (fun index type_ ->
          validate_core_type
            ~context:(("core_type.tuple.element[" ^ Int.to_string index ^ "]") :: context)
            type_)
        elements
  | Cst.CoreType.Parenthesized { inner; _ } ->
      validate_core_type ~context:("core_type.parenthesized" :: context) inner
  | Cst.CoreType.LocalOpen { type_; _ } ->
      validate_core_type ~context:("core_type.local_open.type" :: context) type_
  | Cst.CoreType.PolyVariant poly_variant ->
      validate_poly_variant ~context:("core_type.poly_variant" :: context)
        poly_variant
  | Cst.CoreType.Record { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.record_type_field) ->
          validate_core_type
            ~context:
              (("core_type.record.field[" ^ Int.to_string index ^ "].type") :: context)
            field_type)
        fields
  | Cst.CoreType.Object { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.object_type_field) ->
          validate_core_type
            ~context:
              (("core_type.object.field[" ^ Int.to_string index ^ "].type") :: context)
            field_type)
        fields

and validate_row_field ~context index = function
  | Cst.RowField.Tag tag ->
      Option.iter
        (validate_core_type
           ~context:
             (("row_field[" ^ Int.to_string index ^ "].tag.payload") :: context))
        (Cst.PolyVariantTag.payload_type tag)
  | Cst.RowField.Inherit { type_; _ } ->
      validate_core_type
        ~context:(("row_field[" ^ Int.to_string index ^ "].inherit") :: context)
        type_

and validate_poly_variant ~context poly_variant =
  Cst.PolyVariant.fields poly_variant
  |> List.iteri (validate_row_field ~context)

and validate_apply_argument ~context = function
  | Cst.Positional expr ->
      validate_expression ~context:("apply_argument.positional" :: context) expr
  | Cst.Labeled { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.labeled.value" :: context))
        value
  | Cst.Optional { value; _ } ->
      Option.iter
        (validate_expression ~context:("apply_argument.optional.value" :: context))
        value

and validate_object_member ~context = function
  | Cst.Method { body; type_; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.method.body" :: context))
        body;
      Option.iter
        (validate_core_type ~context:("object_member.method.type" :: context))
        type_
  | Cst.Value { value; type_; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.value.value" :: context))
        value;
      Option.iter
        (validate_core_type ~context:("object_member.value.type" :: context))
        type_
  | Cst.Inherit { expression; _ } ->
      validate_expression ~context:("object_member.inherit.expression" :: context)
        expression
  | Cst.Initializer { body; _ } ->
      Option.iter
        (validate_expression ~context:("object_member.initializer.body" :: context))
        body

and validate_expression ~context = function
  | Cst.Expression.Path _ | Cst.Expression.Operator _ | Cst.Expression.Literal _
  | Cst.Expression.Unreachable _ | Cst.Expression.Attribute _
  | Cst.Expression.Extension _ ->
      ()
  | Cst.Expression.FirstClassModule { module_type; _ } ->
      Option.iter
        (validate_module_type
           ~context:("expression.first_class_module.type" :: context))
        module_type
  | Cst.Expression.Object { self_pattern; members; _ } ->
      Option.iter
        (validate_pattern ~context:("expression.object.self_pattern" :: context))
        self_pattern;
      List.iteri
        (fun index member ->
          validate_object_member
            ~context:(("expression.object.member[" ^ Int.to_string index ^ "]") :: context)
            member)
        members
  | Cst.Expression.LetModule { body; _ } ->
      validate_expression ~context:("expression.let_module.body" :: context) body
  | Cst.Expression.LetException { body; _ } ->
      validate_expression ~context:("expression.let_exception.body" :: context)
        body
  | Cst.Expression.PolyVariant { payload; _ } ->
      Option.iter
        (validate_expression ~context:("expression.poly_variant.payload" :: context))
        payload
  | Cst.Expression.Assert { asserted; _ } ->
      validate_expression ~context:("expression.assert.asserted" :: context)
        asserted
  | Cst.Expression.Lazy { body; _ } ->
      validate_expression ~context:("expression.lazy.body" :: context) body
  | Cst.Expression.While { condition; body; _ } ->
      validate_expression ~context:("expression.while.condition" :: context)
        condition;
      validate_expression ~context:("expression.while.body" :: context) body
  | Cst.Expression.For { start_expr; end_expr; body; _ } ->
      validate_expression ~context:("expression.for.start" :: context) start_expr;
      validate_expression ~context:("expression.for.end" :: context) end_expr;
      validate_expression ~context:("expression.for.body" :: context) body
  | Cst.Expression.Apply { callee; argument; _ } ->
      validate_expression ~context:("expression.apply.callee" :: context) callee;
      validate_apply_argument ~context:("expression.apply.argument" :: context)
        argument
  | Cst.Expression.MethodCall { receiver; _ } ->
      validate_expression ~context:("expression.method_call.receiver" :: context)
        receiver
  | Cst.Expression.New _ ->
      ()
  | Cst.Expression.Prefix { operand; _ } ->
      validate_expression ~context:("expression.prefix.operand" :: context)
        operand
  | Cst.Expression.FieldAccess { receiver; _ } ->
      validate_expression ~context:("expression.field_access.receiver" :: context)
        receiver
  | Cst.Expression.Index { collection; index; _ } ->
      validate_expression ~context:("expression.index.collection" :: context)
        collection;
      validate_expression ~context:("expression.index.index" :: context) index
  | Cst.Expression.ObjectUpdate { fields; _ } ->
      List.iteri
        (fun index (field : Cst.record_expression_field) ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.object_update.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            field.value)
        fields
  | Cst.Expression.InstanceVariableAssign { value; _ } ->
      validate_expression
        ~context:("expression.instance_variable_assign.value" :: context)
        value
  | Cst.Expression.Assign { target; value; _ } ->
      validate_expression ~context:("expression.assign.target" :: context) target;
      validate_expression ~context:("expression.assign.value" :: context) value
  | Cst.Expression.Infix { left; right; _ } ->
      validate_expression ~context:("expression.infix.left" :: context) left;
      validate_expression ~context:("expression.infix.right" :: context) right
  | Cst.Expression.Typed { expression; type_; _ } ->
      validate_expression ~context:("expression.typed.expression" :: context)
        expression;
      validate_core_type ~context:("expression.typed.type" :: context) type_
  | Cst.Expression.Polymorphic { expression; type_; _ } ->
      validate_expression ~context:("expression.polymorphic.expression" :: context)
        expression;
      validate_core_type ~context:("expression.polymorphic.type" :: context)
        type_
  | Cst.Expression.Coerce { expression; from_type; to_type; _ } ->
      validate_expression ~context:("expression.coerce.expression" :: context)
        expression;
      Option.iter
        (validate_core_type ~context:("expression.coerce.from_type" :: context))
        from_type;
      validate_core_type ~context:("expression.coerce.to_type" :: context) to_type
  | Cst.Expression.Sequence { left; right; _ } ->
      validate_expression ~context:("expression.sequence.left" :: context) left;
      validate_expression ~context:("expression.sequence.right" :: context) right
  | Cst.Expression.Tuple { elements; _ }
  | Cst.Expression.List { elements; _ }
  | Cst.Expression.Array { elements; _ } ->
      List.iteri
        (fun index expr ->
          validate_expression
            ~context:(("expression.element[" ^ Int.to_string index ^ "]") :: context)
            expr)
        elements
  | Cst.Expression.Record (Cst.RecordExpression.Literal { fields; _ }) ->
      List.iteri
        (fun index (field : Cst.record_expression_field) ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.record.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            field.value)
        fields
  | Cst.Expression.Record (Cst.RecordExpression.Update { base; fields; _ }) ->
      validate_expression ~context:("expression.record.base" :: context) base;
      List.iteri
        (fun index (field : Cst.record_expression_field) ->
          Option.iter
            (validate_expression
               ~context:
                 (("expression.record.field[" ^ Int.to_string index ^ "].value")
                 :: context))
            field.value)
        fields
  | Cst.Expression.LocalOpen { body; _ } ->
      validate_expression ~context:("expression.local_open.body" :: context) body
  | Cst.Expression.Fun { parameters; body; _ } ->
      List.iteri
        (fun index parameter ->
          validate_parameter
            ~context:(("expression.fun.parameter[" ^ Int.to_string index ^ "]") :: context)
            parameter)
        parameters;
      validate_expression ~context:("expression.fun.body" :: context) body
  | Cst.Expression.Function { cases; _ } ->
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.function.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
      validate_pattern ~context:("expression.let_operator.binding.pattern" :: context)
        binding.binding_pattern;
      validate_expression
        ~context:("expression.let_operator.binding.value" :: context)
        binding.bound_value;
      List.iteri
        (fun index ({ binding_pattern; bound_value; _ } : Cst.binding_operator_binding) ->
          validate_pattern
            ~context:
              (("expression.let_operator.and_bindings[" ^ Int.to_string index
               ^ "].pattern")
              :: context)
            binding_pattern;
          validate_expression
            ~context:
              (("expression.let_operator.and_bindings[" ^ Int.to_string index
               ^ "].value")
              :: context)
            bound_value)
        and_bindings;
      validate_expression ~context:("expression.let_operator.body" :: context)
        body
  | Cst.Expression.Let { binding_pattern; bound_value; and_bindings; body; _ } ->
      validate_pattern ~context:("expression.let.pattern" :: context) binding_pattern;
      validate_expression ~context:("expression.let.bound_value" :: context) bound_value;
      List.iteri
        (fun index binding ->
          validate_pattern
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].pattern")
              :: context)
            (Cst.LetBinding.binding_pattern binding);
          validate_expression
            ~context:
              (("expression.let.and_bindings[" ^ Int.to_string index ^ "].value")
              :: context)
            (Cst.LetBinding.value binding))
        and_bindings;
      validate_expression ~context:("expression.let.body" :: context) body
  | Cst.Expression.Match { scrutinee; cases; _ } ->
      validate_expression ~context:("expression.match.scrutinee" :: context) scrutinee;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.match.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.Try { body; cases; _ } ->
      validate_expression ~context:("expression.try.body" :: context) body;
      List.iteri
        (fun index case ->
          validate_match_case
            ~context:(("expression.try.case[" ^ Int.to_string index ^ "]") :: context)
            case)
        cases
  | Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      validate_expression ~context:("expression.if.condition" :: context) condition;
      validate_expression ~context:("expression.if.then_branch" :: context) then_branch;
      Option.iter
        (validate_expression ~context:("expression.if.else_branch" :: context))
        else_branch
  | Cst.Expression.Parenthesized { inner; _ } ->
      validate_expression ~context:("expression.parenthesized" :: context) inner

and validate_match_case ~context ({ pattern; guard; body; _ } : Cst.match_case) =
  validate_pattern ~context:("match_case.pattern" :: context) pattern;
  Option.iter (validate_expression ~context:("match_case.guard" :: context)) guard;
  validate_expression ~context:("match_case.body" :: context) body

let validate_type_definition ~context = function
  | Cst.TypeDefinition.Abstract -> ()
  | Cst.TypeDefinition.Alias { manifest; _ } ->
      validate_core_type ~context:("type_definition.alias" :: context) manifest
  | Cst.TypeDefinition.Extensible _ -> ()
  | Cst.TypeDefinition.FirstClassModule { module_type; _ } ->
      validate_module_type
        ~context:("type_definition.first_class_module" :: context)
        module_type
  | Cst.TypeDefinition.Object { fields; _ } ->
      List.iteri
        (fun index ({ field_type; _ } : Cst.object_type_field) ->
          validate_core_type
            ~context:
              (("type_definition.object.field[" ^ Int.to_string index ^ "].type")
              :: context)
            field_type)
        fields
  | Cst.TypeDefinition.Record fields ->
      List.iteri
        (fun index field ->
          validate_core_type
            ~context:
              (("type_definition.record.field[" ^ Int.to_string index ^ "].type")
              :: context)
            (Cst.RecordField.field_type field))
        fields
  | Cst.TypeDefinition.Variant constructors ->
      List.iteri
        (fun index constructor ->
          Option.iter
            (validate_core_type
               ~context:
                 (("type_definition.variant.constructor[" ^ Int.to_string index
                  ^ "].payload")
                 :: context))
            (Cst.VariantConstructor.payload_type constructor))
        constructors
  | Cst.TypeDefinition.PolyVariant poly_variant ->
      validate_poly_variant ~context:("type_definition.poly_variant" :: context)
        poly_variant
  | Cst.TypeDefinition.Other syntax_node ->
      bail ~message:"unsupported type definition shape during Ceibo -> CST lifting"
        ~syntax_node ~context

let validate_item ~context = function
  | Cst.Item.TypeDeclaration { type_definition; _ } ->
      validate_type_definition ~context:("item.type_declaration" :: context)
        type_definition
  | Cst.Item.TypeExtension { constructors; _ } ->
      List.iteri
        (fun index constructor ->
          Option.iter
            (validate_core_type
               ~context:
                 (("item.type_extension.constructor[" ^ Int.to_string index
                  ^ "].payload")
                 :: context))
            (Cst.VariantConstructor.payload_type constructor))
        constructors
  | Cst.Item.LetBinding { binding_pattern; value; _ } ->
      validate_pattern ~context:("item.let_binding.pattern" :: context)
        binding_pattern;
      validate_expression ~context:("item.let_binding.value" :: context) value
  | Cst.Item.Expression expr ->
      validate_expression ~context:("item.expression" :: context) expr
  | Cst.Item.ClassDeclaration { class_body; _ } ->
      validate_expression ~context:("item.class_declaration.body" :: context)
        class_body
  | Cst.Item.ClassTypeDeclaration _ ->
      ()
  | Cst.Item.Attribute _ | Cst.Item.Extension _ ->
      ()
  | Cst.Item.ValueDeclaration { type_; _ } ->
      validate_core_type ~context:("item.value_declaration.type" :: context) type_
  | Cst.Item.ExternalDeclaration { type_; _ } ->
      validate_core_type ~context:("item.external_declaration.type" :: context)
        type_
  | Cst.Item.ModuleTypeDeclaration { module_type; _ } ->
      Option.iter
        (validate_module_type ~context:("item.module_type_declaration" :: context))
        module_type
  | Cst.Item.ModuleDeclaration _
  | Cst.Item.OpenStatement _ | Cst.Item.IncludeStatement _
  | Cst.Item.ExceptionDeclaration _ ->
      ()

let validate_source_file source_file =
  List.iteri
    (fun index item ->
      validate_item ~context:[ "source_file.items[" ^ Int.to_string index ^ "]" ] item)
    (Cst.SourceFile.items source_file);
  List.iteri
    (fun index binding ->
      validate_expression
        ~context:[ "source_file.let_bindings[" ^ Int.to_string index ^ "].value" ]
        (Cst.LetBinding.value binding))
    (Cst.SourceFile.let_bindings source_file);
  List.iteri
    (fun index expr ->
      validate_expression
        ~context:[ "source_file.expressions[" ^ Int.to_string index ^ "]" ]
        expr)
    (Cst.SourceFile.expressions source_file)

let lift ~kind tree =
  let syntax_node, items, let_bindings = build_source_file_body tree in
  let expressions = items |> List.concat_map collect_expressions_from_item in
  let cst =
    match kind with
    | `Implementation ->
        Cst.Implementation { syntax_node; items; let_bindings; expressions }
    | `Interface ->
        Cst.Interface { syntax_node; items; let_bindings; expressions }
  in
  validate_source_file cst;
  cst

let create_from_ceibo ~kind tree =
  match lift ~kind tree with
  | cst -> Ok cst
  | exception Bail error -> Error error
