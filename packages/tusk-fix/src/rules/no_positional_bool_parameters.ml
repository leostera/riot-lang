open Std
open Std.Collections

let is_trivia kind =
  let open Syn.SyntaxKind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

let rule_id = "no-positional-bool-parameters"
let rule_description =
  "Positional bool parameters should be replaced with a named parameter or an enum"

let rule_explain =
  {|
Positional booleans are hard on readers because `retry true request` does not explain
what the `true` means. Every call site has to remember whether the flag means
"enabled", "disabled", "retry", "strict", or something else entirely.

The first fix is usually a named parameter. `retry ~enabled:true request` or
`render ~enabled user` makes the call site self-explanatory. If the boolean is really
standing in for a small state machine, a tiny enum often tells the story better than a
flag ever could.

This rule fires because the ambiguity shows up at every caller, not just in the
definition where the author already knows what the boolean means.
|}

let rec direct_non_trivia_nodes (node : Syn.Cst.syntax_node) =
  Syn.Ceibo.Red.SyntaxNode.children node
  |> Array.to_list
  |> List.filter_map (function
       | Syn.Ceibo.Red.Node child
         when not (is_trivia (Syn.Ceibo.Red.SyntaxNode.kind child)) ->
           Some child
       | _ ->
           None)

let is_type_syntax_kind = function
  | Syn.SyntaxKind.TYPE_VAR
  | Syn.SyntaxKind.TYPE_CONSTR
  | Syn.SyntaxKind.TYPE_RECORD
  | Syn.SyntaxKind.TYPE_TUPLE
  | Syn.SyntaxKind.TYPE_ALIAS
  | Syn.SyntaxKind.TYPE_ARROW
  | Syn.SyntaxKind.TYPE_PAREN
  | Syn.SyntaxKind.TYPE_POLY_VARIANT
  | Syn.SyntaxKind.POLY_TYPE
  | Syn.SyntaxKind.FIRST_CLASS_MODULE_TYPE
  | Syn.SyntaxKind.OBJECT_TYPE
  | Syn.SyntaxKind.LOCAL_OPEN_TYPE
  | Syn.SyntaxKind.ATTRIBUTE_EXPR
  | Syn.SyntaxKind.EXTENSION_EXPR ->
      true
  | _ ->
      false

let rec first_core_type_node (node : Syn.Cst.syntax_node) =
  match Syn.Ceibo.Red.SyntaxNode.kind node with
  | Syn.SyntaxKind.ATTRIBUTE_EXPR -> (
      match direct_non_trivia_nodes node |> List.find_map first_core_type_node with
      | Some child ->
          Some child
      | None when is_type_syntax_kind (Syn.Ceibo.Red.SyntaxNode.kind node) ->
          Some node
      | None ->
          None)
  | kind when is_type_syntax_kind kind ->
      Some node
  | _ ->
      direct_non_trivia_nodes node |> List.find_map first_core_type_node

let rec is_bool_type_node (node : Syn.Cst.syntax_node) =
  match Syn.Ceibo.Red.SyntaxNode.kind node with
  | Syn.SyntaxKind.ATTRIBUTE_EXPR
  | Syn.SyntaxKind.TYPE_PAREN
  | Syn.SyntaxKind.TYPE_ALIAS
  | Syn.SyntaxKind.LOCAL_OPEN_TYPE ->
      direct_non_trivia_nodes node |> List.exists is_bool_type_node
  | Syn.SyntaxKind.TYPE_CONSTR ->
      let token_texts : string list =
        Syn.Ceibo.Red.SyntaxNode.children node
        |> Array.to_list
        |> List.filter_map (function
             | Syn.Ceibo.Red.Token syntax_token
               when not (is_trivia (Syn.Ceibo.Red.SyntaxToken.kind syntax_token)) ->
                 Some (Syn.Ceibo.Red.SyntaxToken.text syntax_token)
             | _ ->
                 None)
      in
      (match token_texts with
      | [ "bool" ] ->
          true
      | _ ->
          false)
  | _ ->
      false

let rec strip_type_wrappers = function
  | Syn.Cst.CoreType.Parenthesized { inner; _ }
  | Syn.Cst.CoreType.Attribute { type_ = inner; _ }
  | Syn.Cst.CoreType.Alias { type_ = inner; _ } ->
      strip_type_wrappers inner
  | type_ ->
      type_

let rec is_bool_core_type type_ =
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Constr { constructor_path; arguments = []; _ } -> (
      match Syn.Cst.Ident.name constructor_path with
      | Some "bool" ->
          true
      | _ ->
          false)
  | Syn.Cst.CoreType.LocalOpen { type_; _ } ->
      is_bool_core_type type_
  | _ ->
      false

let parameter_inline_type parameter =
  match parameter with
  | Syn.Cst.Parameter.Positional { syntax_node; _ } ->
      first_core_type_node syntax_node
  | Syn.Cst.Parameter.Labeled _
  | Syn.Cst.Parameter.Optional _
  | Syn.Cst.Parameter.LocallyAbstract _ ->
      None

let rec arrow_parameters type_ =
  match strip_type_wrappers type_ with
  | Syn.Cst.CoreType.Poly { body; _ } ->
      arrow_parameters body
  | Syn.Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      (label, parameter_type) :: arrow_parameters result_type
  | _ ->
      []

let typed_value_function_type binding =
  let rec go = function
    | Syn.Cst.Expression.TypeAscription { expression; kind = Syn.Cst.Type type_; _ }
    | Syn.Cst.Expression.Polymorphic { expression; type_; _ } ->
        if Syn.Cst.LetBinding.is_function binding then
          Some (expression, type_)
        else
          go expression
    | Syn.Cst.Expression.Parenthesized { inner; _ } ->
        go inner
    | _ ->
        None
  in
  go (Syn.Cst.LetBinding.value binding)

let make_diagnostic ~span =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span
    ~suggestion:
      "Replace this positional bool with a named parameter like ~enabled, or introduce an explicit enum"
    ()

let diagnostic_for_parameter parameter =
  match parameter_inline_type parameter with
  | Some type_node when is_bool_type_node type_node ->
      Some
        (make_diagnostic
           ~span:
             (Syn.Ceibo.Red.SyntaxNode.span
                (Syn.Cst.Parameter.syntax_node parameter)))
  | _ ->
      None

let diagnostics_for_binding binding =
  let inline_parameter_diagnostics =
    Syn.Cst.LetBinding.parameters binding
    |> List.filter_map diagnostic_for_parameter
  in
  let typed_arrow_diagnostic =
    match typed_value_function_type binding with
    | Some (_expression, type_) ->
        let positional_parameters =
          Syn.Cst.LetBinding.parameters binding
          |> List.filter_map (function
               | Syn.Cst.Parameter.Positional _ as parameter -> Some parameter
               | _ -> None)
        in
        let unlabeled_arrow_types =
          arrow_parameters type_
          |> List.filter_map (function
               | None, parameter_type -> Some parameter_type
               | Some _, _ -> None)
        in
        let rec first_bool_parameter parameters arrow_types =
          match parameters, arrow_types with
          | parameter :: rest_parameters, parameter_type :: rest_arrow_types ->
              if is_bool_core_type parameter_type then
                Some
                  (make_diagnostic
                     ~span:
                       (Syn.Ceibo.Red.SyntaxNode.span
                          (Syn.Cst.Parameter.syntax_node parameter)))
              else
                first_bool_parameter rest_parameters rest_arrow_types
          | _, _ ->
              None
        in
        first_bool_parameter positional_parameters unlabeled_arrow_types
    | None ->
        None
  in
  inline_parameter_diagnostics
  @ (typed_arrow_diagnostic |> Option.to_list)

let diagnostics_for_value_declaration
    ({ name_token; type_; _ } : Syn.Cst.value_declaration) =
  arrow_parameters type_
  |> List.find_map (function
       | None, parameter_type when is_bool_core_type parameter_type ->
           Some (make_diagnostic ~span:(Syn.Cst.Token.span name_token))
       | _ ->
           None)
  |> Option.to_list

let diagnostics_for_external_declaration
    ({ name_token; type_; _ } : Syn.Cst.external_declaration) =
  arrow_parameters type_
  |> List.find_map (function
       | None, parameter_type when is_bool_core_type parameter_type ->
           Some (make_diagnostic ~span:(Syn.Cst.Token.span name_token))
       | _ ->
           None)
  |> Option.to_list

let check_tree (ctx : Rule.context) _red_root =
  let source_file = ctx.cst in
      let structure_diagnostics =
        Syn.Cst.SourceFile.structure_items source_file
        |> Option.unwrap_or ~default:[]
        |> List.concat_map (function
             | Syn.Cst.StructureItem.LetBinding binding
               when Syn.Cst.LetBinding.is_function binding ->
                 diagnostics_for_binding binding
             | Syn.Cst.StructureItem.ExternalDeclaration decl ->
                 diagnostics_for_external_declaration decl
             | _ ->
                 [])
      in
      let signature_diagnostics =
        Syn.Cst.SourceFile.signature_items source_file
        |> Option.unwrap_or ~default:[]
        |> List.concat_map (function
             | Syn.Cst.SignatureItem.ValueDeclaration decl ->
                 diagnostics_for_value_declaration decl
             | _ ->
                 [])
      in
      structure_diagnostics @ signature_diagnostics

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
