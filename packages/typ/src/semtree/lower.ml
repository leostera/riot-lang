open Std
open Syn

type state = {
  kind: Semantic_tree.file_kind;
  mutable next_binding_stamp: int;
  mutable items: Semantic_tree.item list;
  mutable exports: Semantic_tree.item list;
  mutable diagnostics: Diagnostics.Diagnostic.t list;
}

let make_state = fun kind ->
  {
    kind;
    next_binding_stamp = 0;
    items = [];
    exports = [];
    diagnostics = [];
  }

let span_of_syntax_node = fun syntax_node -> Cst.token_body_span syntax_node

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- stamp + 1;
  Model.Binding_id.local ~stamp ~name:(Model.Surface_path.of_name name)

let path_of_ident = fun ident -> ident |> Cst.Ident.segments |> List.map ~fn:Cst.Token.text

let name_of_name_tokens = fun tokens -> tokens |> List.map ~fn:Cst.Token.text |> String.concat ""

let lower_arrow_label = fun (label: Cst.arrow_label) ->
  { Semantic_tree.name = Cst.ArrowLabel.name label; optional_ = Cst.ArrowLabel.is_optional label }

let push_diagnostic = fun state diagnostic -> state.diagnostics <- diagnostic :: state.diagnostics

let push_unsupported_type = fun state syntax_node summary ->
  push_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedType { span = span_of_syntax_node syntax_node; summary })

let rec lower_type_expr = fun state (type_: Cst.core_type) ->
  match type_ with
  | Cst.CoreType.Wildcard _ ->
      Semantic_tree.AnyType
  | Cst.CoreType.Var { name_token; _ } ->
      Semantic_tree.TypeVar (Cst.Token.text name_token)
  | Cst.CoreType.Constr { constructor_path; arguments; _ } ->
      Semantic_tree.TypeConstr {
        path = path_of_ident constructor_path;
        arguments = List.map arguments ~fn:(lower_type_expr state)
      }
  | Cst.CoreType.Class _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "class";
      Semantic_tree.TypeUnsupported "class"
  | Cst.CoreType.Alias { type_; name_token; _ } ->
      Semantic_tree.TypeAlias {
        type_ = lower_type_expr state type_;
        name = Cst.Token.text name_token
      }
  | Cst.CoreType.Attribute { type_; _ } ->
      lower_type_expr state type_
  | Cst.CoreType.Extension _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "extension";
      Semantic_tree.TypeUnsupported "extension"
  | Cst.CoreType.Poly { binders; body; _ } ->
      Semantic_tree.TypePoly {
        binders = List.map binders ~fn:Cst.TypeBinder.name;
        body = lower_type_expr state body
      }
  | Cst.CoreType.Arrow { label; parameter_type; result_type; _ } ->
      Semantic_tree.TypeArrow {
        label = Option.map label ~fn:lower_arrow_label;
        parameter = lower_type_expr state parameter_type;
        result = lower_type_expr state result_type
      }
  | Cst.CoreType.Tuple { elements; _ } ->
      Semantic_tree.TypeTuple (List.map elements ~fn:(lower_type_expr state))
  | Cst.CoreType.Parenthesized { inner; _ } ->
      lower_type_expr state inner
  | Cst.CoreType.PolyVariant _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "polyvariant";
      Semantic_tree.TypeUnsupported "polyvariant"
  | Cst.CoreType.Record _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "record";
      Semantic_tree.TypeUnsupported "record"
  | Cst.CoreType.FirstClassModule _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "first_class_module";
      Semantic_tree.TypeUnsupported "first_class_module"
  | Cst.CoreType.Object _ ->
      let syntax_node = Cst.CoreType.syntax_node type_ in
      push_unsupported_type state syntax_node "object";
      Semantic_tree.TypeUnsupported "object"

let rec annotation_of_expression = fun state (expression: Cst.expression) ->
  match expression with
  | Cst.Expression.TypeAscription { kind=Cst.Type { type_; _ }; _ } -> Some (lower_type_expr state type_)
  | Cst.Expression.TypeAscription { kind=Cst.Coerce { type_; _ }; _ } -> Some (lower_type_expr
    state
    type_)
  | Cst.Expression.TypeAscription { kind=Cst.ConstraintCoerce { to_type; _ }; _ } -> Some (lower_type_expr
    state
    to_type)
  | Cst.Expression.Polymorphic { type_; _ } -> Some (lower_type_expr state type_)
  | Cst.Expression.Parenthesized { inner; _ } -> annotation_of_expression state inner
  | _ -> None

let rec path_of_module_expression = function
  | Cst.ModuleExpression.Path ident -> Some (path_of_ident ident)
  | Cst.ModuleExpression.Parenthesized { inner; _ } -> path_of_module_expression inner
  | Cst.ModuleExpression.Constraint { module_expression; _ } -> path_of_module_expression module_expression
  | Cst.ModuleExpression.Attribute { module_expression; _ } -> path_of_module_expression module_expression
  | _ -> None

let rec path_of_module_type = function
  | Cst.ModuleType.Path ident -> Some (path_of_ident ident)
  | Cst.ModuleType.TypeOf { module_path; _ } -> Some (path_of_ident module_path)
  | Cst.ModuleType.Parenthesized { inner; _ } -> path_of_module_type inner
  | Cst.ModuleType.With { base; _ } -> path_of_module_type base
  | Cst.ModuleType.Attribute { module_type; _ } -> path_of_module_type module_type
  | _ -> None

let is_export = function
  | Semantic_tree.TypeDeclaration _ -> true
  | Semantic_tree.ValueDeclaration _ -> true
  | Semantic_tree.ModuleDeclaration _ -> true
  | Semantic_tree.ModuleTypeDeclaration _ -> true
  | Semantic_tree.IncludeStatement _ -> true
  | Semantic_tree.ExceptionDeclaration _ -> true
  | Semantic_tree.ExternalDeclaration _ -> true
  | Semantic_tree.OpenStatement _ -> false
  | Semantic_tree.Expression _ -> false
  | Semantic_tree.Unsupported _ -> false

let push_item = fun state item ->
  state.items <- item :: state.items;
  if is_export item then
    state.exports <- item :: state.exports

let push_unsupported = fun state syntax_node summary ->
  push_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = span_of_syntax_node syntax_node;
      kind = Cst.syntax_kind syntax_node;
      summary
    });
  push_item
    state
    (Semantic_tree.Unsupported {
      span = span_of_syntax_node syntax_node;
      kind = Cst.syntax_kind syntax_node;
      summary
    })

let lower_type_declaration = fun state (declaration: Cst.TypeDeclaration.t) ->
  let rec loop declaration =
    let syntax_node = Cst.TypeDeclaration.syntax_node declaration in
    let name = Cst.Token.text (Cst.TypeDeclaration.name_token declaration) in
    push_item state
      (
        Semantic_tree.TypeDeclaration {
          id = fresh_binding_id state ~name;
          span = span_of_syntax_node syntax_node;
          name;
          params =
            Cst.TypeDeclaration.type_params declaration |> List.filter_map ~fn:(
              fun type_parameter ->
                Option.map (Cst.TypeParameter.type_variable type_parameter) ~fn:Cst.TypeVariable.text
            );
          manifest =
            (
              match Cst.TypeDeclaration.manifest_alias declaration with
              | Some manifest -> Some (lower_type_expr state manifest)
              | None ->
                  match Cst.TypeDeclaration.type_definition declaration with
                  | Cst.TypeDefinition.Alias { manifest; _ } -> Some (lower_type_expr state manifest)
                  | _ -> None
            );
          nonrec_ = Option.is_some (Cst.TypeDeclaration.nonrec_token declaration);
          private_ = Cst.TypeDeclaration.is_private declaration;
        }
      );
    match Cst.TypeDeclaration.next_and_declaration declaration with
    | Some next -> loop next
    | None -> ()
  in
  loop declaration

let lower_let_binding = fun state (binding: Cst.LetBinding.t) ->
  let bindings = binding :: Cst.LetBinding.and_bindings binding in
  let recursive = Cst.LetBinding.is_recursive binding in
  bindings |> List.for_each ~fn:(
    fun binding ->
      match Cst.LetBinding.binding_name_token binding with
      | Some name_token ->
          let name = Cst.Token.text name_token in
          push_item state
            (
              Semantic_tree.ValueDeclaration {
                id = fresh_binding_id state ~name;
                span = span_of_syntax_node (Cst.LetBinding.syntax_node binding);
                name = Some name;
                recursive;
                parameter_count = List.length (Cst.LetBinding.parameters binding);
                declared = false;
                annotation = annotation_of_expression state (Cst.LetBinding.value binding);
              }
            )
      | None ->
          push_unsupported
            state
            (Cst.LetBinding.syntax_node binding)
            "let_pattern"
  )

let lower_module_structure = fun state (declaration: Cst.ModuleStructure.t) ->
  let rec loop declaration =
    let name = Cst.ModuleStructure.name declaration in
    let definition =
      match path_of_module_expression (Cst.ModuleStructure.module_expression declaration) with
      | Some path -> Semantic_tree.Alias path
      | None -> Semantic_tree.Opaque
    in
    push_item state
      (
        Semantic_tree.ModuleDeclaration {
          id = fresh_binding_id state ~name;
          span = span_of_syntax_node (Cst.ModuleStructure.syntax_node declaration);
          name;
          recursive = Cst.ModuleStructure.is_recursive declaration;
          definition;
        }
      );
    match Cst.ModuleStructure.next_and_declaration declaration with
    | Some next -> loop next
    | None -> ()
  in
  loop declaration

let lower_module_signature = fun state (declaration: Cst.ModuleSignature.t) ->
  let rec loop declaration =
    let name = Cst.ModuleSignature.name declaration in
    let definition =
      match Cst.ModuleSignature.definition declaration with
      | Cst.ModuleSignature.Alias module_expression -> (
          match path_of_module_expression module_expression with
          | Some path -> Semantic_tree.Alias path
          | None -> Semantic_tree.Opaque
        )
      | Cst.ModuleSignature.Signature _ -> Semantic_tree.Opaque
    in
    push_item state
      (
        Semantic_tree.ModuleDeclaration {
          id = fresh_binding_id state ~name;
          span = span_of_syntax_node (Cst.ModuleSignature.syntax_node declaration);
          name;
          recursive = Cst.ModuleSignature.is_recursive declaration;
          definition;
        }
      );
    match Cst.ModuleSignature.next_and_declaration declaration with
    | Some next -> loop next
    | None -> ()
  in
  loop declaration

let lower_open_statement = fun state (open_statement: Cst.OpenStatement.t) ->
  let target =
    match Cst.OpenStatement.target open_statement with
    | Cst.OpenStatement.Path ident -> Some (path_of_ident ident)
    | Cst.OpenStatement.ModuleExpression module_expression -> path_of_module_expression module_expression
  in
  push_item
    state
    (Semantic_tree.OpenStatement {
      span = span_of_syntax_node (Cst.OpenStatement.syntax_node open_statement);
      target;
      override_ = Cst.OpenStatement.has_bang open_statement
    })

let lower_include_statement = fun state (include_statement: Cst.include_statement) ->
  let target =
    match include_statement.target with
    | Cst.ModuleExpression module_expression -> (
        match path_of_module_expression module_expression with
        | Some path -> Semantic_tree.ModulePath path
        | None -> Semantic_tree.Opaque
      )
    | Cst.ModuleType module_type -> (
        match path_of_module_type module_type with
        | Some path -> Semantic_tree.ModuleTypePath path
        | None -> Semantic_tree.Opaque
      )
  in
  push_item
    state
    (Semantic_tree.IncludeStatement {
      span = span_of_syntax_node include_statement.syntax_node;
      target
    })

let lower_exception_declaration = fun state (declaration: Cst.exception_declaration) ->
  let name = Cst.Token.text declaration.name_token in
  push_item state
    (
      Semantic_tree.ExceptionDeclaration {
        id = fresh_binding_id state ~name;
        span = span_of_syntax_node declaration.syntax_node;
        name;
        rhs =
          match declaration.rhs with
          | Some (Cst.Alias { alias; _ }) -> Some (Semantic_tree.ExceptionAlias (path_of_ident alias))
          | Some (Cst.Payload { payload_type; _ }) -> Some (Semantic_tree.ExceptionPayload (lower_type_expr
            state
            payload_type))
          | None -> None;
      }
    )

let lower_external_declaration = fun state (declaration: Cst.external_declaration) ->
  let name = name_of_name_tokens declaration.name_tokens in
  push_item
    state
    (Semantic_tree.ExternalDeclaration {
      id = fresh_binding_id state ~name;
      span = span_of_syntax_node declaration.syntax_node;
      name;
      annotation = lower_type_expr state declaration.type_
    })

let lower_module_type_declaration = fun state (declaration: Cst.ModuleTypeDeclaration.t) ->
  let name = Cst.ModuleTypeDeclaration.name declaration in
  push_item
    state
    (Semantic_tree.ModuleTypeDeclaration {
      id = fresh_binding_id state ~name;
      span = span_of_syntax_node (Cst.ModuleTypeDeclaration.syntax_node declaration);
      name;
      has_definition = Option.is_some (Cst.ModuleTypeDeclaration.module_type declaration)
    })

let lower_value_declaration = fun state (declaration: Cst.value_declaration) ->
  let name = name_of_name_tokens declaration.name_tokens in
  push_item state
    (
      Semantic_tree.ValueDeclaration {
        id = fresh_binding_id state ~name;
        span = span_of_syntax_node declaration.syntax_node;
        name = Some name;
        recursive = false;
        parameter_count = 0;
        declared = true;
        annotation = Some (lower_type_expr state declaration.type_);
      }
    )

let lower_structure_item = fun state ->
  function
  | Cst.StructureItem.TypeDeclaration declaration -> lower_type_declaration state declaration
  | Cst.StructureItem.LetBinding binding -> lower_let_binding state binding
  | Cst.StructureItem.ModuleDeclaration declaration -> lower_module_structure state declaration
  | Cst.StructureItem.ModuleTypeDeclaration declaration -> lower_module_type_declaration state declaration
  | Cst.StructureItem.OpenStatement open_statement -> lower_open_statement state open_statement
  | Cst.StructureItem.IncludeStatement include_statement -> lower_include_statement state include_statement
  | Cst.StructureItem.ExceptionDeclaration declaration -> lower_exception_declaration state declaration
  | Cst.StructureItem.ExternalDeclaration declaration -> lower_external_declaration state declaration
  | Cst.StructureItem.Expression expression -> push_item
    state
    (Semantic_tree.Expression { span = span_of_syntax_node (Cst.Expression.syntax_node expression) })
  | Cst.StructureItem.Attribute attribute -> push_unsupported state attribute.syntax_node "attribute"
  | Cst.StructureItem.Extension extension -> push_unsupported state extension.syntax_node "extension"
  | Cst.StructureItem.ClassDeclaration declaration -> push_unsupported
    state
    (Cst.ClassDefinition.syntax_node declaration)
    "class declaration"
  | Cst.StructureItem.ClassTypeDeclaration declaration -> push_unsupported
    state
    declaration.syntax_node
    "class type declaration"
  | Cst.StructureItem.TypeExtension declaration -> push_unsupported
    state
    (Cst.TypeExtension.syntax_node declaration)
    "type extension"
  | Cst.StructureItem.Docstring _
  | Cst.StructureItem.Comment _ -> ()

let lower_signature_item = fun state ->
  function
  | Cst.SignatureItem.TypeDeclaration declaration -> lower_type_declaration state declaration
  | Cst.SignatureItem.ModuleDeclaration declaration -> lower_module_signature state declaration
  | Cst.SignatureItem.ModuleTypeDeclaration declaration -> lower_module_type_declaration state declaration
  | Cst.SignatureItem.OpenStatement open_statement -> lower_open_statement state open_statement
  | Cst.SignatureItem.IncludeStatement include_statement -> lower_include_statement state include_statement
  | Cst.SignatureItem.ExceptionDeclaration declaration -> lower_exception_declaration state declaration
  | Cst.SignatureItem.ExternalDeclaration declaration -> lower_external_declaration state declaration
  | Cst.SignatureItem.ValueDeclaration declaration -> lower_value_declaration state declaration
  | Cst.SignatureItem.Attribute attribute -> push_unsupported state attribute.syntax_node "attribute"
  | Cst.SignatureItem.Extension extension -> push_unsupported state extension.syntax_node "extension"
  | Cst.SignatureItem.ClassDeclaration declaration -> push_unsupported
    state
    (Cst.ClassDeclaration.syntax_node declaration)
    "class declaration"
  | Cst.SignatureItem.ClassTypeDeclaration declaration -> push_unsupported
    state
    declaration.syntax_node
    "class type declaration"
  | Cst.SignatureItem.TypeExtension declaration -> push_unsupported
    state
    (Cst.TypeExtension.syntax_node declaration)
    "type extension"
  | Cst.SignatureItem.Docstring _
  | Cst.SignatureItem.Comment _ -> ()

let lower_source_file = fun ~source:_ source_file ->
  let kind =
    match Cst.SourceFile.kind source_file with
    | `Implementation -> `Implementation
    | `Interface -> `Interface
  in
  let state = make_state kind in
  let () =
    match source_file with
    | Cst.Implementation implementation -> implementation.items
    |> List.for_each ~fn:(lower_structure_item state)
    | Cst.Interface interface -> interface.items |> List.for_each ~fn:(lower_signature_item state)
  in
  {
    Semantic_tree.kind = state.kind;
    items = List.reverse state.items;
    exports = List.reverse state.exports;
    diagnostics = List.reverse state.diagnostics
  }
