open Std
open Std.Collections

module Ast = Syn.Ast
module SyntaxKind = Syn.SyntaxKind
module SyntaxTree = Syn.SyntaxTree

type state = {
  kind: Semantic_tree.file_kind;
  mutable next_binding_stamp: int;
  items: Semantic_tree.item Vector.t;
  exports: Semantic_tree.item Vector.t;
  diagnostics: Diagnostics.Diagnostic.t Vector.t;
}

let make_state = fun ~kind ~size_hint ->
  {
    kind;
    next_binding_stamp = 0;
    items = Vector.with_capacity ~size:size_hint;
    exports = Vector.with_capacity ~size:size_hint;
    diagnostics = Vector.with_capacity ~size:4;
  }

let raw_at = fun tree index -> Vector.get_unchecked tree.SyntaxTree.raw_tokens ~at:index

let span_from_raw = fun (span: Ceibo.Span.t) ->
  Syn.Ceibo.Span.make
    ~start:span.Ceibo.Span.start
    ~end_:span.Ceibo.Span.end_

let span_of_raw_range = fun tree ~raw_lo ~raw_hi ->
  if Int.(raw_hi <= raw_lo) then
    Syn.Ceibo.Span.make ~start:0 ~end_:0
  else
    let first = raw_at tree raw_lo in
    let last = raw_at tree (Int.sub raw_hi 1) in
    Syn.Ceibo.Span.make
      ~start:first.Syn.RawToken.span.Ceibo.Span.start
      ~end_:last.Syn.RawToken.span.Ceibo.Span.end_

let token_body_span = fun (token: Ast.Token.t) ->
  let leaf = SyntaxTree.token token.tree token.id in
  (raw_at token.tree leaf.SyntaxTree.body_raw).Syn.RawToken.span

let span_of_token = fun token ->
  let span = token_body_span token in
  span_from_raw span

let span_of_node = fun (node: Ast.Node.t) ->
  let syntax_node = SyntaxTree.node node.tree node.id in
  match Ast.Node.first_descendant_token node with
  | Some first ->
      let first_leaf = SyntaxTree.token first.tree first.id in
      let first_raw = raw_at node.tree first_leaf.SyntaxTree.body_raw in
      let last_raw = raw_at node.tree (Int.sub syntax_node.SyntaxTree.raw_hi 1) in
      Syn.Ceibo.Span.make
        ~start:first_raw.Syn.RawToken.span.Ceibo.Span.start
        ~end_:last_raw.Syn.RawToken.span.Ceibo.Span.end_
  | None -> span_of_raw_range node.tree ~raw_lo:syntax_node.raw_lo ~raw_hi:syntax_node.raw_hi

let span_start = fun span -> span.Syn.Ceibo.Span.start

let span_end = fun span -> span.Syn.Ceibo.Span.end_

let span_from_start_to_end = fun ~start ~end_ -> Syn.Ceibo.Span.make ~start ~end_

let span_from_token_to_end = fun token ~end_ ->
  let start = span_start (span_of_token token) in
  span_from_start_to_end ~start ~end_

let span_of_children = fun tree ~child_count ~child_at ->
  let first = ref None in
  let last = ref None in
  let remember span =
    (
      match !first with
      | Some _ -> ()
      | None -> first := Some (span_start span)
    );
    last := Some (span_end span)
  in
  let span_of_child = function
    | SyntaxTree.Token id ->
        let leaf = SyntaxTree.token tree id in
        let raw = raw_at tree leaf.SyntaxTree.body_raw in
        remember (span_from_raw raw.Syn.RawToken.span)
    | SyntaxTree.Node id -> remember (span_of_node { Ast.tree; id })
    | SyntaxTree.Missing missing ->
        let span = Syn.Ceibo.Span.make ~start:missing.SyntaxTree.offset ~end_:missing.offset in
        remember span
  in
  let rec loop index =
    if Int.(index < child_count) then (
      (
        match child_at index with
        | Some child -> span_of_child child
        | None -> ()
      );
      loop (Int.add index 1)
    )
  in
  loop 0;
  match (!first, !last) with
  | (Some start, Some end_) -> Syn.Ceibo.Span.make ~start ~end_
  | _ -> Syn.Ceibo.Span.make ~start:0 ~end_:0

let span_of_type_member = fun member ->
  let decl = Ast.TypeDeclaration.Member.declaration member in
  span_of_children
    decl.tree
    ~child_count:(Ast.TypeDeclaration.Member.child_count member)
    ~child_at:(Ast.TypeDeclaration.Member.child_at member)

let span_of_module_member = fun member ->
  let decl = Ast.ModuleDeclaration.Member.declaration member in
  span_of_children
    decl.tree
    ~child_count:(Ast.ModuleDeclaration.Member.child_count member)
    ~child_at:(Ast.ModuleDeclaration.Member.child_at member)

let fresh_binding_id = fun state ~name ->
  let stamp = state.next_binding_stamp in
  state.next_binding_stamp <- Int.add stamp 1;
  Model.Binding_id.local ~stamp ~name:(Model.Surface_path.of_name name)

let vector_to_list = fun vector ->
  vector
  |> Vector.to_array
  |> Array.to_list

let collect_path = fun for_each_ident ->
  let segments = Vector.with_capacity ~size:4 in
  for_each_ident ~fn:(fun token -> Vector.push segments ~value:(Ast.Token.text token));
  vector_to_list segments

let path_of_path = fun path -> collect_path (Ast.Path.for_each_ident path)

let path_of_node = fun node ->
  match Ast.Path.cast node with
  | Some path -> Some (path_of_path path)
  | None -> None

let name_of_tokens = fun for_each_token ->
  let parts = Vector.with_capacity ~size:2 in
  for_each_token ~fn:(fun token -> Vector.push parts ~value:(Ast.Token.text token));
  parts
  |> vector_to_list
  |> String.concat ""

let name_of_token_vector = fun tokens ->
  let parts = Vector.with_capacity ~size:(Vector.length tokens) in
  Vector.for_each tokens ~fn:(fun token -> Vector.push parts ~value:(Ast.Token.text token));
  parts
  |> vector_to_list
  |> String.concat ""

let push_diagnostic = fun state diagnostic -> Vector.push state.diagnostics ~value:diagnostic

let push_unsupported_type = fun state node summary ->
  push_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedType { span = span_of_node node; summary })

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
  Vector.push state.items ~value:item;
  if is_export item then
    Vector.push state.exports ~value:item

let push_unsupported = fun state node summary ->
  push_diagnostic
    state
    (Diagnostics.Diagnostic.UnsupportedSyntax {
      span = span_of_node node;
      kind = Ast.Node.kind node;
      summary;
    });
  push_item
    state
    (Semantic_tree.Unsupported { span = span_of_node node; kind = Ast.Node.kind node; summary })

let push_unsupported_with_span = fun state ~span ~kind summary ->
  push_diagnostic state (Diagnostics.Diagnostic.UnsupportedSyntax { span; kind; summary });
  push_item state (Semantic_tree.Unsupported { span; kind; summary })

let collect_type_tuple = fun lower_type_expr state type_expr ->
  let items = Vector.with_capacity ~size:2 in
  let rec collect type_expr =
    match Ast.TypeExpr.view type_expr with
    | Ast.TypeExpr.Tuple { parts } -> Vector.for_each parts ~fn:collect
    | _ -> Vector.push items ~value:(lower_type_expr state type_expr)
  in
  collect type_expr;
  vector_to_list items

let rec lower_type_expr = fun state type_expr ->
  match Ast.TypeExpr.view type_expr with
  | Ast.TypeExpr.Wildcard -> Semantic_tree.AnyType
  | Ast.TypeExpr.Var { name } -> Semantic_tree.TypeVar (Ast.Token.text name)
  | Ast.TypeExpr.Ident { path } ->
      Semantic_tree.TypeConstr { path = path_of_path path; arguments = [] }
  | Ast.TypeExpr.Apply { ident; args } ->
      let arguments = Vector.with_capacity ~size:(Vector.length args) in
      Vector.for_each args ~fn:(fun arg -> Vector.push arguments ~value:(lower_type_expr state arg));
      Semantic_tree.TypeConstr {
        path = path_of_path ident;
        arguments = vector_to_list arguments;
      }
  | Ast.TypeExpr.Arrow { label; arg; ret } ->
      let label =
        Option.map
          label
          ~fn:(fun label ->
            {
              Semantic_tree.name =
                label.Ast.TypeExpr.name
                |> Option.map ~fn:Ast.Token.text
                |> Option.unwrap_or ~default:"";
              optional_ = label.optional_;
            })
      in
      Semantic_tree.TypeArrow {
        label;
        parameter = lower_type_expr state arg;
        result = lower_type_expr state ret;
      }
  | Ast.TypeExpr.Poly { names; body } ->
      let binders = Vector.with_capacity ~size:(Vector.length names) in
      Vector.for_each names ~fn:(fun token -> Vector.push binders ~value:(Ast.Token.text token));
      Semantic_tree.TypePoly {
        binders = vector_to_list binders;
        body = lower_type_expr state body;
      }
  | Ast.TypeExpr.Tuple _ ->
      Semantic_tree.TypeTuple (collect_type_tuple lower_type_expr state type_expr)
  | Ast.TypeExpr.Unknown node -> (
      match Ast.TypeExpr.inner_without_attribute_suffix type_expr with
      | Some inner -> lower_type_expr state inner
      | None ->
          push_unsupported_type state node "unknown";
          Semantic_tree.TypeUnsupported "unknown"
    )
  | Ast.TypeExpr.Error node ->
      push_unsupported_type state node "error";
      Semantic_tree.TypeUnsupported "error"

let rec annotation_of_expression = fun state expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Annotated { annotation; _ } -> Some (lower_type_expr state annotation)
  | _ -> None

let binding_name_token = fun binding ->
  let rec of_pattern pattern =
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Ident { path } -> Ast.Path.last_ident path
    | Ast.Pattern.Constraint { pattern = inner; _ } -> of_pattern inner
    | Ast.Pattern.Alias { alias; _ } -> of_pattern alias
    | _ -> None
  in
  match Ast.LetBinding.pattern binding with
  | Some pattern -> of_pattern pattern
  | None -> None

let parameter_count = fun binding ->
  let count = ref 0 in
  let rec count_parameter_pattern pattern =
    match Ast.Pattern.view pattern with
    | Ast.Pattern.Constraint { pattern; _ } -> count_parameter_pattern pattern
    | _ -> count := Int.add !count 1
  in
  Ast.LetBinding.for_each_parameter binding ~fn:count_parameter_pattern;
  !count

let return_annotation_of_binding = fun binding ->
  let seen_first_pattern = ref false in
  let annotation = ref None in
  Ast.Node.for_each_child_node
    binding
    ~fn:(fun node ->
      match Ast.Pattern.cast node with
      | None -> ()
      | Some pattern ->
          if !seen_first_pattern then
            (
              match Ast.Node.kind node with
              | Syn.SyntaxKind.CONSTRAINT_PATTERN -> (
                  match Ast.Pattern.view pattern with
                  | Ast.Pattern.Constraint { annotation = type_expr; _ } ->
                      annotation := Some type_expr
                  | _ -> ()
                )
              | _ -> ()
            )
          else
            seen_first_pattern := true);
  !annotation

let lower_let_declaration = fun state declaration ->
  let recursive = Option.is_some (Ast.LetDeclaration.rec_token declaration) in
  let declaration_span = span_of_node declaration in
  let declaration_end = span_end declaration_span in
  let first_binding = ref true in
  Ast.LetDeclaration.for_each_binding
    declaration
    ~fn:(fun binding ->
      match binding_name_token binding with
      | Some name_token ->
          let name = Ast.Token.text name_token in
          let annotation =
            match Ast.LetBinding.type_annotation binding with
            | Some annotation -> Some (lower_type_expr state annotation)
            | None -> (
                match return_annotation_of_binding binding with
                | Some annotation -> Some (lower_type_expr state annotation)
                | None -> (
                    match Ast.LetBinding.body binding with
                    | Some body -> annotation_of_expression state body
                    | None -> None
                  )
              )
          in
          let span =
            if !first_binding then
              declaration_span
            else
              span_from_token_to_end name_token ~end_:declaration_end
          in
          first_binding := false;
          push_item
            state
            (
              Semantic_tree.ValueDeclaration {
                id = fresh_binding_id state ~name;
                span;
                name = Some name;
                recursive;
                parameter_count = parameter_count binding;
                declared = false;
                annotation;
              }
            )
      | None ->
          let span =
            if !first_binding then
              declaration_span
            else
              span_of_node binding
          in
          first_binding := false;
          push_unsupported_with_span state ~span ~kind:(Ast.Node.kind binding) "let_pattern")

let type_parameter_name = function
  | Ast.TypeDeclaration.Named { name; quote = Some _; _ } -> Some ("'" ^ Ast.Token.text name)
  | Ast.TypeDeclaration.Named { name; quote = None; _ } -> Some (Ast.Token.text name)
  | Ast.TypeDeclaration.Wildcard _ -> Some "_"

let manifest_of_type_member = fun state member ->
  match Ast.TypeDeclaration.Member.manifest member with
  | Some manifest -> (
      match Ast.TypeExpr.view manifest with
      | Ast.TypeExpr.Unknown _ -> None
      | _ -> Some (lower_type_expr state manifest)
    )
  | None -> None

let member_has_token = fun child_count child_token_at kind ->
  let rec loop index =
    if Int.(index >= child_count) then
      false
    else
      match child_token_at index with
      | Some token when SyntaxKind.(Ast.Token.kind token = kind) -> true
      | _ -> loop (Int.add index 1)
  in
  loop 0

let lower_type_declaration = fun state declaration ->
  let declaration_span = span_of_node declaration in
  let declaration_end = span_end declaration_span in
  let first_member = ref true in
  Ast.TypeDeclaration.for_each_member
    declaration
    ~fn:(fun member ->
      match Ast.TypeDeclaration.Member.name member with
      | Some name_token ->
          let params = Vector.with_capacity ~size:2 in
          Ast.TypeDeclaration.Member.for_each_parameter
            member
            ~fn:(fun parameter ->
              match type_parameter_name parameter with
              | Some name -> Vector.push params ~value:name
              | None -> ());
          let private_ =
            member_has_token
              (Ast.TypeDeclaration.Member.child_count member)
              (Ast.TypeDeclaration.Member.child_token_at member)
              SyntaxKind.PRIVATE_KW
          in
          let name = Ast.Token.text name_token in
          let span =
            if !first_member then
              declaration_span
            else
              span_from_token_to_end name_token ~end_:declaration_end
          in
          first_member := false;
          push_item
            state
            (
              Semantic_tree.TypeDeclaration {
                id = fresh_binding_id state ~name;
                span;
                name;
                params = vector_to_list params;
                manifest = manifest_of_type_member state member;
                nonrec_ = Option.is_some (Ast.TypeDeclaration.Member.nonrec_token member);
                private_;
              }
            )
      | None ->
          first_member := false;
          push_unsupported state declaration "type declaration")

let path_of_module_body = fun for_each_ident ->
  let path = collect_path for_each_ident in
  match path with
  | [] -> None
  | _ -> Some path

let lower_module_declaration = fun state declaration ->
  let declaration_span = span_of_node declaration in
  let declaration_end = span_end declaration_span in
  let first_member = ref true in
  Ast.ModuleDeclaration.for_each_member
    declaration
    ~fn:(fun member ->
      match Ast.ModuleDeclaration.Member.name member with
      | Some name_token ->
          let name = Ast.Token.text name_token in
          let definition =
            match Ast.ModuleDeclaration.Member.module_expr member with
            | Some node -> (
                match path_of_node node with
                | Some path -> Semantic_tree.Alias path
                | None -> Semantic_tree.Opaque
              )
            | None -> (
                match Ast.ModuleDeclaration.Member.module_type member with
                | Some node -> (
                    match path_of_node node with
                    | Some path -> Semantic_tree.Alias path
                    | None -> Semantic_tree.Opaque
                  )
                | None -> Semantic_tree.Opaque
              )
          in
          let recursive =
            match Ast.ModuleDeclaration.Member.find_token member SyntaxKind.REC_KW with
            | Some _ -> true
            | None -> false
          in
          let span =
            if !first_member then
              declaration_span
            else
              span_from_token_to_end name_token ~end_:declaration_end
          in
          first_member := false;
          push_item
            state
            (
              Semantic_tree.ModuleDeclaration {
                id = fresh_binding_id state ~name;
                span;
                name;
                recursive;
                definition;
              }
            )
      | None ->
          first_member := false;
          push_unsupported state declaration "module declaration")

let lower_module_type_declaration = fun state declaration ->
  match Ast.ModuleTypeDeclaration.name declaration with
  | Some name_token ->
      let name = Ast.Token.text name_token in
      let has_definition =
        match Ast.ModuleTypeDeclaration.body declaration with
        | Ast.ModuleTypeDeclaration.Abstract -> false
        | Ast.ModuleTypeDeclaration.Manifest _
        | _ -> true
      in
      push_item
        state
        (
          Semantic_tree.ModuleTypeDeclaration {
            id = fresh_binding_id state ~name;
            span = span_of_node declaration;
            name;
            has_definition;
          }
        )
  | None -> push_unsupported state declaration "module type declaration"

let lower_open_declaration = fun state declaration ->
  let target = path_of_module_body (Ast.OpenDeclaration.for_each_path_ident declaration) in
  let override_ =
    match Ast.Node.first_child_token declaration ~kind:SyntaxKind.BANG with
    | Some _ -> true
    | None -> false
  in
  push_item
    state
    (Semantic_tree.OpenStatement { span = span_of_node declaration; target; override_ })

let lower_include_declaration = fun state declaration ->
  let target =
    match Ast.IncludeDeclaration.body_node declaration with
    | Some node when SyntaxKind.(Ast.Node.kind node = PATH_MODULE_TYPE) -> (
        match path_of_node node with
        | Some path -> Semantic_tree.ModuleTypePath path
        | None -> Semantic_tree.Opaque
      )
    | Some node when SyntaxKind.(Ast.Node.kind node = PATH_MODULE_EXPR) -> (
        match path_of_node node with
        | Some path -> Semantic_tree.ModulePath path
        | None -> Semantic_tree.Opaque
      )
    | Some node -> (
        match path_of_node node with
        | Some path -> Semantic_tree.ModulePath path
        | None -> Semantic_tree.Opaque
      )
    | None -> Semantic_tree.Opaque
  in
  push_item state (Semantic_tree.IncludeStatement { span = span_of_node declaration; target })

let lower_exception_declaration = fun state declaration ->
  match Ast.ExceptionDeclaration.name declaration with
  | Some name_token ->
      let name = Ast.Token.text name_token in
      let rhs =
        match Ast.ExceptionDeclaration.view declaration with
        | Ast.ExceptionDeclaration.Bare -> None
        | Ast.ExceptionDeclaration.Alias { path; _ } ->
            Some (Semantic_tree.ExceptionAlias (path_of_path path))
        | Ast.ExceptionDeclaration.Payload { payload = Ast.ExceptionDeclaration.TypeExpr type_expr; _ } ->
            Some (Semantic_tree.ExceptionPayload (lower_type_expr state type_expr))
        | Ast.ExceptionDeclaration.Payload { payload = Ast.ExceptionDeclaration.Record record; _ } ->
            push_unsupported_type state record "record";
            Some (Semantic_tree.ExceptionPayload (Semantic_tree.TypeUnsupported "record"))
        | Ast.ExceptionDeclaration.Unknown _ -> None
      in
      push_item
        state
        (
          Semantic_tree.ExceptionDeclaration {
            id = fresh_binding_id state ~name;
            span = span_of_node declaration;
            name;
            rhs;
          }
        )
  | None -> push_unsupported state declaration "exception declaration"

let lower_external_declaration = fun state declaration ->
  match Ast.ExternalDeclaration.view declaration with
  | Ast.ExternalDeclaration.External { name = name_tokens; annotation; _ } ->
      let name = name_of_token_vector name_tokens in
      push_item
        state
        (
          Semantic_tree.ExternalDeclaration {
            id = fresh_binding_id state ~name;
            span = span_of_node declaration;
            name;
            annotation = lower_type_expr state annotation;
          }
        )
  | Ast.ExternalDeclaration.Unknown _ -> push_unsupported state declaration "external declaration"

let lower_value_declaration = fun state declaration ->
  match Ast.ValueDeclaration.view declaration with
  | Ast.ValueDeclaration.Value { name = name_tokens; annotation; _ } ->
      let name = name_of_token_vector name_tokens in
      push_item
        state
        (
          Semantic_tree.ValueDeclaration {
            id = fresh_binding_id state ~name;
            span = span_of_node declaration;
            name = Some name;
            recursive = false;
            parameter_count = 0;
            declared = true;
            annotation = Some (lower_type_expr state annotation);
          }
        )
  | Ast.ValueDeclaration.Unknown _ -> push_unsupported state declaration "value declaration"

let lower_expr_item = fun state item ->
  match Ast.ExprItem.expr item with
  | Some expr -> push_item state (Semantic_tree.Expression { span = span_of_node expr })
  | None -> push_unsupported state item "expression"

let lower_structure_item = fun state item ->
  match Ast.StructureItem.view item with
  | Ast.StructureItem.Let declaration -> lower_let_declaration state declaration
  | Ast.StructureItem.Type (Ast.TypeDeclarationItem declaration) ->
      lower_type_declaration state declaration
  | Ast.StructureItem.Module declaration -> lower_module_declaration state declaration
  | Ast.StructureItem.ModuleType declaration -> lower_module_type_declaration state declaration
  | Ast.StructureItem.Open declaration -> lower_open_declaration state declaration
  | Ast.StructureItem.Include declaration -> lower_include_declaration state declaration
  | Ast.StructureItem.External declaration -> lower_external_declaration state declaration
  | Ast.StructureItem.Exception declaration -> lower_exception_declaration state declaration
  | Ast.StructureItem.Expr item -> lower_expr_item state item
  | Ast.StructureItem.Type (Ast.TypeExtensionItem declaration) ->
      push_unsupported_with_span
        state
        ~span:(span_of_node declaration)
        ~kind:SyntaxKind.TYPE_DECL
        "type extension"
  | Ast.StructureItem.Extension item -> push_unsupported state item "extension"
  | Ast.StructureItem.Attribute item -> push_unsupported state item "attribute"
  | Ast.StructureItem.Error node -> push_unsupported state node "error"
  | Ast.StructureItem.Unknown node -> push_unsupported state node "unknown"

let lower_signature_item = fun state item ->
  match Ast.SignatureItem.view item with
  | Ast.SignatureItem.Value declaration -> lower_value_declaration state declaration
  | Ast.SignatureItem.Type (Ast.TypeDeclarationItem declaration) ->
      lower_type_declaration state declaration
  | Ast.SignatureItem.Module declaration -> lower_module_declaration state declaration
  | Ast.SignatureItem.ModuleType declaration -> lower_module_type_declaration state declaration
  | Ast.SignatureItem.Open declaration -> lower_open_declaration state declaration
  | Ast.SignatureItem.Include declaration -> lower_include_declaration state declaration
  | Ast.SignatureItem.External declaration -> lower_external_declaration state declaration
  | Ast.SignatureItem.Exception declaration -> lower_exception_declaration state declaration
  | Ast.SignatureItem.Type (Ast.TypeExtensionItem declaration) ->
      push_unsupported_with_span
        state
        ~span:(span_of_node declaration)
        ~kind:SyntaxKind.TYPE_DECL
        "type extension"
  | Ast.SignatureItem.Extension item -> push_unsupported state item "extension"
  | Ast.SignatureItem.Attribute item -> push_unsupported state item "attribute"
  | Ast.SignatureItem.Error node -> push_unsupported state node "error"
  | Ast.SignatureItem.Unknown node -> push_unsupported state node "unknown"

let lower_source_file = fun ~source:_ (parse_result: Syn.Parser.parse_result) ->
  let source_file = Ast.SourceFile.make parse_result.tree in
  let size_hint = Vector.length parse_result.tree.SyntaxTree.nodes in
  let state = make_state ~kind:parse_result.kind ~size_hint in
  (
    match Ast.SourceFile.view source_file with
    | Ast.SourceFile.Implementation implementation ->
        Ast.Implementation.for_each_item implementation ~fn:(lower_structure_item state)
    | Ast.SourceFile.Interface interface ->
        Ast.Interface.for_each_item interface ~fn:(lower_signature_item state)
  );
  {
    Semantic_tree.kind = state.kind;
    items = vector_to_list state.items;
    exports = vector_to_list state.exports;
    diagnostics = vector_to_list state.diagnostics;
  }
