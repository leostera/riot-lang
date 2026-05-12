open Std
open Std.Collections

module A = Ast

type action =
  | Continue
  | Skip_subtree

type 'value cached =
  | Unknown
  | Cached of 'value option

type arena = {
  mutable tree: Syntax_tree.t option;
  mutable size: int;
  mutable structure_items: A.StructureItem.t cached array;
  mutable signature_items: A.SignatureItem.t cached array;
  mutable let_declarations: A.LetDeclaration.t cached array;
  mutable let_bindings: A.LetBinding.t cached array;
  mutable type_declarations: A.TypeDeclaration.t cached array;
  mutable module_declarations: A.ModuleDeclaration.t cached array;
  mutable module_type_declarations: A.ModuleTypeDeclaration.t cached array;
  mutable open_declarations: A.OpenDeclaration.t cached array;
  mutable include_declarations: A.IncludeDeclaration.t cached array;
  mutable value_declarations: A.ValueDeclaration.t cached array;
  mutable exprs: A.Expr.t cached array;
  mutable patterns: A.Pattern.t cached array;
  mutable parameters: A.Parameter.t cached array;
  mutable type_exprs: A.TypeExpr.t cached array;
}

type 'ctx t = {
  arena: arena;
  ctx: 'ctx;
  hooks: 'ctx hooks;
}

and 'ctx enter_node = 'ctx t -> A.Node.t -> 'ctx t * action

and 'ctx leave_node = 'ctx t -> A.Node.t -> 'ctx t

and 'ctx enter_token = 'ctx t -> A.Token.t -> 'ctx t

and 'ctx enter_structure_item = 'ctx t -> A.StructureItem.t -> 'ctx t * action

and 'ctx enter_signature_item = 'ctx t -> A.SignatureItem.t -> 'ctx t * action

and 'ctx enter_let_declaration = 'ctx t -> A.LetDeclaration.t -> 'ctx t * action

and 'ctx enter_let_binding = 'ctx t -> A.LetBinding.t -> 'ctx t * action

and 'ctx enter_type_declaration = 'ctx t -> A.TypeDeclaration.t -> 'ctx t * action

and 'ctx enter_module_declaration = 'ctx t -> A.ModuleDeclaration.t -> 'ctx t * action

and 'ctx enter_module_functor_parameter =
  'ctx t ->
  A.ModuleDeclaration.Member.functor_parameter ->
  'ctx t * action

and 'ctx enter_module_type_declaration = 'ctx t -> A.ModuleTypeDeclaration.t -> 'ctx t * action

and 'ctx enter_open_declaration = 'ctx t -> A.OpenDeclaration.t -> 'ctx t * action

and 'ctx enter_include_declaration = 'ctx t -> A.IncludeDeclaration.t -> 'ctx t * action

and 'ctx enter_value_declaration = 'ctx t -> A.ValueDeclaration.t -> 'ctx t * action

and 'ctx enter_expr = 'ctx t -> A.Expr.t -> 'ctx t * action

and 'ctx enter_pattern = 'ctx t -> A.Pattern.t -> 'ctx t * action

and 'ctx enter_parameter = 'ctx t -> A.Parameter.t -> 'ctx t * action

and 'ctx enter_type_expr = 'ctx t -> A.TypeExpr.t -> 'ctx t * action

and 'ctx hooks = {
  enter_node: 'ctx enter_node option;
  leave_node: 'ctx leave_node option;
  enter_token: 'ctx enter_token option;
  enter_structure_item: 'ctx enter_structure_item option;
  enter_signature_item: 'ctx enter_signature_item option;
  enter_let_declaration: 'ctx enter_let_declaration option;
  enter_let_binding: 'ctx enter_let_binding option;
  enter_type_declaration: 'ctx enter_type_declaration option;
  enter_module_declaration: 'ctx enter_module_declaration option;
  enter_module_functor_parameter: 'ctx enter_module_functor_parameter option;
  enter_module_type_declaration: 'ctx enter_module_type_declaration option;
  enter_open_declaration: 'ctx enter_open_declaration option;
  enter_include_declaration: 'ctx enter_include_declaration option;
  enter_value_declaration: 'ctx enter_value_declaration option;
  enter_expr: 'ctx enter_expr option;
  enter_pattern: 'ctx enter_pattern option;
  enter_parameter: 'ctx enter_parameter option;
  enter_type_expr: 'ctx enter_type_expr option;
}

let make_slots: type value. int -> value cached array = fun size ->
  Array.make
    ~count:size
    ~value:Unknown

let make_arena = fun ~size ->
  {
    tree = None;
    size;
    structure_items = make_slots size;
    signature_items = make_slots size;
    let_declarations = make_slots size;
    let_bindings = make_slots size;
    type_declarations = make_slots size;
    module_declarations = make_slots size;
    module_type_declarations = make_slots size;
    open_declarations = make_slots size;
    include_declarations = make_slots size;
    value_declarations = make_slots size;
    exprs = make_slots size;
    patterns = make_slots size;
    parameters = make_slots size;
    type_exprs = make_slots size;
  }

let reset_arena = fun arena tree ->
  let size = Vector.length tree.Syntax_tree.nodes in
  arena.tree <- Some tree;
  arena.size <- size;
  arena.structure_items <- make_slots size;
  arena.signature_items <- make_slots size;
  arena.let_declarations <- make_slots size;
  arena.let_bindings <- make_slots size;
  arena.type_declarations <- make_slots size;
  arena.module_declarations <- make_slots size;
  arena.module_type_declarations <- make_slots size;
  arena.open_declarations <- make_slots size;
  arena.include_declarations <- make_slots size;
  arena.value_declarations <- make_slots size;
  arena.exprs <- make_slots size;
  arena.patterns <- make_slots size;
  arena.parameters <- make_slots size;
  arena.type_exprs <- make_slots size

let prepare_arena = fun arena tree ->
  match arena.tree with
  | Some current when Ptr.equal current tree -> ()
  | _ -> reset_arena arena tree

let cached_cast:
  type value. value cached array ->
  A.Node.t ->
  (A.Node.t -> value A.cast_result) ->
  value option = fun slots node cast ->
  let cast_node node = A.cast_result_to_option (cast node) in
  let index = node.Ast.id in
  if index < 0 || index >= Array.length slots then
    cast_node node
  else
    match Array.get_unchecked slots ~at:index with
    | Cached value -> value
    | Unknown ->
        let value = cast_node node in
        Array.set_unchecked slots ~at:index ~value:(Cached value);
        value

let empty_hooks = {
  enter_node = None;
  leave_node = None;
  enter_token = None;
  enter_structure_item = None;
  enter_signature_item = None;
  enter_let_declaration = None;
  enter_let_binding = None;
  enter_type_declaration = None;
  enter_module_declaration = None;
  enter_module_functor_parameter = None;
  enter_module_type_declaration = None;
  enter_open_declaration = None;
  enter_include_declaration = None;
  enter_value_declaration = None;
  enter_expr = None;
  enter_pattern = None;
  enter_parameter = None;
  enter_type_expr = None;
}

let make = fun ~ctx ~hooks -> { arena = make_arena ~size:0; ctx; hooks }

let ctx = fun visitor -> visitor.ctx

let with_ctx = fun visitor ctx -> { visitor with ctx }

let structure_item = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.structure_items node A.StructureItem.cast

let signature_item = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.signature_items node A.SignatureItem.cast

let let_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.let_declarations node A.LetDeclaration.cast

let let_binding = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.let_bindings node A.LetBinding.cast

let type_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.type_declarations node A.TypeDeclaration.cast

let module_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.module_declarations node A.ModuleDeclaration.cast

let module_type_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.module_type_declarations node A.ModuleTypeDeclaration.cast

let open_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.open_declarations node A.OpenDeclaration.cast

let include_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.include_declarations node A.IncludeDeclaration.cast

let value_declaration = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.value_declarations node A.ValueDeclaration.cast

let expr = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.exprs node A.Expr.cast

let pattern = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.patterns node A.Pattern.cast

let parameter = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast
    visitor.arena.parameters
    node
    (fun node ->
      match A.Node.kind node with
      | Syntax_kind.LABELED_PARAM
      | Syntax_kind.OPTIONAL_PARAM
      | Syntax_kind.OPTIONAL_PARAM_DEFAULT -> A.Parameter.cast node
      | _ -> A.Unknown node)

let type_expr = fun visitor (node: A.Node.t) ->
  prepare_arena visitor.arena node.Ast.tree;
  cached_cast visitor.arena.type_exprs node A.TypeExpr.cast

let call_enter_node = fun visitor node ->
  match visitor.hooks.enter_node with
  | Some enter -> enter visitor node
  | None -> (visitor, Continue)

let call_leave_node = fun visitor node ->
  match visitor.hooks.leave_node with
  | Some leave -> leave visitor node
  | None -> visitor

let call_enter_token = fun visitor token ->
  match visitor.hooks.enter_token with
  | Some enter -> enter visitor token
  | None -> visitor

let enter_with_cast = fun hook cast visitor node ->
  match hook with
  | None -> (visitor, Continue)
  | Some enter -> (
      match cast visitor node with
      | Some view -> enter visitor view
      | None -> (visitor, Continue)
    )

let enter_typed_hooks = fun visitor node ->
  let (visitor, action) =
    enter_with_cast visitor.hooks.enter_structure_item structure_item visitor node
  in
  match action with
  | Skip_subtree -> (visitor, Skip_subtree)
  | Continue ->
      let (visitor, action) =
        enter_with_cast visitor.hooks.enter_signature_item signature_item visitor node
      in
      match action with
      | Skip_subtree -> (visitor, Skip_subtree)
      | Continue ->
          let (visitor, action) =
            enter_with_cast visitor.hooks.enter_let_declaration let_declaration visitor node
          in
          match action with
          | Skip_subtree -> (visitor, Skip_subtree)
          | Continue ->
              let (visitor, action) =
                enter_with_cast visitor.hooks.enter_let_binding let_binding visitor node
              in
              match action with
              | Skip_subtree -> (visitor, Skip_subtree)
              | Continue ->
                  let (visitor, action) =
                    enter_with_cast
                      visitor.hooks.enter_type_declaration
                      type_declaration
                      visitor
                      node
                  in
                  match action with
                  | Skip_subtree -> (visitor, Skip_subtree)
                  | Continue ->
                      let (visitor, action) =
                        enter_with_cast
                          visitor.hooks.enter_module_declaration
                          module_declaration
                          visitor
                          node
                      in
                      match action with
                      | Skip_subtree -> (visitor, Skip_subtree)
                      | Continue ->
                          let (visitor, action) =
                            enter_with_cast
                              visitor.hooks.enter_module_type_declaration
                              module_type_declaration
                              visitor
                              node
                          in
                          match action with
                          | Skip_subtree -> (visitor, Skip_subtree)
                          | Continue ->
                              let (visitor, action) =
                                enter_with_cast
                                  visitor.hooks.enter_open_declaration
                                  open_declaration
                                  visitor
                                  node
                              in
                              match action with
                              | Skip_subtree -> (visitor, Skip_subtree)
                              | Continue ->
                                  let (visitor, action) =
                                    enter_with_cast
                                      visitor.hooks.enter_include_declaration
                                      include_declaration
                                      visitor
                                      node
                                  in
                                  match action with
                                  | Skip_subtree -> (visitor, Skip_subtree)
                                  | Continue ->
                                      let (visitor, action) =
                                        enter_with_cast
                                          visitor.hooks.enter_value_declaration
                                          value_declaration
                                          visitor
                                          node
                                      in
                                      match action with
                                      | Skip_subtree -> (visitor, Skip_subtree)
                                      | Continue ->
                                          let (visitor, action) =
                                            enter_with_cast
                                              visitor.hooks.enter_expr
                                              expr
                                              visitor
                                              node
                                          in
                                          match action with
                                          | Skip_subtree -> (visitor, Skip_subtree)
                                          | Continue ->
                                              let (visitor, action) =
                                                enter_with_cast
                                                  visitor.hooks.enter_pattern
                                                  pattern
                                                  visitor
                                                  node
                                              in
                                              match action with
                                              | Skip_subtree -> (visitor, Skip_subtree)
                                              | Continue ->
                                                  let (visitor, action) =
                                                    enter_with_cast
                                                      visitor.hooks.enter_parameter
                                                      parameter
                                                      visitor
                                                      node
                                                  in
                                                  match action with
                                                  | Skip_subtree -> (visitor, Skip_subtree)
                                                  | Continue ->
                                                      enter_with_cast
                                                        visitor.hooks.enter_type_expr
                                                        type_expr
                                                        visitor
                                                        node

let enter_module_functor_parameter_hooks = fun visitor node ->
  match visitor.hooks.enter_module_functor_parameter with
  | None -> (visitor, Continue)
  | Some enter -> (
      match module_declaration visitor node with
      | None -> (visitor, Continue)
      | Some decl ->
          A.ModuleDeclaration.fold_member
            decl
            ~init:(visitor, Continue)
            ~fn:(fun member (visitor, action) ->
              match action with
              | Skip_subtree -> A.Return (visitor, action)
              | Continue ->
                  let (visitor, action) =
                    A.ModuleDeclaration.Member.fold_functor_parameter
                      member
                      ~init:(visitor, Continue)
                      ~fn:(fun parameter (visitor, action) ->
                        match action with
                        | Skip_subtree -> A.Return (visitor, action)
                        | Continue ->
                            let (visitor, action) = enter visitor parameter in
                            (
                              match action with
                              | Skip_subtree -> A.Return (visitor, action)
                              | Continue -> A.Continue (visitor, action)
                            ))
                  in
                  (
                    match action with
                    | Skip_subtree -> A.Return (visitor, action)
                    | Continue -> A.Continue (visitor, action)
                  ))
    )

let node_of_child = fun (parent: A.Node.t) id: A.Node.t -> { tree = parent.Ast.tree; id }

let token_of_child = fun (parent: A.Node.t) id: A.Token.t -> { tree = parent.Ast.tree; id }

let rec visit_node: 'ctx. 'ctx t -> A.Node.t -> 'ctx t = fun visitor node ->
  prepare_arena visitor.arena node.Ast.tree;
  let (visitor, action) = call_enter_node visitor node in
  let (visitor, action) =
    match action with
    | Skip_subtree -> (visitor, Skip_subtree)
    | Continue -> enter_typed_hooks visitor node
  in
  let (visitor, action) =
    match action with
    | Skip_subtree -> (visitor, Skip_subtree)
    | Continue -> enter_module_functor_parameter_hooks visitor node
  in
  let visitor =
    match action with
    | Skip_subtree -> visitor
    | Continue ->
        A.Node.fold_child
          node
          ~init:visitor
          ~fn:(fun __tmp1 ->
            match __tmp1 with
            | Syntax_tree.Node id ->
                fun current -> A.Continue (visit_node current (node_of_child node id))
            | Syntax_tree.Token id ->
                fun current -> A.Continue (call_enter_token current (token_of_child node id))
            | Syntax_tree.Missing _ -> fun current -> A.Continue current)
  in
  call_leave_node visitor node

let visit_source_file = fun visitor (source_file: A.SourceFile.t) ->
  visit_node
    visitor
    (A.SourceFile.as_node source_file)

let visit_implementation = fun visitor (implementation: A.Implementation.t) ->
  visit_node
    visitor
    (A.Implementation.as_node implementation)

let visit_interface = fun visitor (interface: A.Interface.t) ->
  visit_node
    visitor
    (A.Interface.as_node interface)

let visit_structure_item = fun visitor (item: A.StructureItem.t) ->
  visit_node
    visitor
    (A.StructureItem.as_node item)

let visit_signature_item = fun visitor (item: A.SignatureItem.t) ->
  visit_node
    visitor
    (A.SignatureItem.as_node item)

let visit_let_declaration = fun visitor (decl: A.LetDeclaration.t) ->
  visit_node
    visitor
    (A.LetDeclaration.as_node decl)

let visit_let_binding = fun visitor (binding: A.LetBinding.t) ->
  visit_node
    visitor
    (A.LetBinding.as_node binding)

let visit_type_declaration = fun visitor (decl: A.TypeDeclaration.t) ->
  visit_node
    visitor
    (A.TypeDeclaration.as_node decl)

let visit_type_extension_declaration = fun visitor (decl: A.TypeExtensionDeclaration.t) ->
  visit_node
    visitor
    (A.TypeExtensionDeclaration.as_node decl)

let visit_module_declaration = fun visitor (decl: A.ModuleDeclaration.t) ->
  visit_node
    visitor
    (A.ModuleDeclaration.as_node decl)

let visit_module_expr = fun visitor (expr: A.ModuleExpr.t) ->
  visit_node
    visitor
    (A.ModuleExpr.as_node expr)

let visit_module_type_expr = fun visitor (expr: A.ModuleTypeExpr.t) ->
  visit_node
    visitor
    (A.ModuleTypeExpr.as_node expr)

let visit_module_type_declaration = fun visitor (decl: A.ModuleTypeDeclaration.t) ->
  visit_node
    visitor
    (A.ModuleTypeDeclaration.as_node decl)

let visit_module_type_constraint = fun visitor (constraint_: A.ModuleTypeConstraint.t) ->
  visit_node
    visitor
    (A.ModuleTypeConstraint.as_node constraint_)

let visit_open_declaration = fun visitor (decl: A.OpenDeclaration.t) ->
  visit_node
    visitor
    (A.OpenDeclaration.as_node decl)

let visit_include_declaration = fun visitor (decl: A.IncludeDeclaration.t) ->
  visit_node
    visitor
    (A.IncludeDeclaration.as_node decl)

let visit_value_declaration = fun visitor (decl: A.ValueDeclaration.t) ->
  visit_node
    visitor
    (A.ValueDeclaration.as_node decl)

let visit_external_declaration = fun visitor (decl: A.ExternalDeclaration.t) ->
  visit_node
    visitor
    (A.ExternalDeclaration.as_node decl)

let visit_exception_declaration = fun visitor (decl: A.ExceptionDeclaration.t) ->
  visit_node
    visitor
    (A.ExceptionDeclaration.as_node decl)

let visit_extension_item = fun visitor (item: A.ExtensionItem.t) ->
  visit_node
    visitor
    (A.ExtensionItem.as_node item)

let visit_attribute_item = fun visitor (item: A.AttributeItem.t) ->
  visit_node
    visitor
    (A.AttributeItem.as_node item)

let visit_expr_item = fun visitor (item: A.ExprItem.t) ->
  visit_node
    visitor
    (A.ExprItem.as_node item)

let visit_expr = fun visitor (expr: A.Expr.t) -> visit_node visitor (A.Expr.as_node expr)

let visit_pattern = fun visitor (pattern: A.Pattern.t) ->
  visit_node
    visitor
    (A.Pattern.as_node pattern)

let visit_parameter = fun visitor (parameter: A.Parameter.t) ->
  visit_node
    visitor
    (A.Parameter.as_node parameter)

let visit_match_case = fun visitor (case: A.MatchCase.t) ->
  visit_node
    visitor
    (A.MatchCase.as_node case)

let visit_type_expr = fun visitor (type_expr: A.TypeExpr.t) ->
  visit_node
    visitor
    (A.TypeExpr.as_node type_expr)

let visit_record_type = fun visitor (record_type: A.RecordType.t) ->
  visit_node
    visitor
    (A.RecordType.as_node record_type)

let visit_record_field = fun visitor (field: A.RecordField.t) ->
  visit_node
    visitor
    (A.RecordField.as_node field)

let visit_record_expr_field = fun visitor (field: A.RecordExprField.t) ->
  visit_node
    visitor
    (A.RecordExprField.as_node field)

let visit_variant_type = fun visitor (variant_type: A.VariantType.t) ->
  visit_node
    visitor
    (A.VariantType.as_node variant_type)

let visit_variant_constructor = fun visitor (constructor: A.VariantConstructor.t) ->
  visit_node
    visitor
    (A.VariantConstructor.as_node constructor)

let visit_ident = fun visitor (ident: A.Ident.t) ->
  A.Ident.fold_token
    ident
    ~init:visitor
    ~fn:(fun token current -> A.Continue (call_enter_token current token))
