open Std
open Std.Collections

type red_tree = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_node = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_node
type red_token = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_token
type red_element = (Syn.SyntaxKind.t, string) Syn.Ceibo.Red.syntax_element

let is_trivia kind =
  let open Syn.SyntaxKind in
  kind = WHITESPACE || kind = COMMENT || kind = DOCSTRING

(* Core traversal that collects elements *)
let traverse ~visit_node ~visit_token tree =
  let open Syn.Ceibo.Red in
  let rec go elem acc =
    match elem with
    | Node n ->
        let acc = visit_node n acc in
        let children = SyntaxNode.children n in
        let result = ref acc in
        for i = 0 to Array.length children - 1 do
          result := go children.(i) !result
        done;
        !result
    | Token t -> visit_token t acc
  in
  go (Node tree) []

(* Find nodes matching predicate *)
let find_nodes predicate tree =
  traverse
    ~visit_node:(fun node acc -> if predicate node then node :: acc else acc)
    ~visit_token:(fun _token acc -> acc)
    tree
  |> List.rev

(* Find nodes by kind *)
let find_by_kind kind tree =
  find_nodes
    (fun node ->
      let open Syn.Ceibo.Red in
      SyntaxNode.kind node = kind)
    tree

(* Find nodes by multiple kinds *)
let find_by_kinds kinds tree =
  find_nodes
    (fun node ->
      let open Syn.Ceibo.Red in
      List.mem (SyntaxNode.kind node) kinds)
    tree

(* Find tokens matching predicate *)
let find_tokens predicate tree =
  traverse
    ~visit_node:(fun _node acc -> acc)
    ~visit_token:(fun token acc -> if predicate token then token :: acc else acc)
    tree
  |> List.rev

(* First non-trivia child *)
let first_non_trivia_child node =
  let open Syn.Ceibo.Red in
  let children = SyntaxNode.children node in
  let rec find i =
    if i >= Array.length children then None
    else
      match children.(i) with
      | Token t when is_trivia (SyntaxToken.kind t) -> find (i + 1)
      | elem -> Some elem
  in
  find 0

(* First non-trivia token *)
let first_non_trivia_token node =
  match first_non_trivia_child node with
  | Some (Syn.Ceibo.Red.Token t) -> Some t
  | _ -> None

(* Visitor pattern *)
type 'acc visitor = {
  visit_node : red_node -> 'acc -> 'acc;
  visit_token : red_token -> 'acc -> 'acc;
}

let fold visitor init tree =
  let open Syn.Ceibo.Red in
  let rec go elem acc =
    match elem with
    | Node n ->
        let acc = visitor.visit_node n acc in
        let children = SyntaxNode.children n in
        let result = ref acc in
        for i = 0 to Array.length children - 1 do
          result := go children.(i) !result
        done;
        !result
    | Token t -> visitor.visit_token t acc
  in
  go (Node tree) init

let rec let_bindings_of_module_expression = function
  | Syn.Cst.ModuleExpression.Path _
  | Syn.Cst.ModuleExpression.Structure _
  | Syn.Cst.ModuleExpression.Extension _ ->
      []
  | Syn.Cst.ModuleExpression.Functor { body; _ } ->
      let_bindings_of_module_expression body
  | Syn.Cst.ModuleExpression.Apply { callee; argument; _ } ->
      let_bindings_of_module_expression callee
      @ let_bindings_of_module_expression argument
  | Syn.Cst.ModuleExpression.ApplyUnit { callee; _ } ->
      let_bindings_of_module_expression callee
  | Syn.Cst.ModuleExpression.Constraint { module_expression; _ }
  | Syn.Cst.ModuleExpression.Attribute { module_expression; _ } ->
      let_bindings_of_module_expression module_expression
  | Syn.Cst.ModuleExpression.Unpack { expression; _ } ->
      let_bindings_of_expression expression
  | Syn.Cst.ModuleExpression.Parenthesized { inner; _ } ->
      let_bindings_of_module_expression inner

and let_bindings_of_object_member = function
  | Syn.Cst.ObjectMember.Method { body; _ } ->
      Option.to_list body |> List.concat_map let_bindings_of_expression
  | Syn.Cst.ObjectMember.Value { value; _ } ->
      Option.to_list value |> List.concat_map let_bindings_of_expression
  | Syn.Cst.ObjectMember.Inherit { expression; _ } ->
      let_bindings_of_expression expression
  | Syn.Cst.ObjectMember.Extension _ ->
      []
  | Syn.Cst.ObjectMember.Initializer { body; _ } ->
      Option.to_list body |> List.concat_map let_bindings_of_expression

and let_bindings_of_expression expr =
  match expr with
  | Syn.Cst.Expression.Path _
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.New _ ->
      []
  | Syn.Cst.Expression.Constructor { payload; _ } ->
      Option.to_list payload |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.Object { members; _ } ->
      members |> List.concat_map let_bindings_of_object_member
  | Syn.Cst.Expression.PolyVariant { payload; _ } ->
      Option.to_list payload |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.FirstClassModule { module_expression; _ } ->
      let_bindings_of_module_expression module_expression
  | Syn.Cst.Expression.LetModule { module_expression; body; _ } ->
      let_bindings_of_module_expression module_expression
      @ let_bindings_of_expression body
  | Syn.Cst.Expression.LetException { body; _ } ->
      let_bindings_of_expression body
  | Syn.Cst.Expression.Assert { asserted; _ } ->
      let_bindings_of_expression asserted
  | Syn.Cst.Expression.Lazy { body; _ } ->
      let_bindings_of_expression body
  | Syn.Cst.Expression.While { condition; body; _ } ->
      let_bindings_of_expression condition @ let_bindings_of_expression body
  | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } ->
      let_bindings_of_expression start_expr
      @ let_bindings_of_expression end_expr
      @ let_bindings_of_expression body
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let_bindings_of_expression callee
      @ let_bindings_of_apply_argument argument
  | Syn.Cst.Expression.MethodCall { receiver; _ } ->
      let_bindings_of_expression receiver
  | Syn.Cst.Expression.Prefix { operand; _ } ->
      let_bindings_of_expression operand
  | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
      let_bindings_of_expression receiver
  | Syn.Cst.Expression.Index { collection; index; _ } ->
      let_bindings_of_expression collection @ let_bindings_of_expression index
  | Syn.Cst.Expression.ObjectOverride { fields; _ } ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.object_override_field) ->
             Option.to_list field.value |> List.concat_map let_bindings_of_expression)
  | Syn.Cst.Expression.InstanceVariableAssign { value; _ } ->
      let_bindings_of_expression value
  | Syn.Cst.Expression.FieldAssign { target; value; _ } ->
      let_bindings_of_expression (Syn.Cst.Expression.FieldAccess target)
      @ let_bindings_of_expression value
  | Syn.Cst.Expression.Assign { target; value; _ } ->
      let_bindings_of_expression target @ let_bindings_of_expression value
  | Syn.Cst.Expression.Infix { left; right; _ } ->
      let_bindings_of_expression left @ let_bindings_of_expression right
  | Syn.Cst.Expression.Typed { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } ->
      let_bindings_of_expression expression
  | Syn.Cst.Expression.Coerce { expression; _ } ->
      let_bindings_of_expression expression
  | Syn.Cst.Expression.Sequence { left; right; _ } ->
      let_bindings_of_expression left @ let_bindings_of_expression right
  | Syn.Cst.Expression.Tuple { elements; _ }
  | Syn.Cst.Expression.List { elements; _ }
  | Syn.Cst.Expression.Array { elements; _ } ->
      elements |> List.concat_map let_bindings_of_expression
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) ->
      fields
      |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
             let_bindings_of_expression field.value)
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) ->
      let_bindings_of_expression base
      @
      (fields
      |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
             let_bindings_of_expression field.value))
  | Syn.Cst.Expression.LocalOpen { body; _ } ->
      let_bindings_of_expression body
  | Syn.Cst.Expression.Fun { body; _ } ->
      let_bindings_of_function_body body
  | Syn.Cst.Expression.Function { cases; _ } ->
      cases |> List.concat_map let_bindings_of_match_case
  | Syn.Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
      let_bindings_of_expression binding.bound_value
      @
      (and_bindings
      |> List.concat_map (fun ({ bound_value; _ } : Syn.Cst.binding_operator_binding) ->
             let_bindings_of_expression bound_value))
      @ let_bindings_of_expression body
  | Syn.Cst.Expression.Let { bound_value; and_bindings; body; _ } ->
      let_bindings_of_expression bound_value
      @ (and_bindings |> List.concat_map let_bindings_of_let_binding)
      @ let_bindings_of_expression body
  | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
      let_bindings_of_expression scrutinee
      @ (cases |> List.concat_map let_bindings_of_match_case)
  | Syn.Cst.Expression.Try { body; cases; _ } ->
      let_bindings_of_expression body
      @ (cases |> List.concat_map let_bindings_of_match_case)
  | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      let_bindings_of_expression condition
      @ let_bindings_of_expression then_branch
      @
      (Option.to_list else_branch |> List.concat_map let_bindings_of_expression)
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      let_bindings_of_expression inner

and let_bindings_of_function_body = function
  | Syn.Cst.Expression expression ->
      let_bindings_of_expression expression
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.concat_map let_bindings_of_match_case

and let_bindings_of_apply_argument = function
  | Syn.Cst.Positional argument ->
      let_bindings_of_expression argument
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      Option.to_list value |> List.concat_map let_bindings_of_expression

and let_bindings_of_let_binding binding =
  binding :: let_bindings_of_expression (Syn.Cst.LetBinding.value binding)

and let_bindings_of_match_case ({ guard; body; _ } : Syn.Cst.match_case) =
  (Option.to_list guard |> List.concat_map let_bindings_of_expression)
  @ let_bindings_of_expression body

and let_bindings_of_class_field = function
  | Syn.Cst.ClassField.Method { body; _ } ->
      Option.to_list body |> List.concat_map let_bindings_of_expression
  | Syn.Cst.ClassField.Value { value; _ } ->
      Option.to_list value |> List.concat_map let_bindings_of_expression
  | Syn.Cst.ClassField.Inherit { class_expression; _ } ->
      let_bindings_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ ->
      []
  | Syn.Cst.ClassField.Initializer { body; _ } ->
      Option.to_list body |> List.concat_map let_bindings_of_expression
  | Syn.Cst.ClassField.Attribute { field; _ } ->
      let_bindings_of_class_field field
  | Syn.Cst.ClassField.Extension _ ->
      []

and let_bindings_of_class_expression = function
  | Syn.Cst.ClassExpression.Path _ | Syn.Cst.ClassExpression.Extension _ ->
      []
  | Syn.Cst.ClassExpression.Structure { fields; _ } ->
      fields |> List.concat_map let_bindings_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } ->
      let_bindings_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } ->
      let_bindings_of_class_expression callee
      @ let_bindings_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let { bound_value; and_bindings; body; _ } ->
      let_bindings_of_expression bound_value
      @ (and_bindings |> List.concat_map let_bindings_of_let_binding)
      @ let_bindings_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } ->
      let_bindings_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen { class_expression; _ } ->
      let_bindings_of_class_expression class_expression
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } ->
      let_bindings_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } ->
      let_bindings_of_class_expression class_expression

let let_bindings_of_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      let_bindings_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      let_bindings_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } ->
      Option.to_list class_body |> List.concat_map let_bindings_of_class_expression
  | Syn.Cst.StructureItem.ModuleDeclaration decl ->
      Option.to_list (Syn.Cst.ModuleDeclaration.module_expression decl)
      |> List.concat_map let_bindings_of_module_expression
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration decl ->
      Syn.Cst.RecursiveModuleDeclaration.declarations decl
      |> List.concat_map (fun nested_decl ->
             Option.to_list (Syn.Cst.ModuleDeclaration.module_expression nested_decl)
             |> List.concat_map let_bindings_of_module_expression)
  | Syn.Cst.StructureItem.OpenStatement stmt -> (
      match Syn.Cst.OpenStatement.module_expression stmt with
      | Some expr -> let_bindings_of_module_expression expr
      | None -> [])
  | Syn.Cst.StructureItem.IncludeStatement { target; _ } -> (
      match target with
      | Syn.Cst.ModuleExpression expr -> let_bindings_of_module_expression expr
      | Syn.Cst.ModuleType _ -> [])
  | Syn.Cst.StructureItem.TypeDeclaration _
  | Syn.Cst.StructureItem.TypeExtension _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _
  | Syn.Cst.StructureItem.ClassTypeDeclaration _
  | Syn.Cst.StructureItem.ModuleTypeDeclaration _
  | Syn.Cst.StructureItem.ValueDeclaration _
  | Syn.Cst.StructureItem.ExternalDeclaration _
  | Syn.Cst.StructureItem.ExceptionDeclaration _ ->
      []

let rec expressions_of_expression expr =
  let nested =
    match expr with
    | Syn.Cst.Expression.Path _
    | Syn.Cst.Expression.Operator _
    | Syn.Cst.Expression.Literal _
    | Syn.Cst.Expression.Unreachable _
    | Syn.Cst.Expression.Extension _
    | Syn.Cst.Expression.New _ ->
        []
    | Syn.Cst.Expression.Constructor { payload; _ } ->
        Option.to_list payload |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.Object { members; _ } ->
        members
        |> List.concat_map (function
             | Syn.Cst.ObjectMember.Method { body; _ } ->
                 Option.to_list body |> List.concat_map expressions_of_expression
             | Syn.Cst.ObjectMember.Value { value; _ } ->
                 Option.to_list value |> List.concat_map expressions_of_expression
             | Syn.Cst.ObjectMember.Inherit { expression; _ } ->
                 expressions_of_expression expression
             | Syn.Cst.ObjectMember.Extension _ ->
                 []
             | Syn.Cst.ObjectMember.Initializer { body; _ } ->
                 Option.to_list body |> List.concat_map expressions_of_expression)
    | Syn.Cst.Expression.PolyVariant { payload; _ } ->
        Option.to_list payload |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.FirstClassModule _ ->
        []
    | Syn.Cst.Expression.LetModule { body; _ } ->
        expressions_of_expression body
    | Syn.Cst.Expression.LetException { body; _ } ->
        expressions_of_expression body
    | Syn.Cst.Expression.Assert { asserted; _ } ->
        expressions_of_expression asserted
    | Syn.Cst.Expression.Lazy { body; _ } ->
        expressions_of_expression body
    | Syn.Cst.Expression.While { condition; body; _ } ->
        expressions_of_expression condition @ expressions_of_expression body
    | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } ->
        expressions_of_expression start_expr
        @ expressions_of_expression end_expr
        @ expressions_of_expression body
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        expressions_of_expression callee
        @ expressions_of_apply_argument argument
    | Syn.Cst.Expression.MethodCall { receiver; _ } ->
        expressions_of_expression receiver
    | Syn.Cst.Expression.Prefix { operand; _ } ->
        expressions_of_expression operand
    | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
        expressions_of_expression receiver
    | Syn.Cst.Expression.Index { collection; index; _ } ->
        expressions_of_expression collection @ expressions_of_expression index
    | Syn.Cst.Expression.ObjectOverride { fields; _ } ->
        fields
        |> List.concat_map (fun (field : Syn.Cst.object_override_field) ->
               Option.to_list field.value |> List.concat_map expressions_of_expression)
    | Syn.Cst.Expression.InstanceVariableAssign { value; _ } ->
        expressions_of_expression value
    | Syn.Cst.Expression.FieldAssign { target; value; _ } ->
        expressions_of_expression (Syn.Cst.Expression.FieldAccess target)
        @ expressions_of_expression value
    | Syn.Cst.Expression.Assign { target; value; _ } ->
        expressions_of_expression target @ expressions_of_expression value
    | Syn.Cst.Expression.Infix { left; right; _ } ->
        expressions_of_expression left @ expressions_of_expression right
    | Syn.Cst.Expression.Typed { expression; _ }
    | Syn.Cst.Expression.Polymorphic { expression; _ } ->
        expressions_of_expression expression
    | Syn.Cst.Expression.Coerce { expression; _ } ->
        expressions_of_expression expression
    | Syn.Cst.Expression.Sequence { left; right; _ } ->
        expressions_of_expression left @ expressions_of_expression right
    | Syn.Cst.Expression.Tuple { elements; _ }
    | Syn.Cst.Expression.List { elements; _ }
    | Syn.Cst.Expression.Array { elements; _ } ->
        elements |> List.concat_map expressions_of_expression
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) ->
        fields
        |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
               expressions_of_expression field.value)
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) ->
        expressions_of_expression base
        @
        (fields
        |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
               expressions_of_expression field.value))
    | Syn.Cst.Expression.LocalOpen { body; _ } ->
        expressions_of_expression body
    | Syn.Cst.Expression.Fun { body; _ } ->
        expressions_of_function_body body
    | Syn.Cst.Expression.Function { cases; _ } ->
        cases |> List.concat_map expressions_of_match_case
    | Syn.Cst.Expression.LetOperator { binding; and_bindings; body; _ } ->
        expressions_of_expression binding.bound_value
        @
        (and_bindings
        |> List.concat_map (fun ({ bound_value; _ } : Syn.Cst.binding_operator_binding) ->
               expressions_of_expression bound_value))
        @ expressions_of_expression body
    | Syn.Cst.Expression.Let { bound_value; and_bindings; body; _ } ->
        expressions_of_expression bound_value
        @ (and_bindings |> List.concat_map expressions_of_let_binding)
        @ expressions_of_expression body
    | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
        expressions_of_expression scrutinee
        @ (cases |> List.concat_map expressions_of_match_case)
    | Syn.Cst.Expression.Try { body; cases; _ } ->
        expressions_of_expression body
        @ (cases |> List.concat_map expressions_of_match_case)
    | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
        expressions_of_expression condition
        @ expressions_of_expression then_branch
        @
        (Option.to_list else_branch |> List.concat_map expressions_of_expression)
    | Syn.Cst.Expression.Parenthesized { inner; _ } ->
        expressions_of_expression inner
  in
  expr :: nested

and expressions_of_function_body = function
  | Syn.Cst.Expression expression ->
      expressions_of_expression expression
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.concat_map expressions_of_match_case

and expressions_of_apply_argument = function
  | Syn.Cst.Positional argument ->
      expressions_of_expression argument
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      Option.to_list value |> List.concat_map expressions_of_expression

and expressions_of_let_binding binding =
  expressions_of_expression (Syn.Cst.LetBinding.value binding)

and expressions_of_match_case ({ guard; body; _ } : Syn.Cst.match_case) =
  (Option.to_list guard |> List.concat_map expressions_of_expression)
  @ expressions_of_expression body

and expressions_of_class_field = function
  | Syn.Cst.ClassField.Method { body; _ } ->
      Option.to_list body |> List.concat_map expressions_of_expression
  | Syn.Cst.ClassField.Value { value; _ } ->
      Option.to_list value |> List.concat_map expressions_of_expression
  | Syn.Cst.ClassField.Inherit { class_expression; _ } ->
      expressions_of_class_expression class_expression
  | Syn.Cst.ClassField.Constraint _ ->
      []
  | Syn.Cst.ClassField.Initializer { body; _ } ->
      Option.to_list body |> List.concat_map expressions_of_expression
  | Syn.Cst.ClassField.Attribute { field; _ } ->
      expressions_of_class_field field
  | Syn.Cst.ClassField.Extension _ ->
      []

and expressions_of_class_expression = function
  | Syn.Cst.ClassExpression.Path _ | Syn.Cst.ClassExpression.Extension _ ->
      []
  | Syn.Cst.ClassExpression.Structure { fields; _ } ->
      fields |> List.concat_map expressions_of_class_field
  | Syn.Cst.ClassExpression.Fun { body; _ } ->
      expressions_of_class_expression body
  | Syn.Cst.ClassExpression.Apply { callee; argument; _ } ->
      expressions_of_class_expression callee
      @ expressions_of_apply_argument argument
  | Syn.Cst.ClassExpression.Let { bound_value; and_bindings; body; _ } ->
      expressions_of_expression bound_value
      @ (and_bindings |> List.concat_map expressions_of_let_binding)
      @ expressions_of_class_expression body
  | Syn.Cst.ClassExpression.Constraint { class_expression; _ } ->
      expressions_of_class_expression class_expression
  | Syn.Cst.ClassExpression.LocalOpen { class_expression; _ } ->
      expressions_of_class_expression class_expression
  | Syn.Cst.ClassExpression.Parenthesized { inner; _ } ->
      expressions_of_class_expression inner
  | Syn.Cst.ClassExpression.Attribute { class_expression; _ } ->
      expressions_of_class_expression class_expression

let expressions_of_structure_item = function
  | Syn.Cst.StructureItem.TypeDeclaration _
  | Syn.Cst.StructureItem.TypeExtension _
  | Syn.Cst.StructureItem.ModuleDeclaration _
  | Syn.Cst.StructureItem.RecursiveModuleDeclaration _
  | Syn.Cst.StructureItem.ModuleTypeDeclaration _
  | Syn.Cst.StructureItem.OpenStatement _
  | Syn.Cst.StructureItem.ValueDeclaration _
  | Syn.Cst.StructureItem.ExternalDeclaration _
  | Syn.Cst.StructureItem.IncludeStatement _
  | Syn.Cst.StructureItem.ExceptionDeclaration _
  | Syn.Cst.StructureItem.ClassTypeDeclaration _
  | Syn.Cst.StructureItem.Attribute _
  | Syn.Cst.StructureItem.Extension _ ->
      []
  | Syn.Cst.StructureItem.LetBinding binding ->
      expressions_of_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      expressions_of_expression expr
  | Syn.Cst.StructureItem.ClassDeclaration { class_body; _ } ->
      Option.to_list class_body |> List.concat_map expressions_of_class_expression
