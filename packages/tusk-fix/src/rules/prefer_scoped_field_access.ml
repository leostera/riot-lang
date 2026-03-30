open Std

let rec binding_operator_group_items (binding : Syn.Cst.binding_operator_binding) =
  binding
  :: (match binding.and_binding with
     | Some next -> binding_operator_group_items next
     | None -> [])

let rule_id = "prefer-scoped-field-access"
let rule_description =
  "Module-qualified field access should use scoped qualification syntax"

let rule_explain =
  {|
Prefer scoped module qualification for record field access.

The awkward form this rule is trying to avoid is qualification inserted into the middle
of another shape. `record.Module.field` separates the receiver from the field. Repeated
field qualification in record literals does the same thing by making the module prefix
compete with the actual field names.

Scoped qualification reads more cleanly because the module is introduced once around
the whole expression. `Module.(record.field)`, `Module.{ field = value }`, and
`Libc.[| epipe; enoent |]` all keep the expression's natural shape intact while still
making the namespace explicit.

This is mostly a readability rule. It keeps the important syntactic relationship next
to itself instead of interrupting it with repeated qualification.
|}

let is_module_like_name name =
  String.length name > 0
  &&
  let ch = String.get name 0 in
  ch >= 'A' && ch <= 'Z'

let receiver_looks_like_record = function
  | Syn.Cst.Expression.Path { path; _ } -> (
      match Syn.Cst.Ident.name path with
      | Some name -> not (is_module_like_name name)
      | None -> true)
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Parenthesized _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Record _
  | Syn.Cst.Expression.Tuple _
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.Sequence _
  | Syn.Cst.Expression.LocalOpen _ ->
      true
  | _ ->
      false

let make_diagnostic ({ syntax_node; _ } : Syn.Cst.field_access_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Prefer Module.(value.field) style for module-qualified record access."
    ()

let make_record_diagnostic syntax_node =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:
      "Prefer Module.{ field = value } style when all record fields share the same module qualifier."
    ()

let make_local_open_diagnostic syntax_node =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:
      "Prefer Module.[ ... ] or Module.( ... ) shorthand over let open when the body is a single bracketed form."
    ()

let ident_prefix_name ident =
  match Syn.Cst.Ident.segments ident |> List.map Syn.Cst.Token.text with
  | [] | [ _ ] ->
      None
  | segments ->
      let prefix =
        match List.rev segments with
        | [] | [ _ ] ->
            []
        | _last :: reversed_prefix ->
            List.rev reversed_prefix
      in
      Some (String.concat "." prefix)

let body_supports_scoped_brackets = function
  | Syn.Cst.Expression.Array _
  | Syn.Cst.Expression.List _
  | Syn.Cst.Expression.Index _ ->
      true
  | _ ->
      false

let local_diagnostic_for_expression ~inside_local_open = function
  | Syn.Cst.Expression.FieldAccess
      ({ receiver =
           Syn.Cst.Expression.FieldAccess
             {
               receiver;
               field_name = module_name;
               _;
             };
         field_name = _;
         _;
       } as expr)
    when receiver_looks_like_record receiver
         && is_module_like_name (Syn.Cst.Token.text module_name) ->
      Some (make_diagnostic expr)
  | Syn.Cst.Expression.Record
      ( (Syn.Cst.RecordExpression.Literal { syntax_node; fields; _ }
        | Syn.Cst.RecordExpression.Update { syntax_node; fields; _ }) as _record )
    when fields != []
         &&
         let prefixes =
           fields
           |> List.filter_map (fun (field : Syn.Cst.record_expression_field) ->
                  ident_prefix_name field.field_path)
         in
         List.length prefixes = List.length fields
         &&
         match prefixes with
         | [] ->
             false
         | first :: rest ->
             List.for_all (String.equal first) rest ->
      Some (make_record_diagnostic syntax_node)
  | Syn.Cst.Expression.LocalOpen
      { syntax_node; body; via_let_open = true; _ }
    when (not inside_local_open) && body_supports_scoped_brackets body ->
      Some (make_local_open_diagnostic syntax_node)
  | _ -> None

let rec diagnostics_for_function_body ~inside_local_open = function
  | Syn.Cst.Expression expr ->
      diagnostics_for_expression ~inside_local_open expr
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.concat_map (diagnostics_for_match_case ~inside_local_open)

and diagnostics_for_expression ~inside_local_open expr =
  let here = Option.to_list (local_diagnostic_for_expression ~inside_local_open expr) in
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
        Option.to_list payload |> List.concat_map (diagnostics_for_expression ~inside_local_open)
    | Syn.Cst.Expression.Object { members; _ } ->
        members |> List.concat_map (diagnostics_for_object_member ~inside_local_open)
    | Syn.Cst.Expression.PolyVariant { payload; _ } ->
        Option.to_list payload |> List.concat_map (diagnostics_for_expression ~inside_local_open)
    | Syn.Cst.Expression.ModulePack _
    | Syn.Cst.Expression.LetException _ ->
        []
    | Syn.Cst.Expression.LetModule { body; _ } ->
        diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.Assert { asserted; _ } ->
        diagnostics_for_expression ~inside_local_open asserted
    | Syn.Cst.Expression.Lazy { body; _ } ->
        diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.While { condition; body; _ } ->
        diagnostics_for_expression ~inside_local_open condition
        @ diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } ->
        diagnostics_for_expression ~inside_local_open start_expr
        @ diagnostics_for_expression ~inside_local_open end_expr
        @ diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.Apply { callee; argument; _ } ->
        diagnostics_for_expression ~inside_local_open callee
        @ diagnostics_for_apply_argument ~inside_local_open argument
    | Syn.Cst.Expression.MethodCall { receiver; _ } ->
        diagnostics_for_expression ~inside_local_open receiver
    | Syn.Cst.Expression.Prefix { operand; _ } ->
        diagnostics_for_expression ~inside_local_open operand
    | Syn.Cst.Expression.FieldAccess { receiver; _ } ->
        diagnostics_for_expression ~inside_local_open receiver
    | Syn.Cst.Expression.Index { collection; index; _ } ->
        diagnostics_for_expression ~inside_local_open collection
        @ diagnostics_for_expression ~inside_local_open index
    | Syn.Cst.Expression.ObjectOverride { fields; _ } ->
        fields
        |> List.concat_map (fun (field : Syn.Cst.object_override_field) ->
               Option.to_list field.value
               |> List.concat_map (diagnostics_for_expression ~inside_local_open))
    | Syn.Cst.Expression.InstanceVariableAssign { value; _ } ->
        diagnostics_for_expression ~inside_local_open value
    | Syn.Cst.Expression.FieldAssign { target; value; _ } ->
        diagnostics_for_expression ~inside_local_open (Syn.Cst.Expression.FieldAccess target)
        @ diagnostics_for_expression ~inside_local_open value
    | Syn.Cst.Expression.Assign { target; value; _ } ->
        diagnostics_for_expression ~inside_local_open target
        @ diagnostics_for_expression ~inside_local_open value
    | Syn.Cst.Expression.Infix { left; right; _ } ->
        diagnostics_for_expression ~inside_local_open left
        @ diagnostics_for_expression ~inside_local_open right
    | Syn.Cst.Expression.TypeAscription { expression; _ }
    | Syn.Cst.Expression.Polymorphic { expression; _ } ->
        diagnostics_for_expression ~inside_local_open expression
    | Syn.Cst.Expression.Sequence { expressions; _ } ->
        expressions |> List.concat_map (diagnostics_for_expression ~inside_local_open)
    | Syn.Cst.Expression.Tuple { elements; _ }
    | Syn.Cst.Expression.List { elements; _ }
    | Syn.Cst.Expression.Array { elements; _ } ->
        elements |> List.concat_map (diagnostics_for_expression ~inside_local_open)
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) ->
        fields
        |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
               diagnostics_for_expression ~inside_local_open field.value)
    | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) ->
        diagnostics_for_expression ~inside_local_open base
        @
        (fields
        |> List.concat_map (fun (field : Syn.Cst.record_expression_field) ->
               diagnostics_for_expression ~inside_local_open field.value))
    | Syn.Cst.Expression.LocalOpen { body; _ } ->
        diagnostics_for_expression ~inside_local_open:true body
    | Syn.Cst.Expression.Fun { body; _ } ->
        diagnostics_for_function_body ~inside_local_open body
    | Syn.Cst.Expression.Function { cases; _ } ->
        cases |> List.concat_map (diagnostics_for_match_case ~inside_local_open)
    | Syn.Cst.Expression.LetOperator { binding; body; _ } ->
        (binding_operator_group_items binding
        |> List.concat_map (fun ({ bound_value; _ } : Syn.Cst.binding_operator_binding) ->
               diagnostics_for_expression ~inside_local_open bound_value))
        @ diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.Let { bound_value; and_binding; body; _ } ->
        diagnostics_for_expression ~inside_local_open bound_value
        @ (Option.to_list and_binding |> List.concat_map diagnostics_for_let_binding)
        @ diagnostics_for_expression ~inside_local_open body
    | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
        diagnostics_for_expression ~inside_local_open scrutinee
        @ (cases |> List.concat_map (diagnostics_for_match_case ~inside_local_open))
    | Syn.Cst.Expression.Try { body; cases; _ } ->
        diagnostics_for_expression ~inside_local_open body
        @ (cases |> List.concat_map (diagnostics_for_match_case ~inside_local_open))
    | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
        diagnostics_for_expression ~inside_local_open condition
        @ diagnostics_for_expression ~inside_local_open then_branch
        @
        (Option.to_list else_branch
        |> List.concat_map (diagnostics_for_expression ~inside_local_open))
    | Syn.Cst.Expression.Parenthesized { inner; _ } ->
        diagnostics_for_expression ~inside_local_open inner
  in
  here @ nested

and diagnostics_for_apply_argument ~inside_local_open = function
  | Syn.Cst.Positional argument ->
      diagnostics_for_expression ~inside_local_open argument
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      Option.to_list value
      |> List.concat_map (diagnostics_for_expression ~inside_local_open)

and diagnostics_for_let_binding binding =
  diagnostics_for_expression ~inside_local_open:false (Syn.Cst.LetBinding.value binding)

and diagnostics_for_match_case ~inside_local_open ({ guard; body; _ } : Syn.Cst.match_case) =
  (Option.to_list guard
  |> List.concat_map (diagnostics_for_expression ~inside_local_open))
  @ diagnostics_for_expression ~inside_local_open body

and diagnostics_for_object_member ~inside_local_open = function
  | Syn.Cst.ObjectMember.Method { body; _ }
  | Syn.Cst.ObjectMember.Value { value = body; _ }
  | Syn.Cst.ObjectMember.Initializer { body; _ } ->
      diagnostics_for_expression ~inside_local_open body
  | Syn.Cst.ObjectMember.Inherit { expression; _ } ->
      diagnostics_for_expression ~inside_local_open expression
  | Syn.Cst.ObjectMember.Extension _ ->
      []

let diagnostics_for_structure_item = function
  | Syn.Cst.StructureItem.LetBinding binding ->
      diagnostics_for_let_binding binding
  | Syn.Cst.StructureItem.Expression expr ->
      diagnostics_for_expression ~inside_local_open:false expr
  | _ ->
      []

let check_tree (ctx : Rule.context) _red_root =
  let source_file = ctx.cst in
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map diagnostics_for_structure_item

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
