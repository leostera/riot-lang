open Std

let rec binding_operator_group_items (binding : Syn.Cst.binding_operator_binding) =
  binding
  :: (match binding.and_binding with
     | Some next -> binding_operator_group_items next
     | None -> [])

let rule_id = "prefer-record-destructuring-parameters"
let rule_description =
  "Functions that immediately destructure a record argument should destructure it in the parameter"

let rule_explain =
  {|
When a helper function immediately starts with `let { ... } = value in ...`, or spends
its whole body reading `value.name`, `value.email`, and `value.role`, the real shape of
the function is already telling you that it wants a destructured record parameter, not
a temporary name plus a second layer of field lookups.

Writing the destructuring at the parameter boundary makes the contract visible earlier.
Readers can see which fields matter before they enter the function body, and adding a
new record field is harder to ignore accidentally because the pattern is part of the
signature-shaped surface of the helper.

This is especially useful for internal helpers such as serializers, encoders, and view
renderers that consume a record immediately and never use the whole record value on its
own.
|}

let positional_parameter_name_token = function
  | Syn.Cst.Parameter.Positional { name_token = Some token; _ } ->
      Some token
  | _ ->
      None

let single_positional_parameter_name binding =
  let positional_names =
    Syn.Cst.LetBinding.parameters binding
    |> List.filter_map positional_parameter_name_token
  in
  match positional_names with
  | [ token ] -> Some token
  | _ -> None

let rec unwrap_record_pattern = function
  | Syn.Cst.Pattern.Record pattern ->
      Some pattern
  | Syn.Cst.Pattern.Typed { pattern; _ }
  | Syn.Cst.Pattern.Parenthesized { inner = pattern; _ }
  | Syn.Cst.Pattern.Alias { pattern; _ } ->
      unwrap_record_pattern pattern
  | _ ->
      None

let bound_value_is_parameter_name expected_name = function
  | Syn.Cst.Expression.Path { path; _ } ->
      Syn.Cst.Ident.name path
      |> Option.map (String.equal expected_name)
      |> Option.unwrap_or ~default:false
  | _ ->
      false

type parameter_usage = {
  field_names : string list;
  has_whole_value_use : bool;
}

let empty_usage = { field_names = []; has_whole_value_use = false }

let merge_usage left right =
  {
    field_names = left.field_names @ right.field_names;
    has_whole_value_use = left.has_whole_value_use || right.has_whole_value_use;
  }

let merge_all usages =
  List.fold_left merge_usage empty_usage usages

let whole_value_use expected_name = function
  | Syn.Cst.Expression.Path { path; _ } ->
      {
        field_names = [];
        has_whole_value_use =
          (Syn.Cst.Ident.name path
          |> Option.map (String.equal expected_name)
          |> Option.unwrap_or ~default:false);
      }
  | _ ->
      empty_usage

let direct_field_access_name expected_name = function
  | Syn.Cst.Expression.FieldAccess { receiver = Syn.Cst.Expression.Path { path; _ }; field_name; _ } ->
      if
        Syn.Cst.Ident.name path
        |> Option.map (String.equal expected_name)
        |> Option.unwrap_or ~default:false
      then
        Some (Syn.Cst.Token.text field_name)
      else
        None
  | _ ->
      None

let rec usage_in_function_body expected_name = function
  | Syn.Cst.Expression expression ->
      usage_in_expression expected_name expression
  | Syn.Cst.Cases { cases; _ } ->
      cases |> List.map (usage_in_match_case expected_name) |> merge_all

and usage_in_apply_argument expected_name = function
  | Syn.Cst.Positional argument ->
      usage_in_expression expected_name argument
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      Option.to_list value
      |> List.map (usage_in_expression expected_name)
      |> merge_all

and usage_in_match_case expected_name ({ guard; body; _ } : Syn.Cst.match_case) =
  merge_all
    ((Option.to_list guard |> List.map (usage_in_expression expected_name))
    @ [ usage_in_expression expected_name body ])

and usage_in_object_member expected_name = function
  | Syn.Cst.ObjectMember.Method { body; _ } ->
      usage_in_expression expected_name body
  | Syn.Cst.ObjectMember.Value { value; _ } ->
      usage_in_expression expected_name value
  | Syn.Cst.ObjectMember.Inherit { expression; _ } ->
      usage_in_expression expected_name expression
  | Syn.Cst.ObjectMember.Extension _ ->
      empty_usage
  | Syn.Cst.ObjectMember.Initializer { body; _ } ->
      usage_in_expression expected_name body

and usage_in_expression expected_name expr =
  match expr with
  | Syn.Cst.Expression.Path _ ->
      whole_value_use expected_name expr
  | Syn.Cst.Expression.Operator _
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Unreachable _
  | Syn.Cst.Expression.Extension _
  | Syn.Cst.Expression.New _ ->
      empty_usage
  | Syn.Cst.Expression.Constructor { payload; _ } ->
      Option.to_list payload
      |> List.map (usage_in_expression expected_name)
      |> merge_all
  | Syn.Cst.Expression.Object { members; _ } ->
      members |> List.map (usage_in_object_member expected_name) |> merge_all
  | Syn.Cst.Expression.PolyVariant { payload; _ } ->
      Option.to_list payload
      |> List.map (usage_in_expression expected_name)
      |> merge_all
  | Syn.Cst.Expression.ModulePack _ ->
      empty_usage
  | Syn.Cst.Expression.LetModule { body; _ } ->
      usage_in_expression expected_name body
  | Syn.Cst.Expression.LetException { body; _ } ->
      usage_in_expression expected_name body
  | Syn.Cst.Expression.Assert { asserted; _ } ->
      usage_in_expression expected_name asserted
  | Syn.Cst.Expression.Lazy { body; _ } ->
      usage_in_expression expected_name body
  | Syn.Cst.Expression.While { condition; body; _ } ->
      merge_usage
        (usage_in_expression expected_name condition)
        (usage_in_expression expected_name body)
  | Syn.Cst.Expression.For { start_expr; end_expr; body; _ } ->
      merge_all
        [
          usage_in_expression expected_name start_expr;
          usage_in_expression expected_name end_expr;
          usage_in_expression expected_name body;
        ]
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      merge_usage
        (usage_in_expression expected_name callee)
        (usage_in_apply_argument expected_name argument)
  | Syn.Cst.Expression.MethodCall { receiver; _ } ->
      usage_in_expression expected_name receiver
  | Syn.Cst.Expression.Prefix { operand; _ } ->
      usage_in_expression expected_name operand
  | Syn.Cst.Expression.FieldAccess ({ receiver; _ } as field_access) ->
      (match direct_field_access_name expected_name (Syn.Cst.Expression.FieldAccess field_access) with
      | Some field_name ->
          { field_names = [ field_name ]; has_whole_value_use = false }
      | None ->
          usage_in_expression expected_name receiver)
  | Syn.Cst.Expression.Index { collection; index; _ } ->
      merge_usage
        (usage_in_expression expected_name collection)
        (usage_in_expression expected_name index)
  | Syn.Cst.Expression.ObjectOverride { fields; _ } ->
      fields
      |> List.filter_map (fun (field : Syn.Cst.object_override_field) -> field.value)
      |> List.map (usage_in_expression expected_name)
      |> merge_all
  | Syn.Cst.Expression.InstanceVariableAssign { value; _ } ->
      usage_in_expression expected_name value
  | Syn.Cst.Expression.FieldAssign { target; value; _ } ->
      merge_usage
        (usage_in_expression expected_name (Syn.Cst.Expression.FieldAccess target))
        (usage_in_expression expected_name value)
  | Syn.Cst.Expression.Assign { target; value; _ } ->
      merge_usage
        (usage_in_expression expected_name target)
        (usage_in_expression expected_name value)
  | Syn.Cst.Expression.Infix { left; right; _ } ->
      merge_usage
        (usage_in_expression expected_name left)
        (usage_in_expression expected_name right)
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } ->
      usage_in_expression expected_name expression
  | Syn.Cst.Expression.Sequence { expressions; _ } ->
      expressions
      |> List.map (usage_in_expression expected_name)
      |> merge_all
  | Syn.Cst.Expression.Tuple { elements; _ }
  | Syn.Cst.Expression.List { elements; _ }
  | Syn.Cst.Expression.Array { elements; _ } ->
      elements |> List.map (usage_in_expression expected_name) |> merge_all
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Literal { fields; _ }) ->
      fields
      |> List.map (fun (field : Syn.Cst.record_expression_field) ->
             usage_in_expression expected_name field.value)
      |> merge_all
  | Syn.Cst.Expression.Record (Syn.Cst.RecordExpression.Update { base; fields; _ }) ->
      merge_all
        (usage_in_expression expected_name base
        :: List.map
             (fun (field : Syn.Cst.record_expression_field) ->
               usage_in_expression expected_name field.value)
             fields)
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.LetOpen { body; _ })
  | Syn.Cst.Expression.LocalOpen (Syn.Cst.Delimited { body; _ }) ->
      usage_in_expression expected_name body
  | Syn.Cst.Expression.Fun { body; _ } ->
      usage_in_function_body expected_name body
  | Syn.Cst.Expression.Function { cases; _ } ->
      cases |> List.map (usage_in_match_case expected_name) |> merge_all
  | Syn.Cst.Expression.LetOperator { binding; body; _ } ->
      merge_all
        (List.map
             (fun ({ bound_value; _ } : Syn.Cst.binding_operator_binding) ->
               usage_in_expression expected_name bound_value)
             (binding_operator_group_items binding)
        @ [ usage_in_expression expected_name body ])
  | Syn.Cst.Expression.Let { bound_value; and_binding; body; _ } ->
      merge_all
        (usage_in_expression expected_name bound_value
        :: usage_in_expression expected_name body
        :: List.map
             (fun (binding : Syn.Cst.let_binding) ->
               usage_in_expression expected_name binding.value)
             (Option.to_list and_binding |> List.concat_map (fun binding ->
                  binding :: Syn.Cst.LetBinding.and_bindings binding)))
  | Syn.Cst.Expression.Match { scrutinee; cases; _ } ->
      merge_all
        (usage_in_expression expected_name scrutinee
        :: List.map (usage_in_match_case expected_name) cases)
  | Syn.Cst.Expression.Try { body; cases; _ } ->
      merge_all
        (usage_in_expression expected_name body
        :: List.map (usage_in_match_case expected_name) cases)
  | Syn.Cst.Expression.If { condition; then_branch; else_branch; _ } ->
      merge_all
        ((usage_in_expression expected_name condition)
        :: (usage_in_expression expected_name then_branch)
        :: (Option.to_list else_branch
           |> List.map (usage_in_expression expected_name)))
  | Syn.Cst.Expression.Parenthesized { inner; _ } ->
      usage_in_expression expected_name inner

let should_prefer_destructuring expected_name expr =
  let usage = usage_in_expression expected_name expr in
  let distinct_fields = List.sort_uniq String.compare usage.field_names in
  List.length distinct_fields >= 2 && not usage.has_whole_value_use

let is_immediate_record_destructure expected_name = function
  | Syn.Cst.Expression.Let
      {
        binding_pattern;
        bound_value;
        and_binding = None;
        is_recursive = false;
        _;
      } -> (
      match unwrap_record_pattern binding_pattern with
      | Some { fields; _ } ->
          List.length fields >= 2 && bound_value_is_parameter_name expected_name bound_value
      | None ->
          false)
  | _ ->
      false

let diagnostic_for_binding binding =
  match single_positional_parameter_name binding with
  | None ->
      None
  | Some parameter_token ->
      let parameter_name = Syn.Cst.Token.text parameter_token in
      let value = Syn.Cst.LetBinding.value binding in
      if
        is_immediate_record_destructure parameter_name value
        || should_prefer_destructuring parameter_name value
      then
        Some
          (Diagnostic.make ~severity:Warning
             ~kind:(Diagnostic.Known { rule_id; message = rule_description })
             ~span:(Syn.Cst.Token.span parameter_token)
             ~suggestion:
               ("Destructure this record in the parameter list instead of binding "
              ^ parameter_name
              ^ " and immediately unpacking it in the function body")
             ())
      else
        None

let check_tree (ctx : Rule.context) _red_root =
  let source_file = ctx.cst in
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
