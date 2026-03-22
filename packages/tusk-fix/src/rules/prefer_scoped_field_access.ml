open Std

let rule_id = "prefer-scoped-field-access"
let rule_name = "Prefer Scoped Field Access"
let rule_code = "F0140"

let rule_description =
  "Module-qualified field access should use Module.(value.field) style"

let rule_message =
  "Prefer Module.(value.field) over value.Module.field."

let rule_explain =
  {|
Prefer scoped module qualification for record field access.

`value.Module.field` hides the actual field access in the middle of the expression.
`Module.(value.field)` keeps the field access shape intact and scopes the module once around the expression instead of wedging it into the middle.

Examples:
  Avoid:   record.Module.field
  Better:  Module.(record.field)

This keeps the receiver and the field next to each other, which makes record access easier to scan.
|}

let is_module_like_name name =
  String.length name > 0
  &&
  let ch = String.get name 0 in
  ch >= 'A' && ch <= 'Z'

let receiver_looks_like_record = function
  | Syn.Cst.Expression.Path { path; _ } -> (
      match Syn.Cst.ModulePath.name path with
      | Some name -> not (is_module_like_name name)
      | None -> true)
  | Syn.Cst.Expression.FieldAccess _
  | Syn.Cst.Expression.Apply _
  | Syn.Cst.Expression.Parenthesized _
  | Syn.Cst.Expression.Let _
  | Syn.Cst.Expression.Match _
  | Syn.Cst.Expression.Try _
  | Syn.Cst.Expression.If _
  | Syn.Cst.Expression.Unknown _ ->
      true
  | Syn.Cst.Expression.Literal _
  | Syn.Cst.Expression.Infix _
  | Syn.Cst.Expression.Fun _
  | Syn.Cst.Expression.Function _ ->
      false

let make_diagnostic ({ syntax_node; _ } : Syn.Cst.field_access_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Prefer Module.(value.field) style for module-qualified record access."
    ()

let diagnostic_for_expression = function
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
  | _ -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.expressions source_file
      |> List.filter_map diagnostic_for_expression

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
