open Std

let rule_id = "t-first-named-arguments"
let rule_description =
  "When named arguments are present, keep t as the first positional argument"

let rule_explain =
  {|
When a function takes t alongside named arguments, keep t as the first positional argument.

Why this rule exists:
- Riot APIs often treat t as the receiver/state value.
- Keeping t first among positional arguments makes pipeline and method-like usage more predictable.

Examples:
  Better: let render ~width ~height t = ...
  Worse:  let render ~width ~height other t = ...
|}

let parameter_span parameter =
  Syn.Cst.Parameter.syntax_node parameter
  |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic parameter =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(parameter_span parameter)
    ~suggestion:
      "Move t to the front of the positional arguments so the function reads as named configuration followed by the receiver"
    ()

let diagnostic_for_binding binding =
  let parameters = Syn.Cst.LetBinding.parameters binding in
  let has_named_args = List.exists Syn.Cst.Parameter.is_named parameters in
  if not has_named_args then
    None
  else
    let positional_params =
      parameters
      |> List.filter (function
             | Syn.Cst.Parameter.Positional _ -> true
             | Syn.Cst.Parameter.Labeled _
             | Syn.Cst.Parameter.Optional _
             | Syn.Cst.Parameter.LocallyAbstract _ ->
                 false)
    in
    match positional_params with
    | [] -> None
    | first :: _ ->
        let first_name = Syn.Cst.Parameter.name first in
        if first_name = Some "t" then
          None
        else if
          List.exists
            (fun parameter -> Syn.Cst.Parameter.name parameter = Some "t")
            positional_params
        then
          positional_params
          |> List.find_opt (fun parameter ->
                 Syn.Cst.Parameter.name parameter = Some "t")
          |> Option.map make_diagnostic
        else
          None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.structure_items source_file
      |> Option.unwrap_or ~default:[]
      |> List.concat_map Traversal.let_bindings_of_structure_item
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain
    ~run:check_tree ()
