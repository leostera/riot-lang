open Std

let rule_id = "t-first-named-arguments"

let rule_description = "When named arguments are present, keep t as the first positional argument"

let rule_explain = {|
In Riot-style APIs, `t` often plays the role of the receiver or primary state value.
When that is true, callers benefit from seeing it in the same place consistently.

Keeping `t` as the first positional argument after any named arguments makes pipeline
use and method-like reading more predictable. A function such as
`render ~width ~height t` reads naturally as "render this `t` with these options".

If `t` is buried after other positional arguments, callers have to relearn the calling
convention for each function instead of relying on a stable pattern.
|}

let parameter_span = fun parameter ->
  Syn.Cst.Parameter.syntax_node parameter |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic = fun parameter ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(parameter_span parameter)
    ~suggestion:"Move t to the front of the positional arguments so the function reads as named configuration followed by the receiver"
    ()

let diagnostic_for_binding = fun binding ->
  let parameters = Syn.Cst.LetBinding.parameters binding in
  let has_named_args = List.exists Syn.Cst.Parameter.is_named parameters in
  if not has_named_args then
    None
  else
    let positional_params =
      parameters
      |> List.filter
        (
          function
          | Syn.Cst.Parameter.Positional _ -> true
          | Syn.Cst.Parameter.Labeled _
          | Syn.Cst.Parameter.Optional _
          | Syn.Cst.Parameter.LocallyAbstract _ -> false
        )
    in
    match positional_params with
    | [] -> None
    | first :: _ ->
        let first_name = Syn.Cst.Parameter.name first in
        if first_name = Some "t" then
          None
        else if
          List.exists (fun parameter -> Syn.Cst.Parameter.name parameter = Some "t") positional_params
        then
          positional_params
          |> List.find_opt (fun parameter -> Syn.Cst.Parameter.name parameter = Some "t")
          |> Option.map make_diagnostic
        else
          None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.filter Syn.Cst.LetBinding.is_function
  |> List.filter_map diagnostic_for_binding

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
