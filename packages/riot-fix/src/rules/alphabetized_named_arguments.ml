open Std

let rule_id = "alphabetized-named-arguments"

let rule_description = "Labeled and optional arguments should be alphabetized within their groups"

let rule_explain = {|
Alphabetizing named arguments is not about claiming that alphabetical order is
semantically meaningful. It is about removing arbitrary ordering decisions from APIs so
readers do not have to wonder whether the order carries meaning when it does not.

Once a project adopts a stable ordering, small edits become easier to review.
Inserted parameters land in an obvious place, missing ones are easier to spot, and
different modules stop inventing slightly different layouts for the same shape of
function.

This rule only applies within the named-argument groups. It is there to reduce
unproductive bikeshedding and make signatures mechanically easier to scan.
|}

let parameter_name = fun parameter ->
  match Syn.Cst.Parameter.name parameter with
  | Some name -> name
  | None -> ""

let parameter_span = fun parameter ->
  Syn.Cst.Parameter.syntax_node parameter |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic = fun ~previous_name parameter ->
  let current_name = parameter_name parameter in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(parameter_span parameter)
    ~suggestion:("Place " ^ current_name ^ " before " ^ previous_name ^ " so named arguments stay alphabetized")
    ()

let classify_parameter parameter =
  match parameter with
  | Syn.Cst.Parameter.Labeled _ as parameter -> Some (`Labeled, parameter)
  | Syn.Cst.Parameter.Optional _ as parameter -> Some (`Optional, parameter)
  | Syn.Cst.Parameter.Positional _
  | Syn.Cst.Parameter.LocallyAbstract _ -> None

let first_out_of_order = fun parameters ->
  let rec go = fun last_name ->
    function
    | [] -> None
    | parameter :: rest ->
        let name = parameter_name parameter in
        if String.compare name last_name < 0 then
          Some (last_name, parameter)
        else
          go name rest
  in
  match parameters with
  | [] -> None
  | parameter :: rest -> go (parameter_name parameter) rest

let diagnostic_for_binding = fun binding ->
  let labeled_params, optional_params =
    Syn.Cst.LetBinding.parameters binding
    |> List.filter_map ~fn:classify_parameter
    |> List.fold_left ~acc:([], [])
      ~fn:(fun (labeled, optional) (kind, parameter) ->
        match kind with
        | `Labeled -> (labeled @ [ parameter ], optional)
        | `Optional -> (labeled, optional @ [ parameter ]))
  in
  match first_out_of_order labeled_params with
  | Some (previous_name, parameter) -> Some (make_diagnostic ~previous_name parameter)
  | None -> (
      match first_out_of_order optional_params with
      | Some (previous_name, parameter) -> Some (make_diagnostic ~previous_name parameter)
      | None -> None
    )

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.map ~fn:Traversal.let_bindings_of_structure_item
  |> List.concat
  |> List.filter ~fn:Syn.Cst.LetBinding.is_function
  |> List.filter_map ~fn:diagnostic_for_binding

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
