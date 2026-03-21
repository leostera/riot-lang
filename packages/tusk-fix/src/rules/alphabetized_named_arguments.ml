open Std

let rule_id = "alphabetized-named-arguments"
let rule_name = "Alphabetized Named Arguments"

let rule_description =
  "Labeled and optional arguments should be alphabetized within their groups"

let parameter_name parameter =
  match Syn.Cst.Parameter.name parameter with
  | Some name -> name
  | None -> ""

let parameter_span parameter =
  Syn.Cst.Parameter.syntax_node parameter
  |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic ~previous_name parameter =
  let current_name = parameter_name parameter in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known Diagnostic_code.SortedNamedArguments)
    ~span:(parameter_span parameter)
    ~suggestion:
      ("Place " ^ current_name ^ " before " ^ previous_name
     ^ " so named arguments stay alphabetized")
    ()

let classify_parameter = function
  | Syn.Cst.Parameter.Labeled _ as parameter -> Some (`Labeled, parameter)
  | Syn.Cst.Parameter.Optional _ as parameter -> Some (`Optional, parameter)
  | Syn.Cst.Parameter.Positional _
  | Syn.Cst.Parameter.LocallyAbstract _
  | Syn.Cst.Parameter.Unknown _ ->
      None

let first_out_of_order parameters =
  let rec go last_name = function
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

let diagnostic_for_binding binding =
  let labeled_params, optional_params =
    Syn.Cst.LetBinding.parameters binding
    |> List.filter_map classify_parameter
    |> List.fold_left
         (fun (labeled, optional) (kind, parameter) ->
           match kind with
           | `Labeled -> (labeled @ [ parameter ], optional)
           | `Optional -> (labeled, optional @ [ parameter ]))
         ([], [])
  in
  match first_out_of_order labeled_params with
  | Some (previous_name, parameter) ->
      Some (make_diagnostic ~previous_name parameter)
  | None -> (
      match first_out_of_order optional_params with
      | Some (previous_name, parameter) ->
          Some (make_diagnostic ~previous_name parameter)
      | None -> None)

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~name:rule_name ~description:rule_description
    ~run:check_tree ()
