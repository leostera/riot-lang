open Std

let rule_id = "ordered-argument-kinds"

let rule_description = "Function parameters should be ordered as labeled, then optional, then positional"

let rule_explain = {|
This rule enforces one stable parameter layout: labeled arguments first, optional
arguments next, and positional arguments last.

That order keeps the configurable surface of a function near the front of the
signature. Readers can see the knobs first and the required positional data afterward.
When positional parameters come first, the named part of the API is easier to miss and
call sites become less uniform.

The goal is not theoretical purity. It is to make function signatures easier to skim
and easier to keep consistent across a codebase.
|}

let kind_rank = function
  | Syn.Cst.Parameter.Labeled _ -> Some 0
  | Syn.Cst.Parameter.Optional _ -> Some 1
  | Syn.Cst.Parameter.Positional _ -> Some 2
  | Syn.Cst.Parameter.LocallyAbstract _ -> None

let kind_name = function
  | Syn.Cst.Parameter.Labeled _ -> "labeled"
  | Syn.Cst.Parameter.Optional _ -> "optional"
  | Syn.Cst.Parameter.Positional _ -> "positional"
  | Syn.Cst.Parameter.LocallyAbstract _ -> "locally abstract"

let parameter_span = fun parameter ->
  Syn.Cst.Parameter.syntax_node parameter |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic = fun parameter ->
  let current_kind = kind_name parameter in
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known {rule_id;message = rule_description;})
    ~span:(parameter_span parameter)
    ~suggestion:(("Move this " ^ current_kind ^ " argument earlier so parameters stay ordered as labeled, optional, then positional"))
    ()

let diagnostic_for_binding = fun binding ->
  let rec go = fun highest_rank diagnostics ->
    function
    | [] -> List.rev diagnostics
    | parameter :: rest -> (
        match kind_rank parameter with
        | None -> go highest_rank diagnostics rest
        | Some rank ->
            let next_highest = Int.max highest_rank rank in
            if rank < highest_rank then
              go next_highest (make_diagnostic parameter :: diagnostics) rest
            else
              go next_highest diagnostics rest
      )
  in
  match Syn.Cst.LetBinding.parameters binding |> go (-1) [] with
  | diagnostic :: _ -> Some diagnostic
  | [] -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.concat_map Traversal.let_bindings_of_structure_item
  |> List.filter Syn.Cst.LetBinding.is_function
  |> List.filter_map diagnostic_for_binding

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
