open Std

let rule_id = "ordered-argument-kinds"
let rule_name = "Ordered Argument Kinds"
let rule_code = "F0111"

let rule_description =
  "Function parameters should be ordered as labeled, then optional, then positional"

let rule_message =
  "Order function parameters as labeled, then optional, then positional."

let rule_explain =
  {|
Function parameters should be ordered as:
1. labeled arguments
2. optional arguments
3. positional arguments

Why this rule exists:
- A stable order makes APIs easier to skim.
- Putting positional arguments first tends to bury the configurable surface of the function.
|}

let kind_rank = function
  | Syn.Cst.Parameter.Labeled _ -> Some 0
  | Syn.Cst.Parameter.Optional _ -> Some 1
  | Syn.Cst.Parameter.Positional _ -> Some 2
  | Syn.Cst.Parameter.LocallyAbstract _ ->
      None

let kind_name = function
  | Syn.Cst.Parameter.Labeled _ -> "labeled"
  | Syn.Cst.Parameter.Optional _ -> "optional"
  | Syn.Cst.Parameter.Positional _ -> "positional"
  | Syn.Cst.Parameter.LocallyAbstract _ -> "locally abstract"

let parameter_span parameter =
  Syn.Cst.Parameter.syntax_node parameter
  |> Syn.Ceibo.Red.SyntaxNode.span

let make_diagnostic parameter =
  let current_kind = kind_name parameter in
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(parameter_span parameter)
    ~suggestion:
      ("Move this " ^ current_kind
     ^ " argument earlier so parameters stay ordered as labeled, optional, then positional")
    ()

let diagnostic_for_binding binding =
  let rec go highest_rank diagnostics = function
    | [] -> List.rev diagnostics
    | parameter :: rest -> (
        match kind_rank parameter with
        | None -> go highest_rank diagnostics rest
        | Some rank ->
            let next_highest = Int.max highest_rank rank in
            if rank < highest_rank then
              go next_highest (make_diagnostic parameter :: diagnostics) rest
            else
              go next_highest diagnostics rest)
  in
  match Syn.Cst.LetBinding.parameters binding |> go (-1) [] with
  | diagnostic :: _ -> Some diagnostic
  | [] -> None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter Syn.Cst.LetBinding.is_function
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
