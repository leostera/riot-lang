open Std

let rule_id = "no-function-shorthand"
let rule_name = "No Function Shorthand"
let rule_code = "F0122"

let rule_description =
  "Named functions should avoid `function` shorthand and use explicit parameters instead"

let rule_message =
  "Named functions should avoid `function` shorthand and use explicit parameters instead."

let rule_explain =
  {|
Named functions should avoid `function` shorthand.

Why this rule exists:
- Explicit parameters make function signatures and later refactors easier.
- `function` hides the argument list and pushes structure into the branches immediately.

Examples:
  Bad:    let describe = function | Ok x -> x | Error _ -> "error"
  Better: let describe value = match value with | Ok x -> x | Error _ -> "error"
  Better: let describe = fun value -> match value with | Ok x -> x | Error _ -> "error"
|}

let make_diagnostic binding =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.LetBinding.value_syntax_node binding))
    ~suggestion:"Use explicit parameters with `let name x = ...` or `let name = fun x -> ...`"
    ()

let diagnostic_for_binding binding =
  if Syn.Ceibo.Red.SyntaxNode.kind (Syn.Cst.LetBinding.value_syntax_node binding)
     = Syn.SyntaxKind.FUNCTION_EXPR then
    Some (make_diagnostic binding)
  else
    None

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
