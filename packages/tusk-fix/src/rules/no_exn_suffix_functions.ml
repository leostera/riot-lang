open Std
open Std.Collections

let rule_id = "no-exn-suffix-functions"
let rule_name = "No Exn Suffix Functions"
let rule_code = "F0134"

let rule_description =
  "Function names should not end with _exn"

let rule_message =
  "Avoid _exn function names; prefer Result or Option returning APIs."

let rule_explain =
  {|
Avoid naming functions with an `_exn` suffix.

Names like `parse_exn` and `getenv_exn` advertise exception-throwing control flow as part of the normal API surface.
That makes call sites harder to reason about and nudges callers toward exceptions for expected failure cases.
Prefer `Result` or `Option` returning functions, and reserve exceptions for truly exceptional situations.

Examples:
  Avoid:   let parse_exn text = ...
  Better:  let parse text = ...
  Better:  let parse_result text = ...
|}

let should_flag_binding binding =
  Syn.Cst.LetBinding.is_function binding
  && String.ends_with ~suffix:"_exn" (Syn.Cst.LetBinding.name binding)

let make_diagnostic binding =
  let name = Syn.Cst.LetBinding.name binding in
  match Syn.Cst.LetBinding.binding_name_token binding with
  | Some token ->
      Some
        (Diagnostic.make ~severity:Warning
           ~kind:
             (Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
           ~span:(Syn.Cst.Token.syntax_token token |> Syn.Ceibo.Red.SyntaxToken.span)
           ~suggestion:
             ("Rename " ^ name
            ^ " to remove the _exn suffix and prefer a Result/Option API.")
           ())
  | None -> None

let diagnostic_for_binding binding =
  if should_flag_binding binding then
    make_diagnostic binding
  else
    None

let check_tree (ctx : Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some source_file ->
      Syn.Cst.SourceFile.let_bindings source_file
      |> List.filter_map diagnostic_for_binding

let make () =
  Rule.make ~id:rule_id ~code:rule_code ~name:rule_name
    ~description:rule_description ~message:rule_message ~explain:rule_explain
    ~run:check_tree ()
