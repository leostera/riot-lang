open Std

let rule_id = "no-redundant-reraise"
let rule_name = "No Redundant Reraise"
let rule_code = "F0138"

let rule_description =
  "try handlers that only re-raise the same exception should be removed"

let rule_message =
  "Remove try handlers that only re-raise the same exception."

let rule_explain =
  {|
Avoid `try ... with exn -> raise exn` handlers.

These handlers do not recover, transform, or add context.
They catch an exception only to throw the exact same value again, which leaves the surrounding code longer without changing behavior.
If the handler does nothing but re-raise the same exception, remove the whole `try ... with` and keep the body directly.

Examples:
  Avoid:   try render value with exn -> raise exn
  Better:  render value

Keep the `try ... with` only when the handler adds behavior:
  Good:    try render value with exn -> log_error exn; raise exn
  Good:    try render value with Not_found -> default ()
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_parens inner
  | expr -> expr

let path_name = function
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.ModulePath.name path
  | _ -> None

let rec raises_name name expr =
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply
      {
        callee = Syn.Cst.Expression.Path { path; _ };
        argument;
        _;
      } -> (
      match Syn.Cst.ModulePath.name path, path_name (unwrap_parens argument) with
      | Some "raise", Some argument_name -> String.equal name argument_name
      | _ -> false)
  | _ -> false

let is_redundant_reraise_case ({ pattern; guard; body; _ } : Syn.Cst.match_case) =
  match pattern, guard with
  | Syn.Cst.Pattern.Identifier { name_token; _ }, None ->
      raises_name (Syn.Cst.Token.text name_token) body
  | _ -> false

let make_diagnostic ({ syntax_node; _ } : Syn.Cst.try_expression) =
  Diagnostic.make ~severity:Warning
    ~kind:(Diagnostic.Known { code = rule_code; rule_id; message = rule_message })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Remove this try/with and use the body directly."
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Try ({ cases = [ case ]; _ } as try_expr)
    when is_redundant_reraise_case case ->
      Some (make_diagnostic try_expr)
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
