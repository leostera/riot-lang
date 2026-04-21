open Std

let rule_id = Rule_id.of_string "no-redundant-reraise"

let rule_description = "try handlers that only re-raise the same exception should be removed"

let rule_explain = {|
A handler that catches an exception only to raise the exact same value again does not
recover, add context, or change behavior. It merely inserts extra control flow for the
reader to step through.

If the `try ... with` exists only as `with exn -> raise exn`, delete the whole wrapper
and keep the body directly. Keep the handler only when it adds something real, such as
logging, translation to another exception, or a genuine fallback path.

This rule exists because the empty re-raise pattern looks like important error handling
even when it is doing nothing at all.
|}

let rec unwrap_parens = function
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_parens inner
  | expr -> expr

let path_name = function
  | Syn.Cst.Expression.Path { path; _ } -> Syn.Cst.Ident.name path
  | _ -> None

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ }
  | Syn.Cst.Optional { value; _ } -> value

let rec raises_name = fun name expr ->
  match unwrap_parens expr with
  | Syn.Cst.Expression.Apply { callee=Syn.Cst.Expression.Path { path; _ }; argument; _;  } -> (
      let argument_name =
        match expression_of_apply_argument argument with
        | Some argument -> path_name (unwrap_parens argument)
        | None -> None
      in
      match Syn.Cst.Ident.name path, argument_name with
      | Some "raise", Some argument_name -> String.equal name argument_name
      | _ -> false
    )
  | _ -> false

let is_redundant_reraise_case = fun ({ pattern; guard; body; _ }: Syn.Cst.match_case) ->
  match pattern, guard with
  | Syn.Cst.Pattern.Identifier { name_token; _ }, None -> raises_name
    (Syn.Cst.Token.text name_token)
    body
  | _ -> false

let make_diagnostic = fun ({ syntax_node; body; _ }: Syn.Cst.try_expression) ->
  Diagnostic.make
    ~severity:Warning
    ~kind:(Diagnostic.Known { rule_id; message = rule_description })
    ~span:(Syn.Ceibo.Red.SyntaxNode.span syntax_node)
    ~suggestion:"Remove this try/with and use the body directly."
    ~fix:(Fix.make
      ~title:"Remove redundant try/with reraise"
      ~operations:[
        Fix.replace_node ~target:syntax_node ~replacement:(Syn.Cst.Expression.syntax_node body);
      ])
    ()

let diagnostic_for_expression = function
  | Syn.Cst.Expression.Try ({ cases=[ case ]; _ } as try_expr) when is_redundant_reraise_case case -> Some (make_diagnostic
    try_expr)
  | _ -> None

let check_tree = fun (ctx: Rule.context) _red_root ->
  let source_file = ctx.cst in
  Syn.Cst.SourceFile.structure_items source_file
  |> Option.unwrap_or ~default:[]
  |> List.map ~fn:Traversal.expressions_of_structure_item
  |> List.concat
  |> List.filter_map ~fn:diagnostic_for_expression

let make = fun () ->
  Rule.make ~id:rule_id ~description:rule_description ~explain:rule_explain ~run:check_tree ()
