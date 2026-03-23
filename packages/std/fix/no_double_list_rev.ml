open Std
open Std.Collections

module Api = Tusk_fix_api

let package_name = "std"
let package_rule_id = package_name ^ ":no-double-list-rev"

let explanation =
  Api.Explanation.
    {
      rule_id = package_rule_id;
      message = "List.rev (List.rev xs) cancels itself out and should be simplified.";
      body =
        {|
Avoid calling List.rev twice in a row.

Examples:
  Instead of:
    List.rev (List.rev items)

  write either:
    items

  or, if the reversal is still semantically needed:
    List.rev items

`List.rev (List.rev xs)` returns the original list again. The extra reversal
adds work without changing the result, and in real code it usually means an
intermediate transformation was deleted while the cleanup never happened.
|};
    }

let explanations () = [ explanation ]

let text_of_syntax_node syntax_node =
  let parts = ref [] in
  Syn.Ceibo.Red.SyntaxNode.preorder syntax_node (function
    | Syn.Ceibo.Red.Token token ->
        parts := Syn.Ceibo.Red.SyntaxToken.text token :: !parts
    | Syn.Ceibo.Red.Node _ ->
        ());
  !parts |> List.rev |> String.concat ""

let rec unwrap_expression = function
  | Syn.Cst.Expression.Parenthesized expr ->
      unwrap_expression expr.inner
  | expr ->
      expr

let rec flatten_apply expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let head, arguments = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ ->
      (unwrap_expression expr, [])

let path_text path =
  Syn.Cst.Ident.segments path
  |> List.map Syn.Cst.Token.text
  |> String.concat "."

let rec expression_name expr =
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } ->
      Some (path_text path)
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match expression_name receiver with
      | Some receiver_name ->
          Some (receiver_name ^ "." ^ Syn.Cst.Token.text field_name)
      | None ->
          None)
  | _ ->
      None

let path_matches ~expected expr =
  match expression_name expr with
  | Some actual ->
      String.equal actual expected
  | None ->
      false

let expression_of_apply_argument = function
  | Syn.Cst.Positional expr -> Some expr
  | Syn.Cst.Labeled { value; _ } | Syn.Cst.Optional { value; _ } ->
      value

let positional_arguments arguments =
  arguments
  |> List.filter_map expression_of_apply_argument

let make_fix ~(outer : Syn.Cst.Expression.t) ~(replacement : Syn.Cst.Expression.t) =
  let replacement_text =
    replacement |> Syn.Cst.Expression.syntax_node |> text_of_syntax_node
  in
  Api.Fix.make
    ~title:"Replace List.rev (List.rev xs) with xs"
    ~edits:
      [
        Api.Fix.make_text_edit
          ~span:(outer |> Syn.Cst.Expression.syntax_node |> Syn.Ceibo.Red.SyntaxNode.span)
          ~new_text:replacement_text;
      ]

let diagnostic_for_expression expr =
  let outer_head, outer_arguments = flatten_apply expr in
  if not (path_matches ~expected:"List.rev" outer_head) then
    None
  else
    match positional_arguments outer_arguments with
    | [ inner_argument ] ->
        let inner_head, inner_arguments = flatten_apply inner_argument in
        if not (path_matches ~expected:"List.rev" inner_head) then
          None
        else
          (match positional_arguments inner_arguments with
          | [ replacement ] ->
              Some
                (Api.Diagnostic.make ~severity:Warning
                   ~kind:
                     (Api.Diagnostic.Known
                        {
                          rule_id = explanation.Api.Explanation.rule_id;
                          message = explanation.Api.Explanation.message;
                        })
                   ~span:
                     (expr |> Syn.Cst.Expression.syntax_node
                    |> Syn.Ceibo.Red.SyntaxNode.span)
                   ~suggestion:
                     "Replace List.rev (List.rev xs) with xs or keep only one rev if order matters."
                   ~fix:(make_fix ~outer:expr ~replacement) ())
          | _ ->
              None)
    | _ ->
        None

let check_tree (ctx : Api.Rule.context) _red_root =
  match ctx.cst with
  | None -> []
  | Some cst_file ->
      Syn.Visit.(source_file
        {
          default with
          visit_expression =
            (fun diagnostics walk expression ->
              let diagnostics =
                match diagnostic_for_expression expression with
                | Some diagnostic -> diagnostic :: diagnostics
                | None -> diagnostics
              in
              walk.descend_expression diagnostics expression);
        }
        [] cst_file
    )
      |> List.rev

let rule () =
  Api.Rule.make ~id:package_rule_id
    ~description:"Double List.rev calls should be simplified"
    ~explain:explanation.body ~run:check_tree ()
