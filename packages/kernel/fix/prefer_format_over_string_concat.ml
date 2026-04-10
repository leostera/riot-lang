open Std

let package_name = "kernel"

let rule_name = "prefer-format-over-string-concat"

let package_rule_id = Format.format Format.[ str package_name; char ':'; str rule_name ]

let rule_description = "Prefer Kernel.Format fragments over manual string concatenation"

let rule_explain = {|
Repeated `^` chains make otherwise mechanical message rendering harder to read and
harder to extend safely.

`Kernel.Format` gives Riot one primitive string assembly surface:

```ocaml
format Format.[ str "hello "; int count ]
```

This rule rewrites clear `^` chains into `Kernel.Format` fragments. It stays on
the safe side:

- it skips literal-only chains
- it skips chains that contain comments/docstrings
- it only specializes obvious conversion helpers like `string_of_int` and
  `Int.to_string`

When local `format` / `Format` are not obviously in scope, the fix uses an
explicit `Kernel.format Kernel.Format.[ ... ]` form instead of assuming imports.
|}

let explanation =
  Fixme.Explanation.{ rule_id = package_rule_id; message = rule_description; body = rule_explain }

let explanations = fun () -> [ explanation ]

let rec syntax_node_text = fun node ->
  Syn.Ceibo.Red.SyntaxNode.children node |> List.map
    (
      function
      | Syn.Ceibo.Red.Node child -> syntax_node_text child
      | Syn.Ceibo.Red.Token token -> Syn.Ceibo.Red.SyntaxToken.text token
    ) |> String.concat ""

let expression_text = fun expr -> syntax_node_text (Syn.Cst.Expression.syntax_node expr)

let parenthesized_expression_text = fun expr ->
  Format.format Format.[ char '('; str (expression_text expr); char ')' ]

let leading_layout_text = fun expr ->
  let text = expression_text expr in
  let len = String.length text in
  let rec find_non_layout idx =
    if idx >= len then
      idx
    else
      match text.[idx] with
      | ' '
      | '\t'
      | '\r'
      | '\n' -> find_non_layout (idx + 1)
      | _ -> idx
  in
  String.sub text 0 (find_non_layout 0)

let replacement_prefix = fun expr ->
  let prefix = leading_layout_text expr in
  if String.equal prefix "" then
    " "
  else
    prefix

let expression_span = fun expr -> Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.Expression.syntax_node expr)

let span_contains = fun (outer: Syn.Ceibo.Span.t) (inner: Syn.Ceibo.Span.t) ->
  outer.start <= inner.start && outer.end_ >= inner.end_

let rec unwrap_expression = fun expr ->
  match expr with
  | Syn.Cst.Expression.Parenthesized { inner; _ } -> unwrap_expression inner
  | Syn.Cst.Expression.TypeAscription { expression; _ }
  | Syn.Cst.Expression.Polymorphic { expression; _ } -> unwrap_expression expression
  | _ -> expr

let is_kernel_source_path = fun path ->
  String.starts_with ~prefix:"packages/kernel/" path || String.contains path "/packages/kernel/"

let rec expression_name = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Path { path; _ } ->
      Syn.Cst.Ident.segments path
      |> List.map Syn.Cst.Token.text
      |> String.concat "."
      |> fun name -> Some name
  | Syn.Cst.Expression.FieldAccess { receiver; field_name; _ } -> (
      match expression_name receiver with
      | Some receiver_name -> Some (receiver_name ^ "." ^ Syn.Cst.Token.text field_name)
      | None -> None
    )
  | _ ->
      None

let rec flatten_apply = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Apply { callee; argument; _ } ->
      let head, arguments = flatten_apply callee in
      (head, arguments @ [ argument ])
  | _ -> (unwrap_expression expr, [])

let positional_arguments = fun arguments ->
  arguments |> List.filter_map
    (
      function
      | Syn.Cst.Positional expr -> Some expr
      | Syn.Cst.Labeled _
      | Syn.Cst.Optional _ -> None
    )

let helper_fragment = fun helper expr ->
  Format.format Format.[ str helper; char ' '; str (parenthesized_expression_text expr) ]

let is_string_literal = function
  | Syn.Cst.Expression.Literal (Syn.Cst.Literal.String _) -> true
  | _ -> false

let conversion_fragment = fun expr ->
  let head, arguments = flatten_apply expr in
  match expression_name head, positional_arguments arguments with
  | Some ("string_of_int" | "Stdlib.string_of_int" | "Int.to_string" | "Stdlib.Int.to_string" | "Kernel.Int.to_string"), [
    arg
  ] -> Some (helper_fragment "int" arg)
  | Some ("string_of_float" | "Stdlib.string_of_float" | "Float.to_string" | "Stdlib.Float.to_string" | "Kernel.Float.to_string"), [
    arg
  ] -> Some (helper_fragment "float" arg)
  | Some ("string_of_bool" | "Stdlib.string_of_bool" | "Bool.to_string" | "Stdlib.Bool.to_string" | "Kernel.Bool.to_string"), [
    arg
  ] -> Some (helper_fragment "bool" arg)
  | Some ("Int32.to_string" | "Stdlib.Int32.to_string" | "Kernel.Int32.to_string"), [ arg ] -> Some (helper_fragment
    "int32"
    arg)
  | Some ("Int64.to_string" | "Stdlib.Int64.to_string" | "Kernel.Int64.to_string"), [ arg ] -> Some (helper_fragment
    "int64"
    arg)
  | Some ("Bytes.to_string" | "Stdlib.Bytes.to_string"), [ arg ] -> Some (helper_fragment "bytes" arg)
  | _ -> None

let fragment_for_segment = fun expr ->
  match unwrap_expression expr with
  | expr when is_string_literal expr -> Some (Format.format
    Format.[ str "str "; str (expression_text expr) ])
  | expr -> (
      match conversion_fragment expr with
      | Some fragment -> Some fragment
      | None -> Some (Format.format Format.[ str "str "; str (parenthesized_expression_text expr) ])
    )

let rec flatten_concat_chain = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Infix infix when String.equal (Syn.Cst.InfixExpression.operator infix) "^" -> flatten_concat_chain
    (Syn.Cst.InfixExpression.left infix)
  @ flatten_concat_chain (Syn.Cst.InfixExpression.right infix)
  | other -> [ other ]

let is_concat_expression = fun expr ->
  match unwrap_expression expr with
  | Syn.Cst.Expression.Infix infix -> String.equal (Syn.Cst.InfixExpression.operator infix) "^"
  | _ -> false

let contains_comments = fun expr ->
  Fixme.Traversal.find_tokens
    (fun token ->
      match Syn.Ceibo.Red.SyntaxToken.kind token with
      | Syn.SyntaxKind.COMMENT
      | Syn.SyntaxKind.DOCSTRING -> true
      | _ -> false)
    (Syn.Cst.Expression.syntax_node expr) |> function
  | [] -> false
  | _ -> true

let open_statement_brings_format = fun stmt ->
  match Syn.Cst.OpenStatement.module_path stmt with
  | Some path -> (
      match Syn.Cst.Ident.segments path |> List.map Syn.Cst.Token.text with
      | [ "Std" ]
      | [ "Kernel" ]
      | [ "Global" ]
      | ["Std";"Global"]
      | ["Kernel";"Global"] -> true
      | _ -> false
    )
  | None -> false

let structure_items = fun (ctx: Fixme.Rule.context) ->
  match ctx.cst with
  | Syn.Cst.Implementation implementation -> implementation.items
  | Syn.Cst.Interface _ -> []

let has_local_format_scope = fun (ctx: Fixme.Rule.context) expr ->
  let expr_start = (expression_span expr).start in
  structure_items ctx |> List.exists
    (fun item ->
      let item_span = Syn.Ceibo.Red.SyntaxNode.span (Syn.Cst.StructureItem.syntax_node item) in
      item_span.end_ <= expr_start && match item with
      | Syn.Cst.StructureItem.OpenStatement stmt -> open_statement_brings_format stmt
      | _ -> false)

let format_call_for_expression = fun (ctx: Fixme.Rule.context) expr ->
  if is_kernel_source_path ctx.file_path then
    ("Format.format", "Format")
  else if has_local_format_scope ctx expr then
    ("format", "Format")
  else
    ("Kernel.format", "Kernel.Format")

let replacement_text = fun ctx expr ->
  let call_prefix, namespace = format_call_for_expression ctx expr in
  let fragments = flatten_concat_chain expr |> List.filter_map fragment_for_segment in
  Format.format
    Format.[
      str (replacement_prefix expr);
      str call_prefix;
      char ' ';
      str namespace;
      str ".[ ";
      str (String.concat "; " fragments);
      str " ]";
    ]

let make_fix = fun ctx expr ->
  Fixme.Fix.make
    ~title:"Rewrite string concatenation as Kernel.Format fragments"
    ~operations:[
      Fixme.Fix.replace_node_with_text
        ~target:(Syn.Cst.Expression.syntax_node expr)
        ~text:(replacement_text ctx expr);
    ]

let make_diagnostic = fun ctx expr ->
  Fixme.Diagnostic.make ~severity:Warning ~kind:(Fixme.Diagnostic.Known {
    rule_id = package_rule_id;
    message = rule_description
  }) ~span:(expression_span expr) ~suggestion:"Replace this `^` chain with `Kernel.Format` fragments."
    (* ~fix:(make_fix ctx expr) *)
    ()

let should_rewrite_chain = fun expr ->
  let segments = flatten_concat_chain expr in
  List.length segments >= 2
  && List.exists (fun segment -> not (is_string_literal (unwrap_expression segment))) segments
  && not (contains_comments expr)

let concat_chain_roots = fun ctx ->
  let expressions = structure_items ctx |> List.concat_map Fixme.Traversal.expressions_of_structure_item in
  let rec loop covered acc remaining =
    match remaining with
    | [] -> List.rev acc
    | expr :: rest ->
        if not (is_concat_expression expr) then
          loop covered acc rest
        else
          let span = expression_span expr in
          if List.exists (fun outer -> span_contains outer span) covered then
            loop covered acc rest
          else
            loop (span :: covered) (expr :: acc) rest
  in
  loop [] [] expressions

let check_tree = fun (ctx: Fixme.Rule.context) _red_root ->
  concat_chain_roots ctx |> List.filter should_rewrite_chain |> List.map (make_diagnostic ctx)

let rule = fun () ->
  Fixme.Rule.make
    ~id:package_rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
