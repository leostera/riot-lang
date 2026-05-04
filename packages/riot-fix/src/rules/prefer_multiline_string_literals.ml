open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-multiline-string-literals"

let rule_description =
  "String literal chains should use multiline string literals instead of repeated concatenation"

let rule_explain =
  {explain|
When a string literal starts spilling across several concatenated pieces, the reader
has to reconstruct the final text mentally from punctuation. That is tedious for short
examples and error-prone for longer payloads, especially when whitespace matters.

A multiline string literal keeps the content in one place and lets the eye inspect the
actual output instead of a chain of `^` operators. This is particularly helpful for
HTML fragments, SQL, JSON examples, and test fixtures where punctuation and spacing
carry meaning.

Use concatenation when the pieces are genuinely dynamic. Use a multiline literal when
the content is really one static block of text.
|explain}

let expr_is_string_literal = fun expr ->
  match Ast.Expr.view expr with
  | Ast.Expr.Literal { token } -> Syn.SyntaxKind.(Ast.Token.kind token = STRING)
  | _ -> false

let rec string_literal_chain_size = fun expr ->
  match Ast.Expr.view (H.unwrap_expr expr) with
  | Ast.Expr.Literal _ when expr_is_string_literal expr -> Some 1
  | Ast.Expr.Infix { left; operator; right } when String.equal (Ast.Token.text operator) "^" -> (
      match (string_literal_chain_size left, string_literal_chain_size right) with
      | (Some left_count, Some right_count) -> Some (left_count + right_count)
      | _ -> None
    )
  | _ -> None

let rec find_string_literal_chain = fun expr ->
  match string_literal_chain_size expr with
  | Some count when count >= 2 -> Some expr
  | _ ->
      let found = ref None in
      H.iter_fold
        Ast.Expr.fold_child_expr
        expr
        ~fn:(fun child ->
          match !found with
          | Some _ -> ()
          | None -> found := find_string_literal_chain child);
      !found

let make_diagnostic = fun expr ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node (Ast.Expr.as_node expr))
    ~suggestion:"Use a multiline string literal like {| ... |} instead of concatenating string literals"
    ()

let diagnostic_for_binding = fun binding ->
  Ast.LetBinding.body binding
  |> Option.and_then ~fn:find_string_literal_chain
  |> Option.map ~fn:make_diagnostic

let check_tree = fun _ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  H.for_each_let_binding
    root
    ~fn:(fun binding ->
      match diagnostic_for_binding binding with
      | Some diagnostic -> H.push_diagnostic diagnostics diagnostic
      | None -> ());
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
