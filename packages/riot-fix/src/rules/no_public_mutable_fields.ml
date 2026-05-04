open Std

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "no-public-mutable-fields"

let rule_description = "Public record fields should not be mutable"

let rule_explain =
  {|
A mutable field in an interface exposes mutation as part of the public contract.
That makes later refactors harder because callers can observe and change the field
directly.

Prefer keeping the record representation private and exposing named update helpers
when mutation is required.
|}

let is_interface_file = fun ctx ->
  match Path.extension (Path.v ctx.Rule.file_path) with
  | Some ".mli" -> true
  | _ -> false

let diagnostic_for_mutable_token = fun token ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_token token)
    ~suggestion:"Hide mutable fields behind an abstract type or explicit functions."
    ()

let record_type_diagnostics = fun diagnostics record_type ->
  H.iter_fold
    Ast.RecordType.fold_field
    record_type
    ~fn:(fun field ->
      match Ast.RecordField.view field with
      | Ast.RecordField.Field { mutable_token = Some token; _ } ->
          H.push_diagnostic diagnostics (diagnostic_for_mutable_token token)
      | Ast.RecordField.Field { mutable_token = None; _ }
      | Ast.RecordField.Unknown _ -> ())

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  if is_interface_file ctx then (
    let hooks = {
      Syn.Visitor.empty_hooks with
      enter_type_declaration =
        Some (fun visitor declaration ->
          H.iter_fold
            Ast.TypeDeclaration.fold_member
            declaration
            ~fn:(fun member ->
              match Ast.TypeDeclaration.Member.record_type member with
              | Some record_type -> record_type_diagnostics diagnostics record_type
              | None -> ());
          (visitor, Syn.Visitor.Continue));
    }
    in
    Syn.Visitor.make ~ctx:() ~hooks
    |> fun visitor -> ignore (Syn.Visitor.visit_node visitor root)
  );
  H.vector_to_list diagnostics

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
