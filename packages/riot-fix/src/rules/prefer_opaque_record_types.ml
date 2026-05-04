open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "prefer-opaque-record-types"

let rule_description = "Public records should be exposed as opaque types"

let rule_explain =
  {|
Public interfaces that expose both a record representation and field accessors
make the representation part of the module contract twice:

```ocaml
type t = { name : string }
val name : t -> string
```

Prefer `type t` in the interface and keep the record fields private to the
implementation when callers should use accessor functions.
|}

type public_record = {
  name: string;
  fields: string Vector.t;
  node: Ast.Node.t;
}

let is_interface_file = fun ctx ->
  match Path.extension (Path.v ctx.Rule.file_path) with
  | Some ".mli" -> true
  | _ -> false

let rec unwrap_type = fun type_expr -> H.unwrap_type_expr type_expr

let type_ident_last_segment_text = fun type_expr ->
  match Ast.TypeExpr.view (unwrap_type type_expr) with
  | Ast.TypeExpr.Ident { ident } ->
      Ast.Ident.last_segment ident
      |> Option.map ~fn:Ast.Token.text
  | _ -> None

let first_arrow_argument_type = fun type_expr ->
  match Ast.TypeExpr.view (unwrap_type type_expr) with
  | Ast.TypeExpr.Arrow { arg; _ } -> Some arg
  | _ -> None

let value_declaration_targets_record = fun value_declaration record ->
  let name_matches_field =
    match Ast.ValueDeclaration.name value_declaration with
    | Some name ->
        let value_name = Ast.Ident.text name in
        let found = ref false in
        Vector.for_each
          record.fields
          ~fn:(fun field_name ->
            if String.equal field_name value_name then
              found := true);
        !found
    | None -> false
  in
  if not name_matches_field then
    false
  else
    match Ast.ValueDeclaration.type_annotation value_declaration with
    | Some type_annotation -> (
        match first_arrow_argument_type type_annotation
        |> Option.and_then ~fn:type_ident_last_segment_text with
        | Some type_name -> String.equal type_name record.name
        | None -> false
      )
    | None -> false

let diagnostic_for_record = fun record ->
  H.diagnostic
    ~rule_id
    ~message:rule_description
    ~span:(H.span_of_node record.node)
    ~suggestion:"Hide the record fields behind an abstract `type t` in the interface."
    ()

let collect_record = fun records member record_type ->
  match Ast.TypeDeclaration.Member.name member with
  | Some name ->
      let fields = Vector.with_capacity ~size:(Ast.RecordType.field_count record_type) in
      H.iter_fold
        Ast.RecordType.fold_field
        record_type
        ~fn:(fun field ->
          match Ast.RecordField.view field with
          | Ast.RecordField.Field { name = field_name; _ } ->
              Vector.push fields ~value:(Ast.Ident.text field_name)
          | Ast.RecordField.Unknown _ -> ());
      if not (Vector.is_empty fields) then
        Vector.push
          records
          ~value:{
            name = Ast.Ident.text name;
            fields;
            node = Ast.TypeDeclaration.as_node (Ast.TypeDeclaration.Member.declaration member);
          }
  | None -> ()

let check_tree = fun ctx root ->
  let diagnostics = H.diagnostics_for_root root in
  if is_interface_file ctx then (
    let records = Vector.with_capacity ~size:(Ast.Node.child_count root) in
    let hooks = {
      Syn.Visitor.empty_hooks with
      enter_type_declaration =
        Some (fun visitor declaration ->
          H.iter_fold
            Ast.TypeDeclaration.fold_member
            declaration
            ~fn:(fun member ->
              match Ast.TypeDeclaration.Member.record_type member with
              | Some record_type -> collect_record records member record_type
              | None -> ());
          (visitor, Syn.Visitor.Continue));
      enter_value_declaration =
        Some (fun visitor value_declaration ->
          Vector.for_each
            records
            ~fn:(fun record ->
              if value_declaration_targets_record value_declaration record then
                H.push_diagnostic diagnostics (diagnostic_for_record record));
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
