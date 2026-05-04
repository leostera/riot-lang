open Std
open Std.Collections

module H = Rule_helpers
module Ast = Syn.Ast

let rule_id = Rule_id.from_string "limit-open-statements"

let rule_description = "Prefer no more than two open statements per file"

let rule_explain =
  {|
Each file-wide `open` saves a little typing, but it also hides where names come from.
Once a file stacks several of them, readers have to keep a mental import table in
their head before they can tell whether `Response`, `parse`, or `empty` is local or
imported.

Past a small number, the convenience stops paying for the ambiguity. The remaining
modules are usually better referenced explicitly as `Http.Response`, `Json.decode`,
or `Uri.from_string`. If the scope is genuinely tiny, a local open is easier to audit
than another file-wide one.

Two well-chosen opens usually remain readable. A pile of them usually means the file
has become too implicit.
|}

let top_level_open_declarations = fun source_file ->
  let opens = Vector.with_capacity ~size:(Ast.SourceFile.item_count source_file) in
  (
    match Ast.SourceFile.view source_file with
    | Ast.SourceFile.Implementation implementation ->
        H.iter_fold
          Ast.Implementation.fold_item
          implementation
          ~fn:(fun item ->
            match Ast.StructureItem.view item with
            | Ast.StructureItem.Open open_declaration -> Vector.push opens ~value:open_declaration
            | _ -> ())
    | Ast.SourceFile.Interface interface ->
        H.iter_fold
          Ast.Interface.fold_item
          interface
          ~fn:(fun item ->
            match Ast.SignatureItem.view item with
            | Ast.SignatureItem.Open open_declaration -> Vector.push opens ~value:open_declaration
            | _ -> ())
  );
  opens

let diagnostic_for_open_count = fun opens ->
  if Vector.length opens <= 2 then
    None
  else
    Vector.get opens ~at:2
    |> Option.map
      ~fn:(fun third_open ->
        H.diagnostic
          ~rule_id
          ~message:rule_description
          ~span:(H.span_of_node (Ast.OpenDeclaration.as_node third_open))
          ~suggestion:"Keep only the most useful opens and qualify the remaining names."
          ())

let check_tree = fun (ctx: Rule.context) _root ->
  top_level_open_declarations ctx.source_file
  |> diagnostic_for_open_count
  |> Option.to_list

let make = fun () ->
  Rule.make
    ~id:rule_id
    ~description:rule_description
    ~explain:rule_explain
    ~run:check_tree
    ()
