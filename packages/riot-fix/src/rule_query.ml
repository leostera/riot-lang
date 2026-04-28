open Std
open Std.Collections

let iter_fold = fun fold value ~fn ->
  fold
    value
    ~init:()
    ~fn:(fun item () ->
      fn item;
      Syn.Ast.Continue ())


module Ast = Syn.Ast

let to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let structure_items = fun (ctx: Rule.context) ->
  let items = Vector.with_capacity ~size:(Ast.SourceFile.structure_item_count ctx.source_file) in
  iter_fold Ast.SourceFile.fold_structure_item
    ctx.source_file
    ~fn:(fun item -> Vector.push items ~value:item);
  to_list items

let signature_items = fun (ctx: Rule.context) ->
  let items = Vector.with_capacity ~size:(Ast.SourceFile.signature_item_count ctx.source_file) in
  iter_fold Ast.SourceFile.fold_signature_item
    ctx.source_file
    ~fn:(fun item -> Vector.push items ~value:item);
  to_list items

let expressions = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.cast_result_to_option (Ast.Expr.cast node)))
    (Ast.SourceFile.as_node ctx.source_file)
  |> List.filter_map ~fn:(fun node -> Ast.cast_result_to_option (Ast.Expr.cast node))

let let_bindings = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.cast_result_to_option (Ast.LetBinding.cast node)))
    (Ast.SourceFile.as_node ctx.source_file)
  |> List.filter_map ~fn:(fun node -> Ast.cast_result_to_option (Ast.LetBinding.cast node))

let type_declarations = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.cast_result_to_option (Ast.TypeDeclaration.cast node)))
    (Ast.SourceFile.as_node ctx.source_file)
  |> List.filter_map ~fn:(fun node -> Ast.cast_result_to_option (Ast.TypeDeclaration.cast node))
