open Std
open Std.Collections

module Ast = Syn.Ast

let to_list = fun vector ->
  Vector.to_array vector
  |> Array.to_list

let structure_items = fun (ctx: Rule.context) ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count ctx.source_file) in
  Ast.SourceFile.for_each_structure_item
    ctx.source_file
    ~fn:(fun item -> Vector.push items ~value:item);
  to_list items

let signature_items = fun (ctx: Rule.context) ->
  let items = Vector.with_capacity ~size:(Ast.Node.child_count ctx.source_file) in
  Ast.SourceFile.for_each_signature_item
    ctx.source_file
    ~fn:(fun item -> Vector.push items ~value:item);
  to_list items

let expressions = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.Expr.cast node))
    (ctx.source_file: Ast.Node.t)
  |> List.filter_map ~fn:Ast.Expr.cast

let let_bindings = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.LetBinding.cast node))
    (ctx.source_file: Ast.Node.t)
  |> List.filter_map ~fn:Ast.LetBinding.cast

let type_declarations = fun (ctx: Rule.context) ->
  Traversal.find_nodes
    (fun node -> Option.is_some (Ast.TypeDeclaration.cast node))
    (ctx.source_file: Ast.Node.t)
  |> List.filter_map ~fn:Ast.TypeDeclaration.cast
