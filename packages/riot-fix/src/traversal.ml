open Std
open Std.Collections

include Fixme.Traversal

module Ast = Syn.Ast

type binding_site = {
  syntax_node: Ast.Node.t;
  name_token: Ast.Token.t;
  is_function: bool;
}

let binding_name_token = fun binding ->
  match Ast.LetBinding.pattern binding with
  | Some pattern -> Ast.Node.first_descendant_token pattern
  | None -> None

let binding_site_of_let_binding = fun binding ->
  binding_name_token binding
  |> Option.map
    ~fn:(fun name_token -> { syntax_node = (binding: Ast.Node.t); name_token; is_function = false })

let binding_sites_of_structure_item = fun item ->
  let sites = Vector.with_capacity ~size:(Ast.Node.child_count item) in
  let rec visit node =
    match Ast.LetBinding.cast node with
    | Some binding -> (
        match binding_site_of_let_binding binding with
        | Some site -> Vector.push sites ~value:site
        | None -> ()
      )
    | None -> Ast.Node.for_each_child_node node ~fn:visit
  in
  Ast.Node.for_each_child_node item ~fn:visit;
  Vector.to_array sites |> Array.to_list
