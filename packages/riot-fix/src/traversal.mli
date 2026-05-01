include module type of Fixme.Traversal

type binding_site = {
  syntax_node: Syn.Ast.Node.t;
  name_token: Syn.Ast.Token.t;
  is_function: bool;
}

val binding_sites_of_structure_item: Syn.Ast.StructureItem.t -> binding_site list
