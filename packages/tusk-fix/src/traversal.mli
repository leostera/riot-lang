include module type of Fixme.Traversal

type binding_site = {
  syntax_node : Syn.Cst.syntax_node;
  name_token : Syn.Cst.Token.t;
  is_function : bool;
}
val binding_sites_of_structure_item : Syn.Cst.StructureItem.t -> binding_site list
