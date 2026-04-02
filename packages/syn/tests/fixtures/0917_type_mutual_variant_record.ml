type node =
  Leaf of int
  | Branch of tree_info

and tree_info = {
  left: node;
  right: node;
  height: int;
}
