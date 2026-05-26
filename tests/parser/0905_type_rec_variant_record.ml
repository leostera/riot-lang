type tree =
  Leaf
  | Node of { value: int; left: tree; right: tree }
