type 'a tree =
  Leaf of 'a
  | Node of 'a forest

and 'a forest =
  Empty
  | Trees of 'a tree list
