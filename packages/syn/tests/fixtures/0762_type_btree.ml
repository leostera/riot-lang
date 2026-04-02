type 'a btree =
  Empty
  | Branch of 'a btree * 'a * 'a btree
