type 'a btree =
  | Empty
  | Node of { value: 'a; left: 'a btree; right: 'a btree }
