type 'a tree =
  Node of 'a node

and 'a node = {
  value: 'a;
  children: 'a tree list;
}
