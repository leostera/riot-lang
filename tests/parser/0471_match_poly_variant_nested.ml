let x =
  match `A (`B (`C 1)) with
  | `A (`B (`C y)) -> y
