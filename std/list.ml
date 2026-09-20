// Pure list operations.
use Prelude.{List, Nil, Cons}

let map = fn f -> fn values -> match values {
  | Nil -> Nil
  | Cons(item, rest) -> Cons(f item, map f rest)
}

let length = fn values -> match values {
  | Nil -> 0
  | Cons(_, rest) -> 1 + length rest
}

let fold = fn f -> fn initial -> fn values -> match values {
  | Nil -> initial
  | Cons(item, rest) -> fold f (f initial item) rest
}
