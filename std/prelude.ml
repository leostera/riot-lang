// Core algebraic data types and pure standard operations.

type Option<'value> =
  | None
  | Some('value)

type Result<'value, 'error> =
  | Ok('value)
  | Err('error)

type List<'value> =
  | Nil
  | Cons('value, List<'value>)

let option_map = fn f -> fn value -> match value {
  | None -> None
  | Some(item) -> Some(f item)
}

let option_unwrap_or = fn fallback -> fn value -> match value {
  | None -> fallback
  | Some(item) -> item
}

let result_map = fn f -> fn value -> match value {
  | Ok(item) -> Ok(f item)
  | Err(error) -> Err(error)
}

let result_unwrap_or = fn fallback -> fn value -> match value {
  | Ok(item) -> item
  | Err(_) -> fallback
}

let list_map = fn f -> fn values -> match values {
  | Nil -> Nil
  | Cons(item, rest) -> Cons(f item, list_map f rest)
}

let list_length = fn values -> match values {
  | Nil -> 0
  | Cons(_, rest) -> 1 + list_length rest
}

let list_fold = fn f -> fn initial -> fn values -> match values {
  | Nil -> initial
  | Cons(item, rest) -> list_fold f (f initial item) rest
}
