// Pure result operations.
use Prelude.{Result, Ok, Err}

let map = fn f -> fn value -> match value {
  | Ok(item) -> Ok(f item)
  | Err(error) -> Err(error)
}

let unwrap_or = fn fallback -> fn value -> match value {
  | Ok(item) -> item
  | Err(_) -> fallback
}
