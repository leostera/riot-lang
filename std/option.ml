// Pure option operations.
use Prelude.{Option, None, Some}

let map = fn f -> fn value -> match value {
  | None -> None
  | Some(item) -> Some(f item)
}

let unwrap_or = fn fallback -> fn value -> match value {
  | None -> fallback
  | Some(item) -> item
}
