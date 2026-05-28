type option<'a> =
  | None
  | Some('a)

let get_or_zero = fn x -> match x {
  | None -> 0
  | Some(value) -> value
}
