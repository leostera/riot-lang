use list.{map, fold}

type option<'a> =
  | None
  | Some('a)

let double = fn x -> x * 2

let main = fn x -> {
  let y = x + 1;
  double y
}
