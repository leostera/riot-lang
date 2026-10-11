// Core algebraic data types shared by the standard library.

type Option<'value> =
  | None
  | Some('value)

type Result<'value, 'error> =
  | Ok('value)
  | Err('error)

type List<'value> =
  | Nil
  | Cons('value, List<'value>)
