let f x =
  match x with
  | Option.Some y -> y
  | Option.None -> 0
