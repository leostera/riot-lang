type stmt =
  Skip
  | Assign of string * int
  | Seq of stmt * stmt
