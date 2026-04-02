type 'a response =
  Success of 'a
  | Failure of string
  | Retry of int
