type f =
  int * string -> [
    `Ok of 'a
    | `Error of string
  ]
