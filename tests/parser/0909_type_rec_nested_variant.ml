type term =
  Var of string
  | Lambda of string * term
  | App of term * term
