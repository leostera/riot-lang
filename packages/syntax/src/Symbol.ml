open Sexplib.Std

type t = string [@@deriving sexp]

let intern s = s
