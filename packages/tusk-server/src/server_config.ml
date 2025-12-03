open Std
(** Server configuration *)

type t = {
  enable_codedb : bool;
}

let default = { enable_codedb = true }

let equal a b = a.enable_codedb = b.enable_codedb

let no_codedb = { enable_codedb = false }
