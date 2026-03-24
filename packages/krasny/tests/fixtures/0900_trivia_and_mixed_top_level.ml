(* Leading comment for a formatted let binding. *)
let before = 1

(** Docstring for a formatted let binding. *)
let documented = 2

type preserved =
  | A
  | B

let after_type = 3

module Preserved = struct
  let value = 1
end

let after_module = 4
