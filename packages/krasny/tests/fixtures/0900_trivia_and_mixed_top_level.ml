(* TODO(@leostera): we need to add more examples here for:
   - [ ] inline comments within expressions
   - [ ] doc comments over type definitoins
   - [ ] normal comments over type definitoins
   - [ ] doc comments over type constructors
   - [ ] commnets on records and inside records
   - [ ] commnets between function arguments
   - [ ] module-level doc comments
   - [ ] comments within inline signatures
   - [ ] anywhere else that we can fit a comment we should have at least 1 example of it
   *)

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
