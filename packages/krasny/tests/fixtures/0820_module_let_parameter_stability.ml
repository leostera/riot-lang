open Std

(* Internal state for iterator *)
module Iterator = struct
  (* Split string on first occurrence of pattern *)
  let split_on_pattern pattern str =
    pattern ^ str

  let size _state = 0

  (* Unknown size for streaming *)
  let clone state =
    state
end
