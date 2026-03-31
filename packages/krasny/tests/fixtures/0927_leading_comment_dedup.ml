(* First non-trivia token *)
let first_non_trivia_token node =
  match first_non_trivia_child node with
  | Some t -> Some t
  | _ -> None
