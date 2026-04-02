let x =
  match a with
  | Some x -> (
      match x with
      | 1 -> true
      | _ -> false
    )
  | None -> false
