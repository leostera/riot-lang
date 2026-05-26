let x =
  match y with
  | exception Not_found -> None
  | v -> Some v
