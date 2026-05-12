let f () =
  match get_value () with
  | exception (Failure _ as e) -> Error e
  | v -> Ok v
