let bind_option = fun f opt ->
  match opt with
  | Some x -> f x
  | None -> None
