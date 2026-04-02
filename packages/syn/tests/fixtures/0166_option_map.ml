let map_option = fun f opt ->
  match opt with
  | Some x -> Some (f x)
  | None -> None
