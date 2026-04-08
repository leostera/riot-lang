type t

external make: 'value -> t = "kernel_new_async_token_make"

external unsafe_to_value: t -> 'value = "kernel_new_async_token_value"

external id: t -> int = "kernel_new_async_token_id"

let hash = fun token -> Int.hash (id token)

let equal = fun ?eq left right ->
  if Int.equal (id left) (id right) then
    true
  else
    match eq with
    | Some eq -> eq (unsafe_to_value left) (unsafe_to_value right)
    | None -> false
