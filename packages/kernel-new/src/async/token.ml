type t

external make: 'value -> t = "kernel_new_async_token_make"

external unsafe_value: t -> 'value = "kernel_new_async_token_value"

external id: t -> int = "kernel_new_async_token_id"

let hash = fun token -> Int.hash (id token)

let equal = fun left right ->
  Int.equal (id left) (id right)
