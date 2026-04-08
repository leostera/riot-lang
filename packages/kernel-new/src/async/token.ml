type t

external unsafe_cast : 'a -> 'b = "%identity"

let unsafe_to_value = fun (token: t) -> unsafe_cast token

let unsafe_to_int: t -> int = fun token -> unsafe_to_value token

let hash = fun token -> Int.hash (unsafe_to_int token)

let equal = fun ?eq left right ->
  match eq with
  | Some eq -> eq (unsafe_to_value left) (unsafe_to_value right)
  | None -> Int.equal (unsafe_to_int left) (unsafe_to_int right)

let make: 'value -> t = fun value -> unsafe_cast value
