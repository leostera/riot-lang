open Std

type t = int

let compare = Int.compare

let equal = Int.equal

let of_int = fun value -> value

let of_path = fun path ->
  let text = SurfacePath.to_string path in
  let length = String.length text in
  let rec loop index acc =
    if index >= length then
      Int.abs acc
    else
      let code =
        String.get text index
        |> Char.code
      in
      loop (index + 1) ((acc * 65_599) + code)
  in
  loop 0 17

let to_int = fun value -> value

let to_string = fun constructor_id -> format Format.[ str "constructor#"; int constructor_id ]
