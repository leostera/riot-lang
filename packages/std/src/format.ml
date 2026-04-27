type t =
  | String of string
  | Char of char
  | Bool of bool
  | Int of int
  | Bytes of bytes

let str = fun value -> String value

let char = fun value -> Char value

let bool = fun value -> Bool value

let int = fun value -> Int value

let bytes = fun value -> Bytes value

let to_string = fun value ->
  match value with
  | String value -> value
  | Char value -> Kernel.String.make ~len:1 ~char:value
  | Bool value -> Kernel.Bool.to_string value
  | Int value -> Kernel.Int.to_string value
  | Bytes value -> Kernel.Bytes.to_string value

let format = fun values -> Kernel.String.concat "" (Kernel.List.map values ~fn:to_string)
