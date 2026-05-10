open Std

type t =
  | T of string

type error =
  | Empty
  | Too_long of int
  | Invalid_character of char

let max_length = 200

let valid_char = fun char ->
  match char with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '_'
  | '-'
  | '.'
  | ':'
  | '/' -> true
  | _ -> false

let validate value =
  let value = String.trim value in
  if String.equal value "" then
    Error Empty
  else if String.length value > max_length then
    Error (Too_long (String.length value))
  else
    let invalid = ref None in
    String.iter
      (fun char ->
        if not (valid_char char) then
          invalid := Some char)
      value;
    match !invalid with
    | Some char -> Error (Invalid_character char)
    | None -> Ok value

let from_string value =
  match validate value with
  | Ok value -> Ok (T value)
  | Error _ as error -> error

let create () = T (UUID.to_string (UUID.v7_monotonic ()))

let from_string_unchecked value = T value

let to_string (T value) = value

let equal (T left) (T right) = String.equal left right

let error_to_string = fun error ->
  match error with
  | Empty -> "id is required"
  | Too_long length -> "id is too long: " ^ Int.to_string length
  | Invalid_character _ -> "id contains invalid character"
