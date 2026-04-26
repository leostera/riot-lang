open Std

type error =
  | Empty
  | Empty_part
  | Invalid_part of string

type t = {
  parts: string list;
}

let error_message = fun error ->
  match error with
  | Empty -> "namespace must not be empty"
  | Empty_part -> "namespace parts must not be empty"
  | Invalid_part part -> "invalid namespace part: " ^ String.escaped part

let is_invalid_part = fun part ->
  String.equal part "."
  || String.equal part ".."
  || String.contains part "/"
  || String.contains part "\\"

let from_parts = fun parts ->
  if List.is_empty parts then
    Error Empty
  else
    let rec loop acc parts =
      match parts with
      | [] -> Ok { parts = List.reverse acc }
      | part :: _ when String.is_empty part -> Error Empty_part
      | part :: _ when is_invalid_part part -> Error (Invalid_part part)
      | part :: rest -> loop (part :: acc) rest
    in
    loop [] parts

let to_string = fun value -> String.concat "/" value.parts

let parts = fun value -> value.parts
