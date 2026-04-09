open Prelude

type t = string

let of_string = fun path -> path

let to_string = fun path -> path

let drop_leading_slash = fun path ->
  let len = String.length path in
  if len <= 1 then
    ""
  else
    String.init (len - 1)
      (fun index ->
        String.get path (index + 1))

let join = fun left right ->
  match (left, right) with
  | ("", path)
  | (path, "") -> path
  | left, right when String.get left (String.length left - 1) = '/' && String.get right 0 = '/' -> String.append
    left
    (drop_leading_slash right)
  | left, right when String.get left (String.length left - 1) = '/' -> String.append left right
  | left, right when String.get right 0 = '/' -> String.append left right
  | left, right -> String.append (String.append left "/") right

let ( / ) = join
