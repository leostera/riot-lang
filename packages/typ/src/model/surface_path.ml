open Std
open Std.Collections

type t =
  | Bare of string
  | Qualified of string * t

type error =
  | EmptyParts

let rec from_syn_ident = fun ident ->
  match ident with
  | Syn.Ast.Ident.Bare token -> Bare (Syn.Ast.Token.text token)
  | Syn.Ast.Ident.Qualified (token, rest) ->
      Qualified (Syn.Ast.Token.text token, from_syn_ident rest)

let from_parts parts =
  let rec loop part rest =
    match rest with
    | [] -> Bare part
    | next :: rest -> Qualified (part, loop next rest)
  in
  match parts with
  | [] -> Error EmptyParts
  | part :: rest -> Ok (loop part rest)

let to_segments =
  let rec loop acc value =
    match value with
    | Bare name -> List.reverse (name :: acc)
    | Qualified (name, tail) -> loop (name :: acc) tail
  in
  loop []

let to_string = fun value ->
  value
  |> to_segments
  |> String.concat "."

let rec equal = fun left right ->
  match (left, right) with
  | (Bare left_name, Bare right_name) -> String.equal left_name right_name
  | (Qualified (left_name, left_tail), Qualified (right_name, right_tail)) ->
      String.equal left_name right_name && equal left_tail right_tail
  | _ -> false

let rec compare = fun left right ->
  match (left, right) with
  | (Bare left_name, Bare right_name) -> String.compare left_name right_name
  | (Bare _, Qualified _) -> Order.LT
  | (Qualified _, Bare _) -> Order.GT
  | (Qualified (left_name, left_tail), Qualified (right_name, right_tail)) -> (
      match String.compare left_name right_name with
      | Order.EQ -> compare left_tail right_tail
      | order -> order
    )

let serializer =
  Serde.Ser.contramap
    to_segments
    (Serde.Ser.contramap Array.from_list (Serde.Ser.array Serde.Ser.string))
