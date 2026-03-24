open Std
open Std.Collections

type t =
  | Empty
  | Text of string
  | Line
  | Concat of t list
  | Indent of int * t

let empty = Empty
let text value = if value = "" then Empty else Text value
let line = Line
let indent spaces doc = if spaces <= 0 then doc else Indent (spaces, doc)

let concat docs =
  let rec flatten acc = function
    | [] ->
        List.rev acc
    | Empty :: rest ->
        flatten acc rest
    | Concat nested :: rest ->
        flatten acc (nested @ rest)
    | doc :: rest ->
        flatten (doc :: acc) rest
  in
  match flatten [] docs with
  | [] ->
      Empty
  | [ doc ] ->
      doc
  | docs ->
      Concat docs

let join separator docs =
  match docs with
  | [] ->
      Empty
  | first :: rest ->
      concat
        (first
        :: (rest
           |> List.map (fun doc -> [ separator; doc ])
           |> List.flatten))

let rec is_multiline = function
  | Empty ->
      false
  | Text value ->
      String.contains value "\n"
  | Line ->
      true
  | Concat docs ->
      List.exists is_multiline docs
  | Indent (_, doc) ->
      is_multiline doc
