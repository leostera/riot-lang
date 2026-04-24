open Std
open Std.Collections
module Slice = IO.IoVec.IoSlice

type slice = {
  value: Slice.t;
  has_newline: bool;
}

type t =
  | Empty
  | Text of string
  | Slice of slice
  | Space
  | Spaces of int
  | Line
  | Break of string
  | Group of t
  | Concat of t list
  | Indent of int * t

let empty = Empty

let text = fun value ->
  if value = "" then
    Empty
  else
    Text value

let slice = fun ~has_newline value ->
  if Slice.length value = 0 then
    Empty
  else
    Slice { value; has_newline }

let space = Space

let spaces = fun count ->
  if count <= 0 then
    Empty
  else if count = 1 then
    Space
  else
    Spaces count

let line = Line

let break = fun ?(flat = " ") () -> Break flat

let softline = Break ""

let indent = fun spaces doc ->
  if spaces <= 0 then
    doc
  else
    Indent (spaces, doc)

let group = fun doc -> Group doc

let equal = text "="

let arrow = text "->"

let bar = text "|"

let colon = text ":"

let semi = text ";"

let comma = text ","

let lparen = text "("

let rparen = text ")"

let lbrace = text "{"

let rbrace = text "}"

let lbracket = text "["

let rbracket = text "]"

let fast_concat = function
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> Concat docs

let fast_join = fun separator docs ->
  let rec interleave acc = function
    | [] -> List.reverse acc
    | [ doc ] -> List.reverse (doc :: acc)
    | doc :: rest -> interleave (separator :: doc :: acc) rest
  in
  match docs with
  | [] -> Empty
  | docs -> fast_concat (interleave [] docs)

let concat = fun docs ->
  let add_spaces count acc =
    if count <= 0 then
      acc
    else
      match acc with
      | Spaces current :: rest -> Spaces (current + count) :: rest
      | Space :: rest -> Spaces (count + 1) :: rest
      | _ when count = 1 -> Space :: acc
      | _ -> Spaces count :: acc
  in
  let rec flatten acc stack =
    match stack with
    | [] ->
        List.reverse acc
    | [] :: rest ->
        flatten acc rest
    | (Empty :: rest_docs) :: rest ->
        flatten acc (rest_docs :: rest)
    | (Space :: rest_docs) :: rest ->
        flatten (add_spaces 1 acc) (rest_docs :: rest)
    | (Spaces count :: rest_docs) :: rest ->
        flatten (add_spaces count acc) (rest_docs :: rest)
    | (Break flat :: rest_docs) :: rest -> (
        match acc with
        | Break current :: _ when current = flat -> flatten acc (rest_docs :: rest)
        | _ -> flatten (Break flat :: acc) (rest_docs :: rest)
      )
    | (Group doc :: rest_docs) :: rest ->
        flatten (Group doc :: acc) (rest_docs :: rest)
    | (Concat nested :: rest_docs) :: rest ->
        flatten acc (nested :: rest_docs :: rest)
    | (doc :: rest_docs) :: rest ->
        flatten (doc :: acc) (rest_docs :: rest)
  in
  match flatten [] [ docs ] with
  | [] -> Empty
  | [ doc ] -> doc
  | docs -> Concat docs

let join = fun separator docs ->
  let rec interleave acc = function
    | [] -> List.reverse acc
    | [ doc ] -> List.reverse (doc :: acc)
    | doc :: rest -> interleave (separator :: doc :: acc) rest
  in
  match docs with
  | [] -> Empty
  | docs -> concat (interleave [] docs)

let words = fun docs -> join space docs

let lines = fun docs -> join line docs

let padded = fun doc -> concat [ space; doc; space ]

let prefixed = fun prefix doc -> concat [ prefix; doc ]

let suffixed = fun doc suffix -> concat [ doc; suffix ]

let wrapped = fun left doc right -> concat [ left; doc; right ]

let rec is_multiline = function
  | Empty -> false
  | Text value -> String.contains value "\n"
  | Slice value -> value.has_newline
  | Space -> false
  | Spaces _ -> false
  | Line -> true
  | Break _ -> false
  | Group doc -> is_multiline doc
  | Concat docs -> List.any docs ~fn:is_multiline
  | Indent (_, doc) -> is_multiline doc
