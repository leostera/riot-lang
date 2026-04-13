open Std
open Std.Collections

(** Module namespace handling with double-underscore convention *)
type t = string list

let empty = []

let of_string = fun s ->
  if s = "" then
    []
  else
    String.split ~by:"_" s |> List.filter ~fn:(fun part -> not (String.is_empty part)) |> function
    | [] ->
        []
    | parts when List.length parts mod 2 = 0 ->
        (* Try to reconstruct from __ separated *)
        let rec pair = function
          | [] -> []
          | [ x ] -> [ x ]
          | "" :: "" :: rest -> pair rest
          | x :: "" :: rest -> x :: pair rest
          | x :: y :: rest -> (x ^ "_" ^ y) :: pair rest
        in
        pair parts
    | parts ->
        parts

let of_list = fun l -> l

let append = fun ns component -> ns @ [ component ]

let to_string = function
  | [] -> ""
  | ns -> String.concat "__" ns

let to_list = fun ns -> ns

let is_empty = fun ns -> ns = []
