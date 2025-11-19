open Std

(** Module namespace handling with double-underscore convention *)

type t = string list

let empty = []

let separator = "__"

let of_string s =
  if s = "" then []
  else
    String.split_on_char '_' s |> List.filter (fun x -> x != "") |> function
    | [] -> []
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
    | parts -> parts

let of_list l = l
let append ns component = ns @ [ component ]
let to_string = function [] -> "" | ns -> String.concat separator ns
let to_list ns = ns
let is_empty ns = ns = []

(** Quick hack: derive namespace from file path *)
let from_path (path : Path.t) : t =
  let path_str = Path.to_string path in
  let sanitize_name name = 
    String.map (fun c -> if c = '-' then '_' else c) name
  in
  match String.split_on_char '/' path_str with
  | "packages" :: pkg_name :: rest ->
      let pkg_ns = sanitize_name pkg_name |> String.capitalize_ascii in
      let after_src = match rest with
        | "src" :: tail -> tail
        | _ -> rest
      in
      let dir_segments = match List.rev after_src with
        | [] -> []
        | _filename :: rev_dirs -> List.rev rev_dirs
      in
      let capitalized_dirs = List.map String.capitalize_ascii dir_segments in
      pkg_ns :: capitalized_dirs
  | _ -> []
