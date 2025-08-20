(** Extended list utilities *)

include Stdlib.List

let rec find_map f = function
  | [] -> None
  | x :: xs -> (
      match f x with Some _ as result -> result | None -> find_map f xs)

let filter_map f lst =
  let rec go acc = function
    | [] -> rev acc
    | x :: xs -> (
        match f x with Some y -> go (y :: acc) xs | None -> go acc xs)
  in
  go [] lst

let split_at n lst =
  let rec go n acc = function
    | lst when n <= 0 -> (rev acc, lst)
    | [] -> (rev acc, [])
    | x :: xs -> go (n - 1) (x :: acc) xs
  in
  go n [] lst

let take n lst = fst (split_at n lst)
let drop n lst = snd (split_at n lst)

let rec take_while pred = function
  | x :: xs when pred x -> x :: take_while pred xs
  | _ -> []

let rec drop_while pred = function
  | x :: xs when pred x -> drop_while pred xs
  | lst -> lst

let group eq lst =
  let rec go acc current = function
    | [] -> (
        match current with [] -> rev acc | _ -> rev (rev current :: acc))
    | x :: xs -> (
        match current with
        | [] -> go acc [ x ] xs
        | y :: _ when eq x y -> go acc (x :: current) xs
        | _ -> go (rev current :: acc) [ x ] xs)
  in
  go [] [] lst

let uniq eq lst =
  let rec go acc = function
    | [] -> rev acc
    | x :: xs -> if exists (eq x) acc then go acc xs else go (x :: acc) xs
  in
  go [] lst

let intersperse sep = function
  | [] -> []
  | [ x ] -> [ x ]
  | x :: xs ->
      let rec go acc = function
        | [] -> rev acc
        | y :: ys -> go (y :: sep :: acc) ys
      in
      x :: go [] xs

let is_empty = function [] -> true | _ -> false
let rec last = function [] -> None | [ x ] -> Some x | _ :: xs -> last xs

let rec init = function
  | [] -> None
  | [ _ ] -> Some []
  | x :: xs -> (
      match init xs with None -> None | Some rest -> Some (x :: rest))
