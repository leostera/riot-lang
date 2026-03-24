type id = int

type point = {
  x : int;
  y : int;
}

type color =
  | Red
  | Green
  | Blue

type 'a tree =
  | Leaf of 'a
  | Node of 'a tree * 'a tree

type ('ok, 'err) result_like =
  | Ok of 'ok
  | Error of 'err

type 'a decoder = string -> ('a, string) result

type response = {
  status : (int, string) result;
  body : string option;
}
