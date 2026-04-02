(* Private type declarations *)

type t = private {
  x: int;
  y: int;
}

(* Private abstract type *)

type state

(* Private variant *)

type color =
  private Red
  | Green
  | Blue

(* Private type alias *)

type id = private int
