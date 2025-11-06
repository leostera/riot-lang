open Std

type t = {
  width : float;
  height : float;
}

let make ~width ~height = { width; height }
