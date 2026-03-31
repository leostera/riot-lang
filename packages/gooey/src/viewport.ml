open Std

type t = {
  width : float;
  height : float;
}

let make = fun ~width ~height -> {width; height}
