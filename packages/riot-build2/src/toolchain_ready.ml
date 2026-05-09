open Std

type key = {
  target: Riot_model.Target.t;
}

type t = key

let make = fun ~target -> { target }
