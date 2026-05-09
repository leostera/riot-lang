open Std

type key = {
  target: Riot_model.Target.t;
}
type t = key

val make: target:Riot_model.Target.t -> t
