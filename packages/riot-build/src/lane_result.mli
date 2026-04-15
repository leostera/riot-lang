open Std

type t = {
  target: Riot_model.Target.t;
  results: Package_builder.build_result list;
  had_partial_failure: bool;
}

val target: t -> Riot_model.Target.t

val results: t -> Package_builder.build_result list

val had_partial_failure: t -> bool
