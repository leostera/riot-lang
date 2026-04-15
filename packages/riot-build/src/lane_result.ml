open Std

type t = {
  target: Riot_model.Target.t;
  results: Package_builder.build_result list;
  had_partial_failure: bool;
}

let target = fun t -> t.target

let results = fun t -> t.results

let had_partial_failure = fun t -> t.had_partial_failure
