open Global

type bench_config = {
  iterations : int;
  warmup : int;
}

let default_config = {iterations = 100; warmup = 10}

type t = {
  name : string;
  fn : unit -> unit;
  config : bench_config;
  skip : bool;
}

let case = fun name fn -> {name; fn; config = default_config; skip = false}

let skip = fun name fn -> {name; fn; config = default_config; skip = true}

let with_config = fun ~config name fn -> {name; fn; config; skip = false}
