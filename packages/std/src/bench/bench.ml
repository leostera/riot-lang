module Runner = Bench_runner

module Reporter = struct
  module Intf = Reporter.Intf

  module Default = Reporter.Default
end

type bench_case = Bench_case.t

type bench_config = Bench_case.bench_config = { iterations: int; warmup: int }

type comparison = Bench_comparison.t

type bench_item = Bench_runner.bench_item =
  | Single of bench_case
  | Compare of comparison

(* Single benchmark constructors *)
let case = fun name fn -> Single (Bench_case.case name fn)

let skip = fun name fn -> Single (Bench_case.skip name fn)

let with_config = fun ~config name fn -> Single (Bench_case.with_config ~config name fn)

(* Comparison constructors *)
let compare = fun description cases -> Compare (Bench_comparison.compare description cases)

let compare_with_config = fun ~config description cases -> Compare (Bench_comparison.compare_with_config ~config description cases)

(* Helper to make bench_case directly *)
let make_case = Bench_case.case

let make_case_with_config = Bench_case.with_config

module Cli = Cli
