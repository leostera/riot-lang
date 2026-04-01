open Global

(** # Std.Bench - Simple Benchmarking Framework

    A low-level benchmarking framework that mirrors {!Test.case} in design,
    allowing benchmarks to be written as simple test-like cases.

    ## Quick Start

    {[
      open Std

      let bench_vector_push () =
        let v = Vector.create () in
        Vector.push v 42

      let benchmarks = Bench.[
        case "vector push" bench_vector_push;
      ]

      let () =
        Miniriot.run
          ~main:(fun ~args ->
            let config = Bench.Runner.{
              reporter = (module Bench.Reporter.Default);
              suite_info = { name = "My Benchmarks" };
            } in
            let _summary = Bench.Runner.run_benchmarks ~config benchmarks in
            Ok ()
          )
          ~args:Env.args ()
    ]}
*)

(** A benchmark case. *)
(** Benchmark configuration.
    
    - [iterations]: Number of times to run the benchmark for measurement (default: 100)
    - [warmup]: Number of warmup iterations before measurement (default: 10)
*)
type bench_case = Bench_case.t
(** A comparison benchmark. *)
type bench_config = Bench_case.bench_config = {
  iterations: int;
  warmup: int;
}
(** A benchmark item - either a single benchmark or a comparison. *)
type comparison = Bench_comparison.t
type bench_item =
  | Single of bench_case
  | Compare of comparison
module Runner: sig
  type config = {
    reporter: (module Reporter.Intf.Intf);
    suite_info: Reporter.Intf.suite_info;
  }
  type run_summary = Bench_result.summary
  val run_benchmarks: config:config -> bench_item list -> run_summary
end

(** [case name fn] creates a benchmark with default configuration.
    
    Example:
    {[
      Bench.case "hashmap insert" (fun () ->
        let map = HashMap.create () in
        HashMap.insert map "key" "value"
      )
    ]}
*)
module Reporter: sig
  module Intf = Reporter.Intf

  module Default = Reporter.Default
end

val case: string -> (unit -> unit) -> bench_item

(** [skip name fn] creates a skipped benchmark. *)
val skip: string -> (unit -> unit) -> bench_item

(** [with_config ~config name fn] creates a benchmark with custom configuration.
    
    Example:
    {[
      Bench.with_config
        ~config:{ iterations = 1000; warmup = 50 }
        "fast operation"
        (fun () -> let x = 1 + 1 in ignore x)
    ]}
*)
val with_config: config:bench_config -> string -> (unit -> unit) -> bench_item

(** [compare description cases] creates a comparison benchmark.
    
    Example:
    {[
      Bench.compare "insert 10k items" [
        Bench.make_case "HashMap" (fun () -> ...);
        Bench.make_case "Swisstable" (fun () -> ...);
      ]
    ]}
*)
val compare: string -> bench_case list -> bench_item

(** [compare_with_config ~config description cases] creates a comparison benchmark
    with custom configuration. *)
val compare_with_config: config:bench_config -> string -> bench_case list -> bench_item

(** [make_case name fn] creates a benchmark case without wrapping in Single.
    Useful for building comparison benchmarks. *)
val make_case: string -> (unit -> unit) -> bench_case

(** [make_case_with_config ~config name fn] creates a benchmark case with custom config. *)
val make_case_with_config: config:bench_config -> string -> (unit -> unit) -> bench_case

module Cli: sig
  val main:
    name:string ->
    benchmarks:bench_item list ->
    args:string list ->
    (unit, Miniriot.Process.exit_reason) result

  (** Main entry point for benchmark binaries with CLI support.
      
      Supports subcommands:
      - [run-benchmarks]: Execute benchmarks
      - [list-benchmarks]: List all benchmarks
      
      Flags:
      - [--format <fmt>]: Output format
      - [--iterations <n>]: Override iterations
      - [--warmup <n>]: Override warmup count
  *)
end
