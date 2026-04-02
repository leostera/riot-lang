(** CLI interface for benchmark runners *)
open Global

(** Main entry point for benchmark binaries with CLI support.
    
    Accepts subcommands:
    - [run-benchmarks [query]]: Execute all benchmarks, optionally filtering
      benchmark names by substring
    - [list-benchmarks]: List all benchmark names
    
    Flags for [run-benchmarks]:
    - [--format <fmt>]: Output format (default: "default")
    - [--iterations <n>]: Override iteration count for all benchmarks
    - [--warmup <n>]: Override warmup count for all benchmarks
    
    Example:
    {[
      let () =
        Actors.run
          ~main:(fun ~args ->
            Bench.Cli.main
              ~name:"My Benchmarks"
              ~benchmarks:[...]
              ~args
          )
          ~args:Env.args ()
    ]}
*)
val main:
  name:string ->
  benchmarks:Bench_runner.bench_item list ->
  args:string list ->
  (unit, Actors.Process.exit_reason) result
