open Global

(** Configuration for a benchmark.
    
    - [iterations]: Number of times to run the benchmark for measurement
    - [warmup]: Number of warmup iterations before measurement
*)
type bench_config = {
  iterations: int;
  warmup: int;
}
val default_config: bench_config

(** A benchmark case. *)
(** [case name fn] creates a benchmark with default configuration.
    
    Default config: 100 iterations, 10 warmup iterations.
*)
type t = {
  name: string;
  fn: unit -> unit;
  config: bench_config;
  skip: bool;
}
val case: string -> (unit -> unit) -> t
(** [skip name fn] creates a skipped benchmark. *)
val skip: string -> (unit -> unit) -> t
(** [with_config ~config name fn] creates a benchmark with custom configuration. *)
val with_config: config:bench_config -> string -> (unit -> unit) -> t
