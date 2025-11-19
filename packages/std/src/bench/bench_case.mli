open Global

type bench_config = { iterations : int; warmup : int }
(** Configuration for a benchmark.
    
    - [iterations]: Number of times to run the benchmark for measurement
    - [warmup]: Number of warmup iterations before measurement
*)

val default_config : bench_config

type t = {
  name : string;
  fn : unit -> unit;
  config : bench_config;
  skip : bool;
}
(** A benchmark case. *)

val case : string -> (unit -> unit) -> t
(** [case name fn] creates a benchmark with default configuration.
    
    Default config: 100 iterations, 10 warmup iterations.
*)

val skip : string -> (unit -> unit) -> t
(** [skip name fn] creates a skipped benchmark. *)

val with_config : config:bench_config -> string -> (unit -> unit) -> t
(** [with_config ~config name fn] creates a benchmark with custom configuration. *)
