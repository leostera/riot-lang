open Global

(** Configuration for a benchmark.

    - [iterations]: Number of times to run the benchmark for measurement.
    - [warmup]: Number of warmup iterations before measurement.
*)
type bench_config = {
  (** Number of measured benchmark iterations. *)
  iterations: int;
  (** Number of warmup iterations before measurements start. *)
  warmup: int;
}

(** Default benchmark configuration. *)
val default_config: bench_config

(** A benchmark case. *)
type t = {
  (** Human-readable benchmark name. *)
  name: string;
  (** Function executed for each benchmark iteration. *)
  fn: unit -> unit;
  (** Per-benchmark execution configuration. *)
  config: bench_config;
  (** Whether this benchmark should be skipped. *)
  skip: bool;
}

(** [case name fn] creates a benchmark with the default configuration.

    The default config is 100 measured iterations and 10 warmup iterations.

    ## Example

    ```ocaml
    let benchmark =
      Bench_case.case "vector push" (fun () ->
        let v = Vector.create () in
        Vector.push v 42)
    ```
*)
val case: string -> (unit -> unit) -> t

(** [skip name fn] creates a skipped benchmark.

    ## Example

    ```ocaml
    let benchmark = Bench_case.skip "disabled benchmark" (fun () -> ())
    ```
*)
val skip: string -> (unit -> unit) -> t

(** [with_config ~config name fn] creates a benchmark with a custom
    configuration.

    ## Example

    ```ocaml
    let benchmark =
      Bench_case.with_config
        ~config:{ iterations = 1_000; warmup = 50 }
        "hash lookup"
        (fun () ->
          ignore (HashMap.find map "key"))
    ```
*)
val with_config: config:bench_config -> string -> (unit -> unit) -> t
