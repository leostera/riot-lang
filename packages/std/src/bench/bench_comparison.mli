open Global

(** A comparison benchmark that runs multiple implementations and compares them. *)
(** [compare description cases] creates a comparison benchmark with default config.
    
    Example:
    {[
      Bench.(compare "insert 10k items" [
        case "HashMap" (fun () -> ...);
        case "Swisstable" (fun () -> ...);
      ])
    ]}
*)
type t = {
  description : string;
  cases : Bench_case.t list;
  config : Bench_case.bench_config;
}
val compare : string -> Bench_case.t list -> t

(** [compare_with_config ~config description cases] creates a comparison benchmark
    with custom configuration. *)
val compare_with_config : config:Bench_case.bench_config -> string -> Bench_case.t list -> t
