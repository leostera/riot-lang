open Global

type t = {
  description : string;
  cases : Bench_case.t list;
  config : Bench_case.bench_config;
}
(** A comparison benchmark that runs multiple implementations and compares them. *)

val compare : string -> Bench_case.t list -> t
(** [compare description cases] creates a comparison benchmark with default config.
    
    Example:
    {[
      Bench.(compare "insert 10k items" [
        case "HashMap" (fun () -> ...);
        case "Swisstable" (fun () -> ...);
      ])
    ]}
*)

val compare_with_config :
  config:Bench_case.bench_config -> string -> Bench_case.t list -> t
(** [compare_with_config ~config description cases] creates a comparison benchmark
    with custom configuration. *)
