open Global

(** A comparison benchmark that runs multiple implementations and compares them. *)
type t = {
  (** Human-readable comparison description. *)
  description: string;
  (** Benchmark cases included in the comparison. *)
  cases: Bench_case.t list;
  (** Shared configuration used for all cases in the comparison. *)
  config: Bench_case.bench_config;
}

(**
   [compare description cases] creates a comparison benchmark with the default
   configuration.

   ## Example

   ```ocaml
   Bench_comparison.compare
     "insert 10k items"
     [
       Bench_case.case "HashMap" (fun () -> ());
       Bench_case.case "Swisstable" (fun () -> ());
     ]
   ```
*)
val compare: string -> Bench_case.t list -> t

(**
   [compare_with_config ~config description cases] creates a comparison benchmark
   with a custom configuration.

   ## Example

   ```ocaml
   Bench_comparison.compare_with_config
     ~config:{ iterations = 500; warmup = 20 }
     "sort small vectors"
     [
       Bench_case.case "quicksort" (fun () -> ());
       Bench_case.case "mergesort" (fun () -> ());
     ]
   ```
*)
val compare_with_config: config:Bench_case.bench_config -> string -> Bench_case.t list -> t
