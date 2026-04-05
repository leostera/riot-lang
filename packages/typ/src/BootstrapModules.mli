open Std

(** Seeded module summaries used by the prototype host configuration.

    These summaries are a temporary bridge while the build and LSP hosts learn
    to load real type exports from cached package outputs. They intentionally
    use the same [ModuleSummary] shape that those hosts will later persist in
    and restore from [riot_store]. *)
val summaries: ModuleSummary.t list
