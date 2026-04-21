open Std

val duration_to_nanos_float: Time.Duration.t -> float

val duration_median: Time.Duration.t list -> Time.Duration.t option

val int_median: int list -> int option

val coefficient_of_variation: Model.bench_statistics -> float option

val stability_of_cv: current_cv:float option -> baseline_cv:float option -> Model.stability

val baseline_statistics: Model.history_sample list -> Model.bench_statistics
