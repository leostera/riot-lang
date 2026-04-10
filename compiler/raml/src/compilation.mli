open Std.Data

type t
val of_pipeline_json: Json.t -> t

val to_json: t -> Json.t

val lowering_to_json: t -> Json.t

val codegen_to_json: t -> Json.t
