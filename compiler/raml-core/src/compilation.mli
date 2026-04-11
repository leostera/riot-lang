open Std.Data

type t
val create:
  targeting:Json.t ->
  source:Json.t ->
  typing:Json.t ->
  core_ir:Json.t ->
  lowering_fields:(string * Json.t) list ->
  codegen_fields:(string * Json.t) list ->
  t

val of_pipeline_json: Json.t -> t

val to_json: t -> Json.t

val lowering_to_json: t -> Json.t

val codegen_to_json: t -> Json.t

val emitted_output: t -> (string, string) Std.Result.t
