open Std
open Std.Data

type t
val compile_source:
  host:Target.t -> target:Target.t -> relpath:Path.t -> source:string -> (t, string) result

val to_json: t -> Json.t

val lowering_to_json: t -> Json.t

val codegen_to_json: t -> Json.t
