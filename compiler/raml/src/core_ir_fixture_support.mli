open Std
open Std.Data

val parse_compilation_unit: Json.t -> (Core_ir.Compilation_unit.t, string) result
