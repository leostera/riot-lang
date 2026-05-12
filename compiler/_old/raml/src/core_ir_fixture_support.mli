open Std
open Std.Data

module Core_ir = Raml_core.Core_ir

val parse_compilation_unit: Json.t -> (Core_ir.Compilation_unit.t, string) result
