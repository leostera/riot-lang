open Std
open Std.Data

module Core_ir = RamlCore.CoreIR

val parse_compilation_unit: Json.t -> (Core_ir.Compilation_unit.t, string) result
