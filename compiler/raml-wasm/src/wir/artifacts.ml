open Std
open Std.Data
module Core = Raml_core.Core_ir
module Wasm_types = Types

module Module_summary = struct
  type t = {
    unit_name: string;
    imports: string list;
    exports: string list;
    global_count: int;
    function_count: int;
    init_item_count: int;
  }

  let of_compilation_unit = fun (compilation_unit: Wasm_types.Compilation_unit.t) ->
    {
      unit_name = compilation_unit.unit_id.unit_name;
      imports = List.map Wasm_types.Import.key compilation_unit.imports;
      exports = List.map (fun (export: Core.Export.t) -> export.name) compilation_unit.exports;
      global_count = List.length compilation_unit.globals;
      function_count = List.length compilation_unit.functions;
      init_item_count = List.length compilation_unit.init;
    }

  let to_json = fun summary ->
    Json.obj
      [
        ("unit_name", Json.string summary.unit_name);
        ("imports", Json.array (List.map Json.string summary.imports));
        ("exports", Json.array (List.map Json.string summary.exports));
        ("global_count", Json.int summary.global_count);
        ("function_count", Json.int summary.function_count);
        ("init_item_count", Json.int summary.init_item_count);
      ]
end
