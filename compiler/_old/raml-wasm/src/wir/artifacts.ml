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
    function_table_element_count: int;
    has_indirect_calls: bool;
    needs_closure_runtime: bool;
  }

  let from_compilation_unit = fun (compilation_unit: Wasm_types.Compilation_unit.t) ->
    {
      unit_name = compilation_unit.unit_id.unit_name;
      imports = List.map compilation_unit.imports ~fn:Wasm_types.Import.key;
      exports = List.map compilation_unit.exports ~fn:(fun (export: Core.Export.t) -> export.name);
      global_count = List.length compilation_unit.globals;
      function_count = List.length compilation_unit.functions;
      init_item_count = List.length compilation_unit.init;
      function_table_element_count = List.length compilation_unit.runtime_plan.function_table_elements;
      has_indirect_calls = compilation_unit.runtime_plan.has_indirect_calls;
      needs_closure_runtime = compilation_unit.runtime_plan.needs_closure_runtime;
    }

  let to_json = fun summary ->
    Json.obj
      [
        ("unit_name", Json.string summary.unit_name);
        ("imports", Json.array (List.map summary.imports ~fn:Json.string));
        ("exports", Json.array (List.map summary.exports ~fn:Json.string));
        ("global_count", Json.int summary.global_count);
        ("function_count", Json.int summary.function_count);
        ("init_item_count", Json.int summary.init_item_count);
        ("function_table_element_count", Json.int summary.function_table_element_count);
        ("has_indirect_calls", Json.bool summary.has_indirect_calls);
        ("needs_closure_runtime", Json.bool summary.needs_closure_runtime);
      ]
end

module Object = struct
  type t = {
    unit_name: string;
    summary: Module_summary.t;
    program: Wasm_types.Compilation_unit.t;
  }

  let from_compilation_unit = fun (program: Wasm_types.Compilation_unit.t) ->
    {
      unit_name = program.unit_id.unit_name;
      summary = Module_summary.from_compilation_unit program;
      program
    }

  let to_json = fun object_ ->
    Json.obj
      [
        ("unit_name", Json.string object_.unit_name);
        ("summary", Module_summary.to_json object_.summary);
        ("program", Wasm_types.Compilation_unit.to_json object_.program);
      ]
end

module Linked_program = struct
  type t = {
    objects: Object.t list;
    imports: Wasm_types.Import.t list;
    exports: Core.Export.t list;
    function_table_elements: Core.Entity_id.t list;
    needs_closure_runtime: bool;
  }

  let link = fun objects ->
    let seen_imports = Collections.HashSet.create () in
    let seen_table_elements = Collections.HashSet.create () in
    let imports_rev = ref [] in
    let function_table_elements_rev = ref [] in
    let exports_rev = ref [] in
    let needs_closure_runtime = ref false in
    let add_import import =
      let key = Wasm_types.Import.key import in
      if Collections.HashSet.contains seen_imports ~value:key then
        ()
      else
        let _ = Collections.HashSet.insert seen_imports ~value:key in
        imports_rev := import :: !imports_rev
    in
    let add_table_element entity_id =
      let key =
        match Core.Entity_id.binding_id entity_id with
        | Some binding_id -> (
            match Core.Binding_id.stamp binding_id with
            | Some stamp -> Core.Binding_id.name binding_id ^ "#" ^ Int.to_string stamp
            | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)
          )
        | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)
      in
      if Collections.HashSet.contains seen_table_elements ~value:key then
        ()
      else
        let _ = Collections.HashSet.insert seen_table_elements ~value:key in
        function_table_elements_rev := entity_id :: !function_table_elements_rev
    in
    List.for_each
      objects
      ~fn:(fun (object_: Object.t) ->
        List.for_each object_.program.imports ~fn:add_import;
        List.for_each object_.program.runtime_plan.function_table_elements ~fn:add_table_element;
        exports_rev := !exports_rev @ object_.program.exports;
        needs_closure_runtime := !needs_closure_runtime || object_.program.runtime_plan.needs_closure_runtime);
    {
      objects;
      imports = List.rev !imports_rev;
      exports = !exports_rev;
      function_table_elements = List.rev !function_table_elements_rev;
      needs_closure_runtime = !needs_closure_runtime;
    }

  let to_json = fun linked_program ->
    Json.obj
      [
        ("objects", Json.array (List.map linked_program.objects ~fn:Object.to_json));
        ("imports", Json.array (List.map linked_program.imports ~fn:Wasm_types.Import.to_json));
        ("exports", Json.array (List.map linked_program.exports ~fn:Core.Export.to_json));
        (
          "function_table_elements",
          Json.array (List.map linked_program.function_table_elements ~fn:Core.Entity_id.to_json)
        );
        ("needs_closure_runtime", Json.bool linked_program.needs_closure_runtime);
      ]
end
