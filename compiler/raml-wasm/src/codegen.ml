open Std
open Std.Data
module Vector = Collections.Vector
module Artifacts = Wir.Artifacts
module Types = Wir.Types
module Core = Raml_core.Core_ir

let ( let* ) = fun result next ->
  match result with
  | Ok value -> next value
  | Error error -> Error error

type artifact = {
  wasm_base64: string;
  size_bytes: int;
  memory_pages: int;
  node_runner: string;
}

type error =
  | Unsupported_import of Types.Import.t
  | Unsupported_function of Types.Function.t
  | Unsupported_global of Types.Global.t
  | Unsupported_expr of { context: string; expr: Types.Expr.t }
  | Unsupported_indirect_calls
  | Unsupported_closure_runtime
  | Unsupported_integer of { context: string; value: int }
  | Unsupported_char of { value: string }

type string_data = {
  offset: int;
  length: int;
}

type value =
  | Unit
  | I32 of int
  | String_data of string_data

type compiled_expr = {
  instructions: string;
  value: value;
}

type runtime_signature =
  | String_to_unit
  | I32_to_unit
  | Unit_to_unit

type import_binding = {
  import_: Types.Import.t;
  function_index: int;
  signature: runtime_signature;
}

module Binary = struct
  type section = {
    id: int;
    payload: string;
  }

  let byte = fun value -> String.make ~len:1 ~char:(Char.from_int_unchecked value)

  let bytes = fun values ->
    String.concat "" (List.map values ~fn:byte)

  let concat = fun parts ->
    String.concat "" parts

  let encode_u32 = fun value ->
    let rec loop value acc =
      let byte_value = value land 0x7f in
      let remaining = value lsr 7 in
      if remaining = 0 then
        List.rev (byte_value :: acc)
      else
        loop remaining ((byte_value lor 0x80) :: acc)
    in
    bytes (loop value [])

  let encode_s32 = fun value ->
    let rec loop value acc =
      let byte_value = value land 0x7f in
      let next = value asr 7 in
      let sign_bit_set = byte_value land 0x40 = 0x40 in
      let done_ = (next = 0 && not sign_bit_set) || (next = (-1) && sign_bit_set) in
      if done_ then
        List.rev (byte_value :: acc)
      else
        loop next ((byte_value lor 0x80) :: acc)
    in
    bytes (loop value [])

  let encode_name = fun value -> concat [ encode_u32 (String.length value); value ]

  let vec = fun items -> concat [ encode_u32 (List.length items); concat items ]

  let section = fun id payload -> concat [ byte id; encode_u32 (String.length payload); payload ]

  let function_type = fun ~params ~results -> concat [ byte 0x60; vec params; vec results ]
end

let import_kind_to_string = fun kind ->
  match kind with
  | Types.Import.Runtime -> "runtime"
  | Types.Import.Host -> "host"

let error_to_json = fun error ->
  match error with
  | Unsupported_import import -> Json.obj
    [ ("kind", Json.string "unsupported_import"); ("import", Types.Import.to_json import); ]
  | Unsupported_function function_ -> Json.obj
    [ ("kind", Json.string "unsupported_function"); ("function", Types.Function.to_json function_); ]
  | Unsupported_global global -> Json.obj
    [ ("kind", Json.string "unsupported_global"); ("global", Types.Global.to_json global); ]
  | Unsupported_expr { context; expr } -> Json.obj
    [
      ("kind", Json.string "unsupported_expr");
      ("context", Json.string context);
      ("expr", Types.Expr.to_json expr);
    ]
  | Unsupported_indirect_calls -> Json.obj [ ("kind", Json.string "unsupported_indirect_calls") ]
  | Unsupported_closure_runtime -> Json.obj [ ("kind", Json.string "unsupported_closure_runtime") ]
  | Unsupported_integer { context; value } -> Json.obj
    [
      ("kind", Json.string "unsupported_integer");
      ("context", Json.string context);
      ("value", Json.int value);
    ]
  | Unsupported_char { value } -> Json.obj
    [ ("kind", Json.string "unsupported_char"); ("value", Json.string value); ]

let artifact_to_json = fun artifact ->
  Json.obj
    [
      ("status", Json.string "ok");
      ("format", Json.string "wasm");
      ("wasm_base64", Json.string artifact.wasm_base64);
      ("size_bytes", Json.int artifact.size_bytes);
      ("memory_pages", Json.int artifact.memory_pages);
      ("node_runner", Json.string artifact.node_runner);
    ]

let runtime_signature_of_import = fun (import_: Types.Import.t) ->
  match import_.module_name, import_.name with
  | "riot:runtime", "print_endline" -> Some String_to_unit
  | "riot:runtime", "print_string" -> Some String_to_unit
  | "riot:runtime", "print_int" -> Some I32_to_unit
  | "riot:runtime", "print_char" -> Some I32_to_unit
  | "riot:runtime", "print_newline" -> Some Unit_to_unit
  | _ -> None

let type_index_of_signature = fun signature ->
  match signature with
  | String_to_unit -> 0
  | I32_to_unit -> 1
  | Unit_to_unit -> 2

let signature_param_count = fun signature ->
  match signature with
  | String_to_unit -> 2
  | I32_to_unit -> 1
  | Unit_to_unit -> 0

let entity_key = fun entity_id ->
  match Core.Entity_id.binding_id entity_id with
  | Some binding_id -> (
      match Core.Binding_id.stamp binding_id with
      | Some stamp -> Core.Binding_id.name binding_id ^ "#" ^ Int.to_string stamp
      | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)
    )
  | None -> Core.Surface_path.to_string (Core.Entity_id.surface_path entity_id)

let add_string_literal = fun layout value ->
  match Collections.HashMap.get layout ~key:value with
  | Some info -> (layout, info)
  | None ->
      let offset =
        Collections.HashMap.fold_left
          layout
          ~init:0
          ~fn:(fun current _ (info: string_data) -> max current (info.offset + info.length))
      in
      let info = { offset; length = String.length value } in
      let _ = Collections.HashMap.insert layout ~key:value ~value:info in
      (layout, info)

let collect_char_error = fun value errors ->
  if String.length value = 1 then
    errors
  else
    Unsupported_char { value } :: errors

let rec collect_strings_from_expr = fun layout expr errors ->
  match expr with
  | Types.Expr.Constant (Core.Constant.String value) ->
      let layout, _ = add_string_literal layout value in
      (layout, errors)
  | Types.Expr.Constant (Core.Constant.Char value) ->
      let errors = collect_char_error value errors in
      (layout, errors)
  | Types.Expr.Constant _ ->
      (layout, errors)
  | Types.Expr.Var _ ->
      (layout, errors)
  | Types.Expr.Direct_call call ->
      List.fold_left
        call.arguments
        ~init:(layout, errors)
        ~fn:(fun (layout, errors) argument -> collect_strings_from_expr layout argument errors)
  | Types.Expr.Indirect_call call ->
      let layout, errors = collect_strings_from_expr layout call.callee errors in
      List.fold_left
        call.arguments
        ~init:(layout, errors)
        ~fn:(fun (layout, errors) argument -> collect_strings_from_expr layout argument errors)
  | Types.Expr.Lambda lambda ->
      collect_strings_from_expr layout lambda.body errors
  | Types.Expr.Let let_ ->
      let layout, errors =
        List.fold_left
          let_.bindings
          ~init:(layout, errors)
          ~fn:(fun (layout, errors) (binding: Types.Expr.binding) ->
            collect_strings_from_expr layout binding.expr errors)
      in
      collect_strings_from_expr layout let_.body errors
  | Types.Expr.Sequence sequence ->
      let layout, errors = collect_strings_from_expr layout sequence.first errors in
      collect_strings_from_expr layout sequence.second errors
  | Types.Expr.Tuple items ->
      List.fold_left
        items
        ~init:(layout, errors)
        ~fn:(fun (layout, errors) item -> collect_strings_from_expr layout item errors)
  | Types.Expr.Tuple_get tuple_get ->
      collect_strings_from_expr layout tuple_get.tuple errors
  | Types.Expr.If_then_else if_then_else ->
      let layout, errors = collect_strings_from_expr layout if_then_else.condition errors in
      let layout, errors = collect_strings_from_expr layout if_then_else.then_ errors in
      collect_strings_from_expr layout if_then_else.else_ errors
  | Types.Expr.Primitive primitive ->
      List.fold_left
        primitive.arguments
        ~init:(layout, errors)
        ~fn:(fun (layout, errors) argument -> collect_strings_from_expr layout argument errors)

let collect_strings = fun (linked_program: Artifacts.Linked_program.t) ->
  List.fold_left linked_program.objects ~init:(Collections.HashMap.create (), [])
    ~fn:(fun (layout, errors) (object_: Artifacts.Object.t) ->
      List.fold_left object_.program.init ~init:(layout, errors)
        ~fn:(fun (layout, errors) item ->
          match item with
          | Types.Init_item.Global global -> collect_strings_from_expr layout global.expr errors
          | Types.Init_item.Eval expr -> collect_strings_from_expr layout expr errors))

let imported_function_bindings = fun imports ->
  let bindings_rev = ref [] in
  let errors_rev = ref [] in
  List.iteri
    (fun function_index (import_: Types.Import.t) ->
      match runtime_signature_of_import import_ with
      | None -> errors_rev := Unsupported_import import_ :: !errors_rev
      | Some signature -> bindings_rev := { import_; function_index; signature } :: !bindings_rev)
    imports;
  (List.rev !bindings_rev, List.rev !errors_rev)

let i32_value = fun ~context value ->
  let min_i32 = (-2_147_483_648) in
  let max_i32 = 2_147_483_647 in
  if value < min_i32 || value > max_i32 then
    Error [ Unsupported_integer { context; value } ]
  else
    Ok value

let char_code = fun value ->
  if String.length value = 1 then
    Ok (Char.code (String.get_unchecked value ~at:0))
  else
    Error [ Unsupported_char { value } ]

let value_instructions = fun value ->
  match value with
  | Unit -> Ok ""
  | I32 value -> Ok (Binary.concat [ Binary.byte 0x41; Binary.encode_s32 value ])
  | String_data string_data -> Ok (Binary.concat
    [
      Binary.byte 0x41;
      Binary.encode_s32 string_data.offset;
      Binary.byte 0x41;
      Binary.encode_s32 string_data.length;
    ])

let find_string_data = fun layout value ->
  match Collections.HashMap.get layout ~key:value with
  | Some info -> Ok info
  | None -> Error [
    Unsupported_expr {
      context = "missing_string_layout";
      expr = Types.Expr.Constant (Core.Constant.String value)
    };
  ]

let find_binding = fun env entity_id context expr ->
  match Collections.HashMap.get env ~key:(entity_key entity_id) with
  | Some value -> Ok value
  | None -> Error [ Unsupported_expr { context; expr } ]

let find_import_binding = fun imports callee expr ->
  let found =
    List.find_opt
      (fun (binding: import_binding) ->
        match Core.Entity_id.binding_id callee with
        | Some binding_id -> (
            match Core.Binding_id.stamp binding_id with
            | Some _ -> false
            | None ->
                let import_name = Core.Surface_path.last_name (Core.Entity_id.surface_path callee) in
                import_name = binding.import_.name
          )
        | None ->
            let import_name = Core.Surface_path.last_name (Core.Entity_id.surface_path callee) in
            import_name = binding.import_.name)
      imports
  in
  match found with
  | Some binding -> Ok binding
  | None -> Error [ Unsupported_expr { context = "unsupported_direct_callee"; expr } ]

let rec compile_expr = fun ~layout ~imports ~env expr ->
  match expr with
  | Types.Expr.Constant Core.Constant.Unit ->
      Ok { instructions = ""; value = Unit }
  | Types.Expr.Constant (Core.Constant.Bool value) ->
      Ok {
        instructions = "";
        value =
          I32 (
            if value then
              1
            else
              0
          );
      }
  | Types.Expr.Constant (Core.Constant.Int value) ->
      let* value = i32_value ~context:"int_constant" value in
      Ok { instructions = ""; value = I32 value }
  | Types.Expr.Constant (Core.Constant.Char value) ->
      let* value = char_code value in
      Ok { instructions = ""; value = I32 value }
  | Types.Expr.Constant (Core.Constant.String value) ->
      let* data = find_string_data layout value in
      Ok { instructions = ""; value = String_data data }
  | Types.Expr.Constant Core.Constant.Float _ ->
      Error [ Unsupported_expr { context = "float_constant"; expr } ]
  | Types.Expr.Var entity_id ->
      let* value = find_binding env entity_id "unbound_wasm_codegen_var" expr in
      Ok { instructions = ""; value }
  | Types.Expr.Sequence sequence ->
      let* first = compile_expr ~layout ~imports ~env sequence.first in
      let* second = compile_expr ~layout ~imports ~env sequence.second in
      Ok {
        instructions = Binary.concat [ first.instructions; second.instructions ];
        value = second.value
      }
  | Types.Expr.Let let_ -> begin
      match let_.rec_flag with
      | Core.Rec_flag.Recursive -> Error [ Unsupported_expr { context = "recursive_let"; expr } ]
      | Core.Rec_flag.Nonrecursive ->
          let env' = Collections.HashMap.create () in
          List.for_each (Collections.HashMap.to_list env)
            ~fn:(fun (key, value) ->
              let _ = Collections.HashMap.insert env' ~key ~value in
              ());
          let instructions = Vector.with_capacity ~size:(List.length let_.bindings + 1) in
          let errors = Vector.with_capacity ~size:(List.length let_.bindings) in
          List.iter
            (fun (binding: Types.Expr.binding) ->
              match compile_expr ~layout ~imports ~env:env' binding.expr with
              | Ok compiled ->
                  Vector.push instructions ~value:compiled.instructions;
                  let _ = Collections.HashMap.insert
                    env'
                    ~key:(entity_key binding.entity_id)
                    ~value:compiled.value in
                  ()
              | Error compile_errors -> compile_errors
              |> List.for_each ~fn:(fun error -> Vector.push errors ~value:error))
            let_.bindings;
          if Vector.is_empty errors then
            (
              let* body = compile_expr ~layout ~imports ~env:env' let_.body in
              Vector.push instructions ~value:body.instructions;
              Ok {
                instructions =
                  Binary.concat
                    (Vector.to_array instructions |> Array.to_list);
                value = body.value;
              }
            )
          else
            Error (Vector.to_array errors |> Array.to_list)
    end
  | Types.Expr.If_then_else if_then_else ->
      let* condition = compile_expr ~layout ~imports ~env if_then_else.condition in
      begin
        match condition.value with
        | I32 0 -> compile_expr ~layout ~imports ~env if_then_else.else_
        | I32 _ -> compile_expr ~layout ~imports ~env if_then_else.then_
        | _ -> Error [ Unsupported_expr { context = "dynamic_if_condition"; expr } ]
      end
  | Types.Expr.Direct_call call ->
      let* import_binding = find_import_binding imports call.callee expr in
      let* argument_instructions = compile_call_arguments
        ~layout
        ~imports
        ~env
        ~signature:import_binding.signature
        call.arguments in
      Ok {
        instructions = Binary.concat
          [
            argument_instructions;
            Binary.byte 0x10;
            Binary.encode_u32 import_binding.function_index;
          ];
        value = Unit
      }
  | Types.Expr.Indirect_call _ ->
      Error [ Unsupported_indirect_calls ]
  | Types.Expr.Lambda _ ->
      Error [ Unsupported_expr { context = "lambda_value"; expr } ]
  | Types.Expr.Tuple _ ->
      Error [ Unsupported_expr { context = "tuple_value"; expr } ]
  | Types.Expr.Tuple_get _ ->
      Error [ Unsupported_expr { context = "tuple_get"; expr } ]
  | Types.Expr.Primitive _ ->
      Error [ Unsupported_expr { context = "primitive"; expr } ]

and compile_call_arguments = fun ~layout ~imports ~env ~signature arguments ->
  match signature, arguments with
  | Unit_to_unit, [] ->
      Ok ""
  | I32_to_unit, [ argument ] ->
      let* compiled = compile_expr ~layout ~imports ~env argument in
      begin
        match compiled.value with
        | I32 value ->
            let* pushed = value_instructions (I32 value) in
            Ok (Binary.concat [ compiled.instructions; pushed ])
        | _ -> Error [ Unsupported_expr { context = "expected_i32_argument"; expr = argument } ]
      end
  | String_to_unit, [ argument ] ->
      let* compiled = compile_expr ~layout ~imports ~env argument in
      begin
        match compiled.value with
        | String_data data ->
            let* pushed = value_instructions (String_data data) in
            Ok (Binary.concat [ compiled.instructions; pushed ])
        | _ -> Error [ Unsupported_expr { context = "expected_string_argument"; expr = argument } ]
      end
  | _ ->
      let expr =
        match arguments with
        | [] -> Types.Expr.Constant Core.Constant.Unit
        | [ argument ] -> argument
        | arguments -> Types.Expr.Tuple arguments
      in
      Error [ Unsupported_expr { context = "wrong_runtime_arity"; expr } ]

let compile_init_item = fun ~layout ~imports ~env item ->
  match item with
  | Types.Init_item.Eval expr ->
      let* compiled = compile_expr ~layout ~imports ~env expr in
      Ok compiled.instructions
  | Types.Init_item.Global global ->
      let* compiled = compile_expr ~layout ~imports ~env global.expr in
      let _ = Collections.HashMap.insert env ~key:(entity_key global.entity_id) ~value:compiled.value in
      Ok compiled.instructions

let compile_start = fun ~layout ~imports (linked_program: Artifacts.Linked_program.t) ->
  let env = Collections.HashMap.create () in
  let init_count = linked_program.objects
  |> List.fold_left ~init:0 ~fn:(fun count object_ -> count + List.length object_.program.init) in
  let instructions = Vector.with_capacity ~size:init_count in
  let errors = Vector.with_capacity ~size:init_count in
  List.iter
    (fun (object_: Artifacts.Object.t) ->
      List.iter
        (fun item ->
          match compile_init_item ~layout ~imports ~env item with
          | Ok item_instructions -> Vector.push instructions ~value:item_instructions
          | Error compile_errors -> compile_errors
          |> List.for_each ~fn:(fun error -> Vector.push errors ~value:error))
        object_.program.init)
    linked_program.objects;
  if Vector.is_empty errors then
    Ok (
      Binary.concat
        (Vector.to_array instructions |> Array.to_list)
    )
  else
    Error (Vector.to_array errors |> Array.to_list)

let data_segments = fun layout ->
  Collections.HashMap.fold_left
    layout
    ~init:[]
    ~fn:(fun segments value (data: string_data) ->
      Binary.concat
        [
          Binary.encode_u32 0;
          Binary.byte 0x41;
          Binary.encode_s32 data.offset;
          Binary.byte 0x0b;
          Binary.encode_u32 data.length;
          value;
        ]
      :: segments)
  |> List.rev

let memory_pages = fun layout ->
  let bytes =
    Collections.HashMap.fold_left
      layout
      ~init:0
      ~fn:(fun current _ (data: string_data) -> max current (data.offset + data.length))
  in
  let page_size = 65_536 in
  max 1 ((bytes + page_size - 1) / page_size)

let encode_module = fun ~memory_pages ~imports ~start_body ~layout ->
  let function_types = Binary.vec
    [
      Binary.function_type ~params:[ Binary.byte 0x7f; Binary.byte 0x7f ] ~results:[];
      Binary.function_type ~params:[ Binary.byte 0x7f ] ~results:[];
      Binary.function_type ~params:[] ~results:[];
    ] in
  let import_payload =
    let memory_import = Binary.concat
      [
        Binary.encode_name "riot:runtime";
        Binary.encode_name "memory";
        Binary.byte 0x02;
        Binary.byte 0x00;
        Binary.encode_u32 memory_pages;
      ] in
    let function_imports =
      List.map
        imports
        ~fn:(fun (binding: import_binding) ->
          Binary.concat
            [
              Binary.encode_name binding.import_.module_name;
              Binary.encode_name binding.import_.name;
              Binary.byte 0x00;
              Binary.encode_u32 (type_index_of_signature binding.signature);
            ])
    in
    Binary.vec (memory_import :: function_imports)
  in
  let start_function_index = List.length imports in
  let function_payload = Binary.vec [ Binary.encode_u32 2 ] in
  let export_payload = Binary.vec
    [
      Binary.concat
        [ Binary.encode_name "_start"; Binary.byte 0x00; Binary.encode_u32 start_function_index; ];
    ] in
  let start_payload = Binary.encode_u32 start_function_index in
  let locals = Binary.vec [] in
  let body = Binary.concat [ locals; start_body; Binary.byte 0x0b ] in
  let code_payload = Binary.vec [ Binary.concat [ Binary.encode_u32 (String.length body); body ] ] in
  let data_payload = Binary.vec (data_segments layout) in
  Binary.concat
    [
      Binary.bytes [ 0x00; 0x61; 0x73; 0x6d ];
      Binary.bytes [ 0x01; 0x00; 0x00; 0x00 ];
      Binary.section 1 function_types;
      Binary.section 2 import_payload;
      Binary.section 3 function_payload;
      Binary.section 7 export_payload;
      Binary.section 8 start_payload;
      Binary.section 10 code_payload;
      Binary.section 11 data_payload;
    ]

let node_runner = fun ~wasm_base64 ~memory_pages ->
  String.concat "\n"
    [
      "const wasmBase64 = \"" ^ wasm_base64 ^ "\";";
      "const bytes = Uint8Array.from(Buffer.from(wasmBase64, \"base64\"));";
      "const memory = new WebAssembly.Memory({ initial: " ^ Int.to_string memory_pages ^ " });";
      "const decoder = new TextDecoder();";
      "const runtime = {";
      "  memory,";
      "  print_endline(ptr, len) {";
      "    console.log(decoder.decode(new Uint8Array(memory.buffer, ptr, len)));";
      "  },";
      "  print_string(ptr, len) {";
      "    process.stdout.write(decoder.decode(new Uint8Array(memory.buffer, ptr, len)));";
      "  },";
      "  print_int(value) {";
      "    process.stdout.write(String(value));";
      "  },";
      "  print_char(value) {";
      "    process.stdout.write(String.fromCodePoint(value));";
      "  },";
      "  print_newline() {";
      "    process.stdout.write(\"\\n\");";
      "  },";
      "};";
      "const host = {};";
      "WebAssembly.instantiate(bytes, { \"riot:runtime\": runtime, \"riot:host\": host }).catch((error) => {";
      "  console.error(error);";
      "  process.exitCode = 1;";
      "});";
    ]

let emit_linked_program = fun (linked_program: Artifacts.Linked_program.t) ->
  if linked_program.needs_closure_runtime then
    Error [ Unsupported_closure_runtime ]
  else if not (List.is_empty linked_program.function_table_elements) then
    Error [ Unsupported_indirect_calls ]
  else
    let functions = linked_program.objects
    |> List.map ~fn:(fun (object_: Artifacts.Object.t) -> object_.program.functions)
    |> List.concat in
    let unsupported_functions =
      List.map functions ~fn:(fun function_ -> Unsupported_function function_)
    in
    let globals = linked_program.objects
    |> List.map ~fn:(fun (object_: Artifacts.Object.t) -> object_.program.globals)
    |> List.concat in
    let unsupported_globals =
      List.filter_map globals
        ~fn:(fun (global: Types.Global.t) ->
          match global.expr with
          | Types.Expr.Constant _
          | Types.Expr.Var _
          | Types.Expr.Let _
          | Types.Expr.Sequence _
          | Types.Expr.Direct_call _
          | Types.Expr.If_then_else _ -> None
          | _ -> Some (Unsupported_global global))
    in
    let layout, string_errors = collect_strings linked_program in
    let imports, import_errors = imported_function_bindings linked_program.imports in
    match unsupported_functions @ unsupported_globals @ string_errors @ import_errors with
    | _ :: _ as errors -> Error errors
    | [] ->
        let* start_body = compile_start ~layout ~imports linked_program in
        let memory_pages = memory_pages layout in
        let wasm_binary = encode_module ~memory_pages ~imports ~start_body ~layout in
        let wasm_base64 = Encoding.Base64.encode wasm_binary in
        Ok {
          wasm_base64;
          size_bytes = String.length wasm_binary;
          memory_pages;
          node_runner = node_runner ~wasm_base64 ~memory_pages
        }
