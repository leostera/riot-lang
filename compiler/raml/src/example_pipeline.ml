open Std
open Std.Data

type t = {
  targeting: Json.t;
  source: Json.t;
  typing: Json.t;
  lowered: Json.t;
  codegen: Json.t;
}

type typing_state = {
  json: Json.t;
  semantic_tree: Typ.Model.SemanticTree.file option;
  errors: Json.t list;
  is_complete: bool;
}

type 'value stage = {
  json: Json.t;
  value: 'value option;
  errors: Json.t list;
}

let ambient_print_endline =
  let open Typ.Model in (
    SurfacePath.of_name "print_endline",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.unit_)
  )

let ambient_print_newline =
  let open Typ.Model in (
    SurfacePath.of_name "print_newline",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.unit_ ~rhs:TypeRepr.unit_)
  )

let ambient_print_int =
  let open Typ.Model in (
    SurfacePath.of_name "print_int",
    TypeScheme.of_type (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.unit_)
  )

let ambient_print_string =
  let open Typ.Model in (
    SurfacePath.of_name "print_string",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.unit_)
  )

let ambient_print_char =
  let open Typ.Model in (
    SurfacePath.of_name "print_char",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.char ~rhs:TypeRepr.unit_)
  )

let ambient_mod =
  let open Typ.Model in (
    SurfacePath.of_name "mod",
    TypeScheme.of_type
      (TypeRepr.arrow
        ~label:TypeRepr.Nolabel
        ~lhs:TypeRepr.int
        ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.int))
  )

let ambient_printf =
  let open Typ.Model in
    let result_var_id = 0 in
    let result_var = TypeRepr.make_var result_var_id in
    (
      SurfacePath.of_string "Printf.printf",
      TypeScheme.of_explicit
        ~quantified:[ result_var_id ]
        (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:result_var)
    )

let ambient_sqrt =
  let open Typ.Model in (
    SurfacePath.of_name "sqrt",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.float ~rhs:TypeRepr.float)
  )

let ambient_string_of_int =
  let open Typ.Model in (
    SurfacePath.of_name "string_of_int",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.int ~rhs:TypeRepr.string)
  )

let ambient_string_of_float =
  let open Typ.Model in (
    SurfacePath.of_name "string_of_float",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.float ~rhs:TypeRepr.string)
  )

let ambient_int_of_string =
  let open Typ.Model in (
    SurfacePath.of_name "int_of_string",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.int)
  )

let ambient_float_of_string =
  let open Typ.Model in (
    SurfacePath.of_name "float_of_string",
    TypeScheme.of_type
      (TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:TypeRepr.string ~rhs:TypeRepr.float)
  )

let ambient_list_append =
  let open Typ.Model in
    let element_var_id = 1 in
    let element_var = TypeRepr.make_var element_var_id in
    let list_type = TypeRepr.list element_var in
    (
      SurfacePath.of_name "@",
      TypeScheme.of_explicit
        ~quantified:[ element_var_id ]
        (TypeRepr.arrow
          ~label:TypeRepr.Nolabel
          ~lhs:list_type
          ~rhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:list_type ~rhs:list_type))
    )

let ambient_list_iter =
  let open Typ.Model in
    let element_var_id = 2 in
    let element_var = TypeRepr.make_var element_var_id in
    (
      SurfacePath.of_string "List.iter",
      TypeScheme.of_explicit
        ~quantified:[ element_var_id ]
        (TypeRepr.arrow
          ~label:TypeRepr.Nolabel
          ~lhs:(TypeRepr.arrow ~label:TypeRepr.Nolabel ~lhs:element_var ~rhs:TypeRepr.unit_)
          ~rhs:(TypeRepr.arrow
            ~label:TypeRepr.Nolabel
            ~lhs:(TypeRepr.list element_var)
            ~rhs:TypeRepr.unit_))
    )

let typing_config =
  Typ.Config.default
  |> Typ.Config.with_ambient
    ~ambient:[
      ambient_print_endline;
      ambient_print_newline;
      ambient_print_int;
      ambient_print_string;
      ambient_print_char;
      ambient_mod;
      ambient_printf;
      ambient_sqrt;
      ambient_string_of_int;
      ambient_string_of_float;
      ambient_int_of_string;
      ambient_float_of_string;
      ambient_list_append;
      ambient_list_iter
    ]

let wrap_issue = fun stage diagnostic ->
  Json.obj [ ("stage", Json.string stage); ("diagnostic", diagnostic); ]

let completeness_to_json = fun completeness ->
  match completeness with
  | Typ.Model.FileSummary.Complete -> Json.string "complete"
  | Typ.Model.FileSummary.Partial -> Json.string "partial"

let env_to_json = fun env ->
  Json.array
    (env
    |> List.map
      (fun (name, scheme) ->
        Json.obj
          [
            ("name", Json.string name);
            ("scheme", Json.string (Typ.Model.TypePrinter.scheme_to_string scheme));
          ]))

let ok_stage = fun key render value ->
  {
    json = Json.obj [ ("status", Json.string "ok"); (key, render value); ];
    value = Some value;
    errors = []
  }

let ok_stage_with_json = fun json value -> { json; value = Some value; errors = [] }

let error_stage = fun ~stage errors ->
  {
    json = Json.obj
      [
        ("status", Json.string "error");
        ("stage", Json.string stage);
        ("errors", Json.array errors);
      ];
    value = None;
    errors
  }

let blocked_stage = fun ~blocked_on errors ->
  {
    json = Json.obj
      [
        ("status", Json.string "blocked");
        ("blocked_on", Json.string blocked_on);
        ("errors", Json.array errors);
      ];
    value = None;
    errors
  }

let unavailable_stage = fun ~reason ->
  {
    json = Json.obj [ ("status", Json.string "unavailable"); ("reason", Json.string reason); ];
    value = None;
    errors = []
  }

let backend_to_json = Target.backend_to_json

let targeting_to_json = fun ~host ~target ->
  let backend = Target.select_backend ~host ~target in
  Json.obj
    [
      ("host", Target.to_json host);
      ("target", Target.to_json target);
      ("backend", backend_to_json backend);
    ]

let typing_state_of_parse_failure = fun parse_result error ->
  let parse_diagnostics = Syn.Parser.(parse_result.diagnostics) in
  let lowering_diagnostics =
    match error with
    | Syn.Parse_diagnostics _ -> []
    | Syn.Cst_builder_error builder_error -> [
      Typ.Model.Diagnostic.CstBuilderError { builder_error }
    ]
  in
  let errors = (parse_diagnostics |> List.map Syn.Diagnostic.to_json |> List.map (wrap_issue "parse"))
  @ (lowering_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "lowering")) in
  {
    json = Json.obj
      [
        ("status", Json.string "error");
        ("completeness", Json.string "partial");
        ("file_summary", Json.null);
        ("parse_diagnostics", Json.array (List.map Syn.Diagnostic.to_json parse_diagnostics));
        (
          "lowering_diagnostics",
          Json.array (List.map Typ.Model.Diagnostic.to_json lowering_diagnostics)
        );
        ("typing_diagnostics", Json.array []);
        ("exports", Json.array []);
      ];
    semantic_tree = None;
    errors;
    is_complete = false
  }

let typing_state_of_report = fun (report: Typ.Analysis.Check_result.t) ->
  let completeness = Typ.Model.FileSummary.completeness report.file_summary in
  let parse_issues = report.parse_diagnostics
  |> List.map Syn.Diagnostic.to_json
  |> List.map (wrap_issue "parse") in
  let lowering_issues = report.lowering_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "lowering") in
  let typing_issues = report.typing_diagnostics
  |> List.map Typ.Model.Diagnostic.to_json
  |> List.map (wrap_issue "typing") in
  let has_errors =
    if report.parse_diagnostics = [] then
      if report.lowering_diagnostics = [] then
        if report.typing_diagnostics = [] then
          false
        else
          true
      else
        true
    else
      true
  in
  let is_complete =
    if has_errors then
      false
    else if completeness = Typ.Model.FileSummary.Complete then
      Option.is_some report.semantic_tree
    else
      false
  in
  {
    json =
      Json.obj
        [ (
            "status",
            Json.string
              (
                if is_complete then
                  "ok"
                else
                  "error"
              )
          ); ("completeness", completeness_to_json completeness); (
            "file_summary",
            Typ.Model.FileSummary.to_json report.file_summary
          ); (
            "parse_diagnostics",
            Json.array (List.map Syn.Diagnostic.to_json report.parse_diagnostics)
          ); (
            "lowering_diagnostics",
            Json.array (List.map Typ.Model.Diagnostic.to_json report.lowering_diagnostics)
          ); (
            "typing_diagnostics",
            Json.array (List.map Typ.Model.Diagnostic.to_json report.typing_diagnostics)
          ); ("exports", env_to_json report.exports); ];
    semantic_tree =
      if is_complete then
        report.semantic_tree
      else
        None;
    errors = parse_issues @ lowering_issues @ typing_issues;
    is_complete;
  }

let compile_source = fun ~host ~target ~relpath ~source ->
  Result.map
    (fun source_unit ->
      let selected_backend = Target.select_backend ~host ~target in
      let source_json = Source_unit.to_json source_unit in
      let targeting_json = targeting_to_json ~host ~target in
      let parse_result = Syn.parse ~filename:relpath source in
      let typing =
        match Syn.build_cst parse_result with
        | Ok cst -> Typ.Check.check_source_with_config
          ~config:typing_config
          ~filename:relpath
          ~parse_result
          ~cst
        |> typing_state_of_report
        | Error error -> typing_state_of_parse_failure parse_result error
      in
      let core_ir =
        match typing.semantic_tree with
        | None -> blocked_stage ~blocked_on:"typing" typing.errors
        | Some semantic_tree -> (
            match Typ_lowering.lower_file ~source_unit semantic_tree with
            | Ok compilation_unit -> ok_stage "compilation_unit" Core_ir.Compilation_unit.to_json compilation_unit
            | Error errors -> error_stage
              ~stage:"core_ir"
              (List.map Typ_lowering.error_to_json errors)
          )
      in
      let jir =
        match selected_backend with
        | Target.Js -> (
            match core_ir.value with
            | None -> blocked_stage ~blocked_on:"core_ir" core_ir.errors
            | Some compilation_unit -> (
                match Js.Jir.Lowering.lower_compilation_unit compilation_unit with
                | Ok program -> ok_stage "program" Js.Jir.Program.to_json program
                | Error errors -> error_stage
                  ~stage:"jir"
                  (List.map Js.Jir.Lowering.error_to_json errors)
              )
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let nir =
        match selected_backend with
        | Target.Native -> (
            match core_ir.value with
            | None -> blocked_stage ~blocked_on:"core_ir" core_ir.errors
            | Some compilation_unit -> (
                match Native.Nir.Lowering.lower_compilation_unit_with_trace compilation_unit with
                | Ok trace -> ok_stage_with_json (Native.Nir.Lowering.trace_to_json trace) trace.final
                | Error errors -> error_stage
                  ~stage:"nir"
                  (List.map Native.Nir.Lowering.error_to_json errors)
              )
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let mir =
        match selected_backend with
        | Target.Native -> (
            match nir.value with
            | None -> blocked_stage ~blocked_on:"nir" nir.errors
            | Some program ->
                let trace = Native.Mir.Lowering.lower_program_with_trace program in
                ok_stage_with_json (Native.Mir.Lowering.trace_to_json trace) trace.final
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let lir =
        match selected_backend with
        | Target.Native -> (
            match mir.value with
            | None -> blocked_stage ~blocked_on:"mir" mir.errors
            | Some program ->
                let trace = Native.Lir.Lowering.lower_program_with_trace program in
                ok_stage_with_json (Native.Lir.Lowering.trace_to_json trace) trace.final
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let wasm_lowering =
        match selected_backend with
        | Target.Wasm -> unavailable_stage ~reason:"wasm_lowering_not_implemented"
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let js =
        match selected_backend with
        | Target.Js -> (
            match jir.value with
            | None -> blocked_stage ~blocked_on:"jir" jir.errors
            | Some program ->
                let program = Js.Jst.Lowering.lower_program program in
                ok_stage "output" Json.string (Js.Jst.Emitter.emit_program program)
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let native =
        match selected_backend with
        | Target.Native -> (
            match lir.value with
            | None -> blocked_stage ~blocked_on:"lir" lir.errors
            | Some program -> (
                match Native.Emitter.emit_program ~host ~target program with
                | Ok output -> ok_stage "output" Json.string output
                | Error error -> error_stage
                  ~stage:"native_codegen"
                  [ Native.Emitter.error_to_json error ]
              )
          )
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      let wasm =
        match selected_backend with
        | Target.Wasm -> unavailable_stage ~reason:"wasm_codegen_not_implemented"
        | _ -> unavailable_stage ~reason:"backend_not_selected"
      in
      {
        targeting = targeting_json;
        source = source_json;
        typing = typing.json;
        lowered = Json.obj
          [
            ("core_ir", core_ir.json);
            ("jir", jir.json);
            ("nir", nir.json);
            ("mir", mir.json);
            ("lir", lir.json);
            ("wasm", wasm_lowering.json);
          ];
        codegen = Json.obj [ ("js", js.json); ("native", native.json); ("wasm", wasm.json); ];
      })
    (Source_unit.of_source ~relpath ~source)

let to_json = fun pipeline ->
  Json.obj
    [
      ("targeting", pipeline.targeting);
      ("source", pipeline.source);
      ("typing", pipeline.typing);
      ("lowered", pipeline.lowered);
      ("codegen", pipeline.codegen);
    ]

let lowering_to_json = fun pipeline ->
  Json.obj
    [
      ("targeting", pipeline.targeting);
      ("source", pipeline.source);
      ("typing", pipeline.typing);
      ("lowered", pipeline.lowered);
    ]

let codegen_to_json = fun pipeline ->
  Json.obj
    [
      ("targeting", pipeline.targeting);
      ("source", pipeline.source);
      ("typing", pipeline.typing);
      ("lowered", pipeline.lowered);
      ("codegen", pipeline.codegen);
    ]
