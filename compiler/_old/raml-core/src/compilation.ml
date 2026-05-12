open Std
open Std.Data

type frontend_diagnostic =
  | Parse of Syn.Diagnostic.t
  | Lowering of Typ.Diagnostics.Diagnostic.t
  | Typing of Typ.Diagnostics.Diagnostic.t

type t = {
  targeting: Json.t;
  source: Json.t;
  typing: Json.t;
  core_ir: Json.t;
  frontend_diagnostics: frontend_diagnostic list;
  lowering: Json.t;
  codegen: Json.t;
}

let json_field = Json.get_field

let json_field_string = fun name json ->
  match json_field name json with
  | Some value -> Json.get_string value
  | None -> None

let json_field_array = fun name json ->
  match json_field name json with
  | Some value -> Json.get_array value
  | None -> None

let rec json_nested_field = fun path json ->
  match path with
  | [] -> Some json
  | name :: rest -> (
      match json_field name json with
      | Some value -> json_nested_field rest value
      | None -> None
    )

let json_nested_string = fun path json ->
  match json_nested_field path json with
  | Some value -> Json.get_string value
  | None -> None

let json_nested_int = fun path json ->
  match json_nested_field path json with
  | Some value -> Json.get_int value
  | None -> None

let selected_backend = fun targeting ->
  json_field_string "backend" targeting |> Option.unwrap_or ~default:"unknown"

let selected_lowering = fun ~backend lowered ->
  let stages =
    match backend with
    | "js" -> Json.obj [ ("jir", json_field "jir" lowered |> Option.unwrap_or ~default:Json.null) ]
    | "native" -> Json.obj
      [
        ("nir", json_field "nir" lowered |> Option.unwrap_or ~default:Json.null);
        ("mir", json_field "mir" lowered |> Option.unwrap_or ~default:Json.null);
        ("lir", json_field "lir" lowered |> Option.unwrap_or ~default:Json.null);
      ]
    | "wasm" -> Json.obj
      [ ("wasm", json_field "wasm" lowered |> Option.unwrap_or ~default:Json.null) ]
    | _ -> Json.obj []
  in
  Json.obj [ ("backend", Json.string backend); ("stages", stages) ]

let selected_codegen = fun ~backend ~target codegen ->
  let stage =
    match backend with
    | "js" -> json_field "js" codegen |> Option.unwrap_or ~default:Json.null
    | "native" -> json_field "native" codegen |> Option.unwrap_or ~default:Json.null
    | "wasm" -> json_field "wasm" codegen |> Option.unwrap_or ~default:Json.null
    | _ -> Json.null
  in
  Json.obj [ ("backend", Json.string backend); ("target", target); ("stage", stage) ]

let create = fun ~targeting ~source ~typing ~core_ir ~frontend_diagnostics ~lowering_fields ~codegen_fields ->
  let backend = selected_backend targeting in
  let target = json_field "target" targeting |> Option.unwrap_or ~default:Json.null in
  let lowered = Json.obj lowering_fields in
  let codegen = Json.obj codegen_fields in
  {
    targeting;
    source;
    typing;
    core_ir;
    frontend_diagnostics;
    lowering = selected_lowering ~backend lowered;
    codegen = selected_codegen ~backend ~target codegen;
  }

let from_pipeline_json = fun pipeline ->
  let targeting = json_field "targeting" pipeline |> Option.unwrap_or ~default:Json.null in
  let source = json_field "source" pipeline |> Option.unwrap_or ~default:Json.null in
  let typing = json_field "typing" pipeline |> Option.unwrap_or ~default:Json.null in
  let lowered = json_field "lowered" pipeline |> Option.unwrap_or ~default:Json.null in
  let codegen = json_field "codegen" pipeline |> Option.unwrap_or ~default:Json.null in
  create
    ~targeting
    ~source
    ~typing
    ~core_ir:(json_field "core_ir" lowered |> Option.unwrap_or ~default:Json.null)
    ~frontend_diagnostics:[]
    ~lowering_fields:[
      ("jir", json_field "jir" lowered |> Option.unwrap_or ~default:Json.null);
      ("nir", json_field "nir" lowered |> Option.unwrap_or ~default:Json.null);
      ("mir", json_field "mir" lowered |> Option.unwrap_or ~default:Json.null);
      ("lir", json_field "lir" lowered |> Option.unwrap_or ~default:Json.null);
      ("wasm", json_field "wasm" lowered |> Option.unwrap_or ~default:Json.null);
    ]
    ~codegen_fields:[
      ("js", json_field "js" codegen |> Option.unwrap_or ~default:Json.null);
      ("native", json_field "native" codegen |> Option.unwrap_or ~default:Json.null);
      ("wasm", json_field "wasm" codegen |> Option.unwrap_or ~default:Json.null);
    ]

let to_json = fun compilation ->
  Json.obj
    [
      ("targeting", compilation.targeting);
      ("source", compilation.source);
      ("typing", compilation.typing);
      ("core_ir", compilation.core_ir);
      ("lowering", compilation.lowering);
      ("codegen", compilation.codegen);
    ]

let lowering_to_json = fun compilation ->
  Json.obj
    [
      ("targeting", compilation.targeting);
      ("source", compilation.source);
      ("typing", compilation.typing);
      ("core_ir", compilation.core_ir);
      ("lowering", compilation.lowering);
    ]

let codegen_to_json = to_json

let frontend_diagnostics = fun compilation -> compilation.frontend_diagnostics

let has_frontend_errors = fun compilation -> not (List.is_empty compilation.frontend_diagnostics)

let render_codegen_error = fun error ->
  match json_field_string "kind" error with
  | Some "unsupported_function" ->
      let name = json_nested_string [ "function"; "name" ] error |> Option.unwrap_or ~default:"<anonymous>" in
      "unsupported function: " ^ name
  | Some "unsupported_import" ->
      let module_name = json_nested_string [ "import"; "module_name" ] error
      |> Option.unwrap_or ~default:"<unknown>" in
      let name = json_nested_string [ "import"; "name" ] error |> Option.unwrap_or ~default:"<unknown>" in
      "unsupported import: " ^ module_name ^ "." ^ name
  | Some "unsupported_global" ->
      let name = json_nested_string [ "global"; "name" ] error |> Option.unwrap_or ~default:"<anonymous>" in
      "unsupported global: " ^ name
  | Some "unsupported_expr" ->
      let context = json_field_string "context" error |> Option.unwrap_or ~default:"unknown" in
      let expr_kind = json_nested_string [ "expr"; "kind" ] error |> Option.unwrap_or ~default:"unknown" in
      "unsupported expression in " ^ context ^ ": " ^ expr_kind
  | Some "unsupported_indirect_calls" ->
      "indirect calls are not supported yet"
  | Some "unsupported_closure_runtime" ->
      "closure runtime is not supported yet"
  | Some "unsupported_integer" ->
      let context = json_field_string "context" error |> Option.unwrap_or ~default:"unknown" in
      let value = json_field "value" error
      |> Option.map ~fn:Json.to_string
      |> Option.unwrap_or ~default:"<unknown>" in
      "unsupported integer in " ^ context ^ ": " ^ value
  | Some "unsupported_char" ->
      let value = json_field_string "value" error |> Option.unwrap_or ~default:"<unknown>" in
      "unsupported char literal: " ^ value
  | Some kind ->
      "codegen error (" ^ kind ^ "): " ^ Json.to_string error
  | None ->
      Json.to_string error

let render_codegen_errors = fun errors ->
  match errors with
  | [] -> "codegen failed"
  | _ -> String.concat "\n" (List.map errors ~fn:(fun error -> "- " ^ render_codegen_error error))

let emitted_output = fun compilation ->
  let stage = json_field "stage" compilation.codegen in
  match stage with
  | None -> Error "selected codegen stage is missing"
  | Some stage -> (
      match json_field_string "status" stage with
      | Some "ok" -> (
          match json_field_string "output" stage with
          | Some output -> Ok output
          | None -> Error "selected codegen stage succeeded without an output artifact"
        )
      | Some "blocked" ->
          let blocked_on = json_field_string "blocked_on" stage |> Option.unwrap_or ~default:"unknown" in
          Error ("codegen is blocked on " ^ blocked_on)
      | Some "unavailable" ->
          let reason = json_field_string "reason" stage |> Option.unwrap_or ~default:"unknown" in
          Error ("codegen is unavailable: " ^ reason)
      | Some "error" ->
          let message =
            match json_field_array "errors" stage with
            | Some errors when not (List.is_empty errors) -> render_codegen_errors errors
            | _ -> "codegen failed"
          in
          Error message
      | Some status ->
          Error ("unknown codegen status: " ^ status)
      | None ->
          Error "selected codegen stage is missing a status"
    )
