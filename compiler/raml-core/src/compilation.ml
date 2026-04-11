open Std
open Std.Data

type t = {
  targeting: Json.t;
  source: Json.t;
  typing: Json.t;
  core_ir: Json.t;
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

let create = fun ~targeting ~source ~typing ~core_ir ~lowering_fields ~codegen_fields ->
  let backend = selected_backend targeting in
  let target = json_field "target" targeting |> Option.unwrap_or ~default:Json.null in
  let lowered = Json.obj lowering_fields in
  let codegen = Json.obj codegen_fields in
  {
    targeting;
    source;
    typing;
    core_ir;
    lowering = selected_lowering ~backend lowered;
    codegen = selected_codegen ~backend ~target codegen;
  }

let of_pipeline_json = fun pipeline ->
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

let emitted_output = fun compilation ->
  let stage = json_field "stage" compilation.codegen in
  match stage with
  | None ->
      Error "selected codegen stage is missing"
  | Some stage -> (
      match json_field_string "status" stage with
      | Some "ok" -> (
          match json_field_string "output" stage with
          | Some output -> Ok output
          | None -> Error "selected codegen stage succeeded without an output artifact"
        )
      | Some "blocked" ->
          let blocked_on =
            json_field_string "blocked_on" stage |> Option.unwrap_or ~default:"unknown"
          in
          Error ("codegen is blocked on " ^ blocked_on)
      | Some "unavailable" ->
          let reason = json_field_string "reason" stage |> Option.unwrap_or ~default:"unknown" in
          Error ("codegen is unavailable: " ^ reason)
      | Some "error" ->
          let message =
            match json_field_array "errors" stage with
            | Some errors when not (List.is_empty errors) -> Json.array errors |> Json.to_string_pretty
            | _ -> "codegen failed"
          in
          Error message
      | Some status ->
          Error ("unknown codegen status: " ^ status)
      | None ->
          Error "selected codegen stage is missing a status"
    )
