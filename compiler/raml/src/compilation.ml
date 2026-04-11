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
