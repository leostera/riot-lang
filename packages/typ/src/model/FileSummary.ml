open Std

type type_decl = {
  scope_path: SurfacePath.t;
  declaration: TypeDecl.t;
}

type exports = (SurfacePath.t * TypeScheme.t) list

type completeness =
  | Complete
  | Partial

type export_result =
  | TrustedExport of { exports: exports }
  | ErroredExport of { exports: exports }
  | NoExport

type export_status =
  | Trusted
  | Errored
  | Missing

type t = {
  source_id: SourceId.t;
  completeness: completeness;
  export_result: export_result;
  type_decls: type_decl list;
}

let complete = fun ~source_id ?(type_decls = []) exports ->
  { source_id; completeness = Complete; export_result = TrustedExport { exports }; type_decls }

let partial = fun ~source_id ?(type_decls = []) ?exports () ->
  let export_result =
    match exports with
    | Some exports -> ErroredExport { exports }
    | None -> NoExport
  in
  { source_id; completeness = Partial; export_result; type_decls }

let trusted = fun ~source_id ?(type_decls = []) exports -> complete ~source_id ~type_decls exports

let errored = fun ~source_id ?(type_decls = []) exports -> partial ~source_id ~type_decls ~exports ()

let missing = fun ~source_id ?(type_decls = []) () -> partial ~source_id ~type_decls ()

let exports = fun value ->
  match value with
  | { export_result=TrustedExport { exports }; _ }
  | { export_result=ErroredExport { exports }; _ } -> exports
  | { export_result=NoExport; _ } -> []

let completeness = fun summary -> summary.completeness

let export_status = fun value ->
  match value with
  | { completeness=Complete; export_result=TrustedExport _; _ } -> Trusted
  | { completeness=Partial; export_result=TrustedExport _; _ }
  | { export_result=ErroredExport _; _ } -> Errored
  | { export_result=NoExport; _ } -> Missing

let type_decls = fun summary -> summary.type_decls

let exports_to_json = fun exports ->
  Data.Json.Array (exports
  |> List.map
    (fun (name, scheme) ->
      Data.Json.Object [
        ("name", Data.Json.String (SurfacePath.to_string name));
        ("scheme", Data.Json.String (TypePrinter.scheme_to_string scheme));
      ]))

let to_json = fun summary ->
  let completeness =
    match summary.completeness with
    | Complete -> Data.Json.String "complete"
    | Partial -> Data.Json.String "partial"
  in
  let export_result =
    match summary.export_result with
    | TrustedExport { exports } -> Data.Json.Object [
      ("tag", Data.Json.String "trusted_export");
      ("exports", exports_to_json exports);
    ]
    | ErroredExport { exports } -> Data.Json.Object [
      ("tag", Data.Json.String "errored_export");
      ("exports", exports_to_json exports);
    ]
    | NoExport -> Data.Json.Object [
      ("tag", Data.Json.String "no_export");
      ("exports", Data.Json.Array []);
    ]
  in
  Data.Json.Object [
    ("source_id", Data.Json.Int (SourceId.to_int summary.source_id));
    ("completeness", completeness);
    ("export_result", export_result);
  ]

let render_exports = fun exports ->
  match exports with
  | [] -> "none"
  | _ -> exports
  |> List.map
    (fun (name, scheme) ->
      format
        Format.[
          str (SurfacePath.to_string name);
          str " : ";
          str (TypePrinter.scheme_to_string scheme);
        ])
  |> String.concat ", "

let to_string = fun summary ->
  let completeness =
    match summary.completeness with
    | Complete -> "complete"
    | Partial -> "partial"
  in
  match summary.export_result with
  | TrustedExport { exports } -> format
    Format.[
      str "  ";
      str (SourceId.to_string summary.source_id);
      str " ";
      str completeness;
      str " trusted [";
      str (render_exports exports);
      str "]\n";
    ]
  | ErroredExport { exports } -> format
    Format.[
      str "  ";
      str (SourceId.to_string summary.source_id);
      str " ";
      str completeness;
      str " errored [";
      str (render_exports exports);
      str "]\n";
    ]
  | NoExport -> format
    Format.[
      str "  ";
      str (SourceId.to_string summary.source_id);
      str " ";
      str completeness;
      str " no-export\n";
    ]
