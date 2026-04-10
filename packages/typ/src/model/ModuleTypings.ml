open Std

type definition_site = {
  origin: Source.origin;
  span: Syn.Ceibo.Span.t;
}

type value_definition_target =
  | Site of definition_site
  | Export of SurfacePath.t

type value_definition = {
  export_name: SurfacePath.t;
  target: value_definition_target;
}

type t = {
  module_name: string;
  source_hash: Crypto.hash;
  completeness: FileSummary.completeness;
  export_result: FileSummary.export_result;
  type_decls: FileSummary.type_decl list;
  value_definitions: value_definition list;
}

let type_decl_key = fun (type_decl: FileSummary.type_decl) ->
  SurfacePath.append_name type_decl.scope_path type_decl.declaration.type_name

let map_preserving = fun loop xs ->
  let rec walk changed acc = function
    | [] ->
        if changed then
          List.rev acc
        else
          xs
    | x :: rest ->
        let x2 = loop x in
        walk (changed || not (Std.Ptr.equal x x2)) (x2 :: acc) rest
  in
  walk false [] xs

let local_type_decl_index = fun type_decls ->
  let by_path = Collections.HashMap.with_capacity (List.length type_decls) in
  (
    List.iter
      (fun (type_decl: FileSummary.type_decl) ->
        let _ = Collections.HashMap.insert by_path (type_decl_key type_decl) type_decl in
        ())
      type_decls
  );
  by_path

let resolve_named_type_head_for_persistence = fun by_path name ->
  let qualified_external_head =
    match SurfacePath.to_segments name with
    | _ :: _ :: _ -> Some (TypeRepr.named_head
      ~type_constructor_id:(TypeConstructorId.of_path name)
      ~name)
    | _ -> None
  in
  Collections.HashMap.get by_path name
  |> Option.map
    (fun (type_decl: FileSummary.type_decl) ->
      TypeRepr.named_head ~type_constructor_id:type_decl.declaration.type_constructor_id ~name)
  |> fun resolved ->
    Option.or_else resolved
      (fun () ->
        Option.or_else
          (BuiltinTypeConstructors.head_of_path name)
          (fun () -> qualified_external_head))

let canonicalize_type_for_persistence = fun by_path ->
  let replacements = ref [] in
  let lookup_replacement ty =
    List.find_map
      (fun (candidate, replacement) ->
        if Std.Ptr.equal candidate ty then
          Some replacement
        else
          None)
      !replacements
  in
  let remember_replacement ty replacement =
    replacements := (ty, replacement) :: !replacements;
    replacement
  in
  let remember_identity ty =
    replacements := (ty, ty) :: !replacements;
    ty
  in
  let prepare_shell ty =
    let shell = TypeRepr.shell ~level:(TypeRepr.level ty) () in
    replacements := (ty, shell) :: !replacements;
    shell
  in
  let rec loop ty =
    let ty = TypeRepr.prune ty in
    match lookup_replacement ty with
    | Some replacement -> replacement
    | None ->
        match TypeRepr.view ty with
        | TypeRepr.Int
        | TypeRepr.Float
        | TypeRepr.Bool
        | TypeRepr.String
        | TypeRepr.Char
        | TypeRepr.Unit
        | TypeRepr.Hole _ ->
            remember_identity ty
        | TypeRepr.Var { link=None; _ } ->
            remember_identity ty
        | TypeRepr.Var { link=Some linked; _ } ->
            let replacement = loop linked in
            remember_replacement ty replacement
        | TypeRepr.Option element ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Option (loop element);
            shell
        | TypeRepr.Result (ok_ty, error_ty) ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Result (loop ok_ty, loop error_ty);
            shell
        | TypeRepr.Array element ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Array (loop element);
            shell
        | TypeRepr.List element ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.List (loop element);
            shell
        | TypeRepr.Seq element ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Seq (loop element);
            shell
        | TypeRepr.Package signature ->
            let shell = prepare_shell ty in
            let values =
              signature.values
              |> map_preserving
                (fun (value: TypeRepr.package_value) ->
                  let scheme2 = TypeScheme.map_type_preserving loop value.scheme in
                  if Std.Ptr.equal value.scheme scheme2 then
                    value
                  else
                    { value with scheme = scheme2 })
            in
            shell.TypeRepr.desc <- TypeRepr.Package { values };
            shell
        | TypeRepr.Named { head; arguments } ->
            let arguments2 = map_preserving loop arguments in
            let head2 = resolve_named_type_head_for_persistence by_path head.name
            |> Option.unwrap_or ~default:head in
            (
              match BuiltinTypeConstructors.type_of_path head2.name arguments2 with
              | Some builtin -> remember_replacement ty builtin
              | None ->
                  let shell = prepare_shell ty in
                  shell.TypeRepr.desc <- TypeRepr.Named { head = head2; arguments = arguments2 };
                  shell
            )
        | TypeRepr.PolyVariant { bound; tags; inherited } ->
            let shell = prepare_shell ty in
            let tags2 =
              map_preserving
                (fun (tag: TypeRepr.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> { tag with payload_type = Some (loop payload_type) }
                  | None -> tag)
                tags
            in
            let inherited2 = map_preserving loop inherited in
            shell.TypeRepr.desc <- TypeRepr.PolyVariant { bound; tags = tags2; inherited = inherited2 };
            shell
        | TypeRepr.Tuple members ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Tuple (map_preserving loop members);
            shell
        | TypeRepr.Arrow { label; lhs; rhs } ->
            let shell = prepare_shell ty in
            shell.TypeRepr.desc <- TypeRepr.Arrow { label; lhs = loop lhs; rhs = loop rhs };
            shell
  in
  loop

let canonicalize_scheme_for_persistence = fun canonicalize_type scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  let body2 = canonicalize_type body in
  if Std.Ptr.equal body body2 then
    scheme
  else
    TypeScheme.of_explicit ~quantified body2

let canonicalize_type_decl_for_persistence = fun canonicalize_type (type_decl: FileSummary.type_decl) ->
  let declaration = type_decl.declaration in
  let manifest =
    match declaration.manifest with
    | None -> None
    | Some (TypeDecl.Alias manifest_type) -> Some (TypeDecl.Alias (canonicalize_type manifest_type))
    | Some (TypeDecl.PolyVariant { bound; tags; inherited }) ->
        Some (
          TypeDecl.PolyVariant {
            bound;
            tags =
              List.map
                (fun (tag: TypeDecl.poly_variant_tag) ->
                  match tag.payload_type with
                  | Some payload_type -> {
                    tag
                    with payload_type = Some (canonicalize_type payload_type)
                  }
                  | None -> tag)
                tags;
            inherited = List.map canonicalize_type inherited;
          }
        )
  in
  let constructors = declaration.constructors
  |> List.map
    (fun (constructor: TypeDecl.constructor) ->
      {
        constructor
        with scheme = canonicalize_scheme_for_persistence canonicalize_type constructor.scheme
      }) in
  let labels = declaration.labels
  |> List.map
    (fun (label: TypeDecl.label) -> {
      label
      with field_type = canonicalize_scheme_for_persistence canonicalize_type label.field_type
    }) in
  { type_decl with declaration = { declaration with manifest; constructors; labels } }

let canonicalize_export_result_for_persistence = fun canonicalize_type export_result ->
  match export_result with
  | FileSummary.TrustedExport { exports } -> FileSummary.TrustedExport {
    exports = List.map
      (fun (name, scheme) -> (name, canonicalize_scheme_for_persistence canonicalize_type scheme))
      exports
  }
  | FileSummary.ErroredExport { exports } -> FileSummary.ErroredExport {
    exports = List.map
      (fun (name, scheme) -> (name, canonicalize_scheme_for_persistence canonicalize_type scheme))
      exports
  }
  | FileSummary.NoExport -> FileSummary.NoExport

let canonicalize_payload_for_persistence = fun ~export_result ~type_decls ->
  let by_path = local_type_decl_index type_decls in
  let canonicalize_type = canonicalize_type_for_persistence by_path in
  (
    canonicalize_export_result_for_persistence canonicalize_type export_result,
    List.map (canonicalize_type_decl_for_persistence canonicalize_type) type_decls
  )

let complete = fun ~module_name ~source_hash ?(type_decls = []) ?(value_definitions = []) exports ->
  let (export_result, type_decls) = canonicalize_payload_for_persistence
    ~export_result:(FileSummary.TrustedExport { exports })
    ~type_decls in
  {
    module_name;
    source_hash;
    completeness = FileSummary.Complete;
    export_result;
    type_decls;
    value_definitions;
  }

let partial = fun ~module_name ~source_hash ?(type_decls = []) ?(value_definitions = []) ?exports () ->
  let export_result =
    match exports with
    | Some exports -> FileSummary.ErroredExport { exports }
    | None -> FileSummary.NoExport
  in
  let (export_result, type_decls) = canonicalize_payload_for_persistence ~export_result ~type_decls in
  {
    module_name;
    source_hash;
    completeness = FileSummary.Partial;
    export_result;
    type_decls;
    value_definitions;
  }

let trusted = fun ~module_name ~source_hash ?(type_decls = []) ?(value_definitions = []) exports ->
  complete ~module_name ~source_hash ~type_decls ~value_definitions exports

let errored = fun ~module_name ~source_hash ?(type_decls = []) ?(value_definitions = []) exports ->
  partial ~module_name ~source_hash ~type_decls ~value_definitions ~exports ()

let missing = fun ~module_name ~source_hash ?(type_decls = []) ?(value_definitions = []) () ->
  partial ~module_name ~source_hash ~type_decls ~value_definitions ()

let of_file_summary = fun ~module_name ~source_hash ?(value_definitions = []) (
  summary: FileSummary.t
) ->
  let (export_result, type_decls) = canonicalize_payload_for_persistence
    ~export_result:summary.export_result
    ~type_decls:summary.type_decls in
  {
    module_name;
    source_hash;
    completeness = summary.completeness;
    export_result;
    type_decls;
    value_definitions;
  }

let to_file_summary = fun ~source_id summary ->
  {
    FileSummary.source_id;
    completeness = summary.completeness;
    export_result = summary.export_result;
    type_decls = summary.type_decls
  }

let module_name = fun summary -> summary.module_name

let source_hash = fun summary -> summary.source_hash

let export_result = fun summary -> summary.export_result

let completeness = fun summary -> summary.completeness

let export_status = fun summary ->
  match summary.export_result with
  | FileSummary.TrustedExport _ -> (
      match summary.completeness with
      | FileSummary.Complete -> FileSummary.Trusted
      | FileSummary.Partial -> FileSummary.Errored
    )
  | FileSummary.ErroredExport _ ->
      FileSummary.Errored
  | FileSummary.NoExport ->
      FileSummary.Missing

let exports = fun value ->
  match value with
  | { export_result=FileSummary.TrustedExport { exports }; _ }
  | { export_result=FileSummary.ErroredExport { exports }; _ } -> exports
  | { export_result=FileSummary.NoExport; _ } -> []

let type_decls = fun summary -> summary.type_decls

let value_definitions = fun summary -> summary.value_definitions

let find_value_definition = fun summary ~export_name ->
  summary.value_definitions |> List.find_map
    (fun (definition: value_definition) ->
      if SurfacePath.equal export_name definition.export_name then
        Some definition.target
      else
        None)

let rec json_type_name = fun value ->
  match value with
  | Data.Json.Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed t -> json_type_name t

let error_expected = fun expected actual ->
  Error (format
    Format.[ str "expected "; str expected; str " but got "; str (json_type_name actual) ])

let get_object = fun value ->
  match value with
  | Data.Json.Object fields -> Ok fields
  | other -> error_expected "object" other

let get_array = fun value ->
  match value with
  | Data.Json.Array values -> Ok values
  | other -> error_expected "array" other

let get_string = fun value ->
  match value with
  | Data.Json.String value -> Ok value
  | other -> error_expected "string" other

let get_int = fun value ->
  match value with
  | Data.Json.Int value -> Ok value
  | other -> error_expected "int" other

let span_of_json = fun json ->
  match get_object json with
  | Error _ as err -> err
  | Ok fields -> (
      match List.assoc_opt "start" fields, List.assoc_opt "end" fields with
      | Some start_json, Some end_json -> (
          match get_int start_json with
          | Error _ as err -> err
          | Ok start -> (
              match get_int end_json with
              | Error _ as err -> err
              | Ok end_ -> Ok (Syn.Ceibo.Span.make ~start ~end_)
            )
        )
      | None, _ ->
          Error "missing field start"
      | _, None ->
          Error "missing field end"
    )

let field = fun name fields ->
  match List.assoc_opt name fields with
  | Some value -> Ok value
  | None -> Error (format Format.[ str "missing field "; str name ])

let ( let* ) result f =
  match result with
  | Ok value -> f value
  | Error _ as err -> err

let label_to_json = fun value ->
  match value with
  | TypeRepr.Nolabel -> Data.Json.Object [ ("tag", Data.Json.String "nolabel") ]
  | TypeRepr.Labelled label -> Data.Json.Object [
    ("tag", Data.Json.String "labeled");
    ("label", Data.Json.String label);
  ]
  | TypeRepr.Optional label -> Data.Json.Object [
    ("tag", Data.Json.String "optional");
    ("label", Data.Json.String label);
  ]

let rec type_to_json = fun ty ->
  match TypeRepr.view (TypeRepr.prune ty) with
  | TypeRepr.Int ->
      Data.Json.Object [ ("tag", Data.Json.String "int") ]
  | TypeRepr.Float ->
      Data.Json.Object [ ("tag", Data.Json.String "float") ]
  | TypeRepr.Bool ->
      Data.Json.Object [ ("tag", Data.Json.String "bool") ]
  | TypeRepr.String ->
      Data.Json.Object [ ("tag", Data.Json.String "string") ]
  | TypeRepr.Char ->
      Data.Json.Object [ ("tag", Data.Json.String "char") ]
  | TypeRepr.Unit ->
      Data.Json.Object [ ("tag", Data.Json.String "unit") ]
  | TypeRepr.Option element ->
      Data.Json.Object [ ("tag", Data.Json.String "option"); ("element", type_to_json element); ]
  | TypeRepr.Result (ok_ty, error_ty) ->
      Data.Json.Object [
        ("tag", Data.Json.String "result");
        ("ok", type_to_json ok_ty);
        ("error", type_to_json error_ty);
      ]
  | TypeRepr.Array element ->
      Data.Json.Object [ ("tag", Data.Json.String "array"); ("element", type_to_json element); ]
  | TypeRepr.List element ->
      Data.Json.Object [ ("tag", Data.Json.String "list"); ("element", type_to_json element); ]
  | TypeRepr.Seq element ->
      Data.Json.Object [ ("tag", Data.Json.String "seq"); ("element", type_to_json element); ]
  | TypeRepr.Package signature ->
      Data.Json.Object [ ("tag", Data.Json.String "package"); (
          "values",
          Data.Json.Array (
            signature.values |> List.map
              (fun (value: TypeRepr.package_value) ->
                let quantified, body = TypeScheme.to_explicit value.scheme in
                Data.Json.Object [
                  ("name", Data.Json.String value.name);
                  (
                    "scheme",
                    Data.Json.Object [
                      (
                        "quantified",
                        Data.Json.Array (List.map (fun id -> Data.Json.Int id) quantified)
                      );
                      ("body", type_to_json body);
                    ]
                  );
                ])
          )
        ); ]
  | TypeRepr.Named { head={ type_constructor_id; name }; arguments } ->
      Data.Json.Object [
        ("tag", Data.Json.String "named");
        ("type_constructor_id", TypeConstructorId.to_json type_constructor_id);
        ("name", Data.Json.String (SurfacePath.to_string name));
        ("arguments", Data.Json.Array (List.map type_to_json arguments));
      ]
  | TypeRepr.PolyVariant { bound; tags; inherited } ->
      let bound =
        match bound with
        | TypeRepr.Exact -> "exact"
        | TypeRepr.UpperBound -> "upper"
        | TypeRepr.LowerBound -> "lower"
      in
      let tag_to_json (tag: TypeRepr.poly_variant_tag) =
        let fields = [ ("name", Data.Json.String tag.name) ] in
        let fields =
          match tag.payload_type with
          | Some payload_type -> fields @ [ ("payload_type", type_to_json payload_type) ]
          | None -> fields
        in
        Data.Json.Object fields
      in
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("bound", Data.Json.String bound);
        ("tags", Data.Json.Array (List.map tag_to_json tags));
        ("inherited", Data.Json.Array (List.map type_to_json inherited));
      ]
  | TypeRepr.Tuple members ->
      Data.Json.Object [
        ("tag", Data.Json.String "tuple");
        ("members", Data.Json.Array (List.map type_to_json members));
      ]
  | TypeRepr.Arrow { label; lhs; rhs } ->
      Data.Json.Object [
        ("tag", Data.Json.String "arrow");
        ("label", label_to_json label);
        ("lhs", type_to_json lhs);
        ("rhs", type_to_json rhs);
      ]
  | TypeRepr.Var { id; link=None; _ } ->
      Data.Json.Object [ ("tag", Data.Json.String "var"); ("id", Data.Json.Int id); ]
  | TypeRepr.Var { link=Some linked; _ } ->
      type_to_json linked
  | TypeRepr.Hole id ->
      Data.Json.Object [ ("tag", Data.Json.String "hole"); ("id", Data.Json.Int id); ]

let scheme_to_json = fun scheme ->
  let quantified, body = TypeScheme.to_explicit scheme in
  Data.Json.Object [
    ("quantified", Data.Json.Array (List.map (fun id -> Data.Json.Int id) quantified));
    ("body", type_to_json body);
  ]

let exports_to_json = fun exports ->
  Data.Json.Array (
    exports |> List.map
      (fun (name, scheme) ->
        let scheme_json =
          try scheme_to_json scheme with
          | Failure message -> raise
            (Failure (format
              Format.[ str "module typings export "; str (SurfacePath.to_string name); str ": "; str
                  message; ]))
        in
        Data.Json.Object [ ("name", Data.Json.String (SurfacePath.to_string name)); ("scheme", scheme_json); ])
  )

let label_decl_to_json = fun (label: TypeDecl.label) ->
  Data.Json.Object [
    ("label_id", Data.Json.Int (LabelId.to_int label.label_id));
    ("name", Data.Json.String label.name);
    ("field_type", scheme_to_json label.field_type);
    ("mutable", Data.Json.Bool label.mutable_);
  ]

let constructor_to_json = fun (constructor: TypeDecl.constructor) ->
  let fields = [
    ("constructor_id", Data.Json.Int (ConstructorId.to_int constructor.constructor_id));
    ("name", Data.Json.String constructor.name);
    ("scheme", scheme_to_json constructor.scheme);
  ] in
  let fields =
    match constructor.inline_record_labels with
    | Some labels -> fields
    @ [ ("inline_record_labels", Data.Json.Array (List.map label_decl_to_json labels)) ]
    | None -> fields
  in
  Data.Json.Object fields

let manifest_to_json = fun value ->
  match value with
  | TypeDecl.Alias manifest_type -> Data.Json.Object [
    ("tag", Data.Json.String "alias");
    ("type", type_to_json manifest_type);
  ]
  | TypeDecl.PolyVariant { bound; tags; inherited } ->
      let bound =
        match bound with
        | TypeDecl.Exact -> "exact"
        | TypeDecl.UpperBound -> "upper"
        | TypeDecl.LowerBound -> "lower"
      in
      let tag_to_json (tag: TypeDecl.poly_variant_tag) =
        let fields = [ ("name", Data.Json.String tag.name) ] in
        let fields =
          match tag.payload_type with
          | Some payload_type -> fields @ [ ("payload_type", type_to_json payload_type) ]
          | None -> fields
        in
        Data.Json.Object fields
      in
      Data.Json.Object [
        ("tag", Data.Json.String "poly_variant");
        ("bound", Data.Json.String bound);
        ("tags", Data.Json.Array (List.map tag_to_json tags));
        ("inherited", Data.Json.Array (List.map type_to_json inherited));
      ]

let type_decl_to_json = fun (type_decl: FileSummary.type_decl) ->
  let fields = [
    (
      "scope_path",
      Data.Json.Array (SurfacePath.to_segments type_decl.scope_path
      |> List.map (fun segment -> Data.Json.String segment))
    );
    ("type_constructor_id", TypeConstructorId.to_json type_decl.declaration.type_constructor_id);
    ("type_name", Data.Json.String type_decl.declaration.type_name);
    ("nonrec", Data.Json.Bool type_decl.declaration.nonrec_);
    (
      "param_ids",
      Data.Json.Array (List.map (fun id -> Data.Json.Int id) type_decl.declaration.param_ids)
    );
    (
      "param_variances",
      Data.Json.Array (List.map
        (fun variance -> Data.Json.String (TypeDecl.variance_to_string variance))
        type_decl.declaration.param_variances)
    );
    (
      "constructors",
      Data.Json.Array (List.map constructor_to_json type_decl.declaration.constructors)
    );
    ("labels", Data.Json.Array (List.map label_decl_to_json type_decl.declaration.labels));
  ] in
  let fields =
    match type_decl.declaration.manifest with
    | Some manifest -> fields @ [ ("manifest", manifest_to_json manifest) ]
    | None -> fields
  in
  Data.Json.Object fields

let type_decls_to_json = fun type_decls -> Data.Json.Array (List.map type_decl_to_json type_decls)

let export_result_to_json = fun value ->
  match value with
  | FileSummary.TrustedExport { exports } -> Data.Json.Object [
    ("tag", Data.Json.String "trusted_export");
    ("exports", exports_to_json exports);
  ]
  | FileSummary.ErroredExport { exports } -> Data.Json.Object [
    ("tag", Data.Json.String "errored_export");
    ("exports", exports_to_json exports);
  ]
  | FileSummary.NoExport -> Data.Json.Object [
    ("tag", Data.Json.String "no_export");
    ("exports", Data.Json.Array []);
  ]

let completeness_to_json = fun value ->
  match value with
  | FileSummary.Complete -> Data.Json.String "complete"
  | FileSummary.Partial -> Data.Json.String "partial"

let span_to_json = fun (span: Syn.Ceibo.Span.t) ->
  Data.Json.Object [ ("start", Data.Json.Int span.start); ("end", Data.Json.Int span.end_); ]

let source_origin_to_json = fun value ->
  match value with
  | Source.Path path -> Data.Json.Object [
    ("tag", Data.Json.String "path");
    ("value", Data.Json.String (Path.to_string path));
  ]
  | Source.Label label -> Data.Json.Object [
    ("tag", Data.Json.String "label");
    ("value", Data.Json.String label);
  ]

let value_definition_target_to_json = fun value ->
  match value with
  | Site { origin; span } -> Data.Json.Object [
    ("tag", Data.Json.String "site");
    ("origin", source_origin_to_json origin);
    ("span", span_to_json span);
  ]
  | Export path -> Data.Json.Object [
    ("tag", Data.Json.String "export");
    ("path", Data.Json.String (SurfacePath.to_string path));
  ]

let value_definition_to_json = fun (definition: value_definition) ->
  Data.Json.Object [
    ("export_name", Data.Json.String (SurfacePath.to_string definition.export_name));
    ("target", value_definition_target_to_json definition.target);
  ]

let value_definitions_to_json = fun value_definitions ->
  Data.Json.Array (List.map value_definition_to_json value_definitions)

let payload_to_json = fun ~completeness ~export_result ~type_decls ~value_definitions ->
  Data.Json.Object [
    ("completeness", completeness_to_json completeness);
    ("export_result", export_result_to_json export_result);
    ("type_decls", type_decls_to_json type_decls);
    ("value_definitions", value_definitions_to_json value_definitions);
  ]

let synthetic_source_hash = fun ~module_name ~export_result ~type_decls ?(value_definitions = []) () ->
  let completeness =
    match export_result with
    | FileSummary.TrustedExport _ -> FileSummary.Complete
    | FileSummary.ErroredExport _
    | FileSummary.NoExport -> FileSummary.Partial
  in
  payload_to_json ~completeness ~export_result ~type_decls ~value_definitions
  |> Data.Json.to_string
  |> fun json ->
    Crypto.hash_string
      (format Format.[ str "typ-module-typings\x1f"; str module_name; str "\x1f"; str json ])

let label_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "nolabel" ->
      Ok TypeRepr.Nolabel
  | "labeled" ->
      let* label_json = field "label" fields in
      let* label = get_string label_json in
      Ok (TypeRepr.Labelled label)
  | "optional" ->
      let* label_json = field "label" fields in
      let* label = get_string label_json in
      Ok (TypeRepr.Optional label)
  | other ->
      Error (format Format.[ str "unknown module typings type label tag "; str other ])

let rec type_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "int" ->
      Ok TypeRepr.int
  | "float" ->
      Ok TypeRepr.float
  | "bool" ->
      Ok TypeRepr.bool
  | "string" ->
      Ok TypeRepr.string
  | "char" ->
      Ok TypeRepr.char
  | "unit" ->
      Ok TypeRepr.unit_
  | "option" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.option element)
  | "result" ->
      let* ok_json = field "ok" fields in
      let* error_json = field "error" fields in
      let* ok_ty = type_of_json ok_json in
      let* error_ty = type_of_json error_json in
      Ok (TypeRepr.result ok_ty error_ty)
  | "array" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.array element)
  | "list" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.list element)
  | "seq" ->
      let* element_json = field "element" fields in
      let* element = type_of_json element_json in
      Ok (TypeRepr.seq element)
  | "package" ->
      let* values_json = field "values" fields in
      let* values_json = get_array values_json in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value_json :: rest ->
            let* value_fields = get_object value_json in
            let* name_json = field "name" value_fields in
            let* scheme_json = field "scheme" value_fields in
            let* name = get_string name_json in
            let* scheme = scheme_of_json scheme_json in
            loop (TypeRepr.package_value ~name ~scheme :: acc) rest
      in
      let* values = loop [] values_json in
      Ok (TypeRepr.package ~values)
  | "named" ->
      let* type_constructor_json = field "type_constructor_id" fields in
      let* type_constructor_id = TypeConstructorId.of_json type_constructor_json in
      let* name_json = field "name" fields in
      let* name = get_string name_json in
      let name = SurfacePath.of_string name in
      let* arguments_json = field "arguments" fields in
      let* arguments_json = get_array arguments_json in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | head :: tail ->
            let* argument = type_of_json head in
            loop (argument :: acc) tail
      in
      let* arguments = loop [] arguments_json in
      Ok (TypeRepr.named ~head:(TypeRepr.named_head ~type_constructor_id ~name) ~arguments)
  | "poly_variant" ->
      let* bound_json = field "bound" fields in
      let* tags_json = field "tags" fields in
      let* inherited_json = field "inherited" fields in
      let* bound = get_string bound_json in
      let bound =
        match bound with
        | "exact" -> Ok TypeRepr.Exact
        | "upper" -> Ok TypeRepr.UpperBound
        | "lower" -> Ok TypeRepr.LowerBound
        | other -> Error (format
          Format.[ str "unknown module typings structural poly-variant bound "; str other ])
      in
      let* bound = bound in
      let* tags_json = get_array tags_json in
      let rec parse_tags acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* fields = get_object value in
            let* name_json = field "name" fields in
            let* name = get_string name_json in
            let payload_type =
              match List.assoc_opt "payload_type" fields with
              | Some payload_type_json -> type_of_json payload_type_json |> Result.map Option.some
              | None -> Ok None
            in
            let* payload_type = payload_type in
            parse_tags (TypeRepr.poly_variant_tag ?payload_type name :: acc) rest
      in
      let* tags = parse_tags [] tags_json in
      let* inherited_json = get_array inherited_json in
      let rec parse_inherited acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* inherited_type = type_of_json value in
            parse_inherited (inherited_type :: acc) rest
      in
      let* inherited = parse_inherited [] inherited_json in
      Ok (TypeRepr.poly_variant ~bound ~tags ~inherited)
  | "tuple" ->
      let* members_json = field "members" fields in
      let* members_json = get_array members_json in
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | head :: tail ->
            let* member = type_of_json head in
            loop (member :: acc) tail
      in
      let* members = loop [] members_json in
      Ok (TypeRepr.tuple members)
  | "arrow" ->
      let* label_json = field "label" fields in
      let* lhs_json = field "lhs" fields in
      let* rhs_json = field "rhs" fields in
      let* label = label_of_json label_json in
      let* lhs = type_of_json lhs_json in
      let* rhs = type_of_json rhs_json in
      Ok (TypeRepr.arrow ~label ~lhs ~rhs)
  | "var" ->
      let* id_json = field "id" fields in
      let* id = get_int id_json in
      Ok (TypeRepr.make_var id)
  | "hole" ->
      let* id_json = field "id" fields in
      let* id = get_int id_json in
      Ok (TypeRepr.hole id)
  | other ->
      Error (format Format.[ str "unknown module typings type tag "; str other ])

and scheme_of_json = function
  | Data.Json.Object fields -> (
      match (List.assoc_opt "quantified" fields, List.assoc_opt "body" fields) with
      | (Some (Data.Json.Array quantified_json), Some body_json) ->
          let rec parse_quantified acc = function
            | [] -> Ok (List.rev acc)
            | Data.Json.Int id :: rest -> parse_quantified (id :: acc) rest
            | other :: _ -> Error ("expected quantified type variable id int but got "
            ^ json_type_name other)
          in
          begin
            match parse_quantified [] quantified_json, type_of_json body_json with
            | Ok quantified, Ok body -> Ok (TypeScheme.of_explicit ~quantified body)
            | Error err, _ -> Error err
            | _, Error err -> Error err
          end
      | _ -> Error "expected module typings type scheme object with quantified and body fields"
    )
  | other -> Error ("expected module typings type scheme object but got " ^ json_type_name other)

let exports_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* fields = get_object value in
        let* name_json = field "name" fields in
        let* scheme_json = field "scheme" fields in
        let* name = get_string name_json in
        let* scheme = scheme_of_json scheme_json in
        loop ((SurfacePath.of_string name, scheme) :: acc) rest
  in
  loop [] values

let int_list_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* value = get_int value in
        loop (value :: acc) rest
  in
  loop [] values

let string_list_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* value = get_string value in
        loop (value :: acc) rest
  in
  loop [] values

let constructor_of_json = fun json ->
  let* fields = get_object json in
  let* constructor_id_json = field "constructor_id" fields in
  let* name_json = field "name" fields in
  let* scheme_json = field "scheme" fields in
  let generalized_json = List.assoc_opt "generalized" fields in
  let* constructor_id = get_int constructor_id_json in
  let* name = get_string name_json in
  let* scheme = scheme_of_json scheme_json in
  let* generalized =
    match generalized_json with
    | Some (Data.Json.Bool generalized) -> Ok generalized
    | Some other -> error_expected "bool" other
    | None -> Ok false
  in
  let* inline_record_labels =
    match List.assoc_opt "inline_record_labels" fields with
    | Some labels_json ->
        let* labels_json = get_array labels_json in
        let parse_label_json label_json =
          let* fields = get_object label_json in
          let* label_id_json = field "label_id" fields in
          let* name_json = field "name" fields in
          let* field_type_json = field "field_type" fields in
          let* mutable_json = field "mutable" fields in
          let* label_id = get_int label_id_json in
          let* name = get_string name_json in
          let* field_type = scheme_of_json field_type_json in
          let mutable_ =
            match mutable_json with
            | Data.Json.Bool mutable_ -> Ok mutable_
            | other -> error_expected "bool" other
          in
          let* mutable_ = mutable_ in
          Ok { TypeDecl.label_id = LabelId.of_int label_id; name; field_type; mutable_ }
        in
        let rec parse_labels acc = function
          | [] -> Ok (Some (List.rev acc))
          | label_json :: rest ->
              let* label = parse_label_json label_json in
              parse_labels (label :: acc) rest
        in
        parse_labels [] labels_json
    | None -> Ok None
  in
  Ok (
    {
      TypeDecl.constructor_id = ConstructorId.of_int constructor_id;
      name;
      scheme;
      generalized;
      inline_record_labels
    }:
      TypeDecl.constructor
  )

let label_decl_of_json = fun json ->
  let* fields = get_object json in
  let* label_id_json = field "label_id" fields in
  let* name_json = field "name" fields in
  let* field_type_json = field "field_type" fields in
  let* mutable_json = field "mutable" fields in
  let* label_id = get_int label_id_json in
  let* name = get_string name_json in
  let* field_type = scheme_of_json field_type_json in
  let mutable_ =
    match mutable_json with
    | Data.Json.Bool mutable_ -> Ok mutable_
    | other -> error_expected "bool" other
  in
  let* mutable_ = mutable_ in
  Ok ({ TypeDecl.label_id = LabelId.of_int label_id; name; field_type; mutable_ }: TypeDecl.label)

let poly_variant_tag_of_json = fun json ->
  let* fields = get_object json in
  let* name_json = field "name" fields in
  let* name = get_string name_json in
  let payload_type =
    match List.assoc_opt "payload_type" fields with
    | Some payload_type_json -> type_of_json payload_type_json |> Result.map Option.some
    | None -> Ok None
  in
  let* payload_type = payload_type in
  Ok ({ TypeDecl.name = name; payload_type }: TypeDecl.poly_variant_tag)

let manifest_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "alias" ->
      let* type_json = field "type" fields in
      let* manifest_type = type_of_json type_json in
      Ok (TypeDecl.Alias manifest_type)
  | "poly_variant" ->
      let* bound_json = field "bound" fields in
      let* tags_json = field "tags" fields in
      let* inherited_json = field "inherited" fields in
      let* bound = get_string bound_json in
      let bound =
        match bound with
        | "exact" -> Ok TypeDecl.Exact
        | "upper" -> Ok TypeDecl.UpperBound
        | "lower" -> Ok TypeDecl.LowerBound
        | other -> Error (format
          Format.[ str "unknown module typings poly-variant bound "; str other ])
      in
      let* bound = bound in
      let* tags_json = get_array tags_json in
      let rec parse_tags acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* tag = poly_variant_tag_of_json value in
            parse_tags (tag :: acc) rest
      in
      let* tags = parse_tags [] tags_json in
      let* inherited_json = get_array inherited_json in
      let rec parse_inherited acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* inherited = type_of_json value in
            parse_inherited (inherited :: acc) rest
      in
      let* inherited = parse_inherited [] inherited_json in
      Ok (TypeDecl.PolyVariant { bound; tags; inherited })
  | other ->
      Error (format Format.[ str "unknown module typings manifest tag "; str other ])

let type_decl_of_json = fun json ->
  let* fields = get_object json in
  let* scope_path_json = field "scope_path" fields in
  let* type_constructor_id_json = field "type_constructor_id" fields in
  let* type_name_json = field "type_name" fields in
  let* param_ids_json = field "param_ids" fields in
  let param_variances_json = List.assoc_opt "param_variances" fields in
  let* constructors_json = field "constructors" fields in
  let* labels_json = field "labels" fields in
  let* scope_path = string_list_of_json scope_path_json in
  let* type_constructor_id = TypeConstructorId.of_json type_constructor_id_json in
  let* type_name = get_string type_name_json in
  let* param_ids = int_list_of_json param_ids_json in
  let param_variances =
    match param_variances_json with
    | Some param_variances_json ->
        let* values = get_array param_variances_json in
        let rec loop acc = function
          | [] -> Ok (List.rev acc)
          | value :: rest ->
              let* variance_name = get_string value in
              let* variance =
                match variance_name with
                | "covariant" -> Ok TypeDecl.Covariant
                | "contravariant" -> Ok TypeDecl.Contravariant
                | "invariant" -> Ok TypeDecl.Invariant
                | other -> Error (format Format.[ str "unknown module typings variance "; str other ])
              in
              loop (variance :: acc) rest
        in
        loop [] values
    | None -> Ok (List.map (fun _ -> TypeDecl.Invariant) param_ids)
  in
  let* param_variances = param_variances in
  let* constructors_json = get_array constructors_json in
  let rec parse_constructors acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* constructor = constructor_of_json value in
        parse_constructors (constructor :: acc) rest
  in
  let* constructors = parse_constructors [] constructors_json in
  let* labels_json = get_array labels_json in
  let rec parse_labels acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* label = label_decl_of_json value in
        parse_labels (label :: acc) rest
  in
  let* labels = parse_labels [] labels_json in
  let manifest =
    match List.assoc_opt "manifest" fields with
    | Some manifest_json -> manifest_of_json manifest_json |> Result.map Option.some
    | None -> Ok None
  in
  let nonrec_ =
    match List.assoc_opt "nonrec" fields with
    | Some (Data.Json.Bool value) -> Ok value
    | Some other -> error_expected "bool" other
    | None -> Ok false
  in
  let* nonrec_ = nonrec_ in
  let* manifest = manifest in
  Ok {
    FileSummary.scope_path = SurfacePath.of_segments scope_path;
    declaration =
      {
        TypeDecl.type_constructor_id = type_constructor_id;
        TypeDecl.type_name = type_name;
        nonrec_;
        param_ids;
        param_variances;
        constructors;
        labels;
        manifest;
      };
  }

let type_decls_of_json = fun json ->
  let* values = get_array json in
  let rec loop acc = function
    | [] -> Ok (List.rev acc)
    | value :: rest ->
        let* type_decl = type_decl_of_json value in
        loop (type_decl :: acc) rest
  in
  loop [] values

let export_result_of_json = fun json ->
  let* export_result_fields = get_object json in
  let* tag_json = field "tag" export_result_fields in
  let* exports_json = field "exports" export_result_fields in
  let* tag = get_string tag_json in
  let* exports = exports_of_json exports_json in
  match tag with
  | "trusted_export" -> Ok (FileSummary.TrustedExport { exports })
  | "errored_export" -> Ok (FileSummary.ErroredExport { exports })
  | "no_export" -> Ok FileSummary.NoExport
  | other -> Error (format Format.[ str "unknown module typings export_result tag "; str other ])

let completeness_of_json = fun json ->
  let* value = get_string json in
  match value with
  | "complete" -> Ok FileSummary.Complete
  | "partial" -> Ok FileSummary.Partial
  | other -> Error (format Format.[ str "unknown module typings completeness "; str other ])

let completeness_of_export_result = fun export_result ->
  match export_result with
  | FileSummary.TrustedExport _ -> FileSummary.Complete
  | FileSummary.ErroredExport _
  | FileSummary.NoExport -> FileSummary.Partial

let source_origin_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* value_json = field "value" fields in
  let* tag = get_string tag_json in
  let* value = get_string value_json in
  match tag with
  | "path" -> (
      match Path.of_string value with
      | Ok path -> Ok (Source.Path path)
      | Error (Path.InvalidUtf8 { path }) -> Error (format
        Format.[ str "invalid utf-8 path "; str path ])
      | Error (Path.SystemInvalidUtf8 { syscall; path }) -> Error (format
        Format.[ str "invalid utf-8 path from "; str syscall; str ": "; str path ])
      | Error (Path.SystemError message) -> Error message
    )
  | "label" ->
      Ok (Source.Label value)
  | other ->
      Error (format Format.[ str "unknown module typings source origin tag "; str other ])

let value_definition_target_of_json = fun json ->
  let* fields = get_object json in
  let* tag_json = field "tag" fields in
  let* tag = get_string tag_json in
  match tag with
  | "site" ->
      let* origin_json = field "origin" fields in
      let* span_json = field "span" fields in
      let* origin = source_origin_of_json origin_json in
      let* span = span_of_json span_json in
      Ok (Site { origin; span })
  | "export" ->
      let* path_json = field "path" fields in
      let* path = get_string path_json in
      Ok (Export (SurfacePath.of_string path))
  | other ->
      Error (format Format.[ str "unknown module typings value definition target tag "; str other ])

let value_definition_of_json = fun json ->
  let* fields = get_object json in
  let* export_name_json = field "export_name" fields in
  let* target_json = field "target" fields in
  let* export_name = get_string export_name_json in
  let* target = value_definition_target_of_json target_json in
  Ok { export_name = SurfacePath.of_string export_name; target }

let value_definitions_of_json = fun value ->
  match value with
  | Data.Json.Array values ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | value :: rest ->
            let* definition = value_definition_of_json value in
            loop (definition :: acc) rest
      in
      loop [] values
  | other -> error_expected "array" other

let hash_of_hex = fun hex ->
  match Encoding.Hex.decode_bytes hex with
  | Ok bytes -> Ok (Crypto.Hash.of_bytes bytes)
  | Error `Invalid_base16 -> Error (format Format.[ str "invalid source_hash hex digest "; str hex ])

module Json = struct
  let to_json = fun summary ->
    Data.Json.Object [
      ("module_name", Data.Json.String summary.module_name);
      ("source_hash", Data.Json.String (Crypto.Digest.hex summary.source_hash));
      ("completeness", completeness_to_json summary.completeness);
      ("export_result", export_result_to_json summary.export_result);
      ("type_decls", type_decls_to_json summary.type_decls);
      ("value_definitions", value_definitions_to_json summary.value_definitions);
    ]

  let of_json = fun json ->
    let* fields = get_object json in
    let* module_name_json = field "module_name" fields in
    let* source_hash_json = field "source_hash" fields in
    let* export_result_json = field "export_result" fields in
    let* module_name = get_string module_name_json in
    let* source_hash_hex = get_string source_hash_json in
    let* source_hash = hash_of_hex source_hash_hex in
    let* export_result = export_result_of_json export_result_json in
    let completeness =
      match List.assoc_opt "completeness" fields with
      | Some completeness_json -> completeness_of_json completeness_json
      | None -> Ok (completeness_of_export_result export_result)
    in
    let type_decls =
      match List.assoc_opt "type_decls" fields with
      | Some type_decls_json -> type_decls_of_json type_decls_json
      | None -> Ok []
    in
    let value_definitions =
      match List.assoc_opt "value_definitions" fields with
      | Some value_definitions_json -> value_definitions_of_json value_definitions_json
      | None -> Ok []
    in
    let* completeness = completeness in
    let* type_decls = type_decls in
    let* value_definitions = value_definitions in
    Ok {
      module_name;
      source_hash;
      completeness;
      export_result;
      type_decls;
      value_definitions;
    }
end
