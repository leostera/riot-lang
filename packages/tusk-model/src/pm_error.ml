open Std
open Std.Data

type required_by = {
  package: string;
  path: Path.t option;
}

type t =
  | ManifestReadFailed of { manifest_path: Path.t; error: string }
  | ManifestParseFailed of { manifest_path: Path.t; error: string }
  | PathDependencyLoadFailed of { dependency_name: string; dependency_path: Path.t; error: t }
  | PathDependencyDecodeFailed of { dependency_name: string; manifest_path: Path.t; error: string }
  | RegistryLatestReleaseMissing of { package: string; latest_version: string }
  | PackageMetadataReadFailed of { package: string; registry: string; error: string }
  | PackageNotFound of { package: string; registry: string; required_by: required_by option }
  | LockfileReadFailed of { path: Path.t; error: string }
  | LockRefreshCheckFailed of { workspace_root: Path.t; error: string }
  | LockfileWriteFailed of { path: Path.t; error: string }
  | MaterializationFailed of { error: string }
  | ProjectionFailed of { error: string }
  | Unexpected of { error: string }

let format_required_by = fun { package; path } ->
  match path with
  | Some path -> "required by package `" ^ package ^ "` (" ^ Path.to_string path ^ ")"
  | None -> "required by package `" ^ package ^ "`"

let rec headline = function
  | ManifestReadFailed { manifest_path; error } -> "failed to read manifest '"
  ^ Path.to_string manifest_path
  ^ "': "
  ^ error
  | ManifestParseFailed { manifest_path; error } -> "failed to parse manifest '"
  ^ Path.to_string manifest_path
  ^ "': "
  ^ error
  | PathDependencyLoadFailed { dependency_name; dependency_path; error } -> "failed to load path dependency '"
  ^ dependency_name
  ^ "' from "
  ^ Path.to_string dependency_path
  ^ ": "
  ^ message error
  | PathDependencyDecodeFailed { dependency_name; manifest_path; error } -> "failed to decode path dependency '"
  ^ dependency_name
  ^ "' from "
  ^ Path.to_string manifest_path
  ^ ": "
  ^ error
  | RegistryLatestReleaseMissing { package; latest_version } -> "registry package '"
  ^ package
  ^ "' declares latest version '"
  ^ latest_version
  ^ "' but that release is missing from the sparse index document"
  | PackageMetadataReadFailed { package; error; _ } -> "failed to read package document for '"
  ^ package
  ^ "': "
  ^ error
  | PackageNotFound { package; registry; required_by=_ } -> "package `"
  ^ package
  ^ "` was not found in registry `"
  ^ registry
  ^ "`"
  | LockfileReadFailed { path; error } -> "failed to read lockfile '"
  ^ Path.to_string path
  ^ "': "
  ^ error
  | LockRefreshCheckFailed { workspace_root; error } -> "failed to check lock freshness for workspace '"
  ^ Path.to_string workspace_root
  ^ "': "
  ^ error
  | LockfileWriteFailed { path; error } -> "failed to write lockfile '"
  ^ Path.to_string path
  ^ "': "
  ^ error
  | MaterializationFailed { error } -> error
  | ProjectionFailed { error } -> error
  | Unexpected { error } -> error

and detail_lines = function
  | PackageNotFound { required_by=Some required_by; _ } -> [ format_required_by required_by ]
  | _ -> []

and message error =
  match detail_lines error with
  | [] -> headline error
  | lines -> String.concat "\n" (headline error :: lines)

let json_of_path = fun path -> Json.String (Path.to_string path)

let path_of_json = function
  | Json.String path -> (
      match Path.of_string path with
      | Ok path -> Ok path
      | Error (Path.InvalidUtf8 { path=invalid_path }) -> Error ("invalid path '"
      ^ path
      ^ "': invalid utf-8 in '"
      ^ invalid_path
      ^ "'")
      | Error (Path.SystemInvalidUtf8 { syscall; path=invalid_path }) -> Error ("invalid path '"
      ^ path
      ^ "': "
      ^ syscall
      ^ " returned invalid utf-8 for '"
      ^ invalid_path
      ^ "'")
      | Error (Path.SystemError err) -> Error ("invalid path '" ^ path ^ "': " ^ err)
    )
  | _ -> Error "expected path string"

let rec to_json = function
  | ManifestReadFailed { manifest_path; error } -> Json.Object [
    ("kind", Json.String "ManifestReadFailed");
    ("manifest_path", json_of_path manifest_path);
    ("error", Json.String error);
  ]
  | ManifestParseFailed { manifest_path; error } -> Json.Object [
    ("kind", Json.String "ManifestParseFailed");
    ("manifest_path", json_of_path manifest_path);
    ("error", Json.String error);
  ]
  | PathDependencyLoadFailed { dependency_name; dependency_path; error } -> Json.Object [
    ("kind", Json.String "PathDependencyLoadFailed");
    ("dependency_name", Json.String dependency_name);
    ("dependency_path", json_of_path dependency_path);
    ("error", to_json error);
  ]
  | PathDependencyDecodeFailed { dependency_name; manifest_path; error } -> Json.Object [
    ("kind", Json.String "PathDependencyDecodeFailed");
    ("dependency_name", Json.String dependency_name);
    ("manifest_path", json_of_path manifest_path);
    ("error", Json.String error);
  ]
  | RegistryLatestReleaseMissing { package; latest_version } -> Json.Object [
    ("kind", Json.String "RegistryLatestReleaseMissing");
    ("package", Json.String package);
    ("latest_version", Json.String latest_version);
  ]
  | PackageMetadataReadFailed { package; registry; error } -> Json.Object [
    ("kind", Json.String "PackageMetadataReadFailed");
    ("package", Json.String package);
    ("registry", Json.String registry);
    ("error", Json.String error);
  ]
  | PackageNotFound { package; registry; required_by } ->
      Json.Object [
        ("kind", Json.String "PackageNotFound");
        ("package", Json.String package);
        ("registry", Json.String registry);
        (
          "required_by",
          (
            match required_by with
            | None -> Json.Null
            | Some { package; path } ->
                Json.Object [ ("package", Json.String package); (
                    "path",
                    (
                      match path with
                      | Some path -> json_of_path path
                      | None -> Json.Null
                    )
                  ); ]
          )
        );
      ]
  | LockfileReadFailed { path; error } -> Json.Object [
    ("kind", Json.String "LockfileReadFailed");
    ("path", json_of_path path);
    ("error", Json.String error);
  ]
  | LockRefreshCheckFailed { workspace_root; error } -> Json.Object [
    ("kind", Json.String "LockRefreshCheckFailed");
    ("workspace_root", json_of_path workspace_root);
    ("error", Json.String error);
  ]
  | LockfileWriteFailed { path; error } -> Json.Object [
    ("kind", Json.String "LockfileWriteFailed");
    ("path", json_of_path path);
    ("error", Json.String error);
  ]
  | MaterializationFailed { error } -> Json.Object [
    ("kind", Json.String "MaterializationFailed");
    ("error", Json.String error)
  ]
  | ProjectionFailed { error } -> Json.Object [
    ("kind", Json.String "ProjectionFailed");
    ("error", Json.String error)
  ]
  | Unexpected { error } -> Json.Object [
    ("kind", Json.String "Unexpected");
    ("error", Json.String error)
  ]

let rec of_json = function
  | Json.Object fields -> (
      match List.assoc_opt "kind" fields with
      | Some (Json.String "ManifestReadFailed") -> (
          match List.assoc_opt "manifest_path" fields, List.assoc_opt "error" fields with
          | Some path_json, Some (Json.String error) -> path_of_json path_json
          |> Result.map (fun manifest_path -> ManifestReadFailed { manifest_path; error })
          | _ -> Error "invalid ManifestReadFailed"
        )
      | Some (Json.String "ManifestParseFailed") -> (
          match List.assoc_opt "manifest_path" fields, List.assoc_opt "error" fields with
          | Some path_json, Some (Json.String error) -> path_of_json path_json
          |> Result.map (fun manifest_path -> ManifestParseFailed { manifest_path; error })
          | _ -> Error "invalid ManifestParseFailed"
        )
      | Some (Json.String "PathDependencyLoadFailed") -> (
          match List.assoc_opt "dependency_name" fields, List.assoc_opt "dependency_path" fields, List.assoc_opt
            "error"
            fields with
          | Some (Json.String dependency_name), Some path_json, Some error_json -> (
              match path_of_json path_json, of_json error_json with
              | Ok dependency_path, Ok error -> Ok (PathDependencyLoadFailed {
                dependency_name;
                dependency_path;
                error
              })
              | (Error err, _)
              | (_, Error err) -> Error err
            )
          | _ -> Error "invalid PathDependencyLoadFailed"
        )
      | Some (Json.String "PathDependencyDecodeFailed") -> (
          match List.assoc_opt "dependency_name" fields, List.assoc_opt "manifest_path" fields, List.assoc_opt
            "error"
            fields with
          | Some (Json.String dependency_name), Some path_json, Some (Json.String error) -> path_of_json
            path_json
          |> Result.map
            (fun manifest_path ->
              PathDependencyDecodeFailed { dependency_name; manifest_path; error })
          | _ -> Error "invalid PathDependencyDecodeFailed"
        )
      | Some (Json.String "RegistryLatestReleaseMissing") -> (
          match List.assoc_opt "package" fields, List.assoc_opt "latest_version" fields with
          | Some (Json.String package), Some (Json.String latest_version) -> Ok (RegistryLatestReleaseMissing {
            package;
            latest_version
          })
          | _ -> Error "invalid RegistryLatestReleaseMissing"
        )
      | Some (Json.String "PackageMetadataReadFailed") -> (
          match List.assoc_opt "package" fields, List.assoc_opt "registry" fields, List.assoc_opt
            "error"
            fields with
          | Some (Json.String package), Some (Json.String registry), Some (Json.String error) -> Ok (PackageMetadataReadFailed {
            package;
            registry;
            error
          })
          | _ -> Error "invalid PackageMetadataReadFailed"
        )
      | Some (Json.String "PackageNotFound") -> (
          match List.assoc_opt "package" fields, List.assoc_opt "registry" fields with
          | Some (Json.String package), Some (Json.String registry) ->
              let required_by =
                match List.assoc_opt "required_by" fields with
                | Some Json.Null
                | None ->
                    Ok None
                | Some (Json.Object required_by_fields) -> (
                    match List.assoc_opt "package" required_by_fields with
                    | Some (Json.String package) ->
                        let path_result =
                          match List.assoc_opt "path" required_by_fields with
                          | Some Json.Null
                          | None -> Ok None
                          | Some path_json -> path_of_json path_json
                          |> Result.map (fun path -> Some path)
                        in
                        path_result |> Result.map (fun path -> Some { package; path })
                    | _ -> Error "invalid PackageNotFound.required_by"
                  )
                | Some _ ->
                    Error "invalid PackageNotFound.required_by"
              in
              required_by
              |> Result.map (fun required_by -> PackageNotFound { package; registry; required_by })
          | _ -> Error "invalid PackageNotFound"
        )
      | Some (Json.String "LockfileReadFailed") -> (
          match List.assoc_opt "path" fields, List.assoc_opt "error" fields with
          | Some path_json, Some (Json.String error) -> path_of_json path_json
          |> Result.map (fun path -> LockfileReadFailed { path; error })
          | _ -> Error "invalid LockfileReadFailed"
        )
      | Some (Json.String "LockRefreshCheckFailed") -> (
          match List.assoc_opt "workspace_root" fields, List.assoc_opt "error" fields with
          | Some path_json, Some (Json.String error) -> path_of_json path_json
          |> Result.map (fun workspace_root -> LockRefreshCheckFailed { workspace_root; error })
          | _ -> Error "invalid LockRefreshCheckFailed"
        )
      | Some (Json.String "LockfileWriteFailed") -> (
          match List.assoc_opt "path" fields, List.assoc_opt "error" fields with
          | Some path_json, Some (Json.String error) -> path_of_json path_json
          |> Result.map (fun path -> LockfileWriteFailed { path; error })
          | _ -> Error "invalid LockfileWriteFailed"
        )
      | Some (Json.String "MaterializationFailed") -> (
          match List.assoc_opt "error" fields with
          | Some (Json.String error) -> Ok (MaterializationFailed { error })
          | _ -> Error "invalid MaterializationFailed"
        )
      | Some (Json.String "ProjectionFailed") -> (
          match List.assoc_opt "error" fields with
          | Some (Json.String error) -> Ok (ProjectionFailed { error })
          | _ -> Error "invalid ProjectionFailed"
        )
      | Some (Json.String "Unexpected") -> (
          match List.assoc_opt "error" fields with
          | Some (Json.String error) -> Ok (Unexpected { error })
          | _ -> Error "invalid Unexpected"
        )
      | Some (Json.String kind) ->
          Error ("unknown pm error kind '" ^ kind ^ "'")
      | _ ->
          Error "pm error is missing kind"
    )
  | _ -> Error "pm error must be a table"
