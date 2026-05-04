open Std
open Std.Data

type required_by = {
  package: string;
  path: Path.t option;
}

type t =
  | ManifestReadFailed of {
      manifest_path: Path.t;
      error: string;
    }
  | ManifestParseFailed of {
      manifest_path: Path.t;
      error: string;
    }
  | PathDependencyLoadFailed of {
      dependency_name: string;
      dependency_path: Path.t;
      error: t;
    }
  | PathDependencyDecodeFailed of {
      dependency_name: string;
      manifest_path: Path.t;
      error: string;
    }
  | SourceDependencyLoadFailed of {
      dependency_name: string;
      source_locator: string;
      ref_: string option;
      error: string;
    }
  | SourceDependencyDecodeFailed of {
      dependency_name: string;
      manifest_path: Path.t;
      error: string;
    }
  | RegistryLatestReleaseMissing of { package: string; latest_version: string }
  | RegistryReleaseYanked of {
      package: string;
      registry: string;
      version: string;
      required_by: required_by option;
    }
  | PackageMetadataReadFailed of { package: string; registry: string; error: string }
  | PackageNotFound of {
      package: string;
      registry: string;
      required_by: required_by option;
    }
  | RegistryVersionNotFound of {
      package: string;
      registry: string;
      requirement: string;
      available_versions: string list;
      required_by: required_by option;
    }
  | LockfileReadFailed of {
      path: Path.t;
      error: string;
    }
  | LockRefreshCheckFailed of {
      workspace_root: Path.t;
      error: string;
    }
  | LockfileWriteFailed of {
      path: Path.t;
      error: string;
    }
  | MaterializationFailed of { error: string }
  | ProjectionFailed of { error: string }
  | Unexpected of { error: string }

let format_required_by = fun { package; path } ->
  match path with
  | Some path -> "required by package `" ^ package ^ "` (" ^ Path.to_string path ^ ")"
  | None -> "required by package `" ^ package ^ "`"

let rec headline = fun __tmp1 ->
  match __tmp1 with
  | ManifestReadFailed { manifest_path; error } ->
      "failed to read manifest '" ^ Path.to_string manifest_path ^ "': " ^ error
  | ManifestParseFailed { manifest_path; error } ->
      "failed to parse manifest '" ^ Path.to_string manifest_path ^ "': " ^ error
  | PathDependencyLoadFailed { dependency_name; dependency_path; error } ->
      "failed to load path dependency '"
      ^ dependency_name
      ^ "' from "
      ^ Path.to_string dependency_path
      ^ ": "
      ^ message error
  | PathDependencyDecodeFailed { dependency_name; manifest_path; error } ->
      "failed to decode path dependency '"
      ^ dependency_name
      ^ "' from "
      ^ Path.to_string manifest_path
      ^ ": "
      ^ error
  | SourceDependencyLoadFailed {
      dependency_name;
      source_locator;
      ref_;
      error;
    } ->
      let suffix =
        match ref_ with
        | Some ref_ -> "#" ^ ref_
        | None -> ""
      in
      "failed to load source dependency '"
      ^ dependency_name
      ^ "' from "
      ^ source_locator
      ^ suffix
      ^ ": "
      ^ error
  | SourceDependencyDecodeFailed { dependency_name; manifest_path; error } ->
      "failed to decode source dependency '"
      ^ dependency_name
      ^ "' from "
      ^ Path.to_string manifest_path
      ^ ": "
      ^ error
  | RegistryLatestReleaseMissing { package; latest_version } ->
      "registry package '"
      ^ package
      ^ "' declares latest version '"
      ^ latest_version
      ^ "' but that release is missing from the sparse index document"
  | RegistryReleaseYanked {
      package;
      registry;
      version;
      required_by = _;
    } ->
      "package `" ^ package ^ "@" ^ version ^ "` was yanked from registry `" ^ registry ^ "`"
  | PackageMetadataReadFailed { package; error; _ } ->
      "failed to read package document for '" ^ package ^ "': " ^ error
  | PackageNotFound { package; registry; required_by = _ } ->
      "package `" ^ package ^ "` was not found in registry `" ^ registry ^ "`"
  | RegistryVersionNotFound {
      package;
      registry;
      requirement;
      required_by = _;
      _;
    } ->
      "package `"
      ^ package
      ^ "` has no release matching `"
      ^ requirement
      ^ "` in registry `"
      ^ registry
      ^ "`"
  | LockfileReadFailed { path; error } ->
      "failed to read lockfile '" ^ Path.to_string path ^ "': " ^ error
  | LockRefreshCheckFailed { workspace_root; error } ->
      "failed to check lock freshness for workspace '"
      ^ Path.to_string workspace_root
      ^ "': "
      ^ error
  | LockfileWriteFailed { path; error } ->
      "failed to write lockfile '" ^ Path.to_string path ^ "': " ^ error
  | MaterializationFailed { error } -> error
  | ProjectionFailed { error } -> error
  | Unexpected { error } -> error

and detail_lines = fun __tmp1 ->
  match __tmp1 with
  | PackageNotFound { required_by = Some required_by; _ } -> [ format_required_by required_by ]
  | RegistryVersionNotFound { available_versions; required_by; _ } ->
      let version_line =
        match available_versions with
        | [] -> [ "available versions: none" ]
        | versions -> [ "available versions: " ^ String.concat ", " versions ]
      in
      (
        match required_by with
        | Some required_by -> version_line @ [ format_required_by required_by ]
        | None -> version_line
      )
  | RegistryReleaseYanked { required_by = Some required_by; _ } ->
      [ format_required_by required_by ]
  | _ -> []

and message error =
  match detail_lines error with
  | [] -> headline error
  | lines -> String.concat "\n" (headline error :: lines)

let json_of_path = fun path -> Json.String (Path.to_string path)

let path_of_json = fun __tmp1 ->
  match __tmp1 with
  | Json.String path -> (
      match Path.from_string path with
      | Ok path -> Ok path
      | Error (Path.InvalidUtf8 { path = invalid_path }) ->
          Error ("invalid path '" ^ path ^ "': invalid utf-8 in '" ^ invalid_path ^ "'")
      | Error (Path.SystemInvalidUtf8 { syscall; path = invalid_path }) ->
          Error ("invalid path '"
          ^ path
          ^ "': "
          ^ syscall
          ^ " returned invalid utf-8 for '"
          ^ invalid_path
          ^ "'")
      | Error (Path.SystemError err) -> Error ("invalid path '" ^ path ^ "': " ^ err)
    )
  | _ -> Error "expected path string"

let rec to_json = fun __tmp1 ->
  match __tmp1 with
  | ManifestReadFailed { manifest_path; error } ->
      Json.Object [
        ("kind", Json.String "ManifestReadFailed");
        ("manifest_path", json_of_path manifest_path);
        ("error", Json.String error);
      ]
  | ManifestParseFailed { manifest_path; error } ->
      Json.Object [
        ("kind", Json.String "ManifestParseFailed");
        ("manifest_path", json_of_path manifest_path);
        ("error", Json.String error);
      ]
  | PathDependencyLoadFailed { dependency_name; dependency_path; error } ->
      Json.Object [
        ("kind", Json.String "PathDependencyLoadFailed");
        ("dependency_name", Json.String dependency_name);
        ("dependency_path", json_of_path dependency_path);
        ("error", to_json error);
      ]
  | PathDependencyDecodeFailed { dependency_name; manifest_path; error } ->
      Json.Object [
        ("kind", Json.String "PathDependencyDecodeFailed");
        ("dependency_name", Json.String dependency_name);
        ("manifest_path", json_of_path manifest_path);
        ("error", Json.String error);
      ]
  | SourceDependencyLoadFailed {
      dependency_name;
      source_locator;
      ref_;
      error;
    } ->
      Json.Object [
        ("kind", Json.String "SourceDependencyLoadFailed");
        ("dependency_name", Json.String dependency_name);
        ("source_locator", Json.String source_locator);
        ("ref", match ref_ with
        | Some ref_ -> Json.String ref_
        | None -> Json.Null);
        ("error", Json.String error);
      ]
  | SourceDependencyDecodeFailed { dependency_name; manifest_path; error } ->
      Json.Object [
        ("kind", Json.String "SourceDependencyDecodeFailed");
        ("dependency_name", Json.String dependency_name);
        ("manifest_path", json_of_path manifest_path);
        ("error", Json.String error);
      ]
  | RegistryLatestReleaseMissing { package; latest_version } ->
      Json.Object [
        ("kind", Json.String "RegistryLatestReleaseMissing");
        ("package", Json.String package);
        ("latest_version", Json.String latest_version);
      ]
  | RegistryReleaseYanked {
      package;
      registry;
      version;
      required_by;
    } ->
      Json.Object [
        ("kind", Json.String "RegistryReleaseYanked");
        ("package", Json.String package);
        ("registry", Json.String registry);
        ("version", Json.String version);
        ("required_by", match required_by with
        | None -> Json.Null
        | Some { package; path } ->
            Json.Object [
              ("package", Json.String package);
              ("path", match path with
              | Some path -> json_of_path path
              | None -> Json.Null);
            ]);
      ]
  | PackageMetadataReadFailed { package; registry; error } ->
      Json.Object [
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
        ("required_by", match required_by with
        | None -> Json.Null
        | Some { package; path } ->
            Json.Object [
              ("package", Json.String package);
              ("path", match path with
              | Some path -> json_of_path path
              | None -> Json.Null);
            ]);
      ]
  | RegistryVersionNotFound {
      package;
      registry;
      requirement;
      available_versions;
      required_by;
    } ->
      Json.Object [
        ("kind", Json.String "RegistryVersionNotFound");
        ("package", Json.String package);
        ("registry", Json.String registry);
        ("requirement", Json.String requirement);
        (
          "available_versions",
          Json.Array (List.map available_versions ~fn:(fun version -> Json.String version))
        );
        ("required_by", match required_by with
        | None -> Json.Null
        | Some { package; path } ->
            Json.Object [
              ("package", Json.String package);
              ("path", match path with
              | Some path -> json_of_path path
              | None -> Json.Null);
            ]);
      ]
  | LockfileReadFailed { path; error } ->
      Json.Object [
        ("kind", Json.String "LockfileReadFailed");
        ("path", json_of_path path);
        ("error", Json.String error);
      ]
  | LockRefreshCheckFailed { workspace_root; error } ->
      Json.Object [
        ("kind", Json.String "LockRefreshCheckFailed");
        ("workspace_root", json_of_path workspace_root);
        ("error", Json.String error);
      ]
  | LockfileWriteFailed { path; error } ->
      Json.Object [
        ("kind", Json.String "LockfileWriteFailed");
        ("path", json_of_path path);
        ("error", Json.String error);
      ]
  | MaterializationFailed { error } ->
      Json.Object [ ("kind", Json.String "MaterializationFailed"); ("error", Json.String error); ]
  | ProjectionFailed { error } ->
      Json.Object [ ("kind", Json.String "ProjectionFailed"); ("error", Json.String error); ]
  | Unexpected { error } ->
      Json.Object [ ("kind", Json.String "Unexpected"); ("error", Json.String error); ]

let rec from_json = fun __tmp1 ->
  match __tmp1 with
  | Json.Object fields -> (
      match Fields.get "kind" fields with
      | Some (Json.String "ManifestReadFailed") -> (
          match (Fields.get "manifest_path" fields, Fields.get "error" fields) with
          | (Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map ~fn:(fun manifest_path -> ManifestReadFailed { manifest_path; error })
          | _ -> Error "invalid ManifestReadFailed"
        )
      | Some (Json.String "ManifestParseFailed") -> (
          match (Fields.get "manifest_path" fields, Fields.get "error" fields) with
          | (Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map ~fn:(fun manifest_path -> ManifestParseFailed { manifest_path; error })
          | _ -> Error "invalid ManifestParseFailed"
        )
      | Some (Json.String "PathDependencyLoadFailed") -> (
          match (
            Fields.get "dependency_name" fields,
            Fields.get "dependency_path" fields,
            Fields.get "error" fields
          ) with
          | (Some (Json.String dependency_name), Some path_json, Some error_json) -> (
              match (path_of_json path_json, from_json error_json) with
              | (Ok dependency_path, Ok error) ->
                  Ok (PathDependencyLoadFailed { dependency_name; dependency_path; error })
              | (Error err, _)
              | (_, Error err) -> Error err
            )
          | _ -> Error "invalid PathDependencyLoadFailed"
        )
      | Some (Json.String "PathDependencyDecodeFailed") -> (
          match (
            Fields.get "dependency_name" fields,
            Fields.get "manifest_path" fields,
            Fields.get "error" fields
          ) with
          | (Some (Json.String dependency_name), Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map
                ~fn:(fun manifest_path ->
                  PathDependencyDecodeFailed { dependency_name; manifest_path; error })
          | _ -> Error "invalid PathDependencyDecodeFailed"
        )
      | Some (Json.String "SourceDependencyLoadFailed") -> (
          match (
            Fields.get "dependency_name" fields,
            Fields.get "source_locator" fields,
            Fields.get "ref" fields,
            Fields.get "error" fields
          ) with
          | (
              Some (Json.String dependency_name),
              Some (Json.String source_locator),
              ref_json_opt,
              Some (Json.String error)
            ) ->
              let ref_ =
                match ref_json_opt with
                | Some (Json.String ref_) -> Ok (Some ref_)
                | Some Json.Null
                | None -> Ok None
                | Some _ -> Error "invalid SourceDependencyLoadFailed.ref"
              in
              ref_
              |> Result.map
                ~fn:(fun ref_ ->
                  SourceDependencyLoadFailed {
                    dependency_name;
                    source_locator;
                    ref_;
                    error;
                  })
          | _ -> Error "invalid SourceDependencyLoadFailed"
        )
      | Some (Json.String "SourceDependencyDecodeFailed") -> (
          match (
            Fields.get "dependency_name" fields,
            Fields.get "manifest_path" fields,
            Fields.get "error" fields
          ) with
          | (Some (Json.String dependency_name), Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map
                ~fn:(fun manifest_path ->
                  SourceDependencyDecodeFailed { dependency_name; manifest_path; error })
          | _ -> Error "invalid SourceDependencyDecodeFailed"
        )
      | Some (Json.String "RegistryLatestReleaseMissing") -> (
          match (Fields.get "package" fields, Fields.get "latest_version" fields) with
          | (Some (Json.String package), Some (Json.String latest_version)) ->
              Ok (RegistryLatestReleaseMissing { package; latest_version })
          | _ -> Error "invalid RegistryLatestReleaseMissing"
        )
      | Some (Json.String "RegistryReleaseYanked") -> (
          match (
            Fields.get "package" fields,
            Fields.get "registry" fields,
            Fields.get "version" fields,
            Fields.get "required_by" fields
          ) with
          | (
              Some (Json.String package),
              Some (Json.String registry),
              Some (Json.String version),
              required_by_json_opt
            ) ->
              let required_by =
                match required_by_json_opt with
                | Some Json.Null
                | None -> Ok None
                | Some (Json.Object required_by_fields) -> (
                    match (
                      Fields.get "package" required_by_fields,
                      Fields.get "path" required_by_fields
                    ) with
                    | (Some (Json.String package), Some path_json) -> (
                        match path_json with
                        | Json.Null -> Ok (Some { package; path = None })
                        | _ ->
                            path_of_json path_json
                            |> Result.map ~fn:(fun path -> Some { package; path = Some path })
                      )
                    | _ -> Error "invalid RegistryReleaseYanked.required_by"
                  )
                | Some _ -> Error "invalid RegistryReleaseYanked.required_by"
              in
              required_by
              |> Result.map
                ~fn:(fun required_by ->
                  RegistryReleaseYanked {
                    package;
                    registry;
                    version;
                    required_by;
                  })
          | _ -> Error "invalid RegistryReleaseYanked"
        )
      | Some (Json.String "PackageMetadataReadFailed") -> (
          match (
            Fields.get "package" fields,
            Fields.get "registry" fields,
            Fields.get "error" fields
          ) with
          | (Some (Json.String package), Some (Json.String registry), Some (Json.String error)) ->
              Ok (PackageMetadataReadFailed { package; registry; error })
          | _ -> Error "invalid PackageMetadataReadFailed"
        )
      | Some (Json.String "PackageNotFound") -> (
          match (Fields.get "package" fields, Fields.get "registry" fields) with
          | (Some (Json.String package), Some (Json.String registry)) ->
              let required_by =
                match Fields.get "required_by" fields with
                | Some Json.Null
                | None -> Ok None
                | Some (Json.Object required_by_fields) -> (
                    match Fields.get "package" required_by_fields with
                    | Some (Json.String package) ->
                        let path_result =
                          match Fields.get "path" required_by_fields with
                          | Some Json.Null
                          | None -> Ok None
                          | Some path_json ->
                              path_of_json path_json
                              |> Result.map ~fn:(fun path -> Some path)
                        in
                        path_result
                        |> Result.map ~fn:(fun path -> Some { package; path })
                    | _ -> Error "invalid PackageNotFound.required_by"
                  )
                | Some _ -> Error "invalid PackageNotFound.required_by"
              in
              required_by
              |> Result.map
                ~fn:(fun required_by -> PackageNotFound { package; registry; required_by })
          | _ -> Error "invalid PackageNotFound"
        )
      | Some (Json.String "RegistryVersionNotFound") -> (
          match (
            Fields.get "package" fields,
            Fields.get "registry" fields,
            Fields.get "requirement" fields,
            Fields.get "available_versions" fields,
            Fields.get "required_by" fields
          ) with
          | (
              Some (Json.String package),
              Some (Json.String registry),
              Some (Json.String requirement),
              Some (Json.Array available_versions),
              required_by_json_opt
            ) ->
              let available_versions =
                let rec loop acc = fun __tmp1 ->
                  match __tmp1 with
                  | [] -> Ok (List.reverse acc)
                  | (Json.String version) :: rest -> loop (version :: acc) rest
                  | _ -> Error "invalid RegistryVersionNotFound.available_versions"
                in
                loop [] available_versions
              in
              let required_by =
                match required_by_json_opt with
                | Some Json.Null
                | None -> Ok None
                | Some (Json.Object required_by_fields) -> (
                    match (
                      Fields.get "package" required_by_fields,
                      Fields.get "path" required_by_fields
                    ) with
                    | (Some (Json.String package), Some path_json) -> (
                        match path_json with
                        | Json.Null -> Ok (Some { package; path = None })
                        | _ ->
                            path_of_json path_json
                            |> Result.map ~fn:(fun path -> Some { package; path = Some path })
                      )
                    | _ -> Error "invalid RegistryVersionNotFound.required_by"
                  )
                | Some _ -> Error "invalid RegistryVersionNotFound.required_by"
              in
              (
                match (available_versions, required_by) with
                | (Ok available_versions, Ok required_by) ->
                    Ok (
                      RegistryVersionNotFound {
                        package;
                        registry;
                        requirement;
                        available_versions;
                        required_by;
                      }
                    )
                | (Error err, _)
                | (_, Error err) -> Error err
              )
          | _ -> Error "invalid RegistryVersionNotFound"
        )
      | Some (Json.String "LockfileReadFailed") -> (
          match (Fields.get "path" fields, Fields.get "error" fields) with
          | (Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map ~fn:(fun path -> LockfileReadFailed { path; error })
          | _ -> Error "invalid LockfileReadFailed"
        )
      | Some (Json.String "LockRefreshCheckFailed") -> (
          match (Fields.get "workspace_root" fields, Fields.get "error" fields) with
          | (Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map
                ~fn:(fun workspace_root -> LockRefreshCheckFailed { workspace_root; error })
          | _ -> Error "invalid LockRefreshCheckFailed"
        )
      | Some (Json.String "LockfileWriteFailed") -> (
          match (Fields.get "path" fields, Fields.get "error" fields) with
          | (Some path_json, Some (Json.String error)) ->
              path_of_json path_json
              |> Result.map ~fn:(fun path -> LockfileWriteFailed { path; error })
          | _ -> Error "invalid LockfileWriteFailed"
        )
      | Some (Json.String "MaterializationFailed") -> (
          match Fields.get "error" fields with
          | Some (Json.String error) -> Ok (MaterializationFailed { error })
          | _ -> Error "invalid MaterializationFailed"
        )
      | Some (Json.String "ProjectionFailed") -> (
          match Fields.get "error" fields with
          | Some (Json.String error) -> Ok (ProjectionFailed { error })
          | _ -> Error "invalid ProjectionFailed"
        )
      | Some (Json.String "Unexpected") -> (
          match Fields.get "error" fields with
          | Some (Json.String error) -> Ok (Unexpected { error })
          | _ -> Error "invalid Unexpected"
        )
      | Some (Json.String kind) -> Error ("unknown pm error kind '" ^ kind ^ "'")
      | _ -> Error "pm error is missing kind"
    )
  | _ -> Error "pm error must be a table"
