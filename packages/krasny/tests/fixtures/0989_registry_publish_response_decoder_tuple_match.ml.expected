let published_release_of_json = fun json ->
  match json with
  | Data.Json.Object fields -> (
      match
        optional_string_field ~context:"publish response" ~field:"package" fields,
        optional_string_field ~context:"publish response" ~field:"source_url" fields,
        optional_string_field ~context:"publish response" ~field:"package_subdir" fields,
        string_field ~context:"publish response" ~field:"selector" fields,
        string_field ~context:"publish response" ~field:"resolved_sha" fields,
        string_field ~context:"publish response" ~field:"package_name" fields,
        string_field ~context:"publish response" ~field:"package_version" fields,
        object_field ~context:"publish response" ~field:"manifest" fields,
        object_field ~context:"publish response" ~field:"source_archive" fields,
        object_field ~context:"publish response" ~field:"claim" fields,
        object_field ~context:"publish response" ~field:"release" fields,
        object_field ~context:"publish response" ~field:"materialization" fields
      with
      | Ok package_locator, Ok source_url, Ok package_subdir, Ok selector, Ok resolved_sha, Ok package_name, Ok package_version, Ok manifest_json, Ok source_archive_json, Ok claim_json, Ok release_json, Ok materialization_json -> (
          match
            published_artifact_location_of_json ~context:"publish response.manifest" manifest_json,
            published_artifact_location_of_json ~context:"publish response.source_archive" source_archive_json,
            published_record_of_json ~context:"publish response.claim" claim_json,
            published_record_of_json ~context:"publish response.release" release_json,
            published_materialization_of_json ~context:"publish response.materialization" materialization_json
          with
          | Ok manifest, Ok source_archive, Ok claim, Ok release, Ok materialization ->
              Ok {
                package_locator;
                source_url;
                package_subdir;
                selector;
                resolved_sha;
                package_name;
                package_version;
                manifest;
                source_archive;
                claim;
                release;
                materialization;
              }
          | Error err, _, _, _, _
          | _, Error err, _, _, _
          | _, _, Error err, _, _
          | _, _, _, Error err, _
          | _, _, _, _, Error err -> Error err
        )
      | Error err, _, _, _, _, _, _, _, _, _, _, _
      | _, Error err, _, _, _, _, _, _, _, _, _, _
      | _, _, Error err, _, _, _, _, _, _, _, _, _
      | _, _, _, Error err, _, _, _, _, _, _, _, _
      | _, _, _, _, Error err, _, _, _, _, _, _, _
      | _, _, _, _, _, Error err, _, _, _, _, _, _
      | _, _, _, _, _, _, Error err, _, _, _, _, _
      | _, _, _, _, _, _, _, Error err, _, _, _, _
      | _, _, _, _, _, _, _, _, Error err, _, _, _
      | _, _, _, _, _, _, _, _, _, Error err, _, _
      | _, _, _, _, _, _, _, _, _, _, Error err, _
      | _, _, _, _, _, _, _, _, _, _, _, Error err -> Error err
    )
  | _ -> Error "publish response must be an object"
