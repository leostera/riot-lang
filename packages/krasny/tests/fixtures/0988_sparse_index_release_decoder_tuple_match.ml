let release_of_json = fun json ->
  match json with
  | Data.Json.Object fields -> (
      match int_field ~context:"release" ~field:"schema_version" fields with
      | Ok _
      | Error _ -> (
          match
            string_field ~context:"release" ~field:"version" fields,
            string_field ~context:"release" ~field:"published_at" fields,
            string_field ~context:"release" ~field:"canonical_locator" fields,
            string_field ~context:"release" ~field:"repo_url" fields,
            string_field ~context:"release" ~field:"subdir" fields,
            string_field ~context:"release" ~field:"sha" fields,
            optional_string_field ~field:"description" fields,
            optional_string_field ~field:"license" fields,
            optional_string_field ~field:"homepage" fields,
            optional_string_field ~field:"repository" fields,
            optional_string_field ~field:"root_module" fields,
            optional_string_list_field ~field:"categories" fields,
            optional_string_list_field ~field:"keywords" fields,
            string_field ~context:"release" ~field:"manifest_key" fields,
            string_field ~context:"release" ~field:"source_key" fields,
            object_field ~context:"release" ~field:"dependencies" fields
          with
          | Ok version, Ok published_at, Ok canonical_locator, Ok repo_url, Ok subdir, Ok sha, Ok description, Ok license, Ok homepage, Ok repository, Ok root_module, Ok categories, Ok keywords, Ok manifest_key, Ok source_key, Ok dependency_json -> (
              match dependencies_of_json dependency_json with
              | Ok dependencies ->
                  Ok {
                    version;
                    published_at;
                    canonical_locator;
                    repo_url;
                    subdir;
                    sha;
                    description;
                    license;
                    homepage;
                    repository;
                    root_module;
                    categories;
                    keywords;
                    manifest_key;
                    source_key;
                    dependencies;
                  }
              | Error _ as err -> err
            )
          | Error err, _, _, _, _, _, _, _, _, _, _, _, _, _, _, _
          | _, Error err, _, _, _, _, _, _, _, _, _, _, _, _, _, _
          | _, _, Error err, _, _, _, _, _, _, _, _, _, _, _, _, _
          | _, _, _, Error err, _, _, _, _, _, _, _, _, _, _, _, _
          | _, _, _, _, Error err, _, _, _, _, _, _, _, _, _, _, _
          | _, _, _, _, _, Error err, _, _, _, _, _, _, _, _, _, _
          | _, _, _, _, _, _, Error err, _, _, _, _, _, _, _, _, _
          | _, _, _, _, _, _, _, Error err, _, _, _, _, _, _, _, _
          | _, _, _, _, _, _, _, _, Error err, _, _, _, _, _, _, _
          | _, _, _, _, _, _, _, _, _, Error err, _, _, _, _, _, _
          | _, _, _, _, _, _, _, _, _, _, Error err, _, _, _, _, _
          | _, _, _, _, _, _, _, _, _, _, _, Error err, _, _, _, _
          | _, _, _, _, _, _, _, _, _, _, _, _, Error err, _, _, _
          | _, _, _, _, _, _, _, _, _, _, _, _, _, Error err, _, _
          | _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error err, _
          | _, _, _, _, _, _, _, _, _, _, _, _, _, _, _, Error err ->
              Error err
        )
    )
  | _ -> Error "release entries must be objects"
