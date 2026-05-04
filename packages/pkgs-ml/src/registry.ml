open Std

let ( let* ) result fn = Result.and_then result ~fn

let assoc_value = fun entries ~key ->
  List.find entries ~fn:(fun (entry_key, _) -> entry_key = key)
  |> Option.map ~fn:(fun (_, value) -> value)

let protect = fun ~finally f ->
  match f () with
  | value ->
      finally ();
      value
  | exception error ->
      finally ();
      raise error

type fetch_response = { status_code: int; body: string }

type fetch = {
  get: Net.Uri.t -> (fetch_response, string) result;
  post:
    Net.Uri.t ->
    headers:(string * string) list ->
    body:string ->
    (fetch_response, string) result;
}

type published_artifact_location = { key: string; url: string }

type published_record = { key: string; created: bool }

type search_result = {
  package_name: string;
  latest_version: string;
  description: string option;
}

type published_materialization = { manifest: bool; source: bool }

type published_release = {
  artifact_sha256: string;
  package_name: string;
  package_version: string;
  manifest: published_artifact_location;
  source_archive: published_artifact_location;
  claim: published_record;
  release: published_record;
  materialization: published_materialization;
}

type yanked_release = {
  package_name: string;
  package_version: string;
  yanked: bool;
  yanked_at: string option;
  yanked_by_github_login: string option;
}

type release_file = {
  path: Path.t;
  contents: string;
}

type release_source = {
  package_name: string;
  version: string;
  manifest_toml: string;
  files: release_file list;
}

type materialize_result =
  | Materialized
  | Already_present

type source =
  | Filesystem
  | In_memory of {
      config: Sparse_index.config option;
      packages: (string * Sparse_index.package_document) list;
      releases: ((string * string) * release_source) list;
    }

type t = {
  cache: Registry_cache.t;
  fetch: fetch;
  source: source;
}

let io_error_message = fun error ->
  match error with
  | IO.Connection_refused -> "connection refused"
  | IO.Closed -> "connection closed"
  | _ -> IO.error_message error

let blink_error_message = fun err ->
  match err with
  | Blink.Error.NetError Net.Connection_refused -> "connection refused"
  | Blink.Error.NetError Net.Closed -> "connection closed"
  | Blink.Error.NetError (Net.System_error io_err) -> IO.error_message io_err
  | Blink.Error.TlsError Net.TlsStream.Closed -> "tls connection closed"
  | Blink.Error.TlsError (Net.TlsStream.Handshake_failed msg) -> "tls handshake failed: " ^ msg
  | Blink.Error.TlsError (Net.TlsStream.System_error io_err) -> IO.error_message io_err
  | Blink.Error.TlsError (Net.TlsStream.Network_read_failed tcp_err) ->
      "tls read failed: " ^ io_error_message tcp_err
  | Blink.Error.TlsError (Net.TlsStream.Network_write_failed tcp_err) ->
      "tls write failed: " ^ io_error_message tcp_err
  | Blink.Error.TlsError Net.TlsStream.Tls_not_available -> "tls not available"
  | Blink.Error.TlsError Net.TlsStream.Unsupported_vectored_operation -> "unsupported vectored tls operation"
  | Blink.Error.ParseError _
  | Blink.Error.WebSocketParseError _
  | Blink.Error.WebSocketSerializeError _ -> Blink.Error.to_string err
  | Blink.Error.ProtocolError msg -> "protocol error: " ^ msg
  | Blink.Error.HandshakeFailed msg -> "handshake failed: " ^ msg
  | Blink.Error.InvalidFrame -> "invalid frame"
  | Blink.Error.Eof -> "unexpected eof"
  | Blink.Error.Closed -> "connection closed"

let exn_message = fun __tmp1 ->
  match __tmp1 with
  | Failure message -> message
  | exn -> Exception.to_string exn

let configured_riot_agent = ref None

let normalize_riot_agent = fun __tmp1 ->
  match __tmp1 with
  | None -> None
  | Some value ->
      let trimmed = String.trim value in
      if String.equal trimmed "" then
        None
      else
        Some trimmed

let set_riot_agent = fun value -> configured_riot_agent := normalize_riot_agent value

let riot_agent_override = fun () ->
  Env.get Env.String ~var:"RIOT_AGENT_HEADER"
  |> normalize_riot_agent

let default_http_headers = fun headers ->
  let has_riot_agent =
    List.any
      headers
      ~fn:(fun (name, _value) -> String.equal (String.lowercase_ascii name) "x-riot-agent")
  in
  if has_riot_agent then
    headers
  else
    match riot_agent_override () with
    | Some value -> ("X-Riot-Agent", value) :: headers
    | None -> (
        match !configured_riot_agent with
        | Some value -> ("X-Riot-Agent", value) :: headers
        | None -> headers
      )

let make_fetch = fun ~get ?post () ->
  let post =
    match post with
    | Some post -> fun uri ~headers ~body -> post uri ~headers:(default_http_headers headers) ~body
    | None -> fun _ ~headers:_ ~body:_ -> Error "POST fetch is not configured"
  in
  { get; post }

let default_fetch =
  let run method_ uri ~headers ?body () =
    let headers = default_http_headers headers in
    try
      match Blink.connect uri with
      | Error err -> Error (blink_error_message err)
      | Ok conn ->
          let request =
            List.fold_left
              headers
              ~init:(Net.Http.Request.create method_ uri)
              ~fn:(fun request (name, value) ->
                Net.Http.Request.add_header request name value)
          in
          let finish result =
            try
              Blink.close conn;
              result
            with
            | exn -> (
                match result with
                | Error _ -> result
                | Ok _ -> Error (exn_message exn)
              )
          in
          let response =
            try
              match Blink.request conn request ?body () with
              | Error err -> Error (blink_error_message err)
              | Ok () -> (
                  match Blink.await conn with
                  | Error err -> Error (blink_error_message err)
                  | Ok (response, body) ->
                      Ok {
                        status_code = Net.Http.Status.to_int (Net.Http.Response.status response);
                        body;
                      }
                )
            with
            | exn -> Error (exn_message exn)
          in
          finish response
    with
    | exn -> Error (exn_message exn)
  in
  make_fetch
    ~get:(fun uri ->
      run Net.Http.Method.Get uri ~headers:[] ())
    ~post:(fun uri ~headers ~body ->
      run Net.Http.Method.Post uri ~headers ~body ())
    ()

let create_filesystem = fun ?(fetch = default_fetch) ~registry_name ?riot_home () ->
  match Registry_cache.create ?riot_home ~registry_name () with
  | Error _ as err -> err
  | Ok cache -> Ok { cache; fetch; source = Filesystem }

let filesystem = fun ?(fetch = default_fetch) cache -> { cache; fetch; source = Filesystem }

let cache = fun registry -> registry.cache

let name = fun registry -> Registry_cache.registry_name registry.cache

let release_key = fun ~package_name ~version -> (Sparse_index.normalized_name package_name, version)

let in_memory = fun ?config ~cache ?(releases = []) ~packages () ->
  let packages =
    List.map
      packages
      ~fn:(fun (document: Sparse_index.package_document) -> (
        Sparse_index.normalized_name document.name,
        document
      ))
  in
  let releases =
    List.map
      releases
      ~fn:(fun (release: release_source) -> (
        release_key ~package_name:release.package_name ~version:release.version,
        release
      ))
  in
  { cache; fetch = default_fetch; source = In_memory { config; packages; releases } }

let http_status_message = fun status_code ->
  let status = Net.Http.Status.from_int status_code in
  Int.to_string status_code ^ " " ^ Net.Http.Status.reason_phrase status

let protect_fetch = fun ~uri f ->
  try f () with
  | exn ->
      let _ = uri in
      Error (exn_message exn)

let fetch_required = fun registry uri ->
  match protect_fetch ~uri (fun () -> registry.fetch.get uri) with
  | Ok { status_code = 200; body } -> Ok body
  | Ok { status_code; _ } ->
      Error ("request to '"
      ^ Net.Uri.to_string uri
      ^ "' failed with "
      ^ http_status_message status_code)
  | Error err -> Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let fetch_optional = fun registry uri ->
  match protect_fetch ~uri (fun () -> registry.fetch.get uri) with
  | Ok { status_code = 200; body } -> Ok (Some body)
  | Ok { status_code = 404; body = _ } -> Ok None
  | Ok { status_code; _ } ->
      Error ("request to '"
      ^ Net.Uri.to_string uri
      ^ "' failed with "
      ^ http_status_message status_code)
  | Error err -> Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let post_required = fun registry uri ~headers ~body ->
  match protect_fetch ~uri (fun () -> registry.fetch.post uri ~headers ~body) with
  | Ok { status_code = 200; body } -> Ok body
  | Ok { status_code; body } -> (
      match Data.Json.from_string body with
      | Ok (Data.Json.Object fields) -> (
          match assoc_value fields ~key:"message" with
          | Some (Data.Json.String message) -> Error message
          | _ ->
              Error ("request to '"
              ^ Net.Uri.to_string uri
              ^ "' failed with "
              ^ http_status_message status_code)
        )
      | Ok _
      | Error _ ->
          Error ("request to '"
          ^ Net.Uri.to_string uri
          ^ "' failed with "
          ^ http_status_message status_code)
    )
  | Error err -> Error err

let sparse_index_cache_ttl_secs = 300.0

let cache_path_is_fresh = fun path ->
  match Fs.metadata path with
  | Error _ -> Ok false
  | Ok metadata ->
      let now =
        Time.SystemTime.now ()
        |> Time.SystemTime.secs_float
      in
      Ok ((now -. Fs.Metadata.modified metadata) <= sparse_index_cache_ttl_secs)

let rec take = fun n items ->
  if n <= 0 then
    []
  else
    match items with
    | [] -> []
    | item :: rest -> item :: take (n - 1) rest

let object_field = fun ~context ~field fields ->
  match assoc_value fields ~key:field with
  | Some value -> Ok value
  | None -> Error (context ^ " is missing required field '" ^ field ^ "'")

let string_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.String value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be a string")

let optional_string_field = fun ~context ~field fields ->
  match assoc_value fields ~key:field with
  | None -> Ok None
  | Some Data.Json.Null -> Ok None
  | Some (Data.Json.String value) -> Ok (Some value)
  | Some _ -> Error (context ^ "." ^ field ^ " must be a string")

let string_field_with_fallback = fun ~context ~field ~fallback fields ->
  match assoc_value fields ~key:field with
  | Some (Data.Json.String value) -> Ok value
  | Some _ -> Error (context ^ "." ^ field ^ " must be a string")
  | None -> (
      match assoc_value fields ~key:fallback with
      | Some (Data.Json.String value) -> Ok value
      | Some _ -> Error (context ^ "." ^ fallback ^ " must be a string")
      | None -> Error (context ^ " is missing required field '" ^ field ^ "'")
    )

let bool_field = fun ~context ~field fields ->
  match object_field ~context ~field fields with
  | Error _ as err -> err
  | Ok (Data.Json.Bool value) -> Ok value
  | Ok _ -> Error (context ^ "." ^ field ^ " must be a boolean")

let published_artifact_location_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields ->
      let* key = string_field ~context ~field:"key" fields in
      let* url = string_field_with_fallback ~context ~field:"url" ~fallback:"cdn_url" fields in
      Ok { key; url }
  | _ -> Error (context ^ " must be an object")

let published_record_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields -> (
      match (string_field ~context ~field:"key" fields, bool_field ~context ~field:"created" fields) with
      | (Ok key, Ok created) -> Ok { key; created }
      | (Error err, _)
      | (_, Error err) -> Error err
    )
  | _ -> Error (context ^ " must be an object")

let published_materialization_of_json = fun ~context json ->
  match json with
  | Data.Json.Object fields -> (
      match (
        bool_field ~context ~field:"manifest" fields,
        bool_field ~context ~field:"source" fields
      ) with
      | (Ok manifest, Ok source) -> Ok { manifest; source }
      | (Error err, _)
      | (_, Error err) -> Error err
    )
  | _ -> Error (context ^ " must be an object")

let published_release_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* artifact_sha256 =
        string_field ~context:"publish response" ~field:"artifact_sha256" fields
      in
      let* package_name = string_field ~context:"publish response" ~field:"package_name" fields in
      let* package_version =
        string_field ~context:"publish response" ~field:"package_version" fields
      in
      let* manifest_json = object_field ~context:"publish response" ~field:"manifest" fields in
      let* source_archive_json =
        object_field ~context:"publish response" ~field:"source_archive" fields
      in
      let* claim_json = object_field ~context:"publish response" ~field:"claim" fields in
      let* release_json = object_field ~context:"publish response" ~field:"release" fields in
      let* materialization_json =
        object_field ~context:"publish response" ~field:"materialization" fields
      in
      let* manifest =
        published_artifact_location_of_json ~context:"publish response.manifest" manifest_json
      in
      let* source_archive =
        published_artifact_location_of_json
          ~context:"publish response.source_archive"
          source_archive_json
      in
      let* claim = published_record_of_json ~context:"publish response.claim" claim_json in
      let* release = published_record_of_json ~context:"publish response.release" release_json in
      let* materialization =
        published_materialization_of_json
          ~context:"publish response.materialization"
          materialization_json
      in
      Ok {
        artifact_sha256;
        package_name;
        package_version;
        manifest;
        source_archive;
        claim;
        release;
        materialization;
      }
  | _ -> Error "publish response must be an object"

let yanked_release_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* package_name = string_field ~context:"yank response" ~field:"package_name" fields in
      let* package_version = string_field ~context:"yank response" ~field:"package_version" fields in
      let* yanked = bool_field ~context:"yank response" ~field:"yanked" fields in
      let* yanked_at = optional_string_field ~context:"yank response" ~field:"yanked_at" fields in
      let* yanked_by_github_login =
        optional_string_field ~context:"yank response" ~field:"yanked_by_github_login" fields
      in
      Ok {
        package_name;
        package_version;
        yanked;
        yanked_at;
        yanked_by_github_login;
      }
  | _ -> Error "yank response must be an object"

let publish_artifact_url = fun ~registry_name ->
  let url = "https://api." ^ registry_name ^ "/v1/publish" in
  match Net.Uri.from_string url with
  | Ok uri -> Ok uri
  | Error _ -> Error ("failed to build publish url '" ^ url ^ "'")

let yank_release_url = fun ~registry_name ~package_name ~version ->
  let url =
    "https://api."
    ^ registry_name
    ^ "/v1/me/packages/"
    ^ package_name
    ^ "/versions/"
    ^ version
    ^ "/yank"
  in
  match Net.Uri.from_string url with
  | Ok uri -> Ok uri
  | Error _ -> Error ("failed to build yank url '" ^ url ^ "'")

let search_url = fun ~registry_name ~query ~limit ->
  let url =
    "https://api."
    ^ registry_name
    ^ "/v1/search?q="
    ^ Net.Uri.form_encode query
    ^ "&limit="
    ^ Int.to_string limit
  in
  match Net.Uri.from_string url with
  | Ok uri -> Ok uri
  | Error _ -> Error ("failed to build search url '" ^ url ^ "'")

let search_result_of_json = fun json ->
  match json with
  | Data.Json.Object fields ->
      let* package_name = string_field ~context:"search result" ~field:"package_name" fields in
      let* latest_version = string_field ~context:"search result" ~field:"latest_version" fields in
      let* description = optional_string_field ~context:"search result" ~field:"description" fields in
      Ok { package_name; latest_version; description }
  | _ -> Error "search result must be an object"

let search_results_of_json = fun json ->
  match json with
  | Data.Json.Object fields -> (
      match object_field ~context:"search response" ~field:"results" fields with
      | Ok (Data.Json.Array values) ->
          let rec loop acc = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok (List.reverse acc)
            | value :: rest ->
                let* result = search_result_of_json value in
                loop (result :: acc) rest
          in
          loop [] values
      | Ok _ -> Error "search response.results must be an array"
      | Error _ as err -> err
    )
  | _ -> Error "search response must be an object"

let read_config = fun registry ->
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_config registry.cache with
      | Error _ as err -> err
      | Ok (Some _ as cached) -> (
          match cache_path_is_fresh (Sparse_index.config_cache_path registry.cache) with
          | Ok true -> Ok cached
          | Ok false
          | Error _ -> (
              match Sparse_index.bootstrap_config_url ~registry_name:(name registry) with
              | Error _ as err -> err
              | Ok uri -> (
                  match fetch_required registry uri with
                  | Error _ as err -> err
                  | Ok source -> (
                      match Sparse_index.config_of_string source with
                      | Error err ->
                          Error ("failed to decode sparse index config from '"
                          ^ Net.Uri.to_string uri
                          ^ "': "
                          ^ err)
                      | Ok config -> (
                          match Sparse_index.write_cached_config registry.cache ~source with
                          | Error _ as err -> err
                          | Ok () -> Ok (Some config)
                        )
                    )
                )
            )
        )
      | Ok None -> (
          match Sparse_index.bootstrap_config_url ~registry_name:(name registry) with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_required registry uri with
              | Error _ as err -> err
              | Ok source -> (
                  match Sparse_index.config_of_string source with
                  | Error err ->
                      Error ("failed to decode sparse index config from '"
                      ^ Net.Uri.to_string uri
                      ^ "': "
                      ^ err)
                  | Ok config -> (
                      match Sparse_index.write_cached_config registry.cache ~source with
                      | Error _ as err -> err
                      | Ok () -> Ok (Some config)
                    )
                )
            )
        )
    )
  | In_memory { config; packages = _; releases = _ } -> Ok config

let read_package_document = fun registry ~package_name ->
  let fetch_package_document config =
    match Sparse_index.package_document_url config ~package_name with
    | Error _ as err -> err
    | Ok uri -> (
        match fetch_optional registry uri with
        | Error _ as err -> err
        | Ok None -> Ok None
        | Ok (Some source) -> (
            match Sparse_index.package_document_of_string source with
            | Error err ->
                Error ("failed to decode sparse index package document from '"
                ^ Net.Uri.to_string uri
                ^ "': "
                ^ err)
            | Ok document -> (
                match Sparse_index.write_cached_package_document
                  registry.cache
                  ~package_name
                  ~source with
                | Error _ as err -> err
                | Ok () -> Ok (Some document)
              )
          )
      )
  in
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_package_document registry.cache ~package_name with
      | Error _ -> (
          match read_config registry with
          | Error _ as err -> err
          | Ok None ->
              Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
          | Ok (Some config) -> fetch_package_document config
        )
      | Ok (Some _ as cached) -> (
          match cache_path_is_fresh (Sparse_index.package_cache_path registry.cache ~package_name) with
          | Ok true -> Ok cached
          | Ok false
          | Error _ -> (
              match read_config registry with
              | Error _ as err -> err
              | Ok None ->
                  Error ("filesystem registry '"
                  ^ name registry
                  ^ "' is missing sparse index config")
              | Ok (Some config) -> fetch_package_document config
            )
        )
      | Ok None -> (
          match read_config registry with
          | Error _ as err -> err
          | Ok None ->
              Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
          | Ok (Some config) -> fetch_package_document config
        )
    )
  | In_memory { packages; config = _; releases = _ } ->
      Ok (assoc_value packages ~key:(Sparse_index.normalized_name package_name))

let search_packages = fun registry ~query ?(limit = 5) () ->
  if String.equal (String.trim query) "" then
    Ok []
  else
    match registry.source with
    | Filesystem -> (
        match search_url ~registry_name:(name registry) ~query ~limit with
        | Error _ as err -> err
        | Ok uri -> (
            match fetch_required registry uri with
            | Error _ as err -> err
            | Ok source -> (
                match Data.Json.from_string source with
                | Error err ->
                    Error ("failed to decode search response from '"
                    ^ Net.Uri.to_string uri
                    ^ "': "
                    ^ (Data.Json.error_to_string err))
                | Ok json -> (
                    match search_results_of_json json with
                    | Ok results -> Ok results
                    | Error err ->
                        Error ("failed to decode search response from '"
                        ^ Net.Uri.to_string uri
                        ^ "': "
                        ^ err)
                  )
              )
          )
      )
    | In_memory { packages; config = _; releases = _ } ->
        let normalized_query = Sparse_index.normalized_name query in
        packages
        |> List.map ~fn:(fun (_, document) -> document)
        |> List.filter
          ~fn:(fun (document: Sparse_index.package_document) ->
            let normalized_name = Sparse_index.normalized_name document.name in
            String.equal normalized_name normalized_query
            || String.starts_with ~prefix:normalized_query normalized_name
            || String.contains normalized_name normalized_query)
        |> List.sort
          ~compare:(fun
            (left: Sparse_index.package_document) (right: Sparse_index.package_document) ->
            String.compare
              left.name
              right.name)
        |> take limit
        |> List.map
          ~fn:(fun (document: Sparse_index.package_document) ->
            {
              package_name = document.name;
              latest_version = document.latest;
              description =
                match document.releases with
                | release :: _ -> release.description
                | [] ->
                    None;
            })
        |> fun results -> Ok results

let refresh_package_document = fun registry ~package_name ->
  match registry.source with
  | Filesystem -> (
      match read_config registry with
      | Error _ as err -> err
      | Ok None ->
          Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
      | Ok (Some config) ->
          match Sparse_index.package_document_url config ~package_name with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_optional registry uri with
              | Error _ as err -> err
              | Ok None -> Ok None
              | Ok (Some source) -> (
                  match Sparse_index.package_document_of_string source with
                  | Error err ->
                      Error ("failed to decode sparse index package document from '"
                      ^ Net.Uri.to_string uri
                      ^ "': "
                      ^ err)
                  | Ok document -> (
                      match Sparse_index.write_cached_package_document
                        registry.cache
                        ~package_name
                        ~source with
                      | Error _ as err -> err
                      | Ok () -> Ok (Some document)
                    )
                )
            )
    )
  | In_memory { packages; config = _; releases = _ } ->
      Ok (assoc_value packages ~key:(Sparse_index.normalized_name package_name))

let write_release_files = fun ~root (release: release_source) ->
  let manifest_path = Path.(root / Path.v "riot.toml") in
  match Fs.create_dir_all root with
  | Error err ->
      Error ("failed to create package source directory '"
      ^ Path.to_string root
      ^ "': "
      ^ IO.error_message err)
  | Ok () -> (
      match Fs.write release.manifest_toml manifest_path with
      | Error err ->
          Error ("failed to write package manifest '"
          ^ Path.to_string manifest_path
          ^ "': "
          ^ IO.error_message err)
      | Ok () ->
          let rec loop = fun __tmp1 ->
            match __tmp1 with
            | [] -> Ok ()
            | (file: release_file) :: rest ->
                let file_path = Path.(root / file.path) in
                let ensure_parent =
                  match Path.parent file_path with
                  | Some parent -> Fs.create_dir_all parent
                  | None -> Ok ()
                in
                match ensure_parent with
                | Error err ->
                    Error ("failed to create package source parent '"
                    ^ Path.to_string file_path
                    ^ "': "
                    ^ IO.error_message err)
                | Ok () -> (
                    match Fs.write file.contents file_path with
                    | Error err ->
                        Error ("failed to write package source file '"
                        ^ Path.to_string file_path
                        ^ "': "
                        ^ IO.error_message err)
                    | Ok () -> loop rest
                  )
          in
          loop release.files
    )

let tar_entry_kind_to_string = fun __tmp1 ->
  match __tmp1 with
  | Archive.Tar.File -> "file"
  | Archive.Tar.Directory -> "directory"
  | Archive.Tar.Symlink -> "symlink"
  | Archive.Tar.Hardlink -> "hardlink"
  | Archive.Tar.Other kind -> kind

let tar_error_message = Archive.Tar.error_to_string

let tar_extract_file_error_message = fun __tmp1 ->
  match __tmp1 with
  | Archive.Tar.Extract_source_error err -> IO.error_message err
  | Archive.Tar.Extract_fs_error err -> IO.error_message err
  | Archive.Tar.Extract_error err -> tar_error_message err

let gzip_tar_extract_error_message = fun __tmp1 ->
  match __tmp1 with
  | Archive.Tar.Extract_source_error err -> IO.error_message err
  | Archive.Tar.Extract_fs_error err -> IO.error_message err
  | Archive.Tar.Extract_error err -> tar_error_message err

let cached_archive_is_gzip = fun archive_path ->
  match Fs.File.open_read archive_path with
  | Error err ->
      Error ("failed to open cached package archive '"
      ^ Path.to_string archive_path
      ^ "': "
      ^ Fs.File.error_to_string err)
  | Ok file ->
      protect
        ~finally:(fun () ->
          let _ = Fs.File.close file in
          ())
        (fun () ->
          let magic = IO.Bytes.create ~size:2 in
          IO.Bytes.fill magic ~offset:0 ~len:2 ~char:'\000';
          match Fs.File.read file magic ~offset:0 ~len:2 with
          | Error err ->
              Error ("failed to read cached package archive '"
              ^ Path.to_string archive_path
              ^ "': "
              ^ Fs.File.error_to_string err)
          | Ok bytes_read ->
              Ok (bytes_read = 2
              && Char.equal (IO.Bytes.get_unchecked magic ~at:0) '\x1f'
              && Char.equal (IO.Bytes.get_unchecked magic ~at:1) '\x8b'))

let extract_cached_archive = fun ~archive_path ~root ->
  match Fs.create_dir_all root with
  | Error err ->
      Error ("failed to create package source directory '"
      ^ Path.to_string root
      ^ "': "
      ^ IO.error_message err)
  | Ok () -> (
      let fail detail =
        let _ = Fs.remove_dir_all root in
        Error ("failed to extract cached package archive '"
        ^ Path.to_string archive_path
        ^ "': "
        ^ detail)
      in
      match cached_archive_is_gzip archive_path with
      | Error _ as err ->
          let _ = Fs.remove_dir_all root in
          err
      | Ok true -> (
          match Fs.File.open_read archive_path with
          | Error err -> fail (Fs.File.error_to_string err)
          | Ok file ->
              protect
                ~finally:(fun () ->
                  let _ = Fs.File.close file in
                  ())
                (fun () ->
                  let reader =
                    Fs.File.to_reader file
                    |> Compress.Gzip.to_reader
                  in
                  match Archive.Tar.extract reader ~into:root with
                  | Ok () -> Ok ()
                  | Error err -> fail (gzip_tar_extract_error_message err))
        )
      | Ok false -> (
          match Archive.Tar.extract_file ~archive:archive_path ~into:root with
          | Ok () -> Ok ()
          | Error err -> fail (tar_extract_file_error_message err)
        )
    )

let read_dir_entries = fun dir ->
  match Fs.read_dir dir with
  | Error err ->
      Error ("failed to read directory '" ^ Path.to_string dir ^ "': " ^ IO.error_message err)
  | Ok iter -> Ok (Iter.MutIterator.to_list iter)

let move_directory_contents = fun ~src ~dst ->
  let* entries = read_dir_entries src in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok ()
    | entry :: rest ->
        let from_path = Path.(src / entry) in
        let to_path = Path.(dst / entry) in
        let* () =
          Fs.rename ~src:from_path ~dst:to_path
          |> Result.map_err
            ~fn:(fun err ->
              "failed to move extracted entry '"
              ^ Path.to_string from_path
              ^ "' into '"
              ^ Path.to_string dst
              ^ "': "
              ^ IO.error_message err)
        in
        loop rest
  in
  loop entries

let path_is_directory = fun path ->
  match Fs.metadata path with
  | Ok metadata -> Ok (Fs.Metadata.is_dir metadata)
  | Error err -> Error ("failed to stat '" ^ Path.to_string path ^ "': " ^ IO.error_message err)

let normalize_legacy_package_root = fun ~root ~(release:Sparse_index.release) ->
  let manifest_path = Path.(root / Path.v "riot.toml") in
  match Fs.exists manifest_path with
  | Error err ->
      Error ("failed to check package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
  | Ok true -> Ok ()
  | Ok false ->
      let* entries = read_dir_entries root in
      let top_level_dirs =
        List.filter_map
          entries
          ~fn:(fun entry ->
            let full_path = Path.(root / entry) in
            match path_is_directory full_path with
            | Ok true -> Some (Ok full_path)
            | Ok false -> None
            | Error err -> Some (Error err))
      in
      let* top_level_dirs =
        let rec collect acc = fun __tmp1 ->
          match __tmp1 with
          | [] -> Ok (List.reverse acc)
          | (Ok path) :: rest -> collect (path :: acc) rest
          | (Error _ as err) :: _ -> err
        in
        collect [] top_level_dirs
      in
      let candidate_root =
        match top_level_dirs with
        | [ top_level_dir ] when String.equal release.subdir "." || String.equal release.subdir "" ->
            Some top_level_dir
        | [ top_level_dir ] -> Some Path.(top_level_dir / Path.v release.subdir)
        | _ -> None
      in
      match candidate_root with
      | None ->
          Error ("materialized archive did not contain riot.toml at package root '"
          ^ Path.to_string root
          ^ "'")
      | Some candidate_root ->
          let candidate_manifest = Path.(candidate_root / Path.v "riot.toml") in
          match Fs.exists candidate_manifest with
          | Error err ->
              Error ("failed to check candidate manifest '"
              ^ Path.to_string candidate_manifest
              ^ "': "
              ^ IO.error_message err)
          | Ok false ->
              Error ("materialized archive did not contain riot.toml at package root '"
              ^ Path.to_string root
              ^ "'")
          | Ok true ->
              let top_level_dir =
                match top_level_dirs with
                | top_level_dir :: _ -> top_level_dir
                | [] -> root
              in
              let* () = move_directory_contents ~src:candidate_root ~dst:root in
              let* () =
                Fs.remove_dir_all top_level_dir
                |> Result.map_err
                  ~fn:(fun err ->
                    "failed to clean extracted archive root '"
                    ^ Path.to_string top_level_dir
                    ^ "': "
                    ^ IO.error_message err)
              in
              match Fs.exists manifest_path with
              | Ok true -> Ok ()
              | Ok false ->
                  Error ("normalized archive for '"
                  ^ release.canonical_locator
                  ^ "' is still missing riot.toml at '"
                  ^ Path.to_string manifest_path
                  ^ "'")
              | Error err ->
                  Error ("failed to check normalized manifest '"
                  ^ Path.to_string manifest_path
                  ^ "': "
                  ^ IO.error_message err)

let reset_materialized_root = fun root ->
  match Fs.exists root with
  | Error err ->
      Error ("failed to check package source directory '"
      ^ Path.to_string root
      ^ "': "
      ^ IO.error_message err)
  | Ok false -> Ok ()
  | Ok true ->
      Fs.remove_dir_all root
      |> Result.map_err
        ~fn:(fun err ->
          "failed to clean package source directory '"
          ^ Path.to_string root
          ^ "': "
          ^ IO.error_message err)

let find_release = fun registry ~package_name ~version ->
  match read_package_document registry ~package_name with
  | Error _ as err -> err
  | Ok None ->
      Error ("package '" ^ package_name ^ "' was not found in registry '" ^ name registry ^ "'")
  | Ok (Some document) -> (
      match List.find
        document.releases
        ~fn:(fun (release: Sparse_index.release) -> String.equal release.version version) with
      | Some release when release.yanked ->
          Error ("package '"
          ^ package_name
          ^ "@"
          ^ version
          ^ "' was yanked from registry '"
          ^ name registry
          ^ "'")
      | Some release -> Ok release
      | None ->
          Error ("package '"
          ^ package_name
          ^ "' does not have release '"
          ^ version
          ^ "' in registry '"
          ^ name registry
          ^ "'")
    )

let write_cached_archive = fun ~archive_path ~contents ->
  let archive_parent =
    match Path.parent archive_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  let temp_path = Path.(archive_parent / Path.v (Path.basename archive_path ^ ".tmp")) in
  match Fs.create_dir_all archive_parent with
  | Error err ->
      Error ("failed to create package archive directory '"
      ^ Path.to_string archive_parent
      ^ "': "
      ^ IO.error_message err)
  | Ok () -> (
      match Fs.write contents temp_path with
      | Error err ->
          Error ("failed to write cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ IO.error_message err)
      | Ok () -> (
          match Fs.rename ~src:temp_path ~dst:archive_path with
          | Ok () -> Ok ()
          | Error err ->
              Error ("failed to finalize cached package archive '"
              ^ Path.to_string archive_path
              ^ "': "
              ^ IO.error_message err)
        )
    )

let remove_cached_archive = fun archive_path ->
  match Fs.exists archive_path with
  | Error err ->
      Error ("failed to check cached package archive '"
      ^ Path.to_string archive_path
      ^ "': "
      ^ IO.error_message err)
  | Ok false -> Ok ()
  | Ok true ->
      Fs.remove_file archive_path
      |> Result.map_err
        ~fn:(fun err ->
          "failed to remove cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ IO.error_message err)

let fetch_release_archive = fun registry ~package_name ~version ~archive_path ->
  match find_release registry ~package_name ~version with
  | Error _ as err -> err
  | Ok release -> (
      match read_config registry with
      | Error _ as err -> err
      | Ok None ->
          Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
      | Ok (Some config) -> (
          match Sparse_index.release_source_url config release with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_required registry uri with
              | Error _ as err -> err
              | Ok archive -> write_cached_archive ~archive_path ~contents:archive
            )
        )
    )

let materialize_release = fun registry ~package_name ~version ->
  let root = Registry_cache.package_src_dir registry.cache ~package_name ~version in
  let manifest_path = Path.(root / Path.v "riot.toml") in
  let archive_path = Registry_cache.archive_path registry.cache ~package_name ~version in
  let finalize_extracted_root () =
    match Fs.exists manifest_path with
    | Error err ->
        Error ("failed to check package manifest '"
        ^ Path.to_string manifest_path
        ^ "': "
        ^ IO.error_message err)
    | Ok true -> Ok Materialized
    | Ok false ->
        let* release = find_release registry ~package_name ~version in
        normalize_legacy_package_root ~root ~release
        |> Result.map ~fn:(fun () -> Materialized)
  in
  let ensure_cached_archive () =
    match Fs.exists archive_path with
    | Error err ->
        Error ("failed to check cached package archive '"
        ^ Path.to_string archive_path
        ^ "': "
        ^ IO.error_message err)
    | Ok true -> Ok ()
    | Ok false -> fetch_release_archive registry ~package_name ~version ~archive_path
  in
  let extract_with_single_retry () =
    let rec attempt remaining_retries =
      let* () = ensure_cached_archive () in
      match extract_cached_archive ~archive_path ~root with
      | Ok () -> finalize_extracted_root ()
      | Error _ as err when remaining_retries <= 0 -> err
      | Error _ ->
          let* () = reset_materialized_root root in
          let* () = remove_cached_archive archive_path in
          let* () = fetch_release_archive registry ~package_name ~version ~archive_path in
          attempt (remaining_retries - 1)
    in
    attempt 1
  in
  match Fs.exists manifest_path with
  | Error err ->
      Error ("failed to check package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
  | Ok true -> Ok Already_present
  | Ok false -> (
      match registry.source with
      | Filesystem -> (
          match reset_materialized_root root with
          | Error _ as err -> err
          | Ok () -> extract_with_single_retry ()
        )
      | In_memory { releases; config = _; packages = _ } -> (
          match assoc_value releases ~key:(release_key ~package_name ~version) with
          | None ->
              Error ("in-memory registry is missing source contents for '"
              ^ package_name
              ^ "@"
              ^ version
              ^ "'")
          | Some release ->
              match write_release_files ~root release with
              | Ok () -> Ok Materialized
              | Error _ as err -> err
        )
    )

let publish_response = fun registry uri ~api_token ~artifact ->
  match post_required
    registry
    uri
    ~headers:[ ("authorization", "Bearer " ^ api_token); ("content-type", "application/gzip"); ]
    ~body:artifact with
  | Error _ as err -> err
  | Ok body -> (
      match Data.Json.from_string body with
      | Error err ->
          Error ("failed to parse publish response JSON: " ^ Data.Json.error_to_string err)
      | Ok json -> published_release_of_json json
    )

let publish_artifact = fun registry ~api_token ~artifact ->
  match publish_artifact_url ~registry_name:(name registry) with
  | Error _ as err -> err
  | Ok uri -> publish_response registry uri ~api_token ~artifact

let yank_response = fun registry uri ~api_token ->
  match post_required registry uri ~headers:[ ("authorization", "Bearer " ^ api_token); ] ~body:"" with
  | Error _ as err -> err
  | Ok body -> (
      match Data.Json.from_string body with
      | Error err -> Error ("failed to parse yank response JSON: " ^ Data.Json.error_to_string err)
      | Ok json -> yanked_release_of_json json
    )

let yank_release = fun registry ~api_token ~package_name ~version ->
  match yank_release_url ~registry_name:(name registry) ~package_name ~version with
  | Error _ as err -> err
  | Ok uri -> yank_response registry uri ~api_token

let publish_from_locator = fun registry ~locator:_ ~selector:_ ~api_token ~artifact ->
  publish_artifact
    registry
    ~api_token
    ~artifact
