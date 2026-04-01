open Std

type fetch_response = {
  status_code: int;
  body: string;
}

type fetch = {
  get: Net.Uri.t -> (fetch_response, string) result;
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
[
  `Materialized
  | `Already_present
]

type source =
  | Filesystem
  | In_memory of {
      config: Sparse_index.config option;
      packages: (string * Sparse_index.package_document) list;
      releases: ((string * string) * release_source) list
    }

type t = {
  cache: Registry_cache.t;
  fetch: fetch;
  source: source;
}

let make_fetch = fun ~get -> { get }

let tcp_stream_error_message = function
  | Net.TcpStream.Connection_refused -> "connection refused"
  | Net.TcpStream.Closed -> "connection closed"
  | Net.TcpStream.System_error io_err -> IO.error_message io_err

let blink_error_message = function
  | Blink.Error.Net_error Net.Connection_refused -> "connection refused"
  | Blink.Error.Net_error Net.Closed -> "connection closed"
  | Blink.Error.Net_error (Net.System_error io_err) -> IO.error_message io_err
  | Blink.Error.Tls_error Net.TlsStream.Closed -> "tls connection closed"
  | Blink.Error.Tls_error (Net.TlsStream.Handshake_failed msg) -> "tls handshake failed: " ^ msg
  | Blink.Error.Tls_error (Net.TlsStream.System_error io_err) -> IO.error_message io_err
  | Blink.Error.Tls_error (Net.TlsStream.Network_read_failed tcp_err) -> "tls read failed: " ^ tcp_stream_error_message tcp_err
  | Blink.Error.Tls_error (Net.TlsStream.Network_write_failed tcp_err) -> "tls write failed: " ^ tcp_stream_error_message tcp_err
  | Blink.Error.Tls_error Net.TlsStream.Tls_not_available -> "tls not available"
  | Blink.Error.Tls_error Net.TlsStream.Unsupported_vectored_operation -> "unsupported vectored tls operation"
  | Blink.Error.Parse_error msg -> "parse error: " ^ msg
  | Blink.Error.Protocol_error msg -> "protocol error: " ^ msg
  | Blink.Error.Handshake_failed msg -> "handshake failed: " ^ msg
  | Blink.Error.Invalid_frame -> "invalid frame"
  | Blink.Error.Eof -> "unexpected eof"
  | Blink.Error.Closed -> "connection closed"

let default_fetch =
  make_fetch ~get:(fun uri ->
    match Blink.connect uri with
    | Error err -> Error (blink_error_message err)
    | Ok conn ->
        let request = Net.Http.Request.create Net.Http.Method.Get uri in
        let finish result =
          Blink.close conn;
          result
        in
        match Blink.request conn request () with
        | Error err -> finish (Error (blink_error_message err))
        | Ok () -> (
            match Blink.await conn with
            | Error err -> finish (Error (blink_error_message err))
            | Ok (response, body) ->
                finish
                  (Ok {
                    status_code = Net.Http.Status.to_int (Net.Http.Response.status response);
                    body;
                  })
          ))

let create_filesystem = fun ?(fetch = default_fetch) ~registry_name ?tusk_home () ->
  match Registry_cache.create ?tusk_home ~registry_name () with
  | Error _ as err -> err
  | Ok cache -> Ok { cache; fetch; source = Filesystem }

let filesystem = fun ?(fetch = default_fetch) cache -> { cache; fetch; source = Filesystem }

let cache = fun registry -> registry.cache

let name = fun registry -> Registry_cache.registry_name registry.cache

let release_key = fun ~package_name ~version -> (Sparse_index.normalized_name package_name, version)

let in_memory = fun ?config ~cache ?(releases = []) ~packages () ->
  let packages =
    List.map
      (fun (document: Sparse_index.package_document) ->
        (Sparse_index.normalized_name document.name, document))
      packages
  in
  let releases =
    List.map
      (fun (release: release_source) ->
        (release_key ~package_name:release.package_name ~version:release.version, release))
      releases
  in
  { cache; fetch = default_fetch; source = In_memory { config; packages; releases } }

let http_status_message = fun status_code ->
  let status = Net.Http.Status.of_int status_code in
  Int.to_string status_code ^ " " ^ Net.Http.Status.reason_phrase status

let fetch_required = fun registry uri ->
  match registry.fetch.get uri with
  | Ok { status_code = 200; body } -> Ok body
  | Ok { status_code; _ } ->
      Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed with " ^ http_status_message status_code)
  | Error err ->
      Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let fetch_optional = fun registry uri ->
  match registry.fetch.get uri with
  | Ok { status_code = 200; body } -> Ok (Some body)
  | Ok { status_code = 404; body = _ } -> Ok None
  | Ok { status_code; _ } ->
      Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed with " ^ http_status_message status_code)
  | Error err ->
      Error ("request to '" ^ Net.Uri.to_string uri ^ "' failed: " ^ err)

let read_config = fun registry ->
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_config registry.cache with
      | Error _ as err -> err
      | Ok (Some _ as cached) -> Ok cached
      | Ok None -> (
          match Sparse_index.bootstrap_config_url ~registry_name:(name registry) with
          | Error _ as err -> err
          | Ok uri -> (
              match fetch_required registry uri with
              | Error _ as err -> err
              | Ok source -> (
                  match Sparse_index.config_of_string source with
                  | Error err -> Error ("failed to decode sparse index config from '" ^ Net.Uri.to_string uri ^ "': " ^ err)
                  | Ok config -> (
                      match Sparse_index.write_cached_config registry.cache ~source with
                      | Error _ as err -> err
                      | Ok () -> Ok (Some config)
                    )
                )
            )
        )
    )
  | In_memory { config; packages=_; releases=_ } -> Ok config

let read_package_document = fun registry ~package_name ->
  match registry.source with
  | Filesystem -> (
      match Sparse_index.read_cached_package_document registry.cache ~package_name with
      | Error _ as err -> err
      | Ok (Some _ as cached) -> Ok cached
      | Ok None -> (
          match read_config registry with
          | Error _ as err -> err
          | Ok None -> Error ("filesystem registry '" ^ name registry ^ "' is missing sparse index config")
          | Ok (Some config) -> (
              match Sparse_index.package_document_url config ~package_name with
              | Error _ as err -> err
              | Ok uri -> (
                  match fetch_optional registry uri with
                  | Error _ as err -> err
                  | Ok None -> Ok None
                  | Ok (Some source) -> (
                      match Sparse_index.package_document_of_string source with
                      | Error err ->
                          Error ("failed to decode sparse index package document from '" ^ Net.Uri.to_string uri ^ "': " ^ err)
                      | Ok document -> (
                          match Sparse_index.write_cached_package_document registry.cache ~package_name ~source with
                          | Error _ as err -> err
                          | Ok () -> Ok (Some document)
                        )
                    )
                )
            )
        )
    )
  | In_memory { packages; config=_; releases=_ } -> Ok (List.assoc_opt
    (Sparse_index.normalized_name package_name)
    packages)

let write_release_files = fun ~root (release: release_source) ->
  let manifest_path = Path.(root / Path.v "tusk.toml") in
  match Fs.create_dir_all root with
  | Error err -> Error ("failed to create package source directory '"
  ^ Path.to_string root
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      match Fs.write release.manifest_toml manifest_path with
      | Error err -> Error ("failed to write package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
      | Ok () ->
          let rec loop = function
            | [] -> Ok ()
            | (file: release_file) :: rest ->
                let file_path = Path.(root / file.path) in
                let ensure_parent =
                  match Path.parent file_path with
                  | Some parent -> Fs.create_dir_all parent
                  | None -> Ok ()
                in
                match ensure_parent with
                | Error err -> Error ("failed to create package source parent '"
                ^ Path.to_string file_path
                ^ "': "
                ^ IO.error_message err)
                | Ok () -> (
                    match Fs.write file.contents file_path with
                    | Error err -> Error ("failed to write package source file '"
                    ^ Path.to_string file_path
                    ^ "': "
                    ^ IO.error_message err)
                    | Ok () -> loop rest
                  )
          in
          loop release.files
    )

let extract_cached_archive = fun ~archive_path ~root ->
  match Fs.create_dir_all root with
  | Error err -> Error ("failed to create package source directory '"
  ^ Path.to_string root
  ^ "': "
  ^ IO.error_message err)
  | Ok () -> (
      let extract_cmd = Command.make
        ~args:[ "-xf"; Path.to_string archive_path; "-C"; Path.to_string root ]
        "tar" in
      match Command.output extract_cmd with
      | Error (Command.SystemError msg) ->
          let _ = Fs.remove_dir_all root in
          Error ("failed to extract cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ msg)
      | Ok output when output.Command.status != 0 ->
          let _ = Fs.remove_dir_all root in
          let detail =
            if String.equal output.stderr "" then
              output.stdout
            else
              output.stderr
          in
          Error ("failed to extract cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ detail)
      | Ok _ ->
          Ok ()
    )

let write_cached_archive = fun ~archive_path ~contents ->
  let archive_parent =
    match Path.parent archive_path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  match Fs.create_dir_all archive_parent with
  | Error err ->
      Error ("failed to create package archive directory '"
      ^ Path.to_string archive_parent
      ^ "': "
      ^ IO.error_message err)
  | Ok () -> (
      match Fs.write contents archive_path with
      | Ok () -> Ok ()
      | Error err ->
          Error ("failed to write cached package archive '"
          ^ Path.to_string archive_path
          ^ "': "
          ^ IO.error_message err)
    )

let fetch_release_archive = fun registry ~package_name ~version ~archive_path ->
  match read_package_document registry ~package_name with
  | Error _ as err -> err
  | Ok None ->
      Error ("package '" ^ package_name ^ "' was not found in registry '" ^ name registry ^ "'")
  | Ok (Some document) -> (
      match List.find_opt (fun (release: Sparse_index.release) -> String.equal release.version version) document.releases with
      | None ->
          Error ("package '" ^ package_name ^ "' does not have release '" ^ version ^ "' in registry '" ^ name registry ^ "'")
      | Some release -> (
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
                  | Ok archive ->
                      write_cached_archive ~archive_path ~contents:archive
                )
            )
        )
    )

let materialize_release = fun registry ~package_name ~version ->
  let root = Registry_cache.package_src_dir registry.cache ~package_name ~version in
  let manifest_path = Path.(root / Path.v "tusk.toml") in
  match Fs.exists manifest_path with
  | Error err ->
      Error ("failed to check package manifest '"
      ^ Path.to_string manifest_path
      ^ "': "
      ^ IO.error_message err)
  | Ok true ->
      Ok `Already_present
  | Ok false -> (
      match registry.source with
      | Filesystem -> (
          let archive_path = Registry_cache.archive_path registry.cache ~package_name ~version in
          match Fs.exists archive_path with
          | Error err ->
              Error ("failed to check cached package archive '"
              ^ Path.to_string archive_path
              ^ "': "
              ^ IO.error_message err)
          | Ok false ->
              (match fetch_release_archive registry ~package_name ~version ~archive_path with
              | Error _ as err -> err
              | Ok () -> (
                  match extract_cached_archive ~archive_path ~root with
                  | Ok () -> Ok `Materialized
                  | Error _ as err -> err
                ))
          | Ok true -> (
              match extract_cached_archive ~archive_path ~root with
              | Ok () -> Ok `Materialized
              | Error _ as err -> err
            )
        )
      | In_memory { releases; config=_; packages=_ } -> (
          match List.assoc_opt (release_key ~package_name ~version) releases with
          | None -> Error ("in-memory registry is missing source contents for '"
          ^ package_name
          ^ "@"
          ^ version
          ^ "'")
          | Some release ->
              match write_release_files ~root release with
              | Ok () -> Ok `Materialized
              | Error _ as err -> err
        )
    )
