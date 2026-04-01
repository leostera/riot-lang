open Std

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
  source: source;
}

let create_filesystem = fun ~registry_name ?tusk_home () ->
  match Registry_cache.create ?tusk_home ~registry_name () with
  | Error _ as err -> err
  | Ok cache -> Ok { cache; source = Filesystem }

let filesystem = fun cache -> { cache; source = Filesystem }

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
  { cache; source = In_memory { config; packages; releases } }

let read_config = fun registry ->
  match registry.source with
  | Filesystem -> Sparse_index.read_cached_config registry.cache
  | In_memory { config; packages=_; releases=_ } -> Ok config

let read_package_document = fun registry ~package_name ->
  match registry.source with
  | Filesystem -> Sparse_index.read_cached_package_document registry.cache ~package_name
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
              Error ("filesystem registry cannot materialize uncached package '"
              ^ package_name
              ^ "@"
              ^ version
              ^ "'")
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
