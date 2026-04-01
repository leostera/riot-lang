open Std

type source =
  | Filesystem of Registry_cache.t
  | In_memory of {
      config: Sparse_index.config option;
      packages: (string * Sparse_index.package_document) list;
    }

type t = {
  source: source;
}

let filesystem = fun cache -> { source = Filesystem cache }

let in_memory = fun ?config ~packages () ->
  let packages =
    List.map
      (fun (document: Sparse_index.package_document) ->
        (Sparse_index.normalized_name document.name, document))
      packages
  in
  {
    source = In_memory { config; packages };
  }

let read_config = fun registry ->
  match registry.source with
  | Filesystem cache -> Sparse_index.read_cached_config cache
  | In_memory { config; _ } -> Ok config

let read_package_document = fun registry ~package_name ->
  match registry.source with
  | Filesystem cache ->
      Sparse_index.read_cached_package_document cache ~package_name
  | In_memory { packages; _ } ->
      Ok (List.assoc_opt (Sparse_index.normalized_name package_name) packages)
