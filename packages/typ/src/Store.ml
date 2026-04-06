open Std

type t = {
  contentstore: Contentstore.t;
  module_typings_by_name: (string, ModuleTypings.t option) Collections.HashMap.t;
  module_typings_by_hash: (string, ModuleTypings.t option) Collections.HashMap.t;
  package_typings_by_name: (string, ModuleTypings.t list option) Collections.HashMap.t;
}

let module_typings_namespace = "typ/module-typings/by-hash"

let module_typings_name_namespace = "typ/module-typings/by-name"

let package_typings_namespace = "typ/module-typings/by-package"

let source_hash_key = fun source_hash -> Crypto.Digest.hex source_hash

let create = fun contentstore () ->
  {
    contentstore;
    module_typings_by_name = Collections.HashMap.with_capacity 128;
    module_typings_by_hash = Collections.HashMap.with_capacity 128;
    package_typings_by_name = Collections.HashMap.with_capacity 32
  }

let load_module_typings_by_hash = fun store ~source_hash ->
  let key = source_hash_key source_hash in
  match Collections.HashMap.get store.module_typings_by_hash key with
  | Some typings -> typings
  | None ->
      let typings =
        match Contentstore.Store.load_json_bundle
          store.contentstore
          ~namespace:module_typings_namespace
          ~hash:source_hash with
        | None -> None
        | Some json -> ModuleTypings.Json.of_json json |> Result.to_option
      in
      let _ = Collections.HashMap.insert store.module_typings_by_hash key typings in
      typings

let load_module_typings = fun store ~module_name ->
  match Collections.HashMap.get store.module_typings_by_name module_name with
  | Some typings -> typings
  | None ->
      let typings =
        match Contentstore.Store.load_named_json_bundle
          store.contentstore
          ~namespace:module_typings_name_namespace
          ~key:module_name with
        | None -> None
        | Some json -> ModuleTypings.Json.of_json json |> Result.to_option
      in
      let _ = Collections.HashMap.insert store.module_typings_by_name module_name typings in
      typings

let load_package_module_typings = fun store ~package_name ->
  match Collections.HashMap.get store.package_typings_by_name package_name with
  | Some typings -> typings
  | None ->
      let typings =
        match Contentstore.Store.load_named_json_bundle
          store.contentstore
          ~namespace:package_typings_namespace
          ~key:package_name with
        | None ->
            None
        | Some (Data.Json.Array jsons) ->
            let rec loop acc = function
              | [] -> Some (List.rev acc)
              | json :: rest -> (
                  match ModuleTypings.Json.of_json json with
                  | Ok typings -> loop (typings :: acc) rest
                  | Error _ -> None
                )
            in
            loop [] jsons
        | Some _ ->
            None
      in
      let _ = Collections.HashMap.insert store.package_typings_by_name package_name typings in
      typings

let save_module_typings = fun store typings ->
  let json = ModuleTypings.Json.to_json typings in
  let module_name = ModuleTypings.module_name typings in
  let source_hash = ModuleTypings.source_hash typings in
  let source_hash_key = source_hash_key source_hash in
  match Contentstore.Store.save_json_bundle
    store.contentstore
    ~namespace:module_typings_namespace
    ~hash:source_hash
    ~json with
  | Error _ as err -> err
  | Ok () ->
      match Contentstore.Store.save_named_json_bundle
        store.contentstore
        ~namespace:module_typings_name_namespace
        ~key:module_name
        ~json with
      | Error _ as err -> err
      | Ok () ->
          let _ = Collections.HashMap.insert store.module_typings_by_name module_name (Some typings) in
          let _ = Collections.HashMap.insert
            store.module_typings_by_hash
            source_hash_key
            (Some typings) in
          Ok ()

let save_package_module_typings = fun store ~package_name typings ->
  let json = Data.Json.Array (List.map ModuleTypings.Json.to_json typings) in
  match Contentstore.Store.save_named_json_bundle
    store.contentstore
    ~namespace:package_typings_namespace
    ~key:package_name
    ~json with
  | Error _ as err -> err
  | Ok () ->
      let _ = Collections.HashMap.insert store.package_typings_by_name package_name (Some typings) in
      let () =
        typings
        |> List.iter
          (fun typings ->
            let module_name = ModuleTypings.module_name typings in
            let source_hash = ModuleTypings.source_hash typings |> source_hash_key in
            let _ = Collections.HashMap.insert
              store.module_typings_by_name
              module_name
              (Some typings) in
            let _ = Collections.HashMap.insert
              store.module_typings_by_hash
              source_hash
              (Some typings) in
            ())
      in
      Ok ()
