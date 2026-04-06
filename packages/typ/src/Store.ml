open Std

type t = {
  contentstore: Contentstore.t;
}

let module_typings_namespace = "typ/module-typings/by-hash"
let module_typings_name_namespace = "typ/module-typings/by-name"
let package_typings_namespace = "typ/module-typings/by-package"

let create = fun contentstore () -> { contentstore }

let load_module_typings_by_hash = fun store ~source_hash ->
  match Contentstore.Store.load_json_bundle
    store.contentstore
    ~namespace:module_typings_namespace
    ~hash:source_hash
  with
  | None -> None
  | Some json -> ModuleTypings.Json.of_json json |> Result.to_option

let load_module_typings = fun store ~module_name ->
  match Contentstore.Store.load_named_json_bundle
    store.contentstore
    ~namespace:module_typings_name_namespace
    ~key:module_name
  with
  | None -> None
  | Some json -> ModuleTypings.Json.of_json json |> Result.to_option

let load_package_module_typings = fun store ~package_name ->
  match Contentstore.Store.load_named_json_bundle
    store.contentstore
    ~namespace:package_typings_namespace
    ~key:package_name
  with
  | None -> None
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
  | Some _ -> None

let save_module_typings = fun store typings ->
  let json = ModuleTypings.Json.to_json typings in
  let module_name = ModuleTypings.module_name typings in
  let source_hash = ModuleTypings.source_hash typings in
  match
    Contentstore.Store.save_json_bundle
      store.contentstore
      ~namespace:module_typings_namespace
      ~hash:source_hash
      ~json
  with
  | Error _ as err -> err
  | Ok () ->
      Contentstore.Store.save_named_json_bundle
        store.contentstore
        ~namespace:module_typings_name_namespace
        ~key:module_name
        ~json

let save_package_module_typings = fun store ~package_name typings ->
  let json =
    Data.Json.Array (List.map ModuleTypings.Json.to_json typings)
  in
  Contentstore.Store.save_named_json_bundle
    store.contentstore
    ~namespace:package_typings_namespace
    ~key:package_name
    ~json
