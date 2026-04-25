open Std
open Model

type package_bundle = { fingerprint: Crypto.hash; typings: ModuleTypings.t list }

type t = {
  contentstore: Contentstore.t;
  module_typings_by_name: (string, ModuleTypings.t option) Collections.HashMap.t;
  module_typings_by_hash: (string, ModuleTypings.t option) Collections.HashMap.t;
  package_typings_by_name: (string, package_bundle option) Collections.HashMap.t;
}

(* Bump when persisted ModuleTypings payloads or cache semantics change in a
   way that makes older bundles unsafe to hydrate. v4 invalidates v3 entries
   that were computed before CST lifting split multi-argument constructor
   applications correctly.
*)
let namespace_version = "v4"

let versioned_namespace = fun suffix -> format Format.[
  str "typ/";
  str namespace_version;
  str "/";
  str suffix;
]

let module_typings_namespace = versioned_namespace "module-typings/by-hash"

let module_typings_name_namespace = versioned_namespace "module-typings/by-name"

let package_typings_namespace = versioned_namespace "module-typings/by-package"

let source_hash_key = Crypto.Digest.hex

let hash_of_hex = fun hex ->
  let len = String.length hex in
  let bytes = IO.Bytes.create (len / 2) in
  let nibble = function
    | '0' .. '9' as ch -> Char.code ch - Char.code '0'
    | 'a' .. 'f' as ch -> 10 + Char.code ch - Char.code 'a'
    | 'A' .. 'F' as ch -> 10 + Char.code ch - Char.code 'A'
    | _ -> 0
  in
  let rec loop index =
    if index < len then
      (
        let hi = nibble hex.[index] in
        let lo = nibble hex.[index + 1] in
        IO.Bytes.set bytes (index / 2) (Char.chr ((hi lsl 4) lor lo));
        loop (index + 2)
      )
  in
  (loop 0);
  Crypto.Hash.of_bytes bytes

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
        match Contentstore.Store.load_json_bundle store.contentstore ~namespace:module_typings_namespace ~hash:source_hash with
        | None -> None
        | Some json -> ModuleTypings.Json.of_json json |> Result.to_option
      in
      let _ = Collections.HashMap.insert store.module_typings_by_hash key typings in typings

let load_module_typings = fun store ~module_name ->
  match Collections.HashMap.get store.module_typings_by_name module_name with
  | Some typings -> typings
  | None ->
      let typings =
        match Contentstore.Store.load_named_json_bundle store.contentstore ~namespace:module_typings_name_namespace ~key:module_name with
        | None -> None
        | Some json -> ModuleTypings.Json.of_json json |> Result.to_option
      in
      let _ = Collections.HashMap.insert store.module_typings_by_name module_name typings in typings

let rec load_package_module_typings = fun store ~package_name ->
  load_package_bundle store ~package_name |> Option.map
    (
      fun (bundle: package_bundle) -> bundle.typings
    )
and load_package_bundle = fun store ~package_name ->
  match Collections.HashMap.get store.package_typings_by_name package_name with
  | Some typings -> typings
  | None ->
      let bundle =
        match Contentstore.Store.load_named_json_bundle store.contentstore ~namespace:package_typings_namespace ~key:package_name with
        | None -> None
        | Some (Data.Json.Object fields) -> (
          match List.assoc_opt "fingerprint" fields, List.assoc_opt "modules" fields with
          | (Some (Data.Json.String fingerprint_hex), Some (Data.Json.Array jsons)) ->
              let rec loop acc = function
                | [] -> Some { fingerprint = hash_of_hex fingerprint_hex; typings = List.rev acc }
                | json :: rest -> (
                  match ModuleTypings.Json.of_json json with
                  | Ok typings -> loop (typings :: acc) rest
                  | Error _ -> None
                )
              in
              loop [] jsons
          | _ -> None
        )
        | Some (Data.Json.Array jsons) ->
            let rec loop acc = function
              | [] -> Some { fingerprint = Crypto.hash_string ""; typings = List.rev acc }
              | json :: rest -> (
                match ModuleTypings.Json.of_json json with
                | Ok typings -> loop (typings :: acc) rest
                | Error _ -> None
              )
            in
            loop [] jsons
        | Some _ -> None
      in
      let _ = Collections.HashMap.insert store.package_typings_by_name package_name bundle in bundle

let save_module_typings = fun store typings ->
  let json = ModuleTypings.Json.to_json typings in
  let module_name = ModuleTypings.module_name typings in
  let source_hash = ModuleTypings.source_hash typings in
  let source_hash_key = source_hash_key source_hash in
  match Contentstore.Store.save_json_bundle store.contentstore ~namespace:module_typings_namespace ~hash:source_hash ~json with
  | Error _ as err -> err
  | Ok () ->
      match Contentstore.Store.save_named_json_bundle store.contentstore ~namespace:module_typings_name_namespace ~key:module_name ~json with
      | Error _ as err -> err
      | Ok () ->
          let _ = Collections.HashMap.insert store.module_typings_by_name module_name (Some typings) in
          let _ = Collections.HashMap.insert store.module_typings_by_hash source_hash_key (Some typings) in Ok ()

let rec save_package_module_typings = fun store ~package_name typings -> save_package_bundle store ~package_name ~fingerprint:(Crypto.hash_string "") typings
and save_package_bundle = fun store ~package_name ~fingerprint typings ->
  let json =
    Data.Json.Object [
      "fingerprint", Data.Json.String (Crypto.Digest.hex fingerprint);
      "modules", Data.Json.Array (List.map ModuleTypings.Json.to_json typings);
    ]
  in
  match Contentstore.Store.save_named_json_bundle store.contentstore ~namespace:package_typings_namespace ~key:package_name ~json with
  | Error _ as err -> err
  | Ok () ->
      let bundle = Some { fingerprint; typings } in
      let _ = Collections.HashMap.insert store.package_typings_by_name package_name bundle in
      (
        typings |> List.iter
          (
            fun typings ->
              let module_name = ModuleTypings.module_name typings in
              let source_hash = ModuleTypings.source_hash typings |> source_hash_key in
              let _ = Collections.HashMap.insert store.module_typings_by_name module_name (Some typings) in
              let _ = Collections.HashMap.insert store.module_typings_by_hash source_hash (Some typings) in ()
          )
      );
      Ok ()
