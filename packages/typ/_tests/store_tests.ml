open Std
open Typ
open Typ.Model

let with_store = fun f ->
  Fs.with_tempdir
    ~prefix:"typ-store-tests"
    (fun tmpdir ->
      let contentstore =
        Contentstore.create
          ~root:Path.(tmpdir / Path.v "cache")
          ~policy:Contentstore.Policy.default
          ()
      in
      let store = Store.create contentstore () in
      f contentstore store)
  |> Result.unwrap_or ~default:(Error "tempdir creation failed")

let sample_typings = fun () ->
  ModuleTypings.trusted
    ~module_name:"Std"
    ~source_hash:(Crypto.hash_string "std-typings")
    [ (SurfacePath.of_name "answer", TypeScheme.of_type TypeRepr.int); ]

let legacy_module_typings_name_namespace = "typ/v2/module-typings/by-name"

let legacy_package_typings_namespace = "typ/v2/module-typings/by-package"

let legacy_v3_module_typings_name_namespace = "typ/v3/module-typings/by-name"

let legacy_v3_package_typings_namespace = "typ/v3/module-typings/by-package"

let test_store_roundtrips_current_namespace = fun _ctx ->
  with_store
    (fun _contentstore store ->
      let typings = sample_typings () in
      let fingerprint = Crypto.hash_string "std-package" in
      match Store.save_module_typings store typings with
      | Error _ as err -> err
      | Ok () -> (
          match Store.save_package_bundle store ~package_name:"std" ~fingerprint [ typings ] with
          | Error _ as err -> err
          | Ok () -> (
              match (
                Store.load_module_typings store ~module_name:"Std",
                Store.load_package_bundle store ~package_name:"std"
              ) with
              | (Some loaded_module, Some loaded_package) ->
                  let loaded_exports =
                    ModuleTypings.exports loaded_module
                    |> List.map (fun (name, _scheme) ->
                      SurfacePath.to_string name)
                  in
                  let package_modules =
                    loaded_package.typings
                    |> List.map ModuleTypings.module_name
                  in
                  if not (List.equal String.equal loaded_exports [ "answer" ]) then
                    Error (format
                      Format.[
                        str "unexpected module exports: ";
                        str (String.concat ", " loaded_exports);
                      ])
                  else if not (List.equal String.equal package_modules [ "Std" ]) then
                    Error (format
                      Format.[
                        str "unexpected package modules: ";
                        str (String.concat ", " package_modules);
                      ])
                  else
                    Ok ()
              | (None, _) -> Error "expected current module typings bundle"
              | (_, None) -> Error "expected current package bundle"
            )
        ))

let test_store_ignores_legacy_v2_namespace = fun _ctx ->
  with_store
    (fun contentstore store ->
      let typings = sample_typings () in
      let module_json = ModuleTypings.Json.to_json typings in
      let package_json = Data.Json.Object [
        (
          "fingerprint",
          Data.Json.String (Crypto.Digest.hex (Crypto.hash_string "legacy-std-package"))
        );
        ("modules", Data.Json.Array [ module_json ]);
      ]
      in
      match Contentstore.Store.save_named_json_bundle
        contentstore
        ~namespace:legacy_module_typings_name_namespace
        ~key:"Std"
        ~json:module_json with
      | Error _ as err -> err
      | Ok () -> (
          match Contentstore.Store.save_named_json_bundle
            contentstore
            ~namespace:legacy_package_typings_namespace
            ~key:"std"
            ~json:package_json with
          | Error _ as err -> err
          | Ok () -> (
              match (
                Store.load_module_typings store ~module_name:"Std",
                Store.load_package_bundle store ~package_name:"std"
              ) with
              | (None, None) -> Ok ()
              | (Some _, _) -> Error "expected legacy v2 module typings to be ignored"
              | (_, Some _) -> Error "expected legacy v2 package bundle to be ignored"
            )
        ))

let test_store_ignores_legacy_v3_namespace = fun _ctx ->
  with_store
    (fun contentstore store ->
      let typings = sample_typings () in
      let module_json = ModuleTypings.Json.to_json typings in
      let package_json = Data.Json.Object [
        (
          "fingerprint",
          Data.Json.String (Crypto.Digest.hex (Crypto.hash_string "legacy-v3-std-package"))
        );
        ("modules", Data.Json.Array [ module_json ]);
      ]
      in
      match Contentstore.Store.save_named_json_bundle
        contentstore
        ~namespace:legacy_v3_module_typings_name_namespace
        ~key:"Std"
        ~json:module_json with
      | Error _ as err -> err
      | Ok () -> (
          match Contentstore.Store.save_named_json_bundle
            contentstore
            ~namespace:legacy_v3_package_typings_namespace
            ~key:"std"
            ~json:package_json with
          | Error _ as err -> err
          | Ok () -> (
              match (
                Store.load_module_typings store ~module_name:"Std",
                Store.load_package_bundle store ~package_name:"std"
              ) with
              | (None, None) -> Ok ()
              | (Some _, _) -> Error "expected legacy v3 module typings to be ignored"
              | (_, Some _) -> Error "expected legacy v3 package bundle to be ignored"
            )
        ))

let main ~args =
  let tests = [
    Test.case "store roundtrips current namespace" test_store_roundtrips_current_namespace;
    Test.case "store ignores legacy v2 namespace" test_store_ignores_legacy_v2_namespace;
    Test.case "store ignores legacy v3 namespace" test_store_ignores_legacy_v3_namespace;
  ]
  in
  Test.Cli.main ~name:"typ:store" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
