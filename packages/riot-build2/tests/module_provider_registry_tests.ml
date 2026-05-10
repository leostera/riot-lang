open Std
open Std.Result.Syntax

module Test = Std.Test

open Riot_build2

let package = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let dependency_source =
  Riot_model.Package.{
    workspace = true;
    builtin = false;
    path = None;
    source_locator = None;
    ref_ = None;
    version = None;
  }

let package_manifest = fun ?(dependencies = []) name ->
  let name = package name in
  Riot_model.Package.make
    ~name
    ~path:Path.(Path.v "." / Path.v (Riot_model.Package_name.to_string name))
    ~relative_path:(Path.v (Riot_model.Package_name.to_string name))
    ~dependencies
    ()
  |> Riot_model.Package_manifest.from_package

let workspace =
  let dep_name = package "dep-lib" in
  Riot_model.Workspace.make
    ~root:(Path.v ".")
    ~target_dir:(Path.v "_build/riot-build2-module-provider-registry-tests")
    ~packages:[
      package_manifest "dep-lib";
      package_manifest
        ~dependencies:[ Riot_model.Package.{ name = dep_name; source = dependency_source } ]
        "app";
    ]
    ()

let build_package = fun name ->
  Goal.{
    package = package name;
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let expected_dep_build =
  Goal.{
    package = package "dep-lib";
    scope = Goal.Runtime;
    profile = Riot_model.Profile.debug;
    target = Riot_model.Target.current;
  }

let provider_registry = fun () ->
  let catalog = Package_catalog.create workspace in
  Module_provider_registry.create ~catalog ()

let test_declared_dependency_becomes_module_provider = fun _ctx ->
  let registry = provider_registry () in
  Module_provider_registry.providers_for_build registry (build_package "app")
  |> Result.map_err ~fn:Error.message
  |> Result.and_then
    ~fn:(fun providers ->
      match providers with
      | [ (provider: Module_provider_registry.provider) ] when Riot_model.Package_name.equal
        provider.package
        (package "dep-lib")
      && String.equal provider.root_module "Dep_lib"
      && provider.build = expected_dep_build
      && provider.key = Work_node.GoalKey (Goal.BuildPackage expected_dep_build) -> Ok ()
      | _ -> Error "expected declared package dependency to provide its root module")

let test_find_provider_by_root_module = fun _ctx ->
  let registry = provider_registry () in
  let* provider =
    Module_provider_registry.find_for_build registry (build_package "app") ~root_module:"Dep_lib"
    |> Result.map_err ~fn:Error.message
  in
  match provider with
  | Some (provider: Module_provider_registry.provider) when provider.key
  = Work_node.GoalKey (Goal.BuildPackage expected_dep_build) -> Ok ()
  | Some _ -> Error "expected provider lookup to return dependency build key"
  | None -> Error "expected provider lookup to find dependency root module"

let test_missing_provider_returns_none = fun _ctx ->
  let registry = provider_registry () in
  let* provider =
    Module_provider_registry.find_for_build registry (build_package "app") ~root_module:"Missing"
    |> Result.map_err ~fn:Error.message
  in
  match provider with
  | None -> Ok ()
  | Some _ -> Error "expected missing root module lookup to return none"

let tests =
  Test.[
    case
      "declared dependency becomes module provider"
      test_declared_dependency_becomes_module_provider;
    case "find provider by root module" test_find_provider_by_root_module;
    case "missing provider returns none" test_missing_provider_returns_none;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_module_provider_registry_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
