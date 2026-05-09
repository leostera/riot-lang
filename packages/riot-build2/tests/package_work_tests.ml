open Std

module Test = Std.Test

open Riot_build2

let test_scope_maps_to_realization_intent = fun _ctx ->
  let cases: (Package_work.scope * Riot_model.Package.realization_intent) list = [
    (Package_work.Build, Riot_model.Package.Build);
    (Package_work.Runtime, Riot_model.Package.Runtime);
    (Package_work.Dev, Riot_model.Package.Dev);
    (Package_work.Run, Riot_model.Package.Run);
    (Package_work.Test, Riot_model.Package.Test);
    (Package_work.Bench, Riot_model.Package.Bench);
    (Package_work.Doc, Riot_model.Package.Doc);
    (Package_work.Check, Riot_model.Package.Check);
  ]
  in
  if
    List.all cases ~fn:(fun (scope, expected) -> Package_work.realization_intent scope = expected)
  then
    Ok ()
  else
    Error "expected package work scope to map to the matching package realization intent"

let test_scope_maps_to_dependency_scope = fun _ctx ->
  let cases: (Package_work.scope * Riot_model.Package.dependency_scope) list = [
    (Package_work.Build, Riot_model.Package.Build);
    (Package_work.Runtime, Riot_model.Package.Normal);
    (Package_work.Dev, Riot_model.Package.Dev);
    (Package_work.Run, Riot_model.Package.Dev);
    (Package_work.Test, Riot_model.Package.Dev);
    (Package_work.Bench, Riot_model.Package.Dev);
    (Package_work.Doc, Riot_model.Package.Dev);
    (Package_work.Check, Riot_model.Package.Dev);
  ]
  in
  if
    List.all cases ~fn:(fun (scope, expected) -> Package_work.dependency_scope scope = expected)
  then
    Ok ()
  else
    Error "expected package work scope to map to the right package dependency scope"

let tests =
  Test.[
    case "scope maps to realization intent" test_scope_maps_to_realization_intent;
    case "scope maps to dependency scope" test_scope_maps_to_dependency_scope;
  ]

let main ~args = Test.Cli.main ~name:"riot_build2_package_work_tests" ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
