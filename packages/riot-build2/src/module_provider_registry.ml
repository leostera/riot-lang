open Std
open Std.Result.Syntax

module ConcurrentHashMap = Collections.ConcurrentHashMap

type provider = {
  package: Riot_model.Package_name.t;
  root_module: string;
  build: Goal.build_package;
  key: Work_node.key;
}

type t = {
  catalog: Package_catalog.t;
  providers_by_build: (Goal.build_package, provider list) ConcurrentHashMap.t;
}

let create = fun ~catalog () -> {
  catalog;
  providers_by_build = ConcurrentHashMap.with_capacity ~size:128;
}

let provider_for_dependency = fun t (build: Goal.build_package) package ->
  let provider_build = { build with package } in
  let* realized =
    Package_catalog.realize t.catalog ~intent:(Goal.realization_intent build.scope) package
  in
  Ok {
    package;
    root_module = Riot_model.Package.root_module_name realized;
    build = provider_build;
    key = Work_node.GoalKey (Goal.BuildPackage provider_build);
  }

let compute_providers = fun t (build: Goal.build_package) ->
  let* dependencies =
    Package_catalog.dependency_names_for_scope
      t.catalog
      ~scope:(Goal.dependency_scope build.scope)
      build.package
  in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | package :: rest ->
        let* provider = provider_for_dependency t build package in
        loop (provider :: acc) rest
  in
  loop [] dependencies

let providers_for_build = fun t (build: Goal.build_package) ->
  match ConcurrentHashMap.get t.providers_by_build ~key:build with
  | Some providers -> Ok providers
  | None ->
      let* providers = compute_providers t build in
      ignore (ConcurrentHashMap.insert t.providers_by_build ~key:build ~value:providers);
      Ok providers

let find_for_build = fun t (build: Goal.build_package) ~root_module ->
  let* providers = providers_for_build t build in
  Ok (List.find providers ~fn:(fun provider -> String.equal provider.root_module root_module))

let dependency_keys_for_build = fun t (build: Goal.build_package) ->
  providers_for_build t build
  |> Result.map ~fn:(List.map ~fn:(fun provider -> provider.key))
