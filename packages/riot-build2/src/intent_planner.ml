open Std

module Goal = Goal
module Intent = User_intent

let normalize_targets = fun targets ->
  if List.is_empty targets then
    [ Riot_model.Target.current ]
  else
    targets

let package_targets = fun ~all_packages packages ->
  if all_packages || List.is_empty packages then
    [ Goal.WorkspaceMembers ]
  else
    List.map packages ~fn:(fun package -> Goal.Package package)

let expand_build = fun build ->
  let packages = package_targets ~all_packages:build.Intent.all_packages build.packages in
  let targets = normalize_targets build.targets in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | package :: rest ->
        let acc =
          List.fold_left
            targets
            ~init:acc
            ~fn:(fun acc target ->
              Goal.BuildPackage { package; profile = build.profile; target } :: acc)
        in
        loop acc rest
  in
  loop [] packages

let expand_test = fun test ->
  let packages = package_targets ~all_packages:false test.Intent.packages in
  let targets = normalize_targets test.targets in
  List.map
    targets
    ~fn:(fun target ->
      Goal.RunTests {
        packages;
        filter = test.filter;
        profile = test.profile;
        target;
      })

let expand_run = fun run ->
  [
    Goal.RunBinary {
      package = run.Intent.package;
      binary = run.binary;
      args = run.args;
      profile = run.profile;
      target = run.target;
    };
  ]

let expand = fun __tmp1 ->
  match __tmp1 with
  | Intent.Build build -> expand_build build
  | Intent.Test test -> expand_test test
  | Intent.Run run -> expand_run run
