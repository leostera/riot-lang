open Std

module Goal = Goal
module Intent = User_intent

let expand_targets = fun __tmp1 ->
  match __tmp1 with
  | Intent.HostTarget -> [ Riot_model.Target.current ]
  | Intent.AllTargets -> [ Riot_model.Target.current ]
  | Intent.ManyTargets targets ->
      if List.is_empty targets then
        [ Riot_model.Target.current ]
      else
        targets

let expand_package_targets = fun __tmp1 ->
  match __tmp1 with
  | Intent.AllPackages -> [ Goal.WorkspaceMembers ]
  | Intent.NamedPackages packages ->
      if List.is_empty packages then
        [ Goal.WorkspaceMembers ]
      else
        List.map packages ~fn:(fun package -> Goal.Package package)

let expand_profiles = fun __tmp1 ->
  match __tmp1 with
  | Intent.DefaultProfile -> [ Riot_model.Profile.debug ]
  | Intent.ManyProfiles profiles ->
      if List.is_empty profiles then
        [ Riot_model.Profile.debug ]
      else
        profiles

let expand_build = fun (build: Intent.build) ->
  let packages = expand_package_targets build.Intent.packages in
  let targets = expand_targets build.targets in
  let profiles = expand_profiles build.profiles in
  let rec loop_packages acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | package :: rest ->
      let acc =
        List.fold_left
          profiles
          ~init:acc
          ~fn:(fun acc profile ->
            List.fold_left
              targets
              ~init:acc
              ~fn:(fun acc target -> Goal.BuildPackage { package; profile; target } :: acc))
      in
      loop_packages acc rest
  in
  loop_packages [] packages

let expand_test = fun (test: Intent.test) ->
  let packages = expand_package_targets test.Intent.packages in
  let targets = expand_targets test.targets in
  let profiles = expand_profiles test.profiles in
  let rec loop_profiles acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> List.reverse acc
    | profile :: rest ->
      let acc =
        List.fold_left
          targets
          ~init:acc
          ~fn:(fun acc target ->
            Goal.RunTests {
              packages;
              filter = test.filter;
              profile;
              target;
            }
            :: acc)
      in
      loop_profiles acc rest
  in
  loop_profiles [] profiles

let expand_run = fun (run: Intent.run) ->
  let (package, binary) =
    match run.Intent.runnable with
    | Intent.ByName binary -> (None, Some binary)
    | Intent.Scoped { package; binary } -> (Some package, binary)
  in
  [
    Goal.RunBinary {
      package;
      binary;
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
