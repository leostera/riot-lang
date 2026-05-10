open Std
open Std.Result.Syntax

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

let workspace_package_names = fun catalog ->
  Package_catalog.manifests catalog
  |> List.map ~fn:(fun (package: Riot_model.Package_manifest.t) -> package.name)

let expand_package_names = fun catalog __tmp1 ->
  match __tmp1 with
  | Intent.WorkspaceMembers -> Ok (workspace_package_names catalog)
  | Intent.NamedPackages packages ->
      if List.is_empty packages then
        Ok (workspace_package_names catalog)
      else
        Ok packages

let expand_profiles = fun __tmp1 ->
  match __tmp1 with
  | Intent.DefaultProfile -> [ Riot_model.Profile.debug ]
  | Intent.ManyProfiles profiles ->
      if List.is_empty profiles then
        [ Riot_model.Profile.debug ]
      else
        profiles

let expand_build = fun catalog (build: Intent.build) ->
  let* packages = expand_package_names catalog build.Intent.packages in
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
                ~fn:(fun acc target ->
                  Goal.BuildPackage {
                    package;
                    scope = build.scope;
                    profile;
                    target;
                  } :: acc))
        in
        loop_packages acc rest
  in
  Ok (loop_packages [] packages)

let expand_test = fun catalog (test: Intent.test) ->
  let* packages = expand_package_names catalog test.Intent.packages in
  let targets = expand_targets test.targets in
  let profiles = expand_profiles test.profiles in
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
                ~fn:(fun acc target ->
                  Goal.RunTests {
                    package;
                    filter = test.filter;
                    profile;
                    target;
                  } :: acc))
        in
        loop_packages acc rest
  in
  Ok (loop_packages [] packages)

let expand_run = fun (run: Intent.run) ->
  let binary =
    match run.Intent.runnable with
    | Intent.ByName binary -> Goal.BinaryByName binary
    | Intent.Scoped { package; binary = None } -> Goal.DefaultBinaryInPackage package
    | Intent.Scoped { package; binary = Some binary } -> Goal.BinaryInPackage (package, binary)
  in
  [
    Goal.RunBinary {
      binary;
      args = run.args;
      profile = run.profile;
      target = run.target;
    };
  ]

let expand = fun catalog __tmp1 ->
  match __tmp1 with
  | Intent.Build build -> expand_build catalog build
  | Intent.Test test -> expand_test catalog test
  | Intent.Run run -> Ok (expand_run run)
