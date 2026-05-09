open Std

let package_targets = fun catalog __tmp1 ->
  match __tmp1 with
  | Goal.WorkspaceMembers ->
      Package_catalog.manifests catalog
      |> List.map ~fn:(fun (package: Riot_model.Package_manifest.t) -> package.name)
      |> Result.ok
  | Goal.Package package_name ->
      Package_catalog.require_manifest catalog package_name
      |> Result.map ~fn:(fun (_package: Riot_model.Package_manifest.t) -> [ package_name ])

let expand = fun catalog __tmp1 ->
  match __tmp1 with
  | Goal.BuildPackage build ->
      package_targets catalog build.package
      |> Result.map
        ~fn:(fun packages ->
          List.map
            packages
            ~fn:(fun package ->
              Package_work.BuildLibrary {
                package;
                scope = Package_work.Runtime;
                profile = build.profile;
                target = build.target;
              }))
  | Goal.RunTests test ->
      package_targets catalog test.package
      |> Result.map
        ~fn:(fun packages ->
          List.map
            packages
            ~fn:(fun package ->
              Package_work.TestPackage {
                package;
                scope = Package_work.Test;
                filter = test.filter;
                profile = test.profile;
                target = test.target;
              }))
  | Goal.RunBinary run ->
      let package_work package binary =
        Package_catalog.require_manifest catalog package
        |> Result.map
          ~fn:(fun (_package: Riot_model.Package_manifest.t) ->
            [
              Package_work.RunBinary {
                package;
                scope = Package_work.Run;
                binary;
                args = run.args;
                profile = run.profile;
                target = run.target;
              };
            ])
      in
      match run.binary with
      | Goal.BinaryByName _ ->
          Error (Error.UnsupportedGoal { goal = Goal.RunBinary run })
      | Goal.DefaultBinaryInPackage package -> package_work package None
      | Goal.BinaryInPackage (package, binary) -> package_work package (Some binary)
