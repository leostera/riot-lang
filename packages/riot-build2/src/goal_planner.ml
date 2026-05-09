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
              Package_work.BuildLibrary { package; profile = build.profile; target = build.target }))
  | Goal.RunTests test ->
      let rec loop acc = fun __tmp1 ->
        match __tmp1 with
        | [] -> Ok (List.reverse acc)
        | Goal.WorkspaceMembers :: rest ->
            let packages =
              Package_catalog.manifests catalog
              |> List.map ~fn:(fun (package: Riot_model.Package_manifest.t) -> package.name)
            in
            loop
              (
                List.map
                  packages
                  ~fn:(fun package ->
                    Package_work.TestPackage {
                      package;
                      filter = test.filter;
                      profile = test.profile;
                      target = test.target;
                    })
                @ acc
              )
              rest
        | Goal.Package package :: rest ->
            Package_catalog.require_manifest catalog package
            |> Result.and_then
              ~fn:(fun (_package: Riot_model.Package_manifest.t) ->
                loop
                  (
                    Package_work.TestPackage {
                      package;
                      filter = test.filter;
                      profile = test.profile;
                      target = test.target;
                    }
                    :: acc
                  )
                  rest)
      in
      loop [] test.packages
  | Goal.RunBinary run ->
      match run.package with
      | Some package ->
          Package_catalog.require_manifest catalog package
          |> Result.map
            ~fn:(fun (_package: Riot_model.Package_manifest.t) ->
              [
                Package_work.RunBinary {
                  package;
                  binary = run.binary;
                  args = run.args;
                  profile = run.profile;
                  target = run.target;
                };
              ])
      | None -> Error (Error.UnsupportedGoal { goal = Goal.RunBinary run })
