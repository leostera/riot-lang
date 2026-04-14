open Std

type package_status =
  | Built of Riot_store.Artifact.t
  | Cached of Riot_store.Artifact.t
  | Skipped of string
  | Failed of string

type package_output = {
  package_name: Riot_model.Package_name.t;
  status: package_status;
}

type t = {
  packages: package_output list;
}

let package_status_of_build_status = function
  | Riot_executor.Package_builder.Built artifact -> Built artifact
  | Riot_executor.Package_builder.Cached artifact -> Cached artifact
  | Riot_executor.Package_builder.Skipped { reason } -> Skipped reason
  | Riot_executor.Package_builder.Failed error ->
      Failed (Riot_executor.Package_builder.package_error_to_string error)

let of_build_results = fun results ->
  {
    packages =
      List.map results ~fn:(fun (result: Riot_executor.Package_builder.build_result) ->
          {
            package_name = result.package.name;
            status = package_status_of_build_status result.status;
          });
  }

let packages = fun t -> t.packages

let find_package = fun t name ->
  List.find t.packages ~fn:(fun pkg -> Riot_model.Package_name.equal pkg.package_name name)

let package_name = fun t -> t.package_name

let package_status = fun t -> t.status

let package_artifact = fun t ->
  match t.status with
  | Built artifact
  | Cached artifact -> Some artifact
  | Skipped _
  | Failed _ -> None

let find_export = fun t export_name ->
  package_artifact t
  |> Option.and_then ~fn:(fun artifact ->
      let artifact: Riot_store.Artifact.t = artifact in
      List.find artifact.exports ~fn:(fun (entry: Riot_store.Manifest.export_entry) ->
          String.equal entry.name export_name))
