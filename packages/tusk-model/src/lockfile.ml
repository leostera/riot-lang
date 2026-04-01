open Std
open Std.Data

type provenance =
  | Workspace
  | Path of Path.t
  | Registry of { registry: string }

type package_id = {
  registry: string option;
  name: string;
  version: string option;
}

type dependency = {
  name: string;
  package: package_id;
}

type package = {
  id: package_id;
  path: Path.t;
  manifest_path: Path.t;
  provenance: provenance;
  dependencies: dependency list;
  build_dependencies: dependency list;
  dev_dependencies: dependency list;
}

type t = {
  format_version: int;
  packages: package list;
}

let package_id_to_toml = fun (id: package_id) ->
  let fields = [ ("name", Toml.String id.name) ] in
  let fields =
    match id.registry with
    | Some registry -> ("registry", Toml.String registry) :: fields
    | None -> fields
  in
  let fields =
    match id.version with
    | Some version -> ("version", Toml.String version) :: fields
    | None -> fields
  in
  Toml.Table (List.rev fields)

let package_id_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match List.assoc_opt "name" fields with
      | Some (Toml.String name) ->
          let registry =
            match List.assoc_opt "registry" fields with
            | Some (Toml.String registry) -> Some registry
            | _ -> None
          in
          let version =
            match List.assoc_opt "version" fields with
            | Some (Toml.String version) -> Some version
            | _ -> None
          in
          Ok { registry; name; version }
      | _ -> Error "lockfile package id is missing required field 'name'"
    )
  | _ -> Error "lockfile package id must be a table"

let provenance_to_toml = fun provenance ->
  match provenance with
  | Workspace -> Toml.Table [ ("kind", Toml.String "workspace") ]
  | Path path ->
      Toml.Table [ ("kind", Toml.String "path"); ("path", Toml.String (Path.to_string path)) ]
  | Registry { registry } ->
      Toml.Table [ ("kind", Toml.String "registry"); ("registry", Toml.String registry) ]

let provenance_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match List.assoc_opt "kind" fields with
      | Some (Toml.String "workspace") -> Ok Workspace
      | Some (Toml.String "path") -> (
          match List.assoc_opt "path" fields with
          | Some (Toml.String path) -> Ok (Path (Path.v path))
          | _ -> Error "lockfile path provenance is missing required field 'path'"
        )
      | Some (Toml.String "registry") -> (
          match List.assoc_opt "registry" fields with
          | Some (Toml.String registry) -> Ok (Registry { registry })
          | _ -> Error "lockfile registry provenance is missing required field 'registry'"
        )
      | Some (Toml.String kind) -> Error ("unknown lockfile provenance kind '" ^ kind ^ "'")
      | _ -> Error "lockfile provenance is missing required field 'kind'"
    )
  | _ -> Error "lockfile provenance must be a table"

let dependency_to_toml = fun (dep: dependency) ->
  Toml.Table [
    ("name", Toml.String dep.name);
    ("package", package_id_to_toml dep.package);
  ]

let dependency_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match List.assoc_opt "name" fields, List.assoc_opt "package" fields with
      | Some (Toml.String name), Some package_value ->
          package_id_of_toml package_value
          |> Result.map (fun package -> { name; package })
      | _ -> Error "lockfile dependency must contain 'name' and 'package'"
    )
  | _ -> Error "lockfile dependency must be a table"

let dependency_list_to_toml = fun deps ->
  Toml.Array (List.map dependency_to_toml deps)

let dependency_list_of_toml = fun value ->
  match value with
  | Toml.Array items ->
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | item :: rest -> (
            match dependency_of_toml item with
            | Ok dep -> loop (dep :: acc) rest
            | Error _ as err -> err
          )
      in
      loop [] items
  | _ -> Error "lockfile dependency list must be an array"

let package_to_toml = fun (pkg: package) ->
  Toml.Table [
    ("id", package_id_to_toml pkg.id);
    ("path", Toml.String (Path.to_string pkg.path));
    ("manifest_path", Toml.String (Path.to_string pkg.manifest_path));
    ("provenance", provenance_to_toml pkg.provenance);
    ("dependencies", dependency_list_to_toml pkg.dependencies);
    ("build_dependencies", dependency_list_to_toml pkg.build_dependencies);
    ("dev_dependencies", dependency_list_to_toml pkg.dev_dependencies);
  ]

let package_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match
        List.assoc_opt "id" fields,
        List.assoc_opt "path" fields,
        List.assoc_opt "manifest_path" fields,
        List.assoc_opt "provenance" fields,
        List.assoc_opt "dependencies" fields,
        List.assoc_opt "build_dependencies" fields,
        List.assoc_opt "dev_dependencies" fields
      with
      | Some id_value,
        Some (Toml.String path),
        Some (Toml.String manifest_path),
        Some provenance_value,
        Some dependencies_value,
        Some build_dependencies_value,
        Some dev_dependencies_value -> (
          match package_id_of_toml id_value with
          | Error _ as err -> err
          | Ok id -> (
              match provenance_of_toml provenance_value with
              | Error _ as err -> err
              | Ok provenance -> (
                  match dependency_list_of_toml dependencies_value with
                  | Error _ as err -> err
                  | Ok dependencies -> (
                      match dependency_list_of_toml build_dependencies_value with
                      | Error _ as err -> err
                      | Ok build_dependencies -> (
                          match dependency_list_of_toml dev_dependencies_value with
                          | Error _ as err -> err
                          | Ok dev_dependencies ->
                              Ok {
                                id;
                                path = Path.v path;
                                manifest_path = Path.v manifest_path;
                                provenance;
                                dependencies;
                                build_dependencies;
                                dev_dependencies;
                              }))))
        )
      | _ -> Error "lockfile package is missing required fields"
    )
  | _ -> Error "lockfile package must be a table"

let to_toml = fun (lockfile: t) ->
  Toml.Table [
    ("format_version", Toml.Int lockfile.format_version);
    ("packages", Toml.Array (List.map package_to_toml lockfile.packages));
  ]

let of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match List.assoc_opt "format_version" fields, List.assoc_opt "packages" fields with
      | Some (Toml.Int format_version), Some (Toml.Array packages) ->
          let rec loop acc = function
            | [] ->
                Ok {
                  format_version;
                  packages = List.rev acc;
                }
            | pkg :: rest -> (
                match package_of_toml pkg with
                | Ok pkg -> loop (pkg :: acc) rest
                | Error _ as err -> err
              )
          in
          loop [] packages
      | _ -> Error "lockfile is missing required fields 'format_version' and 'packages'"
    )
  | _ -> Error "lockfile must be a table"

let render_string = fun value ->
  Toml.to_string (Toml.String value)

let render_package_id = fun (id: package_id) ->
  let fields = [ ("name", render_string id.name) ] in
  let fields =
    match id.registry with
    | Some registry -> ("registry", render_string registry) :: fields
    | None -> fields
  in
  let fields =
    match id.version with
    | Some version -> ("version", render_string version) :: fields
    | None -> fields
  in
  "{ " ^ String.concat ", " (List.rev_map (fun (key, value) -> key ^ " = " ^ value) fields) ^ " }"

let render_provenance = fun provenance ->
  match provenance with
  | Workspace ->
      "{ kind = " ^ render_string "workspace" ^ " }"
  | Path path ->
      "{ kind = "
      ^ render_string "path"
      ^ ", path = "
      ^ render_string (Path.to_string path)
      ^ " }"
  | Registry { registry } ->
      "{ kind = "
      ^ render_string "registry"
      ^ ", registry = "
      ^ render_string registry
      ^ " }"

let render_dependency = fun (dep: dependency) ->
  "{ name = "
  ^ render_string dep.name
  ^ ", package = "
  ^ render_package_id dep.package
  ^ " }"

let render_dependency_list = fun deps ->
  "[" ^ String.concat ", " (List.map render_dependency deps) ^ "]"

let render_package = fun (pkg: package) ->
  String.concat "\n"
    [
      "[[packages]]";
      "id = " ^ render_package_id pkg.id;
      "path = " ^ render_string (Path.to_string pkg.path);
      "manifest_path = " ^ render_string (Path.to_string pkg.manifest_path);
      "provenance = " ^ render_provenance pkg.provenance;
      "dependencies = " ^ render_dependency_list pkg.dependencies;
      "build_dependencies = " ^ render_dependency_list pkg.build_dependencies;
      "dev_dependencies = " ^ render_dependency_list pkg.dev_dependencies;
    ]

let to_string = fun lockfile ->
  let parts =
    ("format_version = " ^ Int.to_string lockfile.format_version)
    :: List.map render_package lockfile.packages
  in
  String.concat "\n\n" parts ^ "\n"

module Tests = struct
  let test_lockfile_roundtrip_toml () : (unit, string) result =
    let lockfile =
      {
        format_version = 1;
        packages = [
          {
            id = { registry = None; name = "app"; version = None };
            path = Path.v "/workspace/packages/app";
            manifest_path = Path.v "/workspace/packages/app/tusk.toml";
            provenance = Workspace;
            dependencies = [
              {
                name = "std";
                package = {
                  registry = Some "pkgs.ml";
                  name = "std";
                  version = Some "0.1.0";
                };
              };
            ];
            build_dependencies = [];
            dev_dependencies = [];
          };
          {
            id = { registry = Some "pkgs.ml"; name = "std"; version = Some "0.1.0" };
            path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0";
            manifest_path = Path.v "/Users/example/.tusk/registry/pkgs.ml/src/std/0.1.0/tusk.toml";
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          };
        ];
      }
    in
    let parsed =
      match lockfile |> to_string |> Toml.parse with
      | Ok toml -> Ok toml
      | Error err ->
          Error ("expected generated lockfile TOML to parse: " ^ Toml.error_to_string err)
    in
    match parsed with
    | Ok toml -> (
        match of_toml toml with
        | Ok parsed ->
        if
          parsed.format_version = 1
          && List.length parsed.packages = 2
          && (List.hd parsed.packages).id.name = "app"
          && (List.nth parsed.packages 1).id.version = Some "0.1.0"
        then
          Ok ()
        else
          Error "expected lockfile to round-trip through TOML"
        | Error err -> Error err
      )
    | Error err -> Error err
    [@test]

  let test_lockfile_parses_path_provenance () : (unit, string) result =
    let toml =
      Toml.parse
        {|
format_version = 1

[[packages]]
path = "/workspace/vendor/foo"
manifest_path = "/workspace/vendor/foo/tusk.toml"
dependencies = []
build_dependencies = []
dev_dependencies = []

[packages.id]
name = "foo"
version = "1.2.3"

[packages.provenance]
kind = "path"
path = "../vendor/foo"
|}
      |> Result.expect ~msg:"expected test lockfile TOML to parse"
    in
    match of_toml toml with
    | Ok { packages = [ pkg ]; _ } -> (
        match pkg.provenance with
        | Path path when String.equal (Path.to_string path) "../vendor/foo" -> Ok ()
        | _ -> Error "expected path provenance to parse"
      )
    | Ok _ -> Error "expected one package in parsed lockfile"
    | Error err -> Error err
    [@test]
end [@test]
