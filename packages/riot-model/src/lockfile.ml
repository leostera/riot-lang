open Std
open Std.Data

type provenance =
  | Workspace
  | Path of Path.t
  | Source of { locator: string; ref_: string option }
  | Registry of { registry: string }

type package_id = {
  registry: string option;
  name: string;
  version: string option;
  sha256: string option;
}

type dependency = {
  name: string;
  package: package_id;
}

type package = {
  id: package_id;
  root: Path.t option;
  provenance: provenance;
  dependencies: dependency list;
  build_dependencies: dependency list;
  dev_dependencies: dependency list;
}

type t = {
  format_version: int;
  dependency_hash: string;
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
  let fields =
    match id.sha256 with
    | Some sha256 -> ("sha256", Toml.String sha256) :: fields
    | None -> fields
  in
  Toml.Table (List.reverse fields)

let package_id_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "name" fields with
      | Some (Toml.String name) ->
          let registry =
            match Fields.get "registry" fields with
            | Some (Toml.String registry) -> Some registry
            | _ -> None
          in
          let version =
            match Fields.get "version" fields with
            | Some (Toml.String version) -> Some version
            | _ -> None
          in
          let sha256 =
            match Fields.get "sha256" fields with
            | Some (Toml.String sha256) -> Some sha256
            | _ -> None
          in
          Ok { registry; name; version; sha256 }
      | _ -> Error "lockfile package id is missing required field 'name'"
    )
  | _ -> Error "lockfile package id must be a table"

let package_id_of_fields = fun fields ->
  match Fields.get "name" fields with
  | Some (Toml.String name) ->
      let registry =
        match Fields.get "registry" fields with
        | Some (Toml.String registry) -> Some registry
        | _ -> None
      in
      let version =
        match Fields.get "version" fields with
        | Some (Toml.String version) -> Some version
        | _ -> None
      in
      let sha256 =
        match Fields.get "sha256" fields with
        | Some (Toml.String sha256) -> Some sha256
        | _ -> None
      in
      Ok { registry; name; version; sha256 }
  | _ -> Error "lockfile package is missing required field 'name'"

let provenance_to_toml = fun provenance ->
  match provenance with
  | Workspace ->
      Toml.Table [ ("kind", Toml.String "workspace") ]
  | Path path ->
      Toml.Table [ ("kind", Toml.String "path"); ("path", Toml.String (Path.to_string path)) ]
  | Source { locator; ref_ } ->
      let fields = [ ("kind", Toml.String "source"); ("locator", Toml.String locator); ] in
      let fields =
        match ref_ with
        | Some ref_ -> ("ref", Toml.String ref_) :: fields
        | None -> fields
      in
      Toml.Table (List.reverse fields)
  | Registry { registry } ->
      Toml.Table [ ("kind", Toml.String "registry"); ("registry", Toml.String registry) ]

let provenance_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "kind" fields with
      | Some (Toml.String "workspace") ->
          Ok Workspace
      | Some (Toml.String "path") -> (
          match Fields.get "path" fields with
          | Some (Toml.String path) -> Ok (Path (Path.v path))
          | _ -> Error "lockfile path provenance is missing required field 'path'"
        )
      | Some (Toml.String "source") -> (
          match Fields.get "locator" fields with
          | Some (Toml.String locator) ->
              let ref_ =
                match Fields.get "ref" fields with
                | Some (Toml.String ref_) -> Some ref_
                | _ -> None
              in
              Ok (Source { locator; ref_ })
          | _ -> Error "lockfile source provenance is missing required field 'locator'"
        )
      | Some (Toml.String "registry") -> (
          match Fields.get "registry" fields with
          | Some (Toml.String registry) -> Ok (Registry { registry })
          | _ -> Error "lockfile registry provenance is missing required field 'registry'"
        )
      | Some (Toml.String kind) ->
          Error ("unknown lockfile provenance kind '" ^ kind ^ "'")
      | _ ->
          Error "lockfile provenance is missing required field 'kind'"
    )
  | _ -> Error "lockfile provenance must be a table"

let dependency_to_toml = fun (dep: dependency) ->
  let should_use_flat_registry_shape =
    match dep.package.registry with
    | Some "pkgs.ml" -> true
    | Some _ -> false
    | None -> false
  in
  if should_use_flat_registry_shape then
    let fields = [ ("name", Toml.String dep.name) ] in
    let fields =
      if String.equal dep.package.name dep.name then
        fields
      else
        ("package_name", Toml.String dep.package.name) :: fields
    in
    let fields =
      match dep.package.version with
      | Some version -> ("version", Toml.String version) :: fields
      | None -> fields
    in
    let fields =
      match dep.package.sha256 with
      | Some sha256 -> ("sha256", Toml.String sha256) :: fields
      | None -> fields
    in
    Toml.Table (List.reverse fields)
  else
    Toml.Table [ ("name", Toml.String dep.name); ("package", package_id_to_toml dep.package) ]

let dependency_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "name" fields with
      | Some (Toml.String name) -> (
          match Fields.get "package" fields with
          | Some package_value -> package_id_of_toml package_value
          |> Result.map ~fn:(fun package -> { name; package })
          | None ->
              let package_name =
                match Fields.get "package_name" fields with
                | Some (Toml.String package_name) -> package_name
                | _ -> name
              in
              let registry =
                match Fields.get "registry" fields with
                | Some (Toml.String registry) -> Some registry
                | _ ->
                    if
                      List.any fields ~fn:(fun (field_name, _value) -> String.equal field_name "version")
                      || List.any fields ~fn:(fun (field_name, _value) -> String.equal field_name "sha256")
                    then
                      Some "pkgs.ml"
                    else
                      None
              in
              let version =
                match Fields.get "version" fields with
                | Some (Toml.String version) -> Some version
                | _ -> None
              in
              let sha256 =
                match Fields.get "sha256" fields with
                | Some (Toml.String sha256) -> Some sha256
                | _ -> None
              in
              Ok { name; package = { name = package_name; registry; version; sha256 } }
        )
      | _ -> Error "lockfile dependency must contain 'name'"
    )
  | _ -> Error "lockfile dependency must be a table"

let dependency_list_to_toml = fun deps -> Toml.Array (List.map deps ~fn:dependency_to_toml)

let dependency_list_of_toml = fun value ->
  match value with
  | Toml.Array items ->
      let rec loop acc = function
        | [] -> Ok (List.reverse acc)
        | item :: rest -> (
            match dependency_of_toml item with
            | Ok dep -> loop (dep :: acc) rest
            | Error _ as err -> err
          )
      in
      loop [] items
  | _ -> Error "lockfile dependency list must be an array"

let package_to_toml = fun (pkg: package) ->
  let fields = [
    ("name", Toml.String pkg.id.name);
    ("provenance", provenance_to_toml pkg.provenance);
    ("dependencies", dependency_list_to_toml pkg.dependencies);
    ("build_dependencies", dependency_list_to_toml pkg.build_dependencies);
    ("dev_dependencies", dependency_list_to_toml pkg.dev_dependencies);
  ] in
  let fields =
    match pkg.id.registry with
    | Some registry -> ("registry", Toml.String registry) :: fields
    | None -> fields
  in
  let fields =
    match pkg.id.version with
    | Some version -> ("version", Toml.String version) :: fields
    | None -> fields
  in
  let fields =
    match pkg.id.sha256 with
    | Some sha256 -> ("sha256", Toml.String sha256) :: fields
    | None -> fields
  in
  let fields =
    match pkg.root with
    | Some root -> ("root", Toml.String (Path.to_string root)) :: fields
    | None -> fields
  in
  Toml.Table (List.reverse fields)

let package_of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "provenance" fields, Fields.get "dependencies" fields, Fields.get
        "build_dependencies"
        fields, Fields.get "dev_dependencies" fields with
      | Some provenance_value, Some dependencies_value, Some build_dependencies_value, Some dev_dependencies_value -> (
          let id_result =
            match Fields.get "id" fields with
            | Some id_value -> package_id_of_toml id_value
            | None -> package_id_of_fields fields
          in
          match id_result with
          | Error _ as err -> err
          | Ok id -> (
              let root =
                match Fields.get "root" fields with
                | Some (Toml.String root) -> Some (Path.v root)
                | _ -> (
                    match Fields.get "path" fields with
                    | Some (Toml.String legacy_path) -> Some (Path.v legacy_path)
                    | _ -> None
                  )
              in
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
                                root;
                                provenance;
                                dependencies;
                                build_dependencies;
                                dev_dependencies;
                              }
                        )
                    )
                )
            )
        )
      | _ -> Error "lockfile package is missing required fields"
    )
  | _ -> Error "lockfile package must be a table"

let to_toml = fun (lockfile: t) ->
  let fields = [
    ("format_version", Toml.Int lockfile.format_version);
    ("dependency_hash", Toml.String lockfile.dependency_hash);
  ] in
  let fields = ("packages", Toml.Array (List.map lockfile.packages ~fn:package_to_toml)) :: fields in
  Toml.Table (List.reverse fields)

let of_toml = fun value ->
  match value with
  | Toml.Table fields -> (
      match Fields.get "format_version" fields, Fields.get "dependency_hash" fields, Fields.get
        "packages"
        fields with
      | Some (Toml.Int format_version), Some (Toml.String dependency_hash), Some (Toml.Array packages) ->
          let rec loop acc = function
            | [] -> Ok { format_version; dependency_hash; packages = List.reverse acc }
            | pkg :: rest -> (
                match package_of_toml pkg with
                | Ok pkg -> loop (pkg :: acc) rest
                | Error _ as err -> err
              )
          in
          loop [] packages
      | _ -> Error "lockfile is missing required fields 'format_version', 'dependency_hash', and 'packages'"
    )
  | _ -> Error "lockfile must be a table"

let render_string = fun value -> Toml.to_string (Toml.String value)

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
  let fields =
    match id.sha256 with
    | Some sha256 -> ("sha256", render_string sha256) :: fields
    | None -> fields
  in
  "{ "
  ^ String.concat ", " (fields |> List.reverse |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value))
  ^ " }"

let render_provenance = fun provenance ->
  match provenance with
  | Workspace ->
      "{ kind = " ^ render_string "workspace" ^ " }"
  | Path path ->
      "{ kind = " ^ render_string "path" ^ ", path = " ^ render_string (Path.to_string path) ^ " }"
  | Source { locator; ref_ } ->
      let fields = [ ("kind", render_string "source"); ("locator", render_string locator); ] in
      let fields =
        match ref_ with
        | Some ref_ -> ("ref", render_string ref_) :: fields
        | None -> fields
      in
      "{ "
      ^ String.concat ", " (fields |> List.reverse |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value))
      ^ " }"
  | Registry { registry } ->
      "{ kind = " ^ render_string "registry" ^ ", registry = " ^ render_string registry ^ " }"

let render_dependency = fun (dep: dependency) ->
  let is_flat_registry_dependency =
    match dep.package.registry with
    | Some "pkgs.ml" -> true
    | Some _ -> false
    | None -> false
  in
  if is_flat_registry_dependency then
    let fields = [ ("name", render_string dep.name) ] in
    let fields =
      if String.equal dep.package.name dep.name then
        fields
      else
        ("package_name", render_string dep.package.name) :: fields
    in
    let fields =
      match dep.package.version with
      | Some version -> ("version", render_string version) :: fields
      | None -> fields
    in
    let fields =
      match dep.package.sha256 with
      | Some sha256 -> ("sha256", render_string sha256) :: fields
      | None -> fields
    in
    "{ "
    ^ String.concat ", " (fields |> List.reverse |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value))
    ^ " }"
  else
    "{ name = " ^ render_string dep.name ^ ", package = " ^ render_package_id dep.package ^ " }"

let render_dependency_list = fun deps ->
  "[" ^ String.concat ", " (List.map deps ~fn:render_dependency) ^ "]"

let render_package = fun (pkg: package) ->
  let header_lines =
    [ Some ("name = " ^ render_string pkg.id.name); (
        match pkg.id.registry with
        | Some registry -> Some ("registry = " ^ render_string registry)
        | None -> None
      ); (
        match pkg.id.version with
        | Some version -> Some ("version = " ^ render_string version)
        | None -> None
      ); (
        match pkg.id.sha256 with
        | Some sha256 -> Some ("sha256 = " ^ render_string sha256)
        | None -> None
      ); (
        match pkg.root with
        | Some root -> Some ("root = " ^ render_string (Path.to_string root))
        | None -> None
      ); Some ("provenance = " ^ render_provenance pkg.provenance); Some ("dependencies = "
      ^ render_dependency_list pkg.dependencies); Some ("build_dependencies = "
      ^ render_dependency_list pkg.build_dependencies); Some ("dev_dependencies = "
      ^ render_dependency_list pkg.dev_dependencies); ]
    |> List.filter_map ~fn:(fun item -> item)
  in
  String.concat "\n" ("[[packages]]" :: header_lines)

let to_string = fun lockfile ->
  let parts = [
    "format_version = " ^ Int.to_string lockfile.format_version;
    "dependency_hash = " ^ render_string lockfile.dependency_hash;
  ]
  @ (
    if List.is_empty lockfile.packages then
      [ "packages = []" ]
    else
      List.map lockfile.packages ~fn:render_package
  )
  in
  String.concat "\n\n" parts ^ "\n"

module Tests = struct
  let test_lockfile_roundtrip_toml (): (unit, string) result =
    let lockfile = {
      format_version = 1;
      dependency_hash = "deadbeefcafebabe";
      packages =
        [ {
            id = { registry = None; name = "app"; version = None; sha256 = None };
            root = Some (Path.v "packages/app");
            provenance = Workspace;
            dependencies = [
              {
                name = "std";
                package = {
                  registry = Some "pkgs.ml";
                  name = "std";
                  version = Some "0.1.0";
                  sha256 = Some "deadbeef"
                }
              };
            ];
            build_dependencies = [];
            dev_dependencies = [];
          }; {
            id = {
              registry = Some "pkgs.ml";
              name = "std";
              version = Some "0.1.0";
              sha256 = Some "deadbeef"
            };
            root = None;
            provenance = Registry { registry = "pkgs.ml" };
            dependencies = [];
            build_dependencies = [];
            dev_dependencies = [];
          }; ];
    }
    in
    let rendered = to_string lockfile in
    let parsed =
      match rendered |> Toml.parse with
      | Ok toml -> Ok toml
      | Error err -> Error ("expected generated lockfile TOML to parse: " ^ Toml.error_to_string err)
    in
    match parsed with
    | Ok toml -> (
        match of_toml toml with
        | Ok parsed ->
            let first_package = List.get parsed.packages ~at:0 in
            let second_package = List.get parsed.packages ~at:1 in
            if
              parsed.format_version = 1
              && String.equal parsed.dependency_hash "deadbeefcafebabe"
              && List.length parsed.packages = 2
              && (
                match first_package with
                | Some package -> String.equal package.id.name "app"
                | None -> false
              )
              && (
                match second_package with
                | Some package -> package.id.version = Some "0.1.0"
                | None -> false
              )
              && (
                match second_package with
                | Some package -> package.id.sha256 = Some "deadbeef"
                | None -> false
              )
              && String.contains rendered {|dependency_hash = "deadbeefcafebabe"|}
              && String.contains rendered {|dependencies = [{ name = "std", version = "0.1.0", sha256 = "deadbeef" }]|}
              && not (String.contains rendered "package = {")
            then
              Ok ()
            else
              Error "expected lockfile to round-trip through TOML"
        | Error err -> Error err
      )
    | Error err -> Error err [@test]

  let test_lockfile_parses_path_provenance (): (unit, string) result =
    let toml =
      Toml.parse
        {|
format_version = 1

[[packages]]
root = "../vendor/foo"
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
    | Ok { packages=[ pkg ]; _ } -> (
        match pkg.provenance, pkg.root with
        | Path path, Some root when String.equal (Path.to_string path) "../vendor/foo"
        && String.equal (Path.to_string root) "../vendor/foo" -> Ok ()
        | _ -> Error "expected path provenance to parse"
      )
    | Ok _ ->
        Error "expected one package in parsed lockfile"
    | Error err ->
        Error err [@test]

  let test_lockfile_roundtrips_empty_packages (): (unit, string) result =
    let lockfile = { format_version = 1; dependency_hash = "empty-lock"; packages = [] } in
    let rendered = to_string lockfile in
    match Toml.parse rendered with
    | Error err -> Error ("expected generated empty lockfile TOML to parse: "
    ^ Toml.error_to_string err)
    | Ok toml -> (
        match of_toml toml with
        | Ok parsed when Int.equal parsed.format_version 1
        && String.equal parsed.dependency_hash "empty-lock"
        && List.is_empty parsed.packages
        && String.contains rendered "packages = []" -> Ok ()
        | Ok _ -> Error "expected empty lockfile to round-trip through TOML"
        | Error err -> Error err
      ) [@test]
end [@test]
