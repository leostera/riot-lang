open Std
open Std.Data
open Std.Result.Syntax

type provenance =
  | Workspace
  | Path of Path.t
  | Source of {
      locator: string;
      ref_: string option;
    }
  | Registry of { registry: string }

type package_id = {
  registry: string option;
  name: Package_name.t;
  version: string option;
  sha256: string option;
}

type dependency = {
  name: Package_name.t;
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

type container =
  | Lockfile
  | Package
  | PackageId
  | Dependency
  | DependencyList
  | Provenance

type error =
  | ExpectedTable of {
      container: container;
    }
  | ExpectedArray of {
      container: container;
    }
  | MissingField of {
      container: container;
      field: string;
    }
  | InvalidFieldType of {
      container: container;
      field: string;
      expected: string;
    }
  | InvalidPackageName of {
      container: container;
      field: string;
      value: string;
      error: Package_name.error;
    }
  | UnknownProvenanceKind of { value: string }

let container_name = fun __tmp1 ->
  match __tmp1 with
  | Lockfile -> "lockfile"
  | Package -> "lockfile package"
  | PackageId -> "lockfile package id"
  | Dependency -> "lockfile dependency"
  | DependencyList -> "lockfile dependency list"
  | Provenance -> "lockfile provenance"

let error_message = fun __tmp1 ->
  match __tmp1 with
  | ExpectedTable { container } -> container_name container ^ " must be a table"
  | ExpectedArray { container } -> container_name container ^ " must be an array"
  | MissingField { container; field } ->
      container_name container ^ " is missing required field '" ^ field ^ "'"
  | InvalidFieldType { container; field; expected } ->
      container_name container ^ " field '" ^ field ^ "' must be " ^ expected
  | InvalidPackageName {
      container;
      field;
      value;
      error;
    } ->
      container_name container
      ^ " field '"
      ^ field
      ^ "' contains invalid package name '"
      ^ value
      ^ "': "
      ^ Package_name.error_message error
  | UnknownProvenanceKind { value } -> "unknown lockfile provenance kind '" ^ value ^ "'"

let require_table = fun container value ->
  match value with
  | Toml.Table fields -> Ok fields
  | _ -> Error (ExpectedTable { container })

let require_array = fun container value ->
  match value with
  | Toml.Array items -> Ok items
  | _ -> Error (ExpectedArray { container })

let required_string_field = fun container ~field fields ->
  match Fields.get field fields with
  | Some (Toml.String value) -> Ok value
  | Some _ -> Error (InvalidFieldType { container; field; expected = "a string" })
  | None -> Error (MissingField { container; field })

let optional_string_field = fun ~field fields ->
  match Fields.get field fields with
  | Some (Toml.String value) -> Some value
  | _ -> None

let required_int_field = fun container ~field fields ->
  match Fields.get field fields with
  | Some (Toml.Int value) -> Ok value
  | Some _ -> Error (InvalidFieldType { container; field; expected = "an integer" })
  | None -> Error (MissingField { container; field })

let required_array_field = fun container ~field fields ->
  match Fields.get field fields with
  | Some value -> require_array container value
  | None -> Error (MissingField { container; field })

let parse_package_name = fun container ~field value ->
  Package_name.from_string value
  |> Result.map_err
    ~fn:(fun error ->
      InvalidPackageName {
        container;
        field;
        value;
        error;
      })

let package_id_to_toml = fun (id: package_id) ->
  let fields = [ ("name", Toml.String (Package_name.to_string id.name)); ] in
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
  let* fields = require_table PackageId value in
  let* raw_name = required_string_field PackageId ~field:"name" fields in
  let* name = parse_package_name PackageId ~field:"name" raw_name in
  let registry = optional_string_field ~field:"registry" fields in
  let version = optional_string_field ~field:"version" fields in
  let sha256 = optional_string_field ~field:"sha256" fields in
  Ok {
    registry;
    name;
    version;
    sha256;
  }

let package_id_of_fields = fun fields ->
  let* raw_name = required_string_field Package ~field:"name" fields in
  let* name = parse_package_name Package ~field:"name" raw_name in
  let registry = optional_string_field ~field:"registry" fields in
  let version = optional_string_field ~field:"version" fields in
  let sha256 = optional_string_field ~field:"sha256" fields in
  Ok {
    registry;
    name;
    version;
    sha256;
  }

let provenance_to_toml = fun provenance ->
  match provenance with
  | Workspace -> Toml.Table [ ("kind", Toml.String "workspace"); ]
  | Path path ->
      Toml.Table [ ("kind", Toml.String "path"); ("path", Toml.String (Path.to_string path)); ]
  | Source { locator; ref_ } ->
      let fields = [ ("kind", Toml.String "source"); ("locator", Toml.String locator); ] in
      let fields =
        match ref_ with
        | Some ref_ -> ("ref", Toml.String ref_) :: fields
        | None -> fields
      in
      Toml.Table (List.reverse fields)
  | Registry { registry } ->
      Toml.Table [ ("kind", Toml.String "registry"); ("registry", Toml.String registry); ]

let provenance_of_toml = fun value ->
  let* fields = require_table Provenance value in
  let* kind = required_string_field Provenance ~field:"kind" fields in
  match kind with
  | "workspace" -> Ok Workspace
  | "path" ->
      let* path = required_string_field Provenance ~field:"path" fields in
      Ok (Path (Path.v path))
  | "source" ->
      let* locator = required_string_field Provenance ~field:"locator" fields in
      let ref_ = optional_string_field ~field:"ref" fields in
      Ok (Source { locator; ref_ })
  | "registry" ->
      let* registry = required_string_field Provenance ~field:"registry" fields in
      Ok (Registry { registry })
  | value -> Error (UnknownProvenanceKind { value })

let dependency_to_toml = fun (dep: dependency) ->
  let should_use_flat_registry_shape =
    match dep.package.registry with
    | Some "pkgs.ml" -> true
    | Some _ -> false
    | None -> false
  in
  if should_use_flat_registry_shape then
    let fields = [ ("name", Toml.String (Package_name.to_string dep.name)); ] in
    let fields =
      if Package_name.equal dep.package.name dep.name then
        fields
      else
        ("package_name", Toml.String (Package_name.to_string dep.package.name)) :: fields
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
    Toml.Table [
      ("name", Toml.String (Package_name.to_string dep.name));
      ("package", package_id_to_toml dep.package);
    ]

let dependency_of_toml = fun value ->
  let* fields = require_table Dependency value in
  let* raw_name = required_string_field Dependency ~field:"name" fields in
  let* name = parse_package_name Dependency ~field:"name" raw_name in
  match Fields.get "package" fields with
  | Some package_value ->
      package_id_of_toml package_value
      |> Result.map ~fn:(fun package -> { name; package })
  | None ->
      let package_name =
        match Fields.get "package_name" fields with
        | Some (Toml.String package_name) ->
            parse_package_name Dependency ~field:"package_name" package_name
        | Some _ ->
            Error (InvalidFieldType {
              container = Dependency;
              field = "package_name";
              expected = "a string";
            })
        | None -> Ok name
      in
      let* package_name = package_name in
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
      let version = optional_string_field ~field:"version" fields in
      let sha256 = optional_string_field ~field:"sha256" fields in
      Ok {
        name;
        package =
          {
            name = package_name;
            registry;
            version;
            sha256;
          };
      }

let dependency_list_to_toml = fun deps -> Toml.Array (List.map deps ~fn:dependency_to_toml)

let dependency_list_of_toml = fun value ->
  let* items = require_array DependencyList value in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok (List.reverse acc)
    | item :: rest -> (
        match dependency_of_toml item with
        | Ok dep -> loop (dep :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] items

let package_to_toml = fun (pkg: package) ->
  let fields = [
    ("name", Toml.String (Package_name.to_string pkg.id.name));
    ("provenance", provenance_to_toml pkg.provenance);
    ("dependencies", dependency_list_to_toml pkg.dependencies);
    ("build_dependencies", dependency_list_to_toml pkg.build_dependencies);
    ("dev_dependencies", dependency_list_to_toml pkg.dev_dependencies);
  ]
  in
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
  let* fields = require_table Package value in
  let* provenance_value =
    match Fields.get "provenance" fields with
    | Some value -> Ok value
    | None -> Error (MissingField { container = Package; field = "provenance" })
  in
  let* dependencies_value = required_array_field Package ~field:"dependencies" fields in
  let* build_dependencies_value = required_array_field Package ~field:"build_dependencies" fields in
  let* dev_dependencies_value = required_array_field Package ~field:"dev_dependencies" fields in
  let* id =
    match Fields.get "id" fields with
    | Some id_value -> package_id_of_toml id_value
    | None -> package_id_of_fields fields
  in
  let root =
    match Fields.get "root" fields with
    | Some (Toml.String root) -> Some (Path.v root)
    | _ -> (
        match Fields.get "path" fields with
        | Some (Toml.String legacy_path) -> Some (Path.v legacy_path)
        | _ -> None
      )
  in
  let* provenance = provenance_of_toml provenance_value in
  let* dependencies = dependency_list_of_toml (Toml.Array dependencies_value) in
  let* build_dependencies = dependency_list_of_toml (Toml.Array build_dependencies_value) in
  let* dev_dependencies = dependency_list_of_toml (Toml.Array dev_dependencies_value) in
  Ok {
    id;
    root;
    provenance;
    dependencies;
    build_dependencies;
    dev_dependencies;
  }

let to_toml = fun (lockfile: t) ->
  let fields = [
    ("format_version", Toml.Int lockfile.format_version);
    ("dependency_hash", Toml.String lockfile.dependency_hash);
  ]
  in
  let fields =
    ("packages", Toml.Array (List.map lockfile.packages ~fn:package_to_toml)) :: fields
  in
  Toml.Table (List.reverse fields)

let from_toml = fun value ->
  let* fields = require_table Lockfile value in
  let* format_version = required_int_field Lockfile ~field:"format_version" fields in
  let* dependency_hash = required_string_field Lockfile ~field:"dependency_hash" fields in
  let* packages = required_array_field Lockfile ~field:"packages" fields in
  let rec loop acc = fun __tmp1 ->
    match __tmp1 with
    | [] -> Ok { format_version; dependency_hash; packages = List.reverse acc }
    | pkg :: rest -> (
        match package_of_toml pkg with
        | Ok pkg -> loop (pkg :: acc) rest
        | Error _ as err -> err
      )
  in
  loop [] packages

let render_string = fun value -> Toml.to_string (Toml.String value)

let render_package_id = fun (id: package_id) ->
  let fields = [ ("name", render_string (Package_name.to_string id.name)); ] in
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
  ^ String.concat
    ", "
    (
      fields
      |> List.reverse
      |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value)
    )
  ^ " }"

let render_provenance = fun provenance ->
  match provenance with
  | Workspace -> "{ kind = " ^ render_string "workspace" ^ " }"
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
      ^ String.concat
        ", "
        (
          fields
          |> List.reverse
          |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value)
        )
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
    let fields = [ ("name", render_string (Package_name.to_string dep.name)); ] in
    let fields =
      if Package_name.equal dep.package.name dep.name then
        fields
      else
        ("package_name", render_string (Package_name.to_string dep.package.name)) :: fields
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
    ^ String.concat
      ", "
      (
        fields
        |> List.reverse
        |> List.map ~fn:(fun (key, value) -> key ^ " = " ^ value)
      )
    ^ " }"
  else
    "{ name = "
    ^ render_string (Package_name.to_string dep.name)
    ^ ", package = "
    ^ render_package_id dep.package
    ^ " }"

let render_dependency_list = fun deps ->
  "[" ^ String.concat ", " (List.map deps ~fn:render_dependency) ^ "]"

let render_package = fun (pkg: package) ->
  let header_lines =
    [
      Some ("name = " ^ render_string (Package_name.to_string pkg.id.name));
      (
        match pkg.id.registry with
        | Some registry -> Some ("registry = " ^ render_string registry)
        | None -> None
      );
      (
        match pkg.id.version with
        | Some version -> Some ("version = " ^ render_string version)
        | None -> None
      );
      (
        match pkg.id.sha256 with
        | Some sha256 -> Some ("sha256 = " ^ render_string sha256)
        | None -> None
      );
      (
        match pkg.root with
        | Some root -> Some ("root = " ^ render_string (Path.to_string root))
        | None -> None
      );
      Some ("provenance = " ^ render_provenance pkg.provenance);
      Some ("dependencies = " ^ render_dependency_list pkg.dependencies);
      Some ("build_dependencies = " ^ render_dependency_list pkg.build_dependencies);
      Some ("dev_dependencies = " ^ render_dependency_list pkg.dev_dependencies);
    ]
    |> List.filter_map ~fn:(fun item -> item)
  in
  String.concat "\n" ("[[packages]]" :: header_lines)

let to_string = fun lockfile ->
  let parts =
    [
      "format_version = " ^ Int.to_string lockfile.format_version;
      "dependency_hash = " ^ render_string lockfile.dependency_hash;
    ] @ (
      if List.is_empty lockfile.packages then
        [ "packages = []" ]
      else
        List.map lockfile.packages ~fn:render_package
    )
  in
  String.concat "\n\n" parts ^ "\n"
