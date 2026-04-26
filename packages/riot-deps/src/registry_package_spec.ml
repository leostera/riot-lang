open Std
open Std.Result.Syntax

type t = {
  name: Riot_model.Package_name.t;
  requirement: Std.Version.requirement option;
}

type error =
  | InvalidShape of { spec: string }
  | InvalidPackageName of {
      spec: string;
      name: string;
      error: Riot_model.Package_name.error;
    }
  | InvalidRequirement of {
      spec: string;
      requirement: string;
      error: Std.Version.parse_error;
    }

let version_parse_error_message = function
  | Std.Version.Invalid_format msg -> msg
  | Std.Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Std.Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

let parse_name = fun spec raw_name ->
  let name = String.trim raw_name in
  Riot_model.Package_name.from_string name
  |> Result.map_err ~fn:(fun error -> InvalidPackageName { spec; name; error })

let from_string = fun spec ->
  match String.split ~by:"@" spec with
  | [ name ] ->
      let* name = parse_name spec name in
      Ok { name; requirement = Some Std.Version.any }
  | [ name; requirement ] ->
      let* name = parse_name spec name in
      let requirement = String.trim requirement in
      let* requirement =
        Std.Version.parse_requirement requirement
        |> Result.map_err ~fn:(fun error -> InvalidRequirement { spec; requirement; error })
      in
      Ok { name; requirement = Some requirement }
  | _ -> Error (InvalidShape { spec })

let to_string = fun { name; requirement } ->
  let name = Riot_model.Package_name.to_string name in
  match requirement with
  | None -> name
  | Some requirement -> (
      match Std.Version.view_requirement requirement with
      | Std.Version.AnyRequirement -> name
      | _ -> name ^ "@" ^ Std.Version.requirement_to_string requirement
    )

let error_message = function
  | InvalidShape { spec } ->
      "invalid registry package spec '" ^ spec ^ "': expected <name> or <name>@<version>"
  | InvalidPackageName { spec; error; _ } ->
      "invalid registry package spec '" ^ spec ^ "': " ^ Riot_model.Package_name.error_message error
  | InvalidRequirement { spec; error; _ } ->
      "invalid registry package spec '" ^ spec ^ "': " ^ version_parse_error_message error
