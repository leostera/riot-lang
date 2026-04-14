open Std
open Std.Result.Syntax

type t = {
  name: Riot_model.Package_name.t;
  requirement: Std.Version.requirement option;
}

type error =
  | Invalid_spec of { spec: string; error: string }

let from_string = fun spec ->
  match String.split ~by:"@" spec with
  | [ name ] ->
      Riot_model.Package_name.from_string (String.trim name)
      |> Result.map ~fn:(fun name -> { name; requirement = Some Std.Version.any })
      |> Result.map_err ~fn:(fun error -> Invalid_spec { spec; error })
  | [name;requirement] ->
      let* name =
        Riot_model.Package_name.from_string (String.trim name)
        |> Result.map_err ~fn:(fun error -> Invalid_spec { spec; error })
      in
      let requirement = String.trim requirement in
      let* requirement =
        Std.Version.parse_requirement requirement
        |> Result.map_err ~fn:(fun error ->
            Invalid_spec {
              spec;
              error =
                match error with
                | Std.Version.Invalid_format msg -> msg
                | Std.Version.Invalid_version_segment segment ->
                    "invalid version segment: " ^ segment
                | Std.Version.Invalid_pre_release_segment segment ->
                    "invalid pre-release segment: " ^ segment;
            })
      in
      Ok { name; requirement = Some requirement }
  | _ ->
      Error (Invalid_spec {
        spec;
        error = "expected <name> or <name>@<version>";
      })

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
  | Invalid_spec { spec; error } ->
      "invalid registry package spec '" ^ spec ^ "': " ^ error
