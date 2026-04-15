open Std
module Test = Std.Test

let test_from_string_accepts_valid_package_name = fun _ctx ->
  match Riot_model.Package_name.from_string "riot-install" with
  | Ok name ->
      if String.equal (Riot_model.Package_name.to_string name) "riot-install" then
        Ok ()
      else
        Error "expected package name to roundtrip through Package_name.to_string"
  | Error err -> Error ("expected valid package name, got error: " ^ err)

let test_from_string_rejects_invalid_leading_character = fun _ctx ->
  match Riot_model.Package_name.from_string "Riot" with
  | Ok _ -> Error "expected uppercase-leading package name to be rejected"
  | Error err ->
      if String.contains err "start with a lowercase letter" then
        Ok ()
      else
        Error ("expected lowercase-leading validation error, got: " ^ err)

let test_from_string_rejects_invalid_suffix = fun _ctx ->
  match Riot_model.Package_name.from_string "riot-" with
  | Ok _ -> Error "expected trailing hyphen package name to be rejected"
  | Error err ->
      if String.contains err "cannot end with hyphen or underscore" then
        Ok ()
      else
        Error ("expected trailing delimiter validation error, got: " ^ err)

let tests =
  Test.[
    case "Package_name.from_string accepts valid names" test_from_string_accepts_valid_package_name;
    case "Package_name.from_string rejects invalid leading characters" test_from_string_rejects_invalid_leading_character;
    case "Package_name.from_string rejects invalid suffixes" test_from_string_rejects_invalid_suffix;
  ]

let () =
  Runtime.run ~main:(fun ~args -> Test.Cli.main ~name:"package_name" ~tests ~args) ~args:Env.args ()
