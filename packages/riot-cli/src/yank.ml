open Std
open Riot_model
open Std.Result.Syntax

let registry_name = "pkgs.ml"

type error =
  | MissingPackageSpec
  | InvalidPackageSpec of string
  | InvalidPackageName of string
  | InvalidVersion of string
  | NotLoggedIn
  | ConfigFailed of string
  | PromptFailed of string
  | YankFailed of string
  | Aborted

let command =
  let open ArgParser in
    let open Arg in command "yank"
    |> about "Yank a published package version from pkgs.ml"
    |> args [ positional "package" |> help "Package release in the form <name>@<version>"; ]

let message = function
  | MissingPackageSpec -> "missing package release, expected <name>@<version>"
  | InvalidPackageSpec value -> "invalid package release '" ^ value ^ "', expected <name>@<version>"
  | InvalidPackageName err -> err
  | InvalidVersion err -> "invalid version: " ^ err
  | NotLoggedIn -> "not logged in to pkgs.ml; run 'riot login'"
  | ConfigFailed err -> err
  | PromptFailed err -> "failed to read confirmation: " ^ err
  | YankFailed err -> err
  | Aborted -> "aborted"

let fail = fun err ->
  let message = message err in
  eprintln ("\027[1;31mError\027[0m: " ^ message);
  Error (Failure message)

let config_path = fun () -> Riot_dirs.config_path ()

let load_config = fun path ->
  match Fs.exists path with
  | Error io_error -> Error (ConfigFailed ("failed to read config '"
  ^ Path.to_string path
  ^ "': "
  ^ IO.error_message io_error))
  | Ok false -> Ok User_config.default
  | Ok true -> User_config.load path
  |> Result.map_err ~fn:(fun err -> ConfigFailed (User_config.message err))

let version_parse_error_to_string = function
  | Version.Invalid_format msg -> msg
  | Version.Invalid_version_segment segment -> "invalid version segment: " ^ segment
  | Version.Invalid_pre_release_segment segment -> "invalid pre-release segment: " ^ segment

let parse_package_spec = fun raw ->
  match String.split ~by:"@" (String.trim raw) with
  | [package_name;version] when not (String.equal (String.trim package_name) "")
  && not (String.equal (String.trim version) "") -> (
      match Package.validate_name (String.trim package_name) with
      | Error err -> Error (InvalidPackageName err)
      | Ok package_name -> (
          match Version.parse (String.trim version) with
          | Ok parsed_version -> Ok (package_name, Version.to_string parsed_version)
          | Error err -> Error (InvalidVersion (version_parse_error_to_string err))
        )
    )
  | [] ->
      Error MissingPackageSpec
  | _ ->
      Error (InvalidPackageSpec raw)

let parse_request = fun matches ->
  match ArgParser.get_one matches "package" with
  | None -> Error MissingPackageSpec
  | Some value -> parse_package_spec value

let prompt_confirmation = fun ~package_name ~version ->
  eprint ("Yank " ^ Package_name.to_string package_name ^ "@" ^ version ^ " from pkgs.ml? [y/N]: ");
  match Tty.make () with
  | Error Tty.NoTtyConnected ->
      Error (PromptFailed "no tty connected")
  | Error (Tty.SystemError io_error) ->
      Error (PromptFailed (IO.error_message io_error))
  | Ok tty -> (
      match Tty.read_line tty with
      | Error io_error ->
          let _ = Tty.restore tty in
          Error (PromptFailed (IO.error_message io_error))
      | Ok line ->
          let _ = Tty.restore tty in
          let normalized = String.lowercase_ascii (String.trim line) in
          if String.equal normalized "y" || String.equal normalized "yes" then
            Ok ()
          else
            Error Aborted
    )

let run = fun matches ->
  match parse_request matches with
  | Error err -> fail err
  | Ok (package_name, version) -> (
      let path = config_path () in
      match load_config path with
      | Error err -> fail err
      | Ok config -> (
          match User_config.api_token config ~registry_name with
          | None -> fail NotLoggedIn
          | Some api_token -> (
              match prompt_confirmation ~package_name ~version with
              | Error err -> fail err
              | Ok () -> (
                  match Pkgs_ml.Registry.create_filesystem ~registry_name () with
                  | Error err -> fail (YankFailed err)
                  | Ok registry -> (
                      match Pkgs_ml.Registry.yank_release
                        registry
                        ~api_token
                        ~package_name:(Package_name.to_string package_name)
                        ~version with
                      | Error err -> fail (YankFailed err)
                      | Ok _ ->
                          eprintln
                            ("Yanked " ^ Package_name.to_string package_name ^ "@" ^ version ^ " from pkgs.ml");
                          Ok ()
                    )
                )
            )
        )
    )
