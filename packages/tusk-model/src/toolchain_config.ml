open Std

type source = Version of string | Path of Path.t | Url of Net.Uri.t
type t = { version : string; source : source }

let default_ocaml_version = "5.3.0"

let default =
  { version = default_ocaml_version; source = Version default_ocaml_version }

let from_workspace workspace =
  let toolchain_file =
    Path.(workspace.Workspace.root / Path.v "ocaml-toolchain.toml")
  in
  match Fs.exists toolchain_file with
  | Ok false -> default
  | _ -> (
      match Fs.read_to_string toolchain_file with
      | Error _ -> default
      | Ok content -> (
          match Data.Toml.parse content with
          | Error _ -> default
          | Ok (Data.Toml.Table items) -> (
              match List.assoc_opt "toolchain" items with
              | Some (Data.Toml.Table toolchain_items) -> (
                  match List.assoc_opt "version" toolchain_items with
                  | Some (Data.Toml.String v) ->
                      { version = v; source = Version v }
                  | Some (Data.Toml.Table version_items) -> (
                      match List.assoc_opt "path" version_items with
                      | Some (Data.Toml.String path_str) ->
                          let source_path =
                            Path.of_string path_str |> Result.unwrap
                          in
                          let version_name =
                            format "%s-local" (Path.basename source_path)
                          in
                          { version = version_name; source = Path source_path }
                      | _ -> (
                          match List.assoc_opt "url" version_items with
                          | Some (Data.Toml.String url_str) ->
                              let version_name =
                                if String.contains url_str '/' then
                                  let parts =
                                    String.split_on_char '/' url_str
                                  in
                                  let last = List.hd (List.rev parts) in
                                  if String.contains last '.' then
                                    Path.v last |> Path.remove_extension
                                    |> Path.to_string
                                  else last
                                else "custom"
                              in
                              let uri =
                                Net.Uri.of_string url_str |> Result.unwrap
                              in
                              { version = version_name; source = Url uri }
                          | _ -> default))
                  | _ -> default)
              | _ -> default)
          | _ -> default))
