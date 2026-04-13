open Std
open Std.Data
open Std.Collections

type source =
  Version of string
  | Path of Path.t
  | Url of Net.Uri.t

type t = {
  version: string;
  source: source;
  targets: string list;  (* Target architectures for cross-compilation *)
}

let default_ocaml_version = "5.5.0-riot.2"

let default = {
  version = default_ocaml_version;
  source = Version default_ocaml_version;
  targets = []
}

let from_workspace = fun workspace ->
  let toolchain_file = Path.(workspace.Workspace.root / Path.v "ocaml-toolchain.toml") in
  match Fs.exists toolchain_file with
  | Ok false -> default
  | _ -> (
      match Fs.read_to_string toolchain_file with
      | Error _ -> default
      | Ok content -> (
          match Data.Toml.parse content with
          | Error _ ->
              default
          | Ok (Data.Toml.Table items) -> (
              match Fields.get "toolchain" items with
              | Some (Data.Toml.Table toolchain_items) -> (
                  (* Parse targets array *)
                  let targets =
                    match Fields.get "targets" toolchain_items with
                    | Some (Data.Toml.Array arr) ->
                        List.filter_map arr ~fn:(function
                          | Data.Toml.String s -> Some s
                          | _ -> None)
                    | _ -> []
                  in
                  match Fields.get "version" toolchain_items with
                  | Some (Data.Toml.String v) ->
                      { version = v; source = Version v; targets }
                  | Some (Data.Toml.Table version_items) -> (
                      match Fields.get "path" version_items with
                      | Some (Data.Toml.String path_str) -> (
                          match Path.from_string path_str with
                          | Ok source_path ->
                              let version_name = Path.basename source_path ^ "-local" in
                              { version = version_name; source = Path source_path; targets }
                          | Error _ -> default
                        )
                      | _ -> (
                          match Fields.get "url" version_items with
                          | Some (Data.Toml.String url_str) -> (
                              let version_name =
                                if String.contains url_str "/" then
                                  let parts = String.split ~by:"/" url_str in
                                  let last =
                                    match List.reverse parts with
                                    | head :: _ -> head
                                    | [] -> url_str
                                  in
                                  if String.contains last "." then
                                    Path.v last |> Path.remove_extension |> Path.to_string
                                  else
                                    last
                                else
                                  "custom"
                              in
                              match Net.Uri.of_string url_str with
                              | Ok uri -> { version = version_name; source = Url uri; targets }
                              | Error _ -> default
                            )
                          | _ -> default
                        )
                    )
                  | _ ->
                      default
                )
              | _ -> default
            )
          | _ ->
              default
        )
    )
