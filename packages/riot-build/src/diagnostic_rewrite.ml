open Std

let workspace_relative_path = fun (package: Riot_model.Package.t) rel ->
  let package_rel = Path.to_string package.relative_path in
  if String.equal package_rel "." || String.equal package_rel "" then
    "./" ^ Path.to_string rel
  else "./" ^ Path.to_string Path.(package.relative_path / rel)

let rewrite_path = fun ~(package:Riot_model.Package.t) ~sandbox_dir path_str ->
  let sandbox_dir = Path.normalize sandbox_dir in
  let path = Path.normalize (Path.v path_str) in
  match Path.strip_prefix path ~prefix:sandbox_dir with
  | Error _ -> None
  | Ok rel ->
      let actual = Path.(package.path / rel) in
      match Fs.exists actual with
      | Ok true ->
          if Riot_model.Package.is_workspace_member package then
            Some (workspace_relative_path package rel)
          else Some (Path.to_string actual)
      | Ok false | Error _ -> None

let rewrite_diagnostics = fun ~package ~sandbox_dir diagnostics -> List.map ~fn:(Riot_toolchain.Ocamlc.Diagnostic.map_path (rewrite_path ~package ~sandbox_dir)) diagnostics

let rewrite_ocamlc_result = fun ~package ~sandbox_dir result ->
  match result with
  | Riot_toolchain.Ocamlc.Success success -> Riot_toolchain.Ocamlc.Success ({ success with diagnostics = rewrite_diagnostics ~package ~sandbox_dir success.diagnostics })
  | Riot_toolchain.Ocamlc.Failed failure -> Riot_toolchain.Ocamlc.Failed ({ failure with diagnostics = rewrite_diagnostics ~package ~sandbox_dir failure.diagnostics })
