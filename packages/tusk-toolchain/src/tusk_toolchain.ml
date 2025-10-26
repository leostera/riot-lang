open Std

(** Toolchain bootstrapping and management

    This package provides an abstraction layer over OCaml compiler tooling.
    Currently shells out to ocamlc/ocamldep, but designed to support:
    - In-process RAML compiler calls via FFI
    - Multiple compiler backends
    - Easier testing and mocking *)

type source = Version of string | Path of Path.t | Url of Net.Uri.t

module Ocamldep = Ocamldep
module Ocamlc = Ocamlc
module Ocamlformat = Ocamlformat

type t = {
  version : string;
  source : source;
  ocamlc : Ocamlc.t;
  ocamlopt : Path.t;
  ocamldep : Ocamldep.t;
  ocamlformat : Ocamlformat.t;
}

let default_ocaml_version = "5.3.0"

let toolchain_base_dir =
  Path.(Tusk_model.Tusk_dirs.dot_tusk / Path.v "toolchains")

let get_toolchain_path version = Path.(toolchain_base_dir / Path.v version)

let make_toolchain version source =
  let toolchain_path = get_toolchain_path version in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in
  let bin_path bin = Path.(bin_dir / Path.v bin) in
  {
    version;
    source;
    ocamlc = Ocamlc.make (bin_path "ocamlopt.opt");
    ocamlopt = bin_path "ocamlopt.opt";
    ocamldep = Ocamldep.make (bin_path "ocamldep.opt");
    ocamlformat = Ocamlformat.make (bin_path "ocamlformat");
  }

let ocamlc t = t.ocamlc
let ocamlopt_path t = t.ocamlopt
let ocamldep t = t.ocamldep
let ocamlformat t = t.ocamlformat

let check_binaries_exist toolchain =
  let ocamlc_path = Ocamlc.path toolchain.ocamlc in
  let ocamldep_path = Ocamldep.path toolchain.ocamldep in
  match
    ( Fs.exists ocamlc_path,
      Fs.exists toolchain.ocamlopt,
      Fs.exists ocamldep_path )
  with
  | Ok true, Ok true, Ok true -> Ok ()
  | Ok false, _, _ ->
      Error (format "ocamlc not found at %s" (Path.to_string ocamlc_path))
  | _, Ok false, _ ->
      Error
        (format "ocamlopt not found at %s" (Path.to_string toolchain.ocamlopt))
  | _, _, Ok false ->
      Error (format "ocamldep not found at %s" (Path.to_string ocamldep_path))
  | Error (Fs.SystemError msg), _, _
  | _, Error (Fs.SystemError msg), _
  | _, _, Error (Fs.SystemError msg) ->
      Error (format "Failed to check binaries: %s" msg)

let init ~config =
  let version = config.Tusk_model.Toolchain_config.version in
  let source =
    match config.source with
    | Tusk_model.Toolchain_config.Version v -> Version v
    | Tusk_model.Toolchain_config.Path p -> Path p
    | Tusk_model.Toolchain_config.Url u -> Url u
  in
  let toolchain = make_toolchain version source in
  let toolchain_path = get_toolchain_path version in
  let bin_dir = Path.(toolchain_path / Path.v "bin") in

  (* Check if toolchain is already installed *)
  match Fs.is_dir bin_dir with
  | Ok true -> (
      match check_binaries_exist toolchain with
      | Ok () -> Ok toolchain
      | Error _ ->
          Error
            (format "Toolchain at %s is incomplete"
               (Path.to_string toolchain_path)))
  | _ -> (
      (* Try to use ./ocaml/compiler if it exists *)
      let local_compiler = Path.v "./ocaml/compiler" in
      match Fs.is_dir local_compiler with
      | Ok true -> (
          (* Create symlink from ~/.tusk/toolchains/5.3.0 to ./ocaml/compiler *)
          (match Path.parent toolchain_path with
          | Some parent ->
              let _ = Fs.create_dir_all parent in
              ()
          | None -> ());

          (* Check if symlink already exists *)
          match Fs.exists toolchain_path with
          | Ok true -> Ok toolchain
          | _ -> (
              (* Get absolute path for local_compiler *)
              let cwd =
                Env.current_dir () |> Result.expect ~msg:"Failed to get cwd"
              in
              let abs_local =
                if Path.is_absolute local_compiler then local_compiler
                else Path.(cwd / local_compiler)
              in
              match Fs.symlink ~src:abs_local ~dst:toolchain_path with
              | Ok () -> Ok toolchain
              | Error (Fs.SystemError msg) ->
                  Error
                    (format
                       "Failed to create toolchain symlink from %s to %s: %s"
                       (Path.to_string toolchain_path)
                       (Path.to_string abs_local) msg)))
      | _ ->
          Error
            (format
               "Toolchain not found at %s and ./ocaml/compiler doesn't exist.\n\n\
                To bootstrap the toolchain, run:\n\
               \  ./ocaml/build-compiler.sh"
               (Path.to_string toolchain_path)))

let ensure_default_toolchain () =
  let default_config = Tusk_model.Toolchain_config.default in
  match init ~config:default_config with
  | Ok _ -> Ok ()
  | Error msg -> Error msg

let check_health toolchain =
  match check_binaries_exist toolchain with
  | Error msg -> Error msg
  | Ok () -> (
      (* Try to execute ocamlc -version to verify it works *)
      let ocamlc_path = Ocamlc.path toolchain.ocamlc in
      let cmd =
        Command.make ~args:[ "-version" ] (Path.to_string ocamlc_path)
      in
      match Command.output cmd with
      | Ok output when output.Command.status = 0 ->
          Log.debug "Toolchain healthy: ocamlc version = %s"
            (String.trim output.Command.stdout);
          Ok ()
      | Ok output ->
          Error
            (format "ocamlc exists but failed: exit code %d"
               output.Command.status)
      | Error (Command.SystemError msg) ->
          Error (format "ocamlc health check failed: %s" msg))

let hash t =
  let hasher = Crypto.Sha256.create () in
  Crypto.Sha256.write_string hasher t.version;
  Crypto.Sha256.write_string hasher (Path.to_string (Ocamlc.path t.ocamlc));
  Crypto.Sha256.write_string hasher (Path.to_string t.ocamlopt);
  Crypto.Sha256.write_string hasher (Path.to_string (Ocamldep.path t.ocamldep));
  Crypto.Sha256.write_string hasher
    (Path.to_string (Ocamlformat.path t.ocamlformat));
  Crypto.Sha256.finish hasher
