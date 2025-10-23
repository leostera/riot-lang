open Std
open Miniriot

let test_init_toolchain () =
  let config = Tusk_model.Toolchain_config.default in
  match Tusk_toolchain.init ~config with
  | Ok _toolchain -> Ok ()
  | Error msg -> Error (format "Failed to init toolchain: %s" msg)

let test_check_toolchain_health () =
  let config = Tusk_model.Toolchain_config.default in
  match Tusk_toolchain.init ~config with
  | Error msg -> Error (format "Setup failed: %s" msg)
  | Ok toolchain -> (
      (* Check health *)
      match Tusk_toolchain.check_health toolchain with
      | Ok () -> Ok ()
      | Error msg -> Error (format "Health check failed: %s" msg))

let test_toolchain_binaries_exist () =
  let config = Tusk_model.Toolchain_config.default in
  match Tusk_toolchain.init ~config with
  | Error msg -> Error (format "Setup failed: %s" msg)
  | Ok toolchain -> (
      let ocamlc =
        Tusk_toolchain.Ocamlc.path (Tusk_toolchain.ocamlc toolchain)
      in
      let ocamlopt = Tusk_toolchain.ocamlopt_path toolchain in
      let ocamldep =
        Tusk_toolchain.Ocamldep.path (Tusk_toolchain.ocamldep toolchain)
      in

      match (Fs.exists ocamlc, Fs.exists ocamlopt, Fs.exists ocamldep) with
      | Ok true, Ok true, Ok true -> Ok ()
      | Ok false, _, _ ->
          Error (format "ocamlc not found at %s" (Path.to_string ocamlc))
      | _, Ok false, _ ->
          Error (format "ocamlopt not found at %s" (Path.to_string ocamlopt))
      | _, _, Ok false ->
          Error (format "ocamldep not found at %s" (Path.to_string ocamldep))
      | Error (Fs.SystemError msg), _, _
      | _, Error (Fs.SystemError msg), _
      | _, _, Error (Fs.SystemError msg) ->
          Error (format "Failed to check binaries: %s" msg))

let name = "Toolchain Tests"

let tests =
  Test.
    [
      case "init toolchain" test_init_toolchain;
      case "check toolchain health" test_check_toolchain_health;
      case "toolchain binaries exist" test_toolchain_binaries_exist;
    ]

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
