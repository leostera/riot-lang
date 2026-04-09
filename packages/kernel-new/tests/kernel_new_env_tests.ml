open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let lift result =
  match result with
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (Kernel.Env.error_to_string error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with
  | error ->
      finally ();
      raise error

let vars_contain = fun entries ~name ?value () ->
  Kernel.Array.fold_left
    (fun found (entry_name, entry_value) ->
      found || (
        Kernel.String.equal entry_name name && match value with
        | None -> true
        | Some value -> Kernel.String.equal entry_value value
      ))
    false
    entries

let test_args_include_program_name = fun _ctx ->
  if Kernel.Array.length Kernel.Env.args > 0 then
    Ok ()
  else
    Error "expected kernel env args to include at least the program name"

let test_set_get_and_remove_var_roundtrip = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_TEST" in
  let _ = Kernel.Env.remove_var ~name in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove_var ~name in
      ())
    (fun () ->
      let* () = lift (Kernel.Env.set_var ~name ~value:"kernel-new") in
      let value = Kernel.Env.get name in
      let found =
        Kernel.Array.fold_left
          (fun found (entry_name, entry_value) ->
            found
            || (Kernel.String.equal entry_name name && Kernel.String.equal entry_value "kernel-new"))
          false
          (Kernel.Env.vars ())
      in
      let* () = lift (Kernel.Env.remove_var ~name) in
      if value = Some "kernel-new" && found && Kernel.Env.get name = None then
        Ok ()
      else
        Error "expected kernel env variable roundtrip to preserve value and cleanup")

let test_missing_var_and_home_dir_behave_as_expected = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_MISSING" in
  let _ = Kernel.Env.remove_var ~name in
  let snapshot = Kernel.Env.vars () in
  let missing = Kernel.Env.get name = None && not (vars_contain snapshot ~name ()) in
  let home_matches =
    match (Kernel.Env.get "HOME", Kernel.Env.home_dir ()) with
    | (None, None) -> true
    | (Some home, Some path) -> Kernel.Path.to_string path = home
    | _ -> false
  in
  if missing && home_matches then
    Ok ()
  else
    Error "expected missing vars and home_dir to reflect the process environment"

let test_vars_snapshots_are_independent = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_SNAPSHOT" in
  let _ = Kernel.Env.remove_var ~name in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove_var ~name in
      ())
    (fun () ->
      let* () = lift (Kernel.Env.set_var ~name ~value:"before") in
      let before = Kernel.Env.vars () in
      let* () = lift (Kernel.Env.set_var ~name ~value:"after") in
      let after = Kernel.Env.vars () in
      if
        vars_contain before ~name ~value:"before" ()
        && not (vars_contain before ~name ~value:"after" ())
        && vars_contain after ~name ~value:"after" ()
      then
        Ok ()
      else
        Error "expected env snapshots to preserve the values visible at each call site")

let test_current_dir_roundtrips = fun _ctx ->
  let* original = lift (Kernel.Env.current_dir ()) in
  match
    Fs.with_tempdir ~prefix:"kernel_new_env"
      (fun tempdir ->
        let tempdir = Kernel.Path.of_string (Path.to_string tempdir) in
        protect
          ~finally:(fun () ->
            let _ = Kernel.Env.set_current_dir original in
            ())
          (fun () ->
            let* () = lift (Kernel.Env.set_current_dir tempdir) in
            let* current = lift (Kernel.Env.current_dir ()) in
            let current = Path.v (Kernel.Path.to_string current) in
            let tempdir = Path.v (Kernel.Path.to_string tempdir) in
            match (Fs.canonicalize current, Fs.canonicalize tempdir) with
            | (Ok current, Ok tempdir) when Path.to_string current = Path.to_string tempdir -> Ok ()
            | (Ok _, Ok _) -> Error "expected kernel current_dir to reflect the changed directory"
            | (Error err, _)
            | (_, Error err) -> Error (IO.error_message err)))
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_invalid_var_name_is_rejected = fun _ctx ->
  match (Kernel.Env.set_var ~name:"bad=name" ~value:"x", Kernel.Env.remove_var ~name:"") with
  | (Kernel.Result.Error (Kernel.Env.InvalidVarName { name="bad=name" }), Kernel.Result.Error (Kernel.Env.InvalidVarName {
    name=""
  })) -> Ok ()
  | (Kernel.Result.Error error, _) -> Error (Kernel.Env.error_to_string error)
  | (_, Kernel.Result.Error error) -> Error (Kernel.Env.error_to_string error)
  | _ -> Error "expected invalid env variable names to be rejected in kernel-new"

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix (fun tempdir -> fn (Kernel.Path.of_string (Path.to_string tempdir))) with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let test_set_var_accepts_equals_in_values = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_EQUALS_VALUE" in
  let _ = Kernel.Env.remove_var ~name in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove_var ~name in
      ())
    (fun () ->
      let* () = lift (Kernel.Env.set_var ~name ~value:"a=b=c") in
      match Kernel.Env.get name with
      | Some "a=b=c" -> Ok ()
      | Some _ -> Error "expected Env.set_var to preserve '=' characters in values"
      | None -> Error "expected Env.get to recover the stored value")

let test_remove_var_on_missing_name_is_harmless = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_REMOVE_MISSING" in
  let _ = Kernel.Env.remove_var ~name in
  let* () = lift (Kernel.Env.remove_var ~name) in
  if Kernel.Env.get name = None then
    Ok ()
  else
    Error "expected removing a missing environment variable to be harmless"

let test_set_var_overwrites_existing_value = fun _ctx ->
  let name = "RIOT_KERNEL_NEW_ENV_OVERWRITE" in
  let _ = Kernel.Env.remove_var ~name in
  protect
    ~finally:(fun () ->
      let _ = Kernel.Env.remove_var ~name in
      ())
    (fun () ->
      let* () = lift (Kernel.Env.set_var ~name ~value:"before") in
      let* () = lift (Kernel.Env.set_var ~name ~value:"after") in
      let snapshot = Kernel.Env.vars () in
      if Kernel.Env.get name = Some "after" && vars_contain snapshot ~name ~value:"after" () then
        Ok ()
      else
        Error "expected Env.set_var to overwrite the existing value")

let test_set_current_dir_missing_path_fails_cleanly = fun _ctx ->
  match Kernel.Env.set_current_dir "/definitely/missing/kernel-new-env-dir" with
  | Kernel.Result.Error (Kernel.Env.System Kernel.SystemError.NoSuchFileOrDirectory) -> Ok ()
  | Kernel.Result.Error error -> Error (Kernel.Env.error_to_string error)
  | Kernel.Result.Ok () -> Error "expected set_current_dir to reject missing paths"

let test_set_current_dir_regular_file_fails_cleanly = fun _ctx ->
  with_tempdir "kernel_new_env_edge"
    (fun root ->
      let path = Kernel.Path.(root / "plain.txt") in
      match
        Kernel.Fs.File.open_write path |> function
        | Kernel.Result.Error error -> Error (Kernel.Fs.File.error_to_string error)
        | Kernel.Result.Ok file ->
            protect
              ~finally:(fun () ->
                let _ = Kernel.Fs.File.close file in
                ())
              (fun () ->
                match Kernel.Env.set_current_dir path with
                | Kernel.Result.Error (Kernel.Env.System Kernel.SystemError.NotDirectory) -> Ok ()
                | Kernel.Result.Error error -> Error (Kernel.Env.error_to_string error)
                | Kernel.Result.Ok () -> Error "expected set_current_dir to reject regular files")
      with
      | Ok () -> Ok ()
      | Error error -> Error error)

let tests = [
  Test.case "Env.args includes the program name" test_args_include_program_name;
  Test.case "Env set_var, get, vars, and remove_var roundtrip" test_set_get_and_remove_var_roundtrip;
  Test.case "Env missing vars and home_dir reflect the process environment" test_missing_var_and_home_dir_behave_as_expected;
  Test.case "Env vars snapshots preserve each call result" test_vars_snapshots_are_independent;
  Test.case "Env current_dir and set_current_dir roundtrip" test_current_dir_roundtrips;
  Test.case "Env rejects invalid variable names" test_invalid_var_name_is_rejected;
  Test.case "Env.set_var accepts '=' in values" test_set_var_accepts_equals_in_values;
  Test.case "Env.remove_var on a missing name is harmless" test_remove_var_on_missing_name_is_harmless;
  Test.case "Env.set_var overwrites an existing value" test_set_var_overwrites_existing_value;
  Test.case "Env.set_current_dir rejects missing paths cleanly" test_set_current_dir_missing_path_fails_cleanly;
  Test.case "Env.set_current_dir rejects regular files cleanly" test_set_current_dir_regular_file_fails_cleanly;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_env_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
