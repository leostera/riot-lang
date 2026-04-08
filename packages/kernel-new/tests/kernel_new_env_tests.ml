open Std
module Test = Std.Test
module Kernel = Kernel_new

let ( let* ) = Result.and_then

let string_of_error = function
  | Kernel.Error.Unknown code ->
      "unknown kernel error " ^ Int.to_string code
  | error ->
      Kernel.Error.to_string error

let lift = function
  | Kernel.Result.Ok value -> Ok value
  | Kernel.Result.Error error -> Error (string_of_error error)

let protect = fun ~finally fn ->
  try
    let value = fn () in
    finally ();
    value
  with error ->
    finally ();
    raise error

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
      let* () =
        lift (Kernel.Env.set_var ~name ~value:"kernel-new")
      in
      let value = Kernel.Env.get name in
      let found =
        Kernel.Array.fold_left
          (fun found (entry_name, entry_value) ->
            found
            || (Kernel.String.equal entry_name name
                && Kernel.String.equal entry_value "kernel-new"))
          false
          (Kernel.Env.vars ())
      in
      let* () =
        lift (Kernel.Env.remove_var ~name)
      in
      if value = Some "kernel-new"
         && found
         && Kernel.Env.get name = None
      then
        Ok ()
      else
        Error "expected kernel env variable roundtrip to preserve value and cleanup")

let test_current_dir_roundtrips = fun _ctx ->
  let* original =
    lift (Kernel.Env.current_dir ())
  in
  match Fs.with_tempdir
          ~prefix:"kernel_new_env"
          (fun tempdir ->
            let tempdir = Kernel.Path.v (Path.to_string tempdir) in
            protect
              ~finally:(fun () ->
                let _ = Kernel.Env.set_current_dir original in
                ())
              (fun () ->
                let* () =
                  lift (Kernel.Env.set_current_dir tempdir)
                in
                let* current =
                  lift (Kernel.Env.current_dir ())
                in
                let current =
                  Path.v (Kernel.Path.to_string current)
                in
                let tempdir =
                  Path.v (Kernel.Path.to_string tempdir)
                in
                match (Fs.canonicalize current, Fs.canonicalize tempdir) with
                | (Ok current, Ok tempdir)
                  when Path.to_string current = Path.to_string tempdir ->
                    Ok ()
                | (Ok _, Ok _) ->
                    Error "expected kernel current_dir to reflect the changed directory"
                | (Error err, _)
                | (_, Error err) ->
                    Error (IO.error_message err)
                ))
  with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let tests = [
  Test.case "Env.args includes the program name" test_args_include_program_name;
  Test.case "Env set_var, get, vars, and remove_var roundtrip" test_set_get_and_remove_var_roundtrip;
  Test.case "Env current_dir and set_current_dir roundtrip" test_current_dir_roundtrips;
]

let main = fun ~args ->
  Test.Cli.main ~name:"kernel_new_env_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
