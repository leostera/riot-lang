open Std

let print_env = fun name ->
  match Env.var Env.String ~name with
  | Some value -> println (name ^ "=" ^ value)
  | None -> println (name ^ " is not set")

let write_file = fun path content ->
  match Fs.write content path with
  | Ok () -> ()
  | Error error -> panic ("write failed: " ^ IO.error_message error)

let run_example = fun dir ->
  let base = Path.join dir (Path.v ".env") in
  let profile = Path.join dir (Path.v ".env.test") in
  write_file base "DOTENV_EXAMPLE_HOST=localhost
DOTENV_EXAMPLE_PORT=8080
";
  write_file profile "DOTENV_EXAMPLE_HOST=test.local
DOTENV_EXAMPLE_DEBUG=true
";
  match Dotenv.load ~path:base ~env:"test" ~on_existing:Dotenv.OverwriteExisting () with
  | Error error -> panic (Dotenv.error_to_string error)
  | Ok _applied ->
      print_env "DOTENV_EXAMPLE_HOST";
      print_env "DOTENV_EXAMPLE_PORT";
      print_env "DOTENV_EXAMPLE_DEBUG";
      Ok ()

let main ~args:_ =
  match Fs.with_tempdir ~prefix:"dotenv_profile_example_" run_example with
  | Error error -> panic ("tempdir failed: " ^ IO.error_message error)
  | Ok result -> result

let () = Runtime.run ~main ~args:Env.args ()
