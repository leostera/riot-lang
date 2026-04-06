open Std
module Test = Std.Test

let ( let* ) = Result.and_then

let with_tempdir = fun prefix fn ->
  match Fs.with_tempdir ~prefix fn with
  | Ok result -> result
  | Error err -> Error (IO.error_message err)

let repo_root = fun () -> Env.current_dir () |> Result.expect ~msg:"failed to get cwd"

let riot_binary = fun () -> Path.(repo_root () / Path.v "riot") |> Path.to_string

let write_source_file = fun ~root ~relative_path ~source ->
  let path = Path.(root / Path.v relative_path) in
  Fs.create_dir_all (Path.dirname path) |> Result.expect ~msg:"failed to create temp source dir";
  Fs.write source path |> Result.expect ~msg:"failed to write temp source";
  path

let run_fix_apply = fun path ->
  let root = repo_root () |> Path.to_string in
  let cmd = Command.make (riot_binary ()) ~cwd:root ~args:[ "fix"; "--apply"; Path.to_string path ] in
  Command.output cmd |> Result.map_error
    (
      function
      | Command.SystemError message -> message
    )

let assert_fix_rewrite = fun ~relative_path ~source ~expected ->
  with_tempdir "kernel_format_fix"
    (fun tmpdir ->
      let path = write_source_file ~root:tmpdir ~relative_path ~source in
      let* output = run_fix_apply path in
      if not (Int.equal output.status 0) then
        Error (format
          Format.[
            str "expected riot fix to exit 0, got ";
            int output.status;
            str "\nstdout:\n";
            str output.stdout;
            str "\nstderr:\n";
            str output.stderr;
          ])
      else
        let* actual = Fs.read path |> Result.map_error IO.error_message in
        if String.equal expected actual then
          Ok ()
        else
          Error (format Format.[ str "expected:\n"; str expected; str "\nactual:\n"; str actual; ]))

let test_fix_uses_local_format_in_std_files = fun _ctx ->
  assert_fix_rewrite ~relative_path:"packages/std/tests/sample_format_concat_tests.ml"
    ~source:{|
open Std

let render = fun name count ->
  "Hello, " ^ name ^ " #" ^ string_of_int count
|}
    ~expected:{|
open Std

let render = fun name count -> format Format.[ str "Hello, "; str (name); str " #"; int (count) ]
|}

let test_fix_uses_format_module_inside_kernel = fun _ctx ->
  assert_fix_rewrite ~relative_path:"packages/kernel/tests/sample_format_concat_tests.ml"
    ~source:{|
open Global0

let render = fun name count ->
  "Hello, " ^ name ^ " #" ^ string_of_int count
|}
    ~expected:{|
open Global0

let render = fun name count -> Format.format Format.[ str "Hello, "; str (name); str " #"; int (count) ]
|}

let test_fix_falls_back_to_kernel_qualified_format = fun _ctx ->
  assert_fix_rewrite ~relative_path:"packages/riot-build/tests/sample_format_concat_tests.ml"
    ~source:{|
let render = fun name count ->
  "Hello, " ^ name ^ " #" ^ string_of_int count
|}
    ~expected:{|
let render = fun name count -> Kernel.format Kernel.Format.[ str "Hello, "; str (name); str " #"; int (count) ]
|}

let tests = [
  Test.case "format autofix uses local format shorthand in std files" test_fix_uses_local_format_in_std_files;
  Test.case "format autofix uses Format.format inside kernel sources" test_fix_uses_format_module_inside_kernel;
  Test.case "format autofix falls back to Kernel-qualified calls when imports are absent" test_fix_falls_back_to_kernel_qualified_format;
]

let main = fun ~args -> Test.Cli.main ~name:"format_fix_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
