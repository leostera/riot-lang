open Std
module Test = Std.Test

let ( let* ) = Result.and_then

module Fixture = struct
  type file = string * string

  type t = {
    prefix: string;
    package_name: string;
    lib_path: string;
    files: file list;
  }

  let map_io = fun result -> Result.map_error IO.error_message result

  let path_error_message = function
    | Path.InvalidUtf8 { path } -> "invalid utf-8 path: " ^ path
    | Path.SystemInvalidUtf8 { syscall; path } ->
        syscall ^ " returned invalid utf-8 for path " ^ path
    | Path.SystemError msg -> msg

  let with_current_dir = fun dir fn ->
    let restore = ref None in
    let finalize = fun () ->
      match !restore with
      | None -> ()
      | Some original ->
          let _ = Env.set_current_dir original in
          ()
    in
    try
      match Env.current_dir () with
      | Error err ->
          Error ("failed to read current directory: " ^ path_error_message err)
      | Ok original ->
          restore := Some original;
          (
            match Env.set_current_dir dir with
            | Error err ->
                Error ("failed to enter fixture workspace: " ^ path_error_message err)
            | Ok () ->
                let result = fn () in
                finalize ();
                result
          )
    with
    | error ->
        finalize ();
        raise error

  let write_workspace_manifest = fun ~root ~member ->
    let source = String.concat "" [
      "[workspace]\n";
      "members = [\n";
      "  \"";
      member;
      "\"\n";
      "]\n";
    ] in
    Fs.write source Path.(root / Path.v "riot.toml") |> map_io

  let write_package_manifest = fun ~root ~name ~lib_path ->
    let source = String.concat "" [
      "[package]\n";
      "name = \"";
      name;
      "\"\n";
      "version = \"0.0.1\"\n\n";
      "[lib]\n";
      "path = \"";
      lib_path;
      "\"\n";
    ] in
    Fs.write source Path.(root / Path.v "riot.toml") |> map_io

  let write_file = fun ~root (relative_path, source) ->
    let path = Path.(root / Path.v relative_path) in
    let* () = Fs.create_dir_all (Path.dirname path) |> map_io in
    Fs.write source path |> map_io

  let rec write_tree = fun ~root -> function
    | [] -> Ok ()
    | file :: rest ->
        let* () = write_file ~root file in
        write_tree ~root rest

  let build = fun fixture ->
    match
      Fs.with_tempdir ~prefix:fixture.prefix
        (fun workspace_root ->
          let package_root = Path.(workspace_root / Path.v "pkg") in
          let* () = Fs.create_dir_all package_root |> map_io in
          let* () = write_workspace_manifest ~root:workspace_root ~member:"pkg" in
          let* () = write_package_manifest
            ~root:package_root
            ~name:fixture.package_name
            ~lib_path:fixture.lib_path in
          let* () = write_tree ~root:package_root fixture.files in
          with_current_dir workspace_root (fun () ->
            match
              Command.make
                "riot"
                ~env:[ ("RIOT_WORKSPACE_ROOT", Path.to_string workspace_root) ]
                ~args:[ "build"; fixture.package_name ]
              |> Command.output
            with
            | Error (Command.SystemError err) ->
                Error ("failed to spawn riot build: " ^ err)
            | Ok output when output.status = 0 ->
                Ok ()
            | Ok output ->
                Error (String.concat
                  "\n"
                  [
                    "riot build fixture failed with exit status " ^ Int.to_string output.status;
                    "stdout:";
                    output.stdout;
                    "stderr:";
                    output.stderr;
                  ])))
    with
    | Ok result -> result
    | Error err -> Error ("tempdir failed: " ^ IO.error_message err)
end

let nested_backend_workspace_fixture = Fixture.{
  prefix = "kernel_new_layout";
  package_name = "layout-smoke";
  lib_path = "src/layout_smoke.ml";
  files = [
    ("src/layout_smoke.ml", String.concat "\n" [
      "module Fs = Fs";
      "module Time = Time";
      "module Net = Net";
      "module Env = Env";
      "module Process = Process";
      "module Domains = Domains";
      "";
      "let file_answer = Fs.File.answer";
      "let system_time_answer = Time.SystemTime.answer";
      "let monotonic_answer = Time.Monotonic.answer";
      "let tcp_answer = Net.TcpStream.answer";
      "let env_answer = Env.answer";
      "let process_answer = Process.answer";
      "";
      "let user_level = Domains.Admin.Users.Models.Testing.User.level";
      "let report_level = Domains.Admin.Users.Models.Testing.Report.level";
      "";
    ]);
    ("src/fs/fs.ml", "module File = File\n");
    ("src/fs/file/file.ml", "include Unix\n");
    ("src/fs/file/unix.ml", "let answer = 1\n");
    ("src/time/time.ml", String.concat "\n" [
      "module SystemTime = System_time";
      "module Monotonic = Monotonic";
      "";
    ]);
    ("src/time/system_time/system_time.ml", "include Unix\n");
    ("src/time/system_time/unix.ml", "let answer = 2\n");
    ("src/time/monotonic/monotonic.ml", "include Unix\n");
    ("src/time/monotonic/unix.ml", "let answer = 3\n");
    ("src/net/net.ml", "module TcpStream = Tcp_stream\n");
    ("src/net/tcp_stream/tcp_stream.ml", "include Unix\n");
    ("src/net/tcp_stream/unix.ml", "let answer = Fs.File.answer\n");
    ("src/env/env.ml", "include Unix\n");
    ("src/env/unix.ml", "let answer = Net.TcpStream.answer\n");
    ("src/process/process.ml", "include Unix\n");
    ("src/process/unix.ml", "let answer = Env.answer\n");
    ("src/domains/domains.ml", "module Admin = Admin\n");
    ("src/domains/admin/admin.ml", "module Users = Users\n");
    ("src/domains/admin/shared.ml", "let level = \"admin\"\n");
    ("src/domains/admin/users/users.ml", "module Models = Models\n");
    ("src/domains/admin/users/models/models.ml", String.concat "\n" [
      "module Helpers = Helpers";
      "module Testing = Testing";
      "";
    ]);
    ("src/domains/admin/users/models/helpers.ml", "let level = \"models\"\n");
    ("src/domains/admin/users/models/testing/testing.ml", String.concat "\n" [
      "module Shared = Shared";
      "module User = User";
      "module Report = Report";
      "";
    ]);
    ("src/domains/admin/users/models/testing/shared.ml", "let level = \"testing\"\n");
    ("src/domains/admin/users/models/testing/user.ml", "let level = Shared.level\n");
    ("src/domains/admin/users/models/testing/report.ml", "let level = Helpers.level\n");
  ];
}

let test_build_fixture_exercises_nested_backends_and_deep_modules = fun _ctx ->
  Fixture.build nested_backend_workspace_fixture

let tests = [
  Test.case
    "kernel-new layout fixture builds nested backends and deep modules"
    test_build_fixture_exercises_nested_backends_and_deep_modules;
]

let main = fun ~args -> Test.Cli.main ~name:"kernel_new_layout_tests" ~tests ~args

let () = Actors.run ~main ~args:Env.args ()
