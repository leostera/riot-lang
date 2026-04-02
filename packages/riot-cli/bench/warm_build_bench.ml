open Std

let bench_config : Bench.bench_config = {
  iterations = 20;
  warmup = 3;
}

let current_dir = fun () -> Env.current_dir () |> Result.expect ~msg:"Failed to get current directory"

let set_current_dir = fun path ->
  Env.set_current_dir path
  |> Result.expect ~msg:("Failed to change directory to " ^ Path.to_string path)

let with_current_dir = fun path fn ->
  let original = current_dir () in
  set_current_dir path;
  try
    let result = fn () in
    set_current_dir original;
    result
  with
  | exn ->
      set_current_dir original;
      raise exn

let write_file = fun path contents ->
  let parent =
    match Path.parent path with
    | Some parent -> parent
    | None -> Path.v "."
  in
  Fs.create_dir_all parent |> Result.expect ~msg:"expected parent directory to be created";
  Fs.write contents path |> Result.expect ~msg:"expected file to be written"

type fixture = {
  workspace_root: Path.t;
}

let prepare_fixture = fun root ->
  let workspace_root = Path.(root / Path.v "workspace") in
  let package_root = Path.(workspace_root / Path.v "packages" / Path.v "app") in
  write_file
    Path.(workspace_root / Path.v "riot.toml")
    {|
[workspace]
members = [
  "packages/app",
]
|};
  write_file
    Path.(package_root / Path.v "riot.toml")
    {|
[package]
name = "app"
version = "0.0.1"

[lib]
path = "src/app.ml"
|};
  write_file
    Path.(package_root / Path.v "src" / Path.v "app.ml")
    {|
let answer = 42
|};
  with_current_dir
    workspace_root
    (fun () ->
      match Riot_cli.Cli.main ~args:[ "riot"; "build" ] with
      | Ok () -> ()
      | Error exn -> panic ("failed to warm build benchmark fixture: " ^ Exception.to_string exn));
  { workspace_root }

let bench_cli_build_warm = fun (fixture:fixture) () ->
  with_current_dir
    fixture.workspace_root
    (fun () ->
      match Riot_cli.Cli.run ~args:[ "riot"; "build" ] with
      | Ok () -> ()
      | Error exn -> panic ("warm riot build bench failed: " ^ Exception.to_string exn))

let benchmark_suite = fun fixture ->
  Bench.[
    with_config
      ~config:bench_config
      "riot build warm local workspace"
      (bench_cli_build_warm fixture);
  ]

let () =
  Actors.run
    ~main:(fun ~args ->
      Riot_cli.Cli.initialize_runtime ();
      match Fs.with_tempdir ~prefix:"riot_cli_bench" (fun root ->
        let fixture = prepare_fixture root in
        Bench.Cli.main
          ~name:"riot-cli warm build path"
          ~benchmarks:(benchmark_suite fixture)
          ~args) with
      | Ok result -> result
      | Error err -> panic ("failed to prepare warm build benchmark: " ^ IO.error_message err))
    ~args:Env.args
    ()
