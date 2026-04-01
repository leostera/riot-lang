open Std
module Test = Std.Test

let parse_publish = fun args ->
  match ArgParser.get_matches Tusk_cli.Publish.command args with
  | Ok matches -> Ok matches
  | Error err -> Error (ArgParser.error_message err)

let make_package = fun ?(workspace_member = true) ?(dependencies = []) name ->
  Tusk_model.Package.{
    name;
    path = Path.v ("/workspace/packages/" ^ name);
    relative_path =
      if workspace_member then
        Path.v ("packages/" ^ name)
      else
        Path.v ("../external/" ^ name);
    dependencies;
    dev_dependencies = [];
    build_dependencies = [];
    foreign_dependencies = [];
    binaries = [];
    library = None;
    sources = { src = []; native = []; tests = []; examples = []; bench = [] };
    compiler = { profile_overrides = []; target_overrides = [] };
    commands = [];
    fix_providers = [];
  }

let make_workspace = fun packages ->
  Tusk_model.Workspace.make ~root:(Path.v "/workspace") ~packages ()

let local_dep = fun ?(workspace = false) ?path name ->
  Tusk_model.Package.{ name; source = { workspace; builtin = false; path; version = None } }

let test_publish_accepts_package_option = fun () ->
  match parse_publish [ "publish"; "-p"; "demo" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      Test.assert_equal ~expected:(Some "demo") ~actual:(ArgParser.get_one matches "package");
      Ok ()

let test_publish_accepts_workspace_flag = fun () ->
  match parse_publish [ "publish"; "--workspace" ] with
  | Error err -> Error ("expected publish args to parse: " ^ err)
  | Ok matches ->
      if ArgParser.get_flag matches "workspace" then
        Ok ()
      else
        Error "expected --workspace flag to be parsed"

let test_publish_conflicting_selection_fails = fun () ->
  match Tusk_cli.Publish.resolve_request ~package_name:(Some "demo") ~workspace_mode:true with
  | Error Tusk_cli.Publish.ConflictingSelection -> Ok ()
  | Ok _ -> Error "expected conflicting publish selection to fail"
  | Error err -> Error ("unexpected publish selection error: " ^ Tusk_cli.Publish.message err)

let test_publish_select_packages_orders_workspace_dependencies = fun () ->
  let core = make_package "core" in
  let app =
    make_package
      "app"
      ~dependencies:[ local_dep ~workspace:true "core" ]
  in
  let workspace = make_workspace [ app; core ] in
  match Tusk_cli.Publish.select_packages ~workspace Tusk_cli.Publish.Workspace with
  | Error err -> Error ("expected workspace publish selection to succeed: " ^ Tusk_cli.Publish.message err)
  | Ok packages ->
      Test.assert_equal ~expected:[ "core"; "app" ] ~actual:(List.map (fun (pkg: Tusk_model.Package.t) -> pkg.name) packages);
      Ok ()

let test_publish_selects_single_workspace_package = fun () ->
  let workspace = make_workspace [ make_package "app"; make_package "core" ] in
  match Tusk_cli.Publish.select_packages ~workspace (Tusk_cli.Publish.Package "core") with
  | Error err -> Error ("expected single package publish selection to succeed: " ^ Tusk_cli.Publish.message err)
  | Ok [ pkg ] when String.equal pkg.name "core" -> Ok ()
  | Ok _ -> Error "expected only the selected package to be returned"

let tests =
  Test.[
    case "publish: parse -p option" test_publish_accepts_package_option;
    case "publish: parse --workspace flag" test_publish_accepts_workspace_flag;
    case "publish: conflicting selection fails" test_publish_conflicting_selection_fails;
    case "publish: workspace selection orders runtime deps" test_publish_select_packages_orders_workspace_dependencies;
    case "publish: package selection finds a workspace package" test_publish_selects_single_workspace_package;
  ]

let name = "Tusk CLI Publish Tests"

let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args ()
