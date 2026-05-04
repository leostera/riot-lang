open Std

module Test = Std.Test
module Build_context = Riot_build.Internal.Build_context
module Build_lane = Riot_build.Internal.Build_lane
module Build_unit = Riot_planner.Build_unit
module Build_unit_plan = Riot_build.Internal.Build_unit_plan
module Build_work = Riot_build.Internal.Build_work
module Resolved_build = Riot_build.Internal.Resolved_build
module Package_graph = Riot_planner.Package_graph

let package_name = fun name ->
  Riot_model.Package_name.from_string name
  |> Result.expect ~msg:("invalid package name: " ^ name)

let package_dependency = fun name ->
  Riot_model.Package.{
    name = package_name name;
    source =
      {
        workspace = true;
        builtin = false;
        path = None;
        source_locator = None;
        ref_ = None;
        version = None;
      };
  }

let write_workspace_manifest = fun ~root ~members ->
  let members =
    members
    |> List.map ~fn:(fun member -> "  \"" ^ Path.to_string member ^ "\"")
    |> String.concat ",\n"
  in
  let content = "[workspace]\nmembers = [\n" ^ members ^ "\n]\n" in
  Fs.write content Path.(root / Path.v "riot.toml")
  |> Result.expect ~msg:"write workspace riot.toml failed"

let make_package = fun ~root ~name ~source ?(dependencies = []) () ->
  let pkg_dir = Path.(root / Path.v name) in
  let src_dir = Path.(pkg_dir / Path.v "src") in
  let pkg_name = package_name name in
  Fs.create_dir_all src_dir
  |> Result.expect ~msg:"create src failed";
  Fs.write source Path.(src_dir / Path.v "lib.ml")
  |> Result.expect ~msg:"write source failed";
  Fs.write
    ("[package]\nname = \"" ^ name ^ "\"\nversion = \"0.0.1\"\n\n[lib]\npath = \"src/lib.ml\"\n")
    Path.(pkg_dir / Path.v "riot.toml")
  |> Result.expect ~msg:"write riot.toml failed";
  Riot_model.Package.make
    ~name:pkg_name
    ~path:pkg_dir
    ~relative_path:(Path.v name)
    ~dependencies
    ~library:{ path = Path.v "src/lib.ml" }
    ~sources:{
      src = [ Path.v "src/lib.ml" ];
      native = [];
      tests = [];
      examples = [];
      bench = [];
    }
    ()

let make_workspace = fun ~root ~packages ->
  write_workspace_manifest
    ~root
    ~members:(List.map packages ~fn:(fun (pkg: Riot_model.Package.t) -> pkg.relative_path));
  Riot_model.Workspace.make_realized ~root ~packages ()

let make_request = fun ~workspace ~packages ->
  Riot_build.Request.make
    ~workspace
    ~packages
    ~targets:Riot_model.Target.Host
    ~scope:Riot_build.Request.Runtime
    ~profile:Riot_model.Profile.debug
    ()

let with_prepared_lanes = fun request fn ->
  let context =
    Build_context.make request
    |> Result.expect ~msg:"expected build context creation to succeed"
  in
  let resolved =
    Resolved_build.resolve context request
    |> Result.expect ~msg:"expected build resolution to succeed"
  in
  let toolchain =
    Riot_toolchain.init ~config:context.toolchain_config
    |> Result.expect ~msg:"expected toolchain initialization to succeed"
  in
  let lanes =
    Build_work.prepare_lanes context resolved ~toolchain
    |> Result.expect ~msg:"expected lane preparation to succeed"
  in
  fn lanes

let package_key_string = fun key -> Riot_model.Package.key_to_string key

let initial_plan_package_keys = fun lane ->
  Build_work.initial_plan_packages lane
  |> List.map
    ~fn:(fun planned ->
      Build_work.plan_package_key planned
      |> package_key_string)

let initial_plan_targets = fun lane ->
  Build_work.initial_plan_packages lane
  |> List.map ~fn:Build_work.plan_package_target

let build_unit_key_strings = fun lane ->
  Build_lane.build_unit_plan lane
  |> Build_unit_plan.units
  |> List.map ~fn:(fun unit -> Build_unit.key_to_string unit.Build_unit.key)

let test_initial_plan_packages_follow_topological_order = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_prepare_topological"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] in
      with_prepared_lanes
        (make_request ~workspace ~packages:[ package_name "app" ])
        (fun lanes ->
          match lanes with
          | [ lane ] ->
              let planned_keys = initial_plan_package_keys lane in
              let expected_keys = [
                Package_graph.package_key ~package_name:"lib" Package_graph.Runtime
                |> package_key_string;
                Package_graph.package_key ~package_name:"app" Package_graph.Runtime
                |> package_key_string;
              ]
              in
              let host_target = Riot_model.Target.current in
              let all_targets_match =
                initial_plan_targets lane
                |> List.all ~fn:(fun target -> Riot_model.Target.equal host_target target)
              in
              if not all_targets_match then
                Error "expected initial planned package work to stay on the host target"
              else (
                Test.assert_equal ~expected:expected_keys ~actual:planned_keys;
                Ok ()
              )
          | items ->
              Error ("expected one initial host lane, got " ^ Int.to_string (List.length items)))) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_initial_plan_packages_include_all_workspace_packages_when_unfiltered = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_prepare_unfiltered"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let util = make_package ~root:tmpdir ~name:"util" ~source:"let extra = 2\n" () in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app; util ] in
      with_prepared_lanes
        (make_request ~workspace ~packages:[])
        (fun lanes ->
          match lanes with
          | [ lane ] ->
              let actual_keys =
                initial_plan_package_keys lane
                |> List.sort ~compare:String.compare
              in
              let expected_keys =
                [
                  Package_graph.package_key ~package_name:"app" Package_graph.Runtime
                  |> package_key_string;
                  Package_graph.package_key ~package_name:"lib" Package_graph.Runtime
                  |> package_key_string;
                  Package_graph.package_key ~package_name:"util" Package_graph.Runtime
                  |> package_key_string;
                ]
                |> List.sort ~compare:String.compare
              in
              Test.assert_equal ~expected:expected_keys ~actual:actual_keys;
              Ok ()
          | items ->
              Error ("expected one initial host lane, got " ^ Int.to_string (List.length items)))) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let test_prepared_lanes_carry_build_unit_plan = fun _ctx ->
  match Fs.with_tempdir
    ~prefix:"riot_build_prepare_build_units"
    (fun tmpdir ->
      let lib = make_package ~root:tmpdir ~name:"lib" ~source:"let value = 1\n" () in
      let app =
        make_package
          ~root:tmpdir
          ~name:"app"
          ~dependencies:[ package_dependency "lib" ]
          ~source:"let value = Lib.value\n"
          ()
      in
      let workspace = make_workspace ~root:tmpdir ~packages:[ lib; app ] in
      with_prepared_lanes
        (make_request ~workspace ~packages:[ package_name "app" ])
        (fun lanes ->
          match lanes with
          | [ lane ] ->
              let keys = build_unit_key_strings lane in
              Test.assert_true
                (List.any keys ~fn:(fun key -> String.starts_with ~prefix:"lib:library:" key));
              Test.assert_true
                (List.any keys ~fn:(fun key -> String.starts_with ~prefix:"app:library:" key));
              Ok ()
          | items ->
              Error ("expected one initial host lane, got " ^ Int.to_string (List.length items)))) with
  | Ok result -> result
  | Error err -> Error ("tempdir failed: " ^ IO.error_message err)

let tests = let open Test in
[
  case
    "build work prepare: initial plan packages preserve dependency-first order"
    test_initial_plan_packages_follow_topological_order;
  case
    "build work prepare: initial plan packages include all workspace packages when unfiltered"
    test_initial_plan_packages_include_all_workspace_packages_when_unfiltered;
  case
    "build work prepare: prepared lanes carry build unit plan"
    test_prepared_lanes_carry_build_unit_plan;
]

let name = "Riot Build Work Prepare Tests"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
