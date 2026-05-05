open Std
open Riot_model

module Test = Std.Test
module G = Std.Graph.SimpleGraph

let package_name = fun value ->
  Package_name.from_string value
  |> Result.expect ~msg:("expected valid package name: " ^ value)

let make_package = fun ?library ?(binaries = []) name ->
  Package.make
    ~name:(package_name name)
    ~path:(Path.v ".")
    ~relative_path:(Path.v ".")
    ?library
    ~binaries:(List.map
      binaries
      ~fn:(fun (binary_name, path) -> Package.{ name = binary_name; path = Path.v path }))
    ()

let public_namespace = fun package -> Namespace.from_list [ Package.root_module_name package ]

let make_module = fun ~namespace path -> Module.make ~namespace ~filename:(Path.v path)

let add_ml_node = fun graph ~namespace ~path ->
  G.add_node
    graph
    (Riot_planner.Module_node.make_ml
      (make_module ~namespace path)
      (Riot_planner.Module_node.Concrete (Path.v path)))

let add_generated_ml_node = fun graph ~namespace ~path ~contents ->
  G.add_node
    graph
    (Riot_planner.Module_node.make_ml
      (make_module ~namespace path)
      (Riot_planner.Module_node.Generated { path = Path.v path; contents }))

let add_binary_target = fun graph ~name ~source ->
  G.add_node
    graph
    (Riot_planner.Module_node.make_binary ~name ~source:(Path.v source) ~libraries:[] ~includes:[])

let add_dep = fun node ~depends_on -> G.add_edge node ~depends_on

let node_path = fun (node: Riot_planner.Module_node.t G.node) ->
  match (G.value node).file with
  | Riot_planner.Module_node.Concrete path -> path
  | Riot_planner.Module_node.Generated { path; _ } -> path

let source_slice = fun source ->
  IO.IoVec.IoSlice.from_string source
  |> Result.expect ~msg:"failed to create package-layout test source slice"

let analyzed_module = fun (node: Riot_planner.Module_node.t G.node) ~source ->
  let display_path = node_path node in
  let parse_result = Syn.parse ~filename:display_path (source_slice source) in
  let deps = Syn.Deps.from_parse_result parse_result in
  (
    (G.id node),
    Riot_planner.Module_graph.{
      display_path;
      source_hash = Crypto.hash_string source;
      implicit_opens = [];
      parse_result;
      deps;
      resolved_deps = [];
      resolved_dep_ids = (G.deps node);
      unresolved_deps = [];
    }
  )

let validate_layout = fun ~package ~graph ~analyzed ->
  Riot_planner.Package_layout_validator.validate
    ~direct_dependency_modules:[]
    ~package
    ~module_graph:graph
    ~analyzed_modules:(List.map analyzed ~fn:(fun (node, source) -> analyzed_module node ~source))

let test_undeclared_package_module_suggests_available_module_name = fun _ctx ->
  let package = make_package ~library:{ path = Path.v "src/typ.ml" } "typ" in
  let graph = G.make () in
  let _root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/typ.ml" in
  let _surface_path =
    add_ml_node graph ~namespace:(public_namespace package) ~path:"src/model/surface_path.ml"
  in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let (node_id, analyzed) = analyzed_module main ~source:"let _ = SurfacePath.empty\n" in
  let analyzed = { analyzed with Riot_planner.Module_graph.unresolved_deps = [ "SurfacePath" ] } in
  match Riot_planner.Package_layout_validator.validate
    ~direct_dependency_modules:[]
    ~package
    ~module_graph:graph
    ~analyzed_modules:[ (node_id, analyzed) ] with
  | Error (
    Riot_planner.Planning_error.SourceDependsOnUndeclaredPackageModule {
      requested_module;
      suggested_modules;
      _;
    }
  ) ->
      Test.assert_equal ~expected:"SurfacePath" ~actual:requested_module;
      Test.assert_equal ~expected:[ "Surface_path" ] ~actual:suggested_modules;
      Ok ()
  | Error err ->
      Error ("expected undeclared package module planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected misspelled module dependency to fail"

let assert_target_can_use_public_root_module = fun
  ~package_name ~target_name ~target_source_path ~target_source ->
  let package =
    make_package
      ~library:{ path = Path.v ("src/" ^ package_name ^ ".ml") }
      ~binaries:[ (target_name, target_source_path); ]
      package_name
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:("src/" ^ package_name ^ ".ml") in
  let child = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let target = add_ml_node graph ~namespace:(public_namespace package) ~path:target_source_path in
  let _binary = add_binary_target graph ~name:target_name ~source:target_source_path in
  let () = add_dep root ~depends_on:child in
  let () = add_dep target ~depends_on:root in
  match validate_layout ~package ~graph ~analyzed:[ (target, target_source); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected target to use public root, got: " ^ Riot_planner.Planning_error.to_string err)

let assert_target_cannot_use_internal_library_module_directly = fun
  ~package_name ~target_name ~target_source_path ~target_source ->
  let package =
    make_package
      ~library:{ path = Path.v ("src/" ^ package_name ^ ".ml") }
      ~binaries:[ (target_name, target_source_path); ]
      package_name
  in
  let public_module = Package.root_module_name package in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:("src/" ^ package_name ^ ".ml") in
  let child = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let target = add_ml_node graph ~namespace:(public_namespace package) ~path:target_source_path in
  let _binary = add_binary_target graph ~name:target_name ~source:target_source_path in
  let () = add_dep root ~depends_on:child in
  let () = add_dep target ~depends_on:child in
  match validate_layout ~package ~graph ~analyzed:[ (target, target_source); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
      target_name = actual_target_name;
      source;
      requested_module;
      internal_module;
      public_module = actual_public_module;
    }
  ) ->
      Test.assert_equal ~expected:target_name ~actual:actual_target_name;
      Test.assert_equal ~expected:(Path.v target_source_path) ~actual:source;
      Test.assert_equal ~expected:"A" ~actual:requested_module;
      Test.assert_equal ~expected:(public_module ^ "__A") ~actual:internal_module;
      Test.assert_equal ~expected:public_module ~actual:actual_public_module;
      Ok ()
  | Error err ->
      Error ("expected internal-library-module planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected direct access to internal library module to fail"

let assert_target_cannot_use_namespaced_internal_library_module = fun
  ~package_name ~target_name ~target_source_path ~target_source ->
  let package =
    make_package
      ~library:{ path = Path.v ("src/" ^ package_name ^ ".ml") }
      ~binaries:[ (target_name, target_source_path); ]
      package_name
  in
  let public_module = Package.root_module_name package in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:("src/" ^ package_name ^ ".ml") in
  let child = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let target = add_ml_node graph ~namespace:(public_namespace package) ~path:target_source_path in
  let _binary = add_binary_target graph ~name:target_name ~source:target_source_path in
  let () = add_dep root ~depends_on:child in
  let () = add_dep target ~depends_on:child in
  match validate_layout ~package ~graph ~analyzed:[ (target, target_source); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
      target_name = actual_target_name;
      source;
      requested_module;
      internal_module;
      public_module = actual_public_module;
    }
  ) ->
      Test.assert_equal ~expected:target_name ~actual:actual_target_name;
      Test.assert_equal ~expected:(Path.v target_source_path) ~actual:source;
      Test.assert_equal ~expected:(public_module ^ "__A") ~actual:requested_module;
      Test.assert_equal ~expected:(public_module ^ "__A") ~actual:internal_module;
      Test.assert_equal ~expected:public_module ~actual:actual_public_module;
      Ok ()
  | Error err ->
      Error ("expected namespaced-internal planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected namespaced internal access to fail"

let test_same_package_binary_can_use_public_root_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep main ~depends_on:root in
  match validate_layout ~package ~graph ~analyzed:[ (main, "let () = ignore Berrybot.A.value\n"); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected public root access to be valid, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_same_package_binary_can_use_generated_public_root_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root =
    add_generated_ml_node
      graph
      ~namespace:Namespace.empty
      ~path:"src/berrybot.ml"
      ~contents:"module A = A\n"
  in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep main ~depends_on:root in
  match validate_layout ~package ~graph ~analyzed:[ (main, "let () = ignore Berrybot.A.value\n"); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected generated public root access to be valid, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_binary_private_helper_can_use_public_root_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let helper = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/b.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep helper ~depends_on:root in
  let () = add_dep main ~depends_on:helper in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (helper, "let value = Berrybot.A.value\n"); (main, "let () = ignore B.value\n"); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected binary-private helper to use public root, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_same_package_binary_can_use_public_root_via_open = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/syn.ml" }
      ~binaries:[ ("syn", "src/main.ml"); ]
      "syn"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/syn.ml" in
  let _token = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/token.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"syn" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:_token in
  let () = add_dep main ~depends_on:root in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (main, "open Syn\nlet () = ignore Token.WhitespaceTrivia\n"); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected open public root access to be valid, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_same_package_binary_cannot_use_internal_library_module_directly = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep main ~depends_on:a in
  match validate_layout ~package ~graph ~analyzed:[ (main, "let () = ignore A.value\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/main.ml") ~actual:source;
      Test.assert_equal ~expected:"A" ~actual:requested_module;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:internal_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected internal-library-module planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected direct access to internal library module to fail"

let test_same_package_binary_cannot_use_namespaced_internal_library_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep main ~depends_on:a in
  match validate_layout ~package ~graph ~analyzed:[ (main, "let () = ignore Berrybot__A.value\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/main.ml") ~actual:source;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:requested_module;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:internal_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected namespaced-internal planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected namespaced internal access to fail"

let test_binary_private_helper_cannot_use_internal_library_module_directly = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let helper = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/b.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep helper ~depends_on:a in
  let () = add_dep main ~depends_on:helper in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (helper, "let value = A.value\n"); (main, "let () = ignore B.value\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/b.ml") ~actual:source;
      Test.assert_equal ~expected:"A" ~actual:requested_module;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:internal_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected helper direct internal access to fail with planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected helper direct internal access to fail"

let test_binary_private_helper_cannot_use_namespaced_internal_library_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let helper = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/b.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep helper ~depends_on:a in
  let () = add_dep main ~depends_on:helper in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (helper, "let value = Berrybot__A.value\n"); (main, "let () = ignore B.value\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnNamespacedInternalLibraryModule {
      target_name;
      source;
      requested_module;
      internal_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/b.ml") ~actual:source;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:requested_module;
      Test.assert_equal ~expected:"Berrybot__A" ~actual:internal_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected helper namespaced internal access to fail with planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected helper namespaced internal access to fail"

let test_test_target_can_use_public_root_module = fun _ctx ->
  assert_target_can_use_public_root_module
    ~package_name:"berrybot"
    ~target_name:"berrybot-tests"
    ~target_source_path:"tests/berrybot_tests.ml"
    ~target_source:"let () = ignore Berrybot.A.value\n"

let test_example_target_can_use_public_root_module = fun _ctx ->
  assert_target_can_use_public_root_module
    ~package_name:"berrybot"
    ~target_name:"berrybot-example"
    ~target_source_path:"examples/demo.ml"
    ~target_source:"let () = ignore Berrybot.A.value\n"

let test_bench_target_can_use_public_root_module = fun _ctx ->
  assert_target_can_use_public_root_module
    ~package_name:"berrybot"
    ~target_name:"berrybot-bench"
    ~target_source_path:"bench/demo_bench.ml"
    ~target_source:"let () = ignore Berrybot.A.value\n"

let test_test_target_cannot_use_internal_library_module_directly = fun _ctx ->
  assert_target_cannot_use_internal_library_module_directly
    ~package_name:"berrybot"
    ~target_name:"berrybot-tests"
    ~target_source_path:"tests/berrybot_tests.ml"
    ~target_source:"let () = ignore A.value\n"

let test_example_target_cannot_use_internal_library_module_directly = fun _ctx ->
  assert_target_cannot_use_internal_library_module_directly
    ~package_name:"berrybot"
    ~target_name:"berrybot-example"
    ~target_source_path:"examples/demo.ml"
    ~target_source:"let () = ignore A.value\n"

let test_bench_target_cannot_use_namespaced_internal_library_module = fun _ctx ->
  assert_target_cannot_use_namespaced_internal_library_module
    ~package_name:"berrybot"
    ~target_name:"berrybot-bench"
    ~target_source_path:"bench/demo_bench.ml"
    ~target_source:"let () = ignore Berrybot__A.value\n"

let test_multiple_binaries_can_share_private_helper_module = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let a = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml" in
  let shared = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/shared.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let admin = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/admin.ml" in
  let _main_binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let _admin_binary = add_binary_target graph ~name:"admin" ~source:"src/admin.ml" in
  let () = add_dep root ~depends_on:a in
  let () = add_dep shared ~depends_on:root in
  let () = add_dep main ~depends_on:shared in
  let () = add_dep admin ~depends_on:shared in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[
      (shared, "let value = Berrybot.A.value\n");
      (main, "let () = ignore Shared.value\n");
      (admin, "let () = ignore Shared.value\n");
    ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected shared helper module across binaries to be valid, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_no_library_package_can_use_private_helper_module = fun _ctx ->
  let package = make_package ~binaries:[ ("berrybot", "src/main.ml"); ] "berrybot" in
  let graph = G.make () in
  let helper = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/helper.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let _binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let () = add_dep main ~depends_on:helper in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (helper, "let value = 1\n"); (main, "let () = ignore Helper.value\n"); ] with
  | Ok () -> Ok ()
  | Error err ->
      Error ("expected helper module in no-library package to be valid, got: "
      ^ Riot_planner.Planning_error.to_string err)

let test_same_package_binary_cannot_use_other_binary_root_directly = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let root = add_ml_node graph ~namespace:Namespace.empty ~path:"src/berrybot.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let admin = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/admin.ml" in
  let _main_binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let _admin_binary = add_binary_target graph ~name:"admin" ~source:"src/admin.ml" in
  let () = add_dep main ~depends_on:admin in
  let () =
    add_dep
      root
      ~depends_on:(add_ml_node graph ~namespace:(public_namespace package) ~path:"src/a.ml")
  in
  match validate_layout ~package ~graph ~analyzed:[ (main, "let () = ignore Admin.run\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
      target_name;
      source;
      requested_module;
      other_target_name;
      other_target_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/main.ml") ~actual:source;
      Test.assert_equal ~expected:"Admin" ~actual:requested_module;
      Test.assert_equal ~expected:"admin" ~actual:other_target_name;
      Test.assert_equal ~expected:"Berrybot__Admin" ~actual:other_target_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected other-target-root planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected direct access to another target root to fail"

let test_same_package_binary_cannot_use_namespaced_other_binary_root = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let admin = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/admin.ml" in
  let _main_binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let _admin_binary = add_binary_target graph ~name:"admin" ~source:"src/admin.ml" in
  let () = add_dep main ~depends_on:admin in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (main, "let () = ignore Berrybot__Admin.run\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
      target_name;
      source;
      requested_module;
      other_target_name;
      other_target_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/main.ml") ~actual:source;
      Test.assert_equal ~expected:"Berrybot__Admin" ~actual:requested_module;
      Test.assert_equal ~expected:"admin" ~actual:other_target_name;
      Test.assert_equal ~expected:"Berrybot__Admin" ~actual:other_target_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected namespaced other-target-root planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected namespaced access to another target root to fail"

let test_binary_private_helper_cannot_use_other_binary_root = fun _ctx ->
  let package =
    make_package
      ~library:{ path = Path.v "src/berrybot.ml" }
      ~binaries:[ ("berrybot", "src/main.ml"); ("admin", "src/admin.ml"); ]
      "berrybot"
  in
  let graph = G.make () in
  let helper = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/b.ml" in
  let main = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/main.ml" in
  let admin = add_ml_node graph ~namespace:(public_namespace package) ~path:"src/admin.ml" in
  let _main_binary = add_binary_target graph ~name:"berrybot" ~source:"src/main.ml" in
  let _admin_binary = add_binary_target graph ~name:"admin" ~source:"src/admin.ml" in
  let () = add_dep helper ~depends_on:admin in
  let () = add_dep main ~depends_on:helper in
  match validate_layout
    ~package
    ~graph
    ~analyzed:[ (helper, "let value = Admin.run\n"); (main, "let () = ignore B.value\n"); ] with
  | Error (
    Riot_planner.Planning_error.TargetDependsOnOtherTargetRoot {
      target_name;
      source;
      requested_module;
      other_target_name;
      other_target_module;
      public_module;
    }
  ) ->
      Test.assert_equal ~expected:"berrybot" ~actual:target_name;
      Test.assert_equal ~expected:(Path.v "src/b.ml") ~actual:source;
      Test.assert_equal ~expected:"Admin" ~actual:requested_module;
      Test.assert_equal ~expected:"admin" ~actual:other_target_name;
      Test.assert_equal ~expected:"Berrybot__Admin" ~actual:other_target_module;
      Test.assert_equal ~expected:"Berrybot" ~actual:public_module;
      Ok ()
  | Error err ->
      Error ("expected helper other-target-root planner error, got: "
      ^ Riot_planner.Planning_error.to_string err)
  | Ok () -> Error "expected helper access to another target root to fail"

let tests =
  Test.[
    case
      "same-package binary can use public root module"
      test_same_package_binary_can_use_public_root_module;
    case
      "undeclared package module suggests available module name"
      test_undeclared_package_module_suggests_available_module_name;
    case
      "same-package binary can use generated public root module"
      test_same_package_binary_can_use_generated_public_root_module;
    case
      "binary-private helper can use public root module"
      test_binary_private_helper_can_use_public_root_module;
    case
      "same-package binary can use public root module via open"
      test_same_package_binary_can_use_public_root_via_open;
    case
      "same-package binary cannot use internal library module directly"
      test_same_package_binary_cannot_use_internal_library_module_directly;
    case
      "same-package binary cannot use namespaced internal library module"
      test_same_package_binary_cannot_use_namespaced_internal_library_module;
    case
      "binary-private helper cannot use internal library module directly"
      test_binary_private_helper_cannot_use_internal_library_module_directly;
    case
      "binary-private helper cannot use namespaced internal library module"
      test_binary_private_helper_cannot_use_namespaced_internal_library_module;
    case "test target can use public root module" test_test_target_can_use_public_root_module;
    case "example target can use public root module" test_example_target_can_use_public_root_module;
    case "bench target can use public root module" test_bench_target_can_use_public_root_module;
    case
      "test target cannot use internal library module directly"
      test_test_target_cannot_use_internal_library_module_directly;
    case
      "example target cannot use internal library module directly"
      test_example_target_cannot_use_internal_library_module_directly;
    case
      "bench target cannot use namespaced internal library module"
      test_bench_target_cannot_use_namespaced_internal_library_module;
    case
      "multiple binaries can share private helper module"
      test_multiple_binaries_can_share_private_helper_module;
    case
      "no-library package can use private helper module"
      test_no_library_package_can_use_private_helper_module;
    case
      "same-package binary cannot use other binary root directly"
      test_same_package_binary_cannot_use_other_binary_root_directly;
    case
      "same-package binary cannot use namespaced other binary root"
      test_same_package_binary_cannot_use_namespaced_other_binary_root;
    case
      "binary-private helper cannot use other binary root"
      test_binary_private_helper_cannot_use_other_binary_root;
  ]

let name = "riot-planner:package-layout"

let main ~args = Test.Cli.main ~name ~tests ~args ()

let () = Runtime.run ~main ~args:Env.args ()
