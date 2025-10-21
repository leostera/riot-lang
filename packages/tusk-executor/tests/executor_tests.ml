open Std
open Std.Collections
module Test = Std.Test
module G = Std.Graph.SimpleGraph

let test_toolchain =
  lazy
    (Tusk_toolchain.init ()
    |> Result.expect ~msg:"Failed to initialize test toolchain")

let make_test_package () =
  Tusk_model.Package.
    {
      name = "test";
      path = Path.v ".";
      relative_path = Path.v ".";
      dependencies = [];
      binaries = [];
      library = None;
      test_library = None;
      test_modules = [];
    }

let make_action_spec ?(actions = []) ?(outs = []) ?(srcs = []) () =
  {
    Tusk_planner.Action_node.actions;
    outs;
    srcs;
    package = make_test_package ();
    toolchain = Lazy.force test_toolchain;
    hash = Crypto.hash_string "test";
  }

let make_test_node ?actions ?outs ?srcs graph =
  let spec = make_action_spec ?actions ?outs ?srcs () in
  G.add_node graph spec

let test_deps_satisfied_all_built () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let node3 = make_test_node graph in
  let _ = G.add_edge node3 ~depends_on:node1 in
  let _ = G.add_edge node3 ~depends_on:node2 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        { node_id = node1.id; status = `Built; duration_ms = 10; error = None }
  in
  let _ =
    HashMap.insert completed node2.id
      Tusk_executor.Executor.
        { node_id = node2.id; status = `Cached; duration_ms = 0; error = None }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node3 in
  if result then Ok () else Error "Expected deps_satisfied to return true"

let test_deps_satisfied_missing_dep () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let node3 = make_test_node graph in
  let _ = G.add_edge node3 ~depends_on:node1 in
  let _ = G.add_edge node3 ~depends_on:node2 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        { node_id = node1.id; status = `Built; duration_ms = 10; error = None }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node3 in
  if not result then Ok () else Error "Expected deps_satisfied to return false"

let test_deps_satisfied_failed_dep () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let _ = G.add_edge node2 ~depends_on:node1 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        {
          node_id = node1.id;
          status = `Failed;
          duration_ms = 10;
          error = Some "error";
        }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node2 in
  if not result then Ok () else Error "Expected deps_satisfied to return false"

let test_deps_satisfied_no_deps () =
  let graph = G.make () in
  let node = make_test_node graph in
  let completed = HashMap.create () in
  let result = Tusk_executor.Executor.deps_satisfied completed node in
  if result then Ok () else Error "Expected deps_satisfied to return true"

let test_deps_satisfied_mixed_statuses () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let node3 = make_test_node graph in
  let _ = G.add_edge node3 ~depends_on:node1 in
  let _ = G.add_edge node3 ~depends_on:node2 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        { node_id = node1.id; status = `Built; duration_ms = 10; error = None }
  in
  let _ =
    HashMap.insert completed node2.id
      Tusk_executor.Executor.
        {
          node_id = node2.id;
          status = `Failed;
          duration_ms = 5;
          error = Some "fail";
        }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node3 in
  if not result then Ok () else Error "Expected deps_satisfied to return false"

let test_execution_result_status_built () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      { node_id = node.id; status = `Built; duration_ms = 100; error = None }
  in
  match result.status with
  | `Built -> Ok ()
  | _ -> Error "Expected status to be `Built"

let test_execution_result_status_cached () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      { node_id = node.id; status = `Cached; duration_ms = 0; error = None }
  in
  match result.status with
  | `Cached -> Ok ()
  | _ -> Error "Expected status to be `Cached"

let test_execution_result_status_failed () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      {
        node_id = node.id;
        status = `Failed;
        duration_ms = 50;
        error = Some "compilation error";
      }
  in
  match result.status with
  | `Failed -> (
      match result.error with
      | Some _ -> Ok ()
      | None -> Error "Expected error message")
  | _ -> Error "Expected status to be `Failed"

let test_execution_result_duration () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      { node_id = node.id; status = `Built; duration_ms = 250; error = None }
  in
  if result.duration_ms = 250 then Ok ()
  else Error "Expected duration_ms to be 250"

let test_action_copy_file_creates_file () =
  match
    Fs.with_tempdir ~prefix:"executor_test" (fun tmp_dir ->
        let src = Path.(tmp_dir / Path.v "source.txt") in
        let dst = Path.(tmp_dir / Path.v "dest.txt") in
        let _ =
          Fs.write "test content" src |> Result.expect ~msg:"Write failed"
        in
        let action =
          Tusk_planner.Action.CopyFile { source = src; destination = dst }
        in
        match action with
        | CopyFile { source; destination } -> (
            match Fs.copy ~src:source ~dst:destination with
            | Ok () ->
                if Fs.exists destination |> Result.unwrap_or ~default:false then
                  Ok ()
                else Error "Destination file not created"
            | Error _ -> Error "Copy failed")
        | _ -> Error "Wrong action type")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_action_write_file_creates_file () =
  match
    Fs.with_tempdir ~prefix:"executor_test" (fun tmp_dir ->
        let dst = Path.(tmp_dir / Path.v "output.txt") in
        let content = "hello world" in
        let action =
          Tusk_planner.Action.WriteFile { destination = dst; content }
        in
        match action with
        | WriteFile { destination; content = c } -> (
            match Fs.write c destination with
            | Ok () ->
                if Fs.exists destination |> Result.unwrap_or ~default:false then
                  match Fs.read destination with
                  | Ok read_content ->
                      if String.equal read_content content then Ok ()
                      else Error "Content mismatch"
                  | Error _ -> Error "Failed to read written file"
                else Error "Destination file not created"
            | Error _ -> Error "Write failed")
        | _ -> Error "Wrong action type")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_action_to_string_compile_interface () =
  let action =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/foo.mli";
        output = Path.v "build/foo.cmi";
        includes = [ Path.v "lib1"; Path.v "lib2" ];
        flags = [];
      }
  in
  let str = Tusk_planner.Action.to_string action in
  if String.starts_with ~prefix:"CompileInterface" str then Ok ()
  else Error "Expected 'CompileInterface' in string representation"

let test_action_to_string_compile_implementation () =
  let action =
    Tusk_planner.Action.CompileImplementation
      {
        source = Path.v "src/foo.ml";
        output = Path.v "build/foo.cmx";
        includes = [];
        flags = [];
      }
  in
  let str = Tusk_planner.Action.to_string action in
  if String.starts_with ~prefix:"CompileImplementation" str then Ok ()
  else Error "Expected 'CompileImplementation' in string representation"

let test_action_to_string_create_library () =
  let action =
    Tusk_planner.Action.CreateLibrary
      {
        output = Path.v "build/lib.cmxa";
        objects = [ Path.v "build/foo.cmx"; Path.v "build/bar.cmx" ];
        includes = [];
      }
  in
  let str = Tusk_planner.Action.to_string action in
  if String.starts_with ~prefix:"CreateLibrary" str then Ok ()
  else Error "Expected 'CreateLibrary' in string representation"

let test_action_to_string_create_executable () =
  let action =
    Tusk_planner.Action.CreateExecutable
      {
        output = Path.v "build/main.exe";
        objects = [ Path.v "build/main.cmx" ];
        libraries = [ Path.v "build/lib.cmxa" ];
        includes = [];
      }
  in
  let str = Tusk_planner.Action.to_string action in
  if String.starts_with ~prefix:"CreateExecutable" str then Ok ()
  else Error "Expected 'CreateExecutable' in string representation"

let test_action_equal_compile_interface_same () =
  let action1 =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/foo.mli";
        output = Path.v "build/foo.cmi";
        includes = [];
        flags = [];
      }
  in
  let action2 =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/foo.mli";
        output = Path.v "build/foo.cmi";
        includes = [];
        flags = [];
      }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected actions to be equal"

let test_action_equal_compile_interface_different () =
  let action1 =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/foo.mli";
        output = Path.v "build/foo.cmi";
        includes = [];
        flags = [];
      }
  in
  let action2 =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/bar.mli";
        output = Path.v "build/bar.cmi";
        includes = [];
        flags = [];
      }
  in
  if not (Tusk_planner.Action.equal action1 action2) then Ok ()
  else Error "Expected actions to be different"

let test_action_equal_different_types () =
  let action1 =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "src/foo.mli";
        output = Path.v "build/foo.cmi";
        includes = [];
        flags = [];
      }
  in
  let action2 =
    Tusk_planner.Action.CompileImplementation
      {
        source = Path.v "src/foo.ml";
        output = Path.v "build/foo.cmx";
        includes = [];
        flags = [];
      }
  in
  if not (Tusk_planner.Action.equal action1 action2) then Ok ()
  else Error "Expected different action types to be unequal"

let test_action_node_hash_consistency () =
  let graph = G.make () in
  let package = make_test_package () in
  let toolchain = Lazy.force test_toolchain in
  let spec1 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let spec2 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let node1 = G.add_node graph spec1 in
  let node2 = G.add_node graph spec2 in
  let hash1 = Tusk_planner.Action_node.get_hash node1 in
  let hash2 = Tusk_planner.Action_node.get_hash node2 in
  if Crypto.Digest.hex hash1 = Crypto.Digest.hex hash2 then Ok ()
  else Error "Expected identical nodes to have same hash"

let test_action_node_hash_different_actions () =
  let graph = G.make () in
  let package = make_test_package () in
  let toolchain = Lazy.force test_toolchain in
  let spec1 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let spec2 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "bar.mli";
              output = Path.v "bar.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "bar.cmi" ]
      ~srcs:[ Path.v "bar.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let node1 = G.add_node graph spec1 in
  let node2 = G.add_node graph spec2 in
  let hash1 = Tusk_planner.Action_node.get_hash node1 in
  let hash2 = Tusk_planner.Action_node.get_hash node2 in
  if Crypto.Digest.hex hash1 <> Crypto.Digest.hex hash2 then Ok ()
  else Error "Expected different nodes to have different hashes"

let test_action_node_to_json () =
  let graph = G.make () in
  let package = make_test_package () in
  let toolchain = Lazy.force test_toolchain in
  let spec =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let node = G.add_node graph spec in
  let json = Tusk_planner.Action_node.to_json node in
  match Data.Json.get_field "actions" json with
  | Some (Array _) -> Ok ()
  | _ -> Error "Expected 'actions' field to be an array"

let test_action_node_equal_same () =
  let graph = G.make () in
  let package = make_test_package () in
  let toolchain = Lazy.force test_toolchain in
  let spec1 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let spec2 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let node1 = G.add_node graph spec1 in
  let node2 = G.add_node graph spec2 in
  if Tusk_planner.Action_node.equal node1 node2 then Ok ()
  else Error "Expected equal nodes"

let test_action_node_equal_different () =
  let graph = G.make () in
  let package = make_test_package () in
  let toolchain = Lazy.force test_toolchain in
  let spec1 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "foo.mli";
              output = Path.v "foo.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "foo.cmi" ]
      ~srcs:[ Path.v "foo.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let spec2 =
    Tusk_planner.Action_node.make
      ~actions:
        [
          Tusk_planner.Action.CompileInterface
            {
              source = Path.v "bar.mli";
              output = Path.v "bar.cmi";
              includes = [];
              flags = [];
            };
        ]
      ~outs:[ Path.v "bar.cmi" ]
      ~srcs:[ Path.v "bar.mli" ]
      ~package ~toolchain
      ~dependency_hashes:(fun _ -> Crypto.hash_string "dep")
      ~deps:[]
  in
  let node1 = G.add_node graph spec1 in
  let node2 = G.add_node graph spec2 in
  if not (Tusk_planner.Action_node.equal node1 node2) then Ok ()
  else Error "Expected unequal nodes"

let test_action_copy_file_equal () =
  let action1 =
    Tusk_planner.Action.CopyFile
      { source = Path.v "a.txt"; destination = Path.v "b.txt" }
  in
  let action2 =
    Tusk_planner.Action.CopyFile
      { source = Path.v "a.txt"; destination = Path.v "b.txt" }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal CopyFile actions"

let test_action_write_file_equal () =
  let action1 =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "out.txt"; content = "hello" }
  in
  let action2 =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "out.txt"; content = "hello" }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal WriteFile actions"

let test_action_write_file_different_content () =
  let action1 =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "out.txt"; content = "hello" }
  in
  let action2 =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "out.txt"; content = "world" }
  in
  if not (Tusk_planner.Action.equal action1 action2) then Ok ()
  else Error "Expected different WriteFile actions"

let test_action_compile_c_equal () =
  let action1 =
    Tusk_planner.Action.CompileC
      { source = Path.v "foo.c"; output = Path.v "foo.o" }
  in
  let action2 =
    Tusk_planner.Action.CompileC
      { source = Path.v "foo.c"; output = Path.v "foo.o" }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal CompileC actions"

let test_action_generate_interface_equal () =
  let action1 =
    Tusk_planner.Action.GenerateInterface
      {
        source = Path.v "foo.ml";
        output = Path.v "foo.mli";
        includes = [];
        flags = [];
      }
  in
  let action2 =
    Tusk_planner.Action.GenerateInterface
      {
        source = Path.v "foo.ml";
        output = Path.v "foo.mli";
        includes = [];
        flags = [];
      }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal GenerateInterface actions"

let test_action_create_library_equal () =
  let action1 =
    Tusk_planner.Action.CreateLibrary
      {
        output = Path.v "lib.cmxa";
        objects = [ Path.v "a.cmx"; Path.v "b.cmx" ];
        includes = [];
      }
  in
  let action2 =
    Tusk_planner.Action.CreateLibrary
      {
        output = Path.v "lib.cmxa";
        objects = [ Path.v "a.cmx"; Path.v "b.cmx" ];
        includes = [];
      }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal CreateLibrary actions"

let test_action_create_executable_equal () =
  let action1 =
    Tusk_planner.Action.CreateExecutable
      {
        output = Path.v "main.exe";
        objects = [ Path.v "main.cmx" ];
        libraries = [ Path.v "lib.cmxa" ];
        includes = [];
      }
  in
  let action2 =
    Tusk_planner.Action.CreateExecutable
      {
        output = Path.v "main.exe";
        objects = [ Path.v "main.cmx" ];
        libraries = [ Path.v "lib.cmxa" ];
        includes = [];
      }
  in
  if Tusk_planner.Action.equal action1 action2 then Ok ()
  else Error "Expected equal CreateExecutable actions"

let test_action_json_roundtrip_compile_interface () =
  let action =
    Tusk_planner.Action.CompileInterface
      {
        source = Path.v "foo.mli";
        output = Path.v "foo.cmi";
        includes = [];
        flags = [];
      }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_roundtrip_copy_file () =
  let action =
    Tusk_planner.Action.CopyFile
      { source = Path.v "a.txt"; destination = Path.v "b.txt" }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_roundtrip_write_file () =
  let action =
    Tusk_planner.Action.WriteFile
      { destination = Path.v "out.txt"; content = "test" }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_roundtrip_compile_c () =
  let action =
    Tusk_planner.Action.CompileC
      { source = Path.v "foo.c"; output = Path.v "foo.o" }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_roundtrip_create_library () =
  let action =
    Tusk_planner.Action.CreateLibrary
      { output = Path.v "lib.cmxa"; objects = []; includes = [] }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_roundtrip_create_executable () =
  let action =
    Tusk_planner.Action.CreateExecutable
      {
        output = Path.v "main.exe";
        objects = [];
        libraries = [];
        includes = [];
      }
  in
  let json = Tusk_planner.Action.to_json action in
  match Tusk_planner.Action.from_json json with
  | Ok parsed ->
      if Tusk_planner.Action.equal action parsed then Ok ()
      else Error "Roundtrip action mismatch"
  | Error e -> Error (format "Failed to parse JSON: %s" e)

let test_action_json_invalid_type () =
  let json = Data.Json.obj [ ("type", Data.Json.string "InvalidType") ] in
  match Tusk_planner.Action.from_json json with
  | Error _ -> Ok ()
  | Ok _ -> Error "Expected parsing to fail for invalid type"

let test_action_json_missing_type () =
  let json = Data.Json.obj [ ("source", Data.Json.string "foo.ml") ] in
  match Tusk_planner.Action.from_json json with
  | Error _ -> Ok ()
  | Ok _ -> Error "Expected parsing to fail for missing type"

let test_action_json_missing_fields () =
  let json = Data.Json.obj [ ("type", Data.Json.string "CompileInterface") ] in
  match Tusk_planner.Action.from_json json with
  | Error _ -> Ok ()
  | Ok _ -> Error "Expected parsing to fail for missing fields"

let test_deps_satisfied_empty_completed () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let _ = G.add_edge node2 ~depends_on:node1 in
  let completed = HashMap.create () in
  let result = Tusk_executor.Executor.deps_satisfied completed node2 in
  if not result then Ok ()
  else Error "Expected deps_satisfied to return false with empty completed"

let test_deps_satisfied_multiple_deps_all_built () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let node3 = make_test_node graph in
  let node4 = make_test_node graph in
  let _ = G.add_edge node4 ~depends_on:node1 in
  let _ = G.add_edge node4 ~depends_on:node2 in
  let _ = G.add_edge node4 ~depends_on:node3 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        { node_id = node1.id; status = `Built; duration_ms = 10; error = None }
  in
  let _ =
    HashMap.insert completed node2.id
      Tusk_executor.Executor.
        { node_id = node2.id; status = `Built; duration_ms = 20; error = None }
  in
  let _ =
    HashMap.insert completed node3.id
      Tusk_executor.Executor.
        { node_id = node3.id; status = `Built; duration_ms = 15; error = None }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node4 in
  if result then Ok ()
  else Error "Expected deps_satisfied to return true with all deps built"

let test_deps_satisfied_single_cached_dep () =
  let graph = G.make () in
  let node1 = make_test_node graph in
  let node2 = make_test_node graph in
  let _ = G.add_edge node2 ~depends_on:node1 in
  let completed = HashMap.create () in
  let _ =
    HashMap.insert completed node1.id
      Tusk_executor.Executor.
        { node_id = node1.id; status = `Cached; duration_ms = 0; error = None }
  in
  let result = Tusk_executor.Executor.deps_satisfied completed node2 in
  if result then Ok ()
  else Error "Expected deps_satisfied to return true with cached dep"

let test_execution_result_node_id () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      { node_id = node.id; status = `Built; duration_ms = 100; error = None }
  in
  if G.Node_id.eq result.node_id node.id then Ok ()
  else Error "Expected node_id to match"

let test_execution_result_error_message () =
  let graph = G.make () in
  let node = make_test_node graph in
  let error_msg = "Compilation failed: undefined symbol" in
  let result =
    Tusk_executor.Executor.
      {
        node_id = node.id;
        status = `Failed;
        duration_ms = 50;
        error = Some error_msg;
      }
  in
  match result.error with
  | Some msg when String.equal msg error_msg -> Ok ()
  | _ -> Error "Expected error message to match"

let test_execution_result_no_error_on_success () =
  let graph = G.make () in
  let node = make_test_node graph in
  let result =
    Tusk_executor.Executor.
      { node_id = node.id; status = `Built; duration_ms = 100; error = None }
  in
  match result.error with
  | None -> Ok ()
  | Some _ -> Error "Expected no error on successful build"

let test_execute_actions_write_file () =
  match
    Fs.with_tempdir ~prefix:"exec_test" (fun tmpdir ->
        let output = Path.(tmpdir / Path.v "test.txt") in
        let content = "test content" in
        let actions =
          [ Tusk_planner.Action.WriteFile { destination = output; content } ]
        in
        let toolchain = Lazy.force test_toolchain in
        Tusk_executor.Executor.execute_actions toolchain tmpdir actions;
        if Fs.exists output |> Result.unwrap_or ~default:false then
          match Fs.read output with
          | Ok read_content ->
              if String.equal read_content content then Ok ()
              else Error "Content mismatch"
          | Error _ -> Error "Failed to read output"
        else Error "Output file not created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_execute_actions_copy_file () =
  match
    Fs.with_tempdir ~prefix:"exec_test" (fun tmpdir ->
        let src = Path.(tmpdir / Path.v "source.txt") in
        let dst = Path.(tmpdir / Path.v "dest.txt") in
        let _ = Fs.write "copy me" src |> Result.expect ~msg:"Write failed" in
        let actions =
          [ Tusk_planner.Action.CopyFile { source = src; destination = dst } ]
        in
        let toolchain = Lazy.force test_toolchain in
        Tusk_executor.Executor.execute_actions toolchain tmpdir actions;
        if Fs.exists dst |> Result.unwrap_or ~default:false then Ok ()
        else Error "Destination file not created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_execute_actions_multiple () =
  match
    Fs.with_tempdir ~prefix:"exec_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "file1.txt") in
        let file2 = Path.(tmpdir / Path.v "file2.txt") in
        let actions =
          [
            Tusk_planner.Action.WriteFile
              { destination = file1; content = "content1" };
            Tusk_planner.Action.WriteFile
              { destination = file2; content = "content2" };
          ]
        in
        let toolchain = Lazy.force test_toolchain in
        Tusk_executor.Executor.execute_actions toolchain tmpdir actions;
        let both_exist =
          Fs.exists file1 |> Result.unwrap_or ~default:false
          && Fs.exists file2 |> Result.unwrap_or ~default:false
        in
        if both_exist then Ok () else Error "Not all output files created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_execute_actions_copy_after_write () =
  match
    Fs.with_tempdir ~prefix:"exec_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "file1.txt") in
        let file2 = Path.(tmpdir / Path.v "file2.txt") in
        let actions =
          [
            Tusk_planner.Action.WriteFile
              { destination = file1; content = "first" };
            Tusk_planner.Action.CopyFile { source = file1; destination = file2 };
          ]
        in
        let toolchain = Lazy.force test_toolchain in
        Tusk_executor.Executor.execute_actions toolchain tmpdir actions;
        if Fs.exists file2 |> Result.unwrap_or ~default:false then
          match Fs.read file2 with
          | Ok content ->
              if String.equal content "first" then Ok ()
              else Error "Content not propagated"
          | Error _ -> Error "Failed to read file2"
        else Error "Dependent file not created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_execute_node_success () =
  let graph = G.make () in
  match
    Fs.with_tempdir ~prefix:"exec_test" (fun tmpdir ->
        let output = Path.(tmpdir / Path.v "test.txt") in
        let spec =
          make_action_spec
            ~actions:
              [
                Tusk_planner.Action.WriteFile
                  { destination = output; content = "test" };
              ]
            ~outs:[ output ] ()
        in
        let node = G.add_node graph spec in
        let toolchain = Lazy.force test_toolchain in
        let result =
          Tusk_executor.Executor.execute_node toolchain tmpdir node
        in
        if Fs.exists output |> Result.unwrap_or ~default:false then
          match result.status with
          | `Built -> Ok ()
          | _ -> Error "Expected `Built status"
        else Error "Output not created")
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let test_verify_outputs_success () =
  match
    Fs.with_tempdir ~prefix:"verify_test" (fun tmpdir ->
        let file1 = Path.(tmpdir / Path.v "a.txt") in
        let file2 = Path.(tmpdir / Path.v "b.txt") in
        let _ = Fs.write "a" file1 |> Result.expect ~msg:"Write failed" in
        let _ = Fs.write "b" file2 |> Result.expect ~msg:"Write failed" in
        Tusk_executor.Executor.verify_outputs [ file1; file2 ];
        Ok ())
  with
  | Ok r -> r
  | Error _ -> Error "Tempdir creation failed"

let tests =
  Test.
    [
      case "deps_satisfied: all built" test_deps_satisfied_all_built;
      case "deps_satisfied: missing dep" test_deps_satisfied_missing_dep;
      case "deps_satisfied: failed dep" test_deps_satisfied_failed_dep;
      case "deps_satisfied: no deps" test_deps_satisfied_no_deps;
      case "deps_satisfied: mixed statuses" test_deps_satisfied_mixed_statuses;
      case "execution_result: status built" test_execution_result_status_built;
      case "execution_result: status cached" test_execution_result_status_cached;
      case "execution_result: status failed" test_execution_result_status_failed;
      case "execution_result: duration" test_execution_result_duration;
      case "action: copy file creates file" test_action_copy_file_creates_file;
      case "action: write file creates file" test_action_write_file_creates_file;
      case "action: to_string compile_interface"
        test_action_to_string_compile_interface;
      case "action: to_string compile_implementation"
        test_action_to_string_compile_implementation;
      case "action: to_string create_library"
        test_action_to_string_create_library;
      case "action: to_string create_executable"
        test_action_to_string_create_executable;
      case "action: equal compile_interface same"
        test_action_equal_compile_interface_same;
      case "action: equal compile_interface different"
        test_action_equal_compile_interface_different;
      case "action: equal different types" test_action_equal_different_types;
      case "action_node: hash consistency" test_action_node_hash_consistency;
      case "action_node: hash different actions"
        test_action_node_hash_different_actions;
      case "action_node: to_json" test_action_node_to_json;
      case "action_node: equal same" test_action_node_equal_same;
      case "action_node: equal different" test_action_node_equal_different;
      case "action: copy_file equal" test_action_copy_file_equal;
      case "action: write_file equal" test_action_write_file_equal;
      case "action: write_file different content"
        test_action_write_file_different_content;
      case "action: compile_c equal" test_action_compile_c_equal;
      case "action: generate_interface equal"
        test_action_generate_interface_equal;
      case "action: create_library equal" test_action_create_library_equal;
      case "action: create_executable equal" test_action_create_executable_equal;
      case "action: json roundtrip compile_interface"
        test_action_json_roundtrip_compile_interface;
      case "action: json roundtrip copy_file"
        test_action_json_roundtrip_copy_file;
      case "action: json roundtrip write_file"
        test_action_json_roundtrip_write_file;
      case "action: json roundtrip compile_c"
        test_action_json_roundtrip_compile_c;
      case "action: json roundtrip create_library"
        test_action_json_roundtrip_create_library;
      case "action: json roundtrip create_executable"
        test_action_json_roundtrip_create_executable;
      case "action: json invalid type" test_action_json_invalid_type;
      case "action: json missing type" test_action_json_missing_type;
      case "action: json missing fields" test_action_json_missing_fields;
      case "deps_satisfied: empty completed" test_deps_satisfied_empty_completed;
      case "deps_satisfied: multiple deps all built"
        test_deps_satisfied_multiple_deps_all_built;
      case "deps_satisfied: single cached dep"
        test_deps_satisfied_single_cached_dep;
      case "execution_result: node_id" test_execution_result_node_id;
      case "execution_result: error message" test_execution_result_error_message;
      case "execution_result: no error on success"
        test_execution_result_no_error_on_success;
      case "execute_actions: write file" test_execute_actions_write_file;
      case "execute_actions: copy file" test_execute_actions_copy_file;
      case "execute_actions: multiple" test_execute_actions_multiple;
      case "execute_actions: copy after write"
        test_execute_actions_copy_after_write;
      case "execute_node: success" test_execute_node_success;
      case "verify_outputs: success" test_verify_outputs_success;
    ]

let name = "Tusk Executor Tests"
let () = Miniriot.run ~main:(Test.Cli.main ~name ~tests) ~args:Env.args
