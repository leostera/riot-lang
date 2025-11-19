  open Stdlib
(** Minitusk - Minimal OCaml build system

    A self-contained build system that can bootstrap itself and build OCaml
    packages with proper module namespacing and nested library support. *)

(* ===== Main ===== *)

let build_package ~build_results ?(needs_stdlib_and_unix = false) pkg_name pkg_path =
  Printf.printf "\nBuilding package: %s\n" pkg_name;
  Printf.printf "  Path: %s\n" pkg_path;

  let pkg = Package.read pkg_path in
  
  (* Override stdlib/unix dependencies if specified *)
  let pkg =
    if needs_stdlib_and_unix then
      { pkg with uses_stdlib = true; uses_unix = true; uses_dynlink = true }
    else pkg
  in

  (* Create dependency graph for the package, passing build_results for cross-package deps *)
  let dep_graph = Dep_graph.scan ~root:pkg_path ~package:pkg ~build_results in

  File_scanner.print_tree dep_graph.file_tree;

  (* For now, just print what we would build 
  Printf.printf "\n\nDependency Graph: %s\n" pkg_name;
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      let filename =
        match node.value.file with
        | Concrete path -> Filename.basename path
        | Generated { path; _ } -> Filename.basename path ^ " (generated)"
      in
      Printf.printf "  - %s\n%!" filename)
    dep_graph;
*)

  (* Dump graph as dot for debugging *)
  let dot_dir = Printf.sprintf "_build/bootstrap/out/%s" pkg_name in
  Io.mkdir_p dot_dir;
  let dot_file = Printf.sprintf "%s/graph.dot" dot_dir in
  let dot_content = Dep_graph.to_dot dep_graph in
  Io.write_file dot_file dot_content;
  Printf.printf "Dumped graph to %s\n" dot_file;

  let build_plan = Action.from_dep_graph dep_graph in
  (* Printf.printf "\n\nBuild Plan: %s\n" pkg_name;
  Action.print_build_plan build_plan; *)
  Action.execute_build_plan ~build_results build_plan;
  Action.promote_outputs build_plan;

  (* Register this package's outputs for other packages to use *)
  (* IMPORTANT: Only store this package's OWN flags, not accumulated ones! *)
  Dep_graph.Build_results.register build_results pkg build_plan.package_name
    ~outputs:build_plan.outputs
    ~cc_flags:(Package.cc_flags pkg)
    ~ld_flags:(Package.ld_flags pkg)

let () =
  Printf.printf "=== Minitusk Build System ===\n";

  (* Create build results tracker for cross-package dependencies *)
  let build_results = Dep_graph.Build_results.create () in

  (* Build packages in dependency order *)
  build_package ~build_results ~needs_stdlib_and_unix:true "kernel" "packages/kernel";
  build_package ~build_results "miniriot" "packages/miniriot";
  build_package ~build_results "std" "packages/std";
  build_package ~build_results "jsonrpc" "packages/jsonrpc";
  build_package ~build_results "mcp" "packages/mcp";
  build_package ~build_results "propane" "packages/propane";
  build_package ~build_results "ceibo" "packages/ceibo";
  build_package ~build_results "datalog" "packages/datalog";
  build_package ~build_results "poneglyph" "packages/poneglyph";
  build_package ~build_results "tusk-model" "packages/tusk-model";
  build_package ~build_results "tusk-store" "packages/tusk-store";
  build_package ~build_results "tusk-toolchain" "packages/tusk-toolchain";
  build_package ~build_results "codedb" "packages/codedb";
  build_package ~build_results "tusk-planner" "packages/tusk-planner";
  build_package ~build_results "tusk-executor" "packages/tusk-executor";
  build_package ~build_results "tusk-protocol" "packages/tusk-protocol";
  build_package ~build_results "tusk-client" "packages/tusk-client";
  build_package ~build_results "tusk-server" "packages/tusk-server";
  build_package ~build_results "tusk-mcp" "packages/tusk-mcp";
  build_package ~build_results "tusk-init" "packages/tusk-init";
  build_package ~build_results "tusk-cli" "packages/tusk-cli";

  Printf.printf "\n=== Build complete! ===\n"
