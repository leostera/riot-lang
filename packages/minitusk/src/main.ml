(** Minitusk - Minimal OCaml build system

    A self-contained build system that can bootstrap itself and build OCaml
    packages with proper module namespacing and nested library support. *)

(* ===== Main ===== *)

let build_package (pkg : Package.t) =
  Printf.printf "\nBuilding package: %s\n" pkg.name;
  Printf.printf "  Path: %s\n" pkg.path;

  (* Create dependency graph for the package *)
  let dep_graph = Dep_graph.scan ~root:pkg.path ~package_name:pkg.name in

  File_scanner.print_tree dep_graph.file_tree;

  Printf.printf "\n\nDependency Graph: %s\n" pkg.name;
  (* For now, just print what we would build *)
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


  Printf.printf "\n\nModule Registry:\n%!";
  Dep_graph.print_registry dep_graph

let () =
  (* Simple package configuration *)
  let packages =
    Package.
      [
        { name = "kernel"; path = "packages/kernel"; deps = [] };
        { name = "miniriot"; path = "packages/miniriot"; deps = [ "kernel" ] };
        { name = "std"; path = "packages/std"; deps = [ "kernel"; "miniriot" ] };
        { name = "tusk"; path = "packages/tusk"; deps = [ "kernel"; "miniriot"; "std" ]; };
      ]
  in

  Printf.printf "=== Minitusk Build System ===\n";
  Printf.printf "Building %d packages\n" (List.length packages);

  (* Build each package in order *)
  List.iter build_package packages;

  Printf.printf "\n=== Build complete! ===\n"
