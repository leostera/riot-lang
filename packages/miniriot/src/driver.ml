open Stdlib

(**
   Actors - Minimal OCaml build system

   A self-contained build system that can bootstrap itself and build OCaml
   packages with proper module namespacing and nested library support.
*)
(* ===== Main ===== *)

let build_package = fun ~build_results ?(needs_stdlib_and_unix = false) pkg_name pkg_path ->
  Printf.printf "\nBuilding package: %s\n" pkg_name;
  Printf.printf "  Path: %s\n" pkg_path;
  let pkg = Package.read pkg_path in
  (* Override stdlib/unix dependencies if specified *)
  let pkg =
    if needs_stdlib_and_unix then
      { pkg with uses_stdlib = true; uses_unix = true; uses_dynlink = true }
    else
      pkg
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
     Action.print_build_plan build_plan;
  *)
  Action.execute_build_plan ~build_results build_plan;
  Action.promote_outputs build_plan;
  (* Register this package's outputs for other packages to use *)
  (* IMPORTANT: Only store this package's OWN flags, not accumulated ones! *)
  Dep_graph.Build_results.register
    build_results
    pkg
    build_plan.package_name
    ~outputs:build_plan.outputs
    ~cc_flags:(Package.cc_flags pkg)
    ~ld_flags:(Package.ld_flags pkg)

let discover_workspace_packages = fun packages_root ->
  let packages = Hashtbl.create 64 in
  Sys.readdir packages_root |> Array.iter
    (fun entry ->
      let path = Filename.concat packages_root entry in
      let manifest = Filename.concat path "riot.toml" in
      if Sys.file_exists manifest && Sys.is_directory path then
        let pkg = Package.read path in
        Hashtbl.replace packages pkg.Package.name pkg);
  packages

let topo_sort_packages = fun packages roots ->
  let visiting = Hashtbl.create 64 in
  let built = Hashtbl.create 64 in
  let order = ref [] in
  let rec visit name =
    if Hashtbl.mem built name then
      ()
    else if Hashtbl.mem visiting name then
      failwith (Printf.sprintf "Bootstrap dependency cycle detected at package %s" name)
    else
      match Hashtbl.find_opt packages name with
      | None -> failwith (Printf.sprintf "Unknown bootstrap package %s" name)
      | Some pkg ->
          Hashtbl.replace visiting name ();
          Package.deps pkg |> List.iter
            (fun dep ->
              if Hashtbl.mem packages dep then
                visit dep);
          Hashtbl.remove visiting name;
          Hashtbl.replace built name ();
          order := name :: !order
  in
  List.iter visit roots;
  List.rev !order

let () =
  Printf.printf "=== Actors Build System ===\n";
  (* Create build results tracker for cross-package dependencies *)
  let build_results = Dep_graph.Build_results.create () in
  let packages = discover_workspace_packages "packages" in
  let build_order = topo_sort_packages packages [ "riot-cli" ] in
  Printf.printf "Bootstrap build order: %s\n" (String.concat " -> " build_order);
  List.iter
    (fun pkg_name ->
      match Hashtbl.find_opt packages pkg_name with
      | None -> failwith (Printf.sprintf "Unknown bootstrap package %s" pkg_name)
      | Some pkg -> build_package
        ~build_results
        ~needs_stdlib_and_unix:(String.equal pkg_name "kernel")
        pkg_name
        pkg.Package.path)
    build_order;
  Printf.printf "\n=== Build complete! ===\n"
