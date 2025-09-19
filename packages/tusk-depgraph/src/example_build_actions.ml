(* Example of how to use the topological sort with module kinds to generate build actions *)

open Std
open Dep_graph

let generate_build_actions sorted_nodes =
  List.filter_map (fun node ->
    match node.kind with
    | Generated ->
        (* Generated files don't need compilation, just creation *)
        Some (Printf.sprintf "echo '(* Generated alias module *)' > %s" node.file)

    | Interface ->
        (* Compile .mli files to .cmi *)
        Some (Printf.sprintf "ocamlc -c %s" node.file)

    | Implementation ->
        (* For .ml files, check if there's a corresponding .mli *)
        let base = Filename.chop_extension node.file in
        let has_interface =
          List.exists (fun n ->
            n.file = base ^ ".mli" && n.kind = Interface
          ) sorted_nodes
        in
        if has_interface then
          (* If there's an interface, the .cmi will already exist from compiling the .mli *)
          Some (Printf.sprintf "ocamlc -c %s  # (has interface)" node.file)
        else
          (* No interface, so this will create both .cmo and .cmi *)
          Some (Printf.sprintf "ocamlc -c %s  # (no interface)" node.file)
  ) sorted_nodes

let () =
  match Env.args with
  | dir :: _ ->
      (* Extract package name *)
      let package_name =
        let parts = String.split_on_char '/' dir in
        let rec find_package parts =
          match parts with
          | "packages" :: pkg :: _ -> pkg
          | _ :: rest -> find_package rest
          | [] -> Filename.basename dir
        in
        find_package parts |> String.capitalize_ascii
      in

      (* Build dependency graph *)
      let graph = Dep_graph.create ~package_name in
      Dep_graph.build graph dir;

      (* Get topological sort *)
      let sorted = Dep_graph.topological_sort graph in

      (* Generate build actions *)
      Printf.printf "=== Build Actions for %s ===\n" package_name;
      let actions = generate_build_actions sorted in
      List.iteri (fun i action ->
        Printf.printf "%3d. %s\n" (i + 1) action
      ) actions

  | [] ->
      Printf.eprintf "Usage: %s <directory>\n" Sys.argv.(0);
      exit 1