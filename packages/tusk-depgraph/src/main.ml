open Std

let () =
  let args = Env.args in
  match args with
  | dir :: _ ->
      Printf.printf "Building dependency graph for %s\n\n" dir;

      (* Extract package name from directory *)
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

      Printf.printf "Package: %s\n\n" package_name;

      (* Build dependency graph *)
      let graph = Dep_graph.create ~package_name in
      Dep_graph.build graph dir;

      (* Print registry *)
      Printf.printf "\n";
      Module_registry.dump Dep_graph.(graph.registry);

      (* Generate DOT output *)
      let dot = Dep_graph.to_dot graph in
      let dot_file = package_name ^ ".dot" in
      let oc = open_out dot_file in
      output_string oc (Graph.Dot.to_string dot);
      close_out oc;
      Printf.printf "\nWrote DOT graph to: %s\n" dot_file;

      (* Generate Mermaid output *)
      let mermaid = Dep_graph.to_mermaid graph in
      let mermaid_file = package_name ^ ".mermaid" in
      let oc = open_out mermaid_file in
      output_string oc (Graph.Mermaid.to_string mermaid);
      close_out oc;
      Printf.printf "Wrote Mermaid diagram to: %s\n" mermaid_file;

      (* Print topological sort *)
      Printf.printf "\n=== Topological Sort ===\n";
      let sorted = Dep_graph.topological_sort graph in
      List.iteri
        (fun i node ->
          let open Dep_graph in
          let file_str = match node.file_kind with
            | ML -> ".ml"
            | MLI -> ".mli"
            | C -> ".c"
            | H -> ".h"
            | Other ext -> ext
          in
          let node_str = match node.node_kind with
            | File -> ""
            | Generated -> " [gen]"
          in
          Printf.printf "%2d. %s (%s) [%s]%s\n" (i + 1) node.file node.namespaced file_str node_str)
        sorted
  | [] ->
      Printf.eprintf "Usage: tusk-depgraph <directory>\n";
      Printf.eprintf "Example: tusk-depgraph ./packages/miniriot\n";
      exit 1
