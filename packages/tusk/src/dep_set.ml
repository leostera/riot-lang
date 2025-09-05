(** Dependency-ordered set of modules using ocamldep for sorting *)

open Std

type t = {
  name : string;
  toolchain : Toolchains.toolchain;
  tree : Mod_tree.t;
  mutable remaining_nodes : Mod_tree.t list;
      (* Flattened list of nodes to iterate in dependency order *)
}

(** Sort modules within a package/library and return them in dependency order *)
let rec sort_all_files ~toolchain ~tree =
  (* Sort modules at the current level (within a package or library) and then
     recursively process children, returning a depth-first traversal *)
  let rec sort_tree tree =
    match tree with
    | Mod_tree.Package { children; aliases; entry_point; name; _ } ->
        (* Separate direct module children from sub-packages/libraries *)
        let module_children, subtree_children =
          List.partition
            (function Mod_tree.Module _ -> true | _ -> false)
            children
        in

        (* Collect all modules at this level (including direct Module children) *)
        let level_modules =
          let modules =
            match entry_point with
            | Some info -> [ Mod_tree.Module info ]
            | None -> []
          in
          let modules =
            List.fold_left
              (fun acc a -> Mod_tree.Module a :: acc)
              modules aliases
          in
          modules @ module_children
        in

        (* Sort modules at this level using ocamldep *)
        let sorted_level_modules =
          if level_modules = [] then []
          else sort_modules_with_ocamldep ~toolchain level_modules
        in

        (* Recursively sort sub-packages/libraries *)
        let sorted_subtrees = List.concat_map sort_tree subtree_children in

        (* Return: this level's modules first, then subtrees *)
        (* This ensures parent modules are available for subfolder modules *)
        sorted_level_modules @ sorted_subtrees
    | Mod_tree.Library { children; aliases; folder_interface; name; _ } ->
        (* Separate direct module children from sub-packages/libraries *)
        let module_children, subtree_children =
          List.partition
            (function Mod_tree.Module _ -> true | _ -> false)
            children
        in

        (* Collect all modules at this level (including direct Module children) *)
        let level_modules =
          let modules =
            match folder_interface with
            | Some info -> [ Mod_tree.Module info ]
            | None -> []
          in
          let modules =
            List.fold_left
              (fun acc a -> Mod_tree.Module a :: acc)
              modules aliases
          in
          modules @ module_children
        in

        (* Sort modules at this level *)
        let sorted_level_modules =
          if level_modules = [] then []
          else sort_modules_with_ocamldep ~toolchain level_modules
        in

        (* Recursively sort sub-packages/libraries *)
        let sorted_subtrees = List.concat_map sort_tree subtree_children in

        (* Return: this level's modules first, then subtrees *)
        (* This ensures parent modules are available for subfolder modules *)
        sorted_level_modules @ sorted_subtrees
    | Mod_tree.Module _ as m -> [ m ]
  in

  sort_tree tree

(** Helper to sort a list of modules using ocamldep *)
and sort_modules_with_ocamldep ~toolchain modules =
  let all_nodes = modules in

  Format.eprintf "[DEBUG DepSet.sort_modules] Sorting %d modules@."
    (List.length all_nodes);
  List.iter
    (fun node ->
      match node with
      | Mod_tree.Module (Mod_tree.Concrete { namespaced_name; impl; intf; _ })
        ->
          Format.eprintf
            "[DEBUG DepSet.sort_modules]   Concrete module: %s (impl=%b, \
             intf=%b)@."
            namespaced_name (impl <> None) (intf <> None)
      | Mod_tree.Module (Mod_tree.Generated { simple_name; kind; _ }) ->
          let filename = match kind with
            | Mod_tree.Static { path; _ } | Mod_tree.Dynamic { path } -> Path.to_string path
          in
          if filename <> "" then
            Format.eprintf
              "[DEBUG DepSet.sort_modules]   Generated module: %s@." filename
          else
            Format.eprintf
              "[DEBUG DepSet.sort_modules]   Generated module: %s@." simple_name
      | _ -> Format.eprintf "[DEBUG DepSet.sort_modules]   Other node type@.")
    all_nodes;

  (* Now sort these nodes using ocamldep *)
  match
    Fs.with_tempdir ~prefix:"tusk_depset" (fun temp_dir_path ->
        let temp_dir = Path.to_string temp_dir_path in

        (* Build a map from filename to node *)
        let file_to_node = Hashtbl.create 32 in

        (* Check if we need the alias file for interface dependency resolution *)
        let has_interfaces =
          List.exists
            (function
              | Mod_tree.Module (Mod_tree.Concrete { intf = Some _; _ }) -> true
              | _ -> false)
            all_nodes
        in

        (* If we have interfaces, we need to copy the alias file for proper resolution *)
        (* Look for the alias module in the nodes and copy it first *)
        if has_interfaces then
          List.iter
            (function
              | Mod_tree.Module (Mod_tree.Generated { kind = Mod_tree.Static { contents; path = gen_path }; _ })
                when String.ends_with ~suffix:"__aliases.ml.gen" (Path.to_string gen_path) ->
                  let filename = Path.to_string gen_path in
                  let path = Filename.concat temp_dir filename in
                  let oc = open_out path in
                  output_string oc contents;
                  close_out oc;
                  (* Compile the alias .cmi so ocamldep can resolve module references *)
                  let compile_cmd =
                    Printf.sprintf
                      "cd %s && %s -c -no-alias-deps -impl %s -o %s.cmi \
                       2>/dev/null"
                      temp_dir
                      (Toolchains.ocamlc_path toolchain)
                      filename
                      (Filename.chop_suffix filename ".ml.gen")
                  in
                  let _ =
                    Command.exec compile_cmd ~args:[] ()
                    |> Result.expect
                         ~msg:
                           (Printf.sprintf "Failed to compile alias module: %s"
                              compile_cmd)
                  in
                  ()
              | _ -> ())
            all_nodes;

        (* Copy all files to temp dir and build mapping *)
        Format.eprintf "[DEBUG DepSet] About to copy %d nodes to temp dir@."
          (List.length all_nodes);
        List.iter
          (fun node ->
            match node with
            | Mod_tree.Module (Mod_tree.Generated { kind = Mod_tree.Static { contents; path = gen_path }; _ })
              when Path.to_string gen_path <> "" ->
                let filename = Path.to_string gen_path in
                let path = Filename.concat temp_dir filename in
                let oc = open_out path in
                output_string oc contents;
                close_out oc;
                Hashtbl.add file_to_node filename node
            | Mod_tree.Module (Mod_tree.Generated { kind = Mod_tree.Dynamic _; _ }) ->
                (* Skip empty generated modules - these are placeholders *)
                ()
            | Mod_tree.Module
                (Mod_tree.Concrete { impl; intf; namespaced_name; _ }) -> (
                (match impl with
                | Some src ->
                    (* Use original filename to preserve dependencies *)
                    let dest_name = Path.basename src.Build_node.file in
                    let dest = Filename.concat temp_dir dest_name in
                    Format.eprintf "[DEBUG DepSet] Copying %s -> %s@."
                      (Path.to_string src.Build_node.file)
                      dest;
                    Fs.copy_file src.Build_node.file
                      (Path.of_string dest |> Result.expect ~msg:"Invalid path")
                    |> Result.expect ~msg:"Failed to copy";
                    Hashtbl.add file_to_node dest_name node
                | None -> ());
                match intf with
                | Some src ->
                    (* Use original filename to preserve dependencies *)
                    let dest_name = Path.basename src.Build_node.file in
                    let dest = Filename.concat temp_dir dest_name in
                    Format.eprintf "[DEBUG DepSet] Copying %s -> %s@."
                      (Path.to_string src.Build_node.file)
                      dest;
                    Fs.copy_file src.Build_node.file
                      (Path.of_string dest |> Result.expect ~msg:"Invalid path")
                    |> Result.expect ~msg:"Failed to copy";
                    (* For .mli files, we want the same node as the .ml *)
                    if not (Hashtbl.mem file_to_node (namespaced_name ^ ".ml"))
                    then Hashtbl.add file_to_node dest_name node
                | None -> ())
            | _ -> ())
          all_nodes;

        (* Get list of all files we copied *)
        let all_filenames =
          Hashtbl.fold (fun k _ acc -> k :: acc) file_to_node []
        in
        Format.eprintf "[DEBUG DepSet] Copied %d files to temp dir@."
          (List.length all_filenames);

        (* Separate by type *)
        let ml_files =
          List.filter
            (fun f ->
              String.ends_with ~suffix:".ml" f
              || String.ends_with ~suffix:".ml.gen" f)
            all_filenames
        in

        let mli_files =
          List.filter (fun f -> String.ends_with ~suffix:".mli" f) all_filenames
        in

        (* Debug: print files being sorted *)
        Format.eprintf "[DEBUG DepSet] Files to sort in %s:@." temp_dir;
        Format.eprintf "[DEBUG DepSet]   .mli files: %s@."
          (String.concat ", " mli_files);
        Format.eprintf "[DEBUG DepSet]   .ml files: %s@."
          (String.concat ", " ml_files);

        (* Sort using ocamldep - sort .mli and .ml files separately *)
        let sorted_files =
          match (mli_files, ml_files) with
          | [], [] -> []
          | [], ml_files ->
              (* Only .ml files *)
              Format.eprintf "[DEBUG DepSet] Running ocamldep on .ml files...@.";
              let result =
                Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:ml_files
              in
              Format.eprintf "[DEBUG DepSet]   Sorted .ml: %s@."
                (String.concat ", " result);
              result
          | mli_files, [] ->
              (* Only .mli files - sort them by their dependencies *)
              Format.eprintf
                "[DEBUG DepSet] Running ocamldep on .mli files only...@.";
              Format.eprintf "[DEBUG DepSet]   Input .mli files: %s@."
                (String.concat ", " mli_files);
              let result =
                Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:mli_files
              in
              Format.eprintf "[DEBUG DepSet]   Sorted .mli: %s@."
                (String.concat ", " result);
              result
          | mli_files, ml_files ->
              (* Both .mli and .ml files - shouldn't happen in our three-tree approach *)
              (* But if it does, sort each separately and concatenate *)
              Format.eprintf
                "[DEBUG DepSet] Running ocamldep on .mli files...@.";
              let sorted_mli =
                Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:mli_files
              in
              Format.eprintf "[DEBUG DepSet]   Sorted .mli: %s@."
                (String.concat ", " sorted_mli);
              Format.eprintf "[DEBUG DepSet] Running ocamldep on .ml files...@.";
              let sorted_ml =
                Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:ml_files
              in
              Format.eprintf "[DEBUG DepSet]   Sorted .ml: %s@."
                (String.concat ", " sorted_ml);
              sorted_mli @ sorted_ml
        in

        Format.eprintf "[DEBUG DepSet] Final sorted order: %s@."
          (String.concat ", " sorted_files);

        (* Map back to nodes in sorted order *)
        (* Important: keep nodes in exact dependency order without deduplication
           since interface and implementation trees are processed separately *)
        let sorted_nodes =
          List.filter_map
            (fun filename ->
              match Hashtbl.find_opt file_to_node filename with
              | Some node -> Some node
              | None -> None)
            sorted_files
        in

        sorted_nodes)
  with
  | Ok result -> result
  | Error _ -> all_nodes (* Fall back to unsorted on error *)

(** Create a DepSet from a ModTree *)
let create ~name ~toolchain ~tree =
  (* Sort ALL files in the tree using ocamldep *)
  Format.eprintf "[DEBUG DepSet.create] Creating DepSet for %s@." name;
  let sorted_nodes = sort_all_files ~toolchain ~tree in
  Format.eprintf "[DEBUG DepSet.create] %s: Sorted %d nodes@." name
    (List.length sorted_nodes);
  { name; toolchain; tree; remaining_nodes = sorted_nodes }

(** Iterator interface *)
module Iterator = struct
  type state = t
  type item = Mod_tree.t

  let next state =
    match state.remaining_nodes with
    | [] -> None
    | node :: rest ->
        state.remaining_nodes <- rest;
        Some node

  let size state = List.length state.remaining_nodes
end

let iter f t = List.iter f t.remaining_nodes
let to_list t = t.remaining_nodes
let size t = List.length t.remaining_nodes
