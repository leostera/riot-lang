(** Dependency-ordered set of modules using ocamldep for sorting *)

open Std

type t = {
  name : string;
  toolchain : Toolchains.toolchain;
  tree : Mod_tree.t;
  mutable remaining_nodes : Mod_tree.t list;
      (* Flattened list of nodes to iterate in dependency order *)
}

(** Collect all files from a tree and sort them using ocamldep *)
let sort_all_files ~toolchain ~tree =
  (* First, collect ALL nodes from the tree into a flat list *)
  let rec collect_nodes acc tree =
    match tree with
    | Mod_tree.Package { children; aliases; entry_point; _ } ->
        let acc = 
          match entry_point with
          | Some info -> Mod_tree.Module info :: acc
          | None -> acc
        in
        let acc = List.fold_left (fun acc a -> Mod_tree.Module a :: acc) acc aliases in
        List.fold_left collect_nodes acc children
    | Mod_tree.Library { children; aliases; folder_interface; _ } ->
        let acc = 
          match folder_interface with
          | Some info -> Mod_tree.Module info :: acc
          | None -> acc
        in
        let acc = List.fold_left (fun acc a -> Mod_tree.Module a :: acc) acc aliases in
        List.fold_left collect_nodes acc children
    | Mod_tree.Module _ as m -> m :: acc
  in
  
  let all_nodes = collect_nodes [] tree |> List.rev in
  
  (* Now sort these nodes using ocamldep *)
  match
    Fs.with_tempdir ~prefix:"tusk_depset" (fun temp_dir_path ->
        let temp_dir = Path.to_string temp_dir_path in
        
        (* Build a map from filename to node *)
        let file_to_node = Hashtbl.create 32 in
        
        (* Copy all files to temp dir and build mapping *)
        List.iter
          (fun node ->
            match node with
            | Mod_tree.Module (Mod_tree.Generated { filename; contents; _ }) ->
                let path = Filename.concat temp_dir filename in
                let oc = open_out path in
                output_string oc contents;
                close_out oc;
                Hashtbl.add file_to_node filename node
            | Mod_tree.Module (Mod_tree.Concrete { impl; intf; namespaced_name; _ }) -> (
                (match impl with
                | Some src ->
                    (* Use namespaced name for the file in temp dir *)
                    let dest_name = namespaced_name ^ ".ml" in
                    let dest = Filename.concat temp_dir dest_name in
                    Format.eprintf "[DEBUG DepSet] Copying %s -> %s@."
                      (Path.to_string src.Build_node.file) dest;
                    Fs.copy_file src.Build_node.file
                      (Path.of_string dest
                      |> Result.expect ~msg:"Invalid path")
                    |> Result.expect ~msg:"Failed to copy";
                    Hashtbl.add file_to_node dest_name node
                | None -> ());
                match intf with
                | Some src ->
                    (* Use namespaced name for the file in temp dir *)
                    let dest_name = namespaced_name ^ ".mli" in
                    let dest = Filename.concat temp_dir dest_name in
                    Format.eprintf "[DEBUG DepSet] Copying %s -> %s@."
                      (Path.to_string src.Build_node.file) dest;
                    Fs.copy_file src.Build_node.file
                      (Path.of_string dest
                      |> Result.expect ~msg:"Invalid path")
                    |> Result.expect ~msg:"Failed to copy";
                    (* For .mli files, we want the same node as the .ml *)
                    if not (Hashtbl.mem file_to_node (namespaced_name ^ ".ml")) then
                      Hashtbl.add file_to_node dest_name node
                | None -> ())
            | _ -> ())
          all_nodes;
        
        (* Get list of all files we copied *)
        let all_filenames =
          Hashtbl.fold (fun k _ acc -> k :: acc) file_to_node []
        in
        
        (* Separate by type *)
        let ml_files =
          List.filter
            (fun f ->
              String.ends_with ~suffix:".ml" f
              || String.ends_with ~suffix:".ml.gen" f)
            all_filenames
        in
        
        let mli_files =
          List.filter
            (fun f -> String.ends_with ~suffix:".mli" f)
            all_filenames
        in
        
        (* Debug: print files being sorted *)
        Format.eprintf "[DEBUG DepSet] Files to sort in %s:@." temp_dir;
        Format.eprintf "[DEBUG DepSet]   .mli files: %s@."
          (String.concat ", " mli_files);
        Format.eprintf "[DEBUG DepSet]   .ml files: %s@."
          (String.concat ", " ml_files);
        
        (* Sort using ocamldep *)
        let sorted_files =
          let sorted_mli =
            if mli_files = [] then []
            else begin
              Format.eprintf "[DEBUG DepSet] Running ocamldep on .mli files...@.";
              let result = Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:mli_files in
              Format.eprintf "[DEBUG DepSet]   Sorted .mli: %s@."
                (String.concat ", " result);
              result
            end
          in
          let sorted_ml =
            if ml_files = [] then []
            else begin
              Format.eprintf "[DEBUG DepSet] Running ocamldep on .ml files...@.";
              let result = Ocamldep.sort ~toolchain ~cwd:temp_dir ~files:ml_files in
              Format.eprintf "[DEBUG DepSet]   Sorted .ml: %s@."
                (String.concat ", " result);
              result
            end
          in
          sorted_mli @ sorted_ml
        in
        
        Format.eprintf "[DEBUG DepSet] Final sorted order: %s@."
          (String.concat ", " sorted_files);
        
        (* Map back to nodes in sorted order *)
        (* Important: we need to deduplicate - if a module has both .ml and .mli,
           we only want it once in our iteration order *)
        let seen_modules = Hashtbl.create 32 in
        let sorted_nodes =
          List.filter_map
            (fun filename ->
              match Hashtbl.find_opt file_to_node filename with
              | Some node ->
                  (* Extract module name to check for duplicates *)
                  let module_name =
                    if String.ends_with ~suffix:".mli" filename then
                      String.sub filename 0 (String.length filename - 4)
                    else if String.ends_with ~suffix:".ml" filename then
                      String.sub filename 0 (String.length filename - 3)
                    else if String.ends_with ~suffix:".ml.gen" filename then
                      String.sub filename 0 (String.length filename - 7)
                    else filename
                  in
                  if Hashtbl.mem seen_modules module_name then
                    None  (* Already processed this module *)
                  else begin
                    Hashtbl.add seen_modules module_name ();
                    Some node
                  end
              | None -> None)
            sorted_files
        in
        
        sorted_nodes)
  with
  | Ok result -> result
  | Error _ -> all_nodes  (* Fall back to unsorted on error *)

(** Create a DepSet from a ModTree *)
let create ~name ~toolchain ~tree =
  (* Sort ALL files in the tree using ocamldep *)
  let sorted_nodes = sort_all_files ~toolchain ~tree in
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