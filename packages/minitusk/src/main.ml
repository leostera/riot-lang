(** Minitusk - Minimal OCaml build system

    A self-contained build system that can bootstrap itself and build OCaml
    packages with proper module namespacing and nested library support. *)

(* ===== Constants ===== *)

let ml_ext = ".ml"
let mli_ext = ".mli"
let c_ext = ".c"
let h_ext = ".h"
let cmo_ext = ".cmo"
let cmi_ext = ".cmi"
let cma_ext = ".cma"
let aliases_suffix = "__aliases"

(* ===== Utility Functions ===== *)

let read_file path =
  let ic = open_in path in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  close_in ic;
  content

let write_file path content =
  let oc = open_out path in
  output_string oc content;
  close_out oc

let mkdir_p dir =
  let rec create_dirs path =
    if not (Sys.file_exists path) then (
      create_dirs (Filename.dirname path);
      try Unix.mkdir path 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ())
  in
  create_dirs dir

let copy_file src dst =
  let content = read_file src in
  write_file dst content

let run_command cmd =
  Printf.printf "  $ %s\n%!" cmd;
  let ret = Unix.system cmd in
  match ret with
  | Unix.WEXITED 0 -> ()
  | Unix.WEXITED n ->
      failwith (Printf.sprintf "Command failed with exit code %d" n)
  | Unix.WSIGNALED n ->
      failwith (Printf.sprintf "Command killed by signal %d" n)
  | Unix.WSTOPPED n ->
      failwith (Printf.sprintf "Command stopped by signal %d" n)

(* ===== Package Configuration ===== *)

type package = { name : string; path : string; deps : string list }

(* TOML parser module *)
module Toml = struct
  let parse_file filename =
    let ic = open_in filename in
    let rec parse_lines acc =
      try
        let line = input_line ic in
        let trimmed = String.trim line in
        if String.length trimmed > 0 && trimmed.[0] <> '#' then
          parse_lines (trimmed :: acc)
        else parse_lines acc
      with End_of_file ->
        close_in ic;
        List.rev acc
    in
    parse_lines []

  let get_string_value lines key =
    List.find_map
      (fun line ->
        if String.starts_with ~prefix:(key ^ " = ") line then
          let value =
            String.sub line
              (String.length key + 3)
              (String.length line - String.length key - 3)
          in
          let value = String.trim value in
          if
            String.length value >= 2
            && value.[0] = '"'
            && value.[String.length value - 1] = '"'
          then Some (String.sub value 1 (String.length value - 2))
          else Some value
        else None)
      lines

  let get_array_value lines key =
    let rec find_array = function
      | [] -> []
      | line :: rest ->
          if String.starts_with ~prefix:(key ^ " = [") line then
            (* Handle inline array like: members = ["a", "b"] *)
            if String.contains line ']' then
              let start = String.index line '[' + 1 in
              let end_ = String.index line ']' in
              let content = String.sub line start (end_ - start) in
              parse_inline_array content
            else collect_array rest []
          else find_array rest
    and collect_array lines acc =
      match lines with
      | [] -> List.rev acc
      | line :: rest ->
          let trimmed = String.trim line in
          if trimmed = "]" then List.rev acc
          else if String.ends_with ~suffix:"]" trimmed then
            let item = String.sub trimmed 0 (String.length trimmed - 1) in
            List.rev (parse_array_item item :: acc)
          else collect_array rest (parse_array_item trimmed :: acc)
    and parse_array_item item =
      let item = String.trim item in
      let item =
        if String.ends_with ~suffix:"," item then
          String.sub item 0 (String.length item - 1)
        else item
      in
      let item = String.trim item in
      if
        String.length item >= 2
        && item.[0] = '"'
        && item.[String.length item - 1] = '"'
      then String.sub item 1 (String.length item - 2)
      else item
    and parse_inline_array content =
      let items = String.split_on_char ',' content in
      List.map parse_array_item items
    in
    find_array lines
end

let parse_toml_package path =
  let toml_path = Filename.concat path "tusk.toml" in
  if Sys.file_exists toml_path then
    let lines = Toml.parse_file toml_path in
    (* Find lines after [package] section *)
    let rec find_package_section = function
      | [] -> []
      | line :: rest ->
          if line = "[package]" then rest else find_package_section rest
    in
    let package_lines = find_package_section lines in
    let name =
      match Toml.get_string_value package_lines "name" with
      | Some n -> n
      | None -> Filename.basename path
    in
    { name; path; deps = [] }
  else { name = Filename.basename path; path; deps = [] }

module Ocamldep = struct

(** OCamldep wrapper for dependency analysis *)

let ocamldep_path =
  let home = try Sys.getenv "HOME" with Not_found -> "/Users/ostera" in
  Filename.concat home ".tusk/toolchains/5.3.0/bin/ocamldep"

(** Run ocamldep to get module dependencies for a file *)
let get_deps ~(cwd : string) ~(file : string) ?(open_modules = []) () =
  let full_path = Filename.concat cwd file in

  (* Build command arguments *)
  let args = [
    "-I"; cwd;
    "-modules";
    full_path
  ] in

  (* Add open modules *)
  let args = List.fold_left (fun acc m -> "-open" :: m :: acc) args open_modules in
  let args = List.rev args in

  let cmd = String.concat " " (ocamldep_path :: (List.map (Printf.sprintf "%S") args)) in

  try
    let ic = Unix.open_process_in cmd in
    let line = try Some (input_line ic) with End_of_file -> None in
    let _ = Unix.close_process_in ic in
    line
  with _ -> None

(** Parse ocamldep output to extract module names *)
let parse_deps line =
  (* Format: "file.ml: Module1 Module2 Module3" *)
  match String.split_on_char ':' line with
  | [ _file; deps_str ] ->
      let deps = String.trim deps_str in
      if deps = "" then []
      else String.split_on_char ' ' deps |> List.map String.trim
  | _ -> []

(** Sort files in dependency order *)
let sort_files ~(cwd : string) ~(files : string list) =
  if files = [] then []
  else
    (* Build command arguments *)
    let file_paths = List.map (Filename.concat cwd) files in
    let args = "-I" :: cwd :: "-sort" :: file_paths in
    let cmd = String.concat " " (ocamldep_path :: (List.map (Printf.sprintf "%S") args)) in

    try
      let ic = Unix.open_process_in cmd in
      let output = try Some (input_line ic) with End_of_file -> None in
      let _ = Unix.close_process_in ic in
      match output with
      | Some sorted_str when sorted_str <> "" ->
          (* ocamldep returns full paths, convert back to basenames *)
          String.split_on_char ' ' sorted_str
          |> List.filter_map (fun s ->
              if s = "" then None
              else Some (Filename.basename s))
      | _ -> files
    with _ -> files (* Return original list if ocamldep fails *)


end

(* === Module Registry === *)
module Module_registry = struct
  (** Constants for module naming conventions *)
  let namespace_separator = "__"

  (** Convert namespaced parts to string *)
  let namespaced_to_string parts = String.concat namespace_separator parts

  (** Convert string to namespaced parts *)
  let string_to_namespaced str =
    String.split_on_char '_' str |> List.filter (fun s -> s <> "")

  let path_separator = '/'
  let current_dir = "."
  let empty_dir = ""
  let mli_extension = ".mli"
  let ml_extension = ".ml"

  type file_kind = MLI | ML | Alias

  type entry = {
    file : string;
    simple_name : string;
    namespaced : string list;
    kind : file_kind;
    is_library_interface : bool;
  }

  type t = {
    mutable entries : entry list;
    by_simple : (string, entry list) Hashtbl.t;
    by_namespaced : (string, entry list) Hashtbl.t;
    package_name : string;
  }

  let create ~package_name =
    {
      entries = [];
      by_simple = Hashtbl.create 100;
      by_namespaced = Hashtbl.create 100;
      package_name;
    }

  (** Convert a file path to a module name, handling subdirectories *)
  let module_name_from_path path =
        let stem_path = Filename.remove_extension path in
        let stem = Filename.basename stem_path in
        let dir = Filename.dirname path in

        (* Build module parts from directory structure *)
        let module_parts =
          if dir = current_dir || dir = empty_dir then [ stem ]
          else String.split_on_char path_separator dir @ [ stem ]
        in

        (* Capitalize each part and join with namespace separator *)
        module_parts
        |> List.map String.capitalize_ascii
        |> String.concat namespace_separator

  (** Create a namespaced module name *)
  let make_namespaced registry module_name =
    [ registry.package_name; module_name ]

  (** Create an entry from a file path *)
  let entry_from_file registry file =
    let ext_opt =
      try
        let idx = String.rindex file '.' in
        Some (String.sub file idx (String.length file - idx))
      with Not_found -> None
    in
    let kind =
      match ext_opt with
      | Some ext when ext = mli_extension -> MLI
      | Some ext when ext = ml_extension -> ML
      | _ -> failwith ("Unexpected file extension: " ^ file)
    in

    let stem_path = Filename.remove_extension file in
    let simple_name = Filename.basename stem_path |> String.capitalize_ascii in
    let module_name = module_name_from_path file in
    let namespaced = make_namespaced registry module_name in

    {
      file;
      simple_name;
      namespaced;
      kind;
      is_library_interface = false;
      (* TODO: detect library interfaces *)
    }

  let register registry entry =
    registry.entries <- entry :: registry.entries;

    (* Add to simple name index *)
    let simple_entries =
      try Hashtbl.find registry.by_simple entry.simple_name
      with Not_found -> []
    in
    Hashtbl.replace registry.by_simple entry.simple_name
      (entry :: simple_entries);

    (* Add to namespaced index *)
    let namespaced_key = namespaced_to_string entry.namespaced in
    let ns_entries =
      try Hashtbl.find registry.by_namespaced namespaced_key
      with Not_found -> []
    in
    Hashtbl.replace registry.by_namespaced namespaced_key (entry :: ns_entries)

  let find_by_simple_name registry name =
    try Hashtbl.find registry.by_simple name with Not_found -> []

  let find_by_namespaced registry name_parts =
    let name = namespaced_to_string name_parts in
    try Hashtbl.find registry.by_namespaced name with Not_found -> []

  let all_entries registry = List.rev registry.entries

  let dump registry =
    Printf.printf "Module Registry (%d entries):\n"
      (List.length registry.entries);
    List.iter
      (fun entry ->
        let kind_str =
          match entry.kind with
          | MLI -> " [.mli]"
          | ML -> " [.ml]"
          | Alias -> " [alias]"
        in
        Printf.printf "  %s -> %s%s%s\n" entry.file
          (namespaced_to_string entry.namespaced)
          kind_str
          (if entry.is_library_interface then " [library-interface]" else ""))
      (all_entries registry)
end

(* ===== Node ID Management ===== *)

module Node_id = struct
  type t = int

  let counter = ref 0

  let next () =
    incr counter;
    !counter

  let to_int id = id
  let equal = ( = )
  let eq = equal
end

(* ===== Dependency Graph ===== *)

module DepGraph = struct
  (** Constants *)
  let ml_gen_extension = ".ml.gen"

  let aliases_suffix = "__aliases"
  let src_dir = "src"
  let current_dir = "."

  type kind =
    | ML of { module_name : string; namespaced : string list }
    | MLI of { module_name : string; namespaced : string list }
    | C
    | H
    | Other of string

  type file =
    | Concrete of string
    | Generated of { path : string; contents : string }

  type node = {
    id : Node_id.t;
    file : file;
    mutable deps : Node_id.t list;
    kind : kind;
  }

  type t = {
    root : string;
    nodes : (int, node) Hashtbl.t;
    registry : Module_registry.t;
    package_name : string;
  }

  let add_node graph file kind =
    let id = Node_id.next () in
    let node = { id; file; kind; deps = [] } in
    Hashtbl.add graph.nodes (Node_id.to_int id) node;
    node

  let find_node_by_file graph file_path =
    (* Convert to absolute path for comparison *)
    let target_path =
      if String.contains file_path '/' then
        Filename.concat (Filename.concat graph.root "src") file_path
      else Filename.concat (Filename.concat graph.root "src") file_path
    in
    Hashtbl.fold
      (fun _ node acc ->
        match acc with
        | Some _ -> acc
        | None ->
            let node_path =
              match node.file with
              | Concrete path -> path
              | Generated { path; _ } -> path
            in
            if node_path = target_path then Some node else None)
      graph.nodes None

  let make ~root ~package_name =
    let registry = Module_registry.create ~package_name in
    let graph = { root; nodes = Hashtbl.create 100; package_name; registry } in
    graph

  (** Get kind from extension and module info *)
  let kind_of_extension ext ~module_name ~namespaced ~is_package_main =
    match ext with
    | ".mli" ->
        if is_package_main then
          MLI { module_name = String.capitalize_ascii (List.hd namespaced); namespaced = [String.capitalize_ascii (List.hd namespaced)] }
        else
          MLI { module_name; namespaced }
    | ".ml" ->
        if is_package_main then
          ML { module_name = String.capitalize_ascii (List.hd namespaced); namespaced = [String.capitalize_ascii (List.hd namespaced)] }
        else
          ML { module_name; namespaced }
    | ".c" -> C
    | ".h" -> H
    | other -> Other other

  (** Check if kind is an OCaml source file *)
  let is_ocaml_source kind = match kind with ML _ | MLI _ -> true | _ -> false

  (** Recursive directory scanning that builds the graph as it goes *)
  let rec scan_directory graph ~current_path ~relative_path ~namespace =
    Printf.printf "Scanning: %s (namespace: [%s])\n"
      current_path
      (String.concat "; " namespace);

    (* First, collect all entries in the directory *)
    let sources =
      if Sys.file_exists current_path && Sys.is_directory current_path then
        Sys.readdir current_path |> Array.to_list
      else
        failwith ("Could not read directory: " ^ current_path)
    in

    (* Separate files and directories *)
    let files, dirs =
      List.partition
        (fun entry ->
          let entry_path = Filename.concat current_path entry in
          not (Sys.is_directory entry_path))
        sources
    in

    (* Get library interface node for this directory (always exists for non-root) *)
    let library_interface_node =
      if relative_path = "" then None
        (* Root directory doesn't need a library interface *)
      else
        let dir_name = Filename.basename current_path in
        let module_name = String.capitalize_ascii dir_name in

        (* Check if user provided the library interface file *)
        let user_provided_interface =
          List.find_opt
            (fun file ->
              let file_name = Filename.remove_extension file in
              file_name = dir_name
              &&
              let ext =
                try
                  let idx = String.rindex file '.' in
                  String.sub file idx (String.length file - idx)
                with Not_found -> ""
              in
              ext = ".ml" || ext = ".mli")
            files
        in

        (* Always create the library interface node *)
        let file, is_generated =
          match user_provided_interface with
          | Some interface_file ->
              (* User provided it - create node for the actual file *)
              let interface_path = Filename.concat current_path interface_file in
              (Concrete interface_path, false)
          | None ->
              (* Generate library interface file *)
              let interface_path = Filename.concat current_path (dir_name ^ ".ml") in
              let file =
                Generated
                  {
                    path = interface_path;
                    contents =
                      Printf.sprintf
                        "(* Auto-generated library interface for %s *)" dir_name;
                  }
              in
              (file, true)
        in

        let file_ext =
          match user_provided_interface with
          | Some f ->
              (try
                let idx = String.rindex f '.' in
                String.sub f idx (String.length f - idx)
              with Not_found -> ".ml")
          | None -> ".ml"
        in
        let kind =
          if file_ext = ".mli" then MLI { module_name; namespaced = namespace }
          else ML { module_name; namespaced = namespace }
        in
        let node = add_node graph file kind in

        (* Register in module registry with correct path and namespacing *)
        let entry_data =
          {
            Module_registry.file = relative_path ^ "/" ^ dir_name ^ file_ext;
            simple_name = module_name;
            namespaced = namespace;
            kind =
              (if file_ext = ".mli" then Module_registry.MLI
               else Module_registry.ML);
            is_library_interface = true;
          }
        in
        Module_registry.register graph.registry entry_data;

        if is_generated then
          Printf.printf "  Added generated library interface: %s/%s.ml\n"
            relative_path dir_name
        else
          Printf.printf "  Found user-provided library interface: %s/%s\n"
            relative_path
            (Filename.basename
               (match file with
               | Concrete p -> p
               | Generated { path; _ } -> path));

        Some node
    in

    (* Create alias file for this directory *)
    let alias_node =
      let alias_name = String.concat "__" namespace ^ "__aliases" in
      let alias_path = Filename.concat current_path (alias_name ^ ".ml") in
      let kind =
        ML { module_name = alias_name; namespaced = namespace @ [ "Aliases" ] }
      in
      let file =
        Generated
          { path = alias_path; contents = "(* Auto-generated aliases *)" }
      in
      let node = add_node graph file kind in
      Printf.printf "  Added alias file: %s\n" (Filename.basename alias_path);
      node
    in

    (* First register ALL OCaml modules before processing *)
    List.iter
      (fun entry ->
        let entry_path = Filename.concat current_path entry in
        let entry_str = entry in
        let entry_relative =
          if relative_path = "" then entry_str
          else relative_path ^ "/" ^ entry_str
        in

        (* Skip if this is the library interface file we already registered *)
        let dir_name = Filename.basename current_path in
        let file_name = Filename.remove_extension entry in
        let is_library_interface =
          relative_path <> "" && file_name = dir_name
          && (let ext =
                try
                  let idx = String.rindex entry '.' in
                  String.sub entry idx (String.length entry - idx)
                with Not_found -> ""
              in
              ext = ".ml" || ext = ".mli")
        in

        if not is_library_interface then
          let ext =
            try
              let idx = String.rindex entry '.' in
              String.sub entry idx (String.length entry - idx)
            with Not_found -> ""
          in
          let module_name = String.capitalize_ascii file_name in
          let full_namespaced = namespace @ [ module_name ] in

          (* Check if this is the main package file (e.g., kernel.ml for kernel package) *)
          let is_package_main =
            String.lowercase_ascii file_name = String.lowercase_ascii (List.hd namespace)
          in

          let kind =
            kind_of_extension ext ~module_name ~namespaced:full_namespaced ~is_package_main
          in

          if is_ocaml_source kind then
            (* Register in module registry *)
            let entry_data =
              {
                Module_registry.file = entry_relative;
                simple_name = module_name;
                namespaced = full_namespaced;
                kind =
                  (match kind with
                  | ML _ -> Module_registry.ML
                  | MLI _ -> Module_registry.MLI
                  | _ -> Module_registry.ML);
                is_library_interface = false;
              }
            in
            Module_registry.register graph.registry entry_data)
      files;

    (* Now process all files and create nodes *)
    let file_nodes =
      List.filter_map
        (fun entry ->
          let entry_path = Filename.concat current_path entry in
          let entry_str = entry in
          let entry_relative =
            if relative_path = "" then entry_str
            else relative_path ^ "/" ^ entry_str
          in

          (* Skip if this is the library interface file we already processed *)
          let dir_name = Filename.basename current_path in
          let file_name = Filename.remove_extension entry in
          let is_library_interface =
            relative_path <> "" && file_name = dir_name
            && (let ext =
                  try
                    let idx = String.rindex entry '.' in
                    String.sub entry idx (String.length entry - idx)
                  with Not_found -> ""
                in
                ext = ".ml" || ext = ".mli")
          in

          if is_library_interface then None (* Already processed above *)
          else
            let ext =
              try
                let idx = String.rindex entry '.' in
                String.sub entry idx (String.length entry - idx)
              with Not_found -> ""
            in
            let module_name = String.capitalize_ascii file_name in
            let full_namespaced = namespace @ [ module_name ] in

            (* Check if this is the main package file (e.g., kernel.ml for kernel package) *)
            let is_package_main =
              String.lowercase_ascii file_name = String.lowercase_ascii (List.hd namespace)
            in

            let kind =
              kind_of_extension ext ~module_name ~namespaced:full_namespaced ~is_package_main
            in
            let file = Concrete entry_path in

            if is_ocaml_source kind then (
              (* Create node for OCaml source file *)
              let node = add_node graph file kind in

              Printf.printf
                "  Added OCaml file: %s -> module %s (namespace: [%s])\n"
                entry_relative module_name
                (String.concat "; " full_namespaced);

              (* Make file depend on alias module *)
              node.deps <- alias_node.id :: node.deps;
              Some node)
            else
              (* For non-OCaml files, still create a node but don't register *)
              let node = add_node graph file kind in
              Printf.printf "  Added other file: %s\n" entry_relative;
              Some node)
        files
    in

    (* Recursively process subdirectories *)
    let subdir_nodes =
      List.concat_map
        (fun dir ->
          let entry_path = Filename.concat current_path dir in
          let entry_str = dir in
          let entry_relative =
            if relative_path = "" then entry_str
            else relative_path ^ "/" ^ entry_str
          in
          let dir_name = String.capitalize_ascii entry_str in
          let extended_namespace = namespace @ [ dir_name ] in

          scan_directory graph ~current_path:entry_path
            ~relative_path:entry_relative ~namespace:extended_namespace)
        dirs
    in

    (* Return all nodes created *)
    let all_nodes = file_nodes @ [ alias_node ] in
    let all_nodes =
      match library_interface_node with
      | Some n -> n :: all_nodes
      | None -> all_nodes
    in
    all_nodes @ subdir_nodes

  let scan ~(root : string) ~(package_name : string) =
    let graph = make ~root ~package_name in

    (* Start scanning from src directory *)
    let src_root = Filename.concat root "src" in
    Printf.printf "Starting scan from: %s\n" src_root;

    (* First pass: Build the graph with all nodes *)
    let initial_namespace = [ String.capitalize_ascii package_name ] in
    let nodes =
      scan_directory graph ~current_path:src_root ~relative_path:""
        ~namespace:initial_namespace
    in
    Printf.printf "Created %d nodes total\n" (List.length nodes);

    (* Generate proper alias module content now that we have all modules *)
    Printf.printf "Generating alias module content...\n";
    Hashtbl.iter
      (fun _id node ->
        match node.kind with
        | ML { module_name; _ } when String.ends_with ~suffix:"__aliases" module_name ->
            (* Update the alias module content *)
            let aliases = ref [] in
            Hashtbl.iter
              (fun _ other_node ->
                match other_node.kind with
                | ML { module_name; namespaced } | MLI { module_name; namespaced }
                  when List.length namespaced = 2 &&
                       not (String.ends_with ~suffix:"aliases" module_name) &&
                       not (String.lowercase_ascii module_name = String.lowercase_ascii package_name) ->
                    let simple_name = module_name in
                    let full_name = String.concat "__" namespaced in
                    if not (List.mem_assoc simple_name !aliases) then
                      aliases := (simple_name, full_name) :: !aliases
                | ML { module_name; namespaced } | MLI { module_name; namespaced }
                  when List.length namespaced = 1 &&
                       String.lowercase_ascii module_name = String.lowercase_ascii package_name ->
                    (* This is the main package module - don't add to aliases *)
                    ()
                | _ -> ())
              graph.nodes;

            let content =
              if !aliases = [] then
                "(* Auto-generated aliases - empty *)\n"
              else
                "(* Auto-generated aliases *)\n" ^
                String.concat "\n"
                  (List.map (fun (simple, full) -> "module " ^ simple ^ " = " ^ full) !aliases)
            in

            (* Update the node's file content *)
            (match node.file with
            | Generated { path; _ } ->
                let updated_file = Generated { path; contents = content } in
                let updated_node = { node with file = updated_file } in
                Hashtbl.replace graph.nodes (Node_id.to_int node.id) updated_node;
                Printf.printf "  Updated alias content with %d modules\n" (List.length !aliases)
            | _ -> ())
        | _ -> ())
      graph.nodes;

    (* Second pass: Add basic .mli -> .ml dependencies *)
    Printf.printf "Adding basic dependencies...\n";

    (* Add some manual inter-module dependencies to fix compilation order *)
    Printf.printf "Adding manual inter-module dependencies...\n";
    Hashtbl.iter
      (fun _id node ->
        match node.kind with
        | MLI { module_name = "Effects"; namespaced } | ML { module_name = "Effects"; namespaced } ->
            (* Effects depends on Process *)
            let process_entries = Module_registry.find_by_simple_name graph.registry "Process" in
            List.iter
              (fun process_entry ->
                match find_node_by_file graph process_entry.Module_registry.file with
                | Some process_node ->
                    node.deps <- process_node.id :: node.deps;
                    Printf.printf "  %s depends on Process\n"
                      (String.concat "__" namespaced)
                | None -> ())
              process_entries
        | _ -> ())
      graph.nodes;

    (* Find alias module node *)
    let alias_module =
      Hashtbl.fold
        (fun _ node acc ->
          match acc with
          | Some _ -> acc
          | None ->
              (match node.kind with
              | ML { module_name; _ } when String.ends_with ~suffix:"__aliases" module_name ->
                  Some node
              | _ -> None))
        graph.nodes None
    in

    (* Find package root interface node (e.g., kernel.mli) *)
    let package_root_interface =
      Hashtbl.fold
        (fun _ node acc ->
          match acc with
          | Some _ -> acc
          | None ->
              (match node.kind with
              | MLI { module_name; namespaced }
                when List.length namespaced = 2 &&
                     String.lowercase_ascii module_name = String.lowercase_ascii package_name ->
                  Some node
              | _ -> None))
        graph.nodes None
    in

    Hashtbl.iter
      (fun _id node ->
        match node.kind with
        | ML { namespaced; module_name; _ } ->
            (* Make non-alias modules depend on their directory's alias module *)
            if not (String.ends_with ~suffix:"__aliases" module_name) then (
              (* For main package modules (length=1), use the package root alias *)
              let target_namespace, target_alias_namespace =
                if List.length namespaced = 1 then
                  (* Main package module depends on root alias *)
                  ([List.hd namespaced], [List.hd namespaced; "Aliases"])
                else
                  (* Regular modules depend on their directory alias *)
                  let target_ns = List.rev (List.tl (List.rev namespaced)) in
                  (target_ns, target_ns @ ["Aliases"])
              in

              let dir_alias_node =
                Hashtbl.fold
                  (fun _ alias_node acc ->
                    match acc with
                    | Some _ -> acc
                    | None ->
                        (match alias_node.kind with
                        | ML { namespaced = alias_ns; _ } when alias_ns = target_alias_namespace ->
                            Some alias_node
                        | _ -> None))
                  graph.nodes None
              in

              match dir_alias_node with
              | Some alias_node ->
                  node.deps <- alias_node.id :: node.deps;
                  Printf.printf "  %s depends on %s\n"
                    (String.concat "__" namespaced ^ ".ml")
                    (String.concat "__" target_alias_namespace)
              | None ->
                  Printf.printf "  %s: no alias module found for namespace %s\n"
                    (String.concat "__" namespaced ^ ".ml")
                    (String.concat "__" target_namespace)
            );

            (* Find corresponding .mli file *)
            let mli_node_opt =
              Hashtbl.fold
                (fun _ other_node acc ->
                  match acc with
                  | Some _ -> acc
                  | None ->
                      (match other_node.kind with
                      | MLI { namespaced = other_ns; _ } when other_ns = namespaced ->
                          Some other_node
                      | _ -> None))
                graph.nodes None
            in
            (match mli_node_opt with
            | Some mli_node ->
                node.deps <- mli_node.id :: node.deps;
                Printf.printf "  %s depends on %s\n"
                  (String.concat "__" namespaced ^ ".ml")
                  (String.concat "__" namespaced ^ ".mli")
            | None ->
                Printf.printf "  %s: no interface file\n"
                  (String.concat "__" namespaced ^ ".ml"))
        | MLI { module_name; namespaced } ->
            (* Make non-alias interfaces depend on their directory's alias module *)
            if not (String.ends_with ~suffix:"__aliases" module_name) then (
              (* For main package modules (length=1), use the package root alias *)
              let target_namespace, target_alias_namespace =
                if List.length namespaced = 1 then
                  (* Main package module depends on root alias *)
                  ([List.hd namespaced], [List.hd namespaced; "Aliases"])
                else
                  (* Regular modules depend on their directory alias *)
                  let target_ns = List.rev (List.tl (List.rev namespaced)) in
                  (target_ns, target_ns @ ["Aliases"])
              in

              let dir_alias_node =
                Hashtbl.fold
                  (fun _ alias_node acc ->
                    match acc with
                    | Some _ -> acc
                    | None ->
                        (match alias_node.kind with
                        | ML { namespaced = alias_ns; _ } when alias_ns = target_alias_namespace ->
                            Some alias_node
                        | _ -> None))
                  graph.nodes None
              in

              match dir_alias_node with
              | Some alias_node ->
                  node.deps <- alias_node.id :: node.deps;
                  Printf.printf "  %s depends on %s\n"
                    (String.concat "__" namespaced ^ ".mli")
                    (String.concat "__" target_alias_namespace)
              | None ->
                  Printf.printf "  %s: no alias module found for namespace %s\n"
                    (String.concat "__" namespaced ^ ".mli")
                    (String.concat "__" target_namespace)
            );

            (* If this is the package root interface, make it depend on all other interfaces *)
            (match package_root_interface with
            | Some root_iface when Node_id.equal node.id root_iface.id ->
                Hashtbl.iter
                  (fun _ other_node ->
                    match other_node.kind with
                    | MLI { namespaced = other_ns; _ }
                      when List.length other_ns = 2 &&
                           not (Node_id.equal node.id other_node.id) &&
                           not (String.ends_with ~suffix:"aliases" (List.nth other_ns 1)) ->
                        node.deps <- other_node.id :: node.deps;
                        Printf.printf "  %s depends on %s\n"
                          (String.concat "__" namespaced ^ ".mli")
                          (String.concat "__" other_ns ^ ".mli")
                    | _ -> ())
                  graph.nodes
            | _ -> ())
        | _ -> ())
      graph.nodes;

    graph

  (* Simple debug output functions *)
  let print_graph graph =
    Printf.printf "Dependency Graph for %s:\n" graph.package_name;
    Hashtbl.iter
      (fun _id node ->
        let label =
          match node.file with
          | Concrete path -> Filename.basename path
          | Generated { path; _ } -> Filename.basename path
        in
        let kind_str =
          match node.kind with
          | ML _ -> "ML"
          | MLI _ -> "MLI"
          | C -> "C"
          | H -> "H"
          | Other s -> s
        in
        Printf.printf "  Node %d: %s [%s]\n" (Node_id.to_int node.id) label kind_str;
        if node.deps <> [] then
          Printf.printf "    deps: %s\n"
            (String.concat ", " (List.map (fun id -> string_of_int (Node_id.to_int id)) node.deps)))
      graph.nodes

  let iter_nodes graph f =
    Hashtbl.iter (fun _ node -> f node) graph.nodes

  let topological_sort graph =
    (* Kahn's algorithm *)
    let in_degree = Hashtbl.create (Hashtbl.length graph.nodes) in

    (* Initialize in-degrees - using int key for in_degree table *)
    Hashtbl.iter (fun int_id _ -> Hashtbl.add in_degree int_id 0) graph.nodes;

    (* Calculate in-degrees *)
    Hashtbl.iter
      (fun _ node ->
        List.iter
          (fun dep_id ->
            let dep_int_id = Node_id.to_int dep_id in
            let count = Hashtbl.find in_degree dep_int_id in
            Hashtbl.replace in_degree dep_int_id (count + 1))
          node.deps)
      graph.nodes;

    (* Find nodes with no incoming edges *)
    let queue = Queue.create () in
    Hashtbl.iter
      (fun int_id count -> if count = 0 then Queue.add int_id queue)
      in_degree;

    (* Process queue *)
    let sorted = ref [] in
    while not (Queue.is_empty queue) do
      let int_id = Queue.take queue in
      let node = Hashtbl.find graph.nodes int_id in
      sorted := node :: !sorted;

      (* Decrease in-degree of dependent nodes *)
      List.iter
        (fun dep_id ->
          let dep_int_id = Node_id.to_int dep_id in
          let count = Hashtbl.find in_degree dep_int_id in
          let new_count = count - 1 in
          Hashtbl.replace in_degree dep_int_id new_count;
          if new_count = 0 then Queue.add dep_int_id queue)
        node.deps
    done;

    !sorted
end

(* ===== Module Naming ===== *)

let get_module_info ~package_name ~rel_path =
  (* Convert path like "net/http/header.ml" to module_name="Header", namespaced=["Std"; "Net"; "Http"; "Header"] *)
  let safe_pkg =
    String.map (fun c -> if c = '-' then '_' else c) package_name
  in
  let pkg_name = String.capitalize_ascii safe_pkg in

  if rel_path = "lib.ml" || rel_path = "lib.mli" then
    let module_name = pkg_name in
    let namespaced = [ module_name ] in
    (module_name, namespaced)
  else
    let parts = String.split_on_char '/' rel_path in
    let parts = List.map Filename.chop_extension parts in
    let parts = List.map String.capitalize_ascii parts in
    let module_name = List.hd (List.rev parts) in
    let namespaced = pkg_name :: parts in
    (module_name, namespaced)

(* ===== File Scanning ===== *)

let scan_sources src_dir package_name =
  let root_dir = Filename.dirname src_dir in
  DepGraph.scan ~root:root_dir ~package_name

(* ===== Dependency Analysis ===== *)

(* Dependency analysis is now handled by DepGraph.scan *)

(* ===== Build Execution ===== *)

let build_package pkg ~built_packages =
  Printf.printf "\n=== Building package: %s ===\n" pkg.name;

  let src_dir = Filename.concat pkg.path "src" in
  if not (Sys.file_exists src_dir) then
    failwith (Printf.sprintf "Source directory not found: %s" src_dir);

  (* Create build directories *)
  let sandbox_dir = Printf.sprintf "./target/bootstrap/sandbox/%s" pkg.name in
  let out_dir = Printf.sprintf "./target/bootstrap/out/%s" pkg.name in
  mkdir_p sandbox_dir;
  mkdir_p out_dir;

  (* Scan sources and build dependency graph *)
  let graph = scan_sources src_dir pkg.name in

  (* Copy concrete files to sandbox *)
  DepGraph.iter_nodes graph (fun node ->
      match node.DepGraph.file with
      | DepGraph.Concrete path ->
          (* Preserve directory structure *)
          let rel_path =
            let src_prefix = src_dir ^ "/" in
            if String.starts_with ~prefix:src_prefix path then
              String.sub path (String.length src_prefix)
                (String.length path - String.length src_prefix)
            else Filename.basename path
          in
          let dst_path = Filename.concat sandbox_dir rel_path in
          mkdir_p (Filename.dirname dst_path);
          copy_file path dst_path
      | DepGraph.Generated { path; contents } ->
          let dst = Filename.concat sandbox_dir (Filename.basename path) in
          write_file dst contents);

  (* Setup compiler *)
  let home = Sys.getenv "HOME" in
  let ocamlc = Printf.sprintf "%s/.tusk/toolchains/5.3.0/bin/ocamlc" home in

  (* Build include paths for dependencies *)
  let dep_include_paths =
    List.fold_left
      (fun acc dep_pkg ->
        let dep_out_dir = Printf.sprintf "./target/bootstrap/out/%s" dep_pkg in
        if Sys.file_exists dep_out_dir then
          (" -I " ^ dep_out_dir) :: acc
        else acc)
      []
      pkg.deps
  in
  let dep_includes = String.concat "" dep_include_paths in

  (* Get compilation order *)
  let sorted = DepGraph.topological_sort graph in

  (* Debug: Print compilation order *)
  Printf.printf "Compilation order:\n";
  List.iteri
    (fun i node ->
      match node.DepGraph.kind with
      | DepGraph.ML { namespaced; _ } | DepGraph.MLI { namespaced; _ } ->
          Printf.printf "  %d. %s\n" (i + 1) (String.concat "__" namespaced)
      | _ -> ())
    sorted;

  (* Compile each file *)
  let compiled = ref [] in
  List.iter
    (fun node ->
      match node.DepGraph.kind with
      | DepGraph.ML { module_name; namespaced } | DepGraph.MLI { module_name; namespaced } ->
          let is_mli =
            match node.kind with DepGraph.MLI _ -> true | _ -> false
          in
          let output_ext = if is_mli then ".cmi" else ".cmo" in

          let src_file =
            match node.file with
            | DepGraph.Concrete path -> Filename.basename path
            | DepGraph.Generated { path; _ } -> Filename.basename path
          in

          let name = String.concat "__" namespaced in
          let output = name ^ output_ext in

          (* Determine if we need to open an alias module *)
          let open_flag =
            if String.ends_with ~suffix:"__aliases" module_name then
              " -no-alias-deps -w -49"
            else
              (* For main package modules (length=1), use the package root alias *)
              let target_namespace, target_alias_name =
                if List.length namespaced = 1 then
                  (* Main package module opens root alias *)
                  let ns = [List.hd namespaced] in
                  (ns, String.concat "__" (ns @ ["Aliases"]))
                else
                  (* Regular modules open their directory alias *)
                  let target_ns = List.rev (List.tl (List.rev namespaced)) in
                  (target_ns, String.concat "__" (target_ns @ ["Aliases"]))
              in

              let alias_exists =
                Hashtbl.fold
                  (fun _ node acc ->
                    match node.DepGraph.kind with
                    | DepGraph.ML { namespaced = alias_ns; _ }
                      when alias_ns = target_namespace @ ["Aliases"] -> true
                    | _ -> acc)
                  graph.nodes false
              in
              let open_flags = if alias_exists then [" -open " ^ target_alias_name] else [] in

              (* Add opens for external package dependencies *)
              let external_opens =
                List.fold_left
                  (fun acc dep_pkg ->
                    let dep_alias = String.capitalize_ascii (String.map (fun c -> if c = '-' then '_' else c) dep_pkg) ^ "__Aliases" in
                    (" -open " ^ dep_alias) :: acc)
                  []
                  pkg.deps
              in

              String.concat "" (open_flags @ external_opens)
          in

          let cmd =
            Printf.sprintf "cd %s && %s -I +unix -I . %s %s -c -o %s %s"
              sandbox_dir ocamlc dep_includes open_flag output src_file
          in

          run_command cmd;

          if (not is_mli) && not (String.ends_with ~suffix:"__aliases" module_name)
          then compiled := output :: !compiled
      | DepGraph.C ->
          let src_file =
            match node.file with
            | DepGraph.Concrete path -> Filename.basename path
            | _ -> failwith "C files cannot be generated"
          in
          let cmd =
            Printf.sprintf "cd %s && %s -I +unix %s -c %s" sandbox_dir ocamlc
              dep_includes src_file
          in
          run_command cmd;

          let obj_file = Filename.chop_extension src_file ^ ".o" in
          compiled := obj_file :: !compiled
      | DepGraph.H ->
          (* Header files don't need compilation, they're just dependencies *)
          ()
      | DepGraph.Other _ ->
          (* Other files don't need compilation *)
          ())
    sorted;

  (* Create library archive *)
  let cma_file = pkg.name ^ ".cma" in
  if !compiled <> [] then (
    let objs = String.concat " " (List.rev !compiled) in
    let cmd =
      Printf.sprintf "cd %s && %s %s -a -o %s %s" sandbox_dir ocamlc dep_includes cma_file objs
    in
    run_command cmd;

    (* Copy outputs *)
    let cp_cmd = Printf.sprintf "cp %s/%s %s/" sandbox_dir cma_file out_dir in
    run_command cp_cmd;

    let cp_cmd =
      Printf.sprintf "cp %s/*.cmi %s/ 2>/dev/null || true" sandbox_dir out_dir
    in
    ignore (Unix.system cp_cmd));

  Printf.printf "Package %s built successfully!\n" pkg.name

(* ===== Main ===== *)

let () =
  (* Simple package configuration *)
  let packages =
    [
      { name = "kernel"; path = "packages/kernel"; deps = [] };
      { name = "miniriot"; path = "packages/miniriot"; deps = [ "kernel" ] };
      { name = "std"; path = "packages/std"; deps = [ "kernel"; "miniriot" ] };
    ]
  in

  Printf.printf "=== Minitusk Build System ===\n";
  Printf.printf "Building %d packages\n" (List.length packages);

  (* Build each package in order, tracking built packages *)
  let built_packages = ref [] in
  List.iter
    (fun pkg ->
      build_package pkg ~built_packages:!built_packages;
      built_packages := pkg.name :: !built_packages)
    packages;

  Printf.printf "\n=== Build complete! ===\n"
