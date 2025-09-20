type t =
  | CreateDirectory of string
  | WriteFile of { path : string; content : string }
  | CopyFile of { src : string; dst : string }
  | CompileInterface of {
      sandbox_dir : string;
      src_file : string;
      output : string;
      includes : string list;
      opens : string list;
    }
  | CompileImplementation of {
      sandbox_dir : string;
      src_file : string;
      output : string;
      includes : string list;
      opens : string list;
    }
  | CompileC of { sandbox_dir : string; src_file : string }
  | CreateArchive of {
      sandbox_dir : string;
      archive_name : string;
      object_files : string list;
      includes : string list;
    }

let print action =
  match action with
  | CreateDirectory dir -> Printf.printf "CREATE_DIR: %s\n" dir
  | WriteFile { path; content } ->
      Printf.printf "WRITE_FILE: %s (%d bytes)\n" path (String.length content)
  | CopyFile { src; dst } -> Printf.printf "COPY_FILE: %s -> %s\n" src dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } ->
      Printf.printf "COMPILE_MLI: %s -> %s (includes: %s) (opens: %s)\n"
        src_file output
        (String.concat " " includes)
        (String.concat " " opens)
  | CompileImplementation { sandbox_dir; src_file; output; includes; opens } ->
      Printf.printf "COMPILE_ML: %s -> %s (includes: %s) (opens: %s)\n" src_file
        output
        (String.concat " " includes)
        (String.concat " " opens)
  | CompileC { sandbox_dir; src_file } ->
      Printf.printf "COMPILE_C: %s\n" src_file
  | CreateArchive { sandbox_dir; archive_name; object_files; includes } ->
      Printf.printf "CREATE_ARCHIVE: %s (objects: %s) (includes: %s)\n"
        archive_name
        (String.concat " " object_files)
        (String.concat " " includes)

let print_action action =
  match action with
  | CreateDirectory dir -> Printf.printf "CREATE_DIR: %s\n" dir
  | WriteFile { path; _ } -> Printf.printf "WRITE_FILE: %s\n" path
  | CopyFile { src; dst } -> Printf.printf "COPY: %s -> %s\n" src dst
  | CompileInterface { src_file; output; _ } ->
      Printf.printf "COMPILE_INTF: %s -> %s\n" src_file output
  | CompileImplementation { src_file; output; _ } ->
      Printf.printf "COMPILE_IMPL: %s -> %s\n" src_file output
  | CompileC { src_file; _ } -> Printf.printf "COMPILE_C: %s\n" src_file
  | CreateArchive { archive_name; object_files; _ } ->
      Printf.printf "BUILD_ARCHIVE: %s (%d objects)\n" archive_name
        (List.length object_files)

let print_build_plan plan =
  Printf.printf "\n=== BUILD PLAN ===\n";
  List.iteri
    (fun i action ->
      Printf.printf "%3d. " (i + 1);
      print_action action)
    plan;
  Printf.printf "=== END BUILD PLAN ===\n\n"

let execute_action action =
  match action with
  | CreateDirectory dir -> Io.mkdir_p dir
  | WriteFile { path; content } -> Io.write_file path content
  | CopyFile { src; dst } -> Io.copy_file src dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } -> (
      (* Build flags for open modules *)
      let flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* Add sandbox_dir and +unix to includes *)
      let full_includes = sandbox_dir :: "+unix" :: includes in
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let full_output = Filename.concat sandbox_dir output in
      match
        Ocaml_platform.Ocamlc.compile_interface ~includes:full_includes ~flags
          ~output:full_output full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err -> failwith err)
  | CompileImplementation { sandbox_dir; src_file; output; includes; opens }
    -> (
      (* Build flags for open modules *)
      let flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* Add sandbox_dir and +unix to includes *)
      let full_includes = sandbox_dir :: "+unix" :: includes in
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let full_output = Filename.concat sandbox_dir output in
      match
        Ocaml_platform.Ocamlc.compile_impl ~includes:full_includes ~flags
          ~output:full_output full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err -> failwith err)
  | CompileC { sandbox_dir; src_file } -> (
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let output = Filename.chop_extension full_src ^ ".o" in
      match
        Ocaml_platform.Ocamlc.compile_c ~includes:[ sandbox_dir; "+unix" ]
          ~output full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err -> failwith err)
  | CreateArchive { sandbox_dir; archive_name; object_files; includes } -> (
      (* Add sandbox_dir to includes *)
      let full_includes = sandbox_dir :: includes in
      (* Build full paths for object files *)
      let full_objects = List.map (Filename.concat sandbox_dir) object_files in
      let full_output = Filename.concat sandbox_dir archive_name in
      match
        Ocaml_platform.Ocamlc.create_library ~includes:full_includes
          ~output:full_output full_objects
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err -> failwith err)

let execute_build_plan plan =
  Printf.printf "Executing %d actions...\n" (List.length plan);
  List.iter execute_action plan
