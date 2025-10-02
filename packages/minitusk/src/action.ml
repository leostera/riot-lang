type t =
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
      is_aliases : bool;
    }
  | CompileC of { sandbox_dir : string; src_file : string }
  | CreateArchive of {
      sandbox_dir : string;
      archive_name : string;
      object_files : string list;
      includes : string list;
    }
  | CreateExecutable of {
      sandbox_dir : string;
      exe_name : string;
      main_module : string;
      archive : string;
      dependencies : string list; (* List of dependency package names *)
    }
  | SetPermissions of { path : string; executable : bool }

type build_plan = {
  package_name : Dep_graph.Module_name.t;
  sandbox_dir : string;
  actions : t list;
  outputs : string list;
}

let print action =
  (* Simplified action printing *)
  match action with
  | WriteFile { path; content } ->
      Printf.printf "  Writing %s\n" (Filename.basename path)
  | CopyFile { src; dst } ->
      Printf.printf "  Copying %s\n" (Filename.basename src)
  | CompileInterface { src_file; _ } ->
      Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CompileImplementation { src_file; _ } ->
      Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CompileC { src_file; _ } ->
      Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CreateArchive { archive_name; _ } ->
      Printf.printf "  Creating %s\n" archive_name
  | CreateExecutable { exe_name; _ } ->
      Printf.printf "  Creating executable %s\n" exe_name
  | SetPermissions { path; executable } ->
      Printf.printf "  Setting permissions on %s\n" path

let print_action action =
  match action with
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
  | CreateExecutable { exe_name; main_module; archive; _ } ->
      Printf.printf "BUILD_EXECUTABLE: %s from %s + %s\n" exe_name main_module
        archive
  | SetPermissions { path; executable } ->
      Printf.printf "SET_PERMISSIONS: %s (executable=%b)\n" path executable

let print_build_plan plan =
  (* Quiet mode - only show action count *)
  Printf.printf "Building %d actions...\n" (List.length plan.actions)
(*
  Printf.printf "\n=== BUILD PLAN ===\n";
  Printf.printf "Actions: %d\n" (List.length plan.actions);
  Printf.printf "Outputs: %d\n\n" (List.length plan.outputs);

  List.iteri
    (fun i action ->
      Printf.printf "%3d. " (i + 1);
      print_action action)
    plan.actions;

  if plan.outputs <> [] then (
    Printf.printf "\nExpected outputs:\n";
    List.iter
      (fun output -> Printf.printf "  - %s\n" (Filename.basename output))
      plan.outputs);

  Printf.printf "=== END BUILD PLAN ===\n\n"
  *)

let execute_action ~project_root ~package_name action =
  match action with
  | WriteFile { path; content } ->
      Printf.printf "  DEBUG: Writing generated file %s\n" path;
      (* Create parent directory if needed *)
      let dir = Filename.dirname path in
      if dir <> "." && dir <> "" then Io.mkdir_p dir;
      Io.write_file path content
  | CopyFile { src; dst } ->
      (* src is relative from project root, dst preserves directory structure *)
      let src_absolute = Filename.concat project_root src in
      Printf.printf "  DEBUG: Copying %s -> %s\n" src_absolute dst;
      (* Create parent directory if needed *)
      let dst_dir = Filename.dirname dst in
      if dst_dir <> "." && dst_dir <> "" then Io.mkdir_p dst_dir;
      Io.copy_file src_absolute dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } -> (
      (* Build flags for open modules *)
      let base_flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* For non-kernel packages: add -nopervasives -nostdlib and open Kernel *)
      let flags =
        if package_name = "Kernel" then base_flags
        else
          [ Ocaml_platform.Open "Kernel" ]
          @ base_flags
          @ [ Ocaml_platform.NoPervasives; Ocaml_platform.NoStdlib ]
      in
      (* Now we're in sandbox_dir, so use relative paths *)
      match
        Ocaml_platform.Ocamlc.compile_interface ~includes:("." :: includes)
          ~flags ~output src_file
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileImplementation
      { sandbox_dir; src_file; output; includes; opens; is_aliases } -> (
      (* Now we're in sandbox_dir, so use relative paths *)
      (* Build flags for open modules *)
      let base_flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* For non-kernel packages: add -nopervasives -nostdlib and open Kernel *)
      let kernel_open =
        if package_name = "Kernel" then [] else [ Ocaml_platform.Open "Kernel" ]
      in
      let stdlib_flags =
        if package_name = "Kernel" then []
        else [ Ocaml_platform.NoPervasives; Ocaml_platform.NoStdlib ]
      in
      let flags =
        kernel_open @ base_flags @ stdlib_flags
        @ Ocaml_platform.[ Impl src_file ]
        @ if is_aliases then Ocaml_platform.[ NoAliasDeps ] else []
      in
      match
        Ocaml_platform.Ocamlc.compile_impl ~includes:("." :: includes) ~flags
          ~output src_file
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileC { sandbox_dir; src_file } -> (
      (* Already in sandbox directory *)
      let output = Filename.chop_extension src_file ^ ".o" in
      match
        Ocaml_platform.Ocamlc.compile_c ~includes:[ "." ] ~output src_file
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CreateArchive { sandbox_dir; archive_name; object_files; includes } -> (
      (* Already in sandbox directory *)
      match
        Ocaml_platform.Ocamlc.create_library ~includes:("." :: includes)
          ~output:archive_name object_files
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CreateExecutable
      { sandbox_dir; exe_name; main_module; archive; dependencies } -> (
      (* Link an executable from main module and archive *)
      (* Dependencies should have been copied to our sandbox already *)
      let dep_archives =
        List.map
          (fun dep_name ->
            Dep_graph.Module_name.cma (Dep_graph.Module_name.of_string dep_name))
          dependencies
      in
      (* Only link unix.cma for kernel package or packages that depend on kernel *)
      let needs_unix = exe_name = "kernel" || List.mem "Kernel" dependencies in
      let libs =
        (if needs_unix then [ "unix.cma" ] else []) @ dep_archives @ [ archive ]
      in
      (* Everything is in the current sandbox directory *)
      (* Add +unix to includes if we need the unix library *)
      let includes = if needs_unix then [ "."; "+unix" ] else [ "." ] in
      match
        Ocaml_platform.Ocamlc.run ~includes ~libs ~output:(Some exe_name)
          ~mode:Ocaml_platform.CustomExe ~flags:[] [ main_module ]
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | SetPermissions { path; executable } ->
      (* Set file permissions *)
      if executable then Unix.chmod path 0o755 (* rwxr-xr-x *)
      else Unix.chmod path 0o644 (* rw-r--r-- *)

let execute_build_plan ~build_results plan =
  Printf.printf "Executing %d actions...\n" (List.length plan.actions);
  (* Save current directory *)
  let original_cwd = Io.getcwd () in

  (* Get sandbox dir from first action *)
  let sandbox_dir = plan.sandbox_dir in
  (* Get package name *)
  let package_name = Dep_graph.Module_name.to_string plan.package_name in

  (* 1. Remove old sandbox if it exists *)
  Io.rm_rf sandbox_dir;

  (* 2. Create fresh sandbox dir *)
  Io.mkdir_p sandbox_dir;

  (* 3. Copy dependency outputs to sandbox *)
  Dep_graph.Build_results.copy_to_sandbox build_results sandbox_dir;

  (* 4. Change to sandbox directory *)
  Io.chdir sandbox_dir;
  Printf.printf "Working in: %s\n" (Io.getcwd ());

  (* 5. Execute all actions *)
  List.iter
    (execute_action ~project_root:original_cwd ~package_name)
    plan.actions;

  (* 6. Restore original directory *)
  Io.chdir original_cwd

let promote_outputs (plan : build_plan) =
  (* Extract package name from first output path *)
  let package_name =
    match plan.outputs with
    | first :: _ ->
        let parts = String.split_on_char '/' first in
        List.nth parts 3 (* target/bootstrap/sandbox/PACKAGE/... *)
    | [] -> failwith "No outputs to promote"
  in

  let out_dir = Printf.sprintf "target/bootstrap/out/%s" package_name in
  Io.mkdir_p out_dir;

  Printf.printf "\nPromoting outputs for %s:\n" package_name;

  (* Copy each output from sandbox to out directory *)
  List.iter
    (fun src ->
      let basename = Filename.basename src in
      let dst = Filename.concat out_dir basename in
      if Io.file_exists src then (
        (* Use copy_file_with_permissions to preserve executable bit *)
        Io.copy_file_with_permissions src dst;
        Printf.printf "  - %s\n" basename))
    plan.outputs;

  Printf.printf "Promoted %d artifacts to %s\n" (List.length plan.outputs)
    out_dir

let from_dep_graph (dep_graph : Dep_graph.t) : build_plan =
  let sandbox_dir =
    Printf.sprintf "target/bootstrap/sandbox/%s"
      (Dep_graph.Module_name.to_string dep_graph.package_name)
  in
  let actions = ref [] in
  let cmo_files = ref [] in
  let o_files = ref [] in
  let outputs = ref [] in

  (* Create sandbox directory *)
  let opens mods =
    List.filter_map
      (fun (node : Dep_graph.dep Graph.node) ->
        let dep = node.value in
        match dep.kind with
        | ML mod_ | MLI mod_ -> Some (Dep_graph.Module.namespaced_name mod_)
        | _ -> None)
      mods
  in

  (* First, copy all header files *)
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      match node.value with
      | { kind = H; file = Concrete path; _ } ->
          actions := CopyFile { src = path; dst = path } :: !actions
      | _ -> ())
    dep_graph;

  (* Generate actions in dependency order *)
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      match node.value with
      | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          actions := CopyFile { src = path; dst = path } :: !actions;

          (* Compile interface *)
          let output = Module.cmi mod_ in
          let opens = opens open_modules in
          let action =
            CompileInterface
              { sandbox_dir; src_file = path; output; includes = []; opens }
          in
          actions := action :: !actions;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          actions := CopyFile { src = path; dst = path } :: !actions;

          (* Compile implementation *)
          let output = Module.cmo mod_ in
          let opens = opens open_modules in
          let action =
            CompileImplementation
              {
                sandbox_dir;
                src_file = path;
                output;
                includes = [];
                opens;
                is_aliases = false;
              }
          in
          actions := action :: !actions;
          cmo_files := !cmo_files @ [ output ];
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { file = Generated { path; contents }; kind = ML mod_; open_modules; _ }
        ->
          (* Write generated .ml file *)
          let write = WriteFile { path; content = contents } in
          actions := write :: !actions;

          (* Compile the generated file *)
          let output = Module.cmo mod_ in
          let opens = opens open_modules in
          let is_aliases = Module.is_aliases mod_ in
          let compile =
            CompileImplementation
              {
                sandbox_dir;
                src_file = path;
                output;
                includes = [];
                opens;
                is_aliases;
              }
          in
          actions := compile :: !actions;
          cmo_files := !cmo_files @ [ output ];
          outputs := Filename.concat sandbox_dir output :: !outputs;
          (* For aliases modules, also add the .cmi as an output *)
          if is_aliases then
            let cmi_output = Module.cmi mod_ in
            outputs := Filename.concat sandbox_dir cmi_output :: !outputs
      | {
       file = Generated { path; contents };
       kind = MLI mod_;
       open_modules;
       _;
      } ->
          (* Write generated .mli file *)
          let write = WriteFile { path; content = contents } in
          actions := write :: !actions;

          (* Compile the generated interface file *)
          let output = Module.cmi mod_ in
          let opens = opens open_modules in
          let compile =
            CompileInterface
              { sandbox_dir; src_file = path; output; includes = []; opens }
          in
          actions := compile :: !actions;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { file = Generated { path; contents }; _ } ->
          (* Other generated files (not .ml or .mli) *)
          let action = WriteFile { path; content = contents } in
          actions := action :: !actions
      | { kind = C; file = Concrete path; _ } ->
          (* Copy and compile C files *)
          actions := CopyFile { src = path; dst = path } :: !actions;
          let compile = CompileC { sandbox_dir; src_file = path } in
          actions := compile :: !actions;
          (* The .o file is created at the same path as the .c file, just different extension *)
          let obj_file = Filename.chop_extension path ^ ".o" in
          (* For the archive, we just need the basename *)
          let obj_basename =
            Filename.chop_extension (Filename.basename path) ^ ".o"
          in
          o_files := !o_files @ [ obj_basename ];
          (* But for outputs, we need the full path so it gets promoted correctly *)
          outputs := Filename.concat sandbox_dir obj_file :: !outputs
      | { kind = H; _ } ->
          (* Header files already copied at the beginning *)
          ()
      | _ -> () (* Skip Root, etc *))
    dep_graph;

  (* Add final archive creation if we have any .cmo or .o files *)
  let () =
    if !cmo_files <> [] || !o_files <> [] then (
      let archive_name = Dep_graph.Module_name.cma dep_graph.package_name in
      (* cmo_files should now be in correct linking order after edge fixes *)
      let all_objects = !cmo_files @ !o_files in
      let archive =
        CreateArchive
          {
            sandbox_dir;
            archive_name;
            object_files = all_objects;
            includes = [];
          }
      in
      actions := archive :: !actions;
      outputs := Filename.concat sandbox_dir archive_name :: !outputs)
  in

  (* Check if we have a main.ml file - if so, add binary creation *)
  let () =
    let has_main = ref false in
    let main_cmo = ref "" in
    Dep_graph.iter
      (fun node ->
        match node.value with
        | { kind = Dep_graph.ML mod_; file = Concrete path; _ } ->
            if Filename.basename path = "main.ml" then (
              has_main := true;
              main_cmo := Dep_graph.Module.namespaced_name mod_ ^ ".cmo")
        | _ -> ())
      dep_graph;

    if !has_main then (
      (* Create a simple executable linking main.cmo with the archive *)
      let package_name_str =
        Dep_graph.Module_name.to_string dep_graph.package_name
      in
      (* Use lowercase for the binary name *)
      let binary_name = String.lowercase_ascii package_name_str in
      let archive_name = Dep_graph.Module_name.cma dep_graph.package_name in
      Printf.printf "  Adding binary creation for %s (found main.ml)\n"
        binary_name;

      (* Get the package's dependencies in topological order *)
      let dependencies = Dep_graph.get_dependencies dep_graph in
      Printf.printf "  Dependencies for %s: [%s]\n" binary_name
        (String.concat "; " dependencies);

      (* Create executable linking main.cmo with the package's archive *)
      let link_action =
        CreateExecutable
          {
            sandbox_dir;
            exe_name = binary_name;
            main_module = !main_cmo;
            archive = archive_name;
            dependencies;
          }
      in
      actions := link_action :: !actions;

      (* Add action to make it executable *)
      let chmod_action =
        SetPermissions { path = binary_name; executable = true }
      in
      actions := chmod_action :: !actions;

      outputs := Filename.concat sandbox_dir binary_name :: !outputs)
  in

  {
    package_name = dep_graph.package_name;
    sandbox_dir;
    actions = List.rev !actions;
    outputs = List.rev !outputs;
  }
