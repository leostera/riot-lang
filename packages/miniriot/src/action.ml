open Stdlib

type t =
  | WriteFile of { path: string; content: string }
  | CopyFile of { src: string; dst: string }
  | CompileInterface of {
      sandbox_dir: string;
      src_file: string;
      output: string;
      includes: string list;
      opens: string list;
    }
  | CompileImplementation of {
      sandbox_dir: string;
      src_file: string;
      output: string;
      includes: string list;
      opens: string list;
      is_aliases: bool;
    }
  | CompileC of { sandbox_dir: string; src_file: string }
  | CreateArchive of {
      sandbox_dir: string;
      archive_name: string;
      object_files: string list;
      includes: string list;
    }
  | CreateExecutable of {
      sandbox_dir: string;
      exe_name: string;
      main_module: string;
      archive: string;
      dependencies: string list;
      (* List of dependency package names *)
    }
  | SetPermissions of { path: string; executable: bool }

type build_plan = {
  package_name: Dep_graph.Module_name.t;
  package: Package.t;
  sandbox_dir: string;
  actions: t list;
  outputs: string list;
  cc_flags: string list;
  ld_flags: string list;
  uses_stdlib: bool;
  uses_unix: bool;
  uses_dynlink: bool;
}

let print = fun action ->
  (* Simplified action printing *)
  match action with
  | WriteFile { path; content } -> Printf.printf "  Writing %s\n" (Filename.basename path)
  | CopyFile { src; dst } -> Printf.printf "  Copying %s\n" (Filename.basename src)
  | CompileInterface { src_file; _ } ->
      Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CompileImplementation { src_file; _ } ->
      Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CompileC { src_file; _ } -> Printf.printf "  Compiling %s\n" (Filename.basename src_file)
  | CreateArchive { archive_name; _ } -> Printf.printf "  Creating %s\n" archive_name
  | CreateExecutable { exe_name; _ } -> Printf.printf "  Creating executable %s\n" exe_name
  | SetPermissions { path; executable } -> Printf.printf "  Setting permissions on %s\n" path

let print_action = fun action ->
  match action with
  | WriteFile { path; _ } -> Printf.printf "WRITE_FILE: %s\n" path
  | CopyFile { src; dst } -> Printf.printf "COPY: %s -> %s\n" src dst
  | CompileInterface { src_file; output; _ } ->
      Printf.printf "COMPILE_INTF: %s -> %s\n" src_file output
  | CompileImplementation { src_file; output; _ } ->
      Printf.printf "COMPILE_IMPL: %s -> %s\n" src_file output
  | CompileC { src_file; _ } -> Printf.printf "COMPILE_C: %s\n" src_file
  | CreateArchive { archive_name; object_files; _ } ->
      Printf.printf "BUILD_ARCHIVE: %s (%d objects)\n" archive_name (List.length object_files)
  | CreateExecutable { exe_name; main_module; archive; _ } ->
      Printf.printf "BUILD_EXECUTABLE: %s from %s + %s\n" exe_name main_module archive
  | SetPermissions { path; executable } ->
      Printf.printf "SET_PERMISSIONS: %s (executable=%b)\n" path executable

let print_build_plan = fun plan ->
  (* Quiet mode - only show action count *)
  Printf.printf
    "Building %d actions...\n"
    (List.length plan.actions)

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

let execute_action = fun
  ~project_root ~package ~cc_flags ~ld_flags ~uses_stdlib ~uses_unix ~uses_dynlink action ->
  match action with
  | WriteFile { path; content } ->
      Printf.printf "  DEBUG: Writing generated file %s\n" path;
      (* Create parent directory if needed *)
      let dir = Filename.dirname path in
      if dir != "." && dir != "" then
        Io.mkdir_p dir;
      Io.write_file path content
  | CopyFile { src; dst } ->
      (* src is relative from project root, dst preserves directory structure *)
      let src_absolute = Filename.concat project_root src in
      Printf.printf "  DEBUG: Copying %s -> %s\n" src_absolute dst;
      (* Create parent directory if needed *)
      let dst_dir = Filename.dirname dst in
      if dst_dir != "." && dst_dir != "" then
        Io.mkdir_p dst_dir;
      Io.copy_file src_absolute dst
  | CompileInterface {
      sandbox_dir;
      src_file;
      output;
      includes;
      opens;
    } ->
      (
          (* Build flags for open modules *)
          let base_flags = List.map (fun m -> Ocaml_platform.Open m) opens in
          (* Only add -nopervasives -nostdlib if package doesn't use stdlib (including transitively) *)
          let flags =
            if uses_stdlib then
              base_flags
            else
              base_flags @ [ Ocaml_platform.NoPervasives; Ocaml_platform.NoStdlib ]
          in
          (* Add +unix to includes if package uses unix (including transitively) *)
          let final_includes =
            if uses_unix then
              "+unix" :: includes
            else
              includes
          in
          (* Add +dynlink to includes if package uses dynlink (including transitively) *)
          let final_includes =
            if uses_dynlink then
              "+dynlink" :: final_includes
            else
              final_includes
          in
          (* Debug output *)
          Printf.printf
            "  DEBUG: Package %s, uses_stdlib=%b (transitive), uses_unix=%b (transitive), uses_dynlink=%b (transitive)\n"
            package.Package.name
            uses_stdlib
            uses_unix
            uses_dynlink;
          (* Now we're in sandbox_dir, so use relative paths *)
          match Ocaml_platform.Ocamlc.compile_interface
            ~includes:final_includes
            ~flags
            ~output
            src_file with
          | Ok _ -> ()
          | Error err ->
              Printf.printf "%s\n%!" err;
              failwith "compilation error"
        )
  | CompileImplementation {
      sandbox_dir;
      src_file;
      output;
      includes;
      opens;
      is_aliases;
    } ->
      (
          (* Now we're in sandbox_dir, so use relative paths *)
          (* Build flags for open modules *)
          let base_flags = List.map (fun m -> Ocaml_platform.Open m) opens in
          (* Only add -nopervasives -nostdlib if package doesn't use stdlib (including transitively) *)
          let stdlib_flags =
            if uses_stdlib then
              []
            else
              [ Ocaml_platform.NoPervasives; Ocaml_platform.NoStdlib ]
          in
          let flags =
            ((base_flags @ stdlib_flags) @ Ocaml_platform.[ Impl src_file ]) @ if is_aliases then
              Ocaml_platform.[ NoAliasDeps ]
            else
              []
          in
          (* Add +unix to includes if package uses unix (including transitively) *)
          let final_includes =
            if uses_unix then
              "+unix" :: includes
            else
              includes
          in
          (* Add +dynlink to includes if package uses dynlink (including transitively) *)
          let final_includes =
            if uses_dynlink then
              "+dynlink" :: final_includes
            else
              final_includes
          in
          match Ocaml_platform.Ocamlc.compile_impl ~includes:final_includes ~flags ~output src_file with
          | Ok _ -> ()
          | Error err ->
              Printf.printf "%s\n%!" err;
              failwith "compilation error"
        )
  | CompileC { sandbox_dir; src_file } -> (
      (* Already in sandbox directory *)
      (* Output .o file to current directory with just the basename *)
      let output = Filename.basename (Filename.chop_extension src_file) ^ ".o" in
      (* Wrap each cc_flag with -ccopt as separate arguments *)
      let wrapped_cc_flags =
        cc_flags
        |> List.concat_map (fun flag -> [ "-ccopt"; flag ])
      in
      match Ocaml_platform.Ocamlc.compile_c
        ~cc_flags:wrapped_cc_flags
        ~includes:[ "." ]
        ~output
        src_file with
      | Ok _ -> ()
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error"
    )
  | CreateArchive {
      sandbox_dir;
      archive_name;
      object_files;
      includes;
    } ->
      (
          (* Already in sandbox directory *)
          (* Add +unix to includes if package uses unix (including transitively) *)
          let final_includes =
            if uses_unix then
              "+unix" :: includes
            else
              includes
          in
          (* Add +dynlink to includes if package uses dynlink (including transitively) *)
          let final_includes =
            if uses_dynlink then
              "+dynlink" :: final_includes
            else
              final_includes
          in
          (* Don't use -nostdlib if package uses stdlib (including transitively) *)
          let flags =
            if uses_stdlib then
              []
            else
              [ Ocaml_platform.NoStdlib ]
          in
          let has_c_stubs =
            List.exists (fun file -> Filename.check_suffix file ".o") object_files
          in
          let wrapped_cc_flags =
            cc_flags
            |> List.concat_map (fun flag -> [ "-ccopt"; flag ])
          in
          let wrapped_ld_flags =
            ld_flags
            |> List.concat_map (fun flag -> [ "-cclib"; flag ])
          in
          let extra_args =
            (
              (
                if has_c_stubs then
                  [ "-custom" ]
                else
                  []
              ) @ wrapped_cc_flags
            ) @ wrapped_ld_flags
          in
          match Ocaml_platform.Ocamlc.run
            ~includes:("." :: final_includes)
            ~output:(Some archive_name)
            ~mode:Ocaml_platform.Library
            ~flags
            ~extra_args
            object_files with
          | Ok _ -> ()
          | Error err ->
              Printf.printf "%s\n%!" err;
              failwith "compilation error"
        )
  | CreateExecutable {
      sandbox_dir;
      exe_name;
      main_module;
      archive;
      dependencies;
    } ->
      (
          (* Link an executable from main module and archive *)
          (* Dependencies should have been copied to our sandbox already *)
          let dep_archives =
            List.map
              (fun dep_name ->
                Dep_graph.Module_name.cma
                  (Dep_graph.Module_name.from_string dep_name))
              dependencies
          in
          (* Link unix.cma if package uses unix (including transitively) *)
          let needs_unix = uses_unix in
          (* Link dynlink.cma if package uses dynlink (including transitively) *)
          let needs_dynlink = uses_dynlink in
          (* Wrap each cc_flag with -ccopt (needed for frameworks during linking) *)
          let wrapped_cc_flags =
            cc_flags
            |> List.concat_map (fun flag -> [ "-ccopt"; flag ])
          in
          (* Wrap each ld_flag with -cclib as separate arguments *)
          let wrapped_ld_flags =
            ld_flags
            |> List.concat_map (fun flag -> [ "-cclib"; flag ])
          in
          let libs =
            (
              (
                (
                  if needs_unix then
                    [ "unix.cma" ]
                  else
                    []
                ) @ (
                  if needs_dynlink then
                    [ "dynlink.cma" ]
                  else
                    []
                )
              ) @ dep_archives
            ) @ [ archive ]
          in
          (* Everything is in the current sandbox directory *)
          (* Add +unix to includes if we need the unix library *)
          let base_includes =
            if needs_unix then
              [ "."; "+unix" ]
            else
              [ "." ]
          in
          (* Add +dynlink to includes if we need the dynlink library *)
          let includes =
            if needs_dynlink then
              base_includes @ [ "+dynlink" ]
            else
              base_includes
          in
          (* Need to check if any dependency has C stubs to determine if we need -custom *)
          let has_c_stubs = List.mem "Kernel" dependencies in
          (* Build command with both cc_flags and ld_flags *)
          let cmd_parts =
            (
              (
                (
                  (
                    (
                      (
                        [ Ocaml_platform.Ocamlc.ocamlc_path ] @ (
                          if has_c_stubs then
                            [ "-custom" ]
                          else
                            []
                        )
                      ) @ List.concat_map (fun dir -> [ "-I"; dir ]) includes
                    ) @ [ "-o"; exe_name ]
                  ) @ libs
                ) @ wrapped_cc_flags
              ) @ wrapped_ld_flags
            ) @ [ main_module ]
          in
          match Io.run_command_with_output cmd_parts with
          | Ok _ -> ()
          | Error err ->
              Printf.printf "%s\n%!" err;
              failwith "compilation error"
        )
  | SetPermissions { path; executable } ->
      (* Set file permissions *)
      if executable then
        Unix.chmod path 0o755
        (* rwxr-xr-x *)
      else
        Unix.chmod path 0o644

(* rw-r--r-- *)

let execute_build_plan = fun ~build_results plan ->
  Printf.printf "Executing %d actions...\n" (List.length plan.actions);
  (* Save current directory *)
  let original_cwd = Io.getcwd () in
  (* Get sandbox dir from first action *)
  let sandbox_dir = plan.sandbox_dir in
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
    (execute_action
      ~project_root:original_cwd
      ~package:plan.package
      ~cc_flags:plan.cc_flags
      ~ld_flags:plan.ld_flags
      ~uses_stdlib:plan.uses_stdlib
      ~uses_unix:plan.uses_unix
      ~uses_dynlink:plan.uses_dynlink)
    plan.actions;
  (* 6. Restore original directory *)
  Io.chdir original_cwd

let promote_outputs = fun (plan: build_plan) ->
  (* Extract package name from first output path *)
  let package_name =
    match plan.outputs with
    | first :: _ ->
        let parts = String.split_on_char '/' first in
        List.nth parts 3
    | [] -> failwith "No outputs to promote"
  in
  let out_dir = Printf.sprintf "_build/bootstrap/out/%s" package_name in
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
        Printf.printf "  - %s\n" basename
      ))
    plan.outputs;
  Printf.printf "Promoted %d artifacts to %s\n" (List.length plan.outputs) out_dir

let from_dep_graph: Dep_graph.t -> build_plan = fun dep_graph ->
  let sandbox_dir =
    Printf.sprintf
      "_build/bootstrap/sandbox/%s"
      (Dep_graph.Module_name.to_string dep_graph.package_name)
  in
  let actions = ref [] in
  let cmo_files = ref [] in
  let o_files = ref [] in
  let outputs = ref [] in
  (* Calculate transitive flags from dependencies *)
  let dependencies = Dep_graph.get_dependencies dep_graph in
  let transitive_cc_flags =
    Dep_graph.Build_results.get_transitive_cc_flags dep_graph.build_results dependencies
  in
  let transitive_ld_flags =
    Dep_graph.Build_results.get_transitive_ld_flags dep_graph.build_results dependencies
  in
  (* Merge with this package's own flags *)
  let all_cc_flags = transitive_cc_flags @ Package.cc_flags dep_graph.package in
  let all_ld_flags = transitive_ld_flags @ Package.ld_flags dep_graph.package in
  (* Calculate transitive stdlib library usage *)
  let uses_stdlib =
    Package.uses_stdlib dep_graph.package
    || Dep_graph.Build_results.has_stdlib dep_graph.build_results dependencies
  in
  let uses_unix =
    Package.uses_unix dep_graph.package
    || Dep_graph.Build_results.has_unix dep_graph.build_results dependencies
  in
  let uses_dynlink =
    Package.uses_dynlink dep_graph.package
    || Dep_graph.Build_results.has_dynlink dep_graph.build_results dependencies
  in
  (* Create sandbox directory *)
  let opens mods =
    List.filter_map
      (fun (node: Dep_graph.dep Graph.node) ->
        let dep = node.value in
        match dep.kind with
        | ML mod_
        | MLI mod_ -> Some (Dep_graph.Module.namespaced_name mod_)
        | _ -> None)
      mods
  in
  let has_interface mod_ =
    try
      let node_ids =
        Dep_graph.Module_registry.get_by_name dep_graph.registry (Dep_graph.Module.module_name mod_)
      in
      List.exists
        (fun node_id ->
          let node = Graph.get_node dep_graph.graph node_id in
          match node.value.kind with
          | Dep_graph.MLI intf_mod ->
              Dep_graph.Module.module_name intf_mod = Dep_graph.Module.module_name mod_
          | _ -> false)
        node_ids
    with
    | Not_found -> false
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
          let action = CompileInterface {
            sandbox_dir;
            src_file = path;
            output;
            includes = [];
            opens;
          }
          in
          actions := action :: !actions;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          actions := CopyFile { src = path; dst = path } :: !actions;
          (* Compile implementation *)
          let output = Module.cmo mod_ in
          let opens = opens open_modules in
          let action = CompileImplementation {
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
          outputs := Filename.concat sandbox_dir output :: !outputs;
          if not (has_interface mod_) then
            let cmi_output = Module.cmi mod_ in
            outputs := Filename.concat sandbox_dir cmi_output :: !outputs
      | { file = Generated { path; contents }; kind = ML mod_; open_modules; _ } ->
          (* Write generated .ml file *)
          let write = WriteFile { path; content = contents } in
          actions := write :: !actions;
          (* Compile the generated file *)
          let output = Module.cmo mod_ in
          let opens = opens open_modules in
          let is_aliases = Module.is_aliases mod_ in
          let compile = CompileImplementation {
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
          if is_aliases || not (has_interface mod_) then
            let cmi_output = Module.cmi mod_ in
            outputs := Filename.concat sandbox_dir cmi_output :: !outputs
      | { file = Generated { path; contents }; kind = MLI mod_; open_modules; _ } ->
          (* Write generated .mli file *)
          let write = WriteFile { path; content = contents } in
          actions := write :: !actions;
          (* Compile the generated interface file *)
          let output = Module.cmi mod_ in
          let opens = opens open_modules in
          let compile = CompileInterface {
            sandbox_dir;
            src_file = path;
            output;
            includes = [];
            opens;
          }
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
          (* Note: CompileC will get cc_flags from the package when executed *)
          let compile = CompileC { sandbox_dir; src_file = path } in
          actions := compile :: !actions;
          (* The .o file is created in the current directory with just the basename *)
          let obj_basename = Filename.basename (Filename.chop_extension path) ^ ".o" in
          (* For the archive, we use the basename since .o files will be in current dir *)
          o_files := !o_files @ [ obj_basename ];
          (* And for outputs, the .o is at the root of sandbox with basename *)
          outputs := Filename.concat sandbox_dir obj_basename :: !outputs
      | { kind = H; _ } ->
          (* Header files already copied at the beginning *)
          ()
      | _ -> ())
    dep_graph;
  (* Add final archive creation if we have any .cmo or .o files *)
  let () =
    if !cmo_files != [] || !o_files != [] then (
      let archive_name = Dep_graph.Module_name.cma dep_graph.package_name in
      (* cmo_files should now be in correct linking order after edge fixes *)
      let all_objects = !cmo_files @ !o_files in
      let archive = CreateArchive {
        sandbox_dir;
        archive_name;
        object_files = all_objects;
        includes = [];
      }
      in
      actions := archive :: !actions;
      outputs := Filename.concat sandbox_dir archive_name :: !outputs
    )
  in
  (* Compile and link binaries from Package.binaries *)
  let () =
    let package_name_str = Dep_graph.Module_name.to_string dep_graph.package_name in
    let archive_name = Dep_graph.Module_name.cma dep_graph.package_name in
    let dependencies = Dep_graph.get_dependencies dep_graph in
    List.iter
      (fun (binary: Package.binary) ->
        Printf.printf "  Building binary: %s from %s\n" binary.name binary.path;
        (* Copy the binary source *)
        actions := CopyFile { src = binary.path; dst = binary.path } :: !actions;
        (* Compile it with -open Package to access library modules *)
        let binary_basename = Filename.basename binary.path in
        let binary_cmo = Filename.chop_extension binary_basename ^ ".cmo" in
        let compile_binary = CompileImplementation {
          sandbox_dir;
          src_file = binary.path;
          output = binary_cmo;
          includes = [];
          opens = [ package_name_str ];
          is_aliases = false;
        }
        in
        actions := compile_binary :: !actions;
        (* Link the binary with the package archive *)
        let link_action = CreateExecutable {
          sandbox_dir;
          exe_name = binary.name;
          main_module = binary_cmo;
          archive = archive_name;
          dependencies;
        }
        in
        actions := link_action :: !actions;
        (* Make it executable *)
        let chmod_action = SetPermissions { path = binary.name; executable = true } in
        actions := chmod_action :: !actions;
        outputs := Filename.concat sandbox_dir binary.name :: !outputs)
      (Package.binaries dep_graph.package)
  in
  {
    package_name = dep_graph.package_name;
    package = dep_graph.package;
    sandbox_dir;
    actions = List.rev !actions;
    outputs = List.rev !outputs;
    cc_flags = all_cc_flags;
    ld_flags = all_ld_flags;
    uses_stdlib;
    uses_unix;
    uses_dynlink;
  }
