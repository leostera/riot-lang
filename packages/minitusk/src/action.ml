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

type build_plan = { 
  package_name: Dep_graph.Module_name.t;
  sandbox_dir: string ; actions : t list; outputs : string list }

let print action =
  match action with
  | WriteFile { path; content } ->
      Printf.printf "WRITE_FILE: %s (%d bytes)\n" path (String.length content)
  | CopyFile { src; dst } -> Printf.printf "COPY_FILE: %s -> %s\n" src dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } ->
      Printf.printf "COMPILE_MLI: %s -> %s (includes: %s) (opens: %s)\n"
        src_file output
        (String.concat " " includes)
        (String.concat " " opens)
  | CompileImplementation { sandbox_dir; src_file; output; includes; opens; _ }
    ->
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

let execute_action ~project_root action =
  match action with
  | WriteFile { path; content } -> Io.write_file path content
  | CopyFile { src; dst } ->
      (* src is relative from project root, dst preserves directory structure *)
      let src_absolute = Filename.concat project_root src in
      (* Create parent directory if needed *)
      let dst_dir = Filename.dirname dst in
      if dst_dir <> "." && dst_dir <> "" then Io.mkdir_p dst_dir;
      Io.copy_file src_absolute dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } -> (
      (* Build flags for open modules *)
      let flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* Now we're in sandbox_dir, so use relative paths *)
      match
        Ocaml_platform.Ocamlc.compile_interface ~includes:("." :: includes) ~flags
          ~output:output src_file
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileImplementation
      { sandbox_dir; src_file; output; includes; opens; is_aliases } -> (
      (* Now we're in sandbox_dir, so use relative paths *)
      (* Build flags for open modules *)
      let flags =
        List.map (fun m -> Ocaml_platform.Open m) opens
        @ Ocaml_platform.[ Impl src_file ]
        @ if is_aliases then Ocaml_platform.[ NoAliasDeps ] else []
      in
      match
        Ocaml_platform.Ocamlc.compile_impl ~includes:("." :: includes) ~flags
          ~output:output src_file
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileC { sandbox_dir; src_file } -> (
      (* Already in sandbox directory *)
      let output = Filename.chop_extension src_file ^ ".o" in
      match
        Ocaml_platform.Ocamlc.compile_c ~includes:["."] ~output src_file
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

let execute_build_plan ~build_results plan =
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
  List.iter (execute_action ~project_root:original_cwd) plan.actions;

  (* 6. Restore original directory *)
  Io.chdir original_cwd

let promote_outputs (plan : build_plan) =
  (* Extract package name from first output path *)
  let package_name =
    match plan.outputs with
    | first :: _ ->
        let parts = String.split_on_char '/' first in
        List.nth parts 3  (* target/bootstrap/sandbox/PACKAGE/... *)
    | [] -> failwith "No outputs to promote"
  in

  let out_dir = Printf.sprintf "target/bootstrap/out/%s" package_name in
  Io.mkdir_p out_dir;

  Printf.printf "\nPromoting outputs for %s:\n" package_name;

  (* Copy each output from sandbox to out directory *)
  List.iter (fun src ->
    let basename = Filename.basename src in
    let dst = Filename.concat out_dir basename in
    if Io.file_exists src then begin
      Io.copy_file src dst;
      Printf.printf "  - %s\n" basename
    end
  ) plan.outputs;

  Printf.printf "Promoted %d artifacts to %s\n" (List.length plan.outputs) out_dir

let from_dep_graph (dep_graph : Dep_graph.t) : build_plan =
  let sandbox_dir =
    Printf.sprintf "target/bootstrap/sandbox/%s" (Dep_graph.Module_name.to_string dep_graph.package_name)
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
          actions :=
            CopyFile { src = path; dst = path }
            :: !actions
      | _ -> ())
    dep_graph;

  (* Generate actions in dependency order *)
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      match node.value with
      | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          actions :=
            CopyFile { src = path; dst = path }
            :: !actions;

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
          actions :=
            CopyFile { src = path; dst = path }
            :: !actions;

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
          cmo_files := !cmo_files @ [output];
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { file = Generated { path; contents }; kind = ML mod_; open_modules; _ }
        ->
          (* Write generated .ml file *)
          let write =
            WriteFile
              { path; content = contents }
          in
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
          cmo_files := !cmo_files @ [output];
          outputs := Filename.concat sandbox_dir output :: !outputs;
          (* For aliases modules, also add the .cmi as an output *)
          if is_aliases then
            let cmi_output = Module.cmi mod_ in
            outputs := Filename.concat sandbox_dir cmi_output :: !outputs
      | { file = Generated { path; contents }; kind = MLI mod_; open_modules; _ } ->
          (* Write generated .mli file *)
          let write =
            WriteFile
              { path; content = contents }
          in
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
          let action =
            WriteFile
              { path; content = contents }
          in
          actions := action :: !actions
      | { kind = C; file = Concrete path; _ } ->
          (* Copy and compile C files *)
          actions :=
            CopyFile { src = path; dst = path }
            :: !actions;
          let compile = CompileC { sandbox_dir; src_file = path } in
          actions := compile :: !actions;
          let obj_file = Filename.chop_extension (Filename.basename path) ^ ".o" in
          o_files := !o_files @ [obj_file];
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
      (* cmo_files are already in topological order (dependencies last)
         For linking, we need dependencies first, so keep them as-is *)
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

  { 
    package_name = dep_graph.package_name;
    sandbox_dir; actions = List.rev !actions; outputs = List.rev !outputs }
