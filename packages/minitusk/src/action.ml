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
      is_aliases : bool;
    }
  | CompileC of { sandbox_dir : string; src_file : string }
  | CreateArchive of {
      sandbox_dir : string;
      archive_name : string;
      object_files : string list;
      includes : string list;
    }

type build_plan = { actions : t list; outputs : string list }

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

let execute_action action =
  match action with
  | CreateDirectory dir -> Io.mkdir_p dir
  | WriteFile { path; content } -> Io.write_file path content
  | CopyFile { src; dst } -> Io.copy_file src dst
  | CompileInterface { sandbox_dir; src_file; output; includes; opens } -> (
      (* Build flags for open modules *)
      let flags = List.map (fun m -> Ocaml_platform.Open m) opens in
      (* Add sandbox_dir to includes *)
      let full_includes = sandbox_dir :: includes in
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let full_output = Filename.concat sandbox_dir output in
      match
        Ocaml_platform.Ocamlc.compile_interface ~includes:full_includes ~flags
          ~output:full_output full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileImplementation
      { sandbox_dir; src_file; output; includes; opens; is_aliases } -> (
      (* Add sandbox_dir to includes *)
      let full_includes = sandbox_dir :: includes in
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let full_output = Filename.concat sandbox_dir output in
      (* Build flags for open modules *)
      let flags =
        List.map (fun m -> Ocaml_platform.Open m) opens
        @ Ocaml_platform.[ Impl full_src ]
        @ if is_aliases then Ocaml_platform.[ NoAliasDeps ] else []
      in
      match
        Ocaml_platform.Ocamlc.compile_impl ~includes:full_includes ~flags
          ~output:full_output full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
  | CompileC { sandbox_dir; src_file } -> (
      (* Build full paths *)
      let full_src = Filename.concat sandbox_dir src_file in
      let output = Filename.chop_extension full_src ^ ".o" in
      match
        Ocaml_platform.Ocamlc.compile_c ~includes:[ sandbox_dir ] ~output
          full_src
      with
      | Ok _ -> () (* Ignore stdout output *)
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")
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
      | Error err ->
          Printf.printf "%s\n%!" err;
          failwith "compilation error")

let execute_build_plan plan =
  Printf.printf "Executing %d actions...\n" (List.length plan.actions);
  List.iter execute_action plan.actions

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
    if Sys.file_exists src then begin
      Io.copy_file src dst;
      Printf.printf "  - %s\n" basename
    end
  ) plan.outputs;

  Printf.printf "Promoted %d artifacts to %s\n" (List.length plan.outputs) out_dir

let from_dep_graph (dep_graph : Dep_graph.t) : build_plan =
  let sandbox_dir =
    Printf.sprintf "target/bootstrap/sandbox/%s" dep_graph.package_name
  in
  let actions = ref [] in
  let cmo_files = ref [] in
  let outputs = ref [] in

  (* Create sandbox directory *)
  actions := CreateDirectory sandbox_dir :: !actions;

  let opens mods =
    List.filter_map
      (fun (node : Dep_graph.dep Graph.node) ->
        let dep = node.value in
        match dep.kind with
        | ML mod_ | MLI mod_ -> Some (Dep_graph.Module.namespaced_name mod_)
        | _ -> None)
      mods
  in

  (* Generate actions in dependency order *)
  Dep_graph.iter
    (fun node ->
      let open Dep_graph in
      match node.value with
      | { kind = MLI mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          let basename = Filename.basename path in
          actions :=
            CopyFile { src = path; dst = Filename.concat sandbox_dir basename }
            :: !actions;

          (* Compile interface *)
          let output = Module.cmi mod_ in
          let opens = opens open_modules in
          let action =
            CompileInterface
              { sandbox_dir; src_file = basename; output; includes = []; opens }
          in
          actions := action :: !actions;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { kind = ML mod_; file = Concrete path; open_modules; _ } ->
          (* Copy source file to sandbox *)
          let basename = Filename.basename path in
          actions :=
            CopyFile { src = path; dst = Filename.concat sandbox_dir basename }
            :: !actions;

          (* Compile implementation *)
          let output = Module.cmo mod_ in
          let opens = opens open_modules in
          let action =
            CompileImplementation
              {
                sandbox_dir;
                src_file = basename;
                output;
                includes = [];
                opens;
                is_aliases = false;
              }
          in
          actions := action :: !actions;
          cmo_files := output :: !cmo_files;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { file = Generated { path; contents }; kind = ML mod_; open_modules; _ }
        ->
          (* Write generated .ml file *)
          let write =
            WriteFile
              { path = Filename.concat sandbox_dir path; content = contents }
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
          cmo_files := output :: !cmo_files;
          outputs := Filename.concat sandbox_dir output :: !outputs
      | { file = Generated { path; contents }; _ } ->
          (* Other generated files (not .ml) *)
          let action =
            WriteFile
              { path = Filename.concat sandbox_dir path; content = contents }
          in
          actions := action :: !actions
      | { kind = C; file = Concrete path; _ } ->
          (* Copy and compile C files *)
          let basename = Filename.basename path in
          actions :=
            CopyFile { src = path; dst = Filename.concat sandbox_dir basename }
            :: !actions;
          let compile = CompileC { sandbox_dir; src_file = basename } in
          actions := compile :: !actions;
          let obj_file = Filename.chop_extension basename ^ ".o" in
          outputs := Filename.concat sandbox_dir obj_file :: !outputs
      | _ -> () (* Skip Root, etc *))
    dep_graph;

  (* Add final archive creation if we have any .cmo files *)
  let () =
    if !cmo_files <> [] then (
      let archive_name = dep_graph.package_name ^ ".cma" in
      let archive =
        CreateArchive
          {
            sandbox_dir;
            archive_name;
            object_files = List.rev !cmo_files;
            includes = [];
          }
      in
      actions := archive :: !actions;
      outputs := Filename.concat sandbox_dir archive_name :: !outputs)
  in

  { actions = List.rev !actions; outputs = List.rev !outputs }
