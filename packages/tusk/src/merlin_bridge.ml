(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system

    This implements the ocaml-merlin protocol that ocaml-lsp-server uses to get
    build configuration. It uses S-expressions for communication. *)

(** Find the workspace root by looking for the top-level tusk.toml *)
let find_workspace_root () =
  let rec find_root dir =
    let tusk_toml = Filename.concat dir "tusk.toml" in
    let packages_dir = Filename.concat dir "packages" in
    (* We're at workspace root if we have both tusk.toml and packages/ directory *)
    if Sys.file_exists tusk_toml && 
       Sys.file_exists packages_dir && 
       Sys.is_directory packages_dir then
      Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None  (* Reached filesystem root *)
      else find_root parent
  in
  find_root (Sys.getcwd ())

(** Get build configuration from tusk server *)
let get_build_config file_path =
  (* For now, use static configuration based on workspace layout *)
  (* TODO: Connect to tusk server once we figure out how to use Miniriot networking here *)
  let home = System.get_home () in
  let stdlib_path = Printf.sprintf "%s/.tusk/toolchains/5.3.0/lib/ocaml" home in

  (* Find workspace root first *)
  let workspace_root = find_workspace_root () in
  
  (* Make file_path absolute if it's relative *)
  let abs_file_path = 
    if Filename.is_relative file_path then
      match workspace_root with
      | Some root -> 
          (* If we're in a subdirectory of workspace, adjust the path *)
          let cwd = Sys.getcwd () in
          if String.starts_with ~prefix:root cwd then
            if cwd = root then
              (* We're at the root, just use the file path *)
              file_path
            else
              (* We're in a subdirectory *)
              let rel_from_root = String.sub cwd (String.length root + 1) 
                (String.length cwd - String.length root - 1) in
              Filename.concat rel_from_root file_path
          else
            file_path
      | None -> file_path
    else
      file_path
  in

  (* Determine which package this file belongs to *)
  let rec find_package_name path =
    if String.contains path '/' then
      let parts = String.split_on_char '/' path in
      (* Look for packages/ directory pattern *)
      let rec find_in_parts = function
        | "packages" :: pkg_name :: _ -> Some pkg_name
        | _ :: rest -> find_in_parts rest
        | [] -> None
      in
      find_in_parts parts
    else None
  in

  (* Adjust paths based on whether we're at workspace root or not *)
  let make_path path =
    match workspace_root with
    | Some root when Sys.getcwd () <> root ->
        (* We're in a subdirectory, need to adjust paths to be relative from here *)
        let cwd = Sys.getcwd () in
        if String.starts_with ~prefix:root cwd then
          let levels = 
            String.sub cwd (String.length root) (String.length cwd - String.length root)
            |> String.split_on_char '/'
            |> List.filter (fun s -> s <> "")
            |> List.length
          in
          let prefix = String.concat "/" (List.init levels (fun _ -> "..")) in
          Filename.concat prefix path
        else
          path
    | _ -> path
  in

  match find_package_name abs_file_path with
  | Some pkg_name ->
      let source_paths = [ make_path (Printf.sprintf "packages/%s/src" pkg_name) ] in
      let build_paths =
        [
          make_path (Printf.sprintf "target/debug/out/packages/%s" pkg_name);
          make_path "target/debug/out/packages/miniriot";
          make_path "target/debug/out/packages/sexp";
          make_path "target/debug/out/packages/toml";
          make_path "target/debug/out/packages/gluon";
        ]
      in
      let flags = [ "-w"; "-a" ] in
      Some (source_paths, build_paths, flags, stdlib_path)
  | None ->
      (* Default configuration *)
      let source_paths = [ make_path "packages/*/src" ] in
      let build_paths = [ make_path "target/debug/out/packages/*" ] in
      let flags = [ "-w"; "-a" ] in
      Some (source_paths, build_paths, flags, stdlib_path)

(** Convert tusk configuration to merlin directives *)
let config_to_directives file_path =
  match get_build_config file_path with
  | None -> []
  | Some (source_paths, build_paths, flags, stdlib_path) ->
      let directives = ref [] in

      (* Add source paths *)
      List.iter
        (fun path ->
          directives := Sexp.(list [ atom "S"; atom path ]) :: !directives)
        source_paths;

      (* Add build paths *)
      List.iter
        (fun path ->
          directives := Sexp.(list [ atom "B"; atom path ]) :: !directives)
        build_paths;

      (* Add stdlib *)
      directives := Sexp.(list [ atom "B"; atom stdlib_path ]) :: !directives;
      directives :=
        Sexp.(list [ atom "B"; atom (stdlib_path ^ "/unix") ]) :: !directives;

      (* Add flags *)
      List.iter
        (fun flag ->
          directives := Sexp.(list [ atom "FLG"; atom flag ]) :: !directives)
        flags;

      List.rev !directives


(** Main loop for the merlin bridge *)
let rec main_loop () =
  (* Log that we're waiting for input *)
  let home = System.get_home () in
  let log_file = Printf.sprintf "%s/.tusk/logs/ocaml-merlin.log" home in
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
  Printf.fprintf oc "[%s] Waiting for input...\n" 
    (Unix.gettimeofday () |> string_of_float);
  Printf.fprintf oc "  stdin is a tty: %b\n" (Unix.isatty Unix.stdin);
  Printf.fprintf oc "  stdout is a tty: %b\n" (Unix.isatty Unix.stdout);
  flush oc;
  close_out oc;
  
  try
    (* Read a line from stdin and parse as Csexp *)
    let line = System.read_line stdin in
    (* Trim any trailing whitespace that might have snuck in *)
    let line = String.trim line in
    
    (* Log the raw line *)
    let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
    Printf.fprintf oc "[%s] Read line: '%s'\n" 
      (Unix.gettimeofday () |> string_of_float) line;
    close_out oc;
    
    (* Parse the line as a Csexp *)
    match Sexp.Csexp.of_string line with
  | Error msg ->
      (* Log the error *)
      let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
      Printf.fprintf oc "[%s] Error parsing Csexp: %s\n" 
        (Unix.gettimeofday () |> string_of_float) msg;
      close_out oc;
      (* Error reading - return error and continue *)
      let error_resp = Sexp.List [ Sexp.List [ Sexp.Atom "ERROR"; Sexp.Atom msg ] ] in
      Sexp.Csexp.to_channel stdout error_resp;
      flush stdout;
      main_loop ()
  | Ok sexp ->
      (* Log what we parsed *)
      let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
      Printf.fprintf oc "[%s] Parsed as sexp: %s\n" 
        (Unix.gettimeofday () |> string_of_float) (Sexp.to_string sexp);
      close_out oc;
      
      (* Process command directly - ocaml-lsp sends one command at a time *)
      (match sexp with
      | Sexp.List [ Sexp.Atom "File"; Sexp.Atom file_path ] ->
          let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
          Printf.fprintf oc "[%s] File command for: %s\n" 
            (Unix.gettimeofday () |> string_of_float) file_path;
          Printf.fprintf oc "Current directory: %s\n" (Sys.getcwd ());
          Printf.fprintf oc "Workspace root: %s\n" 
            (match find_workspace_root () with Some r -> r | None -> "(not found)");
          
          let directives = config_to_directives file_path in
          Printf.fprintf oc "Generated %d directives\n" (List.length directives);
          List.iteri (fun i d -> 
            Printf.fprintf oc "  Directive %d: %s\n" i (Sexp.to_string d)
          ) directives;
          close_out oc;
          
          let response = Sexp.List directives in
          Sexp.Csexp.to_channel stdout response;
          flush stdout;
          main_loop ()
      | Sexp.List [ Sexp.Atom "Halt" ] ->
          let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
          Printf.fprintf oc "[%s] Halt command received\n" 
            (Unix.gettimeofday () |> string_of_float);
          close_out oc;
          ()
      | _ ->
          let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
          Printf.fprintf oc "[%s] Unknown command: %s\n" 
            (Unix.gettimeofday () |> string_of_float) (Sexp.to_string sexp);
          close_out oc;
          
          (* Return empty response for unknown commands *)
          Sexp.Csexp.to_channel stdout (Sexp.List []);
          flush stdout;
          main_loop ())
  with End_of_file -> 
    (* EOF - log and exit *)
    let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
    Printf.fprintf oc "[%s] EOF received, exiting\n" 
      (Unix.gettimeofday () |> string_of_float);
    close_out oc;
    ()

(** Start the merlin bridge *)
let start () =
  (* Log that we're starting *)
  let home = System.get_home () in
  let log_dir = Printf.sprintf "%s/.tusk/logs" home in
  (* Ensure log directory exists *)
  (try Unix.mkdir log_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let log_file = Printf.sprintf "%s/ocaml-merlin.log" log_dir in
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
  Printf.fprintf oc "[%s] Merlin bridge starting...\n" 
    (Unix.gettimeofday () |> string_of_float);
  Printf.fprintf oc "Environment PATH: %s\n" 
    (try Sys.getenv "PATH" with Not_found -> "(not set)");
  Printf.fprintf oc "Current directory: %s\n" (Sys.getcwd ());
  
  (* Log workspace root detection *)
  let workspace_root = find_workspace_root () in
  Printf.fprintf oc "Workspace root: %s\n" 
    (match workspace_root with Some r -> r | None -> "(not found)");
  close_out oc;
  
  (* Don't try to start server - ocaml-lsp will be calling us repeatedly *)
  (* Just process requests *)
  main_loop ()
