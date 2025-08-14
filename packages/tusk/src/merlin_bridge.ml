(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system

    This implements the ocaml-merlin protocol that ocaml-lsp-server uses to get
    build configuration. It uses S-expressions for communication. *)

(** Find the workspace root by looking for the top-level tusk.toml *)
let find_workspace_root () =
  let rec find_root dir =
    let tusk_toml = Filename.concat dir "tusk.toml" in
    let packages_dir = Filename.concat dir "packages" in
    (* We're at workspace root if we have both tusk.toml and packages/ directory *)
    if
      Sys.file_exists tusk_toml
      && Sys.file_exists packages_dir
      && Sys.is_directory packages_dir
    then Some dir
    else
      let parent = Filename.dirname dir in
      if parent = dir then None (* Reached filesystem root *)
      else find_root parent
  in
  find_root (Sys.getcwd ())

(** Get build configuration from tusk server *)
let get_build_config file_path =
  let home = System.get_home () in
  let stdlib_path = Printf.sprintf "%s/.tusk/toolchains/5.3.0/lib/ocaml" home in

  (* Find workspace root first *)
  let workspace_root = find_workspace_root () in

  (* Connect to the tusk server and get workspace info *)
  let get_workspace_packages () =
    let home = System.get_home () in
    let log_file = Printf.sprintf "%s/.tusk/logs/ocaml-merlin.log" home in
    (* Change to workspace root to connect to the server *)
    let original_cwd = Sys.getcwd () in
    let result =
      match workspace_root with
      | Some root ->
          Sys.chdir root;
          let res = Rpc_json_client.get_workspace_config () in
          Sys.chdir original_cwd;
          res
      | None -> Rpc_json_client.get_workspace_config ()
    in
    match result with
    | Ok config ->
        let oc =
          open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
        in
        Printf.fprintf oc
          "[%s] Successfully got workspace config with %d packages: %s\n"
          (Unix.gettimeofday () |> string_of_float)
          (List.length config.Rpc_json.packages)
          (String.concat ", " config.Rpc_json.packages);
        close_out oc;
        Some config.Rpc_json.packages
    | Error e ->
        let oc =
          open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
        in
        Printf.fprintf oc "[%s] Failed to get workspace config: %s\n"
          (Unix.gettimeofday () |> string_of_float)
          e;
        close_out oc;
        None
  in

  (* Adjust paths based on whether we're at workspace root or not *)
  let make_path path =
    match workspace_root with
    | Some root when Sys.getcwd () <> root ->
        (* We're in a subdirectory, need to adjust paths to be relative from here *)
        let cwd = Sys.getcwd () in
        if String.starts_with ~prefix:root cwd then
          let levels =
            String.sub cwd (String.length root)
              (String.length cwd - String.length root)
            |> String.split_on_char '/'
            |> List.filter (fun s -> s <> "")
            |> List.length
          in
          let prefix = String.concat "/" (List.init levels (fun _ -> "..")) in
          Filename.concat prefix path
        else path
    | _ -> path
  in

  (* Get packages from server - no fallback *)
  match get_workspace_packages () with
  | Some packages ->
      (* Use actual packages from the build graph - use absolute paths *)
      let source_paths =
        match workspace_root with
        | Some root ->
            List.map
              (fun pkg ->
                Filename.concat root (Printf.sprintf "packages/%s/src" pkg))
              packages
        | None ->
            List.map
              (fun pkg -> make_path (Printf.sprintf "packages/%s/src" pkg))
              packages
      in
      let build_paths =
        match workspace_root with
        | Some root ->
            List.map
              (fun pkg ->
                Filename.concat root
                  (Printf.sprintf "target/debug/out/packages/%s" pkg))
              packages
        | None ->
            List.map
              (fun pkg ->
                make_path (Printf.sprintf "target/debug/out/packages/%s" pkg))
              packages
      in
      let flags = [ "-w"; "-a" ] in
      Some (source_paths, build_paths, flags, stdlib_path)
  | None ->
      (* No fallback - if we can't get the build graph, we have nothing to say *)
      None

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
  let oc = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file in
  Printf.fprintf oc "[%s] Waiting for input...\n"
    (Unix.gettimeofday () |> string_of_float);
  Printf.fprintf oc "  stdin is a tty: %b\n" (Unix.isatty Unix.stdin);
  Printf.fprintf oc "  stdout is a tty: %b\n" (Unix.isatty Unix.stdout);
  flush oc;
  close_out oc;

  (* Read a Csexp from stdin by first reading the length, then the content *)
  let read_csexp_from_stdin () =
    (* Read a number until we hit ':' *)
    let rec read_number acc =
      match input_char stdin with
      | '0' .. '9' as c -> read_number (acc ^ String.make 1 c)
      | ':' -> int_of_string acc
      | '(' ->
          (* This is a list, not an atom - we need to parse it differently *)
          raise (Failure "list")
      | c ->
          raise
            (Failure
               (Printf.sprintf
                  "Unexpected character '%c' expecting number or '('" c))
    in

    (* Read exactly n bytes *)
    let read_bytes n =
      let bytes = Bytes.create n in
      really_input stdin bytes 0 n;
      Bytes.to_string bytes
    in

    (* Parse a complete Csexp *)
    let rec parse_csexp () =
      match input_char stdin with
      | '(' ->
          (* Parse a list *)
          let rec parse_list acc =
            match input_char stdin with
            | ')' -> Sexp.List (List.rev acc)
            | c ->
                (* Put the char back by parsing it as start of next element *)
                let elem =
                  if c >= '0' && c <= '9' then
                    (* Parse atom *)
                    let len = read_number (String.make 1 c) in
                    Sexp.Atom (read_bytes len)
                  else if c = '(' then
                    (* Nested list - recursively parse *)
                    let rec nested acc =
                      match input_char stdin with
                      | ')' -> Sexp.List (List.rev acc)
                      | c2 ->
                          if c2 >= '0' && c2 <= '9' then
                            let len = read_number (String.make 1 c2) in
                            nested (Sexp.Atom (read_bytes len) :: acc)
                          else if c2 = '(' then
                            failwith "Nested lists not fully supported"
                          else
                            failwith
                              (Printf.sprintf
                                 "Unexpected char in nested list: %c" c2)
                    in
                    nested []
                  else
                    raise
                      (Failure
                         (Printf.sprintf "Unexpected character in list: '%c'" c))
                in
                parse_list (elem :: acc)
          in
          parse_list []
      | '0' .. '9' as c ->
          (* Parse an atom *)
          let len = read_number (String.make 1 c) in
          Sexp.Atom (read_bytes len)
      | c ->
          raise
            (Failure (Printf.sprintf "Unexpected character '%c' at start" c))
    in

    try Ok (parse_csexp ()) with
    | End_of_file -> Error "EOF"
    | Failure msg -> Error msg
    | e -> Error (Printexc.to_string e)
  in

  let sexp_result = read_csexp_from_stdin () in

  match sexp_result with
  | Error "EOF" -> ()
  | Error msg ->
      let oc =
        open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
      in
      Printf.fprintf oc "[%s] Error reading Csexp: %s\n"
        (Unix.gettimeofday () |> string_of_float)
        msg;
      close_out oc;
      (* Return empty response for errors *)
      Printf.printf "%s%!" (Sexp.Csexp.to_string (Sexp.List []));
      main_loop ()
  | Ok sexp -> (
      (* Log what we parsed *)
      let oc =
        open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
      in
      Printf.fprintf oc "[%s] Parsed as sexp: %s\n"
        (Unix.gettimeofday () |> string_of_float)
        (Sexp.to_string sexp);
      close_out oc;

      (* Process command directly - ocaml-lsp sends one command at a time *)
      match sexp with
      | Sexp.List [ Sexp.Atom "File"; Sexp.Atom file_path ] ->
          let oc =
            open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
          in
          Printf.fprintf oc "[%s] File command for: %s\n"
            (Unix.gettimeofday () |> string_of_float)
            file_path;
          Printf.fprintf oc "Current directory: %s\n" (Sys.getcwd ());
          Printf.fprintf oc "Workspace root: %s\n"
            (match find_workspace_root () with
            | Some r -> r
            | None -> "(not found)");

          let directives = config_to_directives file_path in
          Printf.fprintf oc "Generated %d directives\n" (List.length directives);
          List.iteri
            (fun i d ->
              Printf.fprintf oc "  Directive %d: %s\n" i (Sexp.to_string d))
            directives;
          close_out oc;

          let response = Sexp.List directives in
          Printf.printf "%s%!" (Sexp.Csexp.to_string response);
          main_loop ()
      | Sexp.List [ Sexp.Atom "Halt" ] ->
          let oc =
            open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
          in
          Printf.fprintf oc "[%s] Halt command received\n"
            (Unix.gettimeofday () |> string_of_float);
          close_out oc;
          ()
      | _ ->
          let oc =
            open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file
          in
          Printf.fprintf oc "[%s] Unknown command: %s\n"
            (Unix.gettimeofday () |> string_of_float)
            (Sexp.to_string sexp);
          close_out oc;

          (* Return empty response for unknown commands *)
          Printf.printf "%s%!" (Sexp.Csexp.to_string (Sexp.List []));
          main_loop ())

(** Start the merlin bridge *)
let start () =
  (* Log that we're starting *)
  let home = System.get_home () in
  let log_dir = Printf.sprintf "%s/.tusk/logs" home in
  (* Ensure log directory exists *)
  (try Unix.mkdir log_dir 0o755 with Unix.Unix_error (Unix.EEXIST, _, _) -> ());
  let log_file = Printf.sprintf "%s/ocaml-merlin.log" log_dir in
  let oc = open_out_gen [ Open_creat; Open_append; Open_text ] 0o644 log_file in
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
