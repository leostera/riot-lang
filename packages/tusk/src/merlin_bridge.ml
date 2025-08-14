(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system

    This implements the ocaml-merlin protocol that ocaml-lsp-server uses to get
    build configuration. It uses S-expressions for communication. *)

(** Get build configuration from tusk server *)
let get_build_config file_path =
  (* For now, use static configuration based on workspace layout *)
  (* TODO: Connect to tusk server once we figure out how to use Miniriot networking here *)
  let home = System.get_home () in
  let stdlib_path = Printf.sprintf "%s/.tusk/toolchains/5.3.0/lib/ocaml" home in

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

  match find_package_name file_path with
  | Some pkg_name ->
      let source_paths = [ Printf.sprintf "packages/%s/src" pkg_name ] in
      let build_paths =
        [
          Printf.sprintf "target/debug/out/packages/%s" pkg_name;
          "target/debug/out/packages/miniriot";
          "target/debug/out/packages/sexp";
          "target/debug/out/packages/toml";
          "target/debug/out/packages/gluon";
        ]
      in
      let flags = [ "-w"; "-a" ] in
      Some (source_paths, build_paths, flags, stdlib_path)
  | None ->
      (* Default configuration *)
      let source_paths = [ "packages/*/src" ] in
      let build_paths = [ "target/debug/out/packages/*" ] in
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

(** Handle a merlin protocol request *)
let handle_request request =
  (* Log the raw request to a file for debugging *)
  let log_file = "/tmp/tusk_merlin_requests.log" in
  let oc = open_out_gen [Open_creat; Open_append; Open_text] 0o644 log_file in
  Printf.fprintf oc "Raw request: %S\n" request;
  
  let result = match Sexp.Csexp.of_string request with
  | Ok sexp ->
      Printf.fprintf oc "Parsed as: %s\n" (Sexp.to_string sexp);
      (match sexp with
      | Sexp.List [ Sexp.Atom "File"; Sexp.Atom file_path ] ->
          Printf.fprintf oc "File command for: %s\n" file_path;
          let directives = config_to_directives file_path in
          let response = Sexp.List directives in
          Sexp.Csexp.to_string response
      | Sexp.List [ Sexp.Atom "Halt" ] ->
          Printf.fprintf oc "Halt command received\n";
          Sexp.Csexp.to_string (Sexp.List [])
      | _ ->
          Printf.fprintf oc "Unknown command: %s\n" (Sexp.to_string sexp);
          Sexp.Csexp.to_string (Sexp.List []))
  | Error msg ->
      Printf.fprintf oc "Error parsing request: %s\n" msg;
      Printf.eprintf "Error parsing request: %s\n" msg;
      Sexp.Csexp.to_string (Sexp.List [])
  in
  Printf.fprintf oc "Response: %S\n\n" result;
  close_out oc;
  result

(** Main loop for the merlin bridge *)
let rec main_loop () =
  match Sexp.Csexp.input_opt stdin with
  | Error msg ->
      (* Error reading - return error and exit *)
      let error_resp = Sexp.List [ Sexp.List [ Sexp.Atom "ERROR"; Sexp.Atom msg ] ] in
      Sexp.Csexp.to_channel stdout error_resp;
      ()
  | Ok None -> 
      (* EOF - exit silently *)
      ()
  | Ok (Some sexp) ->
      (* Check if this is a list of commands or a single command *)
      (match sexp with
      | Sexp.List commands when List.length commands > 0 && 
          List.for_all (function Sexp.List _ -> true | _ -> false) commands ->
          (* It's a list of commands - process each one *)
          let rec process_commands = function
            | [] -> ()
            | cmd :: rest ->
                let request_str = Sexp.Csexp.to_string cmd in
                let response = handle_request request_str in
                print_string response;
                flush stdout;
                (* Check if it was a Halt command *)
                match cmd with
                | Sexp.List [ Sexp.Atom "Halt" ] -> ()
                | _ -> process_commands rest
          in
          process_commands commands
      | _ ->
          (* Single command *)
          let request_str = Sexp.Csexp.to_string sexp in
          let response = handle_request request_str in
          (* Response is already in Csexp format from handle_request *)
          print_string response;
          flush stdout;
          (* Check if it was a Halt command *)
          match sexp with
          | Sexp.List [ Sexp.Atom "Halt" ] -> ()
          | _ -> main_loop ())

(** Start the merlin bridge *)
let start () =
  (* Don't try to start server - ocaml-lsp will be calling us repeatedly *)
  (* Just process requests *)
  main_loop ()
