(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system 
    
    This implements the ocaml-merlin protocol that ocaml-lsp-server uses
    to get build configuration. It uses S-expressions (csexp) for communication. *)

(** Parse an S-expression string *)
let parse_sexp str =
  try
    (* Simple S-expression parser for the merlin protocol *)
    let rec parse_list acc i =
      if i >= String.length str then
        List.rev acc, i
      else
        match str.[i] with
        | ' ' | '\t' | '\n' | '\r' -> parse_list acc (i + 1)
        | ')' -> List.rev acc, i + 1
        | '(' ->
            let elem, next_i = parse_sexp_from i in
            parse_list (elem :: acc) next_i
        | _ ->
            let elem, next_i = parse_atom i in
            parse_list (elem :: acc) next_i
    and parse_atom start =
      let rec find_end i =
        if i >= String.length str then i
        else
          match str.[i] with
          | ' ' | '\t' | '\n' | '\r' | '(' | ')' -> i
          | _ -> find_end (i + 1)
      in
      let end_pos = find_end start in
      let atom = String.sub str start (end_pos - start) in
      `Atom atom, end_pos
    and parse_sexp_from i =
      if i >= String.length str then
        `Atom "", i
      else
        match str.[i] with
        | ' ' | '\t' | '\n' | '\r' -> parse_sexp_from (i + 1)
        | '(' ->
            let elems, next_i = parse_list [] (i + 1) in
            `List elems, next_i
        | _ -> parse_atom i
    in
    let sexp, _ = parse_sexp_from 0 in
    Some sexp
  with _ -> None

(** Convert S-expression to string *)
let rec sexp_to_string = function
  | `Atom s -> s
  | `List elems ->
      "(" ^ String.concat " " (List.map sexp_to_string elems) ^ ")"

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
      let source_paths = [Printf.sprintf "packages/%s/src" pkg_name] in
      let build_paths = [
        Printf.sprintf "target/debug/out/packages/%s" pkg_name;
        "target/debug/out/packages/miniriot";
        "target/debug/out/packages/toml";
        "target/debug/out/packages/gluon"
      ] in
      let flags = ["-w"; "-a"] in
      Some (source_paths, build_paths, flags, stdlib_path)
  | None ->
      (* Default configuration *)
      let source_paths = ["packages/*/src"] in
      let build_paths = ["target/debug/out/packages/*"] in
      let flags = ["-w"; "-a"] in
      Some (source_paths, build_paths, flags, stdlib_path)

(** Convert tusk configuration to merlin directives *)
let config_to_directives file_path =
  match get_build_config file_path with
  | None -> []
  | Some (source_paths, build_paths, flags, stdlib_path) ->
      let directives = ref [] in
      
      (* Add source paths *)
      List.iter (fun path ->
        directives := `List [`Atom "S"; `Atom path] :: !directives
      ) source_paths;
      
      (* Add build paths *)
      List.iter (fun path ->
        directives := `List [`Atom "B"; `Atom path] :: !directives
      ) build_paths;
      
      (* Add stdlib *)
      directives := `List [`Atom "B"; `Atom stdlib_path] :: !directives;
      directives := `List [`Atom "B"; `Atom (stdlib_path ^ "/unix")] :: !directives;
      
      (* Add flags *)
      List.iter (fun flag ->
        directives := `List [`Atom "FLG"; `Atom flag] :: !directives
      ) flags;
      
      List.rev !directives

(** Handle a merlin protocol request *)
let handle_request request =
  match parse_sexp request with
  | Some (`List [`Atom "CONFIG"; `Atom file_path]) ->
      (* Return configuration for the file *)
      let directives = config_to_directives file_path in
      let response = `List directives in
      sexp_to_string response
  | Some (`List [`Atom "HALT"]) ->
      (* Shutdown request *)
      "HALT"
  | _ ->
      (* Unknown request *)
      "()"

(** Read a complete S-expression from input *)
let read_sexp_from_channel ic =
  let rec read_until_complete acc depth =
    let line = input_line ic in
    let new_acc = acc ^ line ^ "\n" in
    
    (* Count parentheses to determine if S-expression is complete *)
    let rec count_parens i open_count close_count =
      if i >= String.length line then
        open_count, close_count
      else
        match line.[i] with
        | '(' -> count_parens (i + 1) (open_count + 1) close_count
        | ')' -> count_parens (i + 1) open_count (close_count + 1)
        | _ -> count_parens (i + 1) open_count close_count
    in
    let opens, closes = count_parens 0 0 0 in
    let new_depth = depth + opens - closes in
    
    if new_depth <= 0 then
      new_acc
    else
      read_until_complete new_acc new_depth
  in
  try
    Some (read_until_complete "" 0)
  with End_of_file -> None

(** Main loop for the merlin bridge *)
let rec main_loop () =
  match read_sexp_from_channel stdin with
  | None -> ()
  | Some request ->
      let response = handle_request request in
      if response = "HALT" then (
        Printf.printf "()%!";
        ()
      ) else (
        Printf.printf "%s\n%!" response;
        main_loop ()
      )

(** Start the merlin bridge *)
let start () =
  (* Ensure server is running *)
  if not (Server_manager.is_server_running ()) then
    ignore (Server_manager.start_background ());
  
  (* Process requests - runs in current process *)
  main_loop ()