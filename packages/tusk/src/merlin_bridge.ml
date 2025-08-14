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
  match Sexp.of_string request with
  | Ok (Sexp.List [ Sexp.Atom "CONFIG"; Sexp.Atom file_path ]) ->
      (* Return configuration for the file *)
      let directives = config_to_directives file_path in
      let response = Sexp.List directives in
      Sexp.to_string response
  | Ok (Sexp.List [ Sexp.Atom "HALT" ]) ->
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
      if i >= String.length line then (open_count, close_count)
      else
        match line.[i] with
        | '(' -> count_parens (i + 1) (open_count + 1) close_count
        | ')' -> count_parens (i + 1) open_count (close_count + 1)
        | _ -> count_parens (i + 1) open_count close_count
    in
    let opens, closes = count_parens 0 0 0 in
    let new_depth = depth + opens - closes in

    if new_depth <= 0 then new_acc else read_until_complete new_acc new_depth
  in
  try Some (read_until_complete "" 0) with End_of_file -> None

(** Main loop for the merlin bridge *)
let rec main_loop () =
  match read_sexp_from_channel stdin with
  | None -> ()
  | Some request ->
      let response = handle_request request in
      if response = "HALT" then (
        print_string "()";
        flush stdout;
        ())
      else (
        print_endline response;
        flush stdout;
        main_loop ())

(** Start the merlin bridge *)
let start () =
  (* Don't try to start server - ocaml-lsp will be calling us repeatedly *)
  (* Just process requests *)
  main_loop ()
