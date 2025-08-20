(** Merlin bridge - Bridge between ocaml-lsp-server and tusk build system

    This implements the ocaml-merlin protocol that ocaml-lsp-server uses to get
    build configuration. It uses S-expressions for communication. *)

open Std

(** Simple S-expression type for Canonical S-expressions *)
module Sexp = struct
  type t = Atom of string | List of t list

  (** Convert to human-readable string *)
  let rec to_string = function
    | Atom s -> Printf.sprintf "%S" s
    | List l -> Printf.sprintf "(%s)" (String.concat " " (List.map to_string l))

  (** Canonical S-expression format *)
  module Csexp = struct
    (** Convert to canonical s-expression string *)
    let rec to_string = function
      | Atom s -> Printf.sprintf "%d:%s" (String.length s) s
      | List l ->
          let contents = String.concat "" (List.map to_string l) in
          Printf.sprintf "(%s)" contents
  end
end

module SexprStdioServer = struct
  type t = { handler : Sexp.t -> (Sexp.t, string) result }

  let create ~handler = { handler }

  (** Read a canonical S-expression from stdin *)
  let read_csexp () =
    (* Read a number until we hit ':' *)
    let rec read_number acc =
      match input_char stdin with
      | '0' .. '9' as c -> read_number (acc ^ String.make 1 c)
      | ':' -> int_of_string acc
      | '(' ->
          (* This is a list, not an atom *)
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
                            (* Recursively handle deeply nested lists *)
                            let inner = parse_csexp_from_char () in
                            nested (inner :: acc)
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
    and parse_csexp_from_char () =
      (* Helper for parsing when we've already consumed the '(' *)
      let rec parse_list acc =
        match input_char stdin with
        | ')' -> Sexp.List (List.rev acc)
        | c ->
            let elem =
              if c >= '0' && c <= '9' then
                let len = read_number (String.make 1 c) in
                Sexp.Atom (read_bytes len)
              else if c = '(' then parse_csexp_from_char ()
              else failwith (Printf.sprintf "Unexpected char: %c" c)
            in
            parse_list (elem :: acc)
      in
      parse_list []
    in

    try Ok (parse_csexp ()) with
    | End_of_file -> Error "EOF"
    | Failure msg -> Error msg
    | e -> Error (Printexc.to_string e)

  let listen t =
    let rec loop () =
      match read_csexp () with
      | Error "EOF" -> Ok ()
      | Error msg ->
          (* Log error but continue *)
          Printf.eprintf "Error reading S-expression: %s\n%!" msg;
          (* Send empty response and continue *)
          Printf.printf "%s%!" (Sexp.Csexp.to_string (Sexp.List []));
          loop ()
      | Ok sexp ->
          (* Handle the message *)
          (match t.handler sexp with
          | Ok response -> Printf.printf "%s%!" (Sexp.Csexp.to_string response)
          | Error err ->
              Printf.eprintf "Handler error: %s\n%!" err;
              Printf.printf "%s%!" (Sexp.Csexp.to_string (Sexp.List [])));
          loop ()
    in
    loop ()
end

module MerlinProtocol = struct
  type t = { client : Tusk_jsonrpc.Client.t; workspace : Workspace.t }

  type request =
    | File of string (* File path to get config for *)
    | Halt (* Stop the server *)

  type directive =
    | SourcePath of string
    | BuildPath of string
    | Flags of string list
    | Stdlib of string

  type response = Directives of directive list | Empty

  let request_of_sexpr = function
    | Sexp.List [ Sexp.Atom "File"; Sexp.Atom path ] -> Ok (File path)
    | Sexp.List [ Sexp.Atom "Halt" ] -> Ok Halt
    | sexp -> Error (Printf.sprintf "Unknown request: %s" (Sexp.to_string sexp))

  let directive_to_sexpr = function
    | SourcePath path -> Sexp.List [ Sexp.Atom "S"; Sexp.Atom path ]
    | BuildPath path -> Sexp.List [ Sexp.Atom "B"; Sexp.Atom path ]
    | Flags flags ->
        List.map
          (fun flag -> Sexp.List [ Sexp.Atom "FLG"; Sexp.Atom flag ])
          flags
        |> List.hd (* For simplicity, just take first flag for now *)
    | Stdlib path -> Sexp.List [ Sexp.Atom "B"; Sexp.Atom path ]

  let response_to_sexpr = function
    | Directives directives ->
        Sexp.List (List.map directive_to_sexpr directives)
    | Empty -> Sexp.List []

  let handle_request t request =
    match request with
    | File path ->
        (* FIXME: this should be using the Tusk_jsonrpc_client to get metadata about the build graph instead of hacky heuristics about the current directory structure *)
        let directives = [] in
        Ok (Directives directives)
    | Halt -> Ok Empty

  let handle_message t sexp =
    match request_of_sexpr sexp with
    | Error err -> Error err
    | Ok request -> (
        match handle_request t request with
        | Error err -> Error err
        | Ok response -> Ok (response_to_sexpr response))

  let create ~workspace ~client = { client; workspace }
end

(** Start the merlin bridge *)
let start ~workspace =
  match Server_manager.ensure_running ~workspace with
  | Error err ->
      Printf.eprintf "Failed to ensure server running: %s\n"
        (match err with Error.ScanWorkspaceError -> "ScanWorkspaceError");
      ()
  | Ok client -> (
      let protocol = MerlinProtocol.create ~workspace ~client in
      let handler sexp = MerlinProtocol.handle_message protocol sexp in
      let server = SexprStdioServer.create ~handler in
      match SexprStdioServer.listen server with Ok () -> () | Error _ -> ())

(** Main entry point for the merlin bridge executable *)
let main () =
  (* Get current directory and scan for workspace *)
  let cwd = Std.Env.current_dir () |> Result.unwrap in
  match Workspace_manager.scan cwd with
  | Error err ->
      Printf.eprintf "Failed to find workspace: %s\n"
        (match err with Error.ScanWorkspaceError -> "ScanWorkspaceError");
      exit 1
  | Ok workspace ->
      start ~workspace;
      exit 0
