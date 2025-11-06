open Global
open Sync
(** S-expression parsing and printing library *)

(** S-expression type *)
type t = Atom of string | List of t list

exception Parse_error of string
(** Parse error *)

(** Convert S-expression to string *)
let rec to_string = function
  | Atom s ->
      (* Quote atoms that contain special characters *)
      if
        String.contains s ' ' || String.contains s '(' || String.contains s ')'
        || String.contains s '"' || String.contains s '\n'
        || String.contains s '\t'
      then format "\"%s\"" (String.escaped s)
      else s
  | List elems -> "(" ^ String.concat " " (List.map to_string elems) ^ ")"

(** Pretty print S-expression *)
let rec pp_sexp indent = function
  | Atom s -> format "%s%s" indent (to_string (Atom s))
  | List [] -> format "%s()" indent
  | List [ single ] -> format "%s(%s)" indent (to_string single)
  | List elems ->
      let indent_next = indent ^ "  " in
      format "%s(\n%s\n%s)" indent
        (String.concat "\n" (List.map (pp_sexp indent_next) elems))
        indent

let pretty_print sexp = pp_sexp "" sexp

(** Parser implementation *)
module Parser = struct
  type state = { input : string; mutable pos : int; len : int }

  let create input = { input; pos = 0; len = String.length input }

  let peek state =
    if state.pos < state.len then Some state.input.[state.pos] else None

  let advance state = state.pos <- state.pos + 1

  let skip_whitespace state =
    let rec loop () =
      match peek state with
      | Some (' ' | '\t' | '\n' | '\r') ->
          advance state;
          loop ()
      | _ -> ()
    in
    loop ()

  let parse_string state =
    (* Assumes we're at the opening quote *)
    advance state;
    (* Skip opening quote *)
    let buffer = Buffer.create 16 in
    let rec loop () =
      match peek state with
      | None -> raise (Parse_error "Unexpected end of input in string")
      | Some '"' ->
          advance state;
          Buffer.contents buffer
      | Some '\\' -> (
          advance state;
          match peek state with
          | None ->
              raise (Parse_error "Unexpected end of input in escape sequence")
          | Some 'n' ->
              Buffer.add_char buffer '\n';
              advance state;
              loop ()
          | Some 't' ->
              Buffer.add_char buffer '\t';
              advance state;
              loop ()
          | Some 'r' ->
              Buffer.add_char buffer '\r';
              advance state;
              loop ()
          | Some '\\' ->
              Buffer.add_char buffer '\\';
              advance state;
              loop ()
          | Some '"' ->
              Buffer.add_char buffer '"';
              advance state;
              loop ()
          | Some c ->
              Buffer.add_char buffer c;
              advance state;
              loop ())
      | Some c ->
          Buffer.add_char buffer c;
          advance state;
          loop ()
    in
    loop ()

  let parse_atom state =
    let buffer = Buffer.create 16 in
    let rec loop () =
      match peek state with
      | Some (' ' | '\t' | '\n' | '\r' | '(' | ')') | None ->
          let atom = Buffer.contents buffer in
          if atom = "" then raise (Parse_error "Empty atom") else atom
      | Some c ->
          Buffer.add_char buffer c;
          advance state;
          loop ()
    in
    loop ()

  let rec parse_sexp state =
    skip_whitespace state;
    match peek state with
    | None -> raise (Parse_error "Unexpected end of input")
    | Some '(' ->
        advance state;
        parse_list state
    | Some '"' -> Atom (parse_string state)
    | Some ')' -> raise (Parse_error "Unexpected closing parenthesis")
    | Some _ -> Atom (parse_atom state)

  and parse_list state =
    let rec loop acc =
      skip_whitespace state;
      match peek state with
      | None -> raise (Parse_error "Unclosed list")
      | Some ')' ->
          advance state;
          List (List.rev acc)
      | _ ->
          let elem = parse_sexp state in
          loop (elem :: acc)
    in
    loop []
end

(** Parse a string into an S-expression *)
let of_string str =
  let state = Parser.create str in
  try
    let result = Parser.parse_sexp state in
    Parser.skip_whitespace state;
    if state.pos < state.len then
      raise (Parse_error "Extra input after S-expression")
    else Ok result
  with
  | Parse_error msg -> Error msg
  | _ -> Error "Unknown parse error"

(** Parse a string, raising exception on error *)
let parse_exn str =
  match of_string str with
  | Ok sexp -> sexp
  | Error msg -> raise (Parse_error msg)

(** Parse multiple S-expressions from a string *)
let parse_many str =
  let state = Parser.create str in
  let rec loop acc =
    Parser.skip_whitespace state;
    if state.pos >= state.len then Ok (List.rev acc)
    else
      try
        let sexp = Parser.parse_sexp state in
        loop (sexp :: acc)
      with
      | Parse_error msg -> Error msg
      | _ -> Error "Unknown parse error"
  in
  loop []

(** Helper functions for working with S-expressions *)

let atom s = Atom s
let list l = List l
let is_atom = function Atom _ -> true | _ -> false
let is_list = function List _ -> true | _ -> false
let to_atom = function Atom s -> Some s | _ -> None
let to_list = function List l -> Some l | _ -> None

let rec find_atom name = function
  | [] -> None
  | Atom s :: _ when s = name -> Some (Atom s)
  | List l :: rest -> (
      match find_atom name l with
      | Some v -> Some v
      | None -> find_atom name rest)
  | _ :: rest -> find_atom name rest

let rec assoc key = function
  | [] -> None
  | List (Atom k :: v :: _) :: _ when k = key -> Some v
  | _ :: rest -> assoc key rest

(** Read S-expressions from a file *)
let parse_file filename =
  match Path.of_string filename with
  | Error _ -> Error ("Invalid path: " ^ filename)
  | Ok path -> (
      match Fs.read_to_string path with
      | Ok content -> parse_many content
      | Error err -> Error ("File error: " ^ IO.error_message err))

(** Write S-expression to a file *)
let to_file filename sexp =
  match Path.of_string filename with
  | Error _ -> Error ("Invalid path: " ^ filename)
  | Ok path -> (
      match Fs.write (to_string sexp ^ "\n") path with
      | Ok () -> Ok ()
      | Error err -> Error ("File error: " ^ IO.error_message err))

(** Canonical S-expressions (Csexp) module *)
module Csexp = struct
  (** Convert S-expression to canonical format *)
  let rec to_string = function
    | Atom s ->
        (* Format: <length>:<string> *)
        format "%d:%s" (String.length s) s
    | List elems ->
        (* Format: (<elem1><elem2>...) *)
        let contents = String.concat "" (List.map to_string elems) in
        format "(%s)" contents

  (** Parse canonical S-expression from string *)
  let of_string str =
    let len = String.length str in
    let pos = ref 0 in

    let peek () = if !pos < len then Some str.[!pos] else None in

    let advance () = Cell.incr pos in

    let parse_number () =
      let buffer = Buffer.create 4 in
      let rec loop () =
        match peek () with
        | Some ('0' .. '9' as c) ->
            Buffer.add_char buffer c;
            advance ();
            loop ()
        | _ ->
            let s = Buffer.contents buffer in
            if s = "" then raise (Parse_error "Expected number")
            else int_of_string s
      in
      loop ()
    in

    let parse_atom_content n =
      if !pos + n > len then raise (Parse_error "Atom extends beyond input")
      else
        let content = String.sub str !pos n in
        pos := !pos + n;
        content
    in

    let rec parse_sexp () =
      match peek () with
      | None -> raise (Parse_error "Unexpected end of input")
      | Some '(' ->
          advance ();
          parse_list ()
      | Some '0' .. '9' -> (
          let n = parse_number () in
          match peek () with
          | Some ':' ->
              advance ();
              Atom (parse_atom_content n)
          | _ -> raise (Parse_error "Expected ':' after atom length"))
      | Some c -> raise (Parse_error (format "Unexpected character '%c'" c))
    and parse_list () =
      let rec loop acc =
        match peek () with
        | None -> raise (Parse_error "Unclosed list")
        | Some ')' ->
            advance ();
            List (List.rev acc)
        | Some ('0' .. '9' | '(') ->
            (* Parse an atom or nested list *)
            let elem = parse_sexp () in
            loop (elem :: acc)
        | Some c ->
            raise
              (Parse_error
                 (format "Unexpected character '%c' in list at pos %d" c !pos))
      in
      loop []
    in

    try
      let result = parse_sexp () in
      if !pos < len then Error "Extra input after S-expression" else Ok result
    with
    | Parse_error msg -> Error msg
    | _ -> Error "Parse error"
end
