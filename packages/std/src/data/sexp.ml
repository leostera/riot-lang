open Global
open Collections
open IO
open Sync
open Sync.Cell

(** S-expression parsing and printing library *)

(** S-expression type *)
type t =
  | Atom of string
  | List of t list

exception Parse_error of string

(** Parse error *)

(** Convert S-expression to string *)
let rec to_string = fun __tmp1 ->
  match __tmp1 with
  | Atom s ->
      (* Quote atoms that contain special characters *)
      if
        String.contains s " "
        || String.contains s "("
        || String.contains s ")"
        || String.contains s "\""
        || String.contains s "\n"
        || String.contains s "\t"
      then
        "\"" ^ String.escaped s ^ "\""
      else
        s
  | List elems -> "(" ^ String.concat " " (List.map elems ~fn:to_string) ^ ")"

(** Pretty print S-expression *)
let rec pp_sexp = fun indent ->
  fun __tmp1 ->
    match __tmp1 with
    | Atom s -> indent ^ to_string (Atom s)
    | List [] -> indent ^ "()"
    | List [ single ] -> indent ^ "(" ^ to_string single ^ ")"
    | List elems ->
        let indent_next = indent ^ "  " in
        indent
        ^ "(\n"
        ^ String.concat "\n" (List.map elems ~fn:(pp_sexp indent_next))
        ^ "\n"
        ^ indent
        ^ ")"

let pretty_print = fun sexp -> pp_sexp "" sexp

(** Parser implementation *)
module Parser = struct
  type state = {
    input: string;
    mutable pos: int;
    len: int;
  }

  let create = fun input -> { input; pos = 0; len = String.length input }

  let peek = fun state ->
    if state.pos < state.len then
      Some (String.get_unchecked state.input ~at:state.pos)
    else
      None

  let advance = fun state -> state.pos <- state.pos + 1

  let skip_whitespace = fun state ->
    let rec loop () =
      match peek state with
      | Some (' ' | '\t' | '\n' | '\r') ->
          advance state;
          loop ()
      | _ -> ()
    in
    loop ()

  let parse_string = fun state ->
    (* Assumes we're at the opening quote *)
    advance state;
    (* Skip opening quote *)
    let buffer = Buffer.create ~size:16 in
    let rec loop () =
      match peek state with
      | None -> raise (Parse_error "Unexpected end of input in string")
      | Some '"' ->
          advance state;
          Buffer.contents buffer
      | Some '\\' -> (
          advance state;
          match peek state with
          | None -> raise (Parse_error "Unexpected end of input in escape sequence")
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
              loop ()
        )
      | Some c ->
          Buffer.add_char buffer c;
          advance state;
          loop ()
    in
    loop ()

  let parse_atom = fun state ->
    let buffer = Buffer.create ~size:16 in
    let rec loop () =
      match peek state with
      | Some (' ' | '\t' | '\n' | '\r' | '(' | ')')
      | None ->
          let atom = Buffer.contents buffer in
          if atom = "" then
            raise (Parse_error "Empty atom")
          else
            atom
      | Some c ->
          Buffer.add_char buffer c;
          advance state;
          loop ()
    in
    loop ()

  let rec parse_sexp = fun state ->
    skip_whitespace state;
    match peek state with
    | None -> raise (Parse_error "Unexpected end of input")
    | Some '(' ->
        advance state;
        parse_list state
    | Some '"' -> Atom (parse_string state)
    | Some ')' -> raise (Parse_error "Unexpected closing parenthesis")
    | Some _ -> Atom (parse_atom state)

  and parse_list = fun state ->
    let rec loop acc =
      skip_whitespace state;
      match peek state with
      | None -> raise (Parse_error "Unclosed list")
      | Some ')' ->
          advance state;
          List (List.reverse acc)
      | _ ->
          let elem = parse_sexp state in
          loop (elem :: acc)
    in
    loop []
end

(** Parse a string into an S-expression *)
let from_string = fun str ->
  let state = Parser.create str in
  try
    let result = Parser.parse_sexp state in
    Parser.skip_whitespace state;
    if state.pos < state.len then
      raise (Parse_error "Extra input after S-expression")
    else
      Ok result
  with
  | Parse_error msg -> Error msg
  | _ -> Error "Unknown parse error"

(** Parse a string, raising exception on error *)
let parse_exn = fun str ->
  match from_string str with
  | Ok sexp -> sexp
  | Error msg -> raise (Parse_error msg)

(** Parse multiple S-expressions from a string *)
let parse_many = fun str ->
  let state = Parser.create str in
  let rec loop acc =
    Parser.skip_whitespace state;
    if state.pos >= state.len then
      Ok (List.reverse acc)
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
let atom = fun s -> Atom s

let list = fun l -> List l

let is_atom = fun __tmp1 ->
  match __tmp1 with
  | Atom _ -> true
  | _ -> false

let is_list = fun __tmp1 ->
  match __tmp1 with
  | List _ -> true
  | _ -> false

let to_atom = fun __tmp1 ->
  match __tmp1 with
  | Atom s -> Some s
  | _ -> None

let to_list = fun __tmp1 ->
  match __tmp1 with
  | List l -> Some l
  | _ -> None

let rec find_atom = fun name ->
  fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (Atom s) :: _ when s = name -> Some (Atom s)
    | (List l) :: rest -> (
        match find_atom name l with
        | Some v -> Some v
        | None -> find_atom name rest
      )
    | _ :: rest -> find_atom name rest

let rec assoc = fun key ->
  fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (List ((Atom k) :: v :: _)) :: _ when k = key -> Some v
    | _ :: rest -> assoc key rest

(** Canonical S-expressions (Csexp) module *)
module Csexp = struct
  (** Convert S-expression to canonical format *)
  let rec to_string = fun __tmp1 ->
    match __tmp1 with
    | Atom s ->
        (* Format: <length>:<string> *)
        Int.to_string (String.length s) ^ ":" ^ s
    | List elems ->
        (* Format: (<elem1><elem2>...) *)
        let contents = String.concat "" (List.map elems ~fn:to_string) in
        "(" ^ contents ^ ")"

  (** Parse canonical S-expression from string *)
  let from_string = fun str ->
    let len = String.length str in
    let pos = Cell.create 0 in
    let peek () =
      if !pos < len then
        Some (String.get_unchecked str ~at:!pos)
      else
        None
    in
    let advance () = Cell.incr pos in
    let parse_number () =
      let buffer = Buffer.create ~size:4 in
      let rec loop () =
        match peek () with
        | Some ('0' .. '9' as c) ->
            Buffer.add_char buffer c;
            advance ();
            loop ()
        | _ ->
            let s = Buffer.contents buffer in
            if s = "" then
              raise (Parse_error "Expected number")
            else
              (
                match Int.parse s with
                | Some value -> value
                | None -> raise (Parse_error "Invalid number")
              )
      in
      loop ()
    in
    let parse_atom_content n =
      if !pos + n > len then
        raise (Parse_error "Atom extends beyond input")
      else
        let content = String.sub str ~offset:!pos ~len:n in
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
          | _ -> raise (Parse_error "Expected ':' after atom length")
        )
      | Some c -> raise (Parse_error ("Unexpected character '" ^ String.make ~len:1 ~char:c ^ "'"))
    and parse_list () =
      let rec loop acc =
        match peek () with
        | None -> raise (Parse_error "Unclosed list")
        | Some ')' ->
            advance ();
            List (List.reverse acc)
        | Some ('0' .. '9' | '(') ->
            (* Parse an atom or nested list *)
            let elem = parse_sexp () in
            loop (elem :: acc)
        | Some c ->
            raise
              (Parse_error ("Unexpected character '"
              ^ String.make ~len:1 ~char:c
              ^ "' in list at pos "
              ^ Int.to_string !pos))
      in
      loop []
    in
    try
      let result = parse_sexp () in
      if !pos < len then
        Error "Extra input after S-expression"
      else
        Ok result
    with
    | Parse_error msg -> Error msg
    | _ -> Error "Parse error"
end
