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
      then Printf.sprintf "\"%s\"" (String.escaped s)
      else s
  | List elems -> "(" ^ String.concat " " (List.map to_string elems) ^ ")"

(** Pretty print S-expression *)
let rec pp_sexp indent = function
  | Atom s -> Printf.sprintf "%s%s" indent (to_string (Atom s))
  | List [] -> Printf.sprintf "%s()" indent
  | List [ single ] -> Printf.sprintf "%s(%s)" indent (to_string single)
  | List elems ->
      let indent_next = indent ^ "  " in
      Printf.sprintf "%s(\n%s\n%s)" indent
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
  try
    let ic = open_in filename in
    let len = in_channel_length ic in
    let content = really_input_string ic len in
    close_in ic;
    parse_many content
  with Sys_error msg -> Error msg

(** Write S-expression to a file *)
let to_file filename sexp =
  try
    let oc = open_out filename in
    output_string oc (to_string sexp);
    output_char oc '\n';
    close_out oc;
    Ok ()
  with Sys_error msg -> Error msg

(** Canonical S-expressions (Csexp) module *)
module Csexp = struct
  (** Convert S-expression to canonical format *)
  let rec to_string = function
    | Atom s ->
        (* Format: <length>:<string> *)
        Printf.sprintf "%d:%s" (String.length s) s
    | List elems ->
        (* Format: (<elem1><elem2>...) *)
        let contents = String.concat "" (List.map to_string elems) in
        Printf.sprintf "(%s)" contents

  (** Write S-expression to channel in canonical format *)
  let to_channel oc sexp =
    output_string oc (to_string sexp);
    flush oc

  (** Parse canonical S-expression from string *)
  let of_string str =
    let len = String.length str in
    let pos = ref 0 in

    let peek () = if !pos < len then Some str.[!pos] else None in

    let advance () = incr pos in

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
      | Some c ->
          raise (Parse_error (Printf.sprintf "Unexpected character '%c'" c))
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
                 (Printf.sprintf "Unexpected character '%c' in list at pos %d" c
                    !pos))
      in
      loop []
    in

    try
      let result = parse_sexp () in
      if !pos < len then Error "Extra input after S-expression" else Ok result
    with
    | Parse_error msg -> Error msg
    | _ -> Error "Parse error"

  (** Read canonical S-expression from input channel *)
  let input ic =
    let read_char () = try Some (input_char ic) with End_of_file -> None in

    let read_atom_content n =
      let buffer = Buffer.create n in
      for i = 1 to n do
        match read_char () with
        | Some c -> Buffer.add_char buffer c
        | None -> raise (Parse_error "Unexpected EOF in atom")
      done;
      Buffer.contents buffer
    in

    let rec parse_sexp () =
      match read_char () with
      | None -> raise (Parse_error "Unexpected EOF")
      | Some '(' -> parse_list ()
      | Some ('0' .. '9' as c) ->
          (* Put back the digit and parse number *)
          let first_digit = String.make 1 c in
          let rest =
            let buf = Buffer.create 4 in
            let rec loop () =
              match read_char () with
              | Some ('0' .. '9' as c) ->
                  Buffer.add_char buf c;
                  loop ()
              | Some ':' -> Buffer.contents buf
              | _ -> raise (Parse_error "Expected ':' after atom length")
            in
            loop ()
          in
          let n = int_of_string (first_digit ^ rest) in
          Atom (read_atom_content n)
      | Some c ->
          raise (Parse_error (Printf.sprintf "Unexpected character '%c'" c))
    and parse_list () =
      let rec loop acc =
        match read_char () with
        | None -> raise (Parse_error "Unclosed list")
        | Some ')' -> List (List.rev acc)
        | Some c ->
            (* We need to "unread" this character for parse_sexp *)
            (* For now, we'll use a simpler approach with look-ahead *)
            raise
              (Parse_error "Cannot look ahead in stream - use of_string instead")
      in
      loop []
    in

    try Ok (parse_sexp ()) with
    | Parse_error msg -> Error msg
    | End_of_file -> Error "Unexpected end of file"
    | _ -> Error "Parse error"

  (** Read canonical S-expression from input, returning None on EOF *)
  let input_opt ic =
    (* Read all available input into a string *)
    let buffer = Buffer.create 256 in
    let rec read_all () =
      try
        Buffer.add_char buffer (input_char ic);
        read_all ()
      with End_of_file -> ()
    in
    read_all ();

    let content = Buffer.contents buffer in
    if content = "" then Ok None (* Empty input means EOF *)
    else
      match of_string content with
      | Ok sexp -> Ok (Some sexp)
      | Error msg -> Error msg
end
