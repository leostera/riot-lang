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
