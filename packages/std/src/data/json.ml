(** Simple JSON library for RPC communication *)

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

(** Escape a string for JSON *)
let escape_string s =
  let buffer = Buffer.create (String.length s * 2) in
  String.iter
    (function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c -> Buffer.add_char buffer c)
    s;
  Buffer.contents buffer

(** Serialize JSON to string *)
let rec to_string = function
  | Null -> "null"
  | Bool b -> if b then "true" else "false"
  | Int i -> string_of_int i
  | Float f -> string_of_float f
  | String s -> Printf.sprintf "\"%s\"" (escape_string s)
  | Array items -> "[" ^ String.concat "," (List.map to_string items) ^ "]"
  | Object fields ->
      "{"
      ^ String.concat ","
          (List.map
             (fun (k, v) ->
               Printf.sprintf "\"%s\":%s" (escape_string k) (to_string v))
             fields)
      ^ "}"

(** Parse JSON from string *)
let of_string str =
  let len = String.length str in
  let pos = ref 0 in

  let peek () = if !pos < len then Some str.[!pos] else None in

  let advance () = incr pos in

  let skip_whitespace () =
    while
      !pos < len
      && (str.[!pos] = ' '
         || str.[!pos] = '\t'
         || str.[!pos] = '\n'
         || str.[!pos] = '\r')
    do
      incr pos
    done
  in

  let parse_string () =
    advance ();
    (* skip opening quote *)
    let buffer = Buffer.create 16 in
    let rec loop () =
      if !pos >= len then raise (Failure "Unterminated string")
      else
        match str.[!pos] with
        | '"' ->
            advance ();
            Buffer.contents buffer
        | '\\' ->
            advance ();
            if !pos >= len then raise (Failure "Unterminated string");
            (match str.[!pos] with
            | '"' -> Buffer.add_char buffer '"'
            | '\\' -> Buffer.add_char buffer '\\'
            | 'n' -> Buffer.add_char buffer '\n'
            | 'r' -> Buffer.add_char buffer '\r'
            | 't' -> Buffer.add_char buffer '\t'
            | c -> Buffer.add_char buffer c);
            advance ();
            loop ()
        | c ->
            Buffer.add_char buffer c;
            advance ();
            loop ()
    in
    loop ()
  in

  let parse_number () =
    let start = !pos in
    let is_float = ref false in
    while
      !pos < len
      &&
      match str.[!pos] with
      | '0' .. '9' | '-' | '+' -> true
      | '.' | 'e' | 'E' ->
          is_float := true;
          true
      | _ -> false
    do
      advance ()
    done;
    let num_str = String.sub str start (!pos - start) in
    if !is_float then Float (float_of_string num_str)
    else Int (int_of_string num_str)
  in

  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None -> raise (Failure "Unexpected end of input")
    | Some 'n' ->
        if !pos + 4 <= len && String.sub str !pos 4 = "null" then (
          pos := !pos + 4;
          Null)
        else raise (Failure "Invalid null value")
    | Some 't' ->
        if !pos + 4 <= len && String.sub str !pos 4 = "true" then (
          pos := !pos + 4;
          Bool true)
        else raise (Failure "Invalid true value")
    | Some 'f' ->
        if !pos + 5 <= len && String.sub str !pos 5 = "false" then (
          pos := !pos + 5;
          Bool false)
        else raise (Failure "Invalid false value")
    | Some '"' -> String (parse_string ())
    | Some '[' ->
        advance ();
        skip_whitespace ();
        if peek () = Some ']' then (
          advance ();
          Array [])
        else
          let rec parse_items acc =
            let item = parse_value () in
            skip_whitespace ();
            match peek () with
            | Some ',' ->
                advance ();
                parse_items (item :: acc)
            | Some ']' ->
                advance ();
                Array (List.rev (item :: acc))
            | _ -> raise (Failure "Expected ',' or ']' in array")
          in
          parse_items []
    | Some '{' ->
        advance ();
        skip_whitespace ();
        if peek () = Some '}' then (
          advance ();
          Object [])
        else
          let rec parse_fields acc =
            skip_whitespace ();
            (match peek () with
            | Some '"' -> ()
            | _ -> raise (Failure "Expected string key in object"));
            let key = parse_string () in
            skip_whitespace ();
            (match peek () with
            | Some ':' -> advance ()
            | _ -> raise (Failure "Expected ':' after object key"));
            let value = parse_value () in
            skip_whitespace ();
            match peek () with
            | Some ',' ->
                advance ();
                parse_fields ((key, value) :: acc)
            | Some '}' ->
                advance ();
                Object (List.rev ((key, value) :: acc))
            | _ -> raise (Failure "Expected ',' or '}' in object")
          in
          parse_fields []
    | Some ('-' | '0' .. '9') -> parse_number ()
    | Some c -> raise (Failure (Printf.sprintf "Unexpected character: %c" c))
  in

  try
    let result = parse_value () in
    skip_whitespace ();
    if !pos < len then Error "Extra input after JSON value" else Ok result
  with
  | Failure msg -> Error msg
  | _ -> Error "Parse error"

(** Helper functions *)
let null = Null

let bool b = Bool b
let int i = Int i
let float f = Float f
let string s = String s
let array items = Array items
let obj fields = Object fields

let get_field name = function
  | Object fields -> (
      try Some (List.assoc name fields) with Not_found -> None)
  | _ -> None

let get_string = function String s -> Some s | _ -> None
let get_int = function Int i -> Some i | _ -> None
let get_bool = function Bool b -> Some b | _ -> None
let get_array = function Array a -> Some a | _ -> None
let get_object = function Object o -> Some o | _ -> None
