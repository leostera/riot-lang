(** Simple JSON library for RPC communication *)

open Global

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list

type error =
  | Unterminated_string of { position : int }
  | Invalid_literal of { expected : string; position : int; found : string }
  | Invalid_number of { position : int; text : string }
  | Expected_comma_or_bracket of { kind : string; position : int; found : char option }
  | Expected_string_key of { position : int; found : char option }
  | Expected_colon of { position : int; found : char option }
  | Unexpected_end_of_input of { expected : string }
  | Unexpected_character of { position : int; character : char; expected : string }
  | Extra_input_after_value of { position : int }
  | Unknown_error of string

let error_to_string = function
  | Unterminated_string { position } -> 
      format "Unterminated string at position %d" position
  | Invalid_literal { expected; position; found } -> 
      format "Invalid literal at position %d: expected '%s' but found '%s'" 
        position expected found
  | Invalid_number { position; text } -> 
      format "Invalid number format at position %d: '%s'" position text
  | Expected_comma_or_bracket { kind; position; found } -> 
      let found_str = match found with 
        | Some c -> format "'%c'" c 
        | None -> "end of input" 
      in
      format "Expected ',' or closing bracket in %s at position %d, found %s" 
        kind position found_str
  | Expected_string_key { position; found } -> 
      let found_str = match found with 
        | Some c -> format "'%c'" c 
        | None -> "end of input" 
      in
      format "Expected string key in object at position %d, found %s" 
        position found_str
  | Expected_colon { position; found } -> 
      let found_str = match found with 
        | Some c -> format "'%c'" c 
        | None -> "end of input" 
      in
      format "Expected ':' after object key at position %d, found %s" 
        position found_str
  | Unexpected_end_of_input { expected } -> 
      format "Unexpected end of input while parsing %s" expected
  | Unexpected_character { position; character; expected } -> 
      format "Unexpected character '%c' at position %d (expected %s)" 
        character position expected
  | Extra_input_after_value { position } -> 
      format "Extra input after JSON value at position %d" position
  | Unknown_error msg -> format "Unknown error: %s" msg

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
  | String s -> format "\"%s\"" (escape_string s)
  | Array items -> "[" ^ String.concat "," (List.map to_string items) ^ "]"
  | Object fields ->
      "{"
      ^ String.concat ","
          (List.map
             (fun (k, v) ->
               format "\"%s\":%s" (escape_string k) (to_string v))
             fields)
      ^ "}"

(** Parse JSON from string *)
let of_string str =
  let len = String.length str in
  let pos = ref 0 in
  
  Log.trace "[JSON PARSER] Starting parse, string length: %d" len;
  Log.trace "[JSON PARSER] Input: %s" str;

  let peek () = if !pos < len then Some str.[!pos] else None in

  let advance () = 
    Log.trace "[JSON PARSER] Advancing from pos %d to %d (len=%d)" !pos (!pos + 1) len;
    incr pos in

  let rec skip_whitespace () =
    Log.trace "[JSON PARSER] skip_whitespace at pos %d (len=%d)" !pos len;
    if !pos >= len then ()
    else
      match str.[!pos] with
      | ' ' | '\t' | '\n' | '\r' ->
          Log.trace "[JSON PARSER] skip_whitespace skipping char at pos %d" !pos;
          advance ();
          skip_whitespace ()
      | _ -> ()
  in

  let exception Json_parse_error of error in
  
  let raise_error err = raise (Json_parse_error err) in

  let parse_string () =
    Log.trace "[JSON PARSER] parse_string at pos %d" !pos;
    advance ();
    (* skip opening quote *)
    let buffer = Buffer.create 16 in
    let rec loop () =
      if !pos >= len then 
        raise_error (Unterminated_string { position = !pos })
      else
        match str.[!pos] with
        | '"' ->
            advance ();
            let result = Buffer.contents buffer in
            Log.trace "[JSON PARSER] Parsed string: \"%s\"" result;
            result
        | '\\' ->
            advance ();
            if !pos >= len then 
              raise_error (Unterminated_string { position = !pos });
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
    Log.trace "[JSON PARSER] parse_number at pos %d" !pos;
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
    Log.trace "[JSON PARSER] Parsed number: %s" num_str;
    if !is_float then Float (float_of_string num_str)
    else Int (int_of_string num_str)
  in

  let rec parse_value () =
    skip_whitespace ();
    Log.trace "[JSON PARSER] parse_value at pos %d (len=%d), char: %s" 
      !pos len (match peek () with Some c -> format "%c" c | None -> "EOF");
    match peek () with
    | None -> raise_error (Unexpected_end_of_input { expected = "value" })
    | Some 'n' ->
        let start_pos = !pos in
        Log.trace "[JSON PARSER] Parsing 'null' at pos %d, checking bounds: pos+4=%d <= len=%d = %b" 
          !pos (!pos + 4) len (!pos + 4 <= len);
        if !pos + 4 <= len then (
          let substring = 
            try String.sub str !pos 4
            with Invalid_argument msg ->
              Log.trace "[JSON PARSER] ERROR: String.sub failed: %s (pos=%d, len=4, str_len=%d)" msg !pos len;
              raise (Invalid_argument msg)
          in
          if substring = "null" then (
            pos := !pos + 4;
            Null)
          else
            raise_error (Invalid_literal { expected = "null"; position = start_pos; found = substring })
        ) else 
          let found = 
            if !pos < len then (
              let remaining = len - !pos in
              Log.trace "[JSON PARSER] Getting substring: pos=%d, remaining=%d" !pos remaining;
              String.sub str !pos remaining
            )
            else "" 
          in
          raise_error (Invalid_literal { expected = "null"; position = start_pos; found })
    | Some 't' ->
        let start_pos = !pos in
        if !pos + 4 <= len && String.sub str !pos 4 = "true" then (
          pos := !pos + 4;
          Bool true)
        else 
          let found = 
            if !pos + 4 <= len then String.sub str !pos 4
            else if !pos < len then String.sub str !pos (len - !pos)
            else ""
          in
          raise_error (Invalid_literal { expected = "true"; position = start_pos; found })
    | Some 'f' ->
        let start_pos = !pos in
        if !pos + 5 <= len && String.sub str !pos 5 = "false" then (
          pos := !pos + 5;
          Bool false)
        else 
          let found = 
            if !pos + 5 <= len then String.sub str !pos 5
            else if !pos < len then String.sub str !pos (len - !pos)
            else ""
          in
          raise_error (Invalid_literal { expected = "false"; position = start_pos; found })
    | Some '"' -> String (parse_string ())
    | Some '[' ->
        Log.trace "[JSON PARSER] Parsing array at pos %d" !pos;
        advance ();
        skip_whitespace ();
        if peek () = Some ']' then (
          advance ();
          Log.trace "[JSON PARSER] Parsed empty array";
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
            | Some c -> raise_error (Expected_comma_or_bracket { kind = "array"; position = !pos; found = Some c })
            | None -> raise_error (Expected_comma_or_bracket { kind = "array"; position = !pos; found = None })
          in
          parse_items []
    | Some '{' ->
        Log.trace "[JSON PARSER] Parsing object at pos %d" !pos;
        advance ();
        skip_whitespace ();
        if peek () = Some '}' then (
          advance ();
          Log.trace "[JSON PARSER] Parsed empty object";
          Object [])
        else
          let rec parse_fields acc =
            skip_whitespace ();
            (match peek () with
            | Some '"' -> ()
            | Some c -> raise_error (Expected_string_key { position = !pos; found = Some c })
            | None -> raise_error (Expected_string_key { position = !pos; found = None }));
            let key = parse_string () in
            skip_whitespace ();
            (match peek () with
            | Some ':' -> advance ()
            | Some c -> raise_error (Expected_colon { position = !pos; found = Some c })
            | None -> raise_error (Expected_colon { position = !pos; found = None }));
            let value = parse_value () in
            skip_whitespace ();
            match peek () with
            | Some ',' ->
                advance ();
                parse_fields ((key, value) :: acc)
            | Some '}' ->
                advance ();
                Object (List.rev ((key, value) :: acc))
            | Some c -> raise_error (Expected_comma_or_bracket { kind = "object"; position = !pos; found = Some c })
            | None -> raise_error (Expected_comma_or_bracket { kind = "object"; position = !pos; found = None })
          in
          parse_fields []
    | Some ('-' | '0' .. '9') -> parse_number ()
    | Some c -> raise_error (Unexpected_character { position = !pos; character = c; expected = "value" })
  in

  try
    skip_whitespace ();
    let result = parse_value () in
    Log.trace "[JSON PARSER] Finished parse_value, now at pos %d (len=%d)" !pos len;
    skip_whitespace ();
    Log.trace "[JSON PARSER] After skip_whitespace, at pos %d (len=%d)" !pos len;
    if !pos < len then 
      Error (Extra_input_after_value { position = !pos })
    else Ok result
  with
  | Json_parse_error err -> Error err
  | exn -> 
      Log.error "[JSON PARSER] Exception: %s at pos %d (len=%d)" (Exception.to_string exn) !pos len;
      Error (Unknown_error (Exception.to_string exn))

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
