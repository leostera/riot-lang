(** Simple JSON library for RPC communication *)
open Global
open IO
open Collections

type t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list
  | Embed of t

let rec type_name = function
  | Null -> "null"
  | Bool _ -> "bool"
  | Int _ -> "int"
  | Float _ -> "float"
  | String _ -> "string"
  | Array _ -> "array"
  | Object _ -> "object"
  | Embed t -> type_name t

type error =
  | Unterminated_string of { position: int }
  | Invalid_literal of { expected: string; position: int; found: string }
  | Invalid_number of { position: int; text: string }
  | Expected_comma_or_bracket of { kind: string; position: int; found: char option }
  | Expected_string_key of { position: int; found: char option }
  | Expected_colon of { position: int; found: char option }
  | Unexpected_end_of_input of { expected: string }
  | Unexpected_character of { position: int; character: char; expected: string }
  | Extra_input_after_value of { position: int }
  | Unknown_error of string

let error_to_string = function
  | Unterminated_string { position } ->
      "Unterminated string at position " ^ string_of_int position
  | Invalid_literal { expected; position; found } ->
      "Invalid literal at position "
      ^ string_of_int position
      ^ ": expected '"
      ^ expected
      ^ "' but found '"
      ^ found
      ^ "'"
  | Invalid_number { position; text } ->
      "Invalid number format at position " ^ string_of_int position ^ ": '" ^ text ^ "'"
  | Expected_comma_or_bracket { kind; position; found } ->
      let found_str =
        match found with
        | Some c -> String.make 1 c
        | None -> "end of input"
      in
      "Expected ',' or closing bracket in "
      ^ kind
      ^ " at position "
      ^ string_of_int position
      ^ ", found "
      ^ found_str
  | Expected_string_key { position; found } ->
      let found_str =
        match found with
        | Some c -> String.make 1 c
        | None -> "end of input"
      in
      "Expected string key in object at position " ^ string_of_int position ^ ", found " ^ found_str
  | Expected_colon { position; found } ->
      let found_str =
        match found with
        | Some c -> String.make 1 c
        | None -> "end of input"
      in
      "Expected ':' after object key at position " ^ string_of_int position ^ ", found " ^ found_str
  | Unexpected_end_of_input { expected } ->
      "Unexpected end of input while parsing " ^ expected
  | Unexpected_character { position; character; expected } ->
      "Unexpected character '"
      ^ String.make 1 character
      ^ "' at position "
      ^ string_of_int position
      ^ " (expected "
      ^ expected
      ^ ")"
  | Extra_input_after_value { position } ->
      "Extra input after JSON value at position " ^ string_of_int position
  | Unknown_error msg ->
      "Unknown error: " ^ msg

(** Escape a string for JSON *)
let hex_digit = fun value ->
  match value with
  | 0 -> '0'
  | 1 -> '1'
  | 2 -> '2'
  | 3 -> '3'
  | 4 -> '4'
  | 5 -> '5'
  | 6 -> '6'
  | 7 -> '7'
  | 8 -> '8'
  | 9 -> '9'
  | 10 -> 'A'
  | 11 -> 'B'
  | 12 -> 'C'
  | 13 -> 'D'
  | 14 -> 'E'
  | 15 -> 'F'
  | _ -> panic "invalid hex digit"

let add_unicode_escape = fun buffer code ->
  Buffer.add_string buffer "\\u";
  Buffer.add_char buffer (hex_digit ((code lsr 12) land 0xf));
  Buffer.add_char buffer (hex_digit ((code lsr 8) land 0xf));
  Buffer.add_char buffer (hex_digit ((code lsr 4) land 0xf));
  Buffer.add_char buffer (hex_digit (code land 0xf))

let escape_string = fun s ->
  let buffer = Buffer.create (String.length s * 2) in
  String.iter
    (
      function
      | '"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\012' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c when Char.code c < 0x20 -> add_unicode_escape buffer (Char.code c)
      | c -> Buffer.add_char buffer c
    )
    s;
  Buffer.contents buffer

(** Format float for JSON - ensures valid JSON number format *)
let format_float = fun f ->
  (* Handle special float values *)
  if Float.is_nan f then
    "null"
  else if Float.is_infinite f then
    if f > 0.0 then
      "null"
      (* or could use a large number *)
    else
      "null"
  else
    let s = string_of_float f in
    (* OCaml's string_of_float can produce "2000." which is invalid JSON *)
    (* If it ends with "." add a "0" to make it valid JSON *)
    if String.ends_with ~suffix:"." s then
      s ^ "0"
    else
      s

(** Serialize JSON to string *)
let rec to_string = function
  | Null -> "null"
  | Bool b ->
      if b then
        "true"
      else
        "false"
  | Int i -> string_of_int i
  | Float f -> format_float f
  | String s -> "\"" ^ escape_string s ^ "\""
  | Array items -> "[" ^ String.concat "," (List.map to_string items) ^ "]"
  | Object fields -> "{"
  ^ String.concat
    ","
    (List.map (fun ((k, v)) -> "\"" ^ escape_string k ^ "\":" ^ to_string v) fields)
  ^ "}"
  | Embed t -> to_string t

let indentation = fun depth ->
  String.make (depth * 2) ' '

let rec to_string_pretty = fun ?(depth = 0) json ->
  match json with
  | Null
  | Bool _
  | Int _
  | Float _
  | String _ ->
      to_string json
  | Array [] ->
      "[]"
  | Array items ->
      let item_indent = indentation (depth + 1) in
      let closing_indent = indentation depth in
      "[\n"
      ^ (items
      |> List.map (fun item -> item_indent ^ to_string_pretty ~depth:(depth + 1) item)
      |> String.concat ",\n")
      ^ "\n"
      ^ closing_indent
      ^ "]"
  | Object [] ->
      "{}"
  | Object fields ->
      let field_indent = indentation (depth + 1) in
      let closing_indent = indentation depth in
      "{\n"
      ^ (fields
      |> List.map
        (fun (key, value) ->
          field_indent ^ to_string (String key) ^ ": " ^ to_string_pretty ~depth:(depth + 1) value)
      |> String.concat ",\n")
      ^ "\n"
      ^ closing_indent
      ^ "}"
  | Embed t ->
      to_string_pretty t

(** Parse JSON from string *)
let of_string = fun str ->
  let len = String.length str in
  let pos = cell 0 in
  let peek () =
    if !pos < len then
      Some str.[!pos]
    else
      None
  in
  let advance () =
    pos := !pos + 1
  in
  let rec skip_whitespace () =
    if !pos >= len then
      ()
    else
      match str.[!pos] with
      | ' '
      | '\t'
      | '\n'
      | '\r' ->
          advance ();
          skip_whitespace ()
      | _ -> ()
  in
  let exception Json_parse_error of error in
  let raise_error err = raise (Json_parse_error err) in
  let parse_string () =
    advance ();
    (* skip opening quote *)
    let buffer = Buffer.create 16 in
    let hex_value c =
      match c with
      | '0' .. '9' -> Some (Char.code c - Char.code '0')
      | 'a' .. 'f' -> Some (10 + Char.code c - Char.code 'a')
      | 'A' .. 'F' -> Some (10 + Char.code c - Char.code 'A')
      | _ -> None
    in
    let parse_unicode_escape () =
      if !pos + 4 >= len then
        raise_error (Unterminated_string { position = !pos })
      else
        let decode_at index =
          match hex_value str.[index] with
          | Some value -> value
          | None -> raise_error
            (Unexpected_character {
              position = index;
              character = str.[index];
              expected = "hex digit"
            })
        in
        let code = (decode_at (!pos + 1) lsl 12)
        lor (decode_at (!pos + 2) lsl 8)
        lor (decode_at (!pos + 3) lsl 4)
        lor decode_at (!pos + 4) in
        advance ();
        advance ();
        advance ();
        advance ();
        advance ();
        Buffer.add_utf_8_uchar buffer (Uchar.of_int code)
    in
    let rec loop () =
      if !pos >= len then
        raise_error (Unterminated_string { position = !pos })
      else
        match str.[!pos] with
        | '"' ->
            advance ();
            Buffer.contents buffer
        | '\\' ->
            advance ();
            if !pos >= len then
              raise_error (Unterminated_string { position = !pos });
            (
              match str.[!pos] with
              | '"' ->
                  Buffer.add_char buffer '"';
                  advance ()
              | '\\' ->
                  Buffer.add_char buffer '\\';
                  advance ()
              | '/' ->
                  Buffer.add_char buffer '/';
                  advance ()
              | 'b' ->
                  Buffer.add_char buffer '\b';
                  advance ()
              | 'f' ->
                  Buffer.add_char buffer '\012';
                  advance ()
              | 'n' ->
                  Buffer.add_char buffer '\n';
                  advance ()
              | 'r' ->
                  Buffer.add_char buffer '\r';
                  advance ()
              | 't' ->
                  Buffer.add_char buffer '\t';
                  advance ()
              | 'u' ->
                  parse_unicode_escape ()
              | c ->
                  Buffer.add_char buffer c;
                  advance ()
            );
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
    let is_float = cell false in
    let rec consume () =
      match peek () with
      | Some ('0' .. '9' | '-' | '+') ->
          advance ();
          consume ()
      | Some ('.' | 'e' | 'E') ->
          is_float := true;
          advance ();
          consume ()
      | _ ->
          ()
    in
    consume ();
    let num_str = String.sub str start (!pos - start) in
    if !is_float then
      Float (float_of_string num_str)
    else
      Int (int_of_string num_str)
  in
  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None ->
        raise_error (Unexpected_end_of_input { expected = "value" })
    | Some 'n' ->
        let start_pos = !pos in
        if !pos + 4 <= len then
          let substring = String.sub str !pos 4 in
          if substring = "null" then
            (
              pos := !pos + 4;
              Null
            )
          else
            raise_error
              (Invalid_literal { expected = "null"; position = start_pos; found = substring })
        else
          let found =
            if !pos < len then
              String.sub str !pos (len - !pos)
            else
              ""
          in
          raise_error (Invalid_literal { expected = "null"; position = start_pos; found })
    | Some 't' ->
        let start_pos = !pos in
        if !pos + 4 <= len && String.sub str !pos 4 = "true" then
          (
            pos := !pos + 4;
            Bool true
          )
        else
          let found =
            if !pos + 4 <= len then
              String.sub str !pos 4
            else if !pos < len then
              String.sub str !pos (len - !pos)
            else
              ""
          in
          raise_error (Invalid_literal { expected = "true"; position = start_pos; found })
    | Some 'f' ->
        let start_pos = !pos in
        if !pos + 5 <= len && String.sub str !pos 5 = "false" then
          (
            pos := !pos + 5;
            Bool false
          )
        else
          let found =
            if !pos + 5 <= len then
              String.sub str !pos 5
            else if !pos < len then
              String.sub str !pos (len - !pos)
            else
              ""
          in
          raise_error (Invalid_literal { expected = "false"; position = start_pos; found })
    | Some '"' ->
        String (parse_string ())
    | Some '[' ->
        advance ();
        skip_whitespace ();
        if peek () = Some ']' then
          (
            advance ();
            Array []
          )
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
            | Some c ->
                raise_error
                  (Expected_comma_or_bracket { kind = "array"; position = !pos; found = Some c })
            | None ->
                raise_error
                  (Expected_comma_or_bracket { kind = "array"; position = !pos; found = None })
          in
          parse_items []
    | Some '{' ->
        advance ();
        skip_whitespace ();
        if peek () = Some '}' then
          (
            advance ();
            Object []
          )
        else
          let rec parse_fields acc =
            skip_whitespace ();
            (
              match peek () with
              | Some '"' -> ()
              | Some c -> raise_error (Expected_string_key { position = !pos; found = Some c })
              | None -> raise_error (Expected_string_key { position = !pos; found = None })
            );
            let key = parse_string () in
            skip_whitespace ();
            (
              match peek () with
              | Some ':' -> advance ()
              | Some c -> raise_error (Expected_colon { position = !pos; found = Some c })
              | None -> raise_error (Expected_colon { position = !pos; found = None })
            );
            let value = parse_value () in
            skip_whitespace ();
            match peek () with
            | Some ',' ->
                advance ();
                parse_fields ((key, value) :: acc)
            | Some '}' ->
                advance ();
                Object (List.rev ((key, value) :: acc))
            | Some c ->
                raise_error
                  (Expected_comma_or_bracket { kind = "object"; position = !pos; found = Some c })
            | None ->
                raise_error
                  (Expected_comma_or_bracket { kind = "object"; position = !pos; found = None })
          in
          parse_fields []
    | Some ('-' | '0' .. '9') ->
        parse_number ()
    | Some c ->
        raise_error (Unexpected_character { position = !pos; character = c; expected = "value" })
  in
  try
    skip_whitespace ();
    let result = parse_value () in
    skip_whitespace ();
    if !pos < len then
      Error (Extra_input_after_value { position = !pos })
    else
      Ok result
  with
  | Json_parse_error err -> Error err
  | exn -> Error (Unknown_error (Exception.to_string exn))

(** Helper functions *)
let null = Null

let bool = fun b -> Bool b

let int = fun i -> Int i

let float = fun f -> Float f

let string = fun s -> String s

let array = fun items -> Array items

let obj = fun fields -> Object fields

let get_field = fun name ->
  function
  | Object fields -> (
      try Some (List.assoc name fields) with
      | Not_found -> None
    )
  | _ -> None

let get_string = function
  | String s -> Some s
  | _ -> None

let get_int = function
  | Int i -> Some i
  | _ -> None

let get_bool = function
  | Bool b -> Some b
  | _ -> None

let get_array = function
  | Array a -> Some a
  | _ -> None

let get_object = function
  | Object o -> Some o
  | _ -> None

let rec diff = fun a b ->
  let rec diff_at_path path a b =
    match (a, b) with
    | Null, Null -> []
    | Bool x, Bool y when x = y -> []
    | Int x, Int y when x = y -> []
    | Float x, Float y when x = y -> []
    | String x, String y when x = y -> []
    | Array xs, Array ys -> diff_arrays path xs ys
    | Object xs, Object ys -> diff_objects path xs ys
    | _ -> [ { Diff.path; kind = Diff.Changed (a, b) } ]
  and diff_arrays path xs ys =
    let max_len = max (List.length xs) (List.length ys) in
    let rec loop acc idx =
      if idx >= max_len then
        List.rev acc
      else
        let x_opt =
          try Some (List.nth xs idx) with
          | _ -> None
        in
        let y_opt =
          try Some (List.nth ys idx) with
          | _ -> None
        in
        let idx_path = path @ [ Diff.Index idx ] in
        match (x_opt, y_opt) with
        | Some x, Some y ->
            let diffs = diff_at_path idx_path x y in
            loop (List.rev_append diffs acc) (idx + 1)
        | Some x, None ->
            let diff = { Diff.path = idx_path; kind = Diff.Removed x } in
            loop (diff :: acc) (idx + 1)
        | None, Some y ->
            let diff = { Diff.path = idx_path; kind = Diff.Added y } in
            loop (diff :: acc) (idx + 1)
        | None, None ->
            loop acc (idx + 1)
    in
    loop [] 0
  and diff_objects path xs ys =
    let all_keys =
      let xs_keys = List.map fst xs in
      let ys_keys = List.map fst ys in
      List.sort_uniq String.compare (xs_keys @ ys_keys)
    in
    let rec loop acc keys =
      match keys with
      | [] -> List.rev acc
      | key :: rest ->
          let x_opt = List.assoc_opt key xs in
          let y_opt = List.assoc_opt key ys in
          let key_path = path @ [ Diff.Key key ] in
          let new_diffs =
            match (x_opt, y_opt) with
            | Some x, Some y -> diff_at_path key_path x y
            | Some x, None -> [ { Diff.path = key_path; kind = Diff.Removed x } ]
            | None, Some y -> [ { Diff.path = key_path; kind = Diff.Added y } ]
            | None, None -> []
          in
          loop (List.rev_append new_diffs acc) rest
    in
    loop [] all_keys
  in
  diff_at_path [] a b
