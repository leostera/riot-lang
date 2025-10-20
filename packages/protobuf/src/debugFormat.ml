open Std

type value =
  | String of string
  | Float of float
  | Identifier of string
  | SignedIdentifier of string
  | DecSignedInteger of int
  | DecUnsignedInteger of int
  | Message of field list

and field =
  | ScalarField of { name : string; value : value }
  | MessageField of { name : string; value : value }
  | RepeatedField of { name : string; values : value list }

type t = field list

module Parser = struct
  open Iter.MutCursor

  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_ident_char c = is_letter c || is_digit c

  let skip_whitespace_and_comments cursor =
    let rec loop () =
      skip_while cursor is_whitespace;
      match peek cursor with
      | Some '#' ->
          skip_while cursor (fun c -> c <> '\n');
          (match peek cursor with Some '\n' -> advance cursor | _ -> ());
          loop ()
      | _ -> ()
    in
    loop ()

  let parse_ident cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some c when is_letter c -> Ok (take_while cursor is_ident_char)
    | _ -> Error "Expected identifier"

  let parse_field_name cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some '[' ->
        advance cursor;
        let parts = Cell.create [] in
        let rec loop () =
          match parse_ident cursor with
          | Error e -> Error e
          | Ok part -> (
              Cell.set parts (Cell.get parts @ [ part ]);
              skip_whitespace_and_comments cursor;
              match peek cursor with
              | Some '.' ->
                  advance cursor;
                  loop ()
              | Some '/' ->
                  advance cursor;
                  loop ()
              | Some ']' ->
                  advance cursor;
                  Ok (String.concat "." (Cell.get parts))
              | _ -> Error "Expected ']' or '.' in field name")
        in
        loop ()
    | Some c when is_letter c ->
        let name = take_while cursor is_ident_char in
        Ok name
    | _ -> Error "Expected field name"

  let parse_decimal cursor =
    skip_whitespace_and_comments cursor;
    let negative = ref false in
    (match peek cursor with
    | Some '-' ->
        negative := true;
        advance cursor
    | Some '+' -> advance cursor
    | _ -> ());
    match peek cursor with
    | Some c when is_digit c ->
        let digits = take_while cursor is_digit in
        let value = int_of_string digits in
        Ok (if !negative then -value else value)
    | _ -> Error "Expected decimal integer"

  let parse_float cursor =
    skip_whitespace_and_comments cursor;
    let negative = ref false in
    (match peek cursor with
    | Some '-' ->
        negative := true;
        advance cursor
    | Some '+' -> advance cursor
    | _ -> ());
    let has_dot = ref false in
    let has_exp = ref false in
    let buffer = Buffer.create 16 in
    let rec loop () =
      match peek cursor with
      | Some '.' when not !has_dot ->
          has_dot := true;
          Buffer.add_char buffer '.';
          advance cursor;
          loop ()
      | Some ('e' | 'E') when not !has_exp ->
          has_exp := true;
          Buffer.add_char buffer 'e';
          advance cursor;
          (match peek cursor with
          | Some (('+' | '-') as sign) ->
              Buffer.add_char buffer sign;
              advance cursor
          | _ -> ());
          loop ()
      | Some c when is_digit c ->
          Buffer.add_char buffer c;
          advance cursor;
          loop ()
      | Some ('f' | 'F') ->
          advance cursor;
          ()
      | _ -> ()
    in
    loop ();
    let str = Buffer.contents buffer in
    if String.length str = 0 then Error "Expected float"
    else
      try
        let value = float_of_string str in
        Ok (if !negative then -.value else value)
      with _ -> Error "Invalid float"

  let parse_string cursor =
    skip_whitespace_and_comments cursor;
    let strings = Cell.create [] in
    let rec parse_one () =
      let quote = peek cursor in
      match quote with
      | Some ('"' | '\'') ->
          advance cursor;
          let buffer = Buffer.create 16 in
          let rec loop () =
            match peek cursor with
            | None -> Error "Unterminated string"
            | Some c when Some c = quote -> (
                advance cursor;
                Cell.set strings (Cell.get strings @ [ Buffer.contents buffer ]);
                skip_whitespace_and_comments cursor;
                match peek cursor with
                | Some ('"' | '\'') -> parse_one ()
                | _ -> Ok (String.concat "" (Cell.get strings)))
            | Some '\\' -> (
                advance cursor;
                match peek cursor with
                | None -> Error "Unterminated escape"
                | Some 'n' ->
                    Buffer.add_char buffer '\n';
                    advance cursor;
                    loop ()
                | Some 't' ->
                    Buffer.add_char buffer '\t';
                    advance cursor;
                    loop ()
                | Some 'r' ->
                    Buffer.add_char buffer '\r';
                    advance cursor;
                    loop ()
                | Some '\\' ->
                    Buffer.add_char buffer '\\';
                    advance cursor;
                    loop ()
                | Some c when Some c = quote ->
                    Buffer.add_char buffer c;
                    advance cursor;
                    loop ()
                | Some c ->
                    Buffer.add_char buffer c;
                    advance cursor;
                    loop ())
            | Some c ->
                Buffer.add_char buffer c;
                advance cursor;
                loop ()
          in
          loop ()
      | _ -> Error "Expected string literal"
    in
    parse_one ()

  let rec parse_value cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some ('{' | '<') -> parse_message cursor
    | Some ('"' | '\'') -> (
        match parse_string cursor with
        | Ok s -> Ok (String s)
        | Error e -> Error e)
    | Some '-' -> (
        match parse_decimal cursor with
        | Ok n -> Ok (DecSignedInteger n)
        | Error _ -> (
            match parse_float cursor with
            | Ok f -> Ok (Float f)
            | Error e -> Error e))
    | Some c when is_digit c -> (
        let start = position cursor in
        match parse_decimal cursor with
        | Ok n -> (
            skip_whitespace_and_comments cursor;
            match peek cursor with
            | Some '.' -> (
                advance_by cursor (start - position cursor);
                match parse_float cursor with
                | Ok f -> Ok (Float f)
                | Error e -> Error e)
            | _ -> Ok (DecUnsignedInteger n))
        | Error _ -> (
            advance_by cursor (start - position cursor);
            match parse_float cursor with
            | Ok f -> Ok (Float f)
            | Error e -> Error e))
    | Some c when is_letter c -> (
        match parse_ident cursor with
        | Ok id -> Ok (Identifier id)
        | Error e -> Error e)
    | _ -> Error "Expected value"

  and parse_message cursor =
    skip_whitespace_and_comments cursor;
    let open_char = peek cursor in
    let close_char =
      match open_char with Some '{' -> '}' | Some '<' -> '>' | _ -> '}'
    in
    (match open_char with Some ('{' | '<') -> advance cursor | _ -> ());
    let fields = Cell.create [] in
    let rec loop () =
      skip_whitespace_and_comments cursor;
      match peek cursor with
      | Some c when c = close_char ->
          advance cursor;
          Ok (Message (Cell.get fields))
      | None -> Error "Unterminated message"
      | _ -> (
          match parse_field cursor with
          | Error e -> Error e
          | Ok field ->
              Cell.set fields (Cell.get fields @ [ field ]);
              skip_whitespace_and_comments cursor;
              (match peek cursor with
              | Some (';' | ',') -> advance cursor
              | _ -> ());
              loop ())
    in
    loop ()

  and parse_list cursor =
    skip_whitespace_and_comments cursor;
    (match peek cursor with Some '[' -> advance cursor | _ -> ());
    let values = Cell.create [] in
    let rec loop () =
      skip_whitespace_and_comments cursor;
      match peek cursor with
      | Some ']' ->
          advance cursor;
          Ok (Cell.get values)
      | None -> Error "Unterminated list"
      | _ -> (
          match parse_value cursor with
          | Error e -> Error e
          | Ok value ->
              Cell.set values (Cell.get values @ [ value ]);
              skip_whitespace_and_comments cursor;
              (match peek cursor with Some ',' -> advance cursor | _ -> ());
              loop ())
    in
    loop ()

  and parse_field cursor =
    skip_whitespace_and_comments cursor;
    match parse_field_name cursor with
    | Error e -> Error e
    | Ok name -> (
        skip_whitespace_and_comments cursor;
        let has_colon = ref false in
        (match peek cursor with
        | Some ':' ->
            has_colon := true;
            advance cursor
        | _ -> ());
        skip_whitespace_and_comments cursor;
        match peek cursor with
        | Some '[' -> (
            match parse_list cursor with
            | Error e -> Error e
            | Ok values -> Ok (RepeatedField { name; values }))
        | Some ('{' | '<') -> (
            match parse_value cursor with
            | Error e -> Error e
            | Ok value -> Ok (MessageField { name; value }))
        | _ -> (
            if not !has_colon then Error "Expected ':' for scalar field"
            else
              match parse_value cursor with
              | Error e -> Error e
              | Ok value -> Ok (ScalarField { name; value })))

  let parse cursor =
    skip_whitespace_and_comments cursor;
    let fields = Cell.create [] in
    let rec loop () =
      if is_eof cursor then Ok (Cell.get fields)
      else
        match parse_field cursor with
        | Error e -> Error e
        | Ok field ->
            Cell.set fields (Cell.get fields @ [ field ]);
            skip_whitespace_and_comments cursor;
            (match peek cursor with
            | Some (';' | ',') -> advance cursor
            | _ -> ());
            loop ()
    in
    loop ()
end

let parse input =
  let cursor = Iter.MutCursor.create input in
  Parser.parse cursor

let rec print_value = function
  | String s -> "\"" ^ String.escaped s ^ "\""
  | Float f -> string_of_float f
  | Identifier id -> id
  | SignedIdentifier id -> "-" ^ id
  | DecSignedInteger n -> string_of_int n
  | DecUnsignedInteger n -> string_of_int n
  | Message fields -> "{\n" ^ print_fields fields ^ "}"

and print_field = function
  | ScalarField { name; value } -> name ^ ": " ^ print_value value
  | MessageField { name; value } -> name ^ " " ^ print_value value
  | RepeatedField { name; values } ->
      name ^ ": [" ^ String.concat ", " (List.map print_value values) ^ "]"

and print_fields fields =
  String.concat "\n" (List.map (fun f -> "  " ^ print_field f) fields) ^ "\n"

let print fields = print_fields fields
