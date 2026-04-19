open Global
open IO
open Collections

type t = Json.t =
  | Null
  | Bool of bool
  | Int of int
  | Float of float
  | String of string
  | Array of t list
  | Object of (string * t) list
  | Embed of t

type error = Json.error =
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

module View = StringView

let error_to_string = Json.error_to_string

let from_view = fun source ->
  let len = View.length source in
  let pos = cell 0 in
  let peek () =
    if !pos >= len then
      None
    else
      Some (View.get_unchecked source ~at:!pos)
  in
  let advance () =
    pos := !pos + 1
  in
  let text_range ~off ~len =
    match View.sub source ~off ~len with
    | Ok view -> View.to_string view
    | Error error -> raise (Failure ("JsonStream.view range invariant failed: " ^ Kernel.IO.Error.message error))
  in
  let rec skip_whitespace () =
    if !pos >= len then
      ()
    else
      match peek () with
      | Some ' '
      | Some '\t'
      | Some '\n'
      | Some '\r' ->
          advance ();
          skip_whitespace ()
      | _ -> ()
  in
  let has_prefix_at offset prefix =
    let prefix_len = String.length prefix in
    if offset + prefix_len > len then
      false
    else
      let rec loop index =
        if index >= prefix_len then
          true
        else if View.get_unchecked source ~at:(offset + index) != String.get_unchecked prefix ~at:index then
          false
        else
          loop (index + 1)
      in
      loop 0
  in
  let exception Json_parse_error of error in
  let raise_error err = raise (Json_parse_error err) in
  let parse_string () =
    advance ();
    let buffer = Buffer.create ~size:16 in
    let hex_value c =
      match c with
      | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
      | 'a' .. 'f' -> Some (10 + Char.to_int c - Char.to_int 'a')
      | 'A' .. 'F' -> Some (10 + Char.to_int c - Char.to_int 'A')
      | _ -> None
    in
    let parse_unicode_escape () =
      if !pos + 4 >= len then
        raise_error (Unterminated_string { position = !pos })
      else
        let decode_at index =
          match hex_value (View.get_unchecked source ~at:index) with
          | Some value -> value
          | None ->
              raise_error
                (Unexpected_character {
                  position = index;
                  character = View.get_unchecked source ~at:index;
                  expected = "hex digit";
                })
        in
        let code =
          (decode_at (!pos + 1) lsl 12)
          lor (decode_at (!pos + 2) lsl 8)
          lor (decode_at (!pos + 3) lsl 4)
          lor decode_at (!pos + 4)
        in
        let rune =
          match Kernel.Unicode.Rune.from_int code with
          | Ok rune -> rune
          | Error (Kernel.Unicode.Rune.BadRune { int }) ->
              raise_error (Unknown_error ("invalid unicode scalar value " ^ Int.to_string int))
        in
        advance ();
        advance ();
        advance ();
        advance ();
        advance ();
        Buffer.add_string buffer (Kernel.Unicode.Rune.to_string rune)
    in
    let rec loop () =
      if !pos >= len then
        raise_error (Unterminated_string { position = !pos })
      else
        match peek () with
        | Some '"' ->
            advance ();
            Buffer.contents buffer
        | Some '\\' ->
            advance ();
            if !pos >= len then
              raise_error (Unterminated_string { position = !pos });
            (
              match peek () with
              | Some '"' ->
                  Buffer.add_char buffer '"';
                  advance ()
              | Some '\\' ->
                  Buffer.add_char buffer '\\';
                  advance ()
              | Some '/' ->
                  Buffer.add_char buffer '/';
                  advance ()
              | Some 'b' ->
                  Buffer.add_char buffer '\b';
                  advance ()
              | Some 'f' ->
                  Buffer.add_char buffer '\012';
                  advance ()
              | Some 'n' ->
                  Buffer.add_char buffer '\n';
                  advance ()
              | Some 'r' ->
                  Buffer.add_char buffer '\r';
                  advance ()
              | Some 't' ->
                  Buffer.add_char buffer '\t';
                  advance ()
              | Some 'u' ->
                  parse_unicode_escape ()
              | Some c ->
                  Buffer.add_char buffer c;
                  advance ()
              | None ->
                  raise_error (Unterminated_string { position = !pos })
            );
            loop ()
        | Some c ->
            Buffer.add_char buffer c;
            advance ();
            loop ()
        | None ->
            raise_error (Unterminated_string { position = !pos })
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
    let num_str = text_range ~off:start ~len:(!pos - start) in
    if !is_float then
      match Float.parse num_str with
      | Some value -> Float value
      | None -> raise_error (Invalid_number { position = start; text = num_str })
    else
      match Int.parse num_str with
      | Some value -> Int value
      | None -> raise_error (Invalid_number { position = start; text = num_str })
  in
  let rec parse_value () =
    skip_whitespace ();
    match peek () with
    | None ->
        raise_error (Unexpected_end_of_input { expected = "value" })
    | Some 'n' ->
        let start_pos = !pos in
        if has_prefix_at !pos "null" then
          (
            pos := !pos + 4;
            Null
          )
        else
          let found =
            if !pos + 4 <= len then
              text_range ~off:!pos ~len:4
            else if !pos < len then
              text_range ~off:!pos ~len:(len - !pos)
            else
              ""
          in
          raise_error (Invalid_literal { expected = "null"; position = start_pos; found })
    | Some 't' ->
        let start_pos = !pos in
        if has_prefix_at !pos "true" then
          (
            pos := !pos + 4;
            Bool true
          )
        else
          let found =
            if !pos + 4 <= len then
              text_range ~off:!pos ~len:4
            else if !pos < len then
              text_range ~off:!pos ~len:(len - !pos)
            else
              ""
          in
          raise_error (Invalid_literal { expected = "true"; position = start_pos; found })
    | Some 'f' ->
        let start_pos = !pos in
        if has_prefix_at !pos "false" then
          (
            pos := !pos + 5;
            Bool false
          )
        else
          let found =
            if !pos + 5 <= len then
              text_range ~off:!pos ~len:5
            else if !pos < len then
              text_range ~off:!pos ~len:(len - !pos)
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
                Array (List.reverse (item :: acc))
            | Some c ->
                raise_error
                  (Expected_comma_or_bracket {
                    kind = "array";
                    position = !pos;
                    found = Some c;
                  })
            | None ->
                raise_error
                  (Expected_comma_or_bracket {
                    kind = "array";
                    position = !pos;
                    found = None;
                  })
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
                Object (List.reverse ((key, value) :: acc))
            | Some c ->
                raise_error
                  (Expected_comma_or_bracket {
                    kind = "object";
                    position = !pos;
                    found = Some c;
                  })
            | None ->
                raise_error
                  (Expected_comma_or_bracket {
                    kind = "object";
                    position = !pos;
                    found = None;
                  })
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
  | Failure message -> Error (Unknown_error message)
  | exn -> Error (Unknown_error (Kernel.Exception.to_string exn))

let from_slice = fun slice -> from_view (View.from_slice slice)

let from_string = fun value ->
  match View.from_string value with
  | Ok view -> from_view view
  | Error error -> Error (Unknown_error (Kernel.IO.Error.message error))
