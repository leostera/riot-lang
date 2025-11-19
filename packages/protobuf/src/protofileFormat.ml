open Std
open Std.IO

type literal =
  | LitInt of int
  | LitFloat of float
  | LitString of string
  | LitBool of bool
  | LitIdentifier of string

type field_type =
  | Double
  | Float
  | Int32
  | Int64
  | Uint32
  | Uint64
  | Sint32
  | Sint64
  | Fixed32
  | Fixed64
  | Sfixed32
  | Sfixed64
  | Bool
  | String
  | Bytes
  | MessageType of string
  | EnumType of string

type field_option = { name : string; value : literal }

type field = {
  label : [ `None | `Repeated ] option;
  field_type : field_type;
  name : string;
  number : int;
  options : field_option list;
}

type oneof_field = {
  field_type : field_type;
  name : string;
  number : int;
  options : field_option list;
}

type map_field = {
  key_type : field_type;
  value_type : field_type;
  name : string;
  number : int;
  options : field_option list;
}

type enum_value = { name : string; number : int; options : field_option list }

type enum_def = {
  visibility : [ `Export | `Local ] option;
  name : string;
  values : enum_value list;
  options : field_option list;
}

type oneof = { name : string; fields : oneof_field list }

type message_element =
  | Field of field
  | Oneof of oneof
  | MapField of map_field
  | NestedEnum of enum_def
  | NestedMessage of message_def
  | Option of field_option
  | Reserved of [ `Numbers of (int * int option) list | `Names of string list ]
  | Extensions of (int * int option) list

and message_def = {
  visibility : [ `Export | `Local ] option;
  name : string;
  elements : message_element list;
}

type rpc = {
  name : string;
  input_type : string;
  input_stream : bool;
  output_type : string;
  output_stream : bool;
  options : field_option list;
}

type service_def = {
  name : string;
  rpcs : rpc list;
  options : field_option list;
}

type top_level =
  | Message of message_def
  | Enum of enum_def
  | Service of service_def
  | Option of field_option

type t = {
  edition : string option;
  package : string option;
  imports : ([ `Public | `Weak ] option * string) list;
  definitions : top_level list;
}

module Parser = struct
  type proto_file = t

  open Iter.MutCursor

  let is_whitespace = function ' ' | '\t' | '\n' | '\r' -> true | _ -> false
  let is_letter = function 'a' .. 'z' | 'A' .. 'Z' | '_' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_ident_char c = is_letter c || is_digit c

  let skip_whitespace_and_comments cursor =
    let rec loop () =
      skip_while cursor is_whitespace;
      match peek cursor with
      | Some '/' -> (
          advance cursor;
          match peek cursor with
          | Some '/' ->
              skip_while cursor (fun c -> c != '\n');
              advance cursor;
              loop ()
          | Some '*' ->
              advance cursor;
              let rec skip_block_comment () =
                match peek cursor with
                | None -> ()
                | Some '*' -> (
                    advance cursor;
                    match peek cursor with
                    | Some '/' ->
                        advance cursor;
                        loop ()
                    | _ -> skip_block_comment ())
                | Some _ ->
                    advance cursor;
                    skip_block_comment ()
              in
              skip_block_comment ()
          | _ -> ())
      | _ -> ()
    in
    loop ()

  let parse_ident cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some c when is_letter c ->
        let ident = take_while cursor is_ident_char in
        Ok ident
    | _ -> Error "Expected identifier"

  let parse_full_ident cursor =
    skip_whitespace_and_comments cursor;
    let rec loop acc =
      match parse_ident cursor with
      | Error e -> Error e
      | Ok part ->
          skip_whitespace_and_comments cursor;
          if peek cursor = Some '.' then (
            advance cursor;
            loop (part :: acc))
          else Ok (String.concat "." (List.rev (part :: acc)))
    in
    loop []

  let parse_int cursor =
    skip_whitespace_and_comments cursor;
    let negative =
      match peek cursor with
      | Some '-' ->
          advance cursor;
          true
      | Some '+' ->
          advance cursor;
          false
      | _ -> false
    in
    match peek cursor with
    | Some '0' -> (
        advance cursor;
        match peek cursor with
        | Some ('x' | 'X') ->
            advance cursor;
            let hex_digits =
              take_while cursor (function
                | '0' .. '9' | 'a' .. 'f' | 'A' .. 'F' -> true
                | _ -> false)
            in
            let value = int_of_string ("0x" ^ hex_digits) in
            Ok (if negative then -value else value)
        | Some '0' .. '7' ->
            let oct_digits =
              "0"
              ^ take_while cursor (function '0' .. '7' -> true | _ -> false)
            in
            let value = int_of_string oct_digits in
            Ok (if negative then -value else value)
        | _ -> Ok 0)
    | Some '1' .. '9' ->
        let dec_digits = take_while cursor is_digit in
        let value = int_of_string dec_digits in
        Ok (if negative then -value else value)
    | _ -> Error "Expected integer"

  let parse_string cursor =
    skip_whitespace_and_comments cursor;
    let quote = peek cursor in
    match quote with
    | Some ('"' | '\'') ->
        advance cursor;
        let buffer = Buffer.create 16 in
        let rec loop () =
          match peek cursor with
          | None -> Error "Unterminated string"
          | Some c when Some c = quote ->
              advance cursor;
              Ok (Buffer.contents buffer)
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

  let parse_field_type cursor =
    match parse_ident cursor with
    | Error e -> Error e
    | Ok type_name -> (
        match type_name with
        | "double" -> Ok Double
        | "float" -> Ok Float
        | "int32" -> Ok Int32
        | "int64" -> Ok Int64
        | "uint32" -> Ok Uint32
        | "uint64" -> Ok Uint64
        | "sint32" -> Ok Sint32
        | "sint64" -> Ok Sint64
        | "fixed32" -> Ok Fixed32
        | "fixed64" -> Ok Fixed64
        | "sfixed32" -> Ok Sfixed32
        | "sfixed64" -> Ok Sfixed64
        | "bool" -> Ok Bool
        | "string" -> Ok String
        | "bytes" -> Ok Bytes
        | _ ->
            skip_whitespace_and_comments cursor;
            if peek cursor = Some '.' then
              match parse_full_ident cursor with
              | Error e -> Error e
              | Ok full_name -> Ok (MessageType full_name)
            else Ok (MessageType type_name))

  let expect_char cursor expected =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some c when c = expected ->
        advance cursor;
        Ok ()
    | Some c ->
        Error
          ("Expected '" ^ String.make 1 expected ^ "', got '" ^ String.make 1 c
         ^ "'")
    | None -> Error ("Expected '" ^ String.make 1 expected ^ "', got EOF")

  let expect_keyword cursor keyword =
    skip_whitespace_and_comments cursor;
    match parse_ident cursor with
    | Ok s when s = keyword -> Ok ()
    | Ok s -> Error ("Expected '" ^ keyword ^ "', got '" ^ s ^ "'")
    | Error e -> Error e

  let parse_literal cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some '"' | Some '\'' -> (
        match parse_string cursor with
        | Ok s -> Ok (LitString s)
        | Error e -> Error e)
    | Some '-' | Some '+' | Some '0' .. '9' -> (
        match parse_int cursor with
        | Ok i -> Ok (LitInt i)
        | Error _ -> Error "Expected integer or float")
    | Some c when is_letter c -> (
        match parse_ident cursor with
        | Ok "true" -> Ok (LitBool true)
        | Ok "false" -> Ok (LitBool false)
        | Ok id -> Ok (LitIdentifier id)
        | Error e -> Error e)
    | _ -> Error "Expected literal value"

  let parse_field_option cursor =
    skip_whitespace_and_comments cursor;
    match parse_ident cursor with
    | Error e -> Error e
    | Ok name -> (
        skip_whitespace_and_comments cursor;
        match expect_char cursor '=' with
        | Error e -> Error e
        | Ok () -> (
            match parse_literal cursor with
            | Error e -> Error e
            | Ok value -> Ok { name; value }))

  let parse_field_options cursor =
    let rec loop acc =
      match parse_field_option cursor with
      | Error e -> Error e
      | Ok opt -> (
          skip_whitespace_and_comments cursor;
          match peek cursor with
          | Some ',' ->
              advance cursor;
              loop (opt :: acc)
          | _ -> Ok (List.rev (opt :: acc)))
    in
    loop []

  let parse_ranges cursor =
    let rec loop acc =
      match parse_int cursor with
      | Error e -> Error e
      | Ok start -> (
          skip_whitespace_and_comments cursor;
          let end_num =
            match peek cursor with
            | Some 't' -> (
                match expect_keyword cursor "to" with
                | Error _ -> Some start
                | Ok () -> (
                    skip_whitespace_and_comments cursor;
                    match peek cursor with
                    | Some 'm' -> (
                        match expect_keyword cursor "max" with
                        | Ok () -> None
                        | Error _ -> Some start)
                    | _ -> (
                        match parse_int cursor with
                        | Ok n -> Some n
                        | Error _ -> Some start)))
            | _ -> Some start
          in
          let range = (start, end_num) in
          skip_whitespace_and_comments cursor;
          match peek cursor with
          | Some ',' ->
              advance cursor;
              loop (range :: acc)
          | _ -> Ok (List.rev (range :: acc)))
    in
    loop []

  let parse_field_names cursor =
    let rec loop acc =
      match parse_string cursor with
      | Error e -> Error e
      | Ok name -> (
          skip_whitespace_and_comments cursor;
          match peek cursor with
          | Some ',' ->
              advance cursor;
              loop (name :: acc)
          | _ -> Ok (List.rev (name :: acc)))
    in
    loop []

  let parse_enum_value cursor =
    skip_whitespace_and_comments cursor;
    match parse_ident cursor with
    | Error e -> Error e
    | Ok name -> (
        skip_whitespace_and_comments cursor;
        match expect_char cursor '=' with
        | Error e -> Error e
        | Ok () -> (
            match parse_int cursor with
            | Error e -> Error e
            | Ok number -> (
                skip_whitespace_and_comments cursor;
                let options =
                  match peek cursor with
                  | Some '[' -> (
                      advance cursor;
                      match parse_field_options cursor with
                      | Ok opts -> (
                          skip_whitespace_and_comments cursor;
                          match expect_char cursor ']' with
                          | Ok () -> opts
                          | Error _ -> [])
                      | Error _ -> [])
                  | _ -> []
                in
                skip_whitespace_and_comments cursor;
                match expect_char cursor ';' with
                | Error e -> Error e
                | Ok () -> Ok { name; number; options })))

  let rec parse_enum cursor =
    skip_whitespace_and_comments cursor;
    let visibility =
      match peek cursor with
      | Some 'e' -> (
          let saved_pos = position cursor in
          match parse_ident cursor with
          | Ok "export" ->
              skip_whitespace_and_comments cursor;
              Some `Export
          | _ ->
              advance_by cursor (saved_pos - position cursor);
              None)
      | Some 'l' -> (
          let saved_pos = position cursor in
          match parse_ident cursor with
          | Ok "local" ->
              skip_whitespace_and_comments cursor;
              Some `Local
          | _ ->
              advance_by cursor (saved_pos - position cursor);
              None)
      | _ -> None
    in
    match expect_keyword cursor "enum" with
    | Error e -> Error e
    | Ok () -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '{' with
            | Error e -> Error e
            | Ok () ->
                let rec parse_body values_acc options_acc =
                  skip_whitespace_and_comments cursor;
                  match peek cursor with
                  | Some '}' ->
                      advance cursor;
                      Ok
                        {
                          visibility;
                          name;
                          values = List.rev values_acc;
                          options = List.rev options_acc;
                        }
                  | Some 'o' -> (
                      match expect_keyword cursor "option" with
                      | Ok () -> (
                          match parse_field_option cursor with
                          | Error e -> Error e
                          | Ok opt -> (
                              skip_whitespace_and_comments cursor;
                              match expect_char cursor ';' with
                              | Error e -> Error e
                              | Ok () ->
                                  parse_body values_acc (opt :: options_acc)))
                      | Error _ -> (
                          match parse_enum_value cursor with
                          | Error e -> Error e
                          | Ok value ->
                              parse_body (value :: values_acc) options_acc))
                  | Some 'r' -> (
                      match expect_keyword cursor "reserved" with
                      | Ok () -> (
                          skip_whitespace_and_comments cursor;
                          match peek cursor with
                          | Some '"' | Some '\'' -> (
                              match parse_field_names cursor with
                              | Error e -> Error e
                              | Ok _ -> (
                                  skip_whitespace_and_comments cursor;
                                  match expect_char cursor ';' with
                                  | Error e -> Error e
                                  | Ok () -> parse_body values_acc options_acc))
                          | _ -> (
                              match parse_ranges cursor with
                              | Error e -> Error e
                              | Ok _ -> (
                                  skip_whitespace_and_comments cursor;
                                  match expect_char cursor ';' with
                                  | Error e -> Error e
                                  | Ok () -> parse_body values_acc options_acc))
                          )
                      | Error _ -> (
                          match parse_enum_value cursor with
                          | Error e -> Error e
                          | Ok value ->
                              parse_body (value :: values_acc) options_acc))
                  | Some ';' ->
                      advance cursor;
                      parse_body values_acc options_acc
                  | None -> Error "Unexpected EOF in enum body"
                  | _ -> (
                      match parse_enum_value cursor with
                      | Error e -> Error e
                      | Ok value -> parse_body (value :: values_acc) options_acc
                      )
                in
                parse_body [] []))

  and parse_field cursor =
    skip_whitespace_and_comments cursor;
    let label =
      match peek cursor with
      | Some 'r' -> (
          match parse_ident cursor with
          | Ok "repeated" ->
              skip_whitespace_and_comments cursor;
              Some `Repeated
          | _ -> None)
      | _ -> None
    in
    match parse_field_type cursor with
    | Error e -> Error e
    | Ok field_type -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '=' with
            | Error e -> Error e
            | Ok () -> (
                match parse_int cursor with
                | Error e -> Error e
                | Ok number -> (
                    skip_whitespace_and_comments cursor;
                    let options =
                      match peek cursor with
                      | Some '[' -> (
                          advance cursor;
                          match parse_field_options cursor with
                          | Ok opts -> (
                              skip_whitespace_and_comments cursor;
                              match expect_char cursor ']' with
                              | Ok () -> opts
                              | Error _ -> [])
                          | Error _ -> [])
                      | _ -> []
                    in
                    skip_whitespace_and_comments cursor;
                    match expect_char cursor ';' with
                    | Error e -> Error e
                    | Ok () ->
                        Ok (Field { label; field_type; name; number; options }))
                )))

  and parse_oneof cursor =
    match expect_keyword cursor "oneof" with
    | Error e -> Error e
    | Ok () -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '{' with
            | Error e -> Error e
            | Ok () ->
                let rec parse_body fields_acc =
                  skip_whitespace_and_comments cursor;
                  match peek cursor with
                  | Some '}' ->
                      advance cursor;
                      Ok (Oneof { name; fields = List.rev fields_acc })
                  | Some 'o' -> (
                      match expect_keyword cursor "option" with
                      | Ok () -> (
                          match parse_field_option cursor with
                          | Error e -> Error e
                          | Ok _ -> (
                              skip_whitespace_and_comments cursor;
                              match expect_char cursor ';' with
                              | Error e -> Error e
                              | Ok () -> parse_body fields_acc))
                      | Error _ -> (
                          match parse_field_type cursor with
                          | Error e -> Error e
                          | Ok field_type -> (
                              match parse_ident cursor with
                              | Error e -> Error e
                              | Ok field_name -> (
                                  skip_whitespace_and_comments cursor;
                                  match expect_char cursor '=' with
                                  | Error e -> Error e
                                  | Ok () -> (
                                      match parse_int cursor with
                                      | Error e -> Error e
                                      | Ok number -> (
                                          skip_whitespace_and_comments cursor;
                                          let options =
                                            match peek cursor with
                                            | Some '[' -> (
                                                advance cursor;
                                                match
                                                  parse_field_options cursor
                                                with
                                                | Ok opts -> (
                                                    skip_whitespace_and_comments
                                                      cursor;
                                                    match
                                                      expect_char cursor ']'
                                                    with
                                                    | Ok () -> opts
                                                    | Error _ -> [])
                                                | Error _ -> [])
                                            | _ -> []
                                          in
                                          skip_whitespace_and_comments cursor;
                                          match expect_char cursor ';' with
                                          | Error e -> Error e
                                          | Ok () ->
                                              parse_body
                                                ({
                                                   field_type;
                                                   name = field_name;
                                                   number;
                                                   options;
                                                 }
                                                :: fields_acc)))))))
                  | Some ';' ->
                      advance cursor;
                      parse_body fields_acc
                  | None -> Error "Unexpected EOF in oneof body"
                  | _ -> (
                      match parse_field_type cursor with
                      | Error e -> Error e
                      | Ok field_type -> (
                          match parse_ident cursor with
                          | Error e -> Error e
                          | Ok field_name -> (
                              skip_whitespace_and_comments cursor;
                              match expect_char cursor '=' with
                              | Error e -> Error e
                              | Ok () -> (
                                  match parse_int cursor with
                                  | Error e -> Error e
                                  | Ok number -> (
                                      skip_whitespace_and_comments cursor;
                                      let options =
                                        match peek cursor with
                                        | Some '[' -> (
                                            advance cursor;
                                            match
                                              parse_field_options cursor
                                            with
                                            | Ok opts -> (
                                                skip_whitespace_and_comments
                                                  cursor;
                                                match
                                                  expect_char cursor ']'
                                                with
                                                | Ok () -> opts
                                                | Error _ -> [])
                                            | Error _ -> [])
                                        | _ -> []
                                      in
                                      skip_whitespace_and_comments cursor;
                                      match expect_char cursor ';' with
                                      | Error e -> Error e
                                      | Ok () ->
                                          parse_body
                                            ({
                                               field_type;
                                               name = field_name;
                                               number;
                                               options;
                                             }
                                            :: fields_acc))))))
                in
                parse_body []))

  and parse_map_field cursor =
    match expect_keyword cursor "map" with
    | Error e -> Error e
    | Ok () -> (
        skip_whitespace_and_comments cursor;
        match expect_char cursor '<' with
        | Error e -> Error e
        | Ok () -> (
            match parse_field_type cursor with
            | Error e -> Error e
            | Ok key_type -> (
                skip_whitespace_and_comments cursor;
                match expect_char cursor ',' with
                | Error e -> Error e
                | Ok () -> (
                    match parse_field_type cursor with
                    | Error e -> Error e
                    | Ok value_type -> (
                        skip_whitespace_and_comments cursor;
                        match expect_char cursor '>' with
                        | Error e -> Error e
                        | Ok () -> (
                            match parse_ident cursor with
                            | Error e -> Error e
                            | Ok name -> (
                                skip_whitespace_and_comments cursor;
                                match expect_char cursor '=' with
                                | Error e -> Error e
                                | Ok () -> (
                                    match parse_int cursor with
                                    | Error e -> Error e
                                    | Ok number -> (
                                        skip_whitespace_and_comments cursor;
                                        let options =
                                          match peek cursor with
                                          | Some '[' -> (
                                              advance cursor;
                                              match
                                                parse_field_options cursor
                                              with
                                              | Ok opts -> (
                                                  skip_whitespace_and_comments
                                                    cursor;
                                                  match
                                                    expect_char cursor ']'
                                                  with
                                                  | Ok () -> opts
                                                  | Error _ -> [])
                                              | Error _ -> [])
                                          | _ -> []
                                        in
                                        skip_whitespace_and_comments cursor;
                                        match expect_char cursor ';' with
                                        | Error e -> Error e
                                        | Ok () ->
                                            Ok
                                              (MapField
                                                 {
                                                   key_type;
                                                   value_type;
                                                   name;
                                                   number;
                                                   options;
                                                 }))))))))))

  and parse_message cursor =
    skip_whitespace_and_comments cursor;
    let visibility =
      match peek cursor with
      | Some 'e' -> (
          let saved_pos = position cursor in
          match parse_ident cursor with
          | Ok "export" ->
              skip_whitespace_and_comments cursor;
              Some `Export
          | _ ->
              advance_by cursor (saved_pos - position cursor);
              None)
      | Some 'l' -> (
          let saved_pos = position cursor in
          match parse_ident cursor with
          | Ok "local" ->
              skip_whitespace_and_comments cursor;
              Some `Local
          | _ ->
              advance_by cursor (saved_pos - position cursor);
              None)
      | _ -> None
    in
    match expect_keyword cursor "message" with
    | Error e -> Error e
    | Ok () -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '{' with
            | Error e -> Error e
            | Ok () ->
                let rec parse_body elements_acc =
                  skip_whitespace_and_comments cursor;
                  match peek cursor with
                  | Some '}' ->
                      advance cursor;
                      Ok { visibility; name; elements = List.rev elements_acc }
                  | Some 'e' -> (
                      let saved_pos = position cursor in
                      match parse_ident cursor with
                      | Ok "enum" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_enum cursor with
                          | Error e -> Error e
                          | Ok enum_def ->
                              parse_body (NestedEnum enum_def :: elements_acc))
                      | Ok "extensions" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match expect_keyword cursor "extensions" with
                          | Error e -> Error e
                          | Ok () -> (
                              match parse_ranges cursor with
                              | Error e -> Error e
                              | Ok ranges -> (
                                  skip_whitespace_and_comments cursor;
                                  match expect_char cursor ';' with
                                  | Error e -> Error e
                                  | Ok () ->
                                      parse_body
                                        (Extensions ranges :: elements_acc))))
                      | _ -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_field cursor with
                          | Error e -> Error e
                          | Ok field -> parse_body (field :: elements_acc)))
                  | Some 'm' -> (
                      let saved_pos = position cursor in
                      match parse_ident cursor with
                      | Ok "message" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_message cursor with
                          | Error e -> Error e
                          | Ok msg_def ->
                              parse_body (NestedMessage msg_def :: elements_acc)
                          )
                      | Ok "map" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_map_field cursor with
                          | Error e -> Error e
                          | Ok map_field ->
                              parse_body (map_field :: elements_acc))
                      | _ -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_field cursor with
                          | Error e -> Error e
                          | Ok field -> parse_body (field :: elements_acc)))
                  | Some 'o' -> (
                      let saved_pos = position cursor in
                      match parse_ident cursor with
                      | Ok "oneof" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_oneof cursor with
                          | Error e -> Error e
                          | Ok oneof_def ->
                              parse_body (oneof_def :: elements_acc))
                      | Ok "option" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match expect_keyword cursor "option" with
                          | Error e -> Error e
                          | Ok () -> (
                              match parse_field_option cursor with
                              | Error e -> Error e
                              | Ok opt -> (
                                  skip_whitespace_and_comments cursor;
                                  match expect_char cursor ';' with
                                  | Error e -> Error e
                                  | Ok () ->
                                      parse_body (Option opt :: elements_acc))))
                      | _ -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_field cursor with
                          | Error e -> Error e
                          | Ok field -> parse_body (field :: elements_acc)))
                  | Some 'r' -> (
                      let saved_pos = position cursor in
                      match parse_ident cursor with
                      | Ok "reserved" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match expect_keyword cursor "reserved" with
                          | Error e -> Error e
                          | Ok () -> (
                              skip_whitespace_and_comments cursor;
                              match peek cursor with
                              | Some '"' | Some '\'' -> (
                                  match parse_field_names cursor with
                                  | Error e -> Error e
                                  | Ok names -> (
                                      skip_whitespace_and_comments cursor;
                                      match expect_char cursor ';' with
                                      | Error e -> Error e
                                      | Ok () ->
                                          parse_body
                                            (Reserved (`Names names)
                                            :: elements_acc)))
                              | _ -> (
                                  match parse_ranges cursor with
                                  | Error e -> Error e
                                  | Ok ranges -> (
                                      skip_whitespace_and_comments cursor;
                                      match expect_char cursor ';' with
                                      | Error e -> Error e
                                      | Ok () ->
                                          parse_body
                                            (Reserved (`Numbers ranges)
                                            :: elements_acc)))))
                      | Ok "repeated" -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_field cursor with
                          | Error e -> Error e
                          | Ok field -> parse_body (field :: elements_acc))
                      | _ -> (
                          advance_by cursor (saved_pos - position cursor);
                          match parse_field cursor with
                          | Error e -> Error e
                          | Ok field -> parse_body (field :: elements_acc)))
                  | Some ';' ->
                      advance cursor;
                      parse_body elements_acc
                  | None -> Error "Unexpected EOF in message body"
                  | _ -> (
                      match parse_field cursor with
                      | Error e -> Error e
                      | Ok field -> parse_body (field :: elements_acc))
                in
                parse_body []))

  let parse_rpc cursor =
    match expect_keyword cursor "rpc" with
    | Error e -> Error e
    | Ok () -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '(' with
            | Error e -> Error e
            | Ok () -> (
                skip_whitespace_and_comments cursor;
                let input_stream =
                  match peek cursor with
                  | Some 's' -> (
                      match expect_keyword cursor "stream" with
                      | Ok () ->
                          skip_whitespace_and_comments cursor;
                          true
                      | Error _ -> false)
                  | _ -> false
                in
                match parse_full_ident cursor with
                | Error e -> Error e
                | Ok input_type -> (
                    skip_whitespace_and_comments cursor;
                    match expect_char cursor ')' with
                    | Error e -> Error e
                    | Ok () -> (
                        skip_whitespace_and_comments cursor;
                        match expect_keyword cursor "returns" with
                        | Error e -> Error e
                        | Ok () -> (
                            skip_whitespace_and_comments cursor;
                            match expect_char cursor '(' with
                            | Error e -> Error e
                            | Ok () -> (
                                skip_whitespace_and_comments cursor;
                                let output_stream =
                                  match peek cursor with
                                  | Some 's' -> (
                                      match expect_keyword cursor "stream" with
                                      | Ok () ->
                                          skip_whitespace_and_comments cursor;
                                          true
                                      | Error _ -> false)
                                  | _ -> false
                                in
                                match parse_full_ident cursor with
                                | Error e -> Error e
                                | Ok output_type -> (
                                    skip_whitespace_and_comments cursor;
                                    match expect_char cursor ')' with
                                    | Error e -> Error e
                                    | Ok () ->
                                        skip_whitespace_and_comments cursor;
                                        let options =
                                          match peek cursor with
                                          | Some '{' ->
                                              advance cursor;
                                              let rec parse_opts opts_acc =
                                                skip_whitespace_and_comments
                                                  cursor;
                                                match peek cursor with
                                                | Some '}' ->
                                                    advance cursor;
                                                    List.rev opts_acc
                                                | Some 'o' -> (
                                                    match
                                                      expect_keyword cursor
                                                        "option"
                                                    with
                                                    | Ok () -> (
                                                        match
                                                          parse_field_option
                                                            cursor
                                                        with
                                                        | Error _ ->
                                                            List.rev opts_acc
                                                        | Ok opt -> (
                                                            skip_whitespace_and_comments
                                                              cursor;
                                                            match
                                                              expect_char cursor
                                                                ';'
                                                            with
                                                            | Ok () ->
                                                                parse_opts
                                                                  (opt
                                                                 :: opts_acc)
                                                            | Error _ ->
                                                                List.rev
                                                                  opts_acc))
                                                    | Error _ ->
                                                        List.rev opts_acc)
                                                | Some ';' ->
                                                    advance cursor;
                                                    parse_opts opts_acc
                                                | _ -> List.rev opts_acc
                                              in
                                              parse_opts []
                                          | Some ';' ->
                                              advance cursor;
                                              []
                                          | _ -> []
                                        in
                                        Ok
                                          {
                                            name;
                                            input_type;
                                            input_stream;
                                            output_type;
                                            output_stream;
                                            options;
                                          }))))))))

  let parse_service cursor =
    match expect_keyword cursor "service" with
    | Error e -> Error e
    | Ok () -> (
        match parse_ident cursor with
        | Error e -> Error e
        | Ok name -> (
            skip_whitespace_and_comments cursor;
            match expect_char cursor '{' with
            | Error e -> Error e
            | Ok () ->
                let rec parse_body rpcs_acc options_acc =
                  skip_whitespace_and_comments cursor;
                  match peek cursor with
                  | Some '}' ->
                      advance cursor;
                      Ok
                        {
                          name;
                          rpcs = List.rev rpcs_acc;
                          options = List.rev options_acc;
                        }
                  | Some 'r' -> (
                      match parse_rpc cursor with
                      | Error e -> Error e
                      | Ok rpc -> parse_body (rpc :: rpcs_acc) options_acc)
                  | Some 'o' -> (
                      match expect_keyword cursor "option" with
                      | Error e -> Error e
                      | Ok () -> (
                          match parse_field_option cursor with
                          | Error e -> Error e
                          | Ok opt -> (
                              skip_whitespace_and_comments cursor;
                              match expect_char cursor ';' with
                              | Error e -> Error e
                              | Ok () -> parse_body rpcs_acc (opt :: options_acc)
                              )))
                  | Some ';' ->
                      advance cursor;
                      parse_body rpcs_acc options_acc
                  | None -> Error "Unexpected EOF in service body"
                  | _ -> Error "Expected rpc or option in service"
                in
                parse_body [] []))

  let parse cursor =
    skip_whitespace_and_comments cursor;
    let rec parse_top_level (st : proto_file) : (proto_file, string) result =
      skip_whitespace_and_comments cursor;
      if is_eof cursor then
        Ok
          {
            edition = st.edition;
            package = st.package;
            imports = List.rev st.imports;
            definitions = List.rev st.definitions;
          }
      else
        match peek cursor with
        | Some 's' -> (
            let saved_pos = position cursor in
            match parse_ident cursor with
            | Ok "syntax" -> (
                skip_whitespace_and_comments cursor;
                match expect_char cursor '=' with
                | Error e -> Error e
                | Ok () -> (
                    match parse_string cursor with
                    | Error e -> Error e
                    | Ok _ -> (
                        skip_whitespace_and_comments cursor;
                        match expect_char cursor ';' with
                        | Error e -> Error e
                        | Ok () -> parse_top_level st)))
            | Ok "service" -> (
                advance_by cursor (saved_pos - position cursor);
                match parse_service cursor with
                | Error e -> Error e
                | Ok service_def ->
                    parse_top_level
                      {
                        st with
                        definitions = Service service_def :: st.definitions;
                      })
            | _ ->
                advance_by cursor (saved_pos - position cursor);
                Error "Unknown keyword starting with 's'")
        | Some 'e' -> (
            let saved_pos = position cursor in
            match parse_ident cursor with
            | Ok "edition" -> (
                skip_whitespace_and_comments cursor;
                match expect_char cursor '=' with
                | Error e -> Error e
                | Ok () -> (
                    match parse_string cursor with
                    | Error e -> Error e
                    | Ok ed -> (
                        skip_whitespace_and_comments cursor;
                        match expect_char cursor ';' with
                        | Error e -> Error e
                        | Ok () -> parse_top_level { st with edition = Some ed }
                        )))
            | Ok "enum" -> (
                advance_by cursor (saved_pos - position cursor);
                match parse_enum cursor with
                | Error e -> Error e
                | Ok enum_def ->
                    parse_top_level
                      { st with definitions = Enum enum_def :: st.definitions })
            | _ ->
                advance_by cursor (saved_pos - position cursor);
                Error "Unknown keyword starting with 'e'")
        | Some 'p' -> (
            match expect_keyword cursor "package" with
            | Error e -> Error e
            | Ok () -> (
                match parse_full_ident cursor with
                | Error e -> Error e
                | Ok pkg -> (
                    skip_whitespace_and_comments cursor;
                    match expect_char cursor ';' with
                    | Error e -> Error e
                    | Ok () -> parse_top_level { st with package = Some pkg })))
        | Some 'i' -> (
            match expect_keyword cursor "import" with
            | Error e -> Error e
            | Ok () -> (
                skip_whitespace_and_comments cursor;
                let modifier =
                  match peek cursor with
                  | Some 'p' -> (
                      match expect_keyword cursor "public" with
                      | Ok () ->
                          skip_whitespace_and_comments cursor;
                          Some `Public
                      | Error _ -> None)
                  | Some 'w' -> (
                      match expect_keyword cursor "weak" with
                      | Ok () ->
                          skip_whitespace_and_comments cursor;
                          Some `Weak
                      | Error _ -> None)
                  | _ -> None
                in
                match parse_string cursor with
                | Error e -> Error e
                | Ok path -> (
                    skip_whitespace_and_comments cursor;
                    match expect_char cursor ';' with
                    | Error e -> Error e
                    | Ok () ->
                        parse_top_level
                          { st with imports = (modifier, path) :: st.imports }))
            )
        | Some 'o' -> (
            match expect_keyword cursor "option" with
            | Error e -> Error e
            | Ok () -> (
                match parse_field_option cursor with
                | Error e -> Error e
                | Ok opt -> (
                    skip_whitespace_and_comments cursor;
                    match expect_char cursor ';' with
                    | Error e -> Error e
                    | Ok () ->
                        parse_top_level
                          { st with definitions = Option opt :: st.definitions }
                    )))
        | Some 'm' -> (
            match parse_message cursor with
            | Error e -> Error e
            | Ok msg_def ->
                parse_top_level
                  { st with definitions = Message msg_def :: st.definitions })
        | Some ';' ->
            advance cursor;
            parse_top_level st
        | None -> Error "Unexpected EOF at top level"
        | _ -> Error "Unknown top-level declaration"
    in
    parse_top_level
      { edition = None; package = None; imports = []; definitions = [] }
end

let parse input =
  let cursor = Iter.MutCursor.create input in
  Parser.parse cursor

let print _t = "Not yet implemented"

let to_json t =
  let open Data.Json in
  let literal_to_json = function
    | LitInt i -> obj [ ("type", string "int"); ("value", int i) ]
    | LitFloat f -> obj [ ("type", string "float"); ("value", float f) ]
    | LitString s -> obj [ ("type", string "string"); ("value", string s) ]
    | LitBool b -> obj [ ("type", string "bool"); ("value", bool b) ]
    | LitIdentifier id ->
        obj [ ("type", string "identifier"); ("value", string id) ]
  in

  let field_type_to_json = function
    | Double -> string "double"
    | Float -> string "float"
    | Int32 -> string "int32"
    | Int64 -> string "int64"
    | Uint32 -> string "uint32"
    | Uint64 -> string "uint64"
    | Sint32 -> string "sint32"
    | Sint64 -> string "sint64"
    | Fixed32 -> string "fixed32"
    | Fixed64 -> string "fixed64"
    | Sfixed32 -> string "sfixed32"
    | Sfixed64 -> string "sfixed64"
    | Bool -> string "bool"
    | String -> string "string"
    | Bytes -> string "bytes"
    | MessageType name ->
        obj [ ("type", string "message"); ("name", string name) ]
    | EnumType name -> obj [ ("type", string "enum"); ("name", string name) ]
  in

  let field_option_to_json (field_opt : field_option) =
    obj
      [
        ("name", string field_opt.name);
        ("value", literal_to_json field_opt.value);
      ]
  in

  let field_to_json (f : field) =
    let label_json =
      match f.label with
      | None -> null
      | Some `Repeated -> string "repeated"
      | Some `None -> null
    in
    obj
      [
        ("type", string "field");
        ("label", label_json);
        ("field_type", field_type_to_json f.field_type);
        ("name", string f.name);
        ("number", int f.number);
        ("options", array (List.map field_option_to_json f.options));
      ]
  in

  let oneof_field_to_json (f : oneof_field) =
    obj
      [
        ("type", string "oneof_field");
        ("field_type", field_type_to_json f.field_type);
        ("name", string f.name);
        ("number", int f.number);
        ("options", array (List.map field_option_to_json f.options));
      ]
  in

  let map_field_to_json (f : map_field) =
    obj
      [
        ("type", string "map");
        ("key_type", field_type_to_json f.key_type);
        ("value_type", field_type_to_json f.value_type);
        ("name", string f.name);
        ("number", int f.number);
        ("options", array (List.map field_option_to_json f.options));
      ]
  in

  let enum_value_to_json (v : enum_value) =
    obj
      [
        ("name", string v.name);
        ("number", int v.number);
        ("options", array (List.map field_option_to_json v.options));
      ]
  in

  let rec enum_def_to_json (e : enum_def) =
    let visibility_json =
      match e.visibility with
      | None -> null
      | Some `Export -> string "export"
      | Some `Local -> string "local"
    in
    obj
      [
        ("type", string "enum");
        ("visibility", visibility_json);
        ("name", string e.name);
        ("values", array (List.map enum_value_to_json e.values));
        ("options", array (List.map field_option_to_json e.options));
      ]
  and oneof_to_json (o : oneof) =
    obj
      [
        ("type", string "oneof");
        ("name", string o.name);
        ("fields", array (List.map oneof_field_to_json o.fields));
      ]
  and message_element_to_json (elem : message_element) =
    match elem with
    | Field f -> field_to_json f
    | Oneof o -> oneof_to_json o
    | MapField m -> map_field_to_json m
    | NestedEnum e -> enum_def_to_json e
    | NestedMessage m -> message_def_to_json m
    | Option opt ->
        obj [ ("type", string "option"); ("option", field_option_to_json opt) ]
    | Reserved (`Numbers ranges) ->
        let range_to_json (start, end_opt) =
          match end_opt with
          | None -> obj [ ("start", int start); ("end", string "max") ]
          | Some e -> obj [ ("start", int start); ("end", int e) ]
        in
        obj
          [
            ("type", string "reserved");
            ("kind", string "numbers");
            ("ranges", array (List.map range_to_json ranges));
          ]
    | Reserved (`Names names) ->
        obj
          [
            ("type", string "reserved");
            ("kind", string "names");
            ("names", array (List.map string names));
          ]
    | Extensions ranges ->
        let range_to_json (start, end_opt) =
          match end_opt with
          | None -> obj [ ("start", int start); ("end", string "max") ]
          | Some e -> obj [ ("start", int start); ("end", int e) ]
        in
        obj
          [
            ("type", string "extensions");
            ("ranges", array (List.map range_to_json ranges));
          ]
  and message_def_to_json (m : message_def) =
    let visibility_json =
      match m.visibility with
      | None -> null
      | Some `Export -> string "export"
      | Some `Local -> string "local"
    in
    obj
      [
        ("type", string "message");
        ("visibility", visibility_json);
        ("name", string m.name);
        ("elements", array (List.map message_element_to_json m.elements));
      ]
  in

  let rpc_to_json (r : rpc) =
    obj
      [
        ("name", string r.name);
        ("input_type", string r.input_type);
        ("input_stream", bool r.input_stream);
        ("output_type", string r.output_type);
        ("output_stream", bool r.output_stream);
        ("options", array (List.map field_option_to_json r.options));
      ]
  in

  let service_def_to_json (s : service_def) =
    obj
      [
        ("type", string "service");
        ("name", string s.name);
        ("rpcs", array (List.map rpc_to_json s.rpcs));
        ("options", array (List.map field_option_to_json s.options));
      ]
  in

  let top_level_to_json = function
    | Message m -> message_def_to_json m
    | Enum e -> enum_def_to_json e
    | Service s -> service_def_to_json s
    | Option opt ->
        obj [ ("type", string "option"); ("option", field_option_to_json opt) ]
  in

  let import_to_json (modifier, path) =
    let modifier_json =
      match modifier with
      | None -> null
      | Some `Public -> string "public"
      | Some `Weak -> string "weak"
    in
    obj [ ("modifier", modifier_json); ("path", string path) ]
  in

  obj
    [
      ("edition", match t.edition with None -> null | Some e -> string e);
      ("package", match t.package with None -> null | Some p -> string p);
      ("imports", array (List.map import_to_json t.imports));
      ("definitions", array (List.map top_level_to_json t.definitions));
    ]
