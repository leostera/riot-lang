open Std

(* Same type definitions as before *)
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

  (* Rest of helper functions remain the same but are pure *)
  let parse_ident cursor =
    skip_whitespace_and_comments cursor;
    match peek cursor with
    | Some c when is_letter c -> Ok (take_while cursor is_ident_char)
    | _ -> Error "Expected identifier"

  (* Functional accumulator pattern for lists *)
  let rec parse_list parse_fn cursor =
    let rec loop acc =
      match parse_fn cursor with
      | Error _ -> Ok (List.rev acc)
      | Ok item -> (
          skip_whitespace_and_comments cursor;
          match peek cursor with
          | Some ',' ->
              advance cursor;
              loop (item :: acc)
          | _ -> Ok (List.rev (item :: acc)))
    in
    loop []

  (* Example: parse_field_options without Cell *)
  let parse_field_options cursor =
    let rec loop acc =
      skip_whitespace_and_comments cursor;
      match parse_ident cursor with
      | Error _ -> Ok (List.rev acc)
      | Ok name -> (
          skip_whitespace_and_comments cursor;
          match peek cursor with
          | Some '=' -> (
              advance cursor;
              (* parse literal *)
              skip_whitespace_and_comments cursor;
              let opt = { name; value = LitIdentifier "placeholder" } in
              match peek cursor with
              | Some ',' ->
                  advance cursor;
                  loop (opt :: acc)
              | _ -> Ok (List.rev (opt :: acc)))
          | _ -> Error "Expected '=' in option")
    in
    loop []

  (* Stub - this shows the pattern *)
  let parse _cursor = Error "Functional parser stub"
end

let parse input =
  let cursor = Iter.MutCursor.create input in
  Parser.parse cursor

let print _t = "Not yet implemented"
