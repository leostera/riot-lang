open Global
open Sync
open Sync.Cell
  open IO
  open Collections

type row = string list
type t = row list

type config = {
  delimiter : char;
  quote : char;
  escape : char;
  trim_fields : bool;
}

type error =
  | Unterminated_quote of { line : int; column : int }
  | Invalid_escape_sequence of { line : int; column : int }
  | Empty_input
  | Unknown_error of string

let error_to_string = function
  | Unterminated_quote { line; column } ->
      "Unterminated quote at line " ^ string_of_int line ^ ", column " ^ string_of_int column
  | Invalid_escape_sequence { line; column } ->
      "Invalid escape sequence at line " ^ string_of_int line ^ ", column " ^ string_of_int column
  | Empty_input -> "Empty CSV input"
  | Unknown_error msg -> "Unknown error: " ^ msg

let default_config =
  { delimiter = ','; quote = '"'; escape = '"'; trim_fields = false }

let config ?(delimiter = ',') ?(quote = '"') ?(escape = '"')
    ?(trim_fields = false) () =
  { delimiter; quote; escape; trim_fields }

let of_string ?(config = default_config) str =
  let cursor = Iter.MutCursor.create str in
  let line = Cell.create 1 in
  let column = Cell.create 1 in

  let peek () = Iter.MutCursor.peek cursor in

  let advance () =
    match peek () with
    | Some '\n' ->
        Iter.MutCursor.advance cursor;
      line := (!line + 1);
      column := 1
    | Some _ ->
        Iter.MutCursor.advance cursor;
      column :=  (!column + 1)
    | None -> ()
  in

  let exception Csv_parse_error of error in
  let raise_error err = raise (Csv_parse_error err) in

  let trim s = if config.trim_fields then String.trim s else s in

  let parse_field () =
    match peek () with
    | Some c when c = config.quote ->
        advance ();
        let buffer = Buffer.create 16 in
        let rec loop () =
          match peek () with
          | None ->
              raise_error
                (Unterminated_quote
                   { line = !line; column = !column })
          | Some c when c = config.escape -> (
              advance ();
              match peek () with
              | Some next_c when next_c = config.quote ->
                  Buffer.add_char buffer config.quote;
                  advance ();
                  loop ()
              | Some next_c when next_c = config.escape ->
                  Buffer.add_char buffer config.escape;
                  advance ();
                  loop ()
              | None ->
                  raise_error
                    (Unterminated_quote
                       { line = !line; column = !column })
              | _ ->
                  Buffer.add_char buffer c;
                  loop ())
          | Some c when c = config.quote ->
              advance ();
              trim (Buffer.contents buffer)
          | Some c ->
              Buffer.add_char buffer c;
              advance ();
              loop ()
        in
        loop ()
    | _ ->
        let field =
          Iter.MutCursor.take_while cursor (fun c ->
              c != config.delimiter && c != '\n' && c != '\r')
        in
        trim field
  in

  let parse_row () =
    let rec skip_empty_lines () =
      match peek () with
      | Some '\n' ->
          advance ();
          skip_empty_lines ()
      | Some '\r' ->
          advance ();
          (match peek () with Some '\n' -> advance () | _ -> ());
          skip_empty_lines ()
      | _ -> ()
    in

    skip_empty_lines ();

    if Iter.MutCursor.is_eof cursor then None
    else
      let rec loop acc =
        let field = parse_field () in
        match peek () with
        | Some c when c = config.delimiter ->
            advance ();
            loop (field :: acc)
        | Some '\r' ->
            advance ();
            (match peek () with Some '\n' -> advance () | _ -> ());
            Some (List.rev (field :: acc))
        | Some '\n' ->
            advance ();
            Some (List.rev (field :: acc))
        | None -> Some (List.rev (field :: acc))
        | Some c ->
            advance ();
            loop (field :: acc)
      in
      loop []
  in

  let module CsvIter = struct
    type state = unit
    type item = (row, error) result

    let next () =
      try parse_row () |> Option.map (fun row -> Ok row) with
      | Csv_parse_error err -> Some (Error err)
      | exn -> Some (Error (Unknown_error (Exception.to_string exn)))

    let size () = 0
    let clone () = ()
  end in
  Iter.MutIterator.make (module CsvIter) ()

let read ?(config = default_config) path =
  let content = Fs.read path |> Result.unwrap in
  of_string ~config content

let to_string ?(config = default_config) ?headers data =
  let needs_quoting field =
    String.contains field config.delimiter
    || String.contains field config.quote
    || String.contains field '\n' || String.contains field '\r'
  in

  let escape_field field =
    if needs_quoting field then (
      let buffer = Buffer.create (String.length field + 2) in
      Buffer.add_char buffer config.quote;
      String.iter
        (fun c ->
          if c = config.quote then (
            Buffer.add_char buffer config.escape;
            Buffer.add_char buffer config.quote)
          else Buffer.add_char buffer c)
        field;
      Buffer.add_char buffer config.quote;
      Buffer.contents buffer)
    else field
  in

  let all_rows = match headers with Some h -> h :: data | None -> data in

  let buffer = Buffer.create 256 in
  List.iter
    (fun row ->
      let escaped = List.map escape_field row in
      Buffer.add_string buffer
        (String.concat (String.make 1 config.delimiter) escaped);
      Buffer.add_char buffer '\n')
    all_rows;
  Buffer.contents buffer

let write ?(config = default_config) ?headers ~data path =
  let content = to_string ~config ?headers data in
  Fs.write content path
