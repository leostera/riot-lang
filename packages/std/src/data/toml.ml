open Global
open IO
open Collections
open Sync
open Sync.Cell

type value =
  | String of string
  | Int of int
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of { path: string }
  | File_read_error of { path: string; reason: string }
  | Parse_error of { position: int; context: string; reason: string }
  | Unterminated_string of { position: int }
  | Unterminated_array of { position: int }
  | Unexpected_char of { position: int; found: char; expected: string }

let error_to_string = fun __tmp1 ->
  match __tmp1 with
  | Invalid_path { path } -> "Invalid path: " ^ path
  | File_read_error { path; reason } -> "Failed to read file " ^ path ^ ": " ^ reason
  | Parse_error { position; context; reason } ->
      "Parse error at position " ^ Int.to_string position ^ " (context: " ^ context ^ "): " ^ reason
  | Unterminated_string { position } -> "Unterminated string at position " ^ Int.to_string position
  | Unterminated_array { position } -> "Unterminated array at position " ^ Int.to_string position
  | Unexpected_char { position; found; expected } ->
      "Unexpected character '"
      ^ String.make ~len:1 ~char:found
      ^ "' at position "
      ^ Int.to_string position
      ^ " (expected "
      ^ expected
      ^ ")"

exception Parse_exception of error

type section = {
  name: string;
  items: (string * value) list;
}

let parse = fun content ->
  let len = String.length content in
  let pos = Cell.create 0 in
  let at_end () = !pos >= len in
  let peek () =
    if at_end () then
      None
    else
      Some (String.get_unchecked content ~at:!pos)
  in
  let advance () =
    if not (at_end ()) then
      Cell.incr pos
  in
  let current_char () =
    if at_end () then
      '\000'
    else
      String.get_unchecked content ~at:!pos
  in
  (* Skip whitespace (spaces, tabs) but NOT newlines *)
  let rec skip_ws () =
    match peek () with
    | Some (' ' | '\t' | '\r') ->
        advance ();
        skip_ws ()
    | _ -> ()
  in
  (* Skip to end of line *)
  let skip_to_eol () =
    while (not (at_end ())) && current_char () != '\n' do
      advance ()
    done;
    if not (at_end ()) then
      advance ()
  in
  (* Skip whitespace, newlines, and comments *)
  let rec skip_noise () =
    skip_ws ();
    match peek () with
    | Some '#' ->
        skip_to_eol ();
        skip_noise ()
    | Some '\n' ->
        advance ();
        skip_noise ()
    | _ -> ()
  in
  (* Parse a quoted string *)
  let parse_quoted_string () =
    let start_pos = !pos in
    if current_char () != '"' then
      raise
        (Parse_exception (Unexpected_char {
          position = !pos;
          found = current_char ();
          expected = "double-quote";
        }));
    advance ();
    let buf = Buffer.create ~size:16 in
    let rec loop () =
      if at_end () then
        raise (Parse_exception (Unterminated_string { position = start_pos }));
      match current_char () with
      | '"' ->
          advance ();
          Buffer.contents buf
      | '\\' ->
          advance ();
          if at_end () then
            raise (Parse_exception (Unterminated_string { position = start_pos }));
          (
            match current_char () with
            | 'n' -> Buffer.add_char buf '\n'
            | 't' -> Buffer.add_char buf '\t'
            | 'r' -> Buffer.add_char buf '\r'
            | '\\' -> Buffer.add_char buf '\\'
            | '"' -> Buffer.add_char buf '"'
            | c -> Buffer.add_char buf c
          );
          advance ();
          loop ()
      | c ->
          Buffer.add_char buf c;
          advance ();
          loop ()
    in
    String (loop ())
  in
  (* Parse an array *)
  let rec parse_array () =
    let start_pos = !pos in
    if current_char () != '[' then
      raise
        (Parse_exception (Unexpected_char {
          position = !pos;
          found = current_char ();
          expected = "[";
        }));
    advance ();
    let items = ref [] in
    let rec parse_items () =
      skip_noise ();
      if at_end () then
        raise (Parse_exception (Unterminated_array { position = start_pos }));
      match current_char () with
      | ']' ->
          advance ();
          Array (List.reverse !items)
      | _ -> (
          let value = parse_value () in
          items := value :: !items;
          skip_noise ();
          match peek () with
          | Some ',' ->
              advance ();
              parse_items ()
          | Some ']' ->
              advance ();
              Array (List.reverse !items)
          | None -> raise (Parse_exception (Unterminated_array { position = start_pos }))
          | Some c ->
              raise
                (Parse_exception (Unexpected_char {
                  position = !pos;
                  found = c;
                  expected = ", or ]";
                }))
        )
    in
    parse_items ()
  (* Parse an inline table { key = value, ... } *)
  and parse_inline_table () =
    let start_pos = !pos in
    if current_char () != '{' then
      raise
        (Parse_exception (Unexpected_char {
          position = !pos;
          found = current_char ();
          expected = "{";
        }));
    advance ();
    skip_ws ();
    let items = ref [] in
    let rec parse_items () =
      skip_ws ();
      if at_end () then
        raise
          (Parse_exception (Parse_error {
            position = start_pos;
            context = "inline table";
            reason = "unterminated";
          }));
      match current_char () with
      | '}' ->
          advance ();
          Table (List.reverse !items)
      | _ -> (
          (* Parse key = value *)
          let key_start = !pos in
          while (not (at_end ())) && current_char () != '=' && current_char () != '}' do
            advance ()
          done;
          let key = String.trim (String.sub content ~offset:key_start ~len:(!pos - key_start)) in
          skip_ws ();
          if at_end () || current_char () != '=' then
            raise
              (Parse_exception (Parse_error {
                position = !pos;
                context = "inline table";
                reason = "expected =";
              }));
          advance ();
          skip_ws ();
          let value = parse_value () in
          items := (key, value) :: !items;
          skip_ws ();
          match current_char () with
          | ',' ->
              advance ();
              parse_items ()
          | '}' ->
              advance ();
              Table (List.reverse !items)
          | _ ->
              raise
                (Parse_exception (Parse_error {
                  position = !pos;
                  context = "inline table";
                  reason = "expected , or }";
                }))
        )
    in
    parse_items ()
  (* Parse a value *)
  and parse_value () =
    skip_noise ();
    if at_end () then
      raise
        (Parse_exception (Parse_error {
          position = !pos;
          context = "value";
          reason = "unexpected end";
        }));
    match current_char () with
    | '"' -> parse_quoted_string ()
    | '[' -> parse_array ()
    | '{' -> parse_inline_table ()
    | 't' when !pos + 4 <= len && String.sub content ~offset:!pos ~len:4 = "true" ->
        pos := !pos + 4;
        Bool true
    | 'f' when !pos + 5 <= len && String.sub content ~offset:!pos ~len:5 = "false" ->
        pos := !pos + 5;
        Bool false
    | '0' .. '9'
    | '-'
    | '+' ->
        (* Try to parse as integer first *)
        let start = !pos in
        if current_char () = '-' || current_char () = '+' then
          advance ();
        while not (at_end ()) && current_char () >= '0' && current_char () <= '9' do
          advance ()
        done;
        let str = String.trim (String.sub content ~offset:start ~len:(!pos - start)) in
        (
          match Int.parse str with
          | Some value -> Int value
          | None -> String str
        )
    | _ ->
        (* Bare string - read until comma, bracket, newline, or comment *)
        let start = !pos in
        let rec advance_bare_string () =
          if not (at_end ()) then
            match current_char () with
            | ','
            | ']'
            | '\n'
            | '#'
            | '}' -> ()
            | _ ->
                advance ();
                advance_bare_string ()
        in
        advance_bare_string ();
        let str = String.trim (String.sub content ~offset:start ~len:(!pos - start)) in
        String str
  in
  (* Parse a key (identifier before =) *)
  let parse_key () =
    skip_ws ();
    let start = !pos in
    while (not (at_end ())) && current_char () != '=' do
      advance ()
    done;
    String.trim (String.sub content ~offset:start ~len:(!pos - start))
  in
  (* Parse section header [name] or [[name]] *)
  let parse_section_header () =
    if current_char () != '[' then
      raise
        (Parse_exception (Unexpected_char {
          position = !pos;
          found = current_char ();
          expected = "[";
        }));
    advance ();
    skip_ws ();
    let is_array = current_char () = '[' in
    if is_array then (
      advance ();
      skip_ws ()
    );
    let start = !pos in
    while (not (at_end ())) && current_char () != ']' do
      advance ()
    done;
    if at_end () then
      raise
        (Parse_exception (Parse_error {
          position = start;
          context = "section";
          reason = "unterminated";
        }));
    let name = String.trim (String.sub content ~offset:start ~len:(!pos - start)) in
    advance ();
    (* skip first ] *)
    (* If array of tables, expect another ] *)
    if is_array then (
      skip_ws ();
      if current_char () != ']' then
        raise
          (Parse_exception (Parse_error {
            position = !pos;
            context = "array section";
            reason = "expected ]]";
          }));
      advance ()
    );
    skip_to_eol ();
    (name, is_array)
  in
  (* Main parsing loop *)
  let sections = ref [] in
  let current_section = ref None in
  let current_items = ref [] in
  let array_sections = ref [] in
  let assoc_opt key items =
    let rec loop = fun __tmp1 ->
      match __tmp1 with
      | [] -> None
      | (name, value) :: rest ->
          if String.equal name key then
            Some value
          else
            loop rest
    in
    loop items
  in
  let assoc_remove key items =
    let rec loop acc = fun __tmp1 ->
      match __tmp1 with
      | [] -> List.reverse acc
      | (name, value) :: rest ->
          if String.equal name key then
            loop acc rest
          else
            loop ((name, value) :: acc) rest
    in
    loop [] items
  in
  (* Track [[name]] sections *)
  let save_current_section () =
    match !current_section with
    | Some (name, false) -> sections := { name; items = List.reverse !current_items } :: !sections
    | Some (name, true) ->
        let existing =
          match assoc_opt name !array_sections with
          | Some existing -> existing
          | None -> []
        in
        array_sections := (name, Table (List.reverse !current_items) :: existing)
        :: assoc_remove name !array_sections
    | None ->
        if List.length !current_items > 0 then
          sections := { name = ""; items = List.reverse !current_items } :: !sections
  in
  try
    let rec parse_loop () =
      skip_noise ();
      if not (at_end ()) then
        match current_char () with
        | '[' ->
            save_current_section ();
            let (section_name, is_array) = parse_section_header () in
            current_section := Some (section_name, is_array);
            current_items := [];
            parse_loop ()
        | _ ->
            let key = parse_key () in
            if at_end () || current_char () != '=' then (
              skip_to_eol ();
              parse_loop ()
            ) else (
              advance ();
              let value = parse_value () in
              current_items := (key, value) :: (assoc_remove key !current_items);
              skip_to_eol ();
              parse_loop ()
            )
    in
    parse_loop ();
    save_current_section ();
    let all_sections = List.reverse !sections in
    (* Helper to insert a dotted key path into nested tables *)
    let rec insert_nested_table path value acc =
      match path with
      | [] -> acc
      | [ key ] -> (key, value) :: acc
      | key :: rest ->
          (* Check if this key already exists in acc *)
          let existing_table =
            match assoc_opt key acc with
            | Some (Table items) -> items
            | _ -> []
          in
          let updated_table = insert_nested_table rest value existing_table in
          (* Replace or add the key with updated nested table *)
          let acc_without_key = List.filter acc ~fn:(fun (k, _) -> not (String.equal k key)) in
          (key, Table updated_table) :: acc_without_key
    in
    (* Convert sections to nested tables *)
    let items =
      List.fold_left
        all_sections
        ~init:[]
        ~fn:(fun acc section ->
          if section.name = "" then
            section.items @ acc
          else
            (* Split dotted section names (e.g., "profile.debug" -> ["profile"; "debug"]) *)
            let path = String.split ~by:"." section.name in
            insert_nested_table path (Table section.items) acc)
    in
    (* Add array sections as arrays *)
    let items_with_arrays =
      List.fold_left
        !array_sections
        ~init:items
        ~fn:(fun acc (name, tables) ->
          (* Split dotted array section names (e.g., "log.handler" -> ["log"; "handler"]) *)
          let path = String.split ~by:"." name in
          insert_nested_table path (Array (List.reverse tables)) acc)
    in
    Ok (Table (List.reverse items_with_arrays))
  with
  | Parse_exception err -> Error err
  | exn ->
      Error (Parse_error {
        position = !pos;
        context = "unknown";
        reason = Kernel.Exception.to_string exn;
      })

(* Recursive descent TOML parser *)

let get_string = fun __tmp1 ->
  match __tmp1 with
  | String s -> Some s
  | _ -> None

let get_int = fun __tmp1 ->
  match __tmp1 with
  | Int i -> Some i
  | _ -> None

let get_array = fun __tmp1 ->
  match __tmp1 with
  | Array items -> Some items
  | _ -> None

let get_table = fun __tmp1 ->
  match __tmp1 with
  | Table items -> Some items
  | _ -> None

let rec to_string = fun ?(indent = 0) value ->
  let ind = String.make ~len:(indent * 2) ~char:' ' in
  match value with
  | String s -> "\"" ^ s ^ "\""
  | Int i -> Int.to_string i
  | Bool b ->
      if b then
        "true"
      else
        "false"
  | Array items ->
      let items_str = String.concat ", " (List.map items ~fn:(to_string ~indent:(indent + 1))) in
      "[" ^ items_str ^ "]"
  | Table items ->
      let items_str =
        String.concat
          ",\n"
          (List.map
            items
            ~fn:(fun (k, v) -> ind ^ "  " ^ k ^ " = " ^ to_string ~indent:(indent + 1) v))
      in
      if indent = 0 then
        "{\n" ^ items_str ^ "\n}"
      else
        "{ " ^ items_str ^ " }"
