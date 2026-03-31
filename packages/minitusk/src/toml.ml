open Stdlib

type value =
  | String of string
  | Int of int
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of {
      path : string;
    }
  | File_read_error of {
      path : string;
      reason : string;
    }
  | Parse_error of {
      position : int;
      context : string;
      reason : string;
    }
  | Unterminated_string of {
      position : int;
    }
  | Unterminated_array of {
      position : int;
    }
  | Unexpected_char of {
      position : int;
      found : char;
      expected : string;
    }

let error_to_string =
  function
  | Invalid_path { path } -> "Invalid path: " ^ path
  | File_read_error { path; reason } -> "Failed to read file " ^ path ^ ": " ^ reason
  | Parse_error { position; context; reason } -> "Parse error at position "
  ^ string_of_int position
  ^ " (context: "
  ^ context
  ^ "): "
  ^ reason
  | Unterminated_string { position } -> "Unterminated string at position " ^ string_of_int position
  | Unterminated_array { position } -> "Unterminated array at position " ^ string_of_int position
  | Unexpected_char { position; found; expected } -> "Unexpected character '"
  ^ String.make 1 found
  ^ "' at position "
  ^ string_of_int position
  ^ " (expected "
  ^ expected
  ^ ")"

exception Parse_exception of error

type section = {
  name : string;
  items : (string * value) list;
}

let parse = fun content ->
  let len = String.length content in
  let pos = ref 0 in
  let at_end = fun () -> !pos >= len in
  let peek = fun () ->
    if at_end () then
      None
    else
      Some content.[!pos]
  in
  let advance = fun () ->
    if not (at_end ()) then
      incr pos
  in
  let current_char = fun () ->
    if at_end () then
      '\000'
    else
      content.[!pos]
  in
  (* Skip whitespace (spaces, tabs) but NOT newlines *)
  let rec skip_ws = fun () ->
    match peek () with
    | Some (' ' | '\t' | '\r') ->
        advance ();
        skip_ws ()
    | _ -> ()
  in
  (* Skip to end of line *)
  let skip_to_eol = fun () ->
    while (not (at_end ())) && current_char () != '\n' do
      advance ()
    done;
    if not (at_end ()) then
      advance ()
  in
  (* Skip whitespace, newlines, and comments *)
  let rec skip_noise = fun () ->
    skip_ws ();
    match peek () with
    | Some '#' ->
        skip_to_eol ();
        skip_noise ()
    | Some '\n' ->
        advance ();
        skip_noise ()
    | _ ->
        ()
  in
  (* Parse a quoted string *)
  let parse_quoted_string = fun () ->
    let start_pos = !pos in
    if current_char () != '"' then
      raise
      (Parse_exception (Unexpected_char {
        position = !pos;
        found = current_char ();
        expected = "double-quote";

      }));
    advance ();
    let buf = Buffer.create 16 in
    let rec loop = fun () ->
      if at_end () then
        raise (Parse_exception (Unterminated_string {position = start_pos}));
      match current_char () with
      | '"' ->
          advance ();
          Buffer.contents buf
      | '\\' ->
          advance ();
          if at_end () then
            raise (Parse_exception (Unterminated_string {position = start_pos}));
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
  let rec parse_array = fun () ->
    let start_pos = !pos in
    if current_char () != '[' then
      raise
      (Parse_exception (Unexpected_char {position = !pos; found = current_char (); expected = "["}));
    advance ();
    let items = ref [] in
    let rec parse_items = fun () ->
      skip_noise ();
      if at_end () then
        raise (Parse_exception (Unterminated_array {position = start_pos}));
      match current_char () with
      | ']' ->
          advance ();
          Array (List.rev !items)
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
              Array (List.rev !items)
          | None ->
              raise (Parse_exception (Unterminated_array {position = start_pos}))
          | Some c ->
              parse_items ()
        )
    in
    parse_items ()
  (* Parse an inline table { key = value, ... } *)
  and parse_inline_table = fun () ->
    let start_pos = !pos in
    if current_char () != '{' then
      raise
      (Parse_exception (Unexpected_char {position = !pos; found = current_char (); expected = "{"}));
    advance ();
    skip_ws ();
    let items = ref [] in
    let rec parse_items = fun () ->
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
          Table (List.rev !items)
      | _ -> (
          (* Parse key = value *)
          let key_start = !pos in
          while (not (at_end ())) && current_char () != '=' && current_char () != '}' do
            advance ()
          done;
          let key = String.trim (String.sub content key_start (!pos - key_start)) in
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
              Table (List.rev !items)
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
  and parse_value = fun () ->
    skip_noise ();
    if at_end () then
      raise
      (Parse_exception (Parse_error {position = !pos; context = "value"; reason = "unexpected end"}));
    match current_char () with
    | '"' ->
        parse_quoted_string ()
    | '[' ->
        parse_array ()
    | '{' ->
        parse_inline_table ()
    | 't' when !pos + 4 <= len && String.sub content !pos 4 = "true" ->
        pos := !pos + 4;
        Bool true
    | 'f' when !pos + 5 <= len && String.sub content !pos 5 = "false" ->
        pos := !pos + 5;
        Bool false
    | '0' .. '9'
    | '-'
    | '+' ->
        let start = !pos in
        if current_char () = '-' || current_char () = '+' then
          advance ();
        while not (at_end ()) && current_char () >= '0' && current_char () <= '9' do
          advance ()
        done;
        let str = String.trim (String.sub content start (!pos - start)) in
        (
          try Int (int_of_string str) with
          | Failure _ -> String str
        )
    | _ ->
        (* Bare string - read until comma, bracket, newline, or comment *)
        let start = !pos in
        while not (at_end ()) do
          match current_char () with
          | ','
          | ']'
          | '\n'
          | '#'
          | '}' -> raise Exit
          | _ -> advance ()
        done;
        let str = String.trim (String.sub content start (!pos - start)) in
        String str
  in
  (* Parse a key (identifier before =) *)
  let parse_key = fun () ->
    skip_ws ();
    let start = !pos in
    while (not (at_end ())) && current_char () != '=' do
      advance ()
    done;
    String.trim (String.sub content start (!pos - start))
  in
  (* Parse section header [name] or [[name]] *)
  let parse_section_header = fun () ->
    if current_char () != '[' then
      raise
      (Parse_exception (Unexpected_char {position = !pos; found = current_char (); expected = "["}));
    advance ();
    skip_ws ();
    let is_array = current_char () = '[' in
    if is_array then
      (
        advance ();
        skip_ws ()
      );
    let start = !pos in
    while (not (at_end ())) && current_char () != ']' do
      advance ()
    done;
    if at_end () then
      raise
      (Parse_exception (Parse_error {position = start; context = "section"; reason = "unterminated"}));
    let name = String.trim (String.sub content start (!pos - start)) in
    advance ();
    (* skip first ] *)
    (* If array of tables, expect another ] *)
    if is_array then
      (
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
  (* Track [[name]] sections *)
  try
    while not (at_end ()) do
      skip_noise ();
      if at_end () then
        raise Exit;
      match current_char () with
      | '[' ->
          (* Save previous section *)
          (
            match !current_section with
            | Some (name, false) ->
                sections := {name; items = List.rev !current_items} :: !sections
            | Some (name, true) ->
                (* Array section - add current items as a table to the array *)
                let existing =
                  try List.assoc name !array_sections with
                  | Not_found -> []
                in
                array_sections := (name, Table (List.rev !current_items) :: existing)
                :: List.remove_assoc name !array_sections
            | None ->
                (* Save top-level items with empty section name *)
                if List.length !current_items > 0 then
                  sections := {name = ""; items = List.rev !current_items} :: !sections
          );
          let section_name, is_array = parse_section_header () in
          current_section := Some (section_name, is_array);
          current_items := []
      | _ ->
          (* Parse key = value *)
          let key = parse_key () in
          if at_end () || current_char () != '=' then
            skip_to_eol ()
            (* Skip malformed lines *)
          else (
            advance ();
            (* skip = *)
            try
              let value = parse_value () in
              current_items := (key, value) :: List.remove_assoc key !current_items;
              skip_to_eol ()
            with
            | Exit ->
                (* Bare string parsing hit delimiter *)
                let value = String "" in
                current_items := (key, value) :: List.remove_assoc key !current_items;
                skip_to_eol ()
          )
    done;
    raise Exit
  with
  | Exit ->
      (* Normal termination *)
      (
        match !current_section with
        | Some (name, false) ->
            sections := {name; items = List.rev !current_items} :: !sections
        | Some (name, true) ->
            (* Array section - add current items as final table *)
            let existing =
              try List.assoc name !array_sections with
              | Not_found -> []
            in
            array_sections := (name, Table (List.rev !current_items) :: existing)
            :: List.remove_assoc name !array_sections
        | None ->
            (* Save top-level items with empty section name *)
            if List.length !current_items > 0 then
              sections := {name = ""; items = List.rev !current_items} :: !sections
      );
      let all_sections = List.rev !sections in
      (* Helper to insert a dotted key path into nested tables *)
      let rec insert_nested_table = fun path value acc ->
        match path with
        | [] ->
            acc
        | [ key ] ->
            (key, value) :: acc
        | key :: rest ->
            (* Check if this key already exists in acc *)
            let existing_table =
              match List.assoc_opt key acc with
              | Some (Table items) -> items
              | _ -> []
            in
            let updated_table = insert_nested_table rest value existing_table in
            (* Replace or add the key with updated nested table *)
            let acc_without_key =
              List.filter (fun ((k, _)) -> not (String.equal k key)) acc
            in
            (key, Table updated_table) :: acc_without_key
      in
      (* Convert sections to nested tables *)
      let items =
        List.fold_left
          (fun acc section ->
            if section.name = "" then
              section.items @ acc
            else
              (* Split dotted section names (e.g., "target.macos" -> ["target"; "macos"]) *)
              let path = String.split_on_char '.' section.name in
              insert_nested_table path (Table section.items) acc)
          []
          all_sections
      in
      (* Add array sections as arrays *)
      let items_with_arrays =
        List.fold_left
          (fun acc ((name, tables)) ->
            let path = String.split_on_char '.' name in
            insert_nested_table path (Array (List.rev tables)) acc)
          items
          !array_sections
      in
      Ok (Table (List.rev items_with_arrays))
  | Parse_exception err ->
      Error err
  | exn ->
      Error (Parse_error {position = !pos; context = "unknown"; reason = Printexc.to_string exn; })

(* Helper functions *)

let get_string =
  function
  | String s -> Some s
  | _ -> None

let get_int =
  function
  | Int i -> Some i
  | _ -> None

let get_array =
  function
  | Array items -> Some items
  | _ -> None

let get_table =
  function
  | Table items -> Some items
  | _ -> None

let find = fun key items ->
  try Some (List.assoc key items) with
  | Not_found -> None

let rec to_string = fun ?(indent = 0) value ->
  let ind = String.make (indent * 2) ' ' in
  match value with
  | String s ->
      "\"" ^ s ^ "\""
  | Int i ->
      string_of_int i
  | Bool b ->
      if b then
        "true"
      else
        "false"
  | Array items ->
      let items_str = String.concat ", " (List.map (to_string ~indent:((indent + 1))) items) in
      "[" ^ items_str ^ "]"
  | Table items ->
      let items_str =
        String.concat
        ",\n"
        (List.map (fun ((k, v)) -> ind ^ "  " ^ k ^ " = " ^ to_string ~indent:((indent + 1)) v) items)
      in
      if indent = 0 then
        "{\n" ^ items_str ^ "\n}"
      else
        "{ " ^ items_str ^ " }"
