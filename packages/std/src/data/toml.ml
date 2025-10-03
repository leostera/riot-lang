open Global

type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of { path : string }
  | File_read_error of { path : string; reason : string }
  | Parse_error of { position : int; context : string; reason : string }
  | Unterminated_string of { position : int }
  | Unterminated_array of { position : int }
  | Unexpected_char of { position : int; found : char; expected : string }

let error_to_string = function
  | Invalid_path { path } -> format "Invalid path: %s" path
  | File_read_error { path; reason } ->
      format "Failed to read file %s: %s" path reason
  | Parse_error { position; context; reason } ->
      format "Parse error at position %d (context: %s): %s" position context
        reason
  | Unterminated_string { position } ->
      format "Unterminated string at position %d" position
  | Unterminated_array { position } ->
      format "Unterminated array at position %d" position
  | Unexpected_char { position; found; expected } ->
      format "Unexpected character '%c' at position %d (expected %s)" found
        position expected

exception Parse_exception of error

type section = { name : string; items : (string * value) list }

(* Recursive descent TOML parser *)
let parse_file filename =
  Log.trace "[TOML] Parsing file: %s" filename;

  match Path.of_string filename with
  | Error _ -> Error (Invalid_path { path = filename })
  | Ok path -> (
      match Fs.read_to_string path with
      | Error (Fs.SystemError msg) ->
          Error (File_read_error { path = filename; reason = msg })
      | Ok content -> (
          let len = String.length content in
          let pos = ref 0 in

          let at_end () = !pos >= len in
          let peek () = if at_end () then None else Some content.[!pos] in
          let advance () = if not (at_end ()) then incr pos in
          let current_char () = if at_end () then '\000' else content.[!pos] in

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
            while (not (at_end ())) && current_char () <> '\n' do
              advance ()
            done;
            if not (at_end ()) then advance () (* skip \n *)
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
            if current_char () <> '"' then
              raise
                (Parse_exception
                   (Unexpected_char
                      {
                        position = !pos;
                        found = current_char ();
                        expected = "double-quote";
                      }));
            advance ();

            (* skip opening quote *)
            let buf = Buffer.create 16 in
            let rec loop () =
              if at_end () then
                raise
                  (Parse_exception
                     (Unterminated_string { position = start_pos }));
              match current_char () with
              | '"' ->
                  advance ();
                  Buffer.contents buf
              | '\\' ->
                  advance ();
                  if at_end () then
                    raise
                      (Parse_exception
                         (Unterminated_string { position = start_pos }));
                  (match current_char () with
                  | 'n' -> Buffer.add_char buf '\n'
                  | 't' -> Buffer.add_char buf '\t'
                  | 'r' -> Buffer.add_char buf '\r'
                  | '\\' -> Buffer.add_char buf '\\'
                  | '"' -> Buffer.add_char buf '"'
                  | c -> Buffer.add_char buf c);
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
            if current_char () <> '[' then
              raise
                (Parse_exception
                   (Unexpected_char
                      {
                        position = !pos;
                        found = current_char ();
                        expected = "[";
                      }));
            advance ();

            (* skip [ *)
            let items = ref [] in
            let rec parse_items () =
              skip_noise ();
              if at_end () then
                raise
                  (Parse_exception (Unterminated_array { position = start_pos }));
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
                      raise
                        (Parse_exception
                           (Unterminated_array { position = start_pos }))
                  | Some c -> parse_items ())
              (* Allow newlines between items *)
            in
            parse_items ()
          (* Parse a value *)
          and parse_value () =
            skip_noise ();
            if at_end () then
              raise
                (Parse_exception
                   (Parse_error
                      {
                        position = !pos;
                        context = "value";
                        reason = "unexpected end";
                      }));
            match current_char () with
            | '"' -> parse_quoted_string ()
            | '[' -> parse_array ()
            | 't' when !pos + 4 <= len && String.sub content !pos 4 = "true" ->
                pos := !pos + 4;
                Bool true
            | 'f' when !pos + 5 <= len && String.sub content !pos 5 = "false" ->
                pos := !pos + 5;
                Bool false
            | _ ->
                (* Bare string - read until comma, bracket, newline, or comment *)
                let start = !pos in
                while not (at_end ()) do
                  match current_char () with
                  | ',' | ']' | '\n' | '#' -> raise Exit
                  | _ -> advance ()
                done;
                let str =
                  String.trim (String.sub content start (!pos - start))
                in
                String str
          in

          (* Parse a key (identifier before =) *)
          let parse_key () =
            skip_ws ();
            let start = !pos in
            while (not (at_end ())) && current_char () <> '=' do
              advance ()
            done;
            String.trim (String.sub content start (!pos - start))
          in

          (* Parse section header [name] *)
          let parse_section_header () =
            if current_char () <> '[' then
              raise
                (Parse_exception
                   (Unexpected_char
                      {
                        position = !pos;
                        found = current_char ();
                        expected = "[";
                      }));
            advance ();
            (* skip [ *)
            skip_ws ();

            let start = !pos in
            while (not (at_end ())) && current_char () <> ']' do
              advance ()
            done;
            if at_end () then
              raise
                (Parse_exception
                   (Parse_error
                      {
                        position = start;
                        context = "section";
                        reason = "unterminated";
                      }));

            let name = String.trim (String.sub content start (!pos - start)) in
            advance ();
            (* skip ] *)
            skip_to_eol ();
            name
          in

          (* Main parsing loop *)
          let sections = ref [] in
          let current_section = ref None in
          let current_items = ref [] in

          try
            while not (at_end ()) do
              skip_noise ();
              if at_end () then raise Exit;

              match current_char () with
              | '[' ->
                  (* Save previous section *)
                  (match !current_section with
                  | Some name ->
                      Log.trace "[TOML] Saving section '%s' with %d items" name
                        (List.length !current_items);
                      sections :=
                        { name; items = List.rev !current_items } :: !sections
                  | None -> ());

                  let section_name = parse_section_header () in
                  Log.trace "[TOML] Found section: %s" section_name;
                  current_section := Some section_name;
                  current_items := []
              | _ ->
                  (* Parse key = value *)
                  let key = parse_key () in
                  if at_end () || current_char () <> '=' then
                    skip_to_eol () (* Skip malformed lines *)
                  else (
                    advance ();
                    (* skip = *)
                    Log.trace "[TOML] Parsing key: %s" key;
                    try
                      let value = parse_value () in
                      current_items := (key, value) :: !current_items;
                      skip_to_eol ()
                    with Exit ->
                      (* Bare string parsing hit delimiter *)
                      let value = String "" in
                      current_items := (key, value) :: !current_items;
                      skip_to_eol ())
            done;
            raise Exit
          with
          | Exit ->
              (* Normal termination *)
              (match !current_section with
              | Some name ->
                  Log.trace "[TOML] Saving final section '%s' with %d items"
                    name
                    (List.length !current_items);
                  sections :=
                    { name; items = List.rev !current_items } :: !sections
              | None -> ());

              let all_sections = List.rev !sections in
              Log.trace "[TOML] Successfully parsed %d sections"
                (List.length all_sections);

              (* Convert sections to nested tables *)
              let items =
                List.fold_left
                  (fun acc section ->
                    if section.name = "" then
                      (* Top-level items *)
                      section.items @ acc
                    else
                      (* Create nested table for section *)
                      (section.name, Table section.items) :: acc)
                  [] all_sections
              in
              Ok (Table (List.rev items))
          | Parse_exception err -> Error err
          | exn ->
              Error
                (Parse_error
                   {
                     position = !pos;
                     context = "unknown";
                     reason = Exception.to_string exn;
                   })))

let get_string = function String s -> Some s | _ -> None
let get_array = function Array items -> Some items | _ -> None
let get_table = function Table items -> Some items | _ -> None
