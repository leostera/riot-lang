type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

type error =
  | Invalid_path of { path : string }
  | File_read_error of { path : string; reason : string }
  | Parse_error of { line_number : int; line : string; reason : string }
  | Empty_file of { path : string }

let error_to_string = function
  | Invalid_path { path } -> format "Invalid path: %s" path
  | File_read_error { path; reason } -> format "Failed to read file %s: %s" path reason
  | Parse_error { line_number; line; reason } -> 
      format "Parse error at line %d (%s): %s" line_number line reason
  | Empty_file { path } -> format "Empty TOML file: %s" path

type section = { name : string; items : (string * value) list }

let rec parse_line line =
  let line = String.trim line in
  if line = "" || (String.length line > 0 && String.get line 0 = '#') then None
  else if String.contains line '=' then
    let idx = String.index line '=' in
    let key = String.trim (String.sub line 0 idx) in
    let value_str =
      String.trim (String.sub line (idx + 1) (String.length line - idx - 1))
    in
    let value = parse_value value_str in
    Some (key, value)
  else None

and parse_value value_str =
  let value_str = String.trim value_str in
  if
    String.length value_str >= 2
    && String.get value_str 0 = '"'
    && String.get value_str (String.length value_str - 1) = '"'
  then String (String.sub value_str 1 (String.length value_str - 2))
  else if
    String.length value_str >= 2
    && String.get value_str 0 = '['
    && String.get value_str (String.length value_str - 1) = ']'
  then
    let content = String.sub value_str 1 (String.length value_str - 2) in
    let items =
      String.split_on_char ',' content
      |> List.map (fun s -> parse_value (String.trim s))
      |> List.filter (function String "" -> false | _ -> true)
    in
    Array items
  else if value_str = "true" then Bool true
  else if value_str = "false" then Bool false
  else String value_str

let parse_section_header line =
  let line = String.trim line in
  if
    String.length line >= 2
    && String.get line 0 = '['
    && String.get line (String.length line - 1) = ']'
  then Some (String.sub line 1 (String.length line - 2))
  else None

let parse_file filename =
  Log.debug "[TOML] Parsing file: %s" filename;
  
  let path = match Path.of_string filename with
    | Ok p -> p
    | Error _ -> Error (Invalid_path { path = filename })
  in
  
  match path with
  | Error e -> Error e
  | Ok path ->
      let content = match Fs.read_to_string path with
        | Ok s -> s
        | Error (Fs.SystemError msg) -> 
            return (Error (File_read_error { path = filename; reason = msg }))
      in
      
      match content with
      | Error e -> Error e
      | Ok content ->
          if String.trim content = "" then
            Error (Empty_file { path = filename })
          else
            let lines = String.split_on_char '\n' content in
            let sections = ref [] in
            let current_section = ref None in
            let current_items = ref [] in
            let line_number = ref 0 in

            try
              List.iter
                (fun line ->
                  incr line_number;
                  match parse_section_header line with
                  | Some section_name ->
                      Log.debug "[TOML] Line %d: Found section [%s]" !line_number section_name;
                      (* Save previous section if any *)
                      (match !current_section with
                      | Some name ->
                          Log.debug "[TOML]   Saving previous section '%s' with %d items" name (List.length !current_items);
                          sections :=
                            { name; items = List.rev !current_items } :: !sections
                      | None -> ());
                      (* Start new section *)
                      current_section := Some section_name;
                      current_items := []
                  | None -> (
                      (* Parse regular key-value line *)
                      match parse_line line with
                      | Some (key, value) ->
                          Log.debug "[TOML] Line %d: Found key-value: %s" !line_number key;
                          current_items := (key, value) :: !current_items
                      | None -> 
                          if String.trim line <> "" && String.get (String.trim line) 0 <> '#' then
                            Log.debug "[TOML] Line %d: Skipped: %s" !line_number line))
                lines;

              (* Save last section *)
              (match !current_section with
              | Some name ->
                  Log.debug "[TOML] Saving final section '%s' with %d items" name (List.length !current_items);
                  sections := { name; items = List.rev !current_items } :: !sections
              | None -> 
                  Log.debug "[TOML] No current section at end of file");

              let all_sections = List.rev !sections in
              
              (* Debug: log sections found *)
              Log.debug "[TOML] Parsed %d sections total" (List.length all_sections);
              List.iter (fun section ->
                Log.debug "[TOML]   Section '%s' with %d items" section.name (List.length section.items);
                List.iter (fun (key, _) -> Log.debug "[TOML]     - %s" key) section.items
              ) all_sections;
              
              (* Convert to table format - flatten sections into dotted keys *)
              let items =
                List.fold_left
                  (fun acc section ->
                    List.fold_left
                      (fun acc (key, value) ->
                        let full_key =
                          if section.name = "" then key else section.name ^ "." ^ key
                        in
                        Log.debug "[TOML] Creating flattened key: %s" full_key;
                        (full_key, value) :: acc)
                      acc section.items)
                  [] all_sections
              in
              Log.debug "[TOML] Final table has %d flattened items" (List.length items);
              Ok (Table (List.rev items))
            with exn ->
              Error (Parse_error { 
                line_number = !line_number; 
                line = if !line_number > 0 && !line_number <= List.length lines 
                       then List.nth lines (!line_number - 1) 
                       else ""; 
                reason = Exception.to_string exn 
              })

let rec find_value key = function
  | Table items -> ( try Some (List.assoc key items) with Not_found -> None)
  | _ -> None

let get_string = function String s -> Some s | _ -> None
let get_array = function Array items -> Some items | _ -> None
let get_table = function Table items -> Some items | _ -> None
