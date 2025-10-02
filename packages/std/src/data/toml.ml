type value =
  | String of string
  | Array of value list
  | Table of (string * value) list
  | Bool of bool

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
  try
    let path = match Path.of_string filename with
      | Ok p -> p
      | Error _ -> raise (Failure "Invalid path")
    in
    let content = match Fs.read_to_string path with
      | Ok s -> s
      | Error _ -> raise (Failure "Failed to read file")
    in
    let lines = String.split_on_char '\n' content in
    let sections = ref [] in
    let current_section = ref None in
    let current_items = ref [] in

    List.iter
      (fun line ->
        match parse_section_header line with
        | Some section_name ->
            (* Save previous section if any *)
            (match !current_section with
            | Some name ->
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
                current_items := (key, value) :: !current_items
            | None -> ()))
      lines;

    (* Save last section *)
    (match !current_section with
    | Some name ->
        sections := { name; items = List.rev !current_items } :: !sections
    | None -> ());

    let all_sections = List.rev !sections in
    (* Convert to table format - flatten sections into dotted keys *)
    let items =
      List.fold_left
        (fun acc section ->
          List.fold_left
            (fun acc (key, value) ->
              let full_key =
                if section.name = "" then key else section.name ^ "." ^ key
              in
              (full_key, value) :: acc)
            acc section.items)
        [] all_sections
    in
    Table (List.rev items)
  with _ -> Table []

let rec find_value key = function
  | Table items -> ( try Some (List.assoc key items) with Not_found -> None)
  | _ -> None

let get_string = function String s -> Some s | _ -> None
let get_array = function Array items -> Some items | _ -> None
let get_table = function Table items -> Some items | _ -> None
