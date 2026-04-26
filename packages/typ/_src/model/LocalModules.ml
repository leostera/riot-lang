open Std

module Array = Collections.Array

type ambient_name = int

type required_name = int

type internal_name = int

type 'entry interner = {
  by_raw: (string, int) Collections.HashMap.t;
  mutable entries: 'entry option array;
  mutable next_id: int;
}

let create_interner = fun capacity -> {
  by_raw = Collections.HashMap.with_capacity capacity;
  entries = Array.make (Int.max 8 capacity) None;
  next_id = 0;
}

let ensure_capacity = fun table id ->
  if id < Array.length table.entries then
    ()
  else
    let new_length = ref (Int.max 8 (Array.length table.entries)) in
    while id >= !new_length do
      new_length := !new_length * 2
    done;
  let grown = Array.make !new_length None in
  Array.blit table.entries 0 grown 0 (Array.length table.entries);
  table.entries <- grown

let intern_entry = fun table raw make ->
  match Collections.HashMap.get table.by_raw raw with
  | Some id -> id
  | None ->
      let id = table.next_id in
      table.next_id <- id + 1;
      ensure_capacity table id;
      table.entries.(id) <- Some (make ());
      let _ = Collections.HashMap.insert table.by_raw raw id in
      id

let lookup_entry = fun table kind id ->
  match table.entries.(id) with
  | Some entry -> entry
  | None -> panic (kind ^ ": unknown interned id " ^ Int.to_string id)

type ambient_entry = { raw: string }

type required_entry = { raw: string }

type internal_entry = {
  raw: string;
  segments: string array;
  self_ambient_name: ambient_name;
  required_name: required_name;
  direct_aliases: ambient_name list;
  direct_alias_set: required_name Collections.HashSet.t;
  relative_alias_sets: required_name Collections.HashSet.t option array;
}

let ambient_names = create_interner 128

let required_names = create_interner 128

let internal_names = create_interner 128

let ambient_entry = fun value -> lookup_entry ambient_names "AmbientName" value

let required_entry = fun value -> lookup_entry required_names "RequiredName" value

let internal_entry = fun value -> lookup_entry internal_names "InternalName" value

let intern_ambient_name = fun value -> intern_entry ambient_names value (fun () -> { raw = value })

let ambient_name_to_string = fun value -> (ambient_entry value).raw

let intern_required_name = fun value ->
  intern_entry required_names value (fun () -> { raw = value })

let required_name_to_string = fun value -> (required_entry value).raw

let split_internal_module_name = fun module_name ->
  let rec find_separator index =
    if index + 1 >= String.length module_name then
      None
    else if module_name.[index] = '_' && module_name.[index + 1] = '_' then
      Some index
    else
      find_separator (index + 1)
  in
  let rec loop start acc =
    if start >= String.length module_name then
      List.rev acc
    else
      match find_separator start with
      | Some index ->
          let segment = String.sub module_name start (index - start) in
          loop (index + 2) (segment :: acc)
      | None ->
          let segment = String.sub module_name start (String.length module_name - start) in
          List.rev (segment :: acc)
  in
  loop 0 []

let dedupe_preserving_order = fun names ->
  let seen = Collections.HashSet.with_capacity (List.length names + 1) in
  names
  |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let module_name_suffix_aliases = fun module_name ->
  let segments =
    module_name
    |> String.split_on_char '.'
    |> List.filter (fun segment -> not (String.equal segment ""))
  in
  let rec loop aliases = function
    | [] -> List.rev aliases
    | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
  in
  loop [] segments
  |> dedupe_preserving_order

let class_case_segment = fun segment ->
  segment
  |> String.split_on_char '_'
  |> List.filter (fun piece -> not (String.equal piece ""))
  |> List.map String.capitalize_ascii
  |> String.concat ""

let class_case_module_name = fun module_name ->
  module_name
  |> String.split_on_char '.'
  |> List.filter (fun segment -> not (String.equal segment ""))
  |> List.map class_case_segment
  |> String.concat "."

let local_module_alias_strings_of_local_segments = fun local_segments ->
  match local_segments with
  | [] -> []
  | _ ->
      let dotted_name = String.concat "." local_segments in
      dedupe_preserving_order
        (module_name_suffix_aliases (class_case_module_name dotted_name)
        @ module_name_suffix_aliases dotted_name)

let relative_module_alias_strings_of_segments = fun segments ->
  match Array.to_list segments with
  | [] -> []
  | local_segments -> local_module_alias_strings_of_local_segments local_segments

let local_module_alias_strings_of_segments = fun segments ->
  match Array.to_list segments with
  | [] -> []
  | [ _root ] -> []
  | _root :: local_segments -> local_module_alias_strings_of_local_segments local_segments

let local_module_alias_strings_of_internal_module_name = fun module_name ->
  split_internal_module_name module_name
  |> Array.of_list
  |> local_module_alias_strings_of_segments

module AmbientName = struct
  type t = ambient_name

  let of_string = fun value -> intern_ambient_name value

  let to_string = fun value -> ambient_name_to_string value
end

module InternalName = struct
  type t = internal_name

  let of_string = fun value ->
    intern_entry
      internal_names
      value
      (fun () ->
        let segments =
          split_internal_module_name value
          |> Array.of_list
        in
        let direct_alias_strings = local_module_alias_strings_of_segments segments in
        let direct_aliases = List.map intern_ambient_name direct_alias_strings in
        let direct_alias_set =
          direct_alias_strings
          |> List.map intern_required_name
          |> Collections.HashSet.of_list
        in
        let relative_alias_sets =
          Array.init
            (Array.length segments)
            (fun prefix_length ->
              if prefix_length = 0 || prefix_length >= Array.length segments then
                None
              else
                let suffix_segments =
                  Array.sub segments prefix_length (Array.length segments - prefix_length)
                in
                Some (
                  suffix_segments
                  |> relative_module_alias_strings_of_segments
                  |> List.map intern_required_name
                  |> Collections.HashSet.of_list
                ))
        in
        {
          raw = value;
          segments;
          self_ambient_name = intern_ambient_name value;
          required_name = intern_required_name value;
          direct_aliases;
          direct_alias_set;
          relative_alias_sets;
        })

  let to_string = fun value -> (internal_entry value).raw
end

module RequiredName = struct
  type t = required_name

  let of_string = fun value -> intern_required_name value

  let of_ambient_name = fun value -> of_string (ambient_name_to_string value)

  let of_internal_name = fun value -> (internal_entry value).required_name

  let to_string = fun value -> required_name_to_string value
end

let local_module_aliases_of_internal_name = fun module_name ->
  (internal_entry module_name).direct_aliases

let ambient_name_of_internal_name = fun module_name ->
  (internal_entry module_name).self_ambient_name

let should_include_implicit_open = fun ~current_module_name ~module_name ->
  let current_segments = (internal_entry current_module_name).segments in
  if
    Array.length current_segments <= 1 || not (String.ends_with ~suffix:"__Aliases" module_name)
  then
    true
  else
    let alias_segments =
      module_name
      |> split_internal_module_name
      |> Array.of_list
    in
    match Array.to_list alias_segments
    |> List.rev with
    | "Aliases" :: reversed_prefix ->
        let prefix = List.rev reversed_prefix in
        not
          (not (List.is_empty prefix)
          && List.length prefix <= Array.length current_segments
          && List.for_all2
            String.equal
            prefix
            (Array.to_list (Array.sub current_segments 0 (List.length prefix))))
    | _ -> true

let matches_required_name = fun ~required_name candidate_module_name ->
  let candidate = internal_entry candidate_module_name in
  Int.equal candidate.required_name required_name
  || Collections.HashSet.contains candidate.direct_alias_set required_name

let common_prefix_length = fun left right ->
  let max_length = Int.min (Array.length left) (Array.length right) in
  let rec loop index =
    if index >= max_length then
      index
    else if String.equal left.(index) right.(index) then
      loop (index + 1)
    else
      index
  in
  loop 0

let contextual_match_depth = fun ~current_module_name ~required_module_name ~candidate_module_name ->
  let current_segments = (internal_entry current_module_name).segments in
  let candidate = internal_entry candidate_module_name in
  let best =
    if matches_required_name ~required_name:required_module_name candidate_module_name then
      Some 0
    else
      None
  in
  let max_prefix_length =
    common_prefix_length current_segments candidate.segments
    |> Int.min (Array.length candidate.segments - 1)
  in
  let rec loop best prefix_length =
    if prefix_length > max_prefix_length then
      best
    else
      match candidate.relative_alias_sets.(prefix_length) with
      | Some relative_aliases when Collections.HashSet.contains
        relative_aliases
        required_module_name ->
          let best = Some (
            Option.unwrap_or ~default:prefix_length best
            |> Int.max prefix_length
          )
          in
          loop best (prefix_length + 1)
      | Some _
      | None -> loop best (prefix_length + 1)
  in
  loop best 1

let preferred_local_module_alias = fun module_name ->
  match local_module_alias_strings_of_internal_module_name module_name with
  | alias :: _ -> Some alias
  | [] -> None

let ambient_names_of_internal_name = fun module_name ->
  match local_module_aliases_of_internal_name module_name with
  | [] -> [ ambient_name_of_internal_name module_name ]
  | aliases -> aliases
