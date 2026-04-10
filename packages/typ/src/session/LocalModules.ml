open Std

module InternalName = struct
  type t = string

  let of_string = fun value -> value

  let to_string = fun value -> value
end

module RequiredName = struct
  type t = string

  let of_string = fun value -> value

  let to_string = fun value -> value
end

module AmbientName = struct
  type t = string

  let to_string = fun value -> value
end

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
  names |> List.filter
    (fun name ->
      if Collections.HashSet.contains seen name then
        false
      else
        let _ = Collections.HashSet.insert seen name in
        true)

let module_name_suffix_aliases = fun module_name ->
  let segments = module_name
  |> String.split_on_char '.'
  |> List.filter (fun segment -> not (String.equal segment "")) in
  let rec loop aliases = function
    | [] -> List.rev aliases
    | _ :: rest as current -> loop (String.concat "." current :: aliases) rest
  in
  loop [] segments |> dedupe_preserving_order

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

let local_module_alias_strings_of_internal_module_name = fun module_name ->
  match split_internal_module_name module_name with
  | [] ->
      []
  | [ _root ] ->
      []
  | _root :: local_segments ->
      let dotted_name = String.concat "." local_segments in
      dedupe_preserving_order
        (module_name_suffix_aliases (class_case_module_name dotted_name)
        @ module_name_suffix_aliases dotted_name)

let local_module_aliases_of_internal_name = fun module_name ->
  module_name
  |> InternalName.to_string
  |> local_module_alias_strings_of_internal_module_name
  |> List.map (fun alias -> (alias: AmbientName.t))

let segment_prefixes = fun segments ->
  let rec loop prefix acc = function
    | [] -> List.rev acc
    | segment :: rest ->
        let prefix = prefix @ [ segment ] in
        loop prefix (prefix :: acc) rest
  in
  loop [] [] segments

let strip_segment_prefix = fun ~prefix segments ->
  let rec loop prefix segments =
    match (prefix, segments) with
    | ([], rest) -> Some rest
    | (prefix_head :: prefix_tail, segment_head :: segment_tail) when String.equal prefix_head segment_head -> loop
      prefix_tail
      segment_tail
    | _ -> None
  in
  loop prefix segments

let matches_required_name = fun ~required_name candidate_module_name ->
  let required_name = RequiredName.to_string required_name in
  let candidate_module_name = InternalName.to_string candidate_module_name in
  String.equal candidate_module_name required_name
  || List.mem
    required_name
    (local_module_alias_strings_of_internal_module_name candidate_module_name)

let contextual_match_depth = fun ~current_module_name ~required_module_name ~candidate_module_name ->
  let current_module_name = InternalName.to_string current_module_name in
  let required_module_name = RequiredName.to_string required_module_name in
  let candidate_module_name = InternalName.to_string candidate_module_name in
  let best =
    if
      matches_required_name
        ~required_name:(RequiredName.of_string required_module_name)
        (InternalName.of_string candidate_module_name)
    then
      Some 0
    else
      None
  in
  let current_segments = split_internal_module_name current_module_name in
  let candidate_segments = split_internal_module_name candidate_module_name in
  segment_prefixes current_segments |> List.fold_left
    (fun best prefix ->
      match strip_segment_prefix ~prefix candidate_segments with
      | Some relative_segments when not (List.is_empty relative_segments) ->
          let relative_dotted_name = String.concat "." relative_segments in
          let relative_aliases = dedupe_preserving_order
            (module_name_suffix_aliases (class_case_module_name relative_dotted_name)
            @ module_name_suffix_aliases relative_dotted_name) in
          if List.mem required_module_name relative_aliases then
            Some (Option.unwrap_or ~default:0 best |> Int.max (List.length prefix))
          else
            best
      | _ -> best)
    best

let preferred_local_module_alias = fun module_name ->
  match local_module_alias_strings_of_internal_module_name module_name with
  | alias :: _ -> Some alias
  | [] -> None

let ambient_names_of_internal_name = fun module_name ->
  match local_module_aliases_of_internal_name module_name with
  | [] -> [ InternalName.to_string module_name ]
  | aliases -> aliases
