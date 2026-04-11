open Std
module Array = Collections.Array

module AmbientName = struct
  type t = string

  let of_string = fun value -> value

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
  split_internal_module_name module_name |> Array.of_list |> local_module_alias_strings_of_segments

module InternalName = struct
  type t = {
    raw: string;
    segments: string array;
    direct_aliases: AmbientName.t list;
    direct_alias_set: string Collections.HashSet.t;
    relative_alias_sets: string Collections.HashSet.t option array;
  }

  let of_string = fun value ->
    let segments = split_internal_module_name value |> Array.of_list in
    let direct_aliases = local_module_alias_strings_of_segments segments in
    let relative_alias_sets =
      Array.init (Array.length segments)
        (fun prefix_length ->
          if prefix_length = 0 || prefix_length >= Array.length segments then
            None
          else
            let suffix_segments = Array.sub
              segments
              prefix_length
              (Array.length segments - prefix_length) in
            Some (suffix_segments
            |> relative_module_alias_strings_of_segments
            |> Collections.HashSet.of_list))
    in
    {
      raw = value;
      segments;
      direct_aliases = List.map (fun alias -> (alias: AmbientName.t)) direct_aliases;
      direct_alias_set = Collections.HashSet.of_list direct_aliases;
      relative_alias_sets;
    }

  let to_string = fun value -> value.raw

  let direct_aliases = fun value -> value.direct_aliases

  let direct_alias_set = fun value -> value.direct_alias_set

  let segments = fun value -> value.segments

  let relative_aliases_at_prefix = fun value ~prefix_length ->
    value.relative_alias_sets.(prefix_length)
end

module RequiredName = struct
  type t = {
    raw: string;
  }

  let of_string = fun value -> { raw = value }

  let of_ambient_name = fun value -> of_string (AmbientName.to_string value)

  let of_internal_name = fun value -> of_string (InternalName.to_string value)

  let to_string = fun value -> value.raw
end

let local_module_aliases_of_internal_name = fun module_name -> InternalName.direct_aliases module_name

let matches_required_name = fun ~required_name candidate_module_name ->
  let required_name = RequiredName.to_string required_name in
  String.equal (InternalName.to_string candidate_module_name) required_name
  || Collections.HashSet.contains (InternalName.direct_alias_set candidate_module_name) required_name

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
  let required_name = required_module_name in
  let required_module_name = RequiredName.to_string required_name in
  let best =
    if matches_required_name ~required_name candidate_module_name then
      Some 0
    else
      None
  in
  let max_prefix_length = common_prefix_length
    (InternalName.segments current_module_name)
    (InternalName.segments candidate_module_name)
  |> Int.min (Array.length (InternalName.segments candidate_module_name) - 1) in
  let rec loop best prefix_length =
    if prefix_length > max_prefix_length then
      best
    else
      match InternalName.relative_aliases_at_prefix candidate_module_name ~prefix_length with
      | Some relative_aliases when Collections.HashSet.contains relative_aliases required_module_name ->
          let best = Some (Option.unwrap_or ~default:prefix_length best |> Int.max prefix_length) in
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
  | [] -> [ InternalName.to_string module_name ]
  | aliases -> aliases
