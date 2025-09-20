let parse_file filename =
  let ic = open_in filename in
  let rec parse_lines acc =
    try
      let line = input_line ic in
      let trimmed = String.trim line in
      if String.length trimmed > 0 && trimmed.[0] <> '#' then
        parse_lines (trimmed :: acc)
      else parse_lines acc
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  parse_lines []

let get_string_value lines key =
  List.find_map
    (fun line ->
      if String.starts_with ~prefix:(key ^ " = ") line then
        let value =
          String.sub line
            (String.length key + 3)
            (String.length line - String.length key - 3)
        in
        let value = String.trim value in
        if
          String.length value >= 2
          && value.[0] = '"'
          && value.[String.length value - 1] = '"'
        then Some (String.sub value 1 (String.length value - 2))
        else Some value
      else None)
    lines

let get_array_value lines key =
  let rec find_array = function
    | [] -> []
    | line :: rest ->
        if String.starts_with ~prefix:(key ^ " = [") line then
          (* Handle inline array like: members = ["a", "b"] *)
          if String.contains line ']' then
            let start = String.index line '[' + 1 in
            let end_ = String.index line ']' in
            let content = String.sub line start (end_ - start) in
            parse_inline_array content
          else collect_array rest []
        else find_array rest
  and collect_array lines acc =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
        let trimmed = String.trim line in
        if trimmed = "]" then List.rev acc
        else if String.ends_with ~suffix:"]" trimmed then
          let item = String.sub trimmed 0 (String.length trimmed - 1) in
          List.rev (parse_array_item item :: acc)
        else collect_array rest (parse_array_item trimmed :: acc)
  and parse_array_item item =
    let item = String.trim item in
    let item =
      if String.ends_with ~suffix:"," item then
        String.sub item 0 (String.length item - 1)
      else item
    in
    let item = String.trim item in
    if
      String.length item >= 2
      && item.[0] = '"'
      && item.[String.length item - 1] = '"'
    then String.sub item 1 (String.length item - 2)
    else item
  and parse_inline_array content =
    let items = String.split_on_char ',' content in
    List.map parse_array_item items
  in
  find_array lines
