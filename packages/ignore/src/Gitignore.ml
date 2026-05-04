open Std

module List = Collections.List

type syntax =
  | Ignore_file
  | Override

type parse_error = {
  line: int;
  input: string;
  message: string;
  offset: int option;
}

type file_error =
  | File_system of Fs.error
  | Invalid_glob of parse_error

type rule = {
  matcher: Glob.t;
  action: Match.t;
  only_dir: bool;
}

type t = {
  root: Path.t;
  rules: rule list;
  unmatched_is_ignore_on_files: bool;
}

let empty = fun ~root -> { root; rules = []; unmatched_is_ignore_on_files = false }

let drop_prefix = fun value prefix ->
  let prefix_len = String.length prefix in
  if String.starts_with ~prefix value then
    String.sub value ~offset:prefix_len ~len:(String.length value - prefix_len)
  else
    value

let drop_suffix = fun value suffix ->
  let value_len = String.length value in
  let suffix_len = String.length suffix in
  if String.ends_with ~suffix value then
    String.sub value ~offset:0 ~len:(value_len - suffix_len)
  else
    value

let strip_leading_dot_slash = fun value -> drop_prefix value "./"

let strip_trailing_carriage_return = fun value ->
  if String.ends_with ~suffix:"\r" value then
    String.sub value ~offset:0 ~len:(String.length value - 1)
  else
    value

let action_for_pattern = fun syntax ~negated ->
  match (syntax, negated) with
  | (Ignore_file, false) -> Match.Ignore
  | (Ignore_file, true) -> Match.Whitelist
  | (Override, false) -> Match.Whitelist
  | (Override, true) -> Match.Ignore

let normalize_pattern = fun body ->
  let anchored = String.starts_with ~prefix:"/" body in
  let pattern =
    if anchored then
      drop_prefix body "/"
    else
      body
  in
  let contains_slash = String.contains pattern "/" in
  if anchored || contains_slash then
    pattern
  else
    "**/" ^ pattern

let parse_line = fun ~syntax ~line_number input ->
  let input = strip_trailing_carriage_return input in
  if String.equal input "" then
    Ok None
  else if String.starts_with ~prefix:"#" input then
    Ok None
  else
    let negated = String.starts_with ~prefix:"!" input in
    let body =
      if String.starts_with ~prefix:"\\#" input || String.starts_with ~prefix:"\\!" input then
        String.sub input ~offset:1 ~len:(String.length input - 1)
      else if negated then
        String.sub input ~offset:1 ~len:(String.length input - 1)
      else
        input
    in
    let only_dir = String.ends_with ~suffix:"/" body in
    let body =
      if only_dir then
        drop_suffix body "/"
      else
        body
    in
    if String.equal body "" then
      Ok None
    else
      let pattern = normalize_pattern body in
      match Glob.create [ pattern ] with
      | Ok matcher -> Ok (Some { matcher; action = action_for_pattern syntax ~negated; only_dir })
      | Error (Glob.Invalid_glob { message; offset; _ }) ->
          Error {
            line = line_number;
            input;
            message;
            offset;
          }
      | Error (Glob.Invalid_regex { message; offset }) ->
          Error {
            line = line_number;
            input;
            message;
            offset;
          }
      | Error Glob.Empty ->
          Error {
            line = line_number;
            input;
            message = "empty glob";
            offset = None;
          }

let from_lines = fun ~root ~syntax lines ->
  let rec loop line_number acc = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        let rules = List.reverse acc in
        let unmatched_is_ignore_on_files =
          match syntax with
          | Ignore_file -> false
          | Override -> List.any rules ~fn:(fun rule -> Match.is_whitelist rule.action)
        in
        Ok { root; rules; unmatched_is_ignore_on_files }
    | line :: rest -> (
        match parse_line ~syntax ~line_number line with
        | Ok None -> loop (line_number + 1) acc rest
        | Ok (Some rule) -> loop (line_number + 1) (rule :: acc) rest
        | Error _ as err -> err
      )
  in
  loop 1 [] lines

let from_string = fun ~root ~syntax text -> from_lines
  ~root
  ~syntax
  (String.split ~by:"\n" text)

let from_file = fun ~syntax path ->
  match Fs.exists path with
  | Error err -> Error (File_system err)
  | Ok false -> Ok None
  | Ok true -> (
      match Fs.read path with
      | Ok text ->
          from_string ~root:(Path.dirname path) ~syntax text
          |> fun result ->
            Result.map result ~fn:Option.some
            |> fun result -> Result.map_err result ~fn:(fun err -> Invalid_glob err)
      | Error err -> Error (File_system err)
    )

let relative_path_string = fun matcher path ->
  let relative =
    Path.strip_prefix path ~prefix:matcher.root
    |> Result.unwrap_or ~default:path
  in
  Path.to_string relative
  |> strip_leading_dot_slash

let matches_rule = fun rule ~candidate ~is_dir ->
  if rule.only_dir && not is_dir then
    false
  else
    Glob.matches rule.matcher ~str:candidate
    |> Result.unwrap_or ~default:false

let matched = fun matcher ~path ~is_dir ->
  let candidate = relative_path_string matcher path in
  let rec loop = fun __tmp1 ->
    match __tmp1 with
    | [] ->
        if matcher.unmatched_is_ignore_on_files && not is_dir then
          Match.Ignore
        else
          Match.None_
    | rule :: rest ->
        if matches_rule rule ~candidate ~is_dir then
          rule.action
        else
          loop rest
  in
  loop (List.reverse matcher.rules)
