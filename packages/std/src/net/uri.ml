type error =
  | InvalidScheme
  | InvalidAuthority
  | InvalidPath
  | InvalidQuery
  | InvalidFragment
  | InvalidFormat
  | TooLong

type url_parts = {
  scheme : string option;
  authority : string option;
  path : string;
  query : string option;
  fragment : string option;
}

type t = url_parts
type url = t

(* Character validation *)
let is_scheme_char = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '+' | '-' | '.' -> true
  | _ -> false

let is_authority_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | ':' | '@' | '[' | ']' ->
      true
  | _ -> false

let is_path_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | '/' | ':' | '@' | '!' | '$' | '&' | '\'' | '(' | ')'
  | '*' | '+' | ',' | ';' | '=' ->
      true
  | _ -> false

let is_query_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-' | '.' | '_' | '~' | ':' | '/' | '?' | '#' | '[' | ']' | '@' | '!' | '$'
  | '&' | '\'' | '(' | ')' | '*' | '+' | ',' | ';' | '=' ->
      true
  | _ -> false

(* Parse helpers *)
let parse_scheme s start_pos =
  let len = String.length s in
  let rec find_end pos =
    if pos >= len then None
    else
      match s.[pos] with
      | ':' -> Some pos
      | c when is_scheme_char c -> find_end (pos + 1)
      | _ -> None
  in
  match find_end start_pos with
  | None -> (None, start_pos)
  | Some end_pos ->
      let scheme = String.sub s start_pos (end_pos - start_pos) in
      (Some scheme, end_pos + 1)

let parse_authority s start_pos =
  let len = String.length s in
  if start_pos + 1 < len && s.[start_pos] = '/' && s.[start_pos + 1] = '/' then
    let authority_start = start_pos + 2 in
    let rec find_end pos =
      if pos >= len then pos
      else
        match s.[pos] with
        | '/' | '?' | '#' -> pos
        | c when is_authority_char c -> find_end (pos + 1)
        | _ -> pos
    in
    let authority_end = find_end authority_start in
    let authority =
      String.sub s authority_start (authority_end - authority_start)
    in
    (Some authority, authority_end)
  else (None, start_pos)

let parse_path s start_pos =
  let len = String.length s in
  let rec find_end pos =
    if pos >= len then pos
    else
      match s.[pos] with
      | '?' | '#' -> pos
      | c when is_path_char c -> find_end (pos + 1)
      | _ -> pos
  in
  let path_end = find_end start_pos in
  let path =
    if path_end = start_pos then "/"
    else String.sub s start_pos (path_end - start_pos)
  in
  (path, path_end)

let parse_query s start_pos =
  let len = String.length s in
  if start_pos < len && s.[start_pos] = '?' then
    let query_start = start_pos + 1 in
    let rec find_end pos =
      if pos >= len then pos
      else
        match s.[pos] with
        | '#' -> pos
        | c when is_query_char c -> find_end (pos + 1)
        | _ -> pos
    in
    let query_end = find_end query_start in
    let query = String.sub s query_start (query_end - query_start) in
    (Some query, query_end)
  else (None, start_pos)

let parse_fragment s start_pos =
  let len = String.length s in
  if start_pos < len && s.[start_pos] = '#' then
    let fragment_start = start_pos + 1 in
    let fragment = String.sub s fragment_start (len - fragment_start) in
    (Some fragment, len)
  else (None, start_pos)

(* Main parsing function *)
let of_string s =
  if String.length s > 65535 then Error TooLong
  else
    try
      let pos = 0 in
      let scheme, pos = parse_scheme s pos in
      let authority, pos = parse_authority s pos in
      let path, pos = parse_path s pos in
      let query, pos = parse_query s pos in
      let fragment, _ = parse_fragment s pos in

      Ok { scheme; authority; path; query; fragment }
    with _ -> Error InvalidFormat

let to_string url =
  let parts = [] in
  let parts =
    match url.scheme with
    | None -> parts
    | Some scheme -> scheme :: ":" :: parts
  in
  let parts =
    match url.authority with
    | None -> parts
    | Some authority -> "//" :: authority :: parts
  in
  let parts = url.path :: parts in
  let parts =
    match url.query with None -> parts | Some query -> "?" :: query :: parts
  in
  let parts =
    match url.fragment with
    | None -> parts
    | Some fragment -> "#" :: fragment :: parts
  in
  String.concat "" (List.rev parts)

(* Component access *)
let scheme url = url.scheme
let authority url = url.authority
let path url = url.path
let query url = url.query
let fragment url = url.fragment

let host url =
  match url.authority with
  | None -> None
  | Some auth ->
      (* Extract host from authority (removing userinfo and port) *)
      let auth =
        match String.index_opt auth '@' with
        | None -> auth
        | Some idx -> String.sub auth (idx + 1) (String.length auth - idx - 1)
      in
      let host =
        match String.rindex_opt auth ':' with
        | None -> auth
        | Some idx -> String.sub auth 0 idx
      in
      Some host

let port url =
  match url.authority with
  | None -> None
  | Some auth -> (
      match String.rindex_opt auth ':' with
      | None -> None
      | Some idx -> (
          let port_str =
            String.sub auth (idx + 1) (String.length auth - idx - 1)
          in
          try Some (int_of_string port_str) with _ -> None))

let path_and_query url =
  match url.query with None -> url.path | Some q -> url.path ^ "?" ^ q

(* Component modules *)
module Scheme = struct
  type t = string

  let http = "http"
  let https = "https"
  let ftp = "ftp"
  let file = "file"

  let of_string s =
    if String.for_all is_scheme_char s && String.length s > 0 then Ok s
    else Error InvalidScheme

  let to_string s = s
end

module Authority = struct
  type t = string

  let of_string s =
    if String.for_all is_authority_char s then Ok s else Error InvalidAuthority

  let to_string s = s

  let host auth =
    let auth =
      match String.index_opt auth '@' with
      | None -> auth
      | Some idx -> String.sub auth (idx + 1) (String.length auth - idx - 1)
    in
    match String.rindex_opt auth ':' with
    | None -> auth
    | Some idx -> String.sub auth 0 idx

  let port auth =
    match String.rindex_opt auth ':' with
    | None -> None
    | Some idx -> (
        let port_str =
          String.sub auth (idx + 1) (String.length auth - idx - 1)
        in
        try Some (int_of_string port_str) with _ -> None)

  let userinfo auth =
    match String.index_opt auth '@' with
    | None -> None
    | Some idx -> Some (String.sub auth 0 idx)
end

module PathAndQuery = struct
  type t = { path : string; query : string option }

  let of_string s =
    match String.index_opt s '?' with
    | None -> Ok { path = s; query = None }
    | Some idx ->
        let path = String.sub s 0 idx in
        let query = String.sub s (idx + 1) (String.length s - idx - 1) in
        Ok { path; query = Some query }

  let to_string pq =
    match pq.query with None -> pq.path | Some q -> pq.path ^ "?" ^ q

  let path pq = pq.path
  let query pq = pq.query
end

(* Builder *)
module Builder = struct
  type t = {
    scheme : string option;
    authority : string option;
    host : string option;
    port : int option;
    path : string option;
    query : string option;
    fragment : string option;
  }

  let create () =
    {
      scheme = None;
      authority = None;
      host = None;
      port = None;
      path = None;
      query = None;
      fragment = None;
    }

  let scheme builder s = { builder with scheme = Some s }
  let authority builder s = { builder with authority = Some s }
  let host builder s = { builder with host = Some s }
  let port builder p = { builder with port = Some p }
  let path builder s = { builder with path = Some s }
  let query builder s = { builder with query = Some s }
  let fragment builder s = { builder with fragment = Some s }

  let build builder =
    let authority =
      match builder.authority with
      | Some auth -> Some auth
      | None -> (
          match (builder.host, builder.port) with
          | Some h, Some p -> Some (h ^ ":" ^ string_of_int p)
          | Some h, None -> Some h
          | None, _ -> None)
    in
    let path = match builder.path with Some p -> p | None -> "/" in
    Ok
      {
        scheme = builder.scheme;
        authority;
        path;
        query = builder.query;
        fragment = builder.fragment;
      }
end

(* Utilities *)
let is_absolute url = url.scheme <> None
let is_relative url = url.scheme = None

let join base relative_path =
  match of_string relative_path with
  | Error e -> Error e
  | Ok rel_url ->
      if is_absolute rel_url then Ok rel_url
      else
        let new_path =
          if String.get relative_path 0 = '/' then relative_path
          else
            let base_path = base.path in
            let base_dir =
              match String.rindex_opt base_path '/' with
              | None -> ""
              | Some idx -> String.sub base_path 0 (idx + 1)
            in
            base_dir ^ relative_path
        in
        Ok
          {
            base with
            path = new_path;
            query = rel_url.query;
            fragment = rel_url.fragment;
          }

let equal url1 url2 = String.equal (to_string url1) (to_string url2)
let compare url1 url2 = String.compare (to_string url1) (to_string url2)

(* Query utilities *)
module Query = struct
  type param = string * string
  type t = param list

  let parse query_string =
    if String.length query_string = 0 then []
    else
      let pairs = String.split_on_char '&' query_string in
      List.filter_map
        (fun pair ->
          match String.index_opt pair '=' with
          | None -> Some (pair, "")
          | Some idx ->
              let key = String.sub pair 0 idx in
              let value =
                String.sub pair (idx + 1) (String.length pair - idx - 1)
              in
              Some (key, value))
        pairs

  let to_string params =
    let param_strings =
      List.map
        (fun (k, v) -> if String.length v = 0 then k else k ^ "=" ^ v)
        params
    in
    String.concat "&" param_strings

  let get params key = try Some (List.assoc key params) with Not_found -> None

  let get_all params key =
    List.fold_left
      (fun acc (k, v) -> if String.equal k key then v :: acc else acc)
      [] params
    |> List.rev

  let add params key value = (key, value) :: params

  let remove params key =
    List.filter (fun (k, _) -> not (String.equal k key)) params
end
