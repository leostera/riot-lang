open Global
open IO
open Collections

type error =
  | InvalidScheme
  | InvalidAuthority
  | InvalidPath
  | InvalidQuery
  | InvalidFragment
  | InvalidFormat
  | TooLong

type url_parts = {
  scheme: string option;
  authority: string option;
  path: string;
  query: string option;
  fragment: string option;
}

type t = url_parts

type url = t

(* Character validation *)

let is_scheme_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '+'
  | '-'
  | '.' -> true
  | _ -> false

let is_authority_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-'
  | '.'
  | '_'
  | '~'
  | ':'
  | '@'
  | '['
  | ']' -> true
  | _ -> false

let is_path_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-'
  | '.'
  | '_'
  | '~'
  | '/'
  | ':'
  | '@'
  | '!'
  | '$'
  | '&'
  | '\''
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | ';'
  | '='
  | '%' -> true
  | _ -> false

let is_query_char = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-'
  | '.'
  | '_'
  | '~'
  | ':'
  | '/'
  | '?'
  | '#'
  | '['
  | ']'
  | '@'
  | '!'
  | '$'
  | '&'
  | '\''
  | '('
  | ')'
  | '*'
  | '+'
  | ','
  | ';'
  | '='
  | '%' -> true
  | _ -> false

(* Parse helpers *)

let parse_scheme = fun s start_pos ->
  let len = String.length s in
  let rec find_end pos =
    if pos >= len then
      None
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

let parse_authority = fun s start_pos ->
  let len = String.length s in
  if start_pos + 1 < len then
    if s.[start_pos] = '/' && s.[start_pos + 1] = '/' then
      let authority_start = start_pos + 2 in
      let rec find_end pos =
        if pos >= len then
          pos
        else
          match s.[pos] with
          | '/'
          | '?'
          | '#' -> pos
          | c when is_authority_char c -> find_end (pos + 1)
          | _ -> pos
      in
      let authority_end = find_end authority_start in
      let authority = String.sub s authority_start (authority_end - authority_start) in
      (Some authority, authority_end)
    else
      (None, start_pos)
  else
    (None, start_pos)

let parse_path = fun s start_pos ->
  let len = String.length s in
  let rec find_end pos =
    if pos >= len then
      pos
    else
      match s.[pos] with
      | '?'
      | '#' -> pos
      | c when is_path_char c -> find_end (pos + 1)
      | _ -> pos
  in
  let path_end = find_end start_pos in
  let path =
    if path_end = start_pos then
      "/"
    else
      String.sub s start_pos (path_end - start_pos)
  in
  (path, path_end)

let parse_query = fun s start_pos ->
  let len = String.length s in
  if start_pos < len then
    if s.[start_pos] = '?' then
      let query_start = start_pos + 1 in
      let rec find_end pos =
        if pos >= len then
          pos
        else
          match s.[pos] with
          | '#' -> pos
          | c when is_query_char c -> find_end (pos + 1)
          | _ -> pos
      in
      let query_end = find_end query_start in
      let query = String.sub s query_start (query_end - query_start) in
      (Some query, query_end)
    else
      (None, start_pos)
  else
    (None, start_pos)

let parse_fragment = fun s start_pos ->
  let len = String.length s in
  if start_pos < len then
    if s.[start_pos] = '#' then
      let fragment_start = start_pos + 1 in
      let fragment = String.sub s fragment_start (len - fragment_start) in
      (Some fragment, len)
    else
      (None, start_pos)
  else
    (None, start_pos)

(* Main parsing function *)

let of_string = fun s ->
  if String.length s > 65_535 then
    Error TooLong
  else
    let pos = 0 in
    let scheme, pos = parse_scheme s pos in
    let authority, pos = parse_authority s pos in
    let path, pos = parse_path s pos in
    let query, pos = parse_query s pos in
    let fragment, _ = parse_fragment s pos in
    Ok {
      scheme;
      authority;
      path;
      query;
      fragment;
    }

let to_string = fun url ->
  let buf = Buffer.create 256 in
  (
    match url.scheme with
    | None -> ()
    | Some scheme ->
        Buffer.add_string buf scheme;
        Buffer.add_char buf ':'
  );
  (
    match url.authority with
    | None -> ()
    | Some authority ->
        Buffer.add_string buf "//";
        Buffer.add_string buf authority
  );
  Buffer.add_string buf url.path;
  (
    match url.query with
    | None -> ()
    | Some query ->
        Buffer.add_char buf '?';
        Buffer.add_string buf query
  );
  (
    match url.fragment with
    | None -> ()
    | Some fragment ->
        Buffer.add_char buf '#';
        Buffer.add_string buf fragment
  );
  Buffer.contents buf

(* Component access *)

let scheme = fun url -> url.scheme

let authority = fun url -> url.authority

let path = fun url -> url.path

let query = fun url -> url.query

let fragment = fun url -> url.fragment

let host = fun url ->
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

let port = fun url ->
  match url.authority with
  | None -> None
  | Some auth -> (
      match String.rindex_opt auth ':' with
      | None -> None
      | Some idx -> (
          let port_str = String.sub auth (idx + 1) (String.length auth - idx - 1) in
          try Some (int_of_string port_str) with
          | _ -> None
        )
    )

let path_and_query = fun url ->
  match url.query with
  | None -> url.path
  | Some q -> url.path ^ "?" ^ q

(* Component modules *)

module Scheme = struct
  type t = string

  let http = "http"

  let https = "https"

  let ftp = "ftp"

  let file = "file"

  let of_string = fun s ->
    if String.for_all is_scheme_char s && String.length s > 0 then
      Ok s
    else
      Error InvalidScheme

  let to_string = fun s -> s
end

module Authority = struct
  type t = string

  let of_string = fun s ->
    if String.for_all is_authority_char s then
      Ok s
    else
      Error InvalidAuthority

  let to_string = fun s -> s

  let host = fun auth ->
    let auth =
      match String.index_opt auth '@' with
      | None -> auth
      | Some idx -> String.sub auth (idx + 1) (String.length auth - idx - 1)
    in
    match String.rindex_opt auth ':' with
    | None -> auth
    | Some idx -> String.sub auth 0 idx

  let port = fun auth ->
    match String.rindex_opt auth ':' with
    | None -> None
    | Some idx -> (
        let port_str = String.sub auth (idx + 1) (String.length auth - idx - 1) in
        try Some (int_of_string port_str) with
        | _ -> None
      )

  let userinfo = fun auth ->
    match String.index_opt auth '@' with
    | None -> None
    | Some idx -> Some (String.sub auth 0 idx)
end

module PathAndQuery = struct
  type t = {
    path: string;
    query: string option;
  }

  let of_string = fun s ->
    match String.index_opt s '?' with
    | None -> Ok { path = s; query = None }
    | Some idx ->
        let path = String.sub s 0 idx in
        let query = String.sub s (idx + 1) (String.length s - idx - 1) in
        Ok { path; query = Some query }

  let to_string = fun pq ->
    match pq.query with
    | None -> pq.path
    | Some q -> pq.path ^ "?" ^ q

  let path = fun pq -> pq.path

  let query = fun pq -> pq.query
end

(* Builder *)

module Builder = struct
  type t = {
    scheme: string option;
    authority: string option;
    host: string option;
    port: int option;
    path: string option;
    query: string option;
    fragment: string option;
  }

  let create = fun () ->
    {
      scheme = None;
      authority = None;
      host = None;
      port = None;
      path = None;
      query = None;
      fragment = None;
    }

  let scheme = fun builder s -> { builder with scheme = Some s }

  let authority = fun builder s -> { builder with authority = Some s }

  let host = fun builder s -> { builder with host = Some s }

  let port = fun builder p -> { builder with port = Some p }

  let path = fun builder s -> { builder with path = Some s }

  let query = fun builder s -> { builder with query = Some s }

  let fragment = fun builder s -> { builder with fragment = Some s }

  let build = fun builder ->
    let authority =
      match builder.authority with
      | Some auth -> Some auth
      | None -> (
          match (builder.host, builder.port) with
          | Some h, Some p -> Some (h ^ ":" ^ string_of_int p)
          | Some h, None -> Some h
          | None, _ -> None
        )
    in
    let path =
      match builder.path with
      | Some p -> p
      | None -> "/"
    in
    Ok {
      scheme = builder.scheme;
      authority;
      path;
      query = builder.query;
      fragment = builder.fragment;
    }
end

(* Utilities *)

let is_absolute = fun url -> url.scheme != None

let is_relative = fun url -> url.scheme = None

let join = fun base relative_path ->
  match of_string relative_path with
  | Error e -> Error e
  | Ok rel_url ->
      if is_absolute rel_url then
        Ok rel_url
      else
        let new_path =
          if String.get relative_path 0 = '/' then
            relative_path
          else
            let base_path = base.path in
            let base_dir =
              match String.rindex_opt base_path '/' with
              | None -> ""
              | Some idx -> String.sub base_path 0 (idx + 1)
            in
            base_dir ^ relative_path
        in
        Ok { base with path = new_path; query = rel_url.query; fragment = rel_url.fragment }

let equal = fun url1 url2 ->
  String.equal (to_string url1) (to_string url2)

let compare = fun url1 url2 ->
  String.compare (to_string url1) (to_string url2)

(* Percent encoding/decoding *)
(** Check if character is unreserved per RFC 3986 Section 2.3 *)
let is_unreserved = function
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '-'
  | '.'
  | '_'
  | '~' -> true
  | _ -> false
(** Convert integer to hex char (0-15 -> '0'-'F') *)
let int_to_hex = fun n ->
  if n < 10 then
    Char.chr (Char.code '0' + n)
  else
    Char.chr (Char.code 'A' + n - 10)
(** Encode string per RFC 3986 - encode all except unreserved *)
let percent_encode = fun str ->
  let len = String.length str in
  let buf = Buffer.create (len * 3) in
  (* Worst case: all chars encoded *)
  let rec encode i =
    if i >= len then
      Buffer.contents buf
    else
      let c = String.get str i in
      if is_unreserved c then
        (
          Buffer.add_char buf c;
          encode (i + 1)
        )
      else
        (* Encode as %XX *)
        let code = Char.code c in
        Buffer.add_char buf '%';
        Buffer.add_char buf (int_to_hex (code / 16));
        Buffer.add_char buf (int_to_hex (code mod 16));
        encode (i + 1)
  in
  encode 0
(** Encode for application/x-www-form-urlencoded (space -> +) *)
let form_encode = fun str ->
  let len = String.length str in
  let buf = Buffer.create (len * 3) in
  let rec encode i =
    if i >= len then
      Buffer.contents buf
    else
      let c = String.get str i in
      if is_unreserved c then
        (
          Buffer.add_char buf c;
          encode (i + 1)
        )
      else if c = ' ' then
        (
          (* Space encoded as + in forms *)
          Buffer.add_char buf '+';
          encode (i + 1)
        )
      else
        (* Everything else as %XX *)
        let code = Char.code c in
        Buffer.add_char buf '%';
        Buffer.add_char buf (int_to_hex (code / 16));
        Buffer.add_char buf (int_to_hex (code mod 16));
        encode (i + 1)
  in
  encode 0
(** Decode percent-encoded string per RFC 3986 *)
let percent_decode = fun str ->
  let len = String.length str in
  let buf = Buffer.create len in
  let hex_to_int c =
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
    | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
    | _ -> None
  in
  let rec decode i =
    if i >= len then
      Buffer.contents buf
    else
      match String.get str i with
      | '%' when i + 2 < len ->
          let c1 = String.get str (i + 1) in
          let c2 = String.get str (i + 2) in
          (
            match (hex_to_int c1, hex_to_int c2) with
            | Some h1, Some h2 ->
                let code = (h1 * 16) + h2 in
                Buffer.add_char buf (Char.chr code);
                decode (i + 3)
            | _ ->
                (* Invalid - keep as-is *)
                Buffer.add_char buf '%';
                decode (i + 1)
          )
      | c ->
          Buffer.add_char buf c;
          decode (i + 1)
  in
  decode 0
(** Decode application/x-www-form-urlencoded (+ -> space) *)
let form_decode = fun str ->
  let len = String.length str in
  let buf = Buffer.create len in
  let hex_to_int c =
    match c with
    | '0' .. '9' -> Some (Char.code c - Char.code '0')
    | 'A' .. 'F' -> Some (Char.code c - Char.code 'A' + 10)
    | 'a' .. 'f' -> Some (Char.code c - Char.code 'a' + 10)
    | _ -> None
  in
  let rec decode i =
    if i >= len then
      Buffer.contents buf
    else
      match String.get str i with
      | '%' when i + 2 < len ->
          let c1 = String.get str (i + 1) in
          let c2 = String.get str (i + 2) in
          (
            match (hex_to_int c1, hex_to_int c2) with
            | Some h1, Some h2 ->
                let code = (h1 * 16) + h2 in
                Buffer.add_char buf (Char.chr code);
                decode (i + 3)
            | _ ->
                Buffer.add_char buf '%';
                decode (i + 1)
          )
      | '+' ->
          Buffer.add_char buf ' ';
          decode (i + 1)
      | c ->
          Buffer.add_char buf c;
          decode (i + 1)
  in
  decode 0

(* Query utilities *)

module Query = struct
  type param = string * string

  type t = param list

  let parse = fun query_string ->
    if String.length query_string = 0 then
      []
    else
      let pairs = String.split_on_char '&' query_string in
      List.filter_map
        (fun pair ->
          match String.index_opt pair '=' with
          | None ->
              let key = form_decode pair in
              Some (key, "")
          | Some idx ->
              let key = String.sub pair 0 idx in
              let value = String.sub pair (idx + 1) (String.length pair - idx - 1) in
              Some (form_decode key, form_decode value))
        pairs

  let to_string = fun params ->
    let param_strings =
      List.map
        (fun ((k, v)) ->
          let k_enc = form_encode k in
          let v_enc = form_encode v in
          if String.length v = 0 then
            k_enc
          else
            k_enc ^ "=" ^ v_enc)
        params
    in
    String.concat "&" param_strings

  let get = fun params key ->
    try Some (List.assoc key params) with
    | Not_found -> None

  let get_all = fun params key ->
    List.fold_left
      (fun acc ((k, v)) ->
        if String.equal k key then
          v :: acc
        else
          acc)
      []
      params |> List.rev

  let add = fun params key value -> (key, value) :: params

  let remove = fun params key ->
    List.filter (fun ((k, _)) -> not (String.equal k key)) params
end
