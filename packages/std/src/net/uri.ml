open Global
open IO
open Collections

module Slice = IoVec.IoSlice

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

let is_scheme_char = fun __tmp1 ->
  match __tmp1 with
  | 'a' .. 'z'
  | 'A' .. 'Z'
  | '0' .. '9'
  | '+'
  | '-'
  | '.' -> true
  | _ -> false

let is_authority_char = fun __tmp1 ->
  match __tmp1 with
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

let is_path_char = fun __tmp1 ->
  match __tmp1 with
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

let is_query_char = fun __tmp1 ->
  match __tmp1 with
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

type 'source source_ops = {
  length: 'source -> int;
  get_unchecked: 'source -> at:int -> char;
  sub_to_string: 'source -> off:int -> len:int -> string;
}

let parse_scheme = fun ops source start_pos ->
  let len = ops.length source in
  let rec find_end pos =
    if pos >= len then
      None
    else
      match ops.get_unchecked source ~at:pos with
      | ':' -> Some pos
      | c when is_scheme_char c -> find_end (pos + 1)
      | _ -> None
  in
  match find_end start_pos with
  | None -> (None, start_pos)
  | Some end_pos ->
      let scheme = ops.sub_to_string source ~off:start_pos ~len:(end_pos - start_pos) in
      (Some scheme, end_pos + 1)

let parse_authority = fun ops source start_pos ->
  let len = ops.length source in
  if start_pos + 1 < len then
    if
      ops.get_unchecked source ~at:start_pos = '/'
      && ops.get_unchecked source ~at:(start_pos + 1) = '/'
    then
      let authority_start = start_pos + 2 in
      let rec find_end pos =
        if pos >= len then
          pos
        else
          match ops.get_unchecked source ~at:pos with
          | '/'
          | '?'
          | '#' -> pos
          | c when is_authority_char c -> find_end (pos + 1)
          | _ -> pos
      in
      let authority_end = find_end authority_start in
      let authority =
        ops.sub_to_string source ~off:authority_start ~len:(authority_end - authority_start)
      in
      (Some authority, authority_end)
    else
      (None, start_pos)
  else
    (None, start_pos)

let parse_path = fun ops source start_pos ->
  let len = ops.length source in
  let rec find_end pos =
    if pos >= len then
      pos
    else
      match ops.get_unchecked source ~at:pos with
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
      ops.sub_to_string source ~off:start_pos ~len:(path_end - start_pos)
  in
  (path, path_end)

let parse_query = fun ops source start_pos ->
  let len = ops.length source in
  if start_pos < len then
    if ops.get_unchecked source ~at:start_pos = '?' then
      let query_start = start_pos + 1 in
      let rec find_end pos =
        if pos >= len then
          pos
        else
          match ops.get_unchecked source ~at:pos with
          | '#' -> pos
          | c when is_query_char c -> find_end (pos + 1)
          | _ -> pos
      in
      let query_end = find_end query_start in
      let query = ops.sub_to_string source ~off:query_start ~len:(query_end - query_start) in
      (Some query, query_end)
    else
      (None, start_pos)
  else
    (None, start_pos)

let parse_fragment = fun ops source start_pos ->
  let len = ops.length source in
  if start_pos < len then
    if ops.get_unchecked source ~at:start_pos = '#' then
      let fragment_start = start_pos + 1 in
      let fragment = ops.sub_to_string source ~off:fragment_start ~len:(len - fragment_start) in
      (Some fragment, len)
    else
      (None, start_pos)
  else
    (None, start_pos)

(* Main parsing function *)

let parse_with = fun ops source ->
  if ops.length source > 65_535 then
    Error TooLong
  else
    let pos = 0 in
    let (scheme, pos) = parse_scheme ops source pos in
    let (authority, pos) = parse_authority ops source pos in
    let (path, pos) = parse_path ops source pos in
    let (query, pos) = parse_query ops source pos in
    let (fragment, _) = parse_fragment ops source pos in
    Ok {
      scheme;
      authority;
      path;
      query;
      fragment;
    }

let string_ops = {
  length = String.length;
  get_unchecked = String.get_unchecked;
  sub_to_string = (fun value ~off ~len -> String.sub value ~offset:off ~len);
}

let slice_ops = {
  length = Slice.length;
  get_unchecked = Slice.get_unchecked;
  sub_to_string = (fun value ~off ~len -> Slice.to_string (Slice.sub_unchecked value ~off ~len));
}

let from_string = fun s -> parse_with string_ops s

let parse_origin_form_slice = fun value ->
  let len = Slice.length value in
  let module Origin_scan = struct
    type path_stop =
      | Done of int
      | Query of int
      | Fragment of int

    type next_part =
      | Next_query
      | Next_fragment
  end in
  let rec scan_path pos =
    if pos >= len then
      Ok (Origin_scan.Done pos)
    else
      match Slice.get_unchecked value ~at:pos with
      | '?' -> Ok (Origin_scan.Query pos)
      | '#' -> Ok (Origin_scan.Fragment pos)
      | c when is_path_char c -> scan_path (pos + 1)
      | _ -> Error ()
  in
  let rec scan_query pos =
    if pos >= len then
      Ok pos
    else
      match Slice.get_unchecked value ~at:pos with
      | '#' -> Ok pos
      | c when is_query_char c -> scan_query (pos + 1)
      | _ -> Error ()
  in
  match scan_path 0 with
  | Error () -> parse_with slice_ops value
  | Ok path_stop -> (
      let (path_end, next) =
        match path_stop with
        | Origin_scan.Done pos -> (pos, None)
        | Origin_scan.Query pos -> (pos, Some (Origin_scan.Next_query, pos + 1))
        | Origin_scan.Fragment pos -> (pos, Some (Origin_scan.Next_fragment, pos + 1))
      in
      let path =
        if path_end = 0 then
          "/"
        else
          Slice.to_string (Slice.sub_unchecked value ~off:0 ~len:path_end)
      in
      match next with
      | None ->
          Ok {
            scheme = None;
            authority = None;
            path;
            query = None;
            fragment = None;
          }
      | Some (Origin_scan.Next_fragment, fragment_start) ->
          Ok {
            scheme = None;
            authority = None;
            path;
            query = None;
            fragment = Some (Slice.to_string
              (Slice.sub_unchecked value ~off:fragment_start ~len:(len - fragment_start)));
          }
      | Some (Origin_scan.Next_query, query_start) -> (
          match scan_query query_start with
          | Error () -> parse_with slice_ops value
          | Ok query_end ->
              let query = Some (Slice.to_string
                (Slice.sub_unchecked value ~off:query_start ~len:(query_end - query_start)))
              in
              let fragment =
                if query_end >= len then
                  None
                else if Slice.get_unchecked value ~at:query_end = '#' then
                  Some (Slice.to_string
                    (Slice.sub_unchecked value ~off:(query_end + 1) ~len:(len - query_end - 1)))
                else
                  None
              in
              Ok {
                scheme = None;
                authority = None;
                path;
                query;
                fragment;
              }
        )
    )

let from_slice = fun value ->
  if Slice.length value > 65_535 then
    Error TooLong
  else if Slice.length value > 0 && Slice.get_unchecked value ~at:0 = '/' then
    parse_origin_form_slice value
  else
    parse_with slice_ops value

let to_string = fun url ->
  let buf = Buffer.create ~size:256 in
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
        match String.index_of auth ~char:'@' with
        | None -> auth
        | Some idx -> String.sub auth ~offset:(idx + 1) ~len:(String.length auth - idx - 1)
      in
      let host =
        match String.last_index auth ':' with
        | None -> auth
        | Some idx -> String.sub auth ~offset:0 ~len:idx
      in
      Some host

let port = fun url ->
  match url.authority with
  | None -> None
  | Some auth -> (
      match String.last_index auth ':' with
      | None -> None
      | Some idx -> (
          let port_str = String.sub auth ~offset:(idx + 1) ~len:(String.length auth - idx - 1) in
          Int.parse port_str
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

  let from_string = fun s ->
    if String.for_all s ~fn:is_scheme_char && String.length s > 0 then
      Ok s
    else
      Error InvalidScheme

  let to_string = fun s -> s
end

module Authority = struct
  type t = string

  let from_string = fun s ->
    if String.for_all s ~fn:is_authority_char then
      Ok s
    else
      Error InvalidAuthority

  let to_string = fun s -> s

  let host = fun auth ->
    let auth =
      match String.index_of auth ~char:'@' with
      | None -> auth
      | Some idx -> String.sub auth ~offset:(idx + 1) ~len:(String.length auth - idx - 1)
    in
    match String.last_index auth ':' with
    | None -> auth
    | Some idx -> String.sub auth ~offset:0 ~len:idx

  let port = fun auth ->
    match String.last_index auth ':' with
    | None -> None
    | Some idx -> (
        let port_str = String.sub auth ~offset:(idx + 1) ~len:(String.length auth - idx - 1) in
        Int.parse port_str
      )

  let userinfo = fun auth ->
    match String.index_of auth ~char:'@' with
    | None -> None
    | Some idx -> Some (String.sub auth ~offset:0 ~len:idx)
end

module PathAndQuery = struct
  type t = {
    path: string;
    query: string option;
  }

  let from_string = fun s ->
    match String.index_of s ~char:'?' with
    | None -> Ok { path = s; query = None }
    | Some idx ->
        let path = String.sub s ~offset:0 ~len:idx in
        let query = String.sub s ~offset:(idx + 1) ~len:(String.length s - idx - 1) in
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
          | (Some h, Some p) -> Some (h ^ ":" ^ Int.to_string p)
          | (Some h, None) -> Some h
          | (None, _) -> None
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
  match from_string relative_path with
  | Error e -> Error e
  | Ok rel_url ->
      if is_absolute rel_url then
        Ok rel_url
      else
        let new_path =
          if String.starts_with ~prefix:"/" relative_path then
            relative_path
          else
            let base_path = base.path in
            let base_dir =
              match String.last_index base_path '/' with
              | None -> ""
              | Some idx -> String.sub base_path ~offset:0 ~len:(idx + 1)
            in
            base_dir ^ relative_path
        in
        Ok { base with path = new_path; query = rel_url.query; fragment = rel_url.fragment }

let equal = fun url1 url2 -> String.equal (to_string url1) (to_string url2)

let compare = fun url1 url2 -> String.compare (to_string url1) (to_string url2)

(* Percent encoding/decoding *)
(** Check if character is unreserved per RFC 3986 Section 2.3 *)

let is_unreserved = fun __tmp1 ->
  match __tmp1 with
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
    Char.from_int_unchecked (Char.to_int '0' + n)
  else
    Char.from_int_unchecked (Char.to_int 'A' + n - 10)

(** Encode string per RFC 3986 - encode all except unreserved *)
let percent_encode = fun str ->
  let len = String.length str in
  let buf = Buffer.create ~size:(len * 3) in
  (* Worst case: all chars encoded *)
  let rec encode i =
    if i >= len then
      Buffer.contents buf
    else
      let c = String.get_unchecked str ~at:i in
      if is_unreserved c then (
        Buffer.add_char buf c;
        encode (i + 1)
      ) else
        (* Encode as %XX *)
        let code = Char.to_int c in
        Buffer.add_char buf '%';
    Buffer.add_char buf (int_to_hex (code / 16));
    Buffer.add_char buf (int_to_hex (code mod 16));
    encode (i + 1)
  in
  encode 0

(** Encode for application/x-www-form-urlencoded (space -> +) *)
let form_encode = fun str ->
  let len = String.length str in
  let buf = Buffer.create ~size:(len * 3) in
  let rec encode i =
    if i >= len then
      Buffer.contents buf
    else
      let c = String.get_unchecked str ~at:i in
      if is_unreserved c then (
        Buffer.add_char buf c;
        encode (i + 1)
      ) else if c = ' ' then (
        (* Space encoded as + in forms *)
        Buffer.add_char buf '+';
        encode (i + 1)
      ) else
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
  let buf = Buffer.create ~size:len in
  let hex_to_int c =
    match c with
    | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
    | 'A' .. 'F' -> Some (Char.to_int c - Char.to_int 'A' + 10)
    | 'a' .. 'f' -> Some (Char.to_int c - Char.to_int 'a' + 10)
    | _ -> None
  in
  let rec decode i =
    if i >= len then
      Buffer.contents buf
    else
      match String.get_unchecked str ~at:i with
      | '%' when i + 2 < len ->
          let c1 = String.get_unchecked str ~at:(i + 1) in
          let c2 = String.get_unchecked str ~at:(i + 2) in
          (
            match (hex_to_int c1, hex_to_int c2) with
            | (Some h1, Some h2) ->
                let code = (h1 * 16) + h2 in
                Buffer.add_char buf (Char.from_int_unchecked code);
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
  let buf = Buffer.create ~size:len in
  let hex_to_int c =
    match c with
    | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
    | 'A' .. 'F' -> Some (Char.to_int c - Char.to_int 'A' + 10)
    | 'a' .. 'f' -> Some (Char.to_int c - Char.to_int 'a' + 10)
    | _ -> None
  in
  let rec decode i =
    if i >= len then
      Buffer.contents buf
    else
      match String.get_unchecked str ~at:i with
      | '%' when i + 2 < len ->
          let c1 = String.get_unchecked str ~at:(i + 1) in
          let c2 = String.get_unchecked str ~at:(i + 2) in
          (
            match (hex_to_int c1, hex_to_int c2) with
            | (Some h1, Some h2) ->
                let code = (h1 * 16) + h2 in
                Buffer.add_char buf (Char.from_int_unchecked code);
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
      let pairs = String.split ~by:"&" query_string in
      List.filter_map
        pairs
        ~fn:(fun pair ->
          match String.index_of pair ~char:'=' with
          | None ->
              let key = form_decode pair in
              Some (key, "")
          | Some idx ->
              let key = String.sub pair ~offset:0 ~len:idx in
              let value = String.sub pair ~offset:(idx + 1) ~len:(String.length pair - idx - 1) in
              Some (form_decode key, form_decode value))

  let to_string = fun params ->
    let param_strings =
      List.map
        params
        ~fn:(fun (k, v) ->
          let k_enc = form_encode k in
          let v_enc = form_encode v in
          if String.length v = 0 then
            k_enc
          else
            k_enc ^ "=" ^ v_enc)
    in
    String.concat "&" param_strings

  let get = fun params key ->
    match List.find params ~fn:(fun (param_key, _) -> String.equal param_key key) with
    | None -> None
    | Some (_, value) -> Some value

  let get_all = fun params key ->
    List.fold_left
      params
      ~init:[]
      ~fn:(fun acc (k, v) ->
        if String.equal k key then
          v :: acc
        else
          acc)
    |> List.reverse

  let add = fun params key value -> (key, value) :: params

  let remove = fun params key -> List.filter params ~fn:(fun (k, _) -> not (String.equal k key))
end
