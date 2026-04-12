open Global
open Collections

type name = string

type value = string

type t = (name * value) list

let empty = []

let of_list = fun headers -> headers

let to_list = fun headers -> headers

let add = fun headers name value -> (name, value) :: headers

let set = fun headers name value ->
  let filtered =
    List.filter
      (fun ((n, _)) -> String.compare (String.lowercase_ascii n) (String.lowercase_ascii name) != 0)
      headers
  in
  (name, value) :: filtered

let normalize_name = fun name -> String.lowercase_ascii name

let get = fun headers name ->
  let normalized = normalize_name name in
  let rec find_header = function
    | [] -> None
    | (n, v) :: _ when String.compare (normalize_name n) normalized = 0 -> Some v
    | _ :: rest -> find_header rest
  in
  find_header headers

let get_all = fun headers name ->
  let normalized = normalize_name name in
  List.fold_left
    (fun acc ((n, v)) ->
      if String.compare (normalize_name n) normalized = 0 then
        v :: acc
      else
        acc)
    []
    headers |> List.rev

let remove = fun headers name ->
  let normalized = normalize_name name in
  List.filter (fun ((n, _)) -> String.compare (normalize_name n) normalized != 0) headers

let has = fun headers name ->
  let normalized = normalize_name name in
  List.exists (fun ((n, _)) -> String.compare (normalize_name n) normalized = 0) headers

let iter = fun f headers ->
  List.iter (fun ((n, v)) -> f n v) headers

let fold = fun f headers acc ->
  List.fold_left (fun acc ((n, v)) -> f n v acc) acc headers

let length = fun headers -> List.length headers

let is_empty = fun headers -> headers = []

module Name = struct
  let content_type = "Content-Type"

  let content_length = "Content-Length"

  let authorization = "Authorization"

  let user_agent = "User-Agent"

  let accept = "Accept"

  let accept_encoding = "Accept-Encoding"

  let accept_language = "Accept-Language"

  let cache_control = "Cache-Control"

  let connection = "Connection"

  let cookie = "Cookie"

  let host = "Host"

  let referer = "Referer"

  let server = "Server"

  let set_cookie = "Set-Cookie"

  let transfer_encoding = "Transfer-Encoding"

  let location = "Location"

  let www_authenticate = "WWW-Authenticate"

  let date = "Date"

  let etag = "ETag"

  let expires = "Expires"

  let last_modified = "Last-Modified"

  let if_modified_since = "If-Modified-Since"

  let if_none_match = "If-None-Match"

  let vary = "Vary"

  let x_forwarded_for = "X-Forwarded-For"

  let x_real_ip = "X-Real-IP"
end

module Value = struct
  let parse_content_type = fun value ->
    try
      let parts = String.split_on_char ';' value in
      match parts with
      | [] -> Error `InvalidContentType
      | media_type :: param_parts ->
          let media_type = String.trim media_type in
          let params =
            List.fold_left
              (fun acc part ->
                let trimmed = String.trim part in
                match String.index trimmed '=' with
                | None -> acc
                | Some idx ->
                    let key = String.trim (String.sub trimmed 0 idx) in
                    let value = String.trim
                      (String.sub trimmed (idx + 1) (String.length trimmed - idx - 1)) in
                    (key, value) :: acc)
              []
              param_parts
          in
          Ok (media_type, List.rev params)
    with
    | _ -> Error `InvalidContentType

  let parse_authorization = fun value ->
    try
      match String.index value ' ' with
      | None -> Error `InvalidAuthorization
      | Some idx ->
          let scheme = String.sub value 0 idx in
          let credentials = String.trim (String.sub value (idx + 1) (String.length value - idx - 1)) in
          Ok (scheme, credentials)
    with
    | _ -> Error `InvalidAuthorization

  let parse_cache_control = fun value ->
    let directives = String.split_on_char ',' value in
    List.fold_left
      (fun acc directive ->
        let trimmed = String.trim directive in
        match String.index trimmed '=' with
        | None -> (trimmed, None) :: acc
        | Some idx ->
            let name = String.trim (String.sub trimmed 0 idx) in
            let value = String.trim (String.sub trimmed (idx + 1) (String.length trimmed - idx - 1)) in
            (name, Some value) :: acc)
      []
      directives |> List.rev

  let parse_accept = fun value ->
    let entries = String.split_on_char ',' value in
    List.fold_left
      (fun acc entry ->
        let trimmed = String.trim entry in
        let parts = String.split_on_char ';' trimmed in
        match parts with
        | [] -> acc
        | media_type :: param_parts ->
            let media_type = String.trim media_type in
            let quality = ref None in
            let params =
              List.fold_left
                (fun acc part ->
                  let trimmed = String.trim part in
                  match String.index trimmed '=' with
                  | None -> acc
                  | Some idx ->
                      let key = String.trim (String.sub trimmed 0 idx) in
                      let value = String.trim
                        (String.sub trimmed (idx + 1) (String.length trimmed - idx - 1)) in
                      if String.equal key "q" then
                        (
                          match Float.parse value with
                          | Some parsed ->
                              quality := Some parsed;
                              acc
                          | None -> (key, value) :: acc
                        )
                      else
                        (key, value) :: acc)
                []
                param_parts
            in
            (media_type, !quality, List.rev params) :: acc)
      []
      entries |> List.rev
end
