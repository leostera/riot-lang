open Global
open Collections

type name = string

type value = string

type t = (name * value) list

let empty = []

let from_list = fun headers -> headers

let to_list = fun headers -> headers

let add = fun headers name value -> (name, value) :: headers

let set = fun headers name value ->
  let filtered =
    List.filter
      headers
      ~fn:(fun (n, _) ->
        match String.compare (String.lowercase_ascii n) (String.lowercase_ascii name) with
        | Order.EQ -> false
        | Order.LT
        | Order.GT -> true)
  in
  (name, value) :: filtered

let normalize_name = fun name -> String.lowercase_ascii name

let get = fun headers name ->
  let normalized = normalize_name name in
  let rec find_header = fun __tmp1 ->
    match __tmp1 with
    | [] -> None
    | (n, v) :: rest -> (
        match String.compare (normalize_name n) normalized with
        | Order.EQ -> Some v
        | Order.LT
        | Order.GT -> find_header rest
      )
  in
  find_header headers

let get_all = fun headers name ->
  let normalized = normalize_name name in
  List.fold_left
    headers
    ~init:[]
    ~fn:(fun acc (n, v) ->
      match String.compare (normalize_name n) normalized with
      | Order.EQ -> v :: acc
      | Order.LT
      | Order.GT -> acc)
  |> List.reverse

let remove = fun headers name ->
  let normalized = normalize_name name in
  List.filter
    headers
    ~fn:(fun (n, _) ->
      match String.compare (normalize_name n) normalized with
      | Order.EQ -> false
      | Order.LT
      | Order.GT -> true)

let has = fun headers name ->
  let normalized = normalize_name name in
  List.any
    headers
    ~fn:(fun (n, _) ->
      match String.compare (normalize_name n) normalized with
      | Order.EQ -> true
      | Order.LT
      | Order.GT -> false)

let iter = fun f headers -> List.for_each headers ~fn:(fun (n, v) -> f n v)

let fold = fun f headers acc -> List.fold_left headers ~init:acc ~fn:(fun acc (n, v) -> f n v acc)

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
  type content_type_error =
    | InvalidContentType

  type authorization_error =
    | InvalidAuthorization

  let parse_content_type = fun value ->
    try
      let parts = String.split ~by:";" value in
      match parts with
      | [] -> Error InvalidContentType
      | media_type :: param_parts ->
          let media_type = String.trim media_type in
          let params =
            List.fold_left
              param_parts
              ~init:[]
              ~fn:(fun acc part ->
                let trimmed = String.trim part in
                match String.index_of trimmed ~char:'=' with
                | None -> acc
                | Some idx ->
                    let key = String.trim (String.sub trimmed ~offset:0 ~len:idx) in
                    let value =
                      String.trim
                        (String.sub trimmed ~offset:(idx + 1) ~len:(String.length trimmed - idx - 1))
                    in
                    (key, value) :: acc)
          in
          Ok (media_type, List.reverse params)
    with
    | _ -> Error InvalidContentType

  let parse_authorization = fun value ->
    try
      match String.index_of value ~char:' ' with
      | None -> Error InvalidAuthorization
      | Some idx ->
          let scheme = String.sub value ~offset:0 ~len:idx in
          let credentials =
            String.trim (String.sub value ~offset:(idx + 1) ~len:(String.length value - idx - 1))
          in
          Ok (scheme, credentials)
    with
    | _ -> Error InvalidAuthorization

  let parse_cache_control = fun value ->
    let directives = String.split ~by:"," value in
    List.fold_left
      directives
      ~init:[]
      ~fn:(fun acc directive ->
        let trimmed = String.trim directive in
        match String.index_of trimmed ~char:'=' with
        | None -> (trimmed, None) :: acc
        | Some idx ->
            let name = String.trim (String.sub trimmed ~offset:0 ~len:idx) in
            let value =
              String.trim
                (String.sub trimmed ~offset:(idx + 1) ~len:(String.length trimmed - idx - 1))
            in
            (name, Some value) :: acc)
    |> List.reverse

  let parse_accept = fun value ->
    let entries = String.split ~by:"," value in
    List.fold_left
      entries
      ~init:[]
      ~fn:(fun acc entry ->
        let trimmed = String.trim entry in
        let parts = String.split ~by:";" trimmed in
        match parts with
        | [] -> acc
        | media_type :: param_parts ->
            let media_type = String.trim media_type in
            let quality = ref None in
            let params =
              List.fold_left
                param_parts
                ~init:[]
                ~fn:(fun acc part ->
                  let trimmed = String.trim part in
                  match String.index_of trimmed ~char:'=' with
                  | None -> acc
                  | Some idx ->
                      let key = String.trim (String.sub trimmed ~offset:0 ~len:idx) in
                      let value =
                        String.trim
                          (String.sub
                            trimmed
                            ~offset:(idx + 1)
                            ~len:(String.length trimmed - idx - 1))
                      in
                      if String.equal key "q" then (
                        match Float.parse value with
                        | Some parsed ->
                            quality := Some parsed;
                            acc
                        | None -> (key, value) :: acc
                      ) else
                        (key, value) :: acc)
            in
            (media_type, !quality, List.reverse params) :: acc)
    |> List.reverse
end
