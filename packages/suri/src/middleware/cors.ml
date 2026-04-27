open Std

type config_error =
  | WildcardOriginWithCredentials

exception Invalid_config of config_error

let config_error_to_string = function
  | WildcardOriginWithCredentials -> "CORS wildcard origins cannot be combined with credentials"

type preflight_error =
  | MethodNotAllowed of string
  | HeadersNotAllowed of string list

let preflight_error_to_string = function
  | MethodNotAllowed method_ -> "CORS preflight method is not allowed: " ^ method_
  | HeadersNotAllowed headers ->
      "CORS preflight headers are not allowed: " ^ String.concat ", " headers

let validate_config = fun ~origins ~credentials ->
  if List.contains origins ~value:"*" && credentials then
    Error WildcardOriginWithCredentials
  else
    Ok ()

let default_methods = [ Net.Http.Method.Put; Patch; Delete ]

let simple_methods = [ Net.Http.Method.Get; Head; Post ]

let simple_headers = [ "accept"; "accept-language"; "content-language"; "content-type" ]

(** Check if origin matches a pattern *)
let origin_matches = fun pattern origin ->
  match pattern with
  | "*" -> true
  | p -> String.equal p origin

(** Check if any pattern matches the origin *)
let is_origin_allowed = fun origins origin ->
  List.exists
    (fun pattern -> origin_matches pattern origin)
    origins

(** Get the origin value to send in response headers *)
let get_response_origin = fun origins origin credentials ->
  if List.contains origins ~value:"*" && not credentials then
    "*"
  else
    origin

let normalize_method_name = fun method_ ->
  method_
  |> String.trim
  |> String.uppercase_ascii

let method_names = fun methods ->
  simple_methods @ methods
  |> List.unique ~compare:Net.Http.Method.compare
  |> List.map ~fn:Net.Http.Method.to_string

let normalize_header_name = fun header ->
  header
  |> String.trim
  |> String.lowercase_ascii

let requested_headers = fun request_headers ->
  match request_headers with
  | None -> []
  | Some value ->
      value
      |> String.split_on_char ','
      |> List.map ~fn:normalize_header_name
      |> List.filter ~fn:(fun header -> not (String.equal header ""))

let allowed_header_names = fun headers ->
  simple_headers @ List.map ~fn:normalize_header_name headers
  |> List.unique ~compare:String.compare

let validate_preflight = fun ~methods ~headers ~request_method ~request_headers ->
  let method_ = normalize_method_name request_method in
  let allowed_methods = method_names methods in
  if not (List.contains allowed_methods ~value:method_) then
    Error (MethodNotAllowed method_)
  else
    let allowed_headers = allowed_header_names headers in
    let forbidden_headers =
      requested_headers request_headers
      |> List.filter ~fn:(fun header -> not (List.contains allowed_headers ~value:header))
    in
    match forbidden_headers with
    | [] -> Ok ()
    | _ -> Error (HeadersNotAllowed forbidden_headers)

(** CORS middleware with simple configuration *)
let middleware = fun
  ~origins
  ?(methods = default_methods)
  ?(headers = [])
  ?(credentials = false)
  ?(expose = [])
  ?max_age
  () ->
  (
    match validate_config ~origins ~credentials with
    | Ok () -> ()
    | Error error -> raise (Invalid_config error)
  );
  fun ~conn ~next ->
    (* Extract origin from request *)
    let req_headers = Conn.headers conn in
    match Net.Http.Header.get req_headers "origin" with
    | None ->
        (* No CORS request - pass through *)
        next conn
    | Some req_origin ->
        (* Check if origin is allowed *)
        if not (is_origin_allowed origins req_origin) then (
          Log.debug
            (String.concat
              ""
              [
                "[CORS] Rejected origin: ";
                req_origin;
                " for ";
                Net.Http.Method.to_string (Conn.method_ conn);
                " ";
                Conn.path conn;
              ]);
          conn
          |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"Origin not allowed"
          |> Conn.halt
        )
          (* Check if this is a preflight request *)
        else if Conn.method_ conn = Net.Http.Method.Options
        && (
          Net.Http.Header.get req_headers "access-control-request-method"
          |> Option.is_some
        ) then
          begin
            match validate_preflight
              ~methods
              ~headers
              ~request_method:(
                Net.Http.Header.get req_headers "access-control-request-method"
                |> Option.unwrap_or ~default:""
              )
              ~request_headers:(Net.Http.Header.get req_headers "access-control-request-headers") with
            | Error error ->
                Log.debug (preflight_error_to_string error);
                conn
                |> Conn.respond
                  ~status:Net.Http.Status.Forbidden
                  ~body:(preflight_error_to_string error)
                |> Conn.halt
            | Ok () ->
                (* Preflight request - respond immediately *)
                let origin_val = get_response_origin origins req_origin credentials in
                (* Build allowed methods list *)
                let all_methods =
                  method_names methods
                  |> String.concat ", "
                in
                Log.debug
                  (String.concat
                    ""
                    [ "[CORS] Preflight from origin: "; req_origin; " -> "; origin_val; ]);
                let conn =
                  conn
                  |> Conn.respond ~status:Net.Http.Status.NoContent
                  |> Conn.with_header "access-control-allow-origin" origin_val
                  |> Conn.with_header "access-control-allow-methods" all_methods
                in
                (* Add allowed headers if specified *)
                let conn =
                  match headers with
                  | [] -> conn
                  | _ ->
                      Conn.with_header
                        "access-control-allow-headers"
                        (String.concat ", " headers)
                        conn
                in
                (* Add credentials if enabled *)
                let conn =
                  if credentials then
                    Conn.with_header "access-control-allow-credentials" "true" conn
                  else
                    conn
                in
                (* Add max-age if specified *)
                let conn =
                  match max_age with
                  | Some age -> Conn.with_header "access-control-max-age" (string_of_int age) conn
                  | None -> conn
                in
                (* Add Vary header *)
                let conn =
                  match origin_val with
                  | "*" -> conn
                  | _ -> Conn.with_header "vary" "Origin" conn
                in
                Conn.halt conn
          end
        (* Simple CORS request - add headers to response *)
        else
          begin
            let origin_val = get_response_origin origins req_origin credentials in
            Log.debug
              (String.concat
                ""
                [ "[CORS] Simple request from origin: "; req_origin; " -> "; origin_val; ]);
            (* Call next handler *)
            let conn' = next conn in
            (* Add CORS headers to response *)
            let conn' =
              conn'
              |> Conn.with_header "access-control-allow-origin" origin_val
            in
            (* Add credentials if enabled *)
            let conn' =
              if credentials then
                Conn.with_header "access-control-allow-credentials" "true" conn'
              else
                conn'
            in
            (* Add exposed headers if specified *)
            let conn' =
              match expose with
              | [] -> conn'
              | _ ->
                  Conn.with_header
                    "access-control-expose-headers"
                    (String.concat ", " expose)
                    conn'
            in
            (* Add Vary header *)
            let conn' =
              match origin_val with
              | "*" -> conn'
              | _ -> Conn.with_header "vary" "Origin" conn'
            in
            conn'
          end

module For_testing = struct
  let validate_config = validate_config

  let validate_preflight = validate_preflight

  let requested_headers = requested_headers
end
