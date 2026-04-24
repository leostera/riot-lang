open Std

(** Check if origin matches a pattern *)
let origin_matches = fun pattern origin ->
  match pattern with
  | "*" -> true
  | p -> String.equal p origin

(** Check if any pattern matches the origin *)
let is_origin_allowed = fun origins origin ->
  List.exists (fun pattern -> origin_matches pattern origin) origins

(** Get the origin value to send in response headers *)
let get_response_origin = fun origins origin credentials ->
  if List.contains origins ~value:"*" && not credentials then
    "*"
  else
    origin

(** CORS middleware with simple configuration *)
let middleware = fun ~origins ?(methods = [ Net.Http.Method.Put; Patch; Delete ]) ?(headers = []) ?(credentials = false) ?(expose = []) ?max_age () ->
  (* Validate configuration *)
  let () =
    if List.contains origins ~value:"*" && credentials then
      Log.warn "[CORS] Warning: Wildcard origin with credentials is a security risk!"
  in
  fun ~conn ~next ->
    (* Extract origin from request *)
    let req_headers = Conn.headers conn in
    match Net.Http.Header.get req_headers "origin" with
    | None ->
        (* No CORS request - pass through *)
        next conn
    | Some req_origin ->
        (* Check if origin is allowed *)
        if not (is_origin_allowed origins req_origin) then
          begin
            Log.debug
              (String.concat
                ""
                [
                  "[CORS] Rejected origin: ";
                  req_origin;
                  " for ";
                  Net.Http.Method.to_string (Conn.method_ conn);
                  " ";
                  Conn.path conn
                ]);
            conn
            |> Conn.respond ~status:Net.Http.Status.Forbidden ~body:"Origin not allowed"
            |> Conn.halt
          end
          (* Check if this is a preflight request *)
        else if
          Conn.method_ conn = Net.Http.Method.Options
          && (Net.Http.Header.get req_headers "access-control-request-method" |> Option.is_some)
        then
          begin
            (* Preflight request - respond immediately *)
            let origin_val = get_response_origin origins req_origin credentials in
            (* Build allowed methods list *)
            let all_methods = [ Net.Http.Method.Get; Head; Post ] @ methods
            |> List.unique ~compare
            |> List.map ~fn:Net.Http.Method.to_string
            |> String.concat ", " in
            Log.debug
              (String.concat "" [ "[CORS] Preflight from origin: "; req_origin; " -> "; origin_val ]);
            let conn = conn
            |> Conn.respond ~status:Net.Http.Status.Ok
            |> Conn.with_header "access-control-allow-origin" origin_val
            |> Conn.with_header "access-control-allow-methods" all_methods in
            (* Add allowed headers if specified *)
            let conn =
              match headers with
              | [] -> conn
              | _ -> Conn.with_header "access-control-allow-headers" (String.concat ", " headers) conn
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
                [ "[CORS] Simple request from origin: "; req_origin; " -> "; origin_val ]);
            (* Call next handler *)
            let conn' = next conn in
            (* Add CORS headers to response *)
            let conn' = conn' |> Conn.with_header "access-control-allow-origin" origin_val in
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
              | _ -> Conn.with_header "access-control-expose-headers" (String.concat ", " expose) conn'
            in
            (* Add Vary header *)
            let conn' =
              match origin_val with
              | "*" -> conn'
              | _ -> Conn.with_header "vary" "Origin" conn'
            in
            conn'
          end
