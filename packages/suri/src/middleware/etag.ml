open Std

(** Generate ETag from response body *)
let generate_etag = fun ?(weak = false) body ->
  if String.length body = 0 then
    None
  else
    (* Hash the body content *)
    let hash = Crypto.Sha256.hash_string body in
    let hash_hex = Crypto.Digest.hex hash in
    (* Take first 16 chars of hex for reasonable ETag length *)
    let etag_value = String.sub hash_hex 0 (min 16 (String.length hash_hex)) in
    (* Format as ETag *)
    let etag =
      if weak then
        "W/\"" ^ etag_value ^ "\""
      else
        "\"" ^ etag_value ^ "\""
    in
    Some etag

(** ETag middleware - optional param first to avoid type issues *)
let middleware = fun ?(weak = false) ~conn ~next ->
  (* Process request *)
  let conn' = next conn in
  (* Check if ETag already set *)
  let has_etag =
    List.exists (fun ((name, _)) -> String.lowercase_ascii name = "etag") (Conn.resp_headers conn')
  in
  if has_etag then
    conn'
  else
    (* Generate ETag from response body *)
    let resp = Conn.to_response conn' in
    match generate_etag ~weak resp.Web_server.Response.body with
    | Some etag ->
        (* Add ETag header *)
        Conn.with_header "etag" etag conn'
    | None ->
        (* Empty body, no ETag *)
        conn'
